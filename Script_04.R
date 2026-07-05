# ==============================================================================
# 8. SIMULATORE MONTE CARLO E GRAFICO DELLA DISTRIBUZIONE FINALE
# ==============================================================================
library(tidyverse)
library(ggplot2)

# 1. Configurazione del Portafoglio Reale Attuale (Base ~30k €)
somma_iniziale <- 30000

portafoglio_iniziale <- target_weights * 33818
sum(portafoglio_inziale)

# Parametri Simulazione (Aumentiamo a 1.000 simulazioni per un istogramma più denso e preciso)
orizzonte_mesi <- 240    
quota_mensile  <- 1000   
n_simulazioni  <- 10000   
nomi_asset     <- names(target_weights)

capitale_investito_totale <- sum(portafoglio_iniziale) + (orizzonte_mesi * quota_mensile)
risultati_scenari <- numeric(n_simulazioni)

set.seed(42) 

for (s in 1:n_simulazioni) {
  portafoglio <- portafoglio_iniziale
  
  for (m in 1:orizzonte_mesi) {
    riga_estratta <- sample(1:nrow(dataset_rendimenti), size = 1)
    rendimenti_estratti <- as.numeric(dataset_rendimenti[riga_estratta, nomi_asset])
    names(rendimenti_estratti) <- nomi_asset
    
    portafoglio <- portafoglio * (1 + rendimenti_estratti)
    
    valore_totale_pre <- sum(portafoglio)
    valore_obiettivo_post <- valore_totale_pre + quota_mensile
    
    target_ideale <- valore_obiettivo_post * target_weights
    distanza_da_target <- target_ideale - portafoglio
    quote_da_comprare <- ifelse(distanza_da_target > 0, distanza_da_target, 0)
    
    if (sum(quote_da_comprare) > 0) {
      pesi_acquisto <- quote_da_comprare / sum(quote_da_comprare)
      acquisti_effettivi <- pesi_acquisto * quota_mensile
    } else {
      acquisti_effettivi <- target_weights * quota_mensile
    }
    
    portafoglio <- portafoglio + acquisti_effettivi
  }
  risultati_scenari[s] <- sum(portafoglio)
}

# ==============================================================================
# 9. CREAZIONE DEL DATASET PER IL PLOT E CALCOLO DEI MULTIPLI
# ==============================================================================
df_scenari <- tibble(Valore_Finale = risultati_scenari) %>%
  mutate(Multiplo_Capitale = Valore_Finale / capitale_investito_totale)

# Calcolo percentili per le linee verticali di controllo nel grafico
p10 <- quantile(risultati_scenari, 0.10)
p50 <- quantile(risultati_scenari, 0.50)
p90 <- quantile(risultati_scenari, 0.90)

# ==============================================================================
# 10. PLOT DELL'ISTOGRAMMA DELLA DISTRIBUZIONE FINALE
# ==============================================================================
ggplot(df_scenari, aes(x = Valore_Finale)) +
  # Istogramma delle frequenze dei portafogli finali
  geom_histogram(bins = 40, fill = "cadetblue4", color = "white", alpha = 0.8) +
  
  # Linea verticale del Capitale Puramente Investito (Break-even)
  geom_vline(xintercept = capitale_investito_totale, color = "black", linetype = "solid", linewidth = 1) +
  geom_text(aes(x = capitale_investito_totale, y = 0, label = "Capitale Investito"), 
            angle = 90, vjust = -1, hjust = -0.5, color = "black", size = 3.5, fontface = "bold") +
  
  # Linee verticali dei percentili (Pessimo, Mediano, Ottimo)
  geom_vline(xintercept = c(p10, p50, p90), color = c("firebrick3", "orange", "springgreen4"), 
             linetype = "dashed", linewidth = 0.8) +
  
  # Asse X primario (Valore in Euro) e asse X secondario (Multiplo del Capitale Investito)
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      trans = ~ . / capitale_investito_totale,
      name = "Multiplo del Capitale Investito (Valore Finale / Totale Versato)",
      labels = scales::label_number(suffix = "x", decimal.mark = ",")
    )
  ) +
  
  labs(
    title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)",
    subtitle = paste0("Simulazione su ", n_simulazioni, " (estrazione casuale rendimento mensile dal dataset) | Orizzonte: ", orizzonte_mesi/12, " anni\n",
                      "Valore Portafoglio negli scenari: Rosso = 10° percentile (Pessimo) | Arancio = Mediana | Verde = 90° Percentile (Ottimo)"),
    x = "Controvalore Finale Portafoglio (€)",
    y = "Frequenza (Numero di Scenari)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    panel.grid.minor = element_blank(),
    axis.title.x.top = element_text(color = "blue4", size = 10, face = "italic")
  )

