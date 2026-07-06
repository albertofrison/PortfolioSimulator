# ==============================================================================
# 8. SIMULATORE MONTE CARLO E GRAFICO DELLA DISTRIBUZIONE FINALE
# ==============================================================================
library(tidyverse)
library(ggplot2)

# 1. Configurazione del Portafoglio Reale Attuale (Base ~30k €)
somma_iniziale <- 30000

portafoglio_iniziale <- target_weights * somma_iniziale
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

# ==============================================================================
# CALCOLO METRICHE E CAGR (FORMULA RICHIESTA)
# ==============================================================================
n_anni <- orizzonte_mesi / 12
capitale_iniziale_assoluto <- sum(portafoglio_iniziale)

# ==============================================================================
# CALCOLO RIGOROSO DEL CAGR PER UN PAC (IRR PERIODICO ANNUALIZZATO)
# ==============================================================================
if (!require("FinCal")) install.packages("FinCal")
library(FinCal)

# 1. Prepariamo la struttura dei flussi di cassa reali del tuo PAC:
# Mese 0: Esce il Capitale Iniziale attuale (~30k)
# Mesi da 1 a 120: Escono 1.000 € al mese come quota PAC
flussi_base <- c(-sum(portafoglio_iniziale), rep(-quota_mensile, orizzonte_mesi))

# 2. Funzione per calcolare il CAGR reale inserendo il Valore Finale dello scenario
calcola_cagr_pac <- function(valore_finale) {
  flussi_scenario <- flussi_base
  # All'ultimo mese sommiamo il controvalore finale liquidato (segno positivo)
  flussi_scenario[length(flussi_scenario)] <- flussi_scenario[length(flussi_scenario)] + valore_finale
  
  # Calcolo del tasso interno di rendimento mensile
  tasso_mensile <- FinCal::irr(flussi_scenario)
  
  # Annualizzazione del tasso (CAGR reale composto)
  cagr_annuo <- ((1 + tasso_mensile)^12 - 1) * 100
  return(cagr_annuo)
}

# 3. Ricalcolo corretto dei CAGR per i tre percentili
cagr_p10 <- calcola_cagr_pac(p10)
cagr_p50 <- calcola_cagr_pac(p50)
cagr_p90 <- calcola_cagr_pac(p90)

# ==============================================================================
# AGGIORNAMENTO ETICHETTE DINAMICHE PER IL GRAFICO
# ==============================================================================
fmt_euro <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

lbl_cap_investito <- paste0("Capitale Investito: ", fmt_euro(capitale_investito_totale), " €")
lbl_pessimo       <- paste0("Pessimo (10%): ", fmt_euro(p10), " € (CAGR: ", sprintf("%.2f%%", cagr_p10), ")")
lbl_mediano       <- paste0("Mediano (50%): ", fmt_euro(p50), " € (CAGR: ", sprintf("%.2f%%", cagr_p50), ")")
lbl_ottimo        <- paste0("Ottimo (90%): ", fmt_euro(p90), " € (CAGR: ", sprintf("%.2f%%", cagr_p90), ")")










# ==============================================================================
# GRAFICO MONTE CARLO AGGIORNATO CON ETICHETTE DINAMICHE SULLE LINEE
# ==============================================================================
p_montecarlo <- ggplot(df_scenari, aes(x = Valore_Finale)) +
  # Istogramma delle frequenze dei portafogli finali
  geom_histogram(bins = 40, fill = "aquamarine2", color = "azure3", alpha = 0.8) +
  
  # Linea verticale del Capitale Puramente Investito (Break-even)
  geom_vline(xintercept = capitale_investito_totale, color = "black", linetype = "solid", linewidth = 1) +
  
  # Etichetta Capitale Investito
  annotate("text", x = capitale_investito_totale, y = Inf, label = lbl_cap_investito, 
           angle = 90, vjust = -1, hjust = 1.1, color = "black", size = 3.2, fontface = "bold") +
  
  # Linee verticali dei percentili (Pessimo, Mediano, Ottimo)
  geom_vline(xintercept = c(p10, p50, p90), color = c("red", "orange", "green4"), 
             linetype = "dashed", linewidth = 0.8) +
  
  # Etichette dinamiche con Valore e CAGR agganciate in cima alle linee verticali
  annotate("text", x = p10, y = Inf, label = lbl_pessimo, 
           angle = 90, vjust = -1, hjust = 1.1, color = "red", size = 3.2, fontface = "bold") +
  
  annotate("text", x = p50, y = Inf, label = lbl_mediano, 
           angle = 90, vjust = -1, hjust = 1.1, color = "orange", size = 3.2, fontface = "bold") +
  
  annotate("text", x = p90, y = Inf, label = lbl_ottimo, 
           angle = 90, vjust = -1, hjust = 1.1, color = "green4", size = 3.2, fontface = "bold") +
  
  # Asse X primario (Valore in Euro) e asse X secondario aggiornato con transform
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      transform = ~ . / capitale_investito_totale,
      name = "Multiplo del Capitale Investito (Valore Finale / Totale Versato)",
      labels = scales::label_number(suffix = "x", decimal.mark = ",")
    )
  ) +
  
  labs(
    title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)",
    subtitle = paste0("Simulazione su ", n_simulazioni, " (estrazione casuale rendimento mensile dal dataset) | Orizzonte: ", n_anni, " anni\n",
                      "Analisi dei percentili storici proiettati con piano di accumulo costante | Rendimenti al lordo della tassazione (pre-tax)"),
    x = "Controvalore Finale Portafoglio (€)",
    y = "Frequenza (Numero di Scenari)",
    caption = "Made in R and with love by Alberto Frison - Source data Yahoo Finance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "azure2", color = "azure3"),
    strip.text = element_text(face = "bold", color = "black", size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "black", margin = margin(b = 15)),
    axis.title.x.top = element_text(color = "blue4", size = 10, face = "italic")
  )

# Visualizza il grafico aggiornato a schermo
print(p_montecarlo)
