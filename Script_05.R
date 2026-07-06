# ==============================================================================
# 10. PLOT AVANZATO MONTE CARLO CON TEXT BOX DELLE METRICHE E CAPTION
# ==============================================================================
if (!require("FinCal")) install.packages("FinCal") # Per il calcolo rigoroso dell'IRR/CAGR del PAC
library(FinCal)
library(ggplot2)
library(tidyverse)

# 1. Calcolo della Probabilità di perdita (scenari sotto il capitale investito)
prob_perdita <- mean(df_scenari$Valore_Finale < capitale_investito_totale) * 100

# 2. Calcolo del CAGR (IRR annualizzato) per lo scenario mediano
# Creiamo il vettore dei flussi di cassa del PAC mediano: 
# Mese 0: -Capitale Iniziale | Mesi 1-120: -1000€ | Fine Mese 120: +Valore Mediano
valore_mediano <- p50
flussi_cassa <- c(-sum(portafoglio_iniziale), rep(-quota_mensile, orizzonte_mesi))
flussi_cassa[length(flussi_cassa)] <- flussi_cassa[length(flussi_cassa)] + valore_mediano

# Calcolo tasso mensile e poi annualizzazione (CAGR equivalente per flussi periodici)
tasso_mensile <- irr(flussi_cassa)
cagr_mediano <- ((1 + tasso_mensile)^12 - 1) * 100

# ==============================================================================
# 3. AGGIORNAMENTO DELLA TEXT BOX (CON TOTALE INVESTITO E DISCLAIMER TAX)
# ==============================================================================
testo_box <- paste0(
  "💰 INVESTIMENTO COMPLESSIVO:\n",
  "• Totale Versato: ", format(round(capitale_investito_totale), big.mark = ".", decimal.mark = ","), " €\n\n",
  "📊 METRICHE CASO MEDIANO (50%):\n",
  "• Valore Finale: ", format(round(valore_mediano), big.mark = ".", decimal.mark = ","), " €\n",
  "• Rendimento Annuo (CAGR): ", sprintf("%.2f%%", cagr_mediano), " (pre-tax)\n\n",
  "⚠️ RISCHIO DEL PORTAFOGLIO:\n",
  "• Prob. sotto Capitale Investito: ", sprintf("%.2f%%", prob_perdita)
)

# ==============================================================================
# 4. GENERAZIONE DEL GRAFICO FINALE CON LAYOUT AGGIORNATO
# ==============================================================================
# Generazione del grafico finale ottimizzato
ggplot(df_scenari, aes(x = Valore_Finale)) +
  # Istogramma delle frequenze
  geom_histogram(bins = 40, fill = "cadetblue4", color = "white", alpha = 0.8) +
  
  # Linea di Break-even (Capitale Investito)
  geom_vline(xintercept = capitale_investito_totale, color = "black", linetype = "solid", linewidth = 1) +
  
  # Scritta verticale del Capitale Investito corretta con annotate() per evitare duplicazioni
  annotate("text", x = capitale_investito_totale, y = 0, label = "Capitale Investito", 
           angle = 90, vjust = -1, hjust = -0.5, color = "black", size = 3.5, fontface = "bold") +
  
  # Linee verticali dei percentili
  geom_vline(xintercept = c(p10, p50, p90), color = c("firebrick3", "orange", "springgreen4"), 
             linetype = "dashed", linewidth = 0.8) +
  
  # Inserimento della Text Box dinamica in alto a destra (corretto senza label.size)
  annotate("label", x = Inf, y = Inf, label = testo_box, 
           vjust = 1.1, hjust = 1.1, fill = "gray98", color = "gray15", 
           size = 3.8, fontface = "bold", label.padding = unit(0.6, "lines")) +
  
  # Assi cartesiani specchiati (€ / Multiplo) - aggiornato con transform
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      transform = ~ . / capitale_investito_totale,
      name = "Multiplo del Capitale Investito (Valore Finale / Totale Versato)",
      labels = scales::label_number(suffix = "x", decimal.mark = ",")
    )
  ) +
  
  # Titoli e Caption personalizzata firmata
  labs(
    title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)",
    subtitle = paste0("Simulazione su ", n_simulazioni, " scenari stocastici (Bootstrap) | Orizzonte: ", orizzonte_mesi/12, " anni\n",
                      "Linee tratteggiate: Rosso = 10% (Pessimo) | Arancio = 50% (Mediano) | Verde = 90% (Ottimo)"),
    x = "Controvalore Finale Portafoglio (€)",
    y = "Frequenza (Numero di Scenari)",
    caption = "Made in R and with love by Alberto Frison - Source data Yahoo Finance"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    plot.caption = element_text(face = "italic", color = "gray40", size = 9, margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    axis.title.x.top = element_text(color = "blue4", size = 10, face = "italic")
  )

