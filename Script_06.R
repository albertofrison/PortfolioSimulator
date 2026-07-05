# ==============================================================================
# 12. HEATMAP CORRELAZIONE PROFESSIONALE (PALETTE DIVERGENTE & % TESTUALE)
# ==============================================================================
library(tidyverse)
library(ggplot2)

# 1. Calcolo della matrice di correlazione di Pearson (completa)
matrice_correlazione <- cor(dataset_rendimenti %>% select(-date), method = "pearson")

# 2. Trasformiamo la matrice in formato "long"
df_correlazione_long <- as.data.frame(matrice_correlazione) %>%
  rownames_to_column(var = "ETF_Var1") %>%
  pivot_longer(cols = -ETF_Var1, names_to = "ETF_Var2", values_to = "Correlazione")

# Assicuriamo l'ordine simmetrico degli assi
df_correlazione_long$ETF_Var1 <- factor(df_correlazione_long$ETF_Var1, levels = colnames(matrice_correlazione))
df_correlazione_long$ETF_Var2 <- factor(df_correlazione_long$ETF_Var2, levels = rev(colnames(matrice_correlazione)))

# ==============================================================================
# 13. GENERAZIONE DEL GRAFICO CON PALETTE DOTTRINALE (RdBu)
# ==============================================================================
ggplot(df_correlazione_long, aes(x = ETF_Var1, y = ETF_Var2, fill = Correlazione)) +
  # Quadrati con linee di separazione sottili e neutre
  geom_tile(color = "white", linewidth = 0.3) +
  
  # Valori inseriti in formato percentuale (es. "88%") con colore dinamico per la leggibilità
  geom_text(aes(
    label = paste0(round(Correlazione * 100), "%"),
    color = abs(Correlazione) > 0.5 # Se la correlazione è forte, usa testo bianco, altrimenti scuro
  ), size = 3, fontface = "bold", show.legend = FALSE) +
  
  # Scala di colore del testo dinamica (Bianco sui colori scuri, Grigio antracite sui chiari)
  scale_color_manual(values = c("gray20", "white")) +
  
  # Scala cromatica divergente ufficiale (Red-Yellow-Blue desaturata e bilanciata)
  scale_fill_distiller(
    palette = "RdYlBu", 
    limits = c(-1, 1),
    direction = 1, # Mantiene il Blu sul positivo e il Rosso sul negativo
    name = "Correlazione",
    labels = c("-100%", "-50%", "0%", "50%", "100%")
  ) +
  
  labs(
    title = "Matrice di Correlazione dei Rendimenti Mensili",
    subtitle = "Palette divergente standard: Blu = Correlazione Positiva | Bianco/Giallo = Incorrelati | Rosso = Correlazione Inversa",
    x = NULL,
    y = NULL,
    caption = "Made with love in R by Alberto Frison"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", color = "gray20"),
    axis.text.y = element_text(face = "bold", color = "gray20"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 10)),
    plot.caption = element_text(face = "italic", color = "gray40", size = 9, margin = margin(t = 15)),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8)
  )
