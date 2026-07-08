# ==============================================================================
# LIBRERIE REQUISITE (Assicurarsi che siano caricate nell'ambiente)
# ==============================================================================
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# ==============================================================================
# 1. ELABORAZIONE STATISTICA E CALCOLO DELLA MATRICE DI CORRELAZIONE
# ==============================================================================
# Esclusione della colonna temporale e calcolo dei coefficienti di Pearson
matrice_correlazione <- cor(dataset_rendimenti %>% select(-date), method = "pearson")


# ==============================================================================
# 2. RISTRUTTURAZIONE DATI IN FORMATO LONG (TIDY DATA)
# ==============================================================================
# Conversione della matrice in data frame e indicizzazione delle variabili
df_correlazione_long <- as.data.frame(matrice_correlazione) %>%
  rownames_to_column(var = "ETF_Var1") %>%
  pivot_longer(cols = -ETF_Var1, names_to = "ETF_Var2", values_to = "Correlazione")

# Definizione dei livelli dei fattori per garantire la simmetria geometrica degli assi
df_correlazione_long$ETF_Var1 <- factor(df_correlazione_long$ETF_Var1, levels = colnames(matrice_correlazione))
df_correlazione_long$ETF_Var2 <- factor(df_correlazione_long$ETF_Var2, levels = rev(colnames(matrice_correlazione)))


# ==============================================================================
# 3. PLOT HEATMAP CORRELAZIONE PROFESSIONALE
# ==============================================================================
p_corr <- ggplot(df_correlazione_long, aes(x = ETF_Var1, y = ETF_Var2, fill = Correlazione)) +
  
  # Rendering della griglia (quadrati con linee di separazione nette)
  geom_tile(color = "white", linewidth = 0.3) +
  
  # Inserimento dei valori percentuali con soglia di contrasto dinamica per il font
  geom_text(aes(
    label = paste0(round(Correlazione * 100), "%"),
    color = abs(Correlazione) > 0.5 # TRUE = forte (testo bianco), FALSE = debole (testo scuro)
  ), size = 3, fontface = "bold", show.legend = FALSE) +
  
  # Mappatura manuale dei colori del testo (Grigio antracite per contrasto basso, Bianco per contrasto alto)
  scale_color_manual(values = c("gray20", "white")) +
  
  # Configurazione della scala cromatica divergente (RdYlBu ColorBrewer)
  scale_fill_distiller(
    palette = "RdYlBu", 
    limits = c(-1, 1),
    direction = 1, # Direzione standard: Blu (positivo), Bianco/Giallo (neutro), Rosso (negativo)
    name = "Correlazione",
    labels = c("-100%", "-50%", "0%", "50%", "100%")
  ) +
  
  # Definizione dei testi informativi e dei metadati del grafico
  labs(
    title = "Matrice di Correlazione dei Rendimenti Mensili",
    subtitle = "Palette divergente standard: Blu = Correlazione Positiva | Bianco/Giallo = Incorrelati | Rosso = Correlazione Inversa",
    x = NULL,
    y = NULL,
    caption = "Made in R with love - Source data Yahoo Finance"
  ) +
  
  # Impostazioni del layout e della tipografia del tema
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

# --- Output Grafico ---
print(p_corr)
