# ==============================================================================
# 14. GRAFICO DEL RENDIMENTO CUMULATIVO AD INDICE (BASE 100)
# ==============================================================================
library(tidyverse)
library(ggplot2)

# 1. Ricostruzione della serie storica ad indice (Base 100)
df_cumulativo_long <- dataset_rendimenti %>%
  arrange(date) %>% # Assicuriamo l'ordine cronologico
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  group_by(Asset) %>%
  mutate(
    # Calcolo del rendimento cumulativo: (1 + r1) * (1 + r2) * ... * 100
    Valore_Indice = cumprod(1 + Rendimento) * 100
  ) %>%
  ungroup()

# 2. Creazione dell'etichetta finale per ogni facet (Nome ETF + Performance Totale)
performance_totale <- df_cumulativo_long %>%
  group_by(Asset) %>%
  summarise(
    Valore_Finale = last(Valore_Indice),
    Perf_Totale   = (Valore_Finale - 100),
    
    # 1. Calcoliamo i d_anni convertendo la differenza tra Date in numero
    Anni_Storici  = as.numeric(max(date) - min(date)) / 365.25,
    
    # 2. Calcoliamo il CAGR specifico di questo Asset usando il suo Valore_Finale
    CAGR          = ((Valore_Finale / 100)^(1 / Anni_Storici) - 1) * 100
  ) %>%
  mutate(
    # Etichetta pulita con Nome, Performance Totale e CAGR Annuo
    Asset_Label = paste0(
      Asset, 
      "\nTot: ", sprintf("%+.1f%%", Perf_Totale), 
      " | CAGR: ", sprintf("%.2f%%", CAGR)
    )
  )

# Unione delle etichette descrittive al dataset da plottare
df_plot_cumulativo <- df_cumulativo_long %>%
  inner_join(performance_totale, by = "Asset")

# 3. Generazione del grafico con facet_wrap()
p_cumulativo <- ggplot(df_plot_cumulativo, aes(x = date, y = Valore_Indice, group = Asset)) +
  # Linea di crescita dell'indice
  geom_line(color = "azure3", linewidth = 0.8) +
  
  # Area sfumata sottostante per dare profondità visiva
  geom_area(fill = "aquamarine2", alpha = 0.15) +
  
  # Linea orizzontale di riferimento a Base 100 (Punto di partenza)
  geom_hline(yintercept = 100, color = "orange", linetype = "dashed", linewidth = 0.5) +
  
  # Griglia separata per ogni ETF
  facet_wrap(~ Asset_Label, scales = "free_y", ncol = 3) +
  
  # Formattazione degli assi
  scale_y_continuous(labels = scales::label_number(suffix = " €", big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  
  labs(
    title = "Crescita Storica Cumulativa degli Asset (Base 100 al Mese 1)",
    subtitle = "Evoluzione di un capitale teorico iniziale di 100 € applicando i rendimenti mensili reali (in EUR)",
    x = "Anno",
    y = "Valore dell'Indice (€)",
    caption = "Made in R and with love by Alberto Frison - Source data Yahoo Finance"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "azure2", color = "azure3"),
    strip.text = element_text(face = "bold", color = "black", size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Visualizza il grafico a schermo
print(p_cumulativo)
  