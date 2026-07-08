# ==============================================================================
# CONFIGURAZIONE PARAMETRI GLOBALI PER GRAFICO AD INDICE UNIFICATO
# ==============================================================================
# --- Configurazione Palette Cromatica ---
COLOR_IND_ASSETS  <- "gray50"          # Contrasto ottimizzato per gli asset storici
COLOR_IND_P10     <- "red"             # Rosso per Scenario Sfavorevole (P10)
COLOR_IND_P50     <- "blue"            # Blu per Scenario Mediano (P50)
COLOR_IND_P90     <- "green4"          # Verde per Scenario Favorevole (P90)
COLOR_IND_BASE100 <- "orange"          # Linea di benchmark iniziale (Base 100)

# --- Stringhe di Testo ed Etichette Grafiche ---
TXT_IND_TITLE     <- "Evoluzione Storica Unificata vs Traiettorie Monte Carlo"
TXT_IND_SUBTITLE  <- "Rendimenti puri a Base 100 (No PAC) con allineamento dinamico dei label nativi"
TXT_IND_X_LABEL   <- "Anno"
TXT_IND_Y_LABEL   <- "Valore dell'Indice (Base 100 iniziale)"
TXT_IND_CAPTION   <- "Made in R - Source data: Yahoo Finance | Algoritmo di spaziatura Y nativo applicato a data_finale"


# ==============================================================================
# 1. COSTRUZIONE INDICI A BASE 100 PER GLI ASSET REALI
# ==============================================================================
df_assets_base100 <- dataset_rendimenti %>%
  arrange(date) %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  group_by(Asset) %>%
  mutate(Valore_Indice = cumprod(1 + Rendimento) * 100) %>%
  ungroup()

LUNGHEZZA_STORICA_REALE <- nrow(dataset_rendimenti)
DATA_FINALE_STORICA     <- max(dataset_rendimenti$date)


# ==============================================================================
# 2. ESTRAZIONE E TRONCAMENTO DELLE TRAIETTORIE SIMULATE
# ==============================================================================
estrai_indice_simulato <- function(valore_percentile, nome_legenda) {
  indice_scenario    <- which.min(abs(risultati_scenari - valore_percentile))
  traiettoria        <- matrice_traiettorie[indice_scenario, ]
  valori_t_meno_1    <- traiettoria[1:orizzonte_mesi]
  valori_t           <- traiettoria[2:(orizzonte_mesi + 1)]
  rendimenti_mensili <- ((valori_t - quota_mensile) / valori_t_meno_1) - 1
  
  rendimenti_troncati <- rendimenti_mensili[1:LUNGHEZZA_STORICA_REALE]
  valore_indice       <- cumprod(1 + rendimenti_troncati) * 100
  
  return(tibble(
    date          = dataset_rendimenti$date,
    Asset         = nome_legenda, 
    Valore_Indice = valore_indice
  ))
}

df_p10_indice <- estrai_indice_simulato(p10, "PORTAFOGLIO P10")
df_p50_indice <- estrai_indice_simulato(p50, "PORTAFOGLIO P50")
df_p90_indice <- estrai_indice_simulato(p90, "PORTAFOGLIO P90")


# ==============================================================================
# 3. UNIFICAZIONE E ALGORITMO DI SPAZIATURA VERTICALE DEI LABEL (NATIVO)
# ==============================================================================
# Unione di tutte le serie storiche per elaborare i label in un unico blocco ordinato
df_tutti_indici <- bind_rows(df_assets_base100, df_p10_indice, df_p50_indice, df_p90_indice)

# Isolamento dei soli dati dell'ultimo mese disponibile
df_labels_complessivo <- df_tutti_indici %>%
  filter(date == DATA_FINALE_STORICA) %>%
  arrange(Valore_Indice)

# Algoritmo di scorrimento: impedisce a due etichette di avere una distanza Y inferiore alla soglia
SOGLIA_SPAZIO_Y <- 15.0  # Distanza minima in Euro/Punti indice per evitare sovrapposizioni di testo
griglia_y       <- df_labels_complessivo$Valore_Indice

if (length(griglia_y) > 1) {
  for (i in 2:length(griglia_y)) {
    if ((griglia_y[i] - griglia_y[i-1]) < SOGLIA_SPAZIO_Y) {
      griglia_y[i] <- griglia_y[i-1] + SOGLIA_SPAZIO_Y
    }
  }
}
# Assegnazione delle nuove coordinate Y corrette per il testo
df_labels_complessivo$Y_Aggiustato <- griglia_y

# --- Generazione della Palette Colori esplicita per i Label del testo ---
elenco_asset    <- unique(df_assets_base100$Asset)
palette_labels  <- rep(COLOR_IND_ASSETS, length(elenco_asset))
names(palette_labels) <- elenco_asset
palette_labels["PORTAFOGLIO P10"] <- COLOR_IND_P10
palette_labels["PORTAFOGLIO P50"] <- COLOR_IND_P50
palette_labels["PORTAFOGLIO P90"] <- COLOR_IND_P90


# ==============================================================================
# 4. RENDERING GRAFICO MULTI-LAYER CON SCALA CROMATICA MANUALE DI TESTO
# ==============================================================================
p_unificato_nativo <- ggplot() +
  
  # --- LAYER 1: Linea di origine orizzontale Base 100 ---
  geom_hline(yintercept = 100, color = COLOR_IND_BASE100, linetype = "dashed", linewidth = 0.6) +
  
  # --- LAYER 2: Linee Asset Storici Reali ---
  geom_line(data = df_assets_base100, 
            aes(x = date, y = Valore_Indice, group = Asset), 
            color = COLOR_IND_ASSETS, linewidth = 0.6, alpha = 0.4) +
  
  # --- LAYER 3: Linee Traiettorie Monte Carlo ---
  geom_line(data = df_p10_indice, aes(x = date, y = Valore_Indice), color = COLOR_IND_P10, linewidth = 1.3) +
  geom_line(data = df_p50_indice, aes(x = date, y = Valore_Indice), color = COLOR_IND_P50, linewidth = 1.3) +
  geom_line(data = df_p90_indice, aes(x = date, y = Valore_Indice), color = COLOR_IND_P90, linewidth = 1.3) +
  
  # --- LAYER 4: Rendering Etichette Unificate Spaziate Matematicamente ---
  geom_text(data = df_labels_complessivo, 
            aes(x = date, y = Y_Aggiustato, label = Asset, color = Asset),
            hjust = 0, nudge_x = 45, size = 2.6, fontface = "bold") +
  
  # --- Configurazione Assi e Layout ---
  scale_y_continuous(labels = scales::label_number(suffix = " €", big.mark = ".", decimal.mark = ",")) +
  
  scale_x_date(
    date_labels = "%Y", 
    date_breaks = "2 years",
    expand = expansion(mult = c(0.01, 0.25)) # Alloca lo spazio a destra per ospitare i testi
  ) +
  
  # Applicazione della palette colori isolata unicamente sul livello testuale
  scale_color_manual(values = palette_labels) +
  
  labs(
    title    = TXT_IND_TITLE,
    subtitle = TXT_IND_SUBTITLE,
    x        = TXT_IND_X_LABEL,
    y        = TXT_IND_Y_LABEL,
    caption  = TXT_IND_CAPTION
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = "gray30", margin = margin(b = 15)),
    plot.caption     = element_text(face = "italic", color = "gray40", size = 9, margin = margin(t = 15)),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "none" # Disattivazione totale delle legende automatiche
  )

# --- Output Grafico ---
print(p_unificato_nativo)