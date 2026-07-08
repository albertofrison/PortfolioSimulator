# ==============================================================================
# CONFIGURAZIONE GLOBALE (Colori, Testi e Impostazioni)
# ==============================================================================

# --- Colori del Grafico ---
COLOR_HIST_FILL   <- "aquamarine2"     # Colore di riempimento delle barre dell'istogramma
COLOR_HIST_BORDER <- "azure3"          # Colore del bordo delle barre e dei pannelli
COLOR_LINE_MEAN   <- "red"             # Linea della Media (μ)
COLOR_LINE_SD     <- "deepskyblue3"    # Linee della Deviazione Standard (σ)
COLOR_STRIP_BG    <- "azure2"          # Sfondo dei titoli dei pannelli (facet)
COLOR_TEXT_MAIN   <- "black"           # Colore principale del testo
COLOR_AC_POINTS     <- "deepskyblue4"    # Colore dei punti della nuvola
COLOR_AC_LINE       <- "red"             # Colore della linea di tendenza lineare

# --- Testi ed Etichette ---
TXT_PLOT_TITLE    <- "Distribuzione dei Rendimenti Mensili Storici (in EUR)"
TXT_PLOT_SUBTITLE <- "Linea rossa: Media (μ) | Linee azzurre: Media +/- Deviazione Standard (σ)"
TXT_PLOT_X_LABEL  <- "Rendimento Mensile"
TXT_PLOT_Y_LABEL  <- "Frequenza (Mesi)"
TXT_PLOT_CAPTION  <- "Made in R and with love by Alberto Frison - Source data Yahoo Finance"
TXT_AC_TITLE      <- "Analisi di Autocorrelazione dei Rendimenti (Lag 1)"
TXT_AC_SUBTITLE   <- "Ogni punto rappresenta un mese: Rendimento Originale vs Mese Spostato"
TXT_AC_X_LABEL    <- "Rendimenti Mensili Originali (X)"
TXT_AC_Y_LABEL    <- "Rendimenti Mensili Spostati (Y)"



# ==============================================================================
# DOWNLOAD E CONVERSIONE IN EUR DEI PROXY STORICI (SENZA BUCHI FESTIVI)
# ==============================================================================
if (!require("quantmod")) install.packages("quantmod")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cowplot")) install.packages("cowplot")
library(ggplot2)
library(tidyverse)
library(cowplot)
library(quantmod)


#0. Pulizia ambiente
rm(list=ls())

# Ticker storici americani e tasso di cambio per massima profondità
tickers_storici <- c(
  "SP500"           = "SPY",        # SPDR S&P 500 ETF Trust
  "Bond_Global"     = "AGG",        # iShares Core U.S. Aggregate Bond
  "Emerging_Markets"= "EEM",        # iShares MSCI Emerging Markets
  "World_Small_Cap" = "IWM",        # iShares Russell 2000
  "Gold"            = "GLD",        # SPDR Gold Shares
  "World_Value"     = "IVE",        # iShares S&P 500 Value
  "World_Momentum"  = "PDP",        # Invesco DWA Momentum
  "World_Equal_W"   = "RSP",        # Invesco S&P 500 Equal Weight
  "World_Mid_Cap"   = "IJH",        # iShares Core S&P Mid-Cap
  "World_ex_USA"    = "CWI"         # SPDR MSCI ACWI ex-US
)

# 1. Scarichiamo il tasso di cambio EUR/USD e portiamolo a livello mensile
cat("Download tasso di cambio EUR/USD...\n")
cambio_raw <- getSymbols("EURUSD=X", src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
prezzo_cambio_mensile <- Cl(to.monthly(cambio_raw, indexAt = "lastof"))

lista_rendimenti_eur <- list()

# 2. Ciclo di download, conversione valutaria e calcolo rendimento mensile
for (asset_name in names(tickers_storici)) {
  ticker <- tickers_storici[asset_name]
  cat("Elaborazione di:", asset_name, "... ")
  
  tryCatch({
    # Download dati giornalieri
    dati_giornalieri <- getSymbols(ticker, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    
    # Conversione in dati mensili (prende l'ultimo prezzo disponibile del mese, addio NA festivi)
    prezzi_mensili_usd <- Cl(to.monthly(dati_giornalieri, indexAt = "lastof"))
    
    # Allineamento con il cambio del mese per convertire il prezzo in EUR
    dati_comuni <- merge(prezzi_mensili_usd, prezzo_cambio_mensile, all = FALSE)
    
    # Prezzo in EUR = Prezzo in USD / Tasso di Cambio (es. 100$ / 1.10 = 90.90€)
    prezzi_mensili_eur <- dati_comuni[,1] / dati_comuni[,2]
    
    # Calcolo del rendimento mensile sul prezzo in Euro
    rend_mensile <- ROC(prezzi_mensili_eur, type = "discrete")
    colnames(rend_mensile) <- asset_name
    
    lista_rendimenti_eur[[asset_name]] <- rend_mensile
    cat("OK!\n")
  }, error = function(e) { cat("⚠️ ERRORE\n") })
}

# 3. Unione finale di tutti i rendimenti in EUR
cat("\nAllineamento finale di tutte le serie storiche...\n")
rendimenti_finali_xts <- do.call(merge, lista_rendimenti_eur)

# Convertiamo in tibble per l'algoritmo del PAC
dataset_rendimenti <- as.data.frame(rendimenti_finali_xts) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = as.Date(date)) %>%
  drop_na() # Rimuove solo la riga iniziale del calcolo dei rendimenti

cat("\nDatabase pronto senza buchi festivi!\n")
print(range(dataset_rendimenti$date))
print(head(dataset_rendimenti))

# ==============================================================================
# ANALISI DI AUTOCORRELAZIONE E COSTRUZIONE GRAFICO
# ==============================================================================
# 1. Trasformazione dei dati: creazione del Lag 1 per ciascun asset
dati_autocorr_long <- dataset_rendimenti %>%
  # Portiamo il dataset in formato lungo per lavorare su tutti gli ETF contemporaneamente
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento_X") %>%
  group_by(Asset) %>%
  # Creiamo la colonna Y spostata di 1 mese (il lag inserisce un NA sulla prima riga di ogni asset)
  mutate(Rendimento_Y = lag(Rendimento_X, n = 1)) %>%
  ungroup() %>%
  # Rimuoviamo automaticamente le righe che contengono NA (la prima riga di ogni serie)
  drop_na(Rendimento_Y)

# 2. Calcolo del coefficiente di correlazione e creazione delle etichette per i facet
metriche_autocorr <- dati_autocorr_long %>%
  group_by(Asset) %>%
  summarise(Correlazione = cor(Rendimento_X, Rendimento_Y)) %>%
  ungroup() %>%
  mutate(
    Asset_Label_AC = paste0(
      Asset, "\n",
      "Corr (r): ", sprintf("%.4f", Correlazione)
    )
  )

# Uniamo le etichette calcolate al dataset lungo
dati_grafico_ac <- dati_autocorr_long %>%
  inner_join(metriche_autocorr, by = "Asset")

# 3. Creazione dello Scatter Plot con stile uniformato
p_autocorr <- ggplot(dati_grafico_ac, aes(x = Rendimento_X, y = Rendimento_Y)) +
  # Nuvola di punti (usiamo le variabili globali)
  geom_point(color = COLOR_AC_POINTS, alpha = 0.5, size = 1.5) +
  
  # Aggiungiamo una linea di tendenza lineare (opzionale, ma aiuta visivamente a capire la correlazione)
  geom_smooth(method = "lm", color = COLOR_AC_LINE, se = FALSE, linewidth = 0.8, linetype = "dashed") +
  
  # Dividiamo in facet usando le nuove etichette con la correlazione nel titolo
  facet_wrap(~ Asset_Label_AC, scales = "free", ncol = 3) +
  
  # Formattiamo entrambi gli assi in percentuale
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Testi presi dalla configurazione globale
  labs(
    title    = TXT_AC_TITLE,
    subtitle = TXT_AC_SUBTITLE,
    x        = TXT_AC_X_LABEL,
    y        = TXT_AC_Y_LABEL,
    caption  = TXT_PLOT_CAPTION # Ricicliamo il vecchio caption aziendale/personale
  ) +
  
  # Applichiamo lo stesso tema grafico per uniformità visiva
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = COLOR_STRIP_BG, color = COLOR_HIST_BORDER),
    strip.text       = element_text(face = "bold", color = COLOR_TEXT_MAIN, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = COLOR_TEXT_MAIN, margin = margin(b = 15))
  )

# 4. Mostra il grafico finale di autocorrelazione
ggdraw(p_autocorr)


# ==============================================================================
# 9. CREAZIONE TABELLA TEST STATISTICI DI AUTOCORRELAZIONE
# ==============================================================================
library(tidyverse)

cat("Calcolo dei test di significatività per tutti gli ETF...\n")

tabella_test_autocorr <- dati_autocorr_long %>%
  group_by(Asset) %>%
  summarise(
    # 1. Calcoliamo la correlazione (r)
    Correlazione = cor(Rendimento_X, Rendimento_Y),
    
    # 2. Estraiamo il p-value dal test statistico cor.test()
    P_Value = cor.test(Rendimento_X, Rendimento_Y)$p.value,
    
    # 3. Creiamo un flag visivo per capire se il segnale è valido (Sotto la soglia dello 0.05)
    Significativo = if_else(P_Value < 0.05, "SI", "NO")
  ) %>%
  ungroup() %>%
  # 4. Ordiniamo per p-value crescente (i più significativi in alto)
  arrange(P_Value)

# --- Visualizzazione della Tabella ---
# Formattiamo i numeri per renderli leggibili prima di stamparli
tabella_pulita <- tabella_test_autocorr %>%
  mutate(
    Correlazione = sprintf("%.4f", Correlazione),
    P_Value      = sprintf("%.5f", P_Value)
  )

print(as.data.frame(tabella_pulita))



# ==============================================================================
# ISTOGRAMMI PULITI CON TEXT BOX IN BASSO A DESTRA (VIA COWPLOT)
# ==============================================================================

# 1. Calcolo metriche ed etichette per i titoli dei facet
metriche_grafico <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  group_by(Asset) %>%
  summarise(Media = mean(Rendimento), DevStd = sd(Rendimento)) %>%
  ungroup() %>%
  mutate(
    Asset_Label = paste0(
      Asset, "\n",
      "μ: ", sprintf("%.2f%%", Media * 100), " | ",
      "σ: ", sprintf("%.2f%%", DevStd * 100)
    )
  )

dati_long_con_etichette <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  inner_join(metriche_grafico, by = "Asset")

# 2. Creazione del grafico principale (solo i 10 facet degli ETF)
chart_01 <- ggplot(dati_long_con_etichette, aes(x = Rendimento)) +
  # Usiamo le variabili per il riempimento e il bordo delle barre
  geom_histogram(bins = 30, fill = COLOR_HIST_FILL, color = COLOR_HIST_BORDER, alpha = 0.8) +
  
  # Usiamo la variabile per la linea della media
  geom_vline(data = metriche_grafico, aes(xintercept = Media),
             color = COLOR_LINE_MEAN, linetype = "solid", linewidth = 0.8) +
  
  # Usiamo la variabile per le linee della deviazione standard
  geom_vline(data = metriche_grafico, aes(xintercept = Media + DevStd),
             color = COLOR_LINE_SD, linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = metriche_grafico, aes(xintercept = Media - DevStd),
             color = COLOR_LINE_SD, linetype = "dashed", linewidth = 0.8) +
  
  facet_wrap(~ Asset_Label, scales = "free_y", ncol = 3) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Usiamo le variabili globali per tutti i testi del grafico
  labs(
    title    = TXT_PLOT_TITLE,
    subtitle = TXT_PLOT_SUBTITLE,
    x        = TXT_PLOT_X_LABEL,
    y        = TXT_PLOT_Y_LABEL,
    caption  = TXT_PLOT_CAPTION
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Integrazione delle variabili globali nel tema grafico
    strip.background = element_rect(fill = COLOR_STRIP_BG, color = COLOR_HIST_BORDER),
    strip.text       = element_text(face = "bold", color = COLOR_TEXT_MAIN, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = COLOR_TEXT_MAIN, margin = margin(b = 15))
  )

ggdraw(chart_01)
