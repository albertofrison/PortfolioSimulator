# ==============================================================================
# 1. INIZIALIZZAZIONE LIBRERIE E CONFIGURAZIONE PARAMETRI GLOBALI
# ==============================================================================
library(ggplot2)
library(tidyverse)
library(cowplot)
library(quantmod)

# Reset completo dell'ambiente di lavoro (Rimozione variabili in memoria)
rm(list = ls())

# --- Configurazione Palette Cromatica dei Grafici ---
COLOR_HIST_FILL   <- "aquamarine2"     # Riempimento barre dell'istogramma
COLOR_HIST_BORDER <- "azure3"          # Bordo delle barre e dei pannelli di facet
COLOR_LINE_MEAN   <- "red"             # Identificatore grafico della Media (μ)
COLOR_LINE_SD     <- "deepskyblue3"    # Identificatore della Deviazione Standard (σ)
COLOR_STRIP_BG    <- "azure2"          # Sfondo dei titoli dei pannelli (Facet Strip)
COLOR_TEXT_MAIN   <- "black"           # Colore primario degli elementi testuali
COLOR_AC_POINTS   <- "deepskyblue4"    # Colore delle coordinate nello scatter plot
COLOR_AC_LINE     <- "red"             # Linea di regressione lineare locale

# --- Stringhe di Testo ed Etichette Grafiche ---
TXT_PLOT_TITLE    <- "Distribuzione dei Rendimenti Mensili Storici (in EUR)"
TXT_PLOT_SUBTITLE <- "Linea rossa: Media (μ) | Linee azzurre: Media +/- Deviazione Standard (σ)"
TXT_PLOT_X_LABEL  <- "Rendimento Mensile"
TXT_PLOT_Y_LABEL  <- "Frequenza (Mesi)"
TXT_PLOT_CAPTION  <- "Made in R - Source data: Yahoo Finance"

TXT_AC_TITLE      <- "Analisi di Autocorrelazione dei Rendimenti (Lag 1)"
TXT_AC_SUBTITLE   <- "Rappresentazione stocastica: Rendimento Periodo (t) vs Periodo (t-1)"
TXT_AC_X_LABEL    <- "Rendimenti Mensili Originali (X)"
TXT_AC_Y_LABEL    <- "Rendimenti Mensili Spostati (Y)"


# ==============================================================================
# 2. INGESTIONE DATI, CONVERSIONE VALUTARIA (USD/EUR) E CALCOLO RENDIMENTI
# ==============================================================================
# Definizione dei proxy storici (Ticker Yahoo Finance in USD)
tickers_storici <- c(
  "SP500"            = "SPY",        # SPDR S&P 500 ETF Trust
  "Bond_Global"      = "AGG",        # iShares Core U.S. Aggregate Bond
  "Emerging_Markets"= "EEM",        # iShares MSCI Emerging Markets
  "World_Small_Cap" = "IWM",        # iShares Russell 2000
  "Gold"            = "GLD",        # SPDR Gold Shares
  "World_Value"      = "IVE",        # iShares S&P 500 Value
  "World_Momentum"  = "PDP",        # Invesco DWA Momentum
  "World_Equal_W"   = "RSP",        # Invesco S&P 500 Equal Weight
  "World_Mid_Cap"   = "IJH",        # iShares Core S&P Mid-Cap
  "World_ex_USA"    = "CWI"         # SPDR MSCI ACWI ex-US
)

# Scaricamento della serie storica del tasso di cambio EUR/USD e campionamento mensile
cambio_raw <- getSymbols("EURUSD=X", src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
prezzo_cambio_mensile <- Cl(to.monthly(cambio_raw, indexAt = "lastof"))
lista_rendimenti_eur <- list()

# Ciclo di download sequenziale e normalizzazione valutaria
for (asset_name in names(tickers_storici)) {
  ticker <- tickers_storici[asset_name]
  cat("Download e conversione asset:", asset_name, "... ")
  
  tryCatch({
    # Download serie storica dei prezzi in valuta originale (USD)
    dati_giornalieri <- getSymbols(ticker, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    
    # Conversione a frequenza mensile (Rilevazione sull'ultimo giorno disponibile del mese)
    prezzi_mensili_usd <- Cl(to.monthly(dati_giornalieri, indexAt = "lastof"))
    
    # Intersezione delle serie storiche (Inner Join basato sull'indice temporale comune)
    dati_comuni <- merge(prezzi_mensili_usd, prezzo_cambio_mensile, all = FALSE)
    
    # Omogeneizzazione valutaria: Conversione matematica dei prezzi storici in EUR
    prezzi_mensili_eur <- dati_comuni[, 1] / dati_comuni[, 2]
    
    # Calcolo della variazione percentuale discreta (Arithmetic Returns)
    rend_mensile <- ROC(prezzi_mensili_eur, type = "discrete")
    colnames(rend_mensile) <- asset_name
    
    lista_rendimenti_eur[[asset_name]] <- rend_mensile
    cat("COMPLETATO\n")
  }, error = function(e) { 
    cat("⚠️ ERRORE CRITICO DI SCARICAMENTO\n") 
  })
}

# Unione delle serie xts individuali in un unico oggetto strutturato
rendimenti_finali_xts <- do.call(merge, lista_rendimenti_eur)

# Trasformazione in formato tibble ed eliminazione dei record privi di dati (NA)
dataset_rendimenti <- as.data.frame(rendimenti_finali_xts) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = as.Date(date)) %>%
  drop_na()

# Output di controllo di consistenza del dataset generato
print(range(dataset_rendimenti$date))
print(head(dataset_rendimenti))


# ==============================================================================
# 3. ANALISI STATISTICA DI AUTOCORRELAZIONE (LAG-1) E STRUTTURAZIONE DATI
# ==============================================================================
# Trasformazione del dataset in formato lungo e calcolo della variabile ritardata (Lag 1)
dati_autocorr_long <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento_X") %>%
  group_by(Asset) %>%
  mutate(Rendimento_Y = lag(Rendimento_X, n = 1)) %>%
  ungroup() %>%
  drop_na(Rendimento_Y)

# Calcolo del coefficiente di correlazione per la generazione dei titoli dinamici dei pannelli
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

# Join delle metriche calcolate con la struttura dati del grafico
dati_grafico_ac <- dati_autocorr_long %>%
  inner_join(metriche_autocorr, by = "Asset")


# ==============================================================================
# 4. PLOT SCATTER PLOT DI AUTOCORRELAZIONE
# ==============================================================================
p_autocorr <- ggplot(dati_grafico_ac, aes(x = Rendimento_X, y = Rendimento_Y)) +
  # Rappresentazione delle osservazioni stocastiche tramite nuvola di punti
  geom_point(color = COLOR_AC_POINTS, alpha = 0.5, size = 1.5) +
  
  # Interpolazione lineare grafica per evidenziare la tendenza del trend (Linea di tendenza)
  geom_smooth(method = "lm", color = COLOR_AC_LINE, se = FALSE, linewidth = 0.8, linetype = "dashed") +
  
  # Generazione dei pannelli condizionati basati sull'etichetta dell'asset
  facet_wrap(~ Asset_Label_AC, scales = "free", ncol = 3) +
  
  # Formattazione delle scale degli assi in notazione percentuale
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Iniezione dei parametri testuali globali
  labs(
    title    = TXT_AC_TITLE,
    subtitle = TXT_AC_SUBTITLE,
    x        = TXT_AC_X_LABEL,
    y        = TXT_AC_Y_LABEL,
    caption  = TXT_PLOT_CAPTION
  ) +
  
  # Definizione del layout grafico e dello stile assi
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = COLOR_STRIP_BG, color = COLOR_HIST_BORDER),
    strip.text       = element_text(face = "bold", color = COLOR_TEXT_MAIN, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = COLOR_TEXT_MAIN, margin = margin(b = 15))
  )

# Visualizzazione a schermo del grafico di autocorrelazione
ggdraw(p_autocorr)


# ==============================================================================
# 5. VERIFICA DELLE IPOTESI E TEST DI SIGNIFICATIVITÀ STATISTICA
# ==============================================================================
# Calcolo formale del P-Value associato all'indipendenza lineare delle serie ritardate
tabella_test_autocorr <- dati_autocorr_long %>%
  group_by(Asset) %>%
  summarise(
    Correlazione = cor(Rendimento_X, Rendimento_Y),
    P_Value      = cor.test(Rendimento_X, Rendimento_Y)$p.value,
    Significativo = if_else(P_Value < 0.05, "SI", "NO") # Rifiuto dell'ipotesi nulla ad alfa = 5%
  ) %>%
  ungroup() %>%
  arrange(P_Value)

# Formattazione stringhe numeriche per l'output finale a console
tabella_pulita <- tabella_test_autocorr %>%
  mutate(
    Correlazione = sprintf("%.4f", Correlazione),
    P_Value      = sprintf("%.5f", P_Value)
  )

print(as.data.frame(tabella_pulita))


# ==============================================================================
# 6. CALCOLO METRICHE DESCRITTIVE ED ELABORAZIONE GRAFICO DISTRIBUZIONE
# ==============================================================================
# Generazione delle metriche descrittive (Media campionaria e Deviazione Standard) per pannello
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

# Associazione delle etichette descrittive calcolate al dataset in formato long
dati_long_con_etichette <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  inner_join(metriche_grafico, by = "Asset")


# ==============================================================================
# 7. PLOT ISTOGRAMMA SIMULAZIONE FREQUENZE RENDIMENTI
# ==============================================================================
chart_01 <- ggplot(dati_long_con_etichette, aes(x = Rendimento)) +
  # Costruzione delle barre di frequenza empirica
  geom_histogram(bins = 30, fill = COLOR_HIST_FILL, color = COLOR_HIST_BORDER, alpha = 0.8) +
  
  # Asse verticale di riferimento centrato sulla Media Campionaria (μ)
  geom_vline(data = metriche_grafico, aes(xintercept = Media),
             color = COLOR_LINE_MEAN, linetype = "solid", linewidth = 0.8) +
  
  # Assi verticali simmetrici basati sulle soglie di dispersione della Deviazione Standard (σ)
  geom_vline(data = metriche_grafico, aes(xintercept = Media + DevStd),
             color = COLOR_LINE_SD, linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = metriche_grafico, aes(xintercept = Media - DevStd),
             color = COLOR_LINE_SD, linetype = "dashed", linewidth = 0.8) +
  
  # Strutturazione dei pannelli condizionati ad asse Y indipendente
  facet_wrap(~ Asset_Label, scales = "free_y", ncol = 3) +
  
  # Formattazione asse x e testi descrittivi globali
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = TXT_PLOT_TITLE,
    subtitle = TXT_PLOT_SUBTITLE,
    x        = TXT_PLOT_X_LABEL,
    y        = TXT_PLOT_Y_LABEL,
    caption  = TXT_PLOT_CAPTION
  ) +
  
  # Applicazione e sintonizzazione fine degli elementi del tema grafico
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = COLOR_STRIP_BG, color = COLOR_HIST_BORDER),
    strip.text       = element_text(face = "bold", color = COLOR_TEXT_MAIN, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = COLOR_TEXT_MAIN, margin = margin(b = 15))
  )

# Visualizzazione a schermo dell'istogramma finale delle distribuzioni
ggdraw(chart_01)

