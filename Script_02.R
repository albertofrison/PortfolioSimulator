# ==============================================================================
# DOWNLOAD E CONVERSIONE IN EUR DEI PROXY STORICI (SENZA BUCHI FESTIVI)
# ==============================================================================
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)
library(tidyverse)

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
    dati_giornalieri <- getSymbols(ticker, src = "yahoo", from = "2000-01-01", auto.assign = FALSE)
    
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
# 7. ISTOGRAMMI PULITI CON TEXT BOX IN BASSO A DESTRA (VIA COWPLOT)
# ==============================================================================
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cowplot")) install.packages("cowplot")
library(ggplot2)
library(tidyverse)
library(cowplot)

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
p_grafico <- ggplot(dati_long_con_etichette, aes(x = Rendimento)) +
  geom_histogram(bins = 30, fill = "gray40", color = "white", alpha = 0.8) +
  
  geom_vline(data = metriche_grafico, aes(xintercept = Media),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  
  geom_vline(data = metriche_grafico, aes(xintercept = Media + DevStd),
             color = "deepskyblue3", linetype = "solid", linewidth = 0.5) +
  geom_vline(data = metriche_grafico, aes(xintercept = Media - DevStd),
             color = "deepskyblue3", linetype = "solid", linewidth = 0.5) +
  
  facet_wrap(~ Asset_Label, scales = "free_y", ncol = 3) +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribuzione dei Rendimenti Mensili Storici (in EUR)",
    subtitle = "Linea rossa: Media (μ) | Linee azzurre: Media +/- Deviazione Standard (σ)",
    x = "Rendimento Mensile",
    y = "Frequenza (Mesi)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold", color = "gray20", size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15))
  )

# 3. Testo dettagliato della legenda
testo_legenda <- paste0(
  "📋 MAPPA SOTTOSTANTI (PROXY USA):\n",
  "• Bond_Global: Aggregate Bond (AGG)        ",
  "• Emerging_Markets: MSCI EM (EEM)\n",
  "• Gold: Gold Shares (GLD)        ",
  "• SP500: S&P 500 ETF (SPY)\n",
  "• World_Equal_W: S&P 500 EW (RSP)        ",
  "• World_ex_USA: MSCI ACWI ex-US (CWI)\n",
  "• World_Mid_Cap: S&P Mid-Cap (IJH)        ",
  "• World_Momentum: DWA Momentum (PDP)\n",
  "• World_Small_Cap: Russell 2000 (IWM)        ",
  "• World_Value: S&P 500 Value (IVE)\n\n",
  "📌 Source: Yahoo Finance via quantmod        - ",
  "✍️ Made with love by Alberto Frison"
)

# 4. Sovrapposizione del box di testo nello slot vuoto (in basso a destra)
# Sfruttiamo draw_plot_label o draw_label posizionandola nel quadrante relativo
ggdraw(p_grafico) +
  draw_label(
    testo_legenda, 
    x = 0.72, y = 0.13,          # Coordinate relative allo schermo intero
    hjust = 0, vjust = 0.5,       # Allineamento a sinistra
    fontface = "bold", 
    size = 8, 
    color = "gray20",
    lineheight = 1.25
  )
