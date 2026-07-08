# ==============================================================================
# ETF_MONTECARLO_MASTER_PIPELINE.R
# Script unico e integrato per l'analisi quantitativa e simulazione stocastica
# ==============================================================================

# ==============================================================================
# 1. INIZIALIZZAZIONE, PARAMETRI GLOBALI E CONFIGURAZIONE PALETTE
# ==============================================================================

# --- Pulizia Totale dell'Ambiente di Lavoro (Prevenzione shock da variabili residue) ---
rm(list = ls(all.names = TRUE))
gc() # Garbage Collection forzata per liberare la RAM immediatamente

# --- Caricamento Librerie Requisite ---
library(tidyverse)
library(quantmod)
library(cowplot)
library(parallel)
library(foreach)
library(doParallel)
library(FinCal)

# --- CONFIGURAZIONE PARAMETRI FINANZIARI (L'Unica sorgente della verità) ---
PARAM_PIC_INIZIALE   <- 30000     # Capitale iniziale versato a inizio piano (€)
PARAM_PAC_MENSILE    <- 1000      # Contributo ricorrente mensile (€)
PARAM_ORIZZONTE_ANNI <- 20        # Durata complessiva della simulazione (Anni)
PARAM_ORIZZONTE_MESI <- PARAM_ORIZZONTE_ANNI * 12 # Calcolo automatico della timeline
PARAM_N_SIMULAZIONI  <- 100000    # Numero di traiettorie Monte Carlo generate (Bootstrap)

# --- CONFIGURAZIONE HARDWARE (Calcolo Parallelo) ---
CORES_DISPONIBILI <- parallel::detectCores()
# Salvaguardia di sistema: lasciamo sempre 2 core liberi per evitare il congelamento dell'OS
CORES_DA_USARE    <- max(1, CORES_DISPONIBILI - 2) 

# --- STANDARD CROMATICO UNIFICATO (Palette globale per coerenza visiva) ---
PAL_MAIN_TEXT     <- "gray10"       # Colore primario per i testi e titoli dominanti
PAL_GRID_LIGHT    <- "gray92"       # Linee di griglia secondarie sottili
PAL_STRIP_BG      <- "azure2"       # Sfondo dei titoli dei pannelli (Facet)
PAL_BORDER_LIGHT  <- "azure3"       # Bordi geometrici dei pannelli e delle barre
PAL_HIST_FILL     <- "aquamarine2"  # Riempimento degli istogrammi empirici storici
PAL_MC_FILL       <- "aquamarine3"  # Riempimento istogramma finale Monte Carlo

# --- Codifica Colori Scenari/Percentili (Fissata per tutti i grafici a valle) ---
PAL_LINE_BE       <- "black"        # Linea di Break-Even (Capitale nominale versato)
PAL_SCEN_P10      <- "red"          # Scenario Sfavorevole / Percentile 10%
PAL_SCEN_P50      <- "blue"         # Scenario Mediano / Percentile 50%
PAL_SCEN_P90      <- "green4"       # Scenario Favorevole / Percentile 90%

# --- Metadati e Stringhe di Testo Globali (Richiamate dai layer ggplot) ---
GLOBAL_CAPTION    <- "Sorgente Dati: Yahoo Finance | Author: Alberto Frison with R & Gemini"
GLOBAL_THEME_SIZE <- 11             # Dimensione base dei font del layout grafico

# ==============================================================================
# 2. INGESTIONE DATI, CONVERSIONE VALUTARIA (USD -> EUR) E PREPARAZIONE DATASET
# ==============================================================================

# --- Mappatura dei Proxy Storici (Ticker Yahoo Finance in USD) ---
TICKER_NAMES <- c(
  "Bond_Global"      = "AGG",        # iShares Core U.S. Aggregate Bond
  "World_ex_USA"    = "CWI",        # SPDR MSCI ACWI ex-US
  "World_Equal_W"   = "RSP",        # Invesco S&P 500 Equal Weight
  "SP500"            = "SPY",        # SPDR S&P 500 ETF Trust
  "Emerging_Markets"= "EEM",        # iShares MSCI Emerging Markets
  "World_Value"      = "IVE",        # iShares S&P 500 Value
  "World_Mid_Cap"   = "IJH",        # iShares Core S&P Mid-Cap
  "Gold"            = "GLD",        # SPDR Gold Shares
  "World_Momentum"  = "PDP",        # Invesco DWA Momentum
  "World_Small_Cap" = "IWM"         # iShares Russell 2000
)

# --- Definizione dei Pesi Target del Portafoglio Lazy (Asset Allocation) ---
PORTAFOGLIO_TARGET <- c(
  "Bond_Global"      = 0.1943, # Bond Aggregate Globali
  "World_ex_USA"    = 0.1748, # MSCI World Ex USA
  "World_Equal_W"   = 0.1282, # MSCI World Equal Weight
  "SP500"           = 0.1159, # S&P 500
  "Emerging_Markets"= 0.0829, # MSCI Mercati Emergenti
  "World_Value"      = 0.0739, # MSCI World Value
  "World_Mid_Cap"   = 0.0678, # MSCI World Mid Cap
  "Gold"            = 0.0627, # ETC Oro
  "World_Momentum"  = 0.0547, # MSCI World Momentum
  "World_Small_Cap" = 0.0448  # MSCI World Small Cap
)

# Controlli di sicurezza incrociati sui vettori
if (!all(names(TICKER_NAMES) %in% names(PORTAFOGLIO_TARGET)) || !all(names(PORTAFOGLIO_TARGET) %in% names(TICKER_NAMES))) {
  stop("ERRORE CRITICO: Disallineamento tra i nomi di TICKER_NAMES e PORTAFOGLIO_TARGET!")
}

if (abs(sum(PORTAFOGLIO_TARGET) - 1) > 1e-5) {
  stop("ERRORE CRITICO: La somma dei pesi del portafoglio target non è pari a 1!")
}

NOMI_ASSET <- names(TICKER_NAMES)

# Scaricamento cambio EUR/USD e isolamento immediato del solo prezzo di chiusura (1 colonna)
cat(">>> Scaricamento tasso di cambio EURUSD=X... ")
cambio_raw <- getSymbols("EURUSD=X", src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
prezzo_cambio_mensile <- Cl(xts::to.monthly(cambio_raw, indexAt = "lastof"))
lista_rendimenti_eur  <- list()
cat("FATTO.\n")

# Ciclo di download sequenziale degli asset e conversione matematica in Euro
for (asset_name in NOMI_ASSET) {
  ticker_yahoo <- TICKER_NAMES[asset_name]
  cat(">>> Download e conversione asset:", asset_name, " (Ticker Yahoo:", ticker_yahoo, ")... ")
  
  tryCatch({
    dati_giornalieri <- getSymbols(ticker_yahoo, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    
    # BUG FIX: Applichiamo Cl() per estrarre ESATTAMENTE E SOLO la colonna Close (Chiusura)
    prezzi_mensili_usd <- Cl(xts::to.monthly(dati_giornalieri, indexAt = "lastof"))
    
    # Ora l'unione produce una matrice fidata di sole 2 colonne:
    # Colonna 1: Prezzo di chiusura asset in USD
    # Colonna 2: Prezzo di chiusura cambio EURUSD=X
    dati_comuni <- merge(prezzi_mensili_usd, prezzo_cambio_mensile, all = FALSE)
    
    # Conversione matematica corretta ed impeccabile in EUR
    prezzi_mensili_eur <- dati_comuni[, 1] / dati_comuni[, 2]
    
    # Calcolo dei rendimenti aritmetici
    rend_mensile <- ROC(prezzi_mensili_eur, type = "discrete")
    colnames(rend_mensile) <- asset_name
    
    lista_rendimenti_eur[[asset_name]] <- rend_mensile
    cat("COMPLETATO.\n")
  }, error = function(e) { 
    cat("⚠️ ERRORE NELLO SCARICAMENTO DELL'ASSET:", asset_name, "\n") 
  })
}

# Consolidamento in formato strutturato delle serie storiche
rendimenti_finali_xts <- do.call(merge, lista_rendimenti_eur)

# Trasformazione finale in formato Tidy (Tibble) e rimozione paranoica dei record NA
dataset_rendimenti <- as.data.frame(rendimenti_finali_xts) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = as.Date(date)) %>%
  drop_na()

# Diagnostica finale di verifica di stabilità dei dati storici
cat("\n--- DIAGNOSTICA DATASET GENERATO ---\n")
print(range(dataset_rendimenti$date))
print(head(dataset_rendimenti, 3))


# ==============================================================================
# 3. ANALISI STATISTICA STORICA ED ESPLORATIVA DEI DATI
# ==============================================================================

# --- 3.1 Calcolo delle Metriche Descrittive Storiche ---
df_metriche_storiche <- dataset_rendimenti %>%
  tidyr::pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  dplyr::group_by(Asset) %>%
  dplyr::summarise(Media = mean(Rendimento), DevStd = sd(Rendimento), .groups = "drop") %>%
  dplyr::mutate(
    Asset_Label = paste0(Asset, "\nμ: ", sprintf("%.2f%%", Media * 100), " | σ: ", sprintf("%.2f%%", DevStd * 100))
  )

df_long_con_etichette <- dataset_rendimenti %>%
  tidyr::pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  dplyr::inner_join(df_metriche_storiche, by = "Asset")

# --- 3.2 Plot G1: Istogramma delle Distribuzioni Empiriche ---
p_distribuzione_storica <- ggplot(df_long_con_etichette, aes(x = Rendimento)) +
  geom_histogram(bins = 30, fill = PAL_HIST_FILL, color = PAL_BORDER_LIGHT, alpha = 0.8) +
  geom_vline(data = df_metriche_storiche, aes(xintercept = Media), color = "red", linetype = "solid", linewidth = 0.8) +
  geom_vline(data = df_metriche_storiche, aes(xintercept = Media + DevStd), color = "deepskyblue3", linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = df_metriche_storiche, aes(xintercept = Media - DevStd), color = "deepskyblue3", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ Asset_Label, scales = "free_y", ncol = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribuzione dei Rendimenti Mensili Storici (in EUR)",
    subtitle = "Linea rossa: Media (μ) | Linee azzurre: Media +/- Deviazione Standard (σ)",
    x = "Rendimento Mensile", y = "Frequenza (Mesi)", caption = GLOBAL_CAPTION
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(
    strip.background = element_rect(fill = PAL_STRIP_BG, color = PAL_BORDER_LIGHT),
    strip.text       = element_text(face = "bold", color = PAL_MAIN_TEXT, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = PAL_MAIN_TEXT, margin = margin(b = 15))
  )

# --- 3.3 Analisi di Autocorrelazione Condizionata (Lag-1) ---
df_autocorr_long <- dataset_rendimenti %>%
  tidyr::pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento_X") %>%
  dplyr::group_by(Asset) %>%
  dplyr::mutate(Rendimento_Y = dplyr::lag(Rendimento_X, n = 1)) %>%
  dplyr::ungroup() %>%
  tidyr::drop_na(Rendimento_Y)

df_metriche_ac <- df_autocorr_long %>%
  dplyr::group_by(Asset) %>%
  dplyr::summarise(Correlazione = cor(Rendimento_X, Rendimento_Y), .groups = "drop") %>%
  dplyr::mutate(Asset_Label_AC = paste0(Asset, "\nCorr Lag-1 (r): ", sprintf("%.4f", Correlazione)))

df_grafico_ac <- df_autocorr_long %>%
  dplyr::inner_join(df_metriche_ac, by = "Asset")

# --- 3.4 Plot G2: Scatter Plot di Autocorrelazione ---
p_autocorr_scatter <- ggplot(df_grafico_ac, aes(x = Rendimento_X, y = Rendimento_Y)) +
  geom_point(color = "deepskyblue4", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~ Asset_Label_AC, scales = "free", ncol = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Analisi di Autocorrelazione dei Rendimenti (Lag 1)",
    subtitle = "Rappresentazione stocastica: Rendimento Periodo (t) vs Periodo (t-1)",
    x = "Rendimenti Mensili Originali (X)", y = "Rendimenti Mensili Spostati (Y)", caption = GLOBAL_CAPTION
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(
    strip.background = element_rect(fill = PAL_STRIP_BG, color = PAL_BORDER_LIGHT),
    strip.text       = element_text(face = "bold", color = PAL_MAIN_TEXT, size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14)
  )

# --- 3.5 Test di Significatività Statistica dell'Autocorrelazione ---
tabella_test_autocorr <- df_autocorr_long %>%
  dplyr::group_by(Asset) %>%
  dplyr::summarise(
    Correlazione  = cor(Rendimento_X, Rendimento_Y),
    P_Value       = cor.test(Rendimento_X, Rendimento_Y)$p.value,
    Significativo = if_else(P_Value < 0.05, "SI", "NO"), .groups = "drop"
  ) %>%
  dplyr::arrange(P_Value)

# --- 3.6 Matrice di Correlazione Incrociata dei Rendimenti ---
matrice_correlazione <- cor(dataset_rendimenti %>% dplyr::select(-date), method = "pearson")

df_correlazione_long <- as.data.frame(matrice_correlazione) %>%
  tibble::rownames_to_column(var = "ETF_Var1") %>%
  tidyr::pivot_longer(cols = -ETF_Var1, names_to = "ETF_Var2", values_to = "Correlazione") %>%
  dplyr::mutate(
    ETF_Var1 = factor(ETF_Var1, levels = colnames(matrice_correlazione)),
    ETF_Var2 = factor(ETF_Var2, levels = rev(colnames(matrice_correlazione)))
  )

# --- 3.7 Plot G3: Heatmap delle Correlazioni Incrociate ---
p_heatmap_corr <- ggplot(df_correlazione_long, aes(x = ETF_Var1, y = ETF_Var2, fill = Correlazione)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(
    label = paste0(round(Correlazione * 100), "%"),
    color = abs(Correlazione) > 0.5
  ), size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("gray20", "white")) +
  scale_fill_distiller(
    palette = "RdYlBu", limits = c(-1, 1), direction = 1,
    name = "Correlazione", labels = c("-100%", "-50%", "0%", "50%", "100%")
  ) +
  labs(
    title = "Matrice di Correlazione dei Rendimenti Mensili",
    subtitle = "Blu = Correlazione Positiva | Bianco/Giallo = Incorrelati | Rosso = Correlazione Inversa",
    x = NULL, y = NULL, caption = GLOBAL_CAPTION
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", color = "gray20"),
    axis.text.y = element_text(face = "bold", color = "gray20"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 10))
  )


# ==============================================================================
# 4. MOTORE DI SIMULAZIONE STOCASTICA MONTE CARLO (CALCOLO PARALLELO)
# ==============================================================================

# Calcolo del patrimonio totale nominale versato dall'investitore (Benchmark Break-Even)
portafoglio_iniziale <- PORTAFOGLIO_TARGET * PARAM_PIC_INIZIALE
capitale_investito_totale <- sum(portafoglio_iniziale) + (PARAM_ORIZZONTE_MESI * PARAM_PAC_MENSILE)

# Ottimizzazione computazionale: trasformazione in matrice numerica pura per accelerare i core
matrice_rendimenti <- as.matrix(dataset_rendimenti[, NOMI_ASSET])
n_righe_dataset    <- nrow(matrice_rendimenti)

cat("\n>>> Avvio del cluster parallelo su", CORES_DA_USARE, "core CPU... ")
mio_cluster <- parallel::makeCluster(CORES_DA_USARE)
doParallel::registerDoParallel(mio_cluster)
tempo_inizio <- Sys.time()

# Esecuzione del ciclo Multithreading tramite Bootstrap stocastico
risultati_completi <- foreach::foreach(s = 1:PARAM_N_SIMULAZIONI, .packages = 'FinCal') %dopar% {
  
  portafoglio <- portafoglio_iniziale
  storia_valore <- numeric(PARAM_ORIZZONTE_MESI + 1)
  storia_valore[1] <- sum(portafoglio_iniziale)
  
  for (m in 1:PARAM_ORIZZONTE_MESI) {
    # Estrazione stocastica con reinserimento della riga storica dei rendimenti
    riga_estratta <- sample.int(n_righe_dataset, size = 1)
    rendimenti_estratti <- matrice_rendimenti[riga_estratta, ]
    
    # Rivalutazione dinamica del capitale di ogni singolo asset
    portafoglio <- portafoglio * (1 + rendimenti_estratti)
    
    valore_totale_pre     <- sum(portafoglio)
    valore_obiettivo_post <- valore_totale_pre + PARAM_PAC_MENSILE
    
    # Algoritmo di Ribilanciamento Automatico tramite l'iniezione dei flussi PAC
    target_ideale      <- valore_obiettivo_post * PORTAFOGLIO_TARGET
    distanza_da_target <- target_ideale - portafoglio
    quote_da_comprare  <- ifelse(distanza_da_target > 0, distanza_da_target, 0)
    
    if (sum(quote_da_comprare) > 0) {
      pesi_acquisto      <- quote_da_comprare / sum(quote_da_comprare)
      acquisti_effettivi <- pesi_acquisto * PARAM_PAC_MENSILE
    } else {
      acquisti_effettivi <- PORTAFOGLIO_TARGET * PARAM_PAC_MENSILE
    }
    
    portafoglio <- portafoglio + acquisti_effettivi
    storia_valore[m + 1] <- sum(portafoglio)
  }
  return(storia_valore) 
}

tempo_fine <- Sys.time()
parallel::stopCluster(mio_cluster) # Rilascio paranoico delle risorse hardware della CPU
cat("COMPLETATO in", round(difftime(tempo_fine, tempo_inizio, units = "secs"), 2), "secondi.\n")

# Trasformazione dei risultati grezzi in matrice strutturata (Righe = Simulazioni, Colonne = Mesi)
matrice_traiettorie <- do.call(rbind, risultati_completi)
risultati_scenari   <- matrice_traiettorie[, PARAM_ORIZZONTE_MESI + 1]

df_scenari_terminali <- tibble::tibble(Valore_Finale = risultati_scenari) %>%
  dplyr::mutate(Multiplo_Capitale = Valore_Finale / capitale_investito_totale)

# Estrazione dei percentili statistici chiave sul capitale finale ottenuto
p10 <- quantile(risultati_scenari, 0.10)
p50 <- quantile(risultati_scenari, 0.50)
p90 <- quantile(risultati_scenari, 0.90)


# ==============================================================================
# 5. CALCOLO STRUTTURALE DEL CAGR (IRR SUI FLUSSI FINANZIARI EFFETTIVI)
# ==============================================================================

# Vettore della struttura dei flussi finanziari d'uscita (Negativi perché versati)
flussi_base <- c(-sum(portafoglio_iniziale), rep(-PARAM_PAC_MENSILE, PARAM_ORIZZONTE_MESI))

calcola_cagr_pac <- function(valore_finale_target) {
  flussi_scenario <- flussi_base
  # L'ultimo mese accorpa l'ultimo versamento PAC e la liquidazione totale del montante finale
  flussi_scenario[length(flussi_scenario)] <- flussi_scenario[length(flussi_scenario)] + valore_finale_target
  tasso_mensile   <- FinCal::irr(flussi_scenario)
  cagr_annuo      <- ((1 + tasso_mensile)^12 - 1) * 100
  return(cagr_annuo)
}

cagr_p10 <- calcola_cagr_pac(p10)
cagr_p50 <- calcola_cagr_pac(p50)
cagr_p90 <- calcola_cagr_pac(p90)

# Formattatori testuali dinamici per le etichette dei grafici
fmt_euro <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")
lbl_cap_investito <- paste0("Capitale Investito: ", fmt_euro(capitale_investito_totale), " €")
lbl_pessimo        <- paste0("Pessimo (10%): ", fmt_euro(p10), " € (CAGR: ", sprintf("%.2f%%", cagr_p10), ")")
lbl_mediano        <- paste0("Mediano (50%): ", fmt_euro(p50), " € (CAGR: ", sprintf("%.2f%%", cagr_p50), ")")
lbl_ottimo         <- paste0("Ottimo (90%): ", fmt_euro(p90), " € (CAGR: ", sprintf("%.2f%%", cagr_p90), ")")


# ==============================================================================
# 6. ELABORAZIONE DELLE TRAIETTORIE TEMPORALI E RENDERING GRAFICO MC
# ==============================================================================

# Calcolo progressivo dei percentili geometrici mese dopo mese (Asse temporale)
percentili_nel_tempo <- apply(matrice_traiettorie, 2, function(colonna) {
  quantile(colonna, probs = c(0.10, 0.50, 0.90))
})

df_percentili_tempo <- tibble::tibble(
  Mese = 0:PARAM_ORIZZONTE_MESI,
  P10  = percentili_nel_tempo[1, ],
  P50  = percentili_nel_tempo[2, ],
  P90  = percentili_nel_tempo[3, ],
  Capitale_Investito = PARAM_PIC_INIZIALE + (Mese * PARAM_PAC_MENSILE)
)

# Estrazione casuale controllata di 100 traiettorie campione come rumore di fondo grafico
set.seed(42) 
indici_campione <- sample(1:PARAM_N_SIMULAZIONI, 100)
matrice_campione <- matrice_traiettorie[indici_campione, ]
colnames(matrice_campione) <- 0:PARAM_ORIZZONTE_MESI

df_linee_campione <- as.data.frame(matrice_campione) %>%
  dplyr::mutate(ID_Simulazione = row_number()) %>%
  tidyr::pivot_longer(cols = -ID_Simulazione, names_to = "Mese", values_to = "Valore") %>%
  dplyr::mutate(Mese = as.numeric(Mese))

# --- 6.1 Plot G4: Evoluzione Temporale delle Traiettorie ---
p_traiettorie_evoluzione <- ggplot() +
  geom_line(data = df_linee_campione, aes(x = Mese, y = Valore, group = ID_Simulazione), 
            color = "grey75", alpha = 0.25, linewidth = 0.4) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = Capitale_Investito), color = PAL_LINE_BE, linewidth = 1) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P10), color = PAL_SCEN_P10, linewidth = 1) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P50), color = PAL_SCEN_P50, linewidth = 1) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P90), color = PAL_SCEN_P90, linewidth = 1) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = seq(0, PARAM_ORIZZONTE_MESI, by = 24), labels = seq(0, PARAM_ORIZZONTE_ANNI, by = 2)) +
  labs(
    title = "Evoluzione Temporale e Traiettorie del Portafoglio",
    subtitle = "Evoluzione mensile dei percentili rispetto al capitale versato progressivo",
    x = "Tempo (Anni)", y = "Controvalore Portafoglio (€)",
    caption = "Linea Nera = Capitale Versato Progressivo | Linee Colorate = Percentili"
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 14))

# --- 6.2 Plot G5: Istogramma Distribuzione dei Valori Terminali ---
p_montecarlo_istogramma <- ggplot(df_scenari_terminali, aes(x = Valore_Finale)) +
  geom_histogram(bins = 100, fill = PAL_MC_FILL, color = PAL_BORDER_LIGHT, alpha = 0.8) +
  geom_vline(xintercept = capitale_investito_totale, color = PAL_LINE_BE, linetype = "solid", linewidth = 1) +
  annotate("text", x = capitale_investito_totale, y = Inf, label = lbl_cap_investito, 
           angle = 90, vjust = -1, hjust = 1.1, color = PAL_LINE_BE, size = 3.2, fontface = "bold") +
  geom_vline(xintercept = c(p10, p50, p90), color = c(PAL_SCEN_P10, PAL_SCEN_P50, PAL_SCEN_P90), linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = p10, y = Inf, label = lbl_pessimo, angle = 90, vjust = -1, hjust = 1.1, color = PAL_SCEN_P10, size = 3.2, fontface = "bold") +
  annotate("text", x = p50, y = Inf, label = lbl_mediano, angle = 90, vjust = -1, hjust = 1.1, color = PAL_SCEN_P50, size = 3.2, fontface = "bold") +
  annotate("text", x = p90, y = Inf, label = lbl_ottimo, angle = 90, vjust = -1, hjust = 1.1, color = PAL_SCEN_P90, size = 3.2, fontface = "bold") +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(transform = ~ . / capitale_investito_totale, name = "Multiplo del Capitale Investito", labels = scales::label_number(suffix = "x", decimal.mark = ","))
  ) +
  labs(title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)", x = "Controvalore Finale Portafoglio (€)", y = "Frequenza (Numero di Scenari)", caption = GLOBAL_CAPTION) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 14))


# ==============================================================================
# 7. ANALISI DE-ACCUMULO E PROFILO RISCHIO-RENDIMENTO (SPAZIO MEDIA-VARIANZA)
# ==============================================================================

# Funzione parametrica per estrarre e isolare la traiettoria più vicina a un percentile esatto
analizza_scenario_target <- function(valore_percentile, nome_scenario) {
  indice_scenario    <- which.min(abs(risultati_scenari - valore_percentile))
  traiettoria        <- matrice_traiettorie[indice_scenario, ]
  
  valori_t_meno_1    <- traiettoria[1:PARAM_ORIZZONTE_MESI]
  valori_t           <- traiettoria[2:(PARAM_ORIZZONTE_MESI + 1)]
  
  # Estrazione inversa dei rendimenti mensili netti depurati dall'apporto del flusso PAC costante
  rendimenti_mensili <- ((valori_t - PARAM_PAC_MENSILE) / valori_t_meno_1) - 1
  
  return(tibble::tibble(
    Scenario          = nome_scenario,
    Rendimento_Medio  = mean(rendimenti_mensili),
    Volatilicata      = sd(rendimenti_mensili)
  ))
}

# Consolidamento del profilo strutturale dei 3 scenari simulati
df_portafogli_rv <- dplyr::bind_rows(
  analizza_scenario_target(p10, "P10 (Sfavorevole)"),
  analizza_scenario_target(p50, "P50 (Mediano)"),
  analizza_scenario_target(p90, "P90 (Favorevole)")
) %>%
  dplyr::mutate(
    Asset_Label_RV = paste0(Scenario, "\nμ: ", sprintf("%.2f%%", Rendimento_Medio * 100), " | σ: ", sprintf("%.2f%%", Volatilicata * 100))
  )

# Integrazione logica dei dati degli asset storici per il confronto incrociato nello spazio geometrico
df_assets_rv <- df_metriche_storiche %>%
  dplyr::select(Asset, Rendimento_Medio = Media, Volatilicata = DevStd) %>%
  dplyr::mutate(
    Asset_Label_RV = paste0(Asset, "\nμ: ", sprintf("%.2f%%", Rendimento_Medio * 100), " | σ: ", sprintf("%.2f%%", Volatilicata * 100))
  )

# Algoritmo preventivo anti-clipping per il dimensionamento automatico e sicuro degli assi cartesiani
df_limiti_computati <- dplyr::bind_rows(df_assets_rv[,2:3], df_portafogli_rv[,2:3])
X_MIN_LIMIT <- min(df_limiti_computati$Volatilicata) * 0.7
X_MAX_LIMIT <- max(df_limiti_computati$Volatilicata) * 1.3
Y_MIN_LIMIT <- min(df_limiti_computati$Rendimento_Medio) * 1.3
Y_MAX_LIMIT <- max(df_limiti_computati$Rendimento_Medio) * 1.3

# --- 7.1 Plot G6: Mappatura Rischio-Rendimento ---
p_rischio_rendimento <- ggplot() +
  geom_hline(yintercept = 0, color = "gray60", linetype = "solid", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray60", linetype = "solid", linewidth = 0.5) +
  
  # Layer Asset Singoli (Cerchi blu standard)
  geom_point(data = df_assets_rv, aes(x = Volatilicata, y = Rendimento_Medio), color = "deepskyblue4", size = 3.5, alpha = 0.6) +
  geom_text(data = df_assets_rv, aes(x = Volatilicata, y = Rendimento_Medio, label = Asset_Label_RV), vjust = -0.6, hjust = 0.5, size = 2.6, fontface = "bold", color = "gray10") +
  
  # Layer Portafogli Simulati PAC (Diamanti mappati sui colori globali degli scenari)
  geom_point(data = df_portafogli_rv, aes(x = Volatilicata, y = Rendimento_Medio, color = Scenario), size = 5.0, shape = 18) +
  geom_text(data = df_portafogli_rv, aes(x = Volatilicata, y = Rendimento_Medio, label = Asset_Label_RV), vjust = 1.3, hjust = 0.5, size = 2.8, fontface = "bold", color = "black") +
  
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(
    values = c("P10 (Sfavorevole)" = PAL_SCEN_P10, "P50 (Mediano)" = PAL_SCEN_P50, "P90 (Favorevole)" = PAL_SCEN_P90),
    name = "Scenari PAC Monte Carlo"
  ) +
  expand_limits(x = c(X_MIN_LIMIT, X_MAX_LIMIT), y = c(Y_MIN_LIMIT, Y_MAX_LIMIT)) +
  labs(
    title = "Profilo Rendimento-Rischio: Asset vs Portafogli Target",
    subtitle = "Spazio Media-Varianza in EUR: Asset Singoli (Cerchi) vs Scenari Simulazione Monte Carlo (Diamanti)",
    x = "Volatilità Mensile (Scarto Quadratico Medio - Deviazione Standard σ)", y = "Rendimento Mensile Medio (Media μ)", caption = GLOBAL_CAPTION
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(
    panel.grid.major = element_line(color = PAL_GRID_LIGHT, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom", legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "gray98", color = "gray85")
  )


# ==============================================================================
# 8. COSTRUZIONE EVOLUZIONE STORICA UNIFICATA A BASE 100
# ==============================================================================

# Elaborazione indice a Base 100 sui rendimenti reali puri degli asset storici
df_assets_base100 <- dataset_rendimenti %>%
  dplyr::arrange(date) %>%
  tidyr::pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  dplyr::group_by(Asset) %>%
  dplyr::mutate(Valore_Indice = cumprod(1 + Rendimento) * 100) %>%
  dplyr::ungroup()

LUNGHEZZA_STORICA_REALE <- nrow(dataset_rendimenti)
DATA_FINALE_STORICA     <- max(dataset_rendimenti$date)

# Funzione interna per estrarre la traiettoria simulata e troncarla alla lunghezza della serie reale
estrai_indice_simulato <- function(valore_percentile, nome_legenda) {
  indice_scenario    <- which.min(abs(risultati_scenari - valore_percentile))
  traiettoria        <- matrice_traiettorie[indice_scenario, ]
  valori_t_meno_1    <- traiettoria[1:PARAM_ORIZZONTE_MESI]
  valori_t           <- traiettoria[2:(PARAM_ORIZZONTE_MESI + 1)]
  rendimenti_mensili <- ((valori_t - PARAM_PAC_MENSILE) / valori_t_meno_1) - 1
  
  # Troncatura paranoica per evitare sforamenti degli indici degli assi nel grafico temporale
  rendimenti_troncati <- rendimenti_mensili[1:LUNGHEZZA_STORICA_REALE]
  valore_indice       <- cumprod(1 + rendimenti_troncati) * 100
  
  return(tibble::tibble(
    date          = dataset_rendimenti$date,
    Asset         = nome_legenda, 
    Valore_Indice = valore_indice
  ))
}

df_p10_indice <- estrai_indice_simulato(p10, "PORTAFOGLIO P10")
df_p50_indice <- estrai_indice_simulato(p50, "PORTAFOGLIO P50")
df_p90_indice <- estrai_indice_simulato(p90, "PORTAFOGLIO P90")

# Consolidamento finale in unico data frame per l'ottimizzazione dei vettori di testo
df_tutti_indici <- dplyr::bind_rows(df_assets_base100, df_p10_indice, df_p50_indice, df_p90_indice)

df_labels_complessivo <- df_tutti_indici %>%
  dplyr::filter(date == DATA_FINALE_STORICA) %>%
  dplyr::arrange(Valore_Indice)

# Algoritmo di scorrimento verticale preventivo delle etichette (Risoluzione sovrapposizioni di testo)
SOGLIA_SPAZIO_Y <- 15.0  
griglia_y       <- df_labels_complessivo$Valore_Indice

if (length(griglia_y) > 1) {
  for (i in 2:length(griglia_y)) {
    if ((griglia_y[i] - griglia_y[i-1]) < SOGLIA_SPAZIO_Y) {
      griglia_y[i] <- griglia_y[i-1] + SOGLIA_SPAZIO_Y
    }
  }
}
df_labels_complessivo$Y_Aggiustato <- griglia_y

# Costruzione esplicita della palette di mappatura dei colori sui label testuali del grafico
palette_labels  <- rep("gray50", length(unique(df_assets_base100$Asset)))
names(palette_labels) <- unique(df_assets_base100$Asset)
palette_labels["PORTAFOGLIO P10"] <- PAL_SCEN_P10
palette_labels["PORTAFOGLIO P50"] <- PAL_SCEN_P50
palette_labels["PORTAFOGLIO P90"] <- PAL_SCEN_P90

# --- 8.1 Plot G7: Evoluzione Storica Unificata ---
p_evoluzione_unificata <- ggplot() +
  geom_hline(yintercept = 100, color = "orange", linetype = "dashed", linewidth = 0.6) +
  geom_line(data = df_assets_base100, aes(x = date, y = Valore_Indice, group = Asset), color = "gray50", linewidth = 0.6, alpha = 0.4) +
  geom_line(data = df_p10_indice, aes(x = date, y = Valore_Indice), color = PAL_SCEN_P10, linewidth = 1.3) +
  geom_line(data = df_p50_indice, aes(x = date, y = Valore_Indice), color = PAL_SCEN_P50, linewidth = 1.3) +
  geom_line(data = df_p90_indice, aes(x = date, y = Valore_Indice), color = PAL_SCEN_P90, linewidth = 1.3) +
  geom_text(data = df_labels_complessivo, aes(x = date, y = Y_Aggiustato, label = Asset, color = Asset), hjust = 0, nudge_x = 45, size = 2.6, fontface = "bold") +
  scale_y_continuous(labels = scales::label_number(suffix = " €", big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.01, 0.25))) +
  scale_color_manual(values = palette_labels) +
  labs(
    title = "Evoluzione Storica Unificata vs Traiettorie Monte Carlo",
    subtitle = "Rendimenti puri a Base 100 (No PAC) con allineamento dinamico dei label nativi",
    x = "Anno", y = "Valore dell'Indice (Base 100 iniziale)", caption = GLOBAL_CAPTION
  ) +
  theme_minimal(base_size = GLOBAL_THEME_SIZE) +
  theme(
    panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"
  )


# ==============================================================================
# 9. COMPILAZIONE DOCUMENTO FINALE ED ESPORTAZIONE IN FILE PDF (BATCH MODE)
# ==============================================================================

NOME_FILE_PDF <- "Report_Simulazione_Globale_Portafoglio.pdf"
cat("\n>>> Esportazione di tutti i grafici nel file unificato:", NOME_FILE_PDF, "... ")

# Apertura della periferica grafica PDF (Standard A4 Orizzontale ottimizzato per la visualizzazione di griglie)
pdf(file = NOME_FILE_PDF, width = 11.69, height = 8.27, onefile = TRUE)

# Stampa in sequenza logica rigida dei layer memorizzati in RAM
print(p_distribuzione_storica)
print(p_autocorr_scatter)
print(p_heatmap_corr)
print(p_traiettorie_evoluzione)
print(p_montecarlo_istogramma)
print(p_rischio_rendimento)
print(p_evoluzione_unificata)

# Chiusura formale del canale di esportazione hardware per il consolidamento del file su disco
dev.off()

cat("COMPLETATO CON SUCCESSO.\n\n")
cat("==============================================================================\n")
cat(" PIPELINE ESEGUITA SENZA ERRORI STRUTTURALI\n")
cat(" Generato file PDF contabile con", length(dev.list()), "fogli di analisi visuale.\n")
cat("==============================================================================\n")