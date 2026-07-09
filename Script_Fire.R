# ==============================================================================
# SIMULAZIONE FIRE PARALLELA (VERSIONE SEMPLIFICATA)
# ==============================================================================

# --- Inizializzazione e Librerie ---
rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)
library(quantmod)
library(parallel)
library(foreach)
library(doParallel)

# ==============================================================================
# 1. PARAMETRI DI INPUT
# ==============================================================================
PARAM_MONTANTE_INIZIALE  <- 430000 + 420000   # Capitale totale (€)
PARAM_VALORE_CARICO      <- 266000 + 205000   # Valore di carico fiscale (€)
PARAM_PRELIEVO_ANNUALE   <- 30000    # Prelievo NETTO iniziale (€)
PARAM_INFLAZIONE_ANNUALE <- 0.05     # Inflazione al
PARAM_ETA_INIZIALE       <- 55       # Età di partenza
PARAM_ORIZZONTE_ANNI     <- 30       # Durata simulazione
SIM_PER_CORE             <- 2000     # Ogni core farà 2.000 simulazioni
CORES_DA_USARE           <- 12       # I tuoi 12 core dedicati
TAX_RATE                 <- 0.26     # Tasse al 26%

# Calcoli derivati
N_MESI <- PARAM_ORIZZONTE_ANNI * 12
W_net_iniziale <- PARAM_PRELIEVO_ANNUALE / 12  
TOTAL_SIM <- SIM_PER_CORE * CORES_DA_USARE # Totale: 24.000 simulazioni

# ==============================================================================
# 2. RECUPERO DATI STORICI (Yahoo Finance)
# ==============================================================================
TICKER_NAMES <- c(
  "Bond_Global" = "AGG", "World_ex_USA" = "CWI", "World_Equal_W" = "RSP",
  "SP500" = "SPY", "Emerging_Markets" = "EEM", "World_Value" = "IVE",
  "World_Mid_Cap" = "IJH", "Gold" = "GLD", "World_Momentum" = "PDP", "World_Small_Cap" = "IWM"
)

PORTAFOGLIO_TARGET <- c(
  "Bond_Global" = 0.1943, "World_ex_USA" = 0.1748, "World_Equal_W" = 0.1282,
  "SP500" = 0.1159, "Emerging_Markets" = 0.0829, "World_Value" = 0.0739,
  "World_Mid_Cap" = 0.0678, "Gold" = 0.0627, "World_Momentum" = 0.0547, "World_Small_Cap" = 0.0448
)

cat(">>> Scaricamento dati storici...\n")
cambio_raw <- getSymbols("EURUSD=X", src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
prezzo_cambio_mensile <- Cl(xts::to.monthly(cambio_raw, indexAt = "lastof"))
lista_rendimenti_eur  <- list()

for (asset_name in names(TICKER_NAMES)) {
  tryCatch({
    dati_giornalieri <- getSymbols(TICKER_NAMES[asset_name], src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    prezzi_mensili_usd <- Cl(xts::to.monthly(dati_giornalieri, indexAt = "lastof"))
    dati_comuni <- merge(prezzi_mensili_usd, prezzo_cambio_mensile, all = FALSE)
    prezzi_mensili_eur <- dati_comuni[, 1] / dati_comuni[, 2]
    rend_mensile <- ROC(prezzi_mensili_eur, type = "discrete")
    colnames(rend_mensile) <- asset_name
    lista_rendimenti_eur[[asset_name]] <- rend_mensile
  }, error = function(e) { cat("⚠️ Errore su:", asset_name, "\n") })
}

dataset_rendimenti <- do.call(merge, lista_rendimenti_eur) %>% as.data.frame() %>% drop_na()
rendimenti_portafoglio <- as.matrix(dataset_rendimenti) %*% PORTAFOGLIO_TARGET

(1 + mean(rendimenti_portafoglio))^12-1
sd(rendimenti_portafoglio)*100

# ==============================================================================
# 3. ATTIVAZIONE PARALLELA & SIMULAZIONE MONTE CARLO
# ==============================================================================
cat(">>> Avvio simulazione sui 12 core...\n")
cl <- makeCluster(CORES_DA_USARE)
registerDoParallel(cl)
clusterSetRNGStream(cl, 42) # Seed per replicabilità

matrice_capitale <- foreach(i = 1:CORES_DA_USARE, .combine = 'cbind') %dopar% {
  
  # Ogni core gestisce il suo blocco di vettori
  C <- rep(PARAM_MONTANTE_INIZIALE, SIM_PER_CORE)  
  B <- rep(PARAM_VALORE_CARICO, SIM_PER_CORE)      
  
  res_locale <- matrix(0, nrow = N_MESI + 1, ncol = SIM_PER_CORE)
  res_locale[1, ] <- C
  
  for (m in 1:N_MESI) {
    # 1. Campionamento rendimenti
    r <- sample(rendimenti_portafoglio, size = SIM_PER_CORE, replace = TRUE)
    C <- C * (1 + r)
    
    # 2. Calcolo Inflazione (Scatta ogni 12 mesi)
    anno <- floor((m - 1) / 12)
    W_net <- W_net_iniziale * (1 + PARAM_INFLAZIONE_ANNUALE)^anno
    
    # 3. Tasse (26% su quota plusvalenze) e Prelievo
    pct_gain <- pmax(0, (C - B) / pmax(C, 1e-6))
    W_gross  <- ifelse(C > 0, W_net / (1 - TAX_RATE * pct_gain), 0)
    W_gross  <- pmin(C, W_gross) # Evita di prelevare più di quanto c'è in cassa
    
    # Update base fiscale e decurtazione capitale
    B <- B * (1 - (W_gross / pmax(C, 1e-6)))
    C <- pmax(0, C - W_gross)
    
    res_locale[m + 1, ] <- C
  }
  res_locale # Restituisce i dati di questo core
}

stopCluster(cl)
cat(">>> Fatto!\n")

# ==============================================================================
# 4. ELABORAZIONE DATI (Percentili + Campionamento per Sfondo)
# ==============================================================================
cat(">>> Elaborazione grafici...\n")
timeline_eta <- PARAM_ETA_INIZIALE + (0:N_MESI) / 12

# 4.1 Calcolo dei percentili classici
df_percentili <- t(apply(matrice_capitale, 1, quantile, probs = c(0.10, 0.50, 0.90))) %>%
  as.data.frame() %>%
  rename(P10 = `10%`, P50 = `50%`, P90 = `90%`) %>%
  mutate(Eta = timeline_eta)

# 4.2 Campionamento di 1.000 linee per lo sfondo (grigetto)
set.seed(123)
indici_campione <- sample(1:TOTAL_SIM, size = 1000)
matrice_campione <- matrice_capitale[, indici_campione]

df_traiettorie_background <- as.data.frame(matrice_campione) %>%
  mutate(Eta = timeline_eta) %>%
  pivot_longer(cols = -Eta, names_to = "Simulazione", values_to = "Capitale")

# Calcolo tasso di sopravvivenza finale
survival_rate <- (sum(matrice_capitale[N_MESI + 1, ] > 0) / TOTAL_SIM) * 100

# ==============================================================================
# 5. COSTRUZIONE GRAFICO (CON CORREZIONE BIG.MARK E DECIMAL.MARK)
# ==============================================================================
ggplot() +
  geom_line(data = df_traiettorie_background, 
            aes(x = Eta, y = Capitale, group = Simulazione), 
            color = "gray85", alpha = 0.15, linewidth = 0.3) +
  
  geom_line(data = df_percentili, aes(x = Eta, y = P50, color = "Mediano (50%)"), linewidth = 1.3) +
  geom_line(data = df_percentili, aes(x = Eta, y = P90, color = "Ottimista (90%)"), linewidth = 1, linetype = "dashed") +
  geom_line(data = df_percentili, aes(x = Eta, y = P10, color = "Pessimista (10%)"), linewidth = 1, linetype = "dashed") +
  
  coord_cartesian(ylim = c(0, 5000000)) +
  
  # FIX 1: Aggiunto decimal.mark = "," per l'asse Y
  scale_y_continuous(labels = scales::dollar_format(prefix = "€ ", big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("Mediano (50%)" = "blue", "Ottimista (90%)" = "green4", "Pessimista (10%)" = "red")) +
  
  # FIX 2: Aggiunto decimal.mark = "," dentro tutte le funzioni format()
  labs(
    title = paste0("Simulazione FIRE - Tasso di Sopravvivenza: ", round(survival_rate, 2), "%"),
    subtitle = paste0(
      "Capitale Iniziale: €", format(PARAM_MONTANTE_INIZIALE, big.mark = ".", decimal.mark = ","), 
      " (Carico: €", format(PARAM_VALORE_CARICO, big.mark = ".", decimal.mark = ","), ") | ",
      "Prelievo Netto: €", format(PARAM_PRELIEVO_ANNUALE, big.mark = ".", decimal.mark = ","), "/anno\n",
      "Inflazione Reale: ", PARAM_INFLAZIONE_ANNUALE * 100, "% annua | ",
      "Aliquota Capital Gain: ", TAX_RATE * 100, "%"
    ),
    caption = paste0(
      "Nota tecnica: Analisi Monte Carlo (Bootstrap storico) | Orizzonte: ", PARAM_ORIZZONTE_ANNI, " anni | ",
      "Simulazioni: ", format(TOTAL_SIM, big.mark = ".", decimal.mark = ","), " totali elaborate su ", CORES_DA_USARE, " core | ",
      "Sfondo: 1.000 linee campionate casualmente | Dati: Yahoo Finance"
    ),
    x = "Età del Pensionato", y = "Capitale Residuo (€)", color = "Scenari"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray20", lineheight = 1.2),
    plot.caption = element_text(size = 7.5, color = "gray50", hjust = 0)
  )

