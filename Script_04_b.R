# ==============================================================================
# 1. CARICAMENTO LIBRERIE E CONFIGURAZIONE PARAMETRI GLOBALI
# ==============================================================================
library(parallel)
library(foreach)
library(doParallel)
library(FinCal)
library(dplyr)     # Per manipolazione dati (tibble, mutate)
library(ggplot2)   # Per l'architettura dei grafici
library(tidyr)     # Per la ristrutturazione dei dati (pivot_longer)

# --- Configurazione Hardware per Calcolo Parallelo ---
CORES_DISPONIBILI <- detectCores()
CORES_DA_USARE     <- CORES_DISPONIBILI - 2 # Salvaguardia di 2 core per la stabilità del sistema

# --- Configurazione Palette Cromatica dei Grafici ---
COLOR_MC_FILL     <- "aquamarine2"
COLOR_MC_BORDER   <- "azure3"
COLOR_LINE_BE     <- "black"       # Linea di Break-Even (Capitale nominale versato)
COLOR_P10         <- "red"         # Linea Percentile 10% (Scenario Sfavorevole)
COLOR_P50         <- "orange"      # Linea Percentile 50% (Scenario Mediano)
COLOR_P90         <- "green4"      # Linea Percentile 90% (Scenario Favorevole)


# ==============================================================================
# 2. DEFINIZIONE DEI PESI TARGET DEL PORTAFOGLIO LAZY
# ==============================================================================
target_weights <- c(
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


# ==============================================================================
# 3. CONFIGURAZIONE PARAMETRI E AVVIO SIMULATORE MONTE CARLO
# ==============================================================================
# --- Parametri del Piano di Accumulo (PAC) ---
somma_iniziale       <- 30000
portafoglio_iniziale <- target_weights * somma_iniziale

orizzonte_mesi <- 240    
quota_mensile  <- 1000   
n_simulazioni  <- 100000   
nomi_asset     <- names(target_weights)

capitale_investito_totale <- sum(portafoglio_iniziale) + (orizzonte_mesi * quota_mensile)

# --- Ottimizzazione Dati: Conversione del dataset in matrice numerica ---
# Requisito: Il 'dataset_rendimenti' deve essere pre-caricato nell'ambiente di lavoro
matrice_rendimenti <- as.matrix(dataset_rendimenti[, nomi_asset])
n_righe_dataset    <- nrow(matrice_rendimenti)

# --- Inizializzazione del Cluster per Calcolo Parallelo ---
mio_cluster <- makeCluster(CORES_DA_USARE)
registerDoParallel(mio_cluster)

tempo_inizio <- Sys.time()

# --- Esecuzione Ciclo di Simulazione Multithreading ---
# Viene salvata l'intera traiettoria mensile per ogni singola simulazione
risultati_completi <- foreach(s = 1:n_simulazioni, .packages = 'FinCal') %dopar% {
  
  portafoglio <- portafoglio_iniziale
  
  # Allocazione vettore storico (Orizzonte + Mese 0)
  storia_valore <- numeric(orizzonte_mesi + 1)
  storia_valore[1] <- sum(portafoglio_iniziale)
  
  for (m in 1:orizzonte_mesi) {
    # Estrazione casuale con reinserimento (Bootstrap)
    riga_estratta <- sample.int(n_righe_dataset, size = 1)
    rendimenti_estratti <- matrice_rendimenti[riga_estratta, ]
    
    # Aggiornamento del valore degli asset in base ai rendimenti
    portafoglio <- portafoglio * (1 + rendimenti_estratti)
    
    valore_totale_pre     <- sum(portafoglio)
    valore_obiettivo_post <- valore_totale_pre + quota_mensile
    
    # Algoritmo di Ribilanciamento tramite i flussi del PAC
    target_ideale      <- valore_obiettivo_post * target_weights
    distanza_da_target <- target_ideale - portafoglio
    quote_da_comprare  <- ifelse(distanza_da_target > 0, distanza_da_target, 0)
    
    if (sum(quote_da_comprare) > 0) {
      pesi_acquisto      <- quote_da_comprare / sum(quote_da_comprare)
      acquisti_effettivi <- pesi_acquisto * quota_mensile
    } else {
      acquisti_effettivi <- target_weights * quota_mensile
    }
    
    portafoglio <- portafoglio + acquisti_effettivi
    
    # Registrazione del valore di portafoglio a fine mese
    storia_valore[m + 1] <- sum(portafoglio)
  }
  
  return(storia_valore) 
}

tempo_fine <- Sys.time()

# --- Chiusura del Cluster e Rilascio Risorse CPU ---
stopCluster(mio_cluster)


# ==============================================================================
# 4. STRUTTURAZIONE DATI E CALCOLO STATISTICO DEI PERCENTILI FINALI
# ==============================================================================
# Conversione della lista dei risultati in una matrice (Righe = Simulazioni, Colonne = Mesi)
matrice_traiettorie <- do.call(rbind, risultati_completi)

# Estrazione del valore finale (ultima colonna della matrice)
risultati_scenari <- matrice_traiettorie[, orizzonte_mesi + 1]

df_scenari <- tibble(Valore_Finale = risultati_scenari) %>%
  mutate(Multiplo_Capitale = Valore_Finale / capitale_investito_totale)

# Calcolo dei percentili statistici sul valore terminale
p10 <- quantile(risultati_scenari, 0.10)
p50 <- quantile(risultati_scenari, 0.50)
p90 <- quantile(risultati_scenari, 0.90)

n_anni <- orizzonte_mesi / 12


# ==============================================================================
# 5. CALCOLO RIGOROSO DEL CAGR (RENDIMENTO ANNUO COMPLEMENTARE CORRETTO PER I FLUSSI)
# ==============================================================================
flussi_base <- c(-sum(portafoglio_iniziale), rep(-quota_mensile, orizzonte_mesi))

calcola_cagr_pac <- function(valore_finale) {
  flussi_scenario <- flussi_base
  flussi_scenario[length(flussi_scenario)] <- flussi_scenario[length(flussi_scenario)] + valore_finale
  tasso_mensile   <- FinCal::irr(flussi_scenario)
  cagr_annuo      <- ((1 + tasso_mensile)^12 - 1) * 100
  return(cagr_annuo)
}

cagr_p10 <- calcola_cagr_pac(p10)
cagr_p50 <- calcola_cagr_pac(p50)
cagr_p90 <- calcola_cagr_pac(p90)


# ==============================================================================
# 6. CREAZIONE DELLE ETICHETTE DINAMICHE PER I PLOT
# ==============================================================================
fmt_euro <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

lbl_cap_investito <- paste0("Capitale Investito: ", fmt_euro(capitale_investito_totale), " €")
lbl_pessimo        <- paste0("Pessimo (10%): ", fmt_euro(p10), " € (CAGR: ", sprintf("%.2f%%", cagr_p10), ")")
lbl_mediano        <- paste0("Mediano (50%): ", fmt_euro(p50), " € (CAGR: ", sprintf("%.2f%%", cagr_p50), ")")
lbl_ottimo         <- paste0("Ottimo (90%): ", fmt_euro(p90), " € (CAGR: ", sprintf("%.2f%%", cagr_p90), ")")


# ==============================================================================
# 7. ELABORAZIONE TRAIETTORIE TEMPORALI E CAMPIONAMENTO GRAFICO
# ==============================================================================
# Calcolo dei percentili storici applicato progressivamente mese per mese
percentili_nel_tempo <- apply(matrice_traiettorie, 2, function(colonna) {
  quantile(colonna, probs = c(0.10, 0.50, 0.90))
})

df_percentili_tempo <- tibble(
  Mese = 0:orizzonte_mesi,
  P10  = percentili_nel_tempo[1, ],
  P50  = percentili_nel_tempo[2, ],
  P90  = percentili_nel_tempo[3, ],
  Capitale_Investito = somma_iniziale + (Mese * quota_mensile)
)

# Estrazione statistica di 100 simulazioni casuali per la visualizzazione dello sfondo
set.seed(42) 
indici_campione <- sample(1:n_simulazioni, 100)
matrice_campione <- matrice_traiettorie[indici_campione, ]
colnames(matrice_campione) <- 0:orizzonte_mesi

df_linee_campione <- as.data.frame(matrice_campione) %>%
  mutate(ID_Simulazione = row_number()) %>%
  pivot_longer(cols = -ID_Simulazione, names_to = "Mese", values_to = "Valore") %>%
  mutate(Mese = as.numeric(Mese))


# ==============================================================================
# 8. PLOT EVOLUZIONE TEMPORALE DEL PORTAFOGLIO
# ==============================================================================
p_traiettorie <- ggplot() +
  # Rendering del campione delle 100 traiettorie casuali di sfondo
  geom_line(data = df_linee_campione, aes(x = Mese, y = Valore, group = ID_Simulazione), 
            color = "grey75", alpha = 0.25, linewidth = 0.4) +
  
  # Curva del capitale progressivamente versato (Break-Even)
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = Capitale_Investito), 
            color = COLOR_LINE_BE, linewidth = 1) +
  
  # Curve storiche dei percentili di riferimento
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P10), color = COLOR_P10, linewidth = 1) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P50), color = COLOR_P50, linewidth = 1) +
  geom_line(data = df_percentili_tempo, aes(x = Mese, y = P90), color = COLOR_P90, linewidth = 1) +
  
  # Formattazione assi e testi
  scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = seq(0, orizzonte_mesi, by = 24), labels = seq(0, n_anni, by = 2)) +
  labs(
    title = "Evoluzione Temporale e Traiettorie del Portafoglio",
    subtitle = "Evoluzione mensile dei percentili rispetto al capitale versato progressivo",
    x = "Tempo (Anni)",
    y = "Controvalore Portafoglio (€)",
    caption = "Linea Nera = Capitale Versato Progressivo | Linee Colorate = Percentili"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = "black", margin = margin(b = 15))
  )


# ==============================================================================
# 9. PLOT ISTOGRAMMA SIMULAZIONE MONTE CARLO
# ==============================================================================
p_montecarlo <- ggplot(df_scenari, aes(x = Valore_Finale)) +
  # Istogramma delle frequenze sui valori terminali delle simulazioni
  geom_histogram(bins = 100, fill = COLOR_MC_FILL, color = COLOR_MC_BORDER, alpha = 0.8) +
  
  # Linea verticale di Break-Even sul totale investito
  geom_vline(xintercept = capitale_investito_totale, color = COLOR_LINE_BE, linetype = "solid", linewidth = 1) +
  annotate("text", x = capitale_investito_totale, y = Inf, label = lbl_cap_investito, 
           angle = 90, vjust = -1, hjust = 1.1, color = COLOR_LINE_BE, size = 3.2, fontface = "bold") +
  
  # Linee verticali di benchmark dei percentili finali
  geom_vline(xintercept = c(p10, p50, p90), color = c(COLOR_P10, COLOR_P50, COLOR_P90), linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = p10, y = Inf, label = lbl_pessimo, angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P10, size = 3.2, fontface = "bold") +
  annotate("text", x = p50, y = Inf, label = lbl_mediano, angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P50, size = 3.2, fontface = "bold") +
  annotate("text", x = p90, y = Inf, label = lbl_ottimo, angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P90, size = 3.2, fontface = "bold") +
  
  # Doppio asse x per valori nominali e multipli del capitale
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(transform = ~ . / capitale_investito_totale, name = "Multiplo del Capitale Investito", labels = scales::label_number(suffix = "x", decimal.mark = ","))
  ) +
  labs(
    title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)", 
    x = "Controvalore Finale Portafoglio (€)", 
    y = "Frequenza (Numero di Scenari)", 
    caption = "Source data Yahoo Finance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(), 
    plot.title = element_text(face = "bold", size = 14)
  )

# --- Output Grafici ---
print(p_traiettorie)
print(p_montecarlo)


# ==============================================================================
# CONFIGURAZIONE PARAMETRI GLOBALI PER PROFILO RENDIMENTO-RISCHIO
# ==============================================================================
# --- Configurazione Palette Cromatica ---
COLOR_RV_POINTS   <- "deepskyblue4"    # Colore del punto di coordinata dell'asset
COLOR_RV_TEXT     <- "gray10"          # Colore delle etichette descrittive degli asset
COLOR_RV_GRID     <- "gray90"          # Colore delle linee di griglia del background

# --- Stringhe di Testo ed Etichette Grafiche ---
TXT_RV_TITLE      <- "Profilo Rendimento-Rischio degli Asset (Spazio Media-Varianza)"
TXT_RV_SUBTITLE   <- "Posizionamento dei proxy storici in EUR con metriche puntuali inserite nei label"
TXT_RV_X_LABEL    <- "Volatilità Mensile (Scarto Quadratico Medio - Deviazione Standard σ)"
TXT_RV_Y_LABEL    <- "Rendimento Mensile Medio (Media μ)"
TXT_RV_CAPTION    <- "Made in R - Source data: Yahoo Finance"


# ==============================================================================
# 1. AGGREGAZIONE STATISTICA E COSTRUZIONE STRINGHE DI LABEL DINAMICHE
# ==============================================================================
# Calcolo di Media (μ), Deviazione Standard (σ) e formattazione dell'etichetta testuale
df_rendimento_rischio <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  group_by(Asset) %>%
  summarise(
    Rendimento_Medio = mean(Rendimento),
    Volatilicata     = sd(Rendimento)
  ) %>%
  ungroup() %>%
  # Creazione della stringa descrittiva multiline per il plotting geometrico
  mutate(
    Asset_Label_RV = paste0(
      Asset, "\n",
      "μ: ", sprintf("%.2f%%", Rendimento_Medio * 100), " | ",
      "σ: ", sprintf("%.2f%%", Volatilicata * 100)
    )
  )


# ==============================================================================
# 1. DEFINIZIONE DELLA FUNZIONE PARAMETRICA DI ANALISI TRAIETTORIA
# ==============================================================================
analizza_scenario_target <- function(valore_percentile, nome_scenario) {
  
  # Identificazione geometrica dell'indice di riga più vicino al valore target
  indice_scenario <- which.min(abs(risultati_scenari - valore_percentile))
  
  # Estrazione del vettore storico completo (241 vettori mensili)
  traiettoria <- matrice_traiettorie[indice_scenario, ]
  
  # Separazione dei vettori temporali sfasati per l'isolamento dei rendimenti
  valori_t_meno_1 <- traiettoria[1:orizzonte_mesi]
  valori_t        <- traiettoria[2:(orizzonte_mesi + 1)]
  
  # Applicazione del de-accumulo inverso (Sottrazione quota PAC)
  rendimenti_mensili <- ((valori_t - quota_mensile) / valori_t_meno_1) - 1
  
  # Calcolo delle metriche descrittive campionarie
  media_mensile <- mean(rendimenti_mensili)
  sd_mensile    <- sd(rendimenti_mensili)
  media_annua   <- ((1 + media_mensile)^12 - 1)
  
  # Restituzione dei dati strutturati in formato data frame
  return(tibble(
    Scenario          = nome_scenario,
    Indice_Riga       = indice_scenario,
    Valore_Finale_EUR = risultati_scenari[indice_scenario],
    Media_Mensile_mu  = media_mensile,
    Volatila_Mese_sig = sd_mensile,
    Rend_Annuo_Comp   = media_annua
  ))
}


# ==============================================================================
# 2. ESECUZIONE DELL'ANALISI SUI TRE PERCENTILI TARGET
# ==============================================================================
df_metriche_scenari <- bind_rows(
  analizza_scenario_target(p10, "P10 (Sfavorevole)"),
  analizza_scenario_target(p50, "P50 (Mediano)"),
  analizza_scenario_target(p90, "P90 (Favorevole)")
)


# ==============================================================================
# 3. OUTPUT E STAMPA FORMATTATA DEI RISULTATI COMPARATIVI
# ==============================================================================
# Formattazione delle colonne numeriche per la visualizzazione a console
tabella_comparativa <- df_metriche_scenari %>%
  mutate(
    Valore_Finale_EUR = paste0(format(round(Valore_Finale_EUR), big.mark = "."), " €"),
    Media_Mensile_mu  = sprintf("%.4f%%", Media_Mensile_mu * 100),
    Volatila_Mese_sig = sprintf("%.4f%%", Volatila_Mese_sig * 100),
    Rend_Annuo_Comp   = sprintf("%.2f%%", Rend_Annuo_Comp * 100)
  )

# Visualizzazione della matrice dei risultati a console
cat("MATRICE COMPARATIVA DELLE METRICHE STRUTTURALI DEI TRE SCENARI TARGET\n")
print(as.data.frame(tabella_comparativa))




# ==============================================================================
# CONFIGURAZIONE PARAMETRI GLOBALI PER PROFILO RENDIMENTO-RISCHIO
# ==============================================================================
# --- Configurazione Palette Cromatica Asset Singoli ---
COLOR_RV_POINTS   <- "deepskyblue4"    # Colore del punto di coordinata dell'asset
COLOR_RV_TEXT     <- "gray10"          # Colore delle etichette descrittive degli asset
COLOR_RV_GRID     <- "gray90"          # Colore delle linee di griglia del background

# --- Configurazione Palette Cromatica Portafogli PAC (Richiesta) ---
COLOR_PORT_P10    <- "red"             # Rosso per Scenario Sfavorevole (P10)
COLOR_PORT_P50    <- "blue"            # Blu per Scenario Mediano (P50)
COLOR_PORT_P90    <- "green4"          # Verde per Scenario Favorevole (P90)

# --- Stringhe di Testo ed Etichette Grafiche ---
TXT_RV_TITLE      <- "Profilo Rendimento-Rischio: Asset vs Portafogli Target"
TXT_RV_SUBTITLE   <- "Spazio Media-Varianza in EUR: Asset Singoli (Cerchi) vs Scenari Simulazione Monte Carlo (Diamanti)"
TXT_RV_X_LABEL    <- "Volatilità Mensile (Scarto Quadratico Medio - Deviazione Standard σ)"
TXT_RV_Y_LABEL    <- "Rendimento Mensile Medio (Media μ)"
TXT_RV_CAPTION    <- "Made in R - Source data: Yahoo Finance"


# ==============================================================================
# 1. ELABORAZIONE E STRUTTURAZIONE DATI ASSET SINGOLI
# ==============================================================================
df_rendimento_rischio <- dataset_rendimenti %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Rendimento") %>%
  group_by(Asset) %>%
  summarise(
    Rendimento_Medio = mean(Rendimento),
    Volatilicata     = sd(Rendimento)
  ) %>%
  ungroup() %>%
  mutate(
    Asset_Label_RV = paste0(
      Asset, "\n",
      "μ: ", sprintf("%.2f%%", Rendimento_Medio * 100), " | ",
      "σ: ", sprintf("%.2f%%", Volatilicata * 100)
    )
  )


# ==============================================================================
# 2. STRUTTURAZIONE E CONFIGURAZIONE DATI PORTAFOGLI TARGET (P10, P50, P90)
# ==============================================================================
# Riorganizzazione del data frame delle metriche calcolato precedentemente
df_portafogli_rv <- df_metriche_scenari %>%
  select(
    Scenario, 
    Rendimento_Medio = Media_Mensile_mu, 
    Volatilicata     = Volatila_Mese_sig
  ) %>%
  mutate(
    Asset_Label_RV = paste0(
      Scenario, "\n",
      "μ: ", sprintf("%.2f%%", Rendimento_Medio * 100), " | ",
      "σ: ", sprintf("%.2f%%", Volatilicata * 100)
    )
  )


# ==============================================================================
# 3. CALCOLO DINAMICO DEI LIMITI DI ESPANSIONE DEGLI ASSI (PREVENZIONE CLIPPING)
# ==============================================================================
# Unione temporanea per identificare i minimi e massimi assoluti del grafico
df_limiti_computati <- bind_rows(
  df_rendimento_rischio %>% select(Volatilicata, Rendimento_Medio),
  df_portafogli_rv %>% select(Volatilicata, Rendimento_Medio)
)

X_MIN_LIMIT <- min(df_limiti_computati$Volatilicata) * 0.7
X_MAX_LIMIT <- max(df_limiti_computati$Volatilicata) * 1.3
Y_MIN_LIMIT <- min(df_limiti_computati$Rendimento_Medio) * 1.3
Y_MAX_LIMIT <- max(df_limiti_computati$Rendimento_Medio) * 1.3


# ==============================================================================
# 4. COSTRUZIONE E RENDERING GRAFICO MULTI-LAYER (GGPLOT2)
# ==============================================================================
p_rendimento_rischio <- ggplot() +
  
  # Assi di riferimento cartesiano di origine (Zero)
  geom_hline(yintercept = 0, color = "gray60", linetype = "solid", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray60", linetype = "solid", linewidth = 0.5) +
  
  # --- LAYER 1: Asset Singoli (Geometria a cerchio, colore uniforme) ---
  geom_point(data = df_rendimento_rischio, 
             aes(x = Volatilicata, y = Rendimento_Medio), 
             color = COLOR_RV_POINTS, size = 3.5, alpha = 0.6) +
  
  geom_text(data = df_rendimento_rischio, 
            aes(x = Volatilicata, y = Rendimento_Medio, label = Asset_Label_RV), 
            vjust = -0.6, hjust = 0.5, size = 2.6, fontface = "bold", color = COLOR_RV_TEXT) +
  
  # --- LAYER 2: Portafogli Simulati (Geometria a diamante, colore mappato) ---
  geom_point(data = df_portafogli_rv, 
             aes(x = Volatilicata, y = Rendimento_Medio, color = Scenario), 
             size = 5.0, shape = 18) +
  
  geom_text(data = df_portafogli_rv, 
            aes(x = Volatilicata, y = Rendimento_Medio, label = Asset_Label_RV), 
            vjust = 1.3, hjust = 0.5, size = 2.8, fontface = "bold", color = "black") +
  
  # --- Configurazione Scale, Palette e Assi ---
  scale_x_continuous(breaks = seq(0, 0.08, by = 0.01), labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.012, by = 0.002), labels = scales::percent_format(accuracy = 0.1)) +
  
  # Mappatura manuale dei colori per i tre scenari target
  scale_color_manual(
    values = c(
      "P10 (Sfavorevole)" = COLOR_PORT_P10, 
      "P50 (Mediano)"      = COLOR_PORT_P50, 
      "P90 (Favorevole)"   = COLOR_PORT_P90
    ),
    name = "Scenari PAC Monte Carlo"
  ) +
  
  # Applicazione dei limiti di sicurezza calcolati al punto 3
  expand_limits(x = c(X_MIN_LIMIT, X_MAX_LIMIT), y = c(Y_MIN_LIMIT, Y_MAX_LIMIT)) +
  
  # Iniezione parametri testuali strutturati
  labs(
    title    = TXT_RV_TITLE,
    subtitle = TXT_RV_SUBTITLE,
    x        = TXT_RV_X_LABEL,
    y        = TXT_RV_Y_LABEL,
    caption  = TXT_RV_CAPTION
  ) +
  
  # Ottimizzazione elementi del layout grafico
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = COLOR_RV_GRID, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = "gray30", margin = margin(b = 15)),
    plot.caption     = element_text(face = "italic", color = "gray40", size = 9, margin = margin(t = 15)),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "gray98", color = "gray85")
  )

# --- Output Grafico ---
print(p_rendimento_rischio)

