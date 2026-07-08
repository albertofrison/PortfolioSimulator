# ==============================================================================
# CONFIGURAZIONE GLOBALE (Aggiunta parametri CPU e Colori)
# ==============================================================================
if (!require("parallel")) install.packages("parallel")
library(parallel)

parallel::detectCores()

# --- Configurazione Hardware ---
CORES_DISPONIBILI <- detectCores()
CORES_DA_USARE     <- CORES_DISPONIBILI - 2 # Lasciamo 2 core liberi per non bloccare il PC durante il calcolo

# --- Colori del Grafico Monte Carlo ---
COLOR_MC_FILL     <- "aquamarine2"
COLOR_MC_BORDER   <- "azure3"
COLOR_LINE_BE     <- "black"       # Linea Break-Even (Capitale Investito)
COLOR_P10         <- "red"         # Linea Percentile 10%
COLOR_P50         <- "orange"      # Linea Percentile 50%
COLOR_P90         <- "green4"      # Linea Percentile 90%


# ==============================================================================
# 8. SIMULATORE MONTE CARLO IN PARALLELO
# ==============================================================================
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
library(foreach)
library(doParallel)

# Configurazione del PAC
somma_iniziale       <- 30000
portafoglio_iniziale <- target_weights * somma_iniziale

orizzonte_mesi <- 240    
quota_mensile  <- 1000   
n_simulazioni  <- 100000   
nomi_asset     <- names(target_weights)

capitale_investito_totale <- sum(portafoglio_iniziale) + (orizzonte_mesi * quota_mensile)

# --- SUPER OTTIMIZZAZIONE 1: Trasformiamo il dataset in Matrice ---
# Questo trucco distrugge il collo di bottiglia dell'indicizzazione dei data.frame
matrice_rendimenti <- as.matrix(dataset_rendimenti[, nomi_asset])
n_righe_dataset    <- nrow(matrice_rendimenti)

# Attivazione del Cluster di Core
cat("Attivazione di", CORES_DA_USARE, "core per la simulazione...\n")
mio_cluster <- makeCluster(CORES_DA_USARE)
registerDoParallel(mio_cluster)

cat("Avvio di", n_simulazioni, "simulazioni Monte Carlo in corso...\n")
tempo_inizio <- Sys.time()

# --- SUPER OTTIMIZZAZIONE 2: Ciclo parallelo foreach ---
# Usiamo .combine = 'c' per concatenare i risultati finali di ogni simulazione in un unico vettore numerico
risultati_scenari <- foreach(s = 1:n_simulazioni, .combine = 'c') %dopar% {
  
  portafoglio <- portafoglio_iniziale
  
  for (m in 1:orizzonte_mesi) {
    # Usiamo sample.int che è molto più veloce di sample su matrici
    riga_estratta <- sample.int(n_righe_dataset, size = 1)
    rendimenti_estratti <- matrice_rendimenti[riga_estratta, ]
    
    # Capitalizzazione del portafoglio
    portafoglio <- portafoglio * (1 + rendimenti_estratti)
    
    valore_totale_pre     <- sum(portafoglio)
    valore_obiettivo_post <- valore_totale_pre + quota_mensile
    
    # Logica di ribilanciamento
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
  }
  
  # Restituiamo il valore finale di questo scenario al core principale
  return(sum(portafoglio))
}

tempo_fine <- Sys.time()
cat("Simulazione completata in:", round(difftime(tempo_fine, tempo_inizio, units = "secs"), 2), "secondi.\n")

# Spegniamo subito il cluster per liberare i processori!
stopCluster(mio_cluster)


# ==============================================================================
# 9. CREAZIONE DEL DATASET PER IL PLOT E CALCOLO DEI MULTIPLI / PERCENTILI
# ==============================================================================
df_scenari <- tibble(Valore_Finale = risultati_scenari) %>%
  mutate(Multiplo_Capitale = Valore_Finale / capitale_investito_totale)

# Calcolo dei percentili (Mancava nel codice precedente, fondamentale per i passaggi successivi!)
p10 <- quantile(risultati_scenari, 0.10)
p50 <- quantile(risultati_scenari, 0.50)
p90 <- quantile(risultati_scenari, 0.90)

n_anni <- orizzonte_mesi / 12


# ==============================================================================
# CALCOLO RIGOROSO DEL CAGR PER UN PAC
# ==============================================================================
if (!require("FinCal")) install.packages("FinCal")
library(FinCal)

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
# AGGIORNAMENTO ETICHETTE DINAMICHE PER IL GRAFICO
# ==============================================================================
fmt_euro <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

lbl_cap_investito <- paste0("Capitale Investito: ", fmt_euro(capitale_investito_totale), " €")
lbl_pessimo       <- paste0("Pessimo (10%): ", fmt_euro(p10), " € (CAGR: ", sprintf("%.2f%%", cagr_p10), ")")
lbl_mediano       <- paste0("Mediano (50%): ", fmt_euro(p50), " € (CAGR: ", sprintf("%.2f%%", cagr_p50), ")")
lbl_ottimo        <- paste0("Ottimo (90%): ", fmt_euro(p90), " € (CAGR: ", sprintf("%.2f%%", cagr_p90), ")")


# ==============================================================================
# GRAFICO MONTE CARLO UNIFORMATO CON VARIABILI GLOBALI
# ==============================================================================
p_montecarlo <- ggplot(df_scenari, aes(x = Valore_Finale)) +
  # Usiamo le variabili globali di riempimento e bordo
  geom_histogram(bins = 100, fill = COLOR_MC_FILL, color = COLOR_MC_BORDER, alpha = 0.8) +
  
  # Linea Break-even costante
  geom_vline(xintercept = capitale_investito_totale, color = COLOR_LINE_BE, linetype = "solid", linewidth = 1) +
  
  annotate("text", x = capitale_investito_totale, y = Inf, label = lbl_cap_investito, 
           angle = 90, vjust = -1, hjust = 1.1, color = COLOR_LINE_BE, size = 3.2, fontface = "bold") +
  
  # Linee verticali dei percentili mappate con i vettori di colore globali
  geom_vline(xintercept = c(p10, p50, p90), color = c(COLOR_P10, COLOR_P50, COLOR_P90), 
             linetype = "dashed", linewidth = 0.8) +
  
  # Annotazioni del testo abbinate ai rispettivi colori configurati
  annotate("text", x = p10, y = Inf, label = lbl_pessimo, 
           angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P10, size = 3.2, fontface = "bold") +
  
  annotate("text", x = p50, y = Inf, label = lbl_mediano, 
           angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P50, size = 3.2, fontface = "bold") +
  
  annotate("text", x = p90, y = Inf, label = lbl_ottimo, 
           angle = 90, vjust = -1, hjust = 1.1, color = COLOR_P90, size = 3.2, fontface = "bold") +
  
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "", suffix = " €", big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      transform = ~ . / capitale_investito_totale,
      name = "Multiplo del Capitale Investito (Valore Finale / Totale Versato)",
      labels = scales::label_number(suffix = "x", decimal.mark = ",")
    )
  ) +
  
  labs(
    title = "Distribuzione dei Valori Finali del Portafoglio (Monte Carlo)",
    subtitle = paste0("Simulazione su ", n_simulazioni, " (estrazione casuale rendimento mensile dal dataset) | Orizzonte: ", n_anni, " anni\n",
                      "Analisi dei percentili storici proiettati con piano di accumulo costante | Rendimenti al lordo della tassazione (pre-tax)"),
    x = "Controvalore Finale Portafoglio (€)",
    y = "Frequenza (Numero di Scenari)",
    caption = "Made in R and with love by Alberto Frison - Source data Yahoo Finance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "azure2", color = COLOR_MC_BORDER),
    strip.text       = element_text(face = "bold", color = "black", size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle    = element_text(color = "black", margin = margin(b = 15)),
    axis.title.x.top = element_text(color = "blue4", size = 10, face = "italic")
  )

print(p_montecarlo)
