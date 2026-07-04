# ==============================================================================
# 8. SIMULATORE MONTE CARLO (BOOTSTRAP) CON PORTAFOGLIO REALE E PAC
# ==============================================================================
library(tidyverse)

# 1. Configurazione del Portafoglio Reale Attuale (Base ~30k € da image_a15d04.png)
# Inseriamo i controvalori attuali precisi in Euro per far partire la simulazione la situazione odierna
portafoglio_iniziale <- c(
  "Bond_Global"     = 5829.00, # Corrisponde al ~19.43% di 30k
  "World_ex_USA"    = 5244.00, # Corrisponde al ~17.48% di 30k
  "World_Equal_W"   = 3846.00, # Corrisponde al ~12.82% di 30k
  "SP500"           = 3477.00, # Corrisponde al ~11.59% di 30k
  "Emerging_Markets"= 2487.00, # Corrisponde al ~08.29% di 30k
  "World_Value"     = 2217.00, # Corrisponde al ~07.39% di 30k
  "World_Mid_Cap"   = 2034.00, # Corrisponde al ~06.78% di 30k
  "Gold"            = 1881.00, # Corrisponde al ~06.27% di 30k
  "World_Momentum"  = 1641.00, # Corrisponde al ~05.47% di 30k
  "World_Small_Cap" = 1344.00  # Corrisponde al ~04.48% di 30k
)

# Parametri Simulazione
orizzonte_mesi <- 360    # Durata della simulazione (es. 10 anni di PAC)
quota_mensile  <- 1000   # PAC mensile
n_simulazioni  <- 10000    # Numero di "vite parallele" (scenari) da simulare
nomi_asset     <- names(target_weights)

# Matrice per salvare il valore finale di ogni singola simulazione
risultati_scenari <- numeric(n_simulazioni)

set.seed(42) # Per rendere i risultati riproducibili

# ------------------------------------------------------------------------------
# AVVIO DELLA SIMULAZIONE MONTE CARLO
# ------------------------------------------------------------------------------
for (s in 1:n_simulazioni) {
  
  # Si parte ogni volta col tuo portafoglio attuale reale
  portafoglio <- portafoglio_iniziale
  
  for (m in 1:orizzonte_mesi) {
    
    # STEP 2: Estrazione a caso di un mese storico (mantiene le correlazioni tra asset)
    riga_estratta <- sample(1:nrow(dataset_rendimenti), size = 1)
    rendimenti_estratti <- as.numeric(dataset_rendimenti[riga_estratta, nomi_asset])
    names(rendimenti_estratti) <- nomi_asset
    
    # Ricalcolo del valore del portafoglio dopo il movimento di mercato
    portafoglio <- portafoglio * (1 + rendimenti_estratti)
    
    # STEP 3: PAC mensile con acquisto intelligente (Smart Inflow sui 1.000€)
    valore_totale_pre <- sum(portafoglio)
    valore_obiettivo_post <- valore_totale_pre + quota_mensile
    
    target_ideale <- valore_obiettivo_post * target_weights
    distanza_da_target <- target_ideale - portafoglio
    quote_da_comprare <- ifelse(distanza_da_target > 0, distanza_da_target, 0)
    
    if (sum(quote_da_comprare) > 0) {
      pesi_acquisto <- quote_da_comprare / sum(quote_da_comprare)
      acquisti_effettivi <- pesi_acquisto * quota_mensile
    } else {
      acquisti_effettivi <- target_weights * quota_mensile
    }
    
    # Aggiornamento finale del mese con l'iniezione del PAC
    portafoglio <- portafoglio + acquisti_effettivi
  }
  
  # Salviamo il valore finale del portafoglio per lo scenario 's'
  risultati_scenari[s] <- sum(portafoglio)
}

# ==============================================================================
# ANALISI STATISTICA DEGLI SCENARI FUTURI
# ==============================================================================
capitale_investito_totale <- sum(portafoglio_iniziale) + (orizzonte_mesi * quota_mensile)

# ==============================================================================
# STAMPA RISULTATI PULITA (SENZA WARNING VALUTARI)
# ==============================================================================

cat("\n==================================================\n")
cat("   RISULTATI PROIEZIONE MONTE CARLO (BOOTSTRAP)   \n")
cat("==================================================\n")
cat("Capitale Iniziale (Oggi):     ", format(round(sum(portafoglio_iniziale)), big.mark=".", decimal.mark=","), "€\n")
cat("Anni di PAC simulati:         ", orizzonte_mesi / 12, "anni\n")
cat("Capitale Totale Investito:    ", format(capitale_investito_totale, big.mark=".", decimal.mark=","), "€\n\n")
cat("SCENARIO PESSIMO (Percentile 10%): ", format(round(quantile(risultati_scenari, 0.10)), big.mark=".", decimal.mark=","), "€\n")
cat("SCENARIO MEDIANO (Percentile 50%): ", format(round(quantile(risultati_scenari, 0.50)), big.mark=".", decimal.mark=","), "€\n")
cat("SCENARIO OTTIMO  (Percentile 90%): ", format(round(quantile(risultati_scenari, 0.90)), big.mark=".", decimal.mark=","), "€\n")
cat("==================================================\n")
