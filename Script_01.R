# ==============================================================================
# BACKTESTING PORTAFOGLIO CON PAC E RIBILANCIAMENTO COSTANTE
# FASE CARICAMENTO DATI STORICI PER ETF / BENCHMARK
# ==============================================================================

#0. Pulizia ambiente
rm(list=ls())

# 0. Caricamento librerie necessarie
if (!require("tidyquant")) install.packages("tidyquant")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

library(tidyverse)
library(tidyquant)
library(tidyverse)

# ==============================================================================
# 1. DEFINIZIONE DEL PORTAFOGLIO TARGET
# ==============================================================================
# 1. Definizione dei pesi del Portafoglio Target
target_weights <- c(
  "Bond_Global"     = 0.1943, #Bong Aggregate Globali
  "World_ex_USA"    = 0.1748, #MSCI World Ex USA
  "World_Equal_W"   = 0.1282, #MSCI World Equal Weight - massimo diversificatore
  "SP500"           = 0.1159, #SP500
  "Emerging_Markets"= 0.0829, #MSCI Mercati Emergenti
  "World_Value"     = 0.0739, #MSCI World Value (Value)
  "World_Mid_Cap"   = 0.0678, #MSCI World Mid Cap (Size)
  "Gold"            = 0.0627, #ETC ORO
  "World_Momentum"  = 0.0547, #MSCI World Momentum (Momentum)
  "World_Small_Cap" = 0.0448  #MSCI World Small Cap (Size)
)  

# Verifica che la somma sia 1 (100%)
sum(target_weights) 


# ==============================================================================
# 2. MAPPATURA TICKER YAHOO FINANCE (ETF EQUIVALENTI IN EUR)
# ==============================================================================
# Scegliamo i ticker storici di riferimento quotati sulle borse europee (Xetra/Milano)
tickers <- c(
  "Bond_Global"     = "EUNH.DE",  # CORRETTO: iShares Global Agg Bond EUR Hedged (In EUR)
  "World_ex_USA"    = "EXUS.DE",  # Xtrackers MSCI World ex USA (In EUR)
  "World_Equal_W"   = "XDEW.DE",  # Xtrackers MSCI World Equal Weight (In EUR)
  "SP500"           = "SXR8.DE",  # iShares Core S&P 500 (In EUR)
  "Emerging_Markets"= "IS3N.DE",  # iShares Core MSCI EM IMI (In EUR)
  "World_Value"     = "XDEV.DE",  # Xtrackers MSCI World Value Factor (In EUR)
  "World_Mid_Cap"   = "IS3M.DE",  # iShares Edge MSCI World Size Factor (In EUR)
  "Gold"            = "XGDU.DE",  # CORRETTO: iShares Physical Gold su Xetra (In EUR)
  "World_Momentum"  = "XDEM.DE",  # Xtrackers MSCI World Momentum (In EUR)
  "World_Small_Cap" = "IUSN.DE"   # iShares MSCI World Small Cap (In EUR)
)

# Mappa per ricongiungere i ticker ai nomi descrittivi delle asset class
mappa_nomi <- tibble(symbol = tickers, asset = names(tickers))

# ==============================================================================
# 3. DOWNLOAD DATI E CALCOLO RENDIMENTI MENSILI
# ==============================================================================
cat("Scaricamento dati storici da Yahoo Finance...\n")

# Creiamo una lista per contenere i rendimenti mensili di ciascun asset
lista_rendimenti <- list()

for (asset_name in names(tickers)) {
  ticker <- tickers[asset_name]
  cat("Download di:", asset_name, "(", ticker, ")... ")
  
  # Utilizziamo tryCatch per evitare che l'intero script si blocchi se un ticker fallisce
  tryCatch({
    # Download dei prezzi storici adjusted
    prezzi <- getSymbols(ticker, src = "yahoo", from = "2015-01-01", auto.assign = FALSE)
    prezzi_adj <- Ad(prezzi) # Estrae solo i prezzi Adjusted (con dividendi)
    
    # Calcolo dei rendimenti mensili
    rend_mensili <- monthlyReturn(prezzi_adj, leading = FALSE)
    colnames(rend_mensili) <- asset_name
    
    # Salviamo nel nostro archivio
    lista_rendimenti[[asset_name]] <- rend_mensili
    cat("OK!\n")
  }, error = function(e) {
    cat("⚠️ ERRORE sul ticker:", ticker, "\n")
  })
}

# Uniamo tutti i rendimenti in un unico oggetto xts basato sulle date comuni
cat("\nAllineamento dei dataset...\n")
rendimenti_xts <- do.call(merge, lista_rendimenti)

# Convertiamo in un comodo tibble (tidyverse) pronto per il nostro algoritmo PAC
dataset_rendimenti <- as.data.frame(rendimenti_xts) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = as.Date(date)) %>%
  drop_na() # Mantiene solo la finestra temporale comune a tutti e 10 gli strumenti

cat("\nDownload completato con successo. Finestra temporale comune trovata:\n")
print(range(dataset_rendimenti$date))
print(head(dataset_rendimenti))
