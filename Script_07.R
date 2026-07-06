# ==============================================================================
# CREAZIONE DIRETTA DEL PDF PER IL CAROSELLO LINKEDIN
# ==============================================================================
library(ggplot2)
library(cowplot)

# 1. Definiamo il percorso del file di output
file_pdf <- "Carosello_Analisi_Portafoglio.pdf"

# 2. Apriamo il device PDF impostando le dimensioni delle slide (11x7 pollici)
pdf(file = file_pdf, width = 11, height = 7, onefile = TRUE)

# ------------------------------------------------------------------------------
# PAGINA 1: Gli Istogrammi dei Rendimenti Mensili Storici
# ------------------------------------------------------------------------------
# p_grafico è l'oggetto ggplot dei tuoi 10 facet sviluppato al punto 7
# testo_legenda è la stringa con la mappa dei sottostanti
#grid::grid.newpage() # Pulisce la pagina corrente

print(p_grafico)


print(p_cumulativo)


# ------------------------------------------------------------------------------
# PAGINA 2: La Matrice di Correlazione Professionale (Mappa RdYlBu)
# ------------------------------------------------------------------------------
# Assicurati di salvare la heatmap del punto 13 in un oggetto (es. p_corr)
# Se non l'avevi salvata in un oggetto, basta racchiudere il ggplot tra parentesi o assegnarlo:
# p_corr <- ggplot(df_correlazione_long, aes(...)) + ...
print(p_corr)

# ------------------------------------------------------------------------------
# PAGINA 3: L'Istogrammi della Simulazione Monte Carlo
# ------------------------------------------------------------------------------
# Assicurati di salvare il grafico finale del Monte Carlo in un oggetto (es. p_montecarlo)
print(p_montecarlo)

# 3. Chiudiamo il device grafico salvando definitivamente il file
dev.off()

cat("\n[OK] PDF generato con successo nella tua cartella di lavoro:", file_pdf, "\n")
