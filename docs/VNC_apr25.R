
# 1. Load the VNC function/data from Gries & Hilpert
load("vnc.individual.RData")

# ---------------------------------------------------------------
# 2. Generate Spanish VNC plots
# ---------------------------------------------------------------
# NOTE: After running vnc.individual(), press ENTER in the console
#       to advance through the interactive plotting sequence.

# ---- First plot (dendrogram) ----
vnc.individual("esp_vnc_caus_70y_p.txt")
p1 <- recordPlot()

png("esp_vnc_caus_70y_p_dendogram.png",
    width = 6, height = 4, units = "in", res = 600)
replayPlot(p1)
dev.off()

# Clear all plot history before generating second plot
graphics.off()   # closes all plot devices

# ---- Second plot (scree plot) ----
# Must rerun vnc.individual() for the second plot
vnc.individual("esp_vnc_caus_70y_p.txt")
p2 <- recordPlot()

png("esp_vnc_caus_70y_p_screeplot.png",
    width = 6, height = 4, units = "in", res = 600)
replayPlot(p2)
dev.off()

# Clear everything before moving to next dataset
rm(list = ls())
graphics.off()

# ---------------------------------------------------------------
# 3. Generate Italian VNC plots
# ---------------------------------------------------------------

# ---- First plot (dendrogram) ----
vnc.individual("ita_vnc_caus_70y_p.txt")
p3 <- recordPlot()

png("ita_vnc_caus_70y_p_dendogram.png",
    width = 6, height = 4, units = "in", res = 600)
replayPlot(p3)
dev.off()

# Clear all plot history before generating second plot
graphics.off()

# ---- Second plot (scree plot) ----
vnc.individual("ita_vnc_caus_70y_p.txt")
p4 <- recordPlot()

png("ita_vnc_caus_70y_p_screeplot.png",
    width = 6, height = 4, units = "in", res = 600)
replayPlot(p4)
dev.off()

# ---------------------------------------------------------------
# 4. Final cleanup
# ---------------------------------------------------------------
rm(list = ls())
graphics.off()
