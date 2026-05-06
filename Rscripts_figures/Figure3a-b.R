library(grid)
library(jpeg)

img_a <- readJPEG("../Figures/Figure3a.jpeg")
img_b <- readJPEG("../Figures/Figure3b.jpeg")


dpi      <- 300
out_w_px <- 2125   


aspect_a  <- dim(img_a)[1] / dim(img_a)[2]   
aspect_b  <- dim(img_b)[1] / dim(img_b)[2]

out_h_a_px <- round(out_w_px * aspect_a)
out_h_b_px <- round(out_w_px * aspect_b)
out_h_px   <- out_h_a_px + out_h_b_px


frac_a <- out_h_a_px / out_h_px
frac_b <- out_h_b_px / out_h_px

jpeg(
  filename = "../Figures/Figure3a-b.jpeg",
  width    = out_w_px,
  height   = out_h_px,
  res      = dpi,
  bg       = "white"
)

grid.newpage()

# ── Panel a) ──
pushViewport(viewport(x = 0, y = frac_b, width = 1, height = frac_a,
                      just = c("left", "bottom")))
grid.raster(img_a, width = unit(1, "npc"), height = unit(1, "npc"),
            interpolate = TRUE)
grid.text("a)", x = unit(0.012, "npc"), y = unit(0.98, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 18, fontface = "bold",        
                    fontfamily = "Helvetica", col = "#222222"))
popViewport()

# ── Panel b) ──
pushViewport(viewport(x = 0, y = 0, width = 1, height = frac_b,
                      just = c("left", "bottom")))
grid.raster(img_b, width = unit(1, "npc"), height = unit(1, "npc"),
            interpolate = TRUE)
grid.text("b)", x = unit(0.012, "npc"), y = unit(0.98, "npc"),
          just = c("left", "top"),
          gp = gpar(fontsize = 18, fontface = "bold",
                    fontfamily = "Helvetica", col = "#222222"))
popViewport()

dev.off()
cat(sprintf("Saved: ...\n",
            out_w_px, out_h_px))
