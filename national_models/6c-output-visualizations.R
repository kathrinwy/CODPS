##================================================================
## Project: COD-PS Assessment and Construction, Niger
## Script purpose: Output visualizations
##
## Date created: 16 September 2019
## Last updated: 16 September 2019
##
## Author: Kathrin Weny
## Maintainers: Kathrin Weny, Romesh Silva

# Combine -----------------------------------------------------------------

BFA.figures <- "G:/My Drive/2019/3- Humanitarian data/COD-PS/pop_est/output/plots/BFA"

setwd(BFA.figures )

rl <- lapply(list("WRA_total", "Youth_total",
                  "subnat.pyramids",
                  "pop.trajectoriesI", "pop.trajectoriesII", "pop.trajectoriesIII"), png::readPNG)

gl <- lapply(rl, grid::rasterGrob)

do.call(gridExtra::grid.arrange, gl)


# OR

plot(0:2, 0:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rasterImage(readPNG(source="WRA_total.png"), 0, 1, 1, 2)
rasterImage(readPNG(source="Youth_total.png"), 1, 1, 2, 2)
rasterImage(readPNG(source="subnat.pyramids.png"), 0, 0, 1, 1)
rasterImage(readPNG(source="pop.trajectoriesI.png"), 1, 0, 2, 1)

rasterImage(readPNG(source="pop.trajectoriesII.png"), 0, 0, 1, 1)
rasterImage(readPNG(source="pop.trajectoriesIII.png"), 1, 0, 2, 1)

# write to PDF
ggsave(file = paste("plots/BFA/WRA_total.pdf"), print(plot.label), dpi = 900)