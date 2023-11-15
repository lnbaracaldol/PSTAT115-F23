## devtools::install_github("an-bui/calecopal")

suppressPackageStartupMessages(library(wesanderson))
zissou3 <- wes_palette("Zissou1", type="discrete")
col_zissou <- wesanderson::wes_palettes$Zissou1[c(1, 3, 5)]

cols5 <- colorspace::qualitative_hcl(5, "Set 2")
# cols5 <- RColorBrewer::brewer.pal(5, "Set1")
cols2 <- cols5[c(1, 3)]

cols3 <- c("#F8766D","#00BFC4","#7CAE00")

cols_sequential <- function(n) colorspace::sequential_hcl(n+1, "Greens")[1:n]
cols_diverging <- function(n) colorspace::sequential_hcl(n, "BluGrn")

