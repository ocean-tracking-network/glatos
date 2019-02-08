library(rmarkdown)
library(tint)

render(input = "~/Documents/GLATOS_vignettes/src/gganimate.Rmd", output_file = "gganimate.pdf", output_format = tint::tintPdf(highlight = "haddock"), clean=TRUE)

