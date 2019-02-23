library(rmarkdown)
library(tint)

render(input = "~/Documents/glatos/vignettes/gganimate_vignette.Rmd", output_file = "gganimate.pdf",
       output_format = tint::tintPdf(highlight = "haddock"), clean=TRUE)

