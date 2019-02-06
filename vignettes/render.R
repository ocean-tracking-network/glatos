library(rmarkdown)
library(tint)

render(input = "~/Documents/glatos/vignettes/detection_range_vignette.Rmd", output_file = "detection_range_vignette.pdf", output_format = tint::tintPdf(highlight = "haddock"), clean = FALSE)


