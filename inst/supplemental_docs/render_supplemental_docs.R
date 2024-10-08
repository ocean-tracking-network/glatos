library(rmarkdown)

render(input = "inst/supplemental_docs/receiver_efficiency_index_handout.Rmd", 
  output_dir = "vignettes",
  output_format = "all")

render(input = "inst/supplemental_docs/residence_index_handout.Rmd", 
  output_dir = "vignettes",
  output_format = "all")


render(input = "inst/supplemental_docs/detection_range_handout.Rmd", 
  output_dir = "vignettes", 
  output_format = "all")


#render html versions of vignettes
render(input = "vignettes/data_loading_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "all")
#output format -> make sure html_document is first in Rmd 

render(input = "vignettes/data_requirements_vignette.Rmd", 
  output_dir = "vignettes",
  output_format = "all")
#output format -> make sure html_document is first in Rmd 

render(input = "vignettes/detection_efficiency_vignette.Rmd", 
       output_dir = "vignettes",
       output_format = "all")
#output format -> make sure html_document is first in Rmd 
