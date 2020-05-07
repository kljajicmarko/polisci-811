################Master################

library(here)
library(rmarkdown)

analysis_script<-here("scripts","analysis.R")
cleaning_script<-here("scripts","cleaning.R")
writeup<-here("writing","marko-oliver_final-proj.Rmd")

source(analysis_script)
source(cleaning_script)
rmarkdown::render(writeup,output_format = "all")