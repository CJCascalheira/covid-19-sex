# Dependencies
library(tidyverse)
library(knitr)
library(bibtex)

# Export citation - R software
write_bib(
  x = "base",
  file = "doc/bib/R-base.bib"
)

# Current version of RStudio
RStudio.Version()

# Export citation - tidyverse
write.bib(
  entry = citation("tidyverse"), 
  file = "doc/bib/R-tidyverse.bib", 
  append = FALSE, 
  verbose = TRUE
)
