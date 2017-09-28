# data0 <- structure_date (S_Day=1,S_Month=4,S_Year=7)
library(sinew)
library(FlowRegEnvCost)

roxygen2::roxygenise()
devtools::check(build_args = '--as-cran')
# symbolic link of extdata when documenting
# sudo add-apt-repository ppa:anton+/photo-video-apps
#R CMD build --compact-vignettes='gs+qpdf' FlowRegEnvCost
R CMD build FlowRegEnvCost
#R CMD check --as-cran FlowRegEnvCost_0.1.0.tar.gz

