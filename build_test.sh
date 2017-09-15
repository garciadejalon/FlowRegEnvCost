roxygen2::roxygenise()
# symbolic link of extdata when documenting
# sudo add-apt-repository ppa:anton+/photo-video-apps
#R CMD build --compact-vignettes='gs+qpdf' FlowRegEnvCost
R CMD build FlowRegEnvCost
R CMD check --as-cran FlowRegEnvCost_0.1.0.tar.gz
