
# Run this script to upate HCAPI3 web documentation.
library(staticdocs)
build_site(pkg="../hc-api3", site_path="./")
p <- list.files("./", "*.png")
file.rename(p, paste("./img", p, sep="/"))
p <- setdiff(list.files(no..=T), list.files("./", "*.html"))
p <- setdiff(p, c("R", "js", "css", "icons", "img", "vignettes", "hc-api3-doc.Rproj"))
file.remove(p)
