
# Run this script to upate HCAPI3 web documentation.
library(staticdocs)
build_site(pkg="../hc-api3", site_path="./")
h <- list.files("./", "*.html")
p <- list.files("./", "*.png")
p <- setdiff(list.files(),
  c(h, p, "R", "js", "css", "icons", "img", "vignettes", "hc-api3-doc.Rproj"))
file.remove(p)
