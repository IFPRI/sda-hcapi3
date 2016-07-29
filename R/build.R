
# Run this script to upate HCAPI3 web documentation.
x <- getOption("warn")
options(warn=-1)


library(staticdocs)
h <- list.files("./", "*.html")
file.remove(h)
build_site(pkg="../hc-api3", site_path="./", examples=T)
h <- list.files("./", "*.html")
p <- list.files("./", "*.png")
p <- setdiff(list.files(),
  c(h, p, "R", "js", "css", "icons", "img", "vignettes", "hc-api3-doc.Rproj"))
file.remove(p)

file.copy("../hc-api3/inst/www/staticdocs.css", "./css/staticdocs.css", overwrite=T)
options(warn=x)

# Check index.html manually
# Also remove warning messages