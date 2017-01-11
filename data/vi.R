load("/home/projects/hc-cell5m/rdb/latest/vi.rda")
data.table::setkey(vi, varCode)
vi[published==TRUE]
