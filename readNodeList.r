setwd("c:/rcode/test1")
nodelist <- read.table(file="Nodelist",col.names=c("nodename","iP"),sep="\t")
nodelist <- droplevels(nodelist)