nodelist <- read.table(file="Nodelist",col.names=c("nodename","iP"),sep="\t")
nodelist <- droplevels(nodelist)
temp <- ""
for(i in 1:dim(nodelist)[1]){
  temp <- paste(temp,'"', nodelist$nodename[i], '"', " =", i)
  if(i != dim(nodelist)[1])
    temp <- paste(temp, ",")
}

list <- list()

temp <- paste("nodeList <- list(",temp,")")
eval(parse(text=temp))