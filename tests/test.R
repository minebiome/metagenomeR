# library(tidyverse)
# library("vegan")

pt <- phenotypeTest(system.file("extdata","metadata.txt", package = "metagenomeR"), continuous=c('age'))


comparlist=list(c("A-pp","S-pp"),c("A-sp","S-sp"),c("A-pp","A-sp"),c("S-pp","S-sp"))
data <- read.table(system.file("extdata","metadata.txt", package = "metagenomeR"), header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)

continuous  <- c("age")
mclapply(continuous, function(con){
  # message(con)
  p = boxplotContinuous(data,index=con,color="aaas", group="Group",signif=T,comparlist=comparlist)

},mc.cores=6)


mres <- readMetaphlan(system.file("extdata","metaphlan", package = "metagenomeR"))
metadata <- read.table(system.file("extdata","metadata.txt", package = "metagenomeR"), header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)
permanova(mres, metadata,group = c("A-pp","S-pp"),parallel=T)

