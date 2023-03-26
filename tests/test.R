# library(tidyverse)
library("vegan")
mres <- readMetaphlan(system.file("extdata","metaphlan", package = "metagenomeR"))
metadata <- read.table(system.file("extdata","metadata.txt", package = "metagenomeR"), header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)
permanova(mres, metadata,group = c("A-pp","S-pp"),parallel=T)


