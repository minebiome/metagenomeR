

#' Statistical differences in phenotypic grouping were calculated
#'
#'
#' @docType methods
#' @name phenotypeTest
#' @rdname phenotypeTest
#'
#' @param path The metadata file path
#' @param continuous continuous variable
#' @param discrete discrete variable
#'
#' @author YuDanZhang
#'
#' @examples
#'
#' pt <- phenotypeTest(system.file("extdata","metadata.txt", package = "metagenomeR"), continuous=c('age'))
#'
#' @export
phenotypeTest <- function(path,continuous=NULL,discrete=NULL){
  diversity <- read.table(path, header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)

  out1 <-NA
  if (!is.null(continuous)){

    len1 <- length(continuous)
    out <- matrix(NA,  len1, 2)          #一个9*111的空矩阵
    all10data<- NA

    for (con in continuous){
      # print(con)
      #rm(list=ls())
      #这里整个都要手动改
      #diversity <- na.omit(metadata)
      #View(diversity)
      # diversity

      # gender.p <- kruskal.test(gender ~ Group, data = diversity)$p.value
      diversity0 <- diversity[,c("Group",con)]
      colnames(diversity0)<- c("Group","con")
      con.p <- kruskal.test(con ~ Group, data = diversity0)$p.value
      res <- cbind(con,con.p)
      all10data <- rbind(all10data,res)
    }

    out<-all10data
    out1 <- data.frame(out)
    out1 = out1[-1,]
    colnames(out1)<- c("phenotype","pvalue")
  }


  out3 <-NA
  if (!is.null(discrete)){
    len2 <- length(discrete)
    out2 <- matrix(NA,  len2,2)          #一个9*111的空矩阵
    all10data2<- NA
    for (dis in discrete){
      # print(dis)
      #rm(list=ls())
      #这里整个都要手动改
      #diversity <- na.omit(metadata)
      #View(diversity)
      #diversity
      # library(tidyverse)

      # gender.p <- kruskal.test(gender ~ Group, data = diversity)$p.value
      diversity0 <- diversity[,c("Group",dis)]
      colnames(diversity0)<- c("Group","dis")
      dis.p <- fisher.test(xtabs(~dis+Group,data=diversity0))$p.value
      res2 <- cbind(dis,dis.p)
      all10data2 <- rbind(all10data2,res2)
    }
    out2<-all10data2
    out3 <- data.frame(out2)
    out3 = out2[-1,]
    colnames(out3)<- c("phenotype","pvalue")
  }

  out4 <- rbind(out1,out3)
  out4 <-  na.omit(out4)
}

