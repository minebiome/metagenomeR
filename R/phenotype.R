

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






#' plot boxplot
#'
#'
#' @docType methods
#' @name boxplotContinuous
#' @rdname boxplotContinuous
#'
#' @param .data data frame
#' @param index 选择使用的表型
#' @param color 颜色选择，目前有"aaas","d3"两种
#' @param signif 是否需要展示差异检验（默认秩和检验）
#' @param comparlist 秩和检验分组情况
#'
#' @author YuDanZhang
#'
#' @examples
#'
#' Group age
#' A-pp  29
#' A-pp  36
#' A-pp  22
#' path = system.file("extdata","metadata.txt", package = "metagenomeR")
#' comparlist=list(c("A-pp","S-pp"),c("A-sp","S-sp"),c("A-pp","A-sp"),c("S-pp","S-sp"))
#' data <- read.table(path, header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)
#' boxplotContinuous(data,index="age",color="aaas", group="Group",signif=T,comparlist=comparlist)
#' boxplotContinuous(data,index="age", group="Group")
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme geom_jitter element_text element_rect element_blank element_line position_jitter
#' @importFrom dplyr rename
#' @importFrom ggpubr geom_signif
#' @importFrom ggsci scale_color_aaas scale_fill_aaas scale_color_d3 scale_fill_d3
#'
#' @export
boxplotContinuous <- function (.data,
                               index = NULL,
                                color="default",
                                group="Group",
                                signif=F,
                                comparlist=NULL)
{

  if (!all(c(index,group) %in% colnames(.data))) {
    stop("the argument 'index,group' should specify columns of colnames(.data)")
  }

  colorBy <- match.arg(color, c("aaas", "d3", "default"))

  data <- dplyr::rename(.data,group=group)
  # data <- dplyr::rename(data,idx=index)
  data$group<- factor(data$group,ordered = TRUE)


#
#
  # library(ggpubr)
#   library(ggsci)
#   library(ggrepel)


  p = ggplot(data, aes(x = group, y = .data[[index]], color = group)) +
    #隐藏异常值outlier.shape = NA，设置异常值大小outlier.size = 10，
    #size=0.7箱子外框线条粗细，width = 0.5箱子宽度，ransparent是盒子中间填充，全透明，必须和alpha = 1一起才能起作用
    geom_boxplot(alpha = 1,size = 1.2, width = 0.5,
                 fill = "transparent",outlier.shape = NA)+

    # geom_text_repel(label = paste(rownames(df)),
    #                 colour = "black", size = 1.5)+
    #设置坐标标签
    labs(x = "Groups", y = paste(index),color = group) +
    #背景变白theme_classic()
    #axis设置坐标轴相关
    theme(axis.text=element_text(size=12),
          #设置坐标轴标签大小和字体加粗
          axis.title.y=element_text(size=18,face="bold",colour = 'black'),
          axis.title.x=element_blank(),
          axis.text.x=element_text(family = "sans",colour = 'black',size=15),
          #设置x，y轴粗细
          axis.line = element_line(colour = 'black', size = 0.75),
          #设置分类标签位置
          legend.position = "top",
          #legend.background(color = "red"),
          #设置图例字体样式和大小
          text = element_text(family = "sans",size = 10,colour = 'black'),
          #修改背景颜色
          panel.background = element_rect(fill = 'white'),
          #修改图例legend背景颜色
          legend.key = element_rect(fill = 'white'))+
    #添加抖动点
    geom_jitter(position = position_jitter(0.17), size = 1,alpha = 0.7,shape=16)

  if(signif==T){
    if(is.null(comparlist)){
      stop("comparlist can't be NULL")
    }
    p=p+
      #label.x/y调整显著性标记位置，aes(label=..p.signif..)仅显示显著性水平
      geom_signif(comparisons = comparlist,
                  #显示数字还是显示*
                  map_signif_level = c("***"=0.001,"**"=0.01, "*"=0.05, " "=2),
                  #用哪种检验
                  test =wilcox.test,
                  #设置标注之间的距离
                  step_increase = 0.1,
                  #线条下面长度
                  tip_length=0,
                  #线条粗细
                  size=0.9,
                  #设置颜色
                  col = "black",
                  #上面标注大小
                  textsize = 7,
                  #调整上标上下幅度
                  vjust = 0.65,
                  position = "identity",
                  #调整上标左右幅度
                  #hjust = 0.7,
                  #xmin=2.2, xmax=3#设置整体在左右的位置
                  #设置整体在y轴的位置
                  #y_position = ymax
      )+
      geom_signif(comparisons = comparlist,
                  #显示数字还是显示*
                  map_signif_level = F,
                  #用哪种检验
                  test =wilcox.test,
                  #设置标注之间的距离
                  step_increase = 0.1,
                  #线条下面长度
                  tip_length=0,
                  #线条粗细
                  size=0.9,
                  #设置颜色
                  col = "black",
                  #上面标注大小
                  textsize = 4,
                  #调整上标上下幅度
                  vjust = 1.65,
                  position = "identity",
                  #调整上标左右幅度
                  #hjust = 0.7,
                  #xmin=2.2, xmax=3#设置整体在左右的位置
                  #设置整体在y轴的位置
                  #y_position = ymax
      )
  }
  if(colorBy=="aaas"){
    p=p+
      scale_color_aaas(a=0.55)+
      scale_fill_aaas(a=0.55)
  }
  if(colorBy=="d3"){
    p=p+
      scale_color_d3(a=0.55)+
      scale_fill_d3(a=0.55)
  }
  p

}













