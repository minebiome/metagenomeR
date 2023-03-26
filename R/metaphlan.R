


#' readMetaphlan
#'
#'
#' @docType methods
#' @name readMetaphlan
#' @rdname readMetaphlan
#'
#' @param path metaphlan输出的文件夹
#' @param checkColnames 检测样本名称是否相同
#'
#' @author YangWang
#'
#' @examples
#'
#' path = system.file("extdata","metaphlan", package = "metagenomeR")
#' mres <- readMetaphlan(path)
#'
#' @export
readMetaphlan <- function(path,checkColnames= T,cl_list = c("species", "genus", "class", "family", "order", "phylum")){
  # 界（Kingdom）、门（Phylum）、纲（Class）、目（Order）、科（Family）、属（Genus）、种（Species）
  files <- list.files(path,pattern = "*class.txt|*family.txt|*genus.txt|*order.txt|*phylum.txt|*species.txt", full.names = T)
  mres <- new("metaphlanResult")
  lapply(files, function(path){
    m <- regexec("*class|*family|*genus|*order|*phylum|*species", path)
    var <- regmatches(path, m)[[1]]
    if(var=="class"){
      var <- "clazz"
    }
    if(!.hasSlot(mres, var)){
      stop(paste0(var," doesn't exist!"))
    }
    otutab = read.table(path, header=T, row.names=1, sep="\t", comment.char="",check.names=F)
    slot(mres, var) <<- otutab
    var
    # path
  })
  cnames <- lapply(cl_list, function(name){
    if(name=="class"){
      name <- "clazz"
    }
    colnames(slot(mres,name))
  })
  if(checkColnames &&  !all(unlist(Map(identical,list( cnames[[1]]),cnames)))){
    stop("样本名称不同，或者样本名称顺序不同")
  }
  mres
}

#' permanova
#'
#'
#' @docType methods
#' @name permanova
#' @rdname permanova
#'
#' @param .data metaphlanResult对象
#' @param .metadata .metadata
#' @param group group
#' @param groupColName groupColName
#' @param parallel parallel
#'
#' @author YangWang
#'
#' @examples
#'
#' mres <- readMetaphlan(system.file("extdata","metaphlan", package = "metagenomeR"))
#' metadata <- read.table(system.file("extdata","metadata.txt", package = "metagenomeR"), header=T, row.names=1, sep="\t", comment.char="",check.names=F, stringsAsFactors = F)
#' permanova(mres, metadata,group = c("A-pp","S-pp"),parallel=T)
#'
#' @importFrom vegan adonis2
#' @importFrom parallel mclapply
#'
#' @export
setGeneric("permanova",function(.data,.metadata,group,groupColName="Group",parallel=T) standardGeneric("permanova"))
setMethod("permanova","metaphlanResult",function(.data,.metadata,group,groupColName,parallel){
  permanovaS3(.data,.metadata,group,groupColName,parallel)
})

permanovaS3 <- function(.data,.metadata,group,groupColName="Group",parallel=T){
  metadata <- checkMetadataColname(.metadata,groupColName)
  metadata <- metadata[order(metadata$group,decreasing = TRUE),,drop=FALSE]
  metadata <- metadata[metadata$group %in% group,]
  samples <- rownames(metadata)
  mressub <- mres[,samples]
  if(ncol(mressub) != nrow(metadata)){
    stop("metadata的样本名称和data的样本名称不同")
  }
  mc <- getOption("mc.cores", 6)
  mressub_list <- as.list(mressub)
  if(parallel){
    res <- mclapply(names(mressub_list), function(name){
      message(name)
      count <- mressub_list[[name]]
      otu_adonis <- adonis2(as.formula(paste0("t(count) ~",paste0(c(colnames(metadata)),collapse = "+"))), data = metadata, permutations = 9999, method = "bray",by="margin")
    },mc.cores = mc)
  }else{
    res <- lapply(names(mressub_list), function(name){
      message(name)
      count <- mressub_list[[name]]
      otu_adonis <- adonis2(as.formula(paste0("t(count) ~",paste0(c(colnames(metadata)),collapse = "+"))), data = metadata, permutations = 9999, method = "bray",by="margin")
    })
  }
  names(res) <- names(mressub_list)
  res
}











