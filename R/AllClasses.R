
##' Class "metaphlanResult"
##' This class represents the result of metaphlan analysis.
##'
##'
##' @name metaphlanResult-class
##' @aliases metaphlanResult-class
##'
##' @docType class
##' @slot phylum 门
##' @exportClass metaphlanResult
##' @author YangWang
##' @keywords classes
setClass("metaphlanResult",
         representation=representation(
           phylum         = "data.frame",
           clazz         = "data.frame",
           order         = "data.frame",
           family         = "data.frame",
           genus         = "data.frame",
           species         = "data.frame"
         ),
         prototype=prototype()
)

# 界（Kingdom）、门（Phylum）、纲（Class）、目（Order）、科（Family）、属（Genus）、种（Species）
setMethod("show","metaphlanResult",function(object){
  cat("phylum(门) nrow:",nrow(object@phylum)," ncol:",ncol(object@phylum),"\n")
  cat("clazz(纲) nrow:",nrow(object@clazz)," ncol:",ncol(object@clazz),"\n")
  cat("order(目) nrow:",nrow(object@order)," ncol:",ncol(object@order),"\n")
  cat("family(科) nrow:",nrow(object@family)," ncol:",ncol(object@family),"\n")
  cat("genus(属) nrow:",nrow(object@genus)," ncol:",ncol(object@genus),"\n")
  cat("species(种) nrow:",nrow(object@species)," ncol:",ncol(object@species),"\n")
})

# setGeneric("[",function(obj,...) standardGeneric("metaphlanResult"))
setMethod("[","metaphlanResult",function(x, i, j, ..., drop){
  mres <- new("metaphlanResult")
  mres@phylum <-  x@phylum[i,j]
  mres@clazz <-  x@clazz[i,j]
  mres@order <-  x@order[i,j]
  mres@family <-  x@family[i,j]
  mres@genus <-  x@genus[i,j]
  mres@species <-  x@species[i,j]
  mres
})

setMethod("ncol","metaphlanResult",function(x){
  ncol(x@clazz)
})
setMethod("colnames","metaphlanResult",function(x){
  colnames(x@clazz)
})

setMethod("as.list","metaphlanResult",function(x,cl_list = c("species", "genus", "class", "family", "order", "phylum")){
  res  <- lapply(cl_list, function(name){
    if(name=="class"){
      name <- "clazz"
    }
    slot(x,name)
  })
  names(res) <- cl_list
  res
})


