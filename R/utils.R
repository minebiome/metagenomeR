
checkMetadataColname <- function(.data,group="Group",cnames=c()){
  if (!all(c(group,cnames) %in% colnames(metadata))) {
    stop(paste0("the argument '",c(group,cnames),"' should specify columns of colnames(metadata)"))
  }
  data <- dplyr::rename(.data,group=group)
  data
}
