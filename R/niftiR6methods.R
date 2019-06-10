#niftiR6methods (make S3 Methods for R6 classes for common tasks)

#access array element of niftiDataR6 object
#' @export
`[.niftiDataR6` <- function(x,...) {
  x$data[...]
}

#set value of array element of niftiDataR6 object
#' @export
`[<-.niftiDataR6` <- function(x,...,value) {
  x$data[...] <- value
  return(invisible(x))
}

#show niftiDataR6 object
#' @export
print.niftiDataR6 <- function(x,...) {
  x$show()
}

#get dimensions of data
#' @export
dim.niftiDataR6 <- function(x,...) {
  dim(x$data)
}

#plot niftiDataR6 object
#' @export
plot.niftiDataR6 <- function(x,...) {
  x$plotNifti(...)
}

#plot niftiDataR6 object
#' @export
summary.niftiDataR6 <- function(x,...) {

  #get distribution stats
  nozero <- x$data[-which(x$data==0)]
  nvox <- prod(dim(x$data))

  dist <- rbind(quantile(x$data,seq(0,1,by=.25)),
                quantile(nozero,seq(0,1,by=.25)))

  #plot summary
  cat('< NIfTI data >\n')
  cat(paste0('filename  : ',x$getName(),' \n'))
  cat(paste0('dimensions:'),paste0(dim(x$data),collapse = ' x '),'\n')
  cat(paste0('nvoxels   : ',nvox,' (nonzero = ',nvox-length(nozero),')'),'\n')
  cat('\n')
  rownames(dist) <- c('all','non-zero')
  show(dist)
  cat('\n')

}
