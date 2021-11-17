## Nifti R6 Class Functions (USER)
## (mainly wrappers for the R6 functions)

#' read in a NIfTI-1/NIfTI-2 file
#'
#' Reads in a file and returns an object of niftiDataR6 class
#'
#' @param filename name of the file to open (may include path)
#'
#' @return object of class \code{\link{niftiDataR6}}
#'
#' @export
readNifti <- function(filename = NULL, ignoreDims=F) {

  if(is.null(filename)) stop('Please give a proper filename.')

  if(!file.exists(filename)) stop(paste('File',filename,'does not seem to exist.\n'))

  niftiObject <- niftiDataR6$new()
  niftiObject$readData(filename,ignoreDims)

  return(niftiObject)
}

#' write to a NIfTI-1/NIfTI-2 file
#'
#' writes niftiDataR6 object to the file specified in the niftiDataR6object
#' or to a file provided by filename.
#'
#' @param niftiObject object of class \code{\link{niftiDataR6}}
#' @param overwriteWarning warn (with a user option to override) when about to overwrite a file
#'
#' @return object of class \code{\link{niftiDataR6}}
#'
#' @export
writeNifti <- function(niftiObject, overwriteWarning = TRUE) {

  if(any(class(niftiObject)=='R6') & any(class(niftiObject)=='niftiDataR6')) {

    if(overwriteWarning==TRUE & file.exists(niftiObject$getFullPath())) {

      inp <- readline(paste0('This will overwrite file ',niftiObject$getFullPath(),' | overwrite (y/n)? '))
      inp <- tolower(trimws(inp,'both'))

      if(inp=='y' | inp=='yes') {
        niftiObject$writeData()
      } else {
        cat('No file written.\n')
        return(invisible(FALSE))
      }

    } else {
        #file does not exist or overwriteWarning=FALSE
        niftiObject$writeData()
    }

  } else {
    stop('Provide an object of class \'niftiDataR6\'.')
  }

  invisible(TRUE)
}

#' Extract fMRI time-series
#'
#' Extract time-series and compile in a voxel by time matrix
#'
#' @param niftiData object of class\code{\link{niftiDataR6}} containing 4D time-series
#' @param maskData object of class \code{\link{niftiDataR6}} containing 3D mask
#' @param xyzloc matrix with x,y,z locations of to-be-extracted data
#'
#' @return matrix of voxel by time
#'
#' @export
extractTS <- function(niftiObject, maskData = NULL, xyzloc = NULL) {

  #check dimensions of 4D data
  if(!(any(class(niftiObject)=='R6') & any(class(niftiObject)=='niftiDataR6'))) stop('Input must be a niftiDataR6 object')

  voxByTime <- niftiObject$extractTS(maskData = maskData, xyzloc = xyzloc)

  return(voxByTime)

}

#' plot a NIfTI-1/NIfTI-2 object
#'
#' plots a NIfTI-1/NIfTI-2 object
#'
#' @param niftiObject niftiR6 object
#'
#' @export
plotNifti <- function(niftiObject, type = c('sag','axi','cor'), volume = 'avg', color = c('grayscale','red-yellow'), asp = 1) {

  niftiObject$plotNifti(type = type, volume = volume, color = color, asp = asp)

}
