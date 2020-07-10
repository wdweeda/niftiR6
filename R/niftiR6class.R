# Nifti R6 Class Definition

#' Class with file information of NIfTI files
#'
#' @docType class
#' @import R6
#' @keywords data
#' @return Object of \code{\link{R6Class}} with information on a NIfTI file
#' @format \code{\link{R6Class}} object.
#' @examples
#' niftiInfo <- niftiInfoR6$new()
#' @field fullpath Full path of the file
#' @field filename Name of the file (without extension)
#' @field extension Extension of the file
#' @field gzipped Is the file compressed?
#' @field endian Endianness of the file
#' @field data.type Type of data in file
#' @field data.signed Are data.type signed?
#' @field connection Connection to the file
#'
#' @section Methods:
#' \describe{
#'   \item{\code{getFullPath()}}{Returns string of path to the file.}
#'   \item{\code{changeName()}}{Changes the name of the file in the R6 object.}
#'   \item{\code{changePath()}}{Changes the path of the file in the R6 object.}
#'   \item{\code{getName()}}{Returns string of the filename.}
#'   \item{\code{showInfo()}}{Shows the info of the file.}
#'
#' }
#'
#' @export
niftiInfoR6 <- R6::R6Class("niftiInfoR6",

  public = list(

    initialize = function() {
      private$endian <- .Platform$endian
      },

    getFullPath = function() {
        if(private$gzipped==TRUE) {
          out <- paste0(private$fullpath,.Platform$file.sep,private$filename,'.',private$extension,'.gz')
        } else {
          out <- paste0(private$fullpath,.Platform$file.sep,private$filename,'.',private$extension)
        }
        return(out)
      },

    changeName = function(filename) {
        private$filename <- filename
      },

    changePath = function(filepath) {
        private$fullpath <- filepath
      },

    getName = function() {
      private$filename
    },

    showInfo = function() {
      cat('fullpath  :',private$fullpath,'\n')
      cat('filename  :',private$filename,'\n')
      cat('filetype  :',private$filetype,'\n')
      cat('extension :',private$extension,'\n')
      cat('gzipped   :',private$gzipped,'\n')
      cat('endian    :',private$endian,'\n')
      cat('data.type :',private$data.type,'\n')
      cat('signed    :',private$data.signed,'\n')
    }

  ),

  private = list(

    #slots
    fullpath = NULL,
    filename = NULL,
    filetype = NULL,
    extension = NULL,
    gzipped = NULL,
    endian = NULL,
    data.type=NULL,
    data.signed=NULL,
    connection=NULL,

    #functions
    openConnection = function(cmode='rb') {

      if(private$gzipped==TRUE) {
        private$connection <- gzfile(paste0(private$fullpath,.Platform$file.sep,private$filename,'.',private$extension,'.gz'),open=cmode)
      } else {
        private$connection <- file(paste0(private$fullpath,.Platform$file.sep,private$filename,'.',private$extension),open=cmode)
      }

      return(private)
    },

    getFileInfo = function(filename) {

      readin <- function(private) {

        #open connection to file
        private$openConnection()

        #read in size_of_header
        headinfo <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)

        close(private$connection)

        return(headinfo)
      }

      readmagic <- function(private,sizeofheader) {

        #open connection to file
        private$openConnection()

        if(sizeofheader==348) {
          stuffbeforemagic <- readBin(private$connection,raw(),n=344)
          magic <- rawToChar(removeNullString(readBin(private$connection,raw(),n=4)))
        }

        if(sizeofheader==540) {
          stuffbeforemagic <- readBin(private$connection,raw(),n=4)
          magic <- rawToChar(removeNullString(readBin(private$connection,raw(),n=8)))
        }

        close(private$connection)

        return(magic)

      }

      #check if only file else pre-append working directory
      if(length(grep(.Platform$file.sep,filename))==0) filename <- paste0(getwd(),.Platform$file.sep,filename)

      #check if file exists
      if(!file.exists(filename)) stop(paste('File',filename,'does noet exist!'))

      #split filename into path, name and extensions
      fsplit_path <- dirname(filename)
      fsplit_name <- strsplit(basename(filename),'\\.')[[1]]

      #check gzippedness and set extension and filename
      if(fsplit_name[length(fsplit_name)]=='gz') {
        private$gzipped <- TRUE
        private$extension <- fsplit_name[length(fsplit_name)-1]
        private$filename <- paste0(fsplit_name[-c(length(fsplit_name)-1,length(fsplit_name))],sep='.',collapse='')
        private$filename <- substr(private$filename,1,nchar(private$filename)-1)
      } else {
        private$gzipped <- FALSE
        private$extension <- fsplit_name[length(fsplit_name)]
        private$filename <- paste0(fsplit_name[-length(fsplit_name)],sep='.',collapse='')
        private$filename <- substr(private$filename,1,nchar(private$filename)-1)
      }

      #set path
      private$fullpath <- fsplit_path

      #check valid files if img/hdr pair and if IMG try and open header
      if(private$extension=='img') {
        if(private$gzipped==TRUE) {
          if(file.exists(paste0(private$fullpath,.Platform$file.sep,private$filename,'.hdr.gz'))) {
            private$extension <- 'hdr'
          } else {
            stop('No valid img/hdr pair found. HDR does not exist.\n')
          }
        } else {
          if(file.exists(paste0(private$fullpath,.Platform$file.sep,private$filename,'.hdr'))) {
            private$extension <- 'hdr'
          } else {
            stop('No valid img/hdr pair found. HDR does not exist.\n')
          }
        }
      }

      if(private$extension=='hdr') {
        if(private$gzipped==TRUE) {
          if(!file.exists(paste0(private$fullpath,.Platform$file.sep,private$filename,'.img.gz'))) stop('No valid img/hdr pair found. IMG does not exist.\n')
        } else {
          if(!file.exists(paste0(private$fullpath,.Platform$file.sep,private$filename,'.img'))) stop('No valid img/hdr pair found. IMG does noet exist.\n')
        }
      }


      #check nifti header. Returns fileinf of class nifti.header
      sizeof_hdr <- readin(private)

      #check if header is NULL
      if(length(sizeof_hdr)==0) stop('Nifti file is empty.')

      #check if header size is 348 (if not try other endian)
      if(!(sizeof_hdr==348 | sizeof_hdr==540)) {

        #change endian
        if(private$endian=='big') private$endian <- 'little' else private$endian <- 'big'

        #check header again
        sizeof_hdr <- readin(private)

        if(!(sizeof_hdr==348 | sizeof_hdr==540)) {
          #then I don't know
          stop('Header is not of size 348 or 540 (even after swapping endian). File might not be of the correct format (or corrupted).\n')
        }
      }

      #read in magic string
      magic <- readmagic(private,sizeof_hdr)

      #set filetype according to header info (not based on extension)
      if(magic=='n+1') {
        private$filetype <- 'nifti+1'

      } else	if(magic=='ni1') {
        private$filetype <- 'nifti1'

      } else if(magic=='') {
        private$filetype <- 'analyze'

      } else if(magic=='n+2\r\n\032\n') {
        private$filetype <- 'nifti+2'

      } else if(magic=='ni2\r\n\032\n') {
        private$filetype <- 'nifti2'

      } else {
        stop('Magicstring contains unknown characters (',magic,').File might not be the correct format.\n')
      }

      #Returns object of class nifti.header
      invisible(private)

    }

  )
)

#' Class with header information from a NIfTI file
#'
#' @docType class
#' @import R6
#' @keywords data
#' @return Object of \code{\link{R6Class}} with information on a NIfTI file
#' @format \code{\link{R6Class}} object.
#' @examples
#' niftiHeader <- niftiHeaderR6$new()
#'
#' @field sizeof_hdr nifti header slot
#' @field data_type nifti header slot
#' @field db_name nifti header slot
#' @field extents nifti header slot
#' @field session_error nifti header slot
#' @field regular nifti header slot
#' @field dim_info nifti header slot
#' @field dims nifti header slot
#' @field intent_p1  nifti header slot
#' @field intent_p2 nifti header slot
#' @field intent_p3 nifti header slot
#' @field intent_code nifti header slot
#' @field datatype nifti header slot
#' @field bitpix nifti header slot
#' @field slice_start nifti header slot
#' @field pixdim nifti header slot
#' @field vox_offset nifti header slot
#' @field scl_slope nifti header slot
#' @field scl_inter nifti header slot
#' @field slice_end nifti header slot
#' @field slice_code nifti header slot
#' @field xyzt_units nifti header slot
#' @field cal_max nifti header slot
#' @field cal_min nifti header slot
#' @field slice_duration nifti header slot
#' @field toffset nifti header slot
#' @field glmax nifti header slot
#' @field glmin nifti header slot
#' @field descrip nifti header slot
#' @field aux_file nifti header slot
#' @field qform_code nifti header slot
#' @field sform_code nifti header slot
#' @field quatern_b nifti header slot
#' @field quatern_c nifti header slot
#' @field quatern_d nifti header slot
#' @field qoffset_x nifti header slot
#' @field qoffset_y nifti header slot
#' @field qoffset_z nifti header slot
#' @field srow_x nifti header slot
#' @field srow_y nifti header slot
#' @field srow_z nifti header slot
#' @field intent_name nifti header slot
#' @field magic nifti header slot
#' @field extension_char nifti header slot
#' @field extension_data nifti header slot
#'
#' @section Methods:
#' \describe{
#'   \item{\code{show()}}{Shows header info}
#'   \item{\code{readHeader(filename)}}{Reads in header info}
#'   \item{\code{writeHeader()}}{Writes header info to file}
#'   \item{\code{changeDatatype(datatype)}}{sets the data type + bitpix according a nifti-code (numeric or string)}
#'
#'
#'}
#'
#' @export
niftiHeaderR6 <- R6::R6Class("niftiHeaderR6",

  inherit = niftiInfoR6,

  public = list(

    #slots
    sizeof_hdr = 348, #standard 348
    data_type = NULL,
    db_name = NULL,
    extents = NULL,
    session_error = NULL,
    regular = NULL,
    dim_info = NULL,
    dims = NULL,
    intent_p1 = NULL,
    intent_p2 = NULL,
    intent_p3 = NULL,
    intent_code = NULL,
    datatype = 16,
    bitpix = 32,
    slice_start = NULL,
    pixdim = NULL,
    vox_offset = 352,
    scl_slope = NULL,
    scl_inter = NULL,
    slice_end = NULL,
    slice_code = NULL,
    xyzt_units = '\n',
    cal_max = NULL,
    cal_min = NULL,
    slice_duration = NULL,
    toffset = NULL,
    glmax = NULL,
    glmin = NULL,
    descrip = NULL,
    aux_file = NULL,
    qform_code = NULL,
    sform_code = NULL,
    quatern_b = NULL,
    quatern_c = NULL,
    quatern_d = NULL,
    qoffset_x = NULL,
    qoffset_y = NULL,
    qoffset_z = NULL,
    srow_x = NULL,
    srow_y = NULL,
    srow_z = NULL,
    intent_name = NULL,
    magic = 'n+1',
    extension_char = c(0,0,0,0),
    extension_data = NULL,

    #functions
    show = function() {
      cat('< NIfTI File >\n')
      super$showInfo()
      cat('\n')
      cat('< header >\n')
      cat('sizeof_hdr',self$sizeof_hdr,'\n')
      cat('data_type',self$data_type,'\n')
      cat('db_name',self$db_name,'\n')
      cat('extents',self$extents,'\n')
      cat('session_error',self$session_error,'\n')
      cat('regular',self$regular,'\n')
      cat('dim_info',self$dim_info,'\n')
      cat('dims',self$dims,'\n')
      cat('intent_p1',self$intent_p1,'\n')
      cat('intent_p2',self$intent_p2,'\n')
      cat('intent_p3',self$intent_p3,'\n')
      cat('intent_code',self$intent_code,'\n')
      cat('datatype',self$datatype,'\n')
      cat('bitpix',self$bitpix,'\n')
      cat('slice_start',self$slice_start,'\n')
      cat('pixdim',self$pixdim,'\n')
      cat('vox_offset',self$vox_offset,'\n')
      cat('scl_slope',self$scl_slope,'\n')
      cat('scl_inter',self$scl_inter,'\n')
      cat('slice_end',self$slice_end,'\n')
      cat('slice_code',self$slice_code,'\n')
      #cat('xyzt_units',self$xyzt_units,'\n')
      xyzt <- private$xyztTounits()
      cat('xyzt_units, space in',xyzt$space,'| time in',xyzt$time,'\n')
      cat('cal_max',self$cal_max,'\n')
      cat('cal_min',self$cal_min,'\n')
      cat('slice_duration',self$slice_duration,'\n')
      cat('toffset',self$toffset,'\n')
      cat('glmax',self$glmax,'\n')
      cat('glmin',self$glmin,'\n')
      cat('descrip',self$descrip,'\n')
      cat('aux_file',self$aux_file,'\n')
      cat('qform_code',self$qform_code,'\n')
      cat('sform_code',self$sform_code,'\n')
      cat('quatern_b',self$quatern_b,'\n')
      cat('quatern_c',self$quatern_c,'\n')
      cat('quatern_d',self$quatern_d,'\n')
      cat('qoffset_x',self$qoffset_x,'\n')
      cat('qoffset_y',self$qoffset_y,'\n')
      cat('qoffset_z',self$qoffset_z,'\n')
      cat('srow_x',self$srow_x,'\n')
      cat('srow_y',self$srow_y,'\n')
      cat('srow_z',self$srow_z,'\n')
      cat('intent_name',self$intent_name,'\n')
      cat('magic',self$magic,'\n')
      cat('extension',self$extension_char,'\n')
    },

    readHeader = function(filename) {

      #check whether a filename is given
      if(is.null(filename)) stop('Please provide a valid filename.')

      private$getFileInfo(filename)

      private$openConnection()

      if(private$filetype=='nifti1' | private$filetype=='nifti+1' | private$filetype=='analyze') private$readHeaderNifti1()
      if(private$filetype=='nifti2' | private$filetype=='nifti+2') private$readHeaderNifti2()

      close(private$connection)

      invisible(self)
    },

    writeHeader = function() {

      #separate files nifti-1
      if((private$filetype=='nifti1' | private$filetype=='analyze')) {

        #write header
        private$extension <- 'hdr'
        private$openConnection('wb')
        private$writeHeaderNifti1()
        close(private$connection)

      }

      #nii nifti-1 file
      if(private$filetype=='nifti+1') {

        private$openConnection('wb')
        private$writeHeaderNifti1()
        close(private$connection)

      }

      #separate files nifti-2
      if(private$filetype=='nifti2') {

        #write header
        private$extension <- 'hdr'
        private$openConnection('wb')
        private$writeHeaderNifti2()
        close(private$connection)

      }

      #nii nifti-2 file
      if(private$filetype=='nifti+2') {

        private$openConnection('wb')
        private$writeHeaderNifti2()
        close(private$connection)

      }

      invisible(self)
    },

    changeDatatype = function(datatype) {

      #change datatype to string based on datatype number
      if(is.numeric(datatype)) {
        if(as.integer(datatype) == 0) {datatype <- 'NONE'} else
          if(as.integer(datatype) == 1) {datatype <- 'BINARY'} else
            if(as.integer(datatype) == 2) {datatype <- 'UINT8'} else
              if(as.integer(datatype) == 4) {datatype <- 'INT16'} else
                if(as.integer(datatype) == 8) {datatype <- 'INT32'} else
                  if(as.integer(datatype) == 16) {datatype <- 'FLOAT32'} else
                    if(as.integer(datatype) == 32) {datatype <- 'COMPLEX64'} else
                      if(as.integer(datatype) == 64) {datatype <- 'FLOAT64'} else
                        if(as.integer(datatype) == 128) {datatype <- 'RGB24'} else
                          if(as.integer(datatype) == 256) {datatype <- 'INT8'} else
                            if(as.integer(datatype) == 512) {datatype <- 'UINT16'} else
                              if(as.integer(datatype) == 768) {datatype <- 'UINT32'} else
                                if(as.integer(datatype) == 1024) {datatype <- 'INT64'} else
                                  if(as.integer(datatype) == 1280) {datatype <- 'UINT64'} else
                                    if(as.integer(datatype) == 1536) {datatype <- 'FLOAT128'} else
                                      if(as.integer(datatype) == 1792) {datatype <- 'COMPLEX128'} else
                                        if(as.integer(datatype) == 2048) {datatype <- 'COMPLEX256'} else {stop('Unknown datatype input. Not changing')}
      }

      #make datatype and bitpix change
      if(is.character(datatype)) {
        if(toupper(datatype) == 'NONE' | toupper(datatype) == 'UNKNOWN') {self$datatype <- 0; self$bitpix <- 0} else
          if(toupper(datatype) == 'BINARY') {self$datatype <- 1; self$bitpix <- 1} else
            if(toupper(datatype) == 'UINT8')  {self$datatype <- 2; self$bitpix <- 8} else
              if(toupper(datatype) == 'INT16')  {self$datatype <- 4; self$bitpix <- 16} else
                if(toupper(datatype) == 'INT32')  {self$datatype <- 8; self$bitpix <- 32} else
                  if(toupper(datatype) == 'FLOAT32')  {self$datatype <- 16; self$bitpix <- 32} else
                    if(toupper(datatype) == 'COMPLEX64')  {self$datatype <- 32; self$bitpix <- 64} else
                      if(toupper(datatype) == 'FLOAT64')  {self$datatype <- 64; self$bitpix <- 64} else
                        if(toupper(datatype) == 'RGB24')  {self$datatype <- 128; self$bitpix <- 24} else
                          if(toupper(datatype) == 'INT8')  {self$datatype <- 256; self$bitpix <- 8} else
                            if(toupper(datatype) == 'UINT16')  {self$datatype <- 512; self$bitpix <- 16} else
                              if(toupper(datatype) == 'UINT32')  {self$datatype <- 768; self$bitpix <- 32} else
                                if(toupper(datatype) == 'INT64')  {self$datatype <- 1024; self$bitpix <- 64} else
                                  if(toupper(datatype) == 'UINT64')  {self$datatype <- 1280; self$bitpix <- 64} else
                                    if(toupper(datatype) == 'FLOAT128')  {self$datatype <- 1536; self$bitpix <- 128} else
                                      if(toupper(datatype) == 'COMPLEX128')  {self$datatype <- 1792; self$bitpix <- 128} else
                                        if(toupper(datatype) == 'COMPLEX256')  {self$datatype <- 2048; self$bitpix <- 256} else {stop('Unknown datatype string. Not changing datatype.') }
      }

      #make datatype change in fileinfo
      private$setdatatype()
    }

  ),

  private = list(

    #functions
    setdatatype = function() {

      #set the data.type and data.signed slots of fileInfo based on nifti datatype slot
      if(self$datatype==0) { private$data.type<-'none';private$data.signed<-TRUE } else
        if(self$datatype==1) { private$data.type<-'raw';private$data.signed<-TRUE } else
          if(self$datatype==2) { private$data.type<-'integer';private$data.signed<-FALSE } else
            if(self$datatype==4) { private$data.type<-'integer';private$data.signed<-TRUE } else
              if(self$datatype==8) { private$data.type<-'integer';private$data.signed<-TRUE } else
                if(self$datatype==16) { private$data.type<-'double';private$data.signed<-TRUE } else
                  if(self$datatype==32) { private$data.type<-'complex';private$data.signed<-TRUE } else
                    if(self$datatype==64) { private$data.type<-'double';private$data.signed<-TRUE } else
                      if(self$datatype==256) { private$data.type<-'integer';private$data.signed<-TRUE } else
                        if(self$datatype==512) { private$data.type<-'integer';private$data.signed<-FALSE } else
                          if(self$datatype==768) { private$data.type<-'integer';private$data.signed<-FALSE } else
                            if(self$datatype==1024) { private$data.type<-'integer';private$data.signed<-TRUE } else
                              if(self$datatype==1280) { private$data.type<-'integer';private$data.signed<-FALSE } else
                                if(self$datatype==1536) { private$data.type<-'double';private$data.signed<-TRUE } else
                                  if(self$datatype==1792) { private$data.type<-'complex';private$data.signed<-TRUE } else
                                    if(self$datatype==2048) { private$data.type<-'complex';private$data.signed<-TRUE } else { stop('Datatype is unknown!') }

      #check for (as yet) unsupported datatypes
      if(any(self$dataype == 32 | self$dataype == 128 | self$dataype == 1792 | self$dataype == 2048)) {
        warning(paste0('Datatype (RGB,COMPLEX) not yet supported. Performance unknown.'))
      }

      invisible(self)
    },

    readHeaderNifti1 = function() {

      #read in all elements of Nifti-1 file
      self$sizeof_hdr  <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)         #  0, int32,  MUST BE 348
      self$data_type  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=10,endian=private$endian)))#  4, char[10]
      self$db_name  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=18,endian=private$endian)))  # 14, char[18]
      self$extents  <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)            # 32, int32
      self$session_error  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian =private$endian)     # 36, short
      self$regular  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=1,endian=private$endian)))   # 38, char[1]
      self$dim_info  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=1,endian=private$endian)))  # 39, char[1], MRI SLICE ORDERING
      self$dims  <- readBin(private$connection,integer(),n=8,size=2,signed=T,endian=private$endian)               # 40, short[8], DATA ARRAY DIMENSIONS
      self$intent_p1  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    # 56, float, 1ST INTENT PARAMETER
      self$intent_p2  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    # 60, float, 2ND INTENT PARAMETER
      self$intent_p3  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    # 64, float, 3RD INTENT PARAMETER
      self$intent_code  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)        # 68, short, NIFITINTENT CODE
      self$datatype  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)           # 70, short, DEFINES DATA TYPE
      self$bitpix  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)             # 72, short, NUMBER BITS PER VOXEL
      self$slice_start  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)        # 74, short, FIRST SLICE INDEX
      self$pixdim  <- readBin(private$connection,double(),n=8,size=4,endian=private$endian)                       # 76, float[8], GRID SPACINGS
      self$vox_offset  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                   #108, float, OFFSET INTO .NII FILE
      self$scl_slope  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #112, float, DATA SCALING: SLOPE
      self$scl_inter  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #116, float, DATA SCALING: INTERCEPT
      self$slice_end  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)          #120, short, LAST SLICE INDEX
      self$slice_code  <- readBin(private$connection,integer(),size=1,n=1,signed=T,endian=private$endian)         #122, integer[1], SLICE TIMING ORDER
      self$xyzt_units  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=1,endian=private$endian)))#123, raw[1], UNITS OF PIXDIM
      self$cal_max  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                      #124, float, MAX DISPLAY INTENSITY
      self$cal_min  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                      #128, float, MIN DISPLAY INTENSITY
      self$slice_duration  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)               #132, float, TIME FOR 1 SLICE
      self$toffset  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                      #136, float, TIME AXIS SHIFT
      self$glmax  <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)              #140, int32
      self$glmin  <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)              #144, int32
      self$descrip  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=80,endian=private$endian))) 	#148, char[80], TEXT
      self$aux_file  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=24,endian=private$endian)))	#228, char[24], AUXILIARY FILENAME
      self$qform_code  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)         #252, short, NIFITXFORM CODE
      self$sform_code  <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)         #254, short, NIFITXFORM CODE
      self$quatern_b  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #256, float, QUATERNION B PARAM
      self$quatern_c  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #260, float, QUATERNION C PARAM
      self$quatern_d  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #264, float, QUATERNION D PARAM
      self$qoffset_x  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #268, float, QUATERNION X SHIFT
      self$qoffset_y  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #272, float, QUATERNION Y SHIFT
      self$qoffset_z  <- readBin(private$connection,double(),n=1,size=4,endian=private$endian)                    #276, float, QUATERNION Z SHIFT
      self$srow_x  <- readBin(private$connection,double(),n=4,size=4,endian=private$endian)                       #280, float[4], 1ST ROW AFFINE TRANSFORM
      self$srow_y  <- readBin(private$connection,double(),n=4,size=4,endian=private$endian)                       #296, float[4], 2ND ROW AFFINE TRANSFORM
      self$srow_z  <- readBin(private$connection,double(),n=4,size=4,endian=private$endian)                       #312, float[4], 3RD ROW AFFINE TRANSFORM
      self$intent_name  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=16,endian=private$endian)))#328, char[16], NAME OR MEANING OF DATA
      self$magic  <- rawToChar(removeNullString(readBin(private$connection,raw(),n=4,endian=private$endian)))     #344, char[4], MAGICSTRING!

      #read in extension bits check if extension bits are non-zero
      self$extension_char <- readBin(private$connection,integer(),size=1,n=4,endian=private$endian)
      if(length(self$extension_char) != 0) {
        if(self$extension_char[1] != 0) {
          #read in extension data
          private$readExtension()
        }
      }

      #set correct datatype
      private$setdatatype()

      invisible(self)
    },

    readHeaderNifti2 = function() {

      #read in all elements of Nifti-2 file
      self$sizeof_hdr <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)          #  0, int32[1],  MUST BE 540
      self$magic <- rawToChar(removeNullString(readBin(private$connection,raw(),n=8,endian=private$endian)))    	#  4, char[8], MAGICSTRING!
      self$datatype <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)            # 12, int16_t[1], DEFINES DATA TYPE
      self$bitpix <- readBin(private$connection,integer(),n=1,size=2,signed=T,endian=private$endian)              # 14, int16_t[1], NUMBER BITS PER VOXEL
      self$dims <- readBin(private$connection,integer(),n=8,size=8,signed=T,endian=private$endian)                # 16, int64_t[8], DATA ARRAY DIMENSIONS
      self$intent_p1 <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     # 80, double[1], 1ST INTENT PARAMETER
      self$intent_p2 <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     # 88, double[1], 2ND INTENT PARAMETER
      self$intent_p3 <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     # 96, double[1], 3RD INTENT PARAMETER
      self$pixdim <- readBin(private$connection,double(),n=8,size=8,endian=private$endian)                        #104, double[8], GRID SPACINGS
      self$vox_offset <- readBin(private$connection,integer(),n=1,size=8,endian=private$endian)                   #168, int64_t[1], OFFSET INTO .NII FILE
      self$scl_slope <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #176, double[1], DATA SCALING: SLOPE
      self$scl_inter <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #184, double[1], DATA SCALING: INTERCEPT
      self$cal_max <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                       #192, double[1], MAX DISPLAY INTENSITY
      self$cal_min <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                       #200, double[1], MIN DISPLAY INTENSITY
      self$slice_duration <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                #208, double[1], TIME FOR 1 SLICE
      self$toffset <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                       #216, double[1], TIME AXIS SHIFT
      self$slice_start <- readBin(private$connection,integer(),n=1,size=8,signed=T,endian=private$endian)         #224, int64_t[1], FIRST SLICE INDEX
      self$slice_end  <- readBin(private$connection,integer(),n=1,size=8,signed=T,endian=private$endian)          #232, int64_t[1], LAST SLICE INDEX
      self$descrip <- rawToChar(removeNullString(readBin(private$connection,raw(),n=80,endian=private$endian))) 	#240, char[80], TEXT
      self$aux_file <- rawToChar(removeNullString(readBin(private$connection,raw(),n=24,endian=private$endian)))	#320, char[24], AUXILIARY FILENAME
      self$qform_code <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)          #344, int32[1], NIFITXFORM CODE
      self$sform_code <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)          #348, int32[1], NIFITXFORM CODE
      self$quatern_b <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #352, double[1], QUATERNION B PARAM
      self$quatern_c <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #360, double[1], QUATERNION C PARAM
      self$quatern_d <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #368, double[1], QUATERNION D PARAM
      self$qoffset_x <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #376, double[1], QUATERNION X SHIFT
      self$qoffset_y <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #384, double[1], QUATERNION Y SHIFT
      self$qoffset_z <- readBin(private$connection,double(),n=1,size=8,endian=private$endian)                     #392, double[1], QUATERNION Z SHIFT
      self$srow_x <- readBin(private$connection,double(),n=4,size=8,endian=private$endian)                        #400, double[4], 1ST ROW AFFINE TRANSFORM
      self$srow_y <- readBin(private$connection,double(),n=4,size=8,endian=private$endian)                        #432, double[4], 2ND ROW AFFINE TRANSFORM
      self$srow_z <- readBin(private$connection,double(),n=4,size=8,endian=private$endian)                        #464, double[4], 3RD ROW AFFINE TRANSFORM
      self$slice_code <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)          #496, int32[1], SLICE TIMING ORDER
      self$xyzt_units <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)          #500, int32[1], UNITS OF PIXDIM
      self$intent_code <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)         #504, int32[1], NIFITINTENT CODE
      self$intent_name <- rawToChar(removeNullString(readBin(private$connection,raw(),n=16,endian=private$endian))) #508, char[16], NAME OR MEANING OF DATA
      self$dim_info <- rawToChar(removeNullString(readBin(private$connection,raw(),n=1,endian=private$endian)))   #524, char[1], MRI SLICE ORDERING
      unused_str <- rawToChar(removeNullString(readBin(private$connection,raw(),n=15,endian=private$endian)))     #525, cahr[15], UNUSED STRING (not stored fill with \0)

      #deprecated ANALYZE headers
      #DEPR. ANALYZE#
      self$data_type <- 'depr'      # 12, char[10]
      self$db_name <- 'depr'        # 14, char[18]
      self$extents <- 0             # 32, int32
      self$session_error <- 0       # 36, short
      self$regular <- 'd'           # 38, char[1]
      self$glmax <- 0               #140, int32
      self$glmin <- 0               #144, int32

      #check if extension bits are non-zero
      self$extension_char <- readBin(private$connection,integer(),size=1,n=4,endian=private$endian)
      if(length(self$extension_char) != 0) {
        if(self$extension_char[1] != 0) {
          #read in extension data
          private$readExtension()
        }
      }

      #set correct datatype
      private$setdatatype()

      invisible(self)
    },

    writeHeaderNifti1 = function() {

      #write binary nifti-1 header
      writeBin(as.integer(self$sizeof_hdr),private$connection,size=4,endian=private$endian)
      writeBin(charToRaw(self$data_type),private$connection,endian=private$endian)
      writeBin(raw(10-nchar(self$data_type)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$db_name),private$connection,endian=private$endian)
      writeBin(raw(18-nchar(self$db_name)),private$connection,endian=private$endian)
      writeBin(as.integer(self$extents),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$session_error),private$connection,size=2,endian=private$endian)
      writeBin(charToRaw(self$regular),private$connection,endian=private$endian)
      writeBin(raw(1-nchar(self$regular)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$dim_info),private$connection,endian=private$endian)
      writeBin(raw(1-nchar(self$dim_info)),private$connection,endian=private$endian)
      writeBin(as.integer(self$dims),private$connection,size=2,endian=private$endian)
      writeBin(as.double(self$intent_p1),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$intent_p2),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$intent_p3),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$intent_code),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$datatype),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$bitpix),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$slice_start),private$connection,size=2,endian=private$endian)
      writeBin(as.double(self$pixdim),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$vox_offset),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$scl_slope),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$scl_inter),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$slice_end),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$slice_code),private$connection,size=1,endian=private$endian)
      writeBin(charToRaw(self$xyzt_units),private$connection,endian=private$endian)
      writeBin(raw(1-nchar(self$xyzt_units)),private$connection,endian=private$endian)
      writeBin(as.double(self$cal_max),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$cal_min),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$slice_duration),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$toffset),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$glmax),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$glmin),private$connection,size=4,endian=private$endian)
      writeBin(charToRaw(self$descrip),private$connection,endian=private$endian)
      writeBin(raw(80-nchar(self$descrip)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$aux_file),private$connection,endian=private$endian)
      writeBin(raw(24-nchar(self$aux_file)),private$connection,endian=private$endian)
      writeBin(as.integer(self$qform_code),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$sform_code),private$connection,size=2,endian=private$endian)
      writeBin(as.double(self$quatern_b),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$quatern_c),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$quatern_d),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$qoffset_x),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$qoffset_y),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$qoffset_z),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$srow_x),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$srow_y),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$srow_z),private$connection,size=4,endian=private$endian)
      writeBin(charToRaw(self$intent_name),private$connection,endian=private$endian)
      writeBin(raw(16-nchar(self$intent_name)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$magic),private$connection,endian=private$endian)
      writeBin(raw(4-nchar(self$magic)),private$connection,endian=private$endian)

      #Check for extension data
      if(length(self$extension_char) != 0) {

        #write extension block
        writeBin(as.integer(self$extension_char),private$connection,size=1,endian=private$endian)

        #Write extension blocks
        if(self$extension_char[1] != 0) {
          private$writeExtension()
        }
      }

      invisible(self)

    },

    writeHeaderNifti2 = function() {

      writeBin(as.integer(self$sizeof_hdr),private$connection,size=4,endian=private$endian)
      writeBin(charToRaw(self$magic),private$connection,endian=private$endian)
      writeBin(raw(8-nchar(self$magic)),private$connection,endian=private$endian)
      writeBin(as.integer(self$datatype),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$bitpix),private$connection,size=2,endian=private$endian)
      writeBin(as.integer(self$dims),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$intent_p1),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$intent_p2),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$intent_p3),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$pixdim),private$connection,size=8,endian=private$endian)
      writeBin(as.integer(self$vox_offset),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$scl_slope),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$scl_inter),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$cal_max),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$cal_min),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$slice_duration),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$toffset),private$connection,size=8,endian=private$endian)
      writeBin(as.integer(self$slice_start),private$connection,size=8,endian=private$endian)
      writeBin(as.integer(self$slice_end ),private$connection,size=8,endian=private$endian)
      writeBin(charToRaw(self$descrip),private$connection,endian=private$endian)
      writeBin(raw(80-nchar(self$descrip)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$aux_file),private$connection,endian=private$endian)
      writeBin(raw(24-nchar(self$aux_file)),private$connection,endian=private$endian)
      writeBin(as.integer(self$qform_code),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$sform_code),private$connection,size=4,endian=private$endian)
      writeBin(as.double(self$quatern_b),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$quatern_c),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$quatern_d),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$qoffset_x),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$qoffset_y),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$qoffset_z),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$srow_x),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$srow_y),private$connection,size=8,endian=private$endian)
      writeBin(as.double(self$srow_z),private$connection,size=8,endian=private$endian)
      writeBin(as.integer(self$slice_code),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$xyzt_units),private$connection,size=4,endian=private$endian)
      writeBin(as.integer(self$intent_code),private$connection,size=4,endian=private$endian)
      writeBin(charToRaw(self$intent_name),private$connection,endian=private$endian)
      writeBin(raw(16-nchar(self$intent_name)),private$connection,endian=private$endian)
      writeBin(charToRaw(self$dim_info),private$connection,endian=private$endian)
      writeBin(raw(1-nchar(self$dim_info)),private$connection,endian=private$endian)
      writeBin(raw(15),private$connection,endian=private$endian)

      #Check for extension data
      if(length(self$extension_char) != 0) {

        #write extension block
        writeBin(as.integer(self$extension_char),private$connection,size=1,endian=private$endian)

        if(self$extension_char[1] != 0) {
          private$writeExtension()
        }
      }

      invisible(self)
    },

    #read in extension data if availble
    readExtension = function() {

      #loop over extension sections (if available)
      extensionData <- list()
      reachedEnd <- FALSE
      extnum <- 1

      while(!reachedEnd) {
        esize <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)
        ecode <- readBin(private$connection,integer(),n=1,size=4,signed=T,endian=private$endian)

        if(esize != 0) {
          #read in everything up until esize (raw at the moment, adapt based on ecode)
          extensionList <- list(esize = esize, ecode = ecode, rawData = readBin(private$connection,raw(),n=(esize - 8),endian=private$endian))
          extensionData[[extnum]] <- extensionList
        } else {
          reachedEnd <- TRUE
        }

        extnum <- extnum + 1
      }

      self$extension_data <- extensionData

      invisible(self)
    },

    #writeExtension data (loop over list of extension blocks (list(esize,ecode,rawData)) and write each block)
    writeExtension = function() {

      #loop over extension blocks and write
      for(extnum in 1:length(self$extension_data)) {

        writeBin(as.integer(self$extension_data[[extnum]]$esize),private$connection,endian=private$endian)
        writeBin(as.integer(self$extension_data[[extnum]]$ecode),private$connection,endian=private$endian)
        writeBin(self$extension_data[[extnum]]$rawData,private$connection,endian=private$endian)

      invisible(self)
    }

    },

    xyztTounits = function() {

      xyzt_bits <- rawToBits(charToRaw(self$xyzt_units))
      if(length(xyzt_bits)>=6) {
        if(private$endian == 'little') {
          space_int <- bin2dec(rev(xyzt_bits[1:3]))
          time_int <- bin2dec(rev(xyzt_bits[4:6]))*8
        } else {
          space_int <- bin2dec(xyzt_bits[1:3])
          time_int <- bin2dec(xyzt_bits[4:6])*8
        }
      } else {
        space_int <- 0
        time_int <- 0
      }

      space_char = 'unknown'
      if(space_int==1) space_char = 'meters'
      if(space_int==2) space_char = 'millimeters'
      if(space_int==3) space_char = 'micrometers'

      time_char = 'unknown'
      if(time_int==8)  time_char = 'seconds'
      if(time_int==16) time_char = 'milliseconds'
      if(time_int==24) time_char = 'microseconds'
      if(time_int==32) time_char = 'Hertz'
      if(time_int==40) time_char = 'ppm'
      if(time_int==48) time_char = 'rad/sec'

      return(list(space = space_char, time = time_char))

    }

  )
)

#' Class with file information of NIfTI files
#'
#' @docType class
#' @import R6
#' @keywords data
#' @return Object of \code{\link{R6Class}} with info, header and data of a NIfTI file
#' @format \code{\link{R6Class}} object.
#' @examples
#' niftiData <- niftiDataR6$new()
#'
#' @field data array of data
#'
#' @section Methods:
#' \describe{
#'   \item{\code{show()}}{Shows the niftiData object.}
#'   \item{\code{readData()}}{Reads in the data (+header)from a NIfTI file.}
#'   \item{\code{writeData()}}{Writes data (+header) to a NIfTI file.}
#'   \item{\code{setData()}}{Adds an entire array/matrix/vector to the data slot with checks.}
#'}
#'
#' @export
niftiDataR6 <- R6::R6Class("niftiDataR6",

  inherit = niftiHeaderR6,

  public = list(

    #slots
    data = NULL,

    #functions
    readData = function(filename) {

      #read in Header
      self$readHeader(filename)

      #open connection
      private$openConnection()

      #calculate vector size
      n <- (self$dims[2]*self$dims[3]*self$dims[4]*self$dims[5]*(self$bitpix/8))

      #read (and discard) everthing before vox_offset (is already read in by readHeader)
      readBin(private$connection,raw(),self$vox_offset)

      #read in data
      if(private$data.type == 'complex' | private$data.type == 'raw') {
        datavec <- readBin(private$connection, what=private$data.type, n=n, signed=private$data.signed, endian=private$endian)
      } else {
        datavec <- readBin(private$connection, what=private$data.type, n=n, size=(self$bitpix/8), signed=private$data.signed, endian=private$endian)

        #check for scaling and scale values
        if(self$scl_slope!=0) {
          datavec <- (self$scl_slope*datavec) + self$scl_inter
        }
      }

      #make daya into a vector
      private$vec2array(datavec)

      #close connection
      close(private$connection)

      return(self)
    },

    writeData = function() {

      #separate files nifti-1
      if((private$filetype=='nifti1' | private$filetype=='analyze')) {

        #write image
        private$extension <- 'img'
        private$openConnection('wb')
        private$writeDataNifti()
        close(private$connection)

        #write header
        private$extension <- 'hdr'
        private$openConnection('wb')
        private$writeHeaderNifti1()
        close(private$connection)

      }

      #nii nifti-1 file
      if(private$filetype=='nifti+1') {

        #write header + extension
        private$openConnection('wb')
        private$writeHeaderNifti1()
        private$writeDataNifti()
        close(private$connection)

      }

      #separate files nifti-2
      if(private$filetype=='nifti2') {

        #write image
        private$extension <- 'img'
        private$openConnection('wb')
        private$writeDataNifti()
        close(private$connection)

        #write header
        private$extension <- 'hdr'
        private$openConnection('wb')
        private$writeHeaderNifti2()
        close(private$connection)

      }

      #nii nifti-2 file
      if(private$filetype=='nifti+2') {

        #write header + extension
        private$openConnection('wb')
        private$writeHeaderNifti2()
        private$writeDataNifti()
        close(private$connection)

      }

      invisible(self)
    },

    setData = function(data) {

      #check whether data is numeric or complex
      if(!is.numeric(data) & !is.complex(data)) stop('Provided data must be numeric (and a vector/matrix/array).')

      #check if data is array. If so, also set dimensions.
      if(is.array(data) | is.matrix(data)) {

        dims <- dim(data)
        ndim <- length(dims)

        if(ndim > 0 & ndim < 8) {

          #set dimensions
          self$dims[1] <- ndim
          self$dims[2:(ndim+1)] <- dims
          if(ndim < 7) self$dims[(ndim+2):8] <- 1

          #set data
          self$data <- data

          #return self
          return(invisible(self))

        } else {
          stop('Dimensions of provided array/matrix exceed 7. Unable to set.')
        }

      }

      #check if data is vector. If so, check dimension and set array
      if(is.vector(data)) {

        #set data
        private$vec2array(data)

        #return self
        return(invisible(self))
      }

      #if we are here there is an unknown (not covered) type of data provided.
      stop('Unkown numeric-like data found (data must be a vector/matrix/array).')
    },

    show = function() {
      super$show()
    },

    plotNifti = function(type = c('sag','axi','cor'), volume = 'avg', color = c('grayscale','red-yellow'), asp = 1) {

      if(self$sform_code > 0 & self$srow_x[1]>0) flipx = TRUE else flipx = FALSE

      plotdata <- private$matrixfy(type = type, volume = volume, flipx = flipx)

      plotdata[plotdata==0] = NA

      par(las = 1, mar = c(2,2,2,2) + 0.1)
      image(x = 1:nrow(plotdata), y = 1:ncol(plotdata), z = matrix(0,nrow=nrow(plotdata),ncol=ncol(plotdata)),
            col = rgb(0,0,0), axes = F, asp = asp , xlab = '', ylab = '')
      image(x = 1:nrow(plotdata), y = 1:ncol(plotdata), z = plotdata, col = gray.colors(128), axes = F, asp = dim(plotdata)[2]/dim(plotdata)[1],add = T)

    },

    extractTS = function(maskData = NULL, xyzloc = NULL) {

      #check dimensions of 4D data
      if(self$dims[1] != 4) stop('Can only extract time-series for 4D data')

      #check for mask or location matrix
      if(is.null(maskData) & is.null(xyzloc)) stop('Specify mask (nifti) or location matrix.')

      if(!is.null(maskData) & !is.null(xyzloc)) {
        warning('Both mask and location matrix are specified, using mask.')
        xyzloc <- NULL
      }

      #extract mask locations if mask is given
      if(!is.null(maskData)) {

        #check dimensions of both mask and input
        if(!(any(class(maskData)=='R6') & any(class(maskData)=='niftiDataR6'))) stop('Mask must be a niftiDataR6 object')
        if(maskData$dims[1] != 3) stop('Can only use a 3D mask')
        if(any(self$dims[2:4] != maskData$dims[2:4])) stop(paste0('Dimensions of input (',paste0(self$dims[2:4],collapse=','),') and mask (',paste0(maskData$dims[2:4],collapse=','),') do not match'))

        #extract time-series for non-zero mask objects
        xyzloc <- which(maskData$data != 0, arr.ind = TRUE)

        #check integrity of xyzloc
        if(sum(xyzloc) == 0) stop('no non-zero values in mask')

      }

      #check location matrix integrity
      if(is.vector(xyzloc)) xyzloc <- matrix(xyzloc,nrow = 1)
      if(ncol(xyzloc) != 3) stop('Location matrix doesn\'t have 3 columns')

      #get dimensions from 4D Data
      dx <- self$dims[2]
      dy <- self$dims[3]
      dz <- self$dims[4]
      dt <- self$dims[5]

      #vectorize data
      datavec <- self$data

      #make voxByTime matrix and name rows as locations(xyz)
      voxByTime <- matrix(NA, nrow = nrow(xyzloc), ncol = dt)
      rownames(voxByTime) = apply(xyzloc,1,function(x) { nm <- paste0(x,sep='.',collapse=''); return(substr(nm,1,nchar(nm)-1)) })

      #run over locations and extract time-series
      for(i in 1:nrow(xyzloc)) {

        x <- xyzloc[i,1]
        y <- xyzloc[i,2]
        z <- xyzloc[i,3]

        voxByTime[i,] <- datavec[ ( x + dx*(y-1) ) + ( (dx*dy)*(z-1) ) + ( (dx*dy*dz)*((1:dt)-1) ) ]

      }

      #return matrix
      return(voxByTime)

    }


  ),

  private = list(

    #functions
    writeDataNifti = function() {

      #write the data
      #mode(self$data) <- private$data.type
      #writeBin(as.vector(self$data),private$connection,size=(self$bitpix/8),endian=private$endian)

      #check for slope and make data
      tempdat <- as.vector(self$data)

      if(self$scl_slope!=0) {
        tempdat <- tempdat/self$scl_slope - self$scl_inter
      }

      mode(tempdat) <- private$data.type
      writeBin(as.vector(tempdat),private$connection,size=(self$bitpix/8),endian=private$endian)

      rm(tempdat)

      invisible(self)

    },

    vec2array = function(vec) {

      #check dimension with warning if mismatch
      if(prod(self$dims[2:(self$dims[1]+1)])!=length(vec)) stop('Dimensions in header do not match length of data vector provided.')

      #convert data to array
      self$data <- array(vec,dim=self$dims[2:(self$dims[1]+1)])

      invisible(self)
    },

    matrixfy = function(type = c('sag','axi','cor'), volume = 1, flipx = FALSE) {

      if(self$dims[1]==4) {
        if(is.numeric(volume)) {
          if(volume >= 1 & volume <= self$dims[5]) {
            dat <- self$data[,,,volume]
          } else stop('volume must be within range 1 to',self$dims[5],'\n')
        } else {
          dat <- apply(self$data,c(1,2,3),mean)
        }
      } else if(self$dims[1]==3) dat <- self$data
      else stop('image only possible for 3D or 4D data.')

      if(flipx) {
        dat <- dat[seq(dim(dat)[1],1,-1),,]
      }

      dx <- self$dims[2]
      dy <- self$dims[3]
      dz <- self$dims[4]

      if(tolower(type)[1]=='axi') {

        numrect <- round(sqrt(dz) + .5)
        totmat <- matrix(0,nrow = dx*numrect,ncol = dy*numrect)

        nz = 1
        for(y in numrect:1) {
          for(x in 1:numrect) {

            if(nz>dz) {
              break()
            } else {
              totmat[(1:dx)+(dx*(x-1)),(1:dy)+(dy*(y-1))] <- dat[,,nz]
              nz = nz + 1
            }
          }
        }
      }

      if(tolower(type)[1]=='sag') {

        numrect <- round(sqrt(dx) + .5)
        totmat <- matrix(0,nrow = dy*numrect,ncol = dz*numrect)

        nx = 1
        for(z in numrect:1) {
          for(y in 1:numrect) {

            if(nx>dx) {
              break()
            } else {
              totmat[(1:dy)+(dy*(y-1)),(1:dz)+(dz*(z-1))] <- dat[nx,,]
              nx = nx + 1
            }
          }
        }
      }

      if(tolower(type)[1]=='cor') {

        numrect <- round(sqrt(dy) + .5)
        totmat <- matrix(0,nrow = dx*numrect,ncol = dz*numrect)

        ny = 1
        for(z in numrect:1) {
          for(x in 1:numrect) {

            if(ny>dy) {
              break()
            } else {
              totmat[(1:dx)+(dx*(x-1)),(1:dz)+(dz*(z-1))] <- dat[,ny,]
              ny = ny + 1
            }
          }
        }
      }

      return(totmat)
    }

  )
)

