#niftiR6 helperFunctions (not exported)

#functions
removeNullString <- function(raw)
#remove embedded and trailing nullstrings
{
  rm = which(raw==0)
  if(length(rm)>0) raw = raw[-rm]
  return(raw)
}

#calculate decimal value for a binary vector
bin2dec <- function(binvec)
{
  value <- as.integer(binvec[1])
  if(length(binvec)>1) {
    for(i in 2:(length(binvec))) {
      temp = value
      value = temp*2+as.integer(binvec[i])
    }
  }
  return(as.integer(value))
}

#onLoad function displays when
.onLoad <- function(...) {
      packageStartupMessage(paste0('This is niftiR6 version ',packageVersion('niftiR6'),'\nniftiR6 is BETA software and comes with ABSOLUTELY NO WARRANTY.'))
}


