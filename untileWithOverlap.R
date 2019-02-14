#!/usr/bin/Rscript --vanilla
#    untileWithOverlap - Revert Tiling of an image into overlapping tiles
#    Copyright (C) 2019 Matthias Weigand -- matthias.weigand[at]protonmail.com
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>


#==============================================================================#
# Header for program flags
library(optparse)
option_list = list(
  make_option(c('-i', '--input'), type = 'character', default = "C[[:digit:]]+R[[:digit:]]+.tif", help = 'character. input data file R regex pattern. Default "C[[:digit:]]+R[[:digit:]]+.tif"'),
  make_option(c('-d', '--directory'), type = 'character', default = "tiles", help = 'character. input data file directory'),
  make_option(c('-o', '--output'), type = 'character', default = "tile", help = 'character. output file'),
  make_option(c('-s', '--tileShapes'), type = 'character',default = NA, help = 'character. Location of nBuffPolygons.sqlite'),
  make_option(c('-t', '--dataType'), default = "uint16", type = 'character', help = 'character. Datatype for output tif (see https://gdal.org/frmt_gtiff.html for more info). Default "INT2U"'),
  # make_option(c('-w', '--overwrite'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the output file be overwritten if necessary?'),
  make_option(c('-m', '--multicore'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the program run in parallel?')
)
opt_parser <- OptionParser(usage = 'Usage: %prog -i "\\w.tif$" -d tiles -o untiled.tif -s tiles/nBuffPolygons.sqlite -m -t uint16', 
                           option_list=option_list)
opt <- parse_args(opt_parser)
#==============================================================================#

#==============================================================================#
# Funtion body

suppressMessages(library(raster))
suppressMessages(library(parallel))
suppressMessages(library(gdalUtils))
suppressMessages(library(sf))
suppressMessages(library(stringi))
suppressMessages(library(stringr))
suppressMessages(library(dplyr))


# check whether input, output and tileShape are available
if (is.na(opt$input) | is.na(opt$output) | is.na(opt$tileShapes)){
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call. = FALSE)
}

# detect all input data by input pattern
files <- list.files(path = opt$directory, pattern = opt$input, full.names = TRUE)
cat(paste("Files to be tiled:", length(files), "\n"))

## Correct datatypes ##
# all files for consistent data types
# necessary since vrt does not allow to have differing datatypes
dtypes <- sapply(files, function(x){
  dataType(raster(x))
})

# identify the highest order data types from all dtypes in files
for (dt in dtypes){
  
  # check if target variable was already defined. Usually necessary in 1st iteration
  if (!exists("highestdt")){
    highestdt <- dt
  } else {
    
    # if datatype differs from the current highest
    if (dt != highestdt){
      
      # arrange the current and the highest in an order using factors. 
      # thus comparison by order is possible 
      ord <- ordered(c(dt, highestdt), 
                     c("LOG1S", "INT1U", "INT1S", "INT2U", "INT2S", "INT4U", 
                       "INT4S", "FLT4S", "FLT8S"))
      
      # check whether the current dt is of higher order than currently highest
      if (ord[1] > ord[2]) {
        highestdt <- dt
      }
    }
  }
}

# identify tiles not having the highest order datatype 
filesToCorrect <- files[dtypes != highestdt]

# convert dataType to GDAL notation of datatypes
gdalNotation <- function(d){
  
  if (d == "INT1U"){
    return("Byte")
  } 
  if (d == "LOG1S"){
    return("Byte")
  } 
  
  type <- str_sub(d, 1, 3)
  type <- ifelse(type == "INT", "Int", "Float")
  bit <- as.numeric(str_sub(d, 4, 4)) * 8
  sign <- str_sub(d, 5, 5)
  
  if (sign == "U"){
    return(paste0(sign, type, bit))
  } else {
    return(paste0(type, bit))
  }
}

# function to convert the data type to the highest dt
translateToDt <- function(ras, hdt){
  gdal_translate(ras, 
                 paste0(dirname(ras), "/tmp_", basename(ras)), 
                 ot = hdt, 
                 co = c("COMPRESS=LZW", "BIGTIFF=YES"))
  file.remove(ras)
  file.rename(from = paste0(dirname(ras), "/tmp_", basename(ras)), 
              to = ras)
}

# translate all files not having the highest datatype
cat(paste("Must correct datatype of", length(filesToCorrect), "files\n"))

if (!opt$multicore){
  dump <- sapply(filesToCorrect, 
                 function(x) translateToDt(x, gdalNotation(highestdt)))
} else {
  dump <- mclapply(filesToCorrect, 
                   function(x) translateToDt(x, gdalNotation(highestdt)), 
                   mc.cores = detectCores())
}


## Untile ##
# read polygon shapes from database
polygons <- st_read(opt$tileShapes, quiet = TRUE)

# define function which is applied later
untile <- function(rasterFile){
  # extract the tilename from the raster file's name
  tileName <- stri_extract(rasterFile, regex = "C[[:digit:]]+R[[:digit:]]+")
  
  # generate target extent for vrt
  tileExtent <- polygons %>% 
    filter(tilename == tileName) %>% 
    st_bbox()
  
  # build the vrt
  gdalbuildvrt(gdalfile = rasterFile, 
               output.vrt = gsub(".tif$", ".vrt", rasterFile), 
               te = tileExtent, 
               resolution = "highest")
  
  # return the vrt file name in order to generate large vrt afterwards
  return(gsub(".tif$", ".vrt", rasterFile))
}

# apply the function in parallel if allowed
if (opt$multicore){
  tileVRTs <- unlist(mclapply(files, untile, mc.cores = detectCores()))
} else {
  tileVRTs <- sapply(files, untile)
}

# build one giant vrt from all vrt files
invisible(gdalbuildvrt(gdalfile = tileVRTs, 
                       output.vrt = "tmpout.vrt",
                       resolution = "highest"))

# gdal create options with or without multicore 
if (!opt$multicore){
  cos <- c("COMPRESS=LZW", "BIGTIFF=YES")
} else {
  cos <- c("COMPRESS=LZW", "BIGTIFF=YES", "NUM_THREADS=ALL_CPUS")
}

invisible(gdal_translate("tmpout.vrt", dst_dataset = opt$output, 
               ot = opt$dataType, of = "GTiff", 
               co = cos))

invisible(file.remove(tileVRTs))
invisible(file.remove("tmpout.vrt"))




