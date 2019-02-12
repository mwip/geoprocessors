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
suppressMessages(library(dplyr))


# check whether input, output and tileShape are available
if (is.na(opt$input) | is.na(opt$output) | is.na(opt$tileShapes)){
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call. = FALSE)
}

# detect all input data by input pattern
files <- list.files(path = opt$directory, pattern = opt$input, full.names = TRUE)

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
               te = tileExtent)
  
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
                       output.vrt = "tmpout.vrt"))

# gdal create options with or without multicore 
if (!opt$multicore){
  cos <- c("COMPRESS=LZW", "BIGTIFF=IF_NEEDED")
} else {
  cos <- c("COMPRESS=LZW", "BIGTIFF=IF_NEEDED", "NUM_THREADS=ALL_CPUS")
}

invisible(gdal_translate("tmpout.vrt", dst_dataset = opt$output, 
               ot = opt$dataType, of = "GTiff", 
               co = cos))

invisible(file.remove(tileVRTs))
invisible(file.remove("tmpout.vrt"))




