#!/usr/bin/Rscript --vanilla
#    tileWithOverlap - Tile an image into overlapping tiles
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
  make_option(c('-i', '--input'), type = 'character', default = NA, help = 'character. input data file'),
  make_option(c('-o', '--output'), type = 'character', default = "tile", help = 'character. output data directory'),
  make_option(c('-c', '--dimByCell'), type = 'character', default = NULL, help = 'Character. String of one or two numbers (e.g. "2500,2500", or "130"). Defines the \'x\' and \'y\' dimensions in number of cells.'),
  make_option(c('-d', '--dimByDist'), type = 'character', default = NULL, help = 'Character. String of one or two numbers (e.g. "2500,2500", or "130"). Defines the \'x\' and \'y\' dimensions in distance units of input (Check geographic reference system for units).'),
  make_option(c('-b', '--buffer'), type = 'numeric', default = 0, help = 'numeric. Default 0. If set to >0, overlapping buffers will be created around each tile. Defined in cell number or distance unit, depending on the the usage of dim.by.cell or dim.by.dist respectively'),
  make_option(c('-s', '--bufferspill'), action = 'store_true', type = 'logical', default = FALSE, help = 'logical. Default FALSE, in which case the tiling grid will be pushed inwards so that the buffers of the outer tiles are within the extent of input. If set to TRUE, the buffers will extend outside of the extent of input'),
  make_option(c('-r', '--removeEmpty'), action = 'store_true', type = 'logical', default = FALSE, help = 'logical. Default is FALSE. If set to TRUE, tiles containing only NA cell values will be removed from the tiling scheme.'),
  make_option(c('-a', '--aoi'), default = NULL, type = 'character', help = 'character. File holding polygon geometry of the Area of Interest, the tiles will be selected by.'),
  make_option(c('-t', '--dataType'), default = "UInt16", type = 'character', help = 'character. GDAL datatype for output tif. Default "UInt16"'),
  make_option(c('-e', '--exportShapes'), action = 'store_true', type = 'logical',default = FALSE, help = 'logical. Export tile index to output directory? See `Rscript -e "?TileManager::TileScheme"` section \'Value\' for info on the three output shapes that will be genereated if -e is given.'),
  make_option(c('-w', '--overwrite'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the output file be overwritten if necessary?'),
  make_option(c('-m', '--multicore'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the program run in parallel?'),
  make_option(c('-p', '--processors'), type = 'numeric', default = 0, help = 'numeric. Default 0. If set to >0, the respective number of processors will be used. if set to 0 ALL processors will be used')
)
opt_parser <- OptionParser(usage = "Usage: %prog -i test.tif -o tiles -c 1000 -b 20 -m -e -w", 
                           option_list=option_list)
opt <- parse_args(opt_parser)
#==============================================================================#

#==============================================================================#
# Funtion body

suppressMessages(library(TileManager))
suppressMessages(library(raster))
suppressMessages(library(parallel))
suppressMessages(library(sf))

# check if input and output files are given
if (is.na(opt$input) | is.na(opt$output)){
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call. = FALSE)
}

# create function handling character input of dimensions -d or -c
handleChrStrings <- function(x){
  if (is.null(x)){
    return(NULL)
  }
  
  as.numeric(unlist(strsplit(x, ",")))
  
}

# create output directory
dir.create(opt$output, showWarnings = FALSE)

# read raster
r <- stack(opt$input)

# apply the tiling scheme
ts <- TileScheme(r, dimByCell = handleChrStrings(opt$dimByCell), 
                 dimByDist = handleChrStrings(opt$dimByDist),
                 buffer = opt$buffer, bufferspill = opt$bufferspill,
                 removeEmpty = opt$removeEmpty)

if (!is.null(opt$aoi)){
  cat("Using aoi to select tiles\n")
  aoi <- st_read(opt$aoi, quiet = TRUE) %>% st_transform(as.character(crs(r)))
  tilesToExport <- which(st_as_sf(ts[[2]]) %>% st_intersects(aoi) %>% lengths() > 0)
} else {
  tilesToExport <- 1:length(ts[[2]])
}

# create function which is applied later
tileRasters <- function(x){

  # select tile
  t <- ts[[2]][x, ]
  bbx <- st_bbox(t)

  # create output file name
  ofn <- paste0(opt$output, "/", gsub(".tif$|.vrt$", paste0("_", t$tileName, ".tif"), 
                                      basename(opt$input)))

  # do the magic gdal style
  gdalUtils::gdalwarp(
    ot = opt$dataType, 
    te = bbx, 
    srcfile = opt$input, 
    dstfile = ofn, 
    tap = TRUE,
    tr = res(r),
    co = c("BIGTIFF=YES", "COMPRESS=LZW"), 
    overwrite = TRUE
  )
}

# export tile scheme if set
if (opt$exportShapes){
  suppressWarnings(file.remove(paste0(opt$output, "/tilePolygons.sqlite"), 
                               paste0(opt$output, "/buffPolygons.sqlite"), 
                               paste0(opt$output, "/nBuffPolygons.sqlite")))
  sf::st_write(sf::st_as_sf(ts[[1]][tilesToExport,]), quiet = TRUE, 
               dsn = paste0(opt$output, "/tilePolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
  sf::st_write(sf::st_as_sf(ts[[2]][tilesToExport,]), quiet = TRUE,
               dsn = paste0(opt$output, "/buffPolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
  sf::st_write(sf::st_as_sf(ts[[3]][tilesToExport,]), quiet = TRUE,
               dsn = paste0(opt$output, "/nBuffPolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
}

# apply the function either multi or single core
if (opt$multicore) {
  dump <- mclapply(tilesToExport, tileRasters, mc.cores = ifelse(opt$processors == 0, parallel::detectCores(), opt$processors))
} else {
  dump <- lapply(tilesToExport, tileRasters)
}


# #### dev ####
# setwd("~/Documents/tmp/")
# list.files()
# opt <- list(input = "test.tif",
#             output = "tiletest",
#             multicore = TRUE,
#             dimByCell = "750, 750",
#             dimByDist = NULL,
#             buffer = 10,
#             bufferspill = FALSE,
#             removeEmpty = FALSE,
#             dataType = "UInt16", 
#             aoi = "aoi.sqlite")
