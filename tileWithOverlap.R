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
  make_option(c('-e', '--exportShapes'), action = 'store_true', type = 'logical',default = FALSE, help = 'character. Export tile index to output directory? See `?TileManager::TileScheme` for info on the three output shapes.'),
  make_option(c('-c', '--dimByCell'), type = 'numeric', default = NULL, help = 'Vector of two numbers. Defines the \'x\' and \'y\' dimensions in distance units of input.'),
  make_option(c('-d', '--dimByDist'), type = 'numeric', default = NULL, help = 'Vector of two numbers. Defines the \'x\' and \'y\' dimensions in number of cells.'),
  make_option(c('-b', '--buffer'), type = 'numeric', default = 0, help = 'numeric. Default 0. If set to >0, overlapping buffers will be created around each tile. Defined in cell number or distance unit, depending on the the usage of dim.by.cell or dim.by.dist respectively'),
  make_option(c('-s', '--bufferspill'), action = 'store_true', type = 'logical', default = FALSE, help = 'logical. Default FALSE, in which case the tiling grid will be pushed inwards so that the buffers of the outer tiles are within the extent of input. If set to TRUE, the buffers will extend outside of the extent of input'),
  make_option(c('-r', '--removeEmpty'), action = 'store_true', type = 'logical', default = FALSE, help = 'logical. Default is FALSE. If set to TRUE, tiles containing only NA cell values will be removed from the tiling scheme.'),
  make_option(c('-t', '--dataType'), default = "INT2U", type = 'character', help = 'character. Datatype for output tif (see `?raster::dataType` for more info). Default "INT2U"'),
  make_option(c('-w', '--overwrite'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the output file be overwritten if necessary?'),
  make_option(c('-m', '--multicore'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. Should the program run in parallel?')
)
opt_parser <- OptionParser(usage = "Usage: %prog -i input.tif -o output.tif -r 100 -f mean -overwrite", 
                           option_list=option_list)
opt <- parse_args(opt_parser)
#==============================================================================#

#==============================================================================#
# Funtion body

suppressMessages(library(TileManager))
suppressMessages(library(raster))
suppressMessages(library(parallel))
suppressMessages(library(rgdal))

# check if input and output files are given
if (is.na(opt$input) | is.na(opt$output)){
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call. = FALSE)
}

# create output directory
dir.create(opt$output, showWarnings = FALSE)

# read raster
r <- raster(opt$input)

# apply the tiling scheme
ts <- TileScheme(r, dimByCell = opt$dimByCell, dimByDist = opt$dimByDist,
                 buffer = opt$buffer, bufferspill = opt$bufferspill,
                 removeEmpty = opt$removeEmpty)

# create function which is applied later
tileRasters <- function(x){

  # select tile
  t <- ts[[2]][x, ]

  # create output file name
  ofn <- paste0(opt$output, "/", gsub(".tif$", paste0("_", t$tileName, ".tif"), opt$input))

  # do the magic
  crop(r, t,
       filename = ofn,
       format = "GTiff", datatype = opt$dataType,
       overwrite = opt$overwrite,
       options = c("COMPRESS=LZW", "BIGTIFF=IF_NEEDED"))
}

# apply the function either multi or single core
if (opt$multicore) {
  dump <- mclapply(1:length(ts[[2]]), tileRasters, mc.cores = parallel::detectCores())
} else {
  dump <- lapply(1:length(ts[[2]]), tileRasters)
}

# export tile scheme if set
if (opt$exportShapes){
  suppressWarnings(file.remove(paste0(opt$output, "/tilePolygons.sqlite"), 
                               paste0(opt$output, "/buffPolygons.sqlite"), 
                               paste0(opt$output, "/nBuffPolygons.sqlite")))
  sf::st_write(sf::st_as_sf(ts[[1]]), quiet = TRUE, 
               dsn = paste0(opt$output, "/tilePolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
  sf::st_write(sf::st_as_sf(ts[[2]]), quiet = TRUE,
               dsn = paste0(opt$output, "/buffPolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
  sf::st_write(sf::st_as_sf(ts[[3]]), quiet = TRUE,
               dsn = paste0(opt$output, "/nBuffPolygons.sqlite"), layer = "tiles",
               driver = "SQLite", dataset_options = c("SPATIALITE=YES"))
}

