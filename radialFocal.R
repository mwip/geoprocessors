#! /usr/bin/Rscript --vanilla
#    radialFocal - Calculate a radial focal filter for an image
#    Copyright (C) 2020 Matthias Weigand -- matthias.weigand[at]protonmail.com
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
option_list <- list(
  make_option(c('-i', '--input'), type = 'character', default = NA, help = 'character. input data file'),
  make_option(c('-o', '--output'), type = 'character', default = NA, help = 'character. output data file, must be GeoTIFF'),
  make_option(c('-r', '--radius'), type = 'numeric', default = 100, help = 'numeric. radius in map units (defined by inputfile crs), default 100'),
  make_option(c('-f', '--fun'), type = 'character', default = "mean", help = 'character. specify the function you want to use, refer to: R -e "?raster::focal"'), 
  make_option(c('-w', '--overwrite'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. should the output file be overwritten if necessary')
)
opt_parser <- OptionParser(usage = "Usage: %prog -i input.tif -o output.tif -r 100 -f mean -overwrite", 
                           option_list=option_list)
opt <- parse_args(opt_parser)
#==============================================================================#


#==============================================================================#
# Funtion body
suppressMessages(library(raster))

# check if input and output files are given
if (is.na(opt$input) | is.na(opt$output)) {
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call. = FALSE)
}

# load input image
r <- raster(opt$input)

# create circular focal weight matrix
w <- focalWeight(r, d = opt$radius, type = 'circle')
w <- ifelse(w > 0, 1, NA)

the_fun <- get(opt$fun)

# apply focal matrix ver image
focal(r, w,
      fun = function(x, na.rm, ... = the_fun) {
        if (na.rm) {
          x <- na.omit(x)
        }
        ...(x)
      },
      pad = TRUE, pad.value = NA, na.rm = TRUE,
      filename = opt$output, overwrite = opt$overwrite,
      options = c("COMPRESS=LZW", "BIGTIFF=IF_NEEDED"))

# remove created temporary files
removeTmpFiles(0)
