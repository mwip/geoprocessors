#! /usr/bin/Rscript --vanilla
# connectedCells - derive clumps of connected raster cells sharing the 
#   same raster value
# Copyright (C) 2019 AUTHOR NAME -- matthias.weigand[at]protonmail.com
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#==============================================================================#
# Header for program flags
library(optparse)
option_list = list(
  make_option(c('-i', '--input'), type = 'character', default = NA, help = 'character. input data file'),
  make_option(c('-o', '--output'), type = 'character', default = NA, help = 'character. output data file, must be GeoTIFF'),
  make_option(c('-d', '--directions'), type = 'numeric', default = 8, help = 'integer. which cells are considered adjacent? Should be 8 (Queen\'s case) or 4 (Rook\'s case). default 8'),
  make_option(c('-g', '--gaps'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical.  If TRUE, there may be ’gaps’ in the chunk numbers (e.g. you may have clumps with IDs 1, 2, 3 and 5, but not 4).  If it is FALSE, these numbers will be recoded from 1 to n (4 in this example)'),
  make_option(c('-w', '--overwrite'), action = 'store_true', default = FALSE, type = 'logical', help = 'logical. should the output file be overwritten if necessary')
)
opt_parser <- OptionParser(usage = "Usage: %prog -i input.tif -o output.tif  -w",
                           option_list=option_list)
opt <- parse_args(opt_parser)
#==============================================================================#


#==============================================================================#
# Funtion body
suppressMessages(library(raster))

# check if input and output files are given
if (is.na(opt$input) | is.na(opt$output)){
  print_help(opt_parser)
  stop("Please specify input AND output files\n", call.=FALSE)
}

# check if output file already exists
if (file.exists(opt$output) & !opt$overwrite){
  stop("Output file exists and would be overwritten.\n
       Consider adding -w for overwriting existing files.")
} else if (file.exists(opt$output) & opt$overwrite) {
  # fix issue with clump(overwrite = ...) not accepting opt$overwrite
  invisible(file.remove(opt$output))
}

# this is where the magic happens
suppressPackageStartupMessages(
  clump(raster(opt$input), directions = opt$directions, gaps = opt$gaps,
        filename = opt$output, overwrite = opt$overwrite,
        options = c("COMPRESS=LZW", "BIGTIFF=IF_NEEDED"))
)

# remove created temporary files
removeTmpFiles(0)

