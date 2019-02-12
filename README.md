# geoprocessors
A collection of R scripts (among others) for geographic processing wrapped in command line tools. 
---
## Overview
geoprocessors is a collection of command line tools created from the languages **R** and **Python** and others. 
Please refer to the requirements necessary for running all programs. 

## Installation 

Install this toolbox by installing all needed dependencies (see [here](https://github.com/mwip/geoprocessors#requirements)) and clone this repository. Ensure `chmod +x` each of the processors, if they are not marked as executable. 

## Export Path

If you want to use the wrapper programs from all directories just by calling the function's name, you need to adjust the PATH variable. This is done by adding the directory you cloned this repository into to the environment.  
Add the following to your `~/.bashrc` or `~/.profile`:
```
export PATH=$PATH:/the/path/to/geoprocessors
```


## Requirements
- GDAL
- R
- R-Packages
    + sf
    + raster
    + sp
    + optparse
    + igraph
    + [TileManager](https://github.com/AndyPL22/TileManager)
- Python 3
- QGIS
    + python-qgis
    + python-gdal

---
## Contributions

The authors acknowledge all the work and effort by the contrigutors to the various R and python libraries. Withouth their work, this collection of geoprocessors would never be possible. 


