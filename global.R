# Global functions to be called at app initiation.
# Copyright (C) 2015 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#############################################################################
# Load packages and source files
#############################################################################
library(DT)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(shinyBS)

library(leaflet)
library(maptools)
library(sp)

library(googleVis)
library(plyr)

source("data_mgmt/make_dataframes.R")
source("data_mgmt/subset_fx.R")
source("data_mgmt/summary_fx.R")
source("plot/graphs.R")
source("txt/metadata.R")
source("txt/notes.R")

#############################################################################
# Load the data and basic data prep
#############################################################################
# load("data/WLFW_data_final_31Dec2015.RData")
load("data/23Feb2017_WLFW_data_upd.rda")
full$practice_code <- as.factor(full$practice_code)
full$practice_units <- as.factor(full$practice_units)
full$species <- as.factor(full$species)

#############################################################################
# Get variables for selections
#
# To facilitate adding new data, generate the vectors from the data
years <- c("All", unique(full$fy))
states <- c("All", levels(full$st_nm))
counties <- c("All", levels(full$cnt_st))
programs <- c("All", levels(full$program_name))
species <- c("All", levels(full$species))
contract_stats <- c("All", levels(full$contract_status))
practice_codes <- c("All", as.character(levels(as.factor(full$practice_code))))
practice_names <- c("All", levels(full$practice_name))

#############################################################################
# Get core/supporting data
core <- read.table("data/core_practices.tab",
                   sep="\t",
                   header=TRUE,
                   stringsAsFactors=FALSE)
support <- read.table("data/supporting_practices.tab",
                      sep="\t",
                      header=TRUE,
                      stringsAsFactors=FALSE)

#############################################################################
# Now, load and prep each of the shapefiles...
allsp_shp <- readShapePoly("data/all_spp_simple2.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
allsp_shp <- allsp_shp[order(allsp_shp@data[[4]]), ]
allsp_shp@data[[18]] <- duplicated(allsp_shp@data[[4]])
allsp_shp <- allsp_shp[allsp_shp@data[[18]] == FALSE, ]
allsp_shp@data[[4]] <- as.character(allsp_shp@data[[4]])

BOTU_shp <- readShapePoly("data/BOTU/BOTU_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
BOTU_shp <- BOTU_shp[order(BOTU_shp@data[[4]]), ]
BOTU_shp@data[[18]] <- duplicated(BOTU_shp@data[[4]])
BOTU_shp <- BOTU_shp[BOTU_shp@data[[18]] == FALSE, ]
BOTU_shp@data[[4]] <- as.character(BOTU_shp@data[[4]])

GOTO_shp <- readShapePoly("data/GOTO/GOTO_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
GOTO_shp <- GOTO_shp[order(GOTO_shp@data[[4]]), ]
GOTO_shp@data[[18]] <- duplicated(GOTO_shp@data[[4]])
GOTO_shp <- GOTO_shp[GOTO_shp@data[[18]] == FALSE, ]
GOTO_shp@data[[4]] <- as.character(GOTO_shp@data[[4]])

GRSG_shp <- readShapePoly("data/GRSG/GRSG_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
GRSG_shp <- GRSG_shp[order(GRSG_shp@data[[4]]), ]
GRSG_shp@data[[18]] <- duplicated(GRSG_shp@data[[4]])
GRSG_shp <- GRSG_shp[GRSG_shp@data[[18]] == FALSE, ]
GRSG_shp@data[[4]] <- as.character(GRSG_shp@data[[4]])

GWWA_shp <- readShapePoly("data/GWWA/GWWA_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
GWWA_shp <- GWWA_shp[order(GWWA_shp@data[[4]]), ]
GWWA_shp@data[[18]] <- duplicated(GWWA_shp@data[[4]])
GWWA_shp <- GWWA_shp[GWWA_shp@data[[18]] == FALSE, ]
GWWA_shp@data[[4]] <- as.character(GWWA_shp@data[[4]])

LPCH_shp <- readShapePoly("data/LPCH/LPCH_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
LPCH_shp <- LPCH_shp[order(LPCH_shp@data[[4]]), ]
LPCH_shp@data[[18]] <- duplicated(LPCH_shp@data[[4]])
LPCH_shp <- LPCH_shp[LPCH_shp@data[[18]] == FALSE, ]
LPCH_shp@data[[4]] <- as.character(LPCH_shp@data[[4]])

NECO_shp <- readShapePoly("data/NECO/NECO_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
NECO_shp <- NECO_shp[order(NECO_shp@data[[4]]), ]
NECO_shp@data[[18]] <- duplicated(NECO_shp@data[[4]])
NECO_shp <- NECO_shp[NECO_shp@data[[18]] == FALSE, ]
NECO_shp@data[[4]] <- as.character(NECO_shp@data[[4]])

SWFL_shp <- readShapePoly("data/SWFL/SWFL_simple005.shp",
                           proj4string=CRS("+proj=merc +lon_0=90w"))
SWFL_shp <- SWFL_shp[order(SWFL_shp@data[[4]]), ]
SWFL_shp@data[[18]] <- duplicated(SWFL_shp@data[[4]])
SWFL_shp <- SWFL_shp[SWFL_shp@data[[18]] == FALSE, ]
SWFL_shp@data[[4]] <- as.character(SWFL_shp@data[[4]])

#############################################################################
# Now, get the extents and midpoint of each of the shapefiles for zooming in
getExtents <- function(x) {
    extent <- as.vector(bbox(x))
    xmin <- extent[1]
    ymin <- extent[2]
    xmax <- extent[3]
    ymax <- extent[4]
    xmid <- (xmin + xmax) / 2
    ymid <- (ymin + ymax) / 2
    return(list(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, xmid=xmid, ymid=ymid))
}

allsp_ext <- getExtents(allsp_shp)
BOTU_ext <- getExtents(BOTU_shp)
GOTO_ext <- getExtents(GOTO_shp)
GRSG_ext <- getExtents(GRSG_shp)
GWWA_ext <- getExtents(GWWA_shp)
LPCH_ext <- getExtents(LPCH_shp)
NECO_ext <- getExtents(NECO_shp)
SWFL_ext <- getExtents(SWFL_shp)

#############################################################################
# update colors for CSS
validColors_2 <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                   "navy", "teal", "olive", "lime", "orange", "orange_d", "fuchsia",
                   "purple", "maroon", "black")

validateColor_2 <- function(color) {
    if (color %in% validColors_2) {
        return(TRUE)
    }

    stop("Invalid color: ", color, ". Valid colors are: ",
         paste(validColors_2, collapse = ", "), ".")
}

