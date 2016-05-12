# Functions to subset an input dataset, x.
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

##############################################################################
# Return a subset of the WLFW db (x) based on a suite of variables.
##############################################################################
sub_df <- function(x, FY, species, practice) {
    if (FY != "All") {
        x <- x[x$fy == FY, ]
    }
    if (species != "All") {
        cur_sp <- switch(species,
            "botu" = "Bog turtle",
            "goto" = "Gopher tortoise",
            "gwwa" = "Golden-winged Warbler",
            "lpch" = "Lesser Prairie-Chicken",
            "neco" = "New England cottontail",
            "grsg" = "Sage Grouse",
            "swfl" = "Southwestern Willow Flycatcher")
        x <- x[x$species %in% cur_sp, ]
    }
    if (practice != "All") {
        x <- x[as.character(x$practice_name) == practice, ]
    }
    return(x)

}

