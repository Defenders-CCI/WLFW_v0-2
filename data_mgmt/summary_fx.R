# Functions to summarize data for a Shiny App.
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

############################################################################
# Helper functions for factors
fac2num <- function(x) {
    return(as.numeric(as.character(x)))
}

fac2char <- function(x) {
    return(as.character(x))
}

##############################################################################
# Make the data writeable.
##############################################################################
make_writeable <- function(x) {
    a_copy <- x
    a_copy$spp_ev_ls <- unlist(lapply(a_copy$spp_ev_ls, FUN=paste, collapse="; "))
    a_copy$spp_BO_ls <- unlist(lapply(a_copy$spp_BO_ls, FUN=paste, collapse="| "))
    spp_ev_ls <- gsub(pattern="\n", replace="", a_copy$spp_ev_ls)
    spp_BO_ls <- gsub(pattern="\n", replace="", a_copy$spp_BO_ls)
    a_copy$spp_ev_ls <- spp_ev_ls
    a_copy$spp_BO_ls <- spp_BO_ls
    return(a_copy)
}


