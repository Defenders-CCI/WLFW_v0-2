# Functions to create dataframes, typically for plot generation.
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
# Create the df for mapping the selected values
#
# Yes, this contains some ugly code...
make_alt_map_df <- function(sub, esls, resp) {
    ac_per_cont <- function(x) {
        x$tmp <- paste(x$contract_id, x$total_treated_acres, x$GEOID)
        y <- x[duplicated(x$tmp) == FALSE, ]
        res <- tapply(y$total_treated_acres,
                      INDEX=y$GEOID,
                      FUN=sum, na.rm=TRUE)
        res
    }

    tot_spend <- tapply(sub$Practice_Obligations,
                        INDEX=sub$GEOID,
                        FUN=sum, na.rm=TRUE)
    ac_treated <- ac_per_cont(sub)
    dol_ac <- tapply(sub$Practice_Obligations,
                     INDEX=sub$GEOID,
                     FUN=sum, na.rm=TRUE) / ac_per_cont(sub)
    n_contract <- tapply(sub$contract_id,
                         INDEX=sub$GEOID,
                         FUN=function(x) {length(levels(droplevels(x)))})
    if (length(tot_spend) > 0) {
        to_remove <- setdiff(esls@data[[4]], names(tot_spend))
        to_keep <- intersect(esls@data[[4]], names(tot_spend))
        shp_absent <- esls[esls@data[[4]] %in% to_remove, ]
        shp_presnt <- esls[esls@data[[4]] %in% to_keep, ]
        shp_absent@data[[19]] <- rep(NA, length(shp_absent@data[[4]]))
        shp_absent@data[[20]] <- rep(NA, length(shp_absent@data[[4]]))
        shp_absent@data[[21]] <- rep(NA, length(shp_absent@data[[4]]))
        shp_absent@data[[22]] <- rep(NA, length(shp_absent@data[[4]]))
        tot_spend <- tot_spend[to_keep]
        ac_treated <- ac_treated[to_keep]
        dol_ac <- dol_ac[to_keep]
        n_contract <- n_contract[to_keep]
        shp_presnt@data[[19]] <- as.vector(tot_spend)
        shp_presnt@data[[20]] <- as.vector(ac_treated)
        shp_presnt@data[[21]] <- as.vector(dol_ac)
        shp_presnt@data[[22]] <- as.vector(n_contract)
        new_shp <- rbind(shp_presnt, shp_absent)
    } else {
        esls@data[[19]] <- rep(0, length(esls@data[[4]]))
        esls@data[[20]] <- rep(0, length(esls@data[[4]]))
        esls@data[[21]] <- rep(0, length(esls@data[[4]]))
        esls@data[[22]] <- rep(0, length(esls@data[[4]]))
        new_shp <- esls
    }
    return(new_shp)
}

############################################################################
# Create a small dataframe for top 25 species bar plot
tooltips <- function(sp, dol, src) {
    paste0("<div style='padding:5px 5px 5px 5px;'><b>",
           sp, '</b><br>', src, ":<br>$", prettyNum(dol, big.mark=","), "</div>")
}

make_top_25_df <- function(sub, x, y) {
  observe({print(c(x, y))})
    res <- tapply(sub[[y]], INDEX=sub[[x]], FUN=sum, na.rm=TRUE)
    if (x == "fy") {
        res_df <- data.frame(num=rep(1:length(res)))
        res_df[[axis_lab(x)]] <- names(res)
        res_df[[axis_lab(y)]] <- as.vector(res)
    } else {
        end <- ifelse(length(res) > 24, 25, length(res))
        res <- sort(res, decreasing=TRUE)[1:end]
        res_df <- data.frame(num=rep(1:length(res)))
        res_df[[axis_lab(x)]] <- names(res)
        res_df[[axis_lab(y)]] <- as.vector(res)
    }
    return(res_df)
}

make_scatterp_df <- function(sub, x, y) {
    if (y != "number_contracts") {
        res_df <- data.frame(num=c(1:length(sub[[x]])))
        res_df[[axis_lab(x)]] <- sub[[x]]
        res_df[[axis_lab(y)]] <- sub[[y]]
        res_df <- res_df[, -1]
    } else {
        n_items <- table(sub$contract_id)
        dol_contract <- tapply(sub$Contract_Obligation,
                               INDEX=sub$contract_id,
                               FUN=mean, na.rm=TRUE)
        observe({print(head(dol_contract))})
        res_df <- data.frame(num=c(1:length(sub[[x]])))
        res_df[[axis_lab(x)]] <- as.vector(dol_contract)
        res_df[[axis_lab(y)]] <- as.vector(n_items)
        res_df <- res_df[, -1]
    }
    return(res_df)
}

# make_hist_df <- function(sub, y) {
#     if (y != "number_contracts") {
#         res_df2 <- data.frame(num=c(1:length(sub[[y]])))
#         res_df2[[axis_lab(y)]] <- sub[[y]]
#     } else {
#         n_contr <- table(sub$contract_id)
#         res_df2 <- data.frame(num=c(1:length(n_contr)))
#         res_df2[[axis_lab(y)]] <- as.vector(n_contr)
#     }
#     return(res_df2)
# }

