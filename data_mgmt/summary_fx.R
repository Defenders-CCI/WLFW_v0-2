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

cv <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

make_bar_caption <- function(dat, x, y) {
  cur_dat <- make_top_25_df(dat, x, y)
  cur_cv <- round(cv(cur_dat[, 3]), 3)
  if(cur_cv <= 0.33) {
    qual <- "relatively low"
  } else if(cur_cv >= 1) {
    qual <- "relatively high"
  } else {
    qual <- "moderate"
  }
  resp <- sprintf(
    "<p>With a coefficient of variation of %s, the selected data shows %s
    variability among categories.</p>",
    cur_cv,
    qual
  )
  return(resp)
}

make_scatter_caption <- function(dat, x, y) {
  cur_dat <- make_scatterp_df(dat, x, y)
  correl <- cor.test(log(cur_dat[,1]+1), log(cur_dat[,2]+1))
  if(correl$estimate > 0.5) {
    qual <- "relatively strong"
  } else if(correl$estimate < 0.2) {
    qual <- "relatively weak"
  } else {
    qual <- "moderate"
  }
  cap <- sprintf("<p>The correlation, %s (p = %s), is %s.</p>",
                 round(correl$estimate, 3),
                 format(correl$p.value, scientific = TRUE),
                 qual)
  return(cap)
}
