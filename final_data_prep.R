# BSD_2_clause
#
# One final round of data prep before publishing the app.

library(anytime)
library(dplyr)
library(rio)

load("data/WLFW_data_final_31Dec2015.RData") # 'full'

# Turns out that NRCS did the data dump differently than past data dumps. Yay.
# So I am going to do some basic joins and filters on a too-big dataframe,
# export to xlsx to move columns around faster, then re-import for joins with
# TIGR. Sigh.
cont <- import("data/Req_103_16_FOIA_WLFW_rev.xlsx", which = 1)
proj <- import("data/Req_103_16_FOIA_WLFW_rev.xlsx", which = 2)

newd <- left_join(proj, cont, by = "contract_id")
newrec <- setdiff(unique(newd$contract_id),
                  unique(as.character(full$contract_id)))

dat <- filter(newd, contract_id %in% newrec)
length(unique(dat$contract_id))
export(dat, file = "data/fix2016_FOIA.xlsx")

# Changes: reorder columns and homogenize variable names with overlap in `full`

dat <- import("data/f2016_FOIA_fixed.xlsx")


simpleCap <- function(x) {
    if (!is.na(x)) {
        s <- strsplit(x, " ")[[1]]
        paste(substring(s, 1,1), tolower(substring(s, 2)),
              sep="", collapse=" ")
    } else {
        NA
    }
}

dat$county_name <- sapply(dat$cnty_nm, FUN=simpleCap)
dat$county_name <- gsub("St ", "St. ", dat$county_name, fixed=TRUE)
dat$county_name <- gsub("Mccone", "McCone", dat$county_name, fixed=TRUE)
dat$county_name <- gsub("Miami-dade", "Miami-Dade", dat$county_name, fixed=TRUE)
dat$county_name <- gsub("Dona Ana", "Doña Ana", dat$county_name, fixed=TRUE)
dat$state_name <- as.character(dat$st_nm)
dat$county_state <- paste(dat$county_name, dat$state_name)
head(dat$county_state)

# On to TIGR and FIPS!
fixFIPS <- function(x) {
    if (nchar(x) == 4) {
        return(paste0("0", x))
    } else {
        return(x)
    }
}

# Now get the TIGER data to pull in GEOIDs
TIGER <- read.csv("data/prep_data/US_states_counties_TIGER_v1-1.tab",
                  header=TRUE,
                  sep="\t")
TIGER$GEOID <- as.character(TIGER$GEOID)
TIGER$GEOID <- sapply(TIGER$GEOID, FUN=fixFIPS)
head(TIGER$GEOID)
TIGER$county_state <- paste(TIGER$NAME, TIGER$STATE)

combo <- left_join(dat, TIGER, by = "county_state")
names(combo) == names(full)
names(combo)[13] <- "contract_item_id"
names(combo)[19] <- "Practice_Obligations"
combo$dups <- NA
all(names(combo) == names(full))

combo$cnt_st <- paste(combo$county_name, "Co.,", combo$state_name)
combo$species <- ifelse(
  combo$account_type_name == "Sage-Grouse Initiative",
  "Sage Grouse",
  ifelse(
    combo$account_type_name == "G Tortoise WLFW",
    "Gopher tortoise",
    ifelse(
      combo$account_type_name == "G Winged Warbler WLFW",
      "Golden-winged Warbler",
      ifelse(
        combo$account_type_name == "Lesser Prairie Chicken Initiative",
        "Lesser Prairie-Chicken",
        ifelse(
          combo$account_type_name == "NE Cottontail WLFW",
          "New England cottontail",
          ifelse(
            combo$account_type_name == "SWN Flycatcher WLFW",
            "Southwestern Willow Flycatcher",
            "Bog turtle"
          )
        )
      )
    )
  )
)

test <- rbind(full, combo)
full_bak <- full
save(full_bak, file = "data/23Feb2017_full_bak.rda")
full <- test
save(full, file = "data/23Feb2017_WLFW_data_upd.rda")

