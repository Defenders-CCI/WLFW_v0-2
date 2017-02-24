# Interactive map page for the WLFW app.
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

openSans <- "<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400'
      rel='stylesheet' type='text/css'>"
fontAwesome <- "<link rel='stylesheet'
         href='https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css'/>"

map_page <- {
  tabPanel(
    title="Interactive map",
		div(class="outer",
		  tags$head(
		    HTML("<title>Working Lands for Wildlife</title>"),
        HTML(paste(openSans, fontAwesome)),
        includeCSS("www/custom_styles.css")
      ),
      tags$style(type="text/css", "body {padding-top: 80px;}"),
      leafletOutput("map", height="100%", width="100%"),

      # # Add the Defenders logo
      absolutePanel(id = "help-button-box", class = "panel panel-default",
        fixed = TRUE, draggable = FALSE, top = "auto", left = "auto",
        right = 20, bottom = 20, width = 100, height = "auto",
        a(href="http://www.defenders.org",
          target = "_blank",
          imageOutput("defenders", height=NULL))
      ),

      # Add map style choices
      absolutePanel(id = "controls", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE, top = 60, left = "auto",
        right = 340, bottom = "auto", width = 200, height = "auto",

        box(title="Map options",
          status="warning",
          solidHeader=FALSE,
          height=NULL,
          width=NULL,
          collapsible=TRUE,
          collapsed=TRUE,
          selectInput(
            inputId="map_tile",
            label="Map style",
            choices=c("Stamen toner light" = "Stamen.TonerLite",
                  "Stamen toner dark" = "Stamen.Toner",
                  "Stamen watercolor" = "Stamen.Watercolor",
                  "OpenStreetMap Mapnik" = "OpenStreetMap.Mapnik",
                  "Open Topo" = "OpenTopoMap"),
            selected="Stamen.TonerLite",
            width="95%"
          )
        )
      ),

      # Although this panel comes in further down the page than the data
      # selection panel, we call this one first so that it is "overtopped"
      # by an expanded data selection panel (until the selection panel is
      # shrunk again)
      absolutePanel(id = "map-filt-box", class = "panel panel-default",
        fixed = TRUE, draggable = TRUE, top = 60, left = "auto",
        right = 20, bottom = "auto", width = 300, height = "auto",

        box(title="Select & graph data",
          status="warning",
          solidHeader=FALSE,
          height=NULL,
          width=NULL,
          collapsible=TRUE,
          collapsed=TRUE,
          selectInput(
            inputId="cur_species",
            label="Show species",
            choices=c(
              "All species" = "All",
              "Bog turtle" = "botu",
              "Gopher tortoise" = "goto",
              "Greater Sage-Grouse" = "grsg",
              "Golden-winged Warbler" = "gwwa",
              "Lesser Prairie-Chicken" = "lpch",
              "New England cottontail" = "neco",
              "Southwestern Willow Flycatcher" = "swfl"),
            selected="all",
            width="95%"
          ),
          selectInput(
            inputId="cur_response",
            label="Output measure",
            choices=c("Total spending", "Acres treated",
                  "Dollars / acre treated", "# contracts"),
            selected="Total spending",
            width="95%"
          ),
          uiOutput("practice_choose"),
          selectInput(
            inputId="FY",
            label="Year",
            choices=years,
            selected="All",
            width="95%"
          ),
          hr(),

          h4("Graph the data"),
          selectInput(
            inputId="x_axis",
            label="X axis",
            choices=c("Fiscal Year" = "fy",
                  "State" = "STATE",
                  "County (state)" = "cnt_st",
                  "Work type (practice)" = "practice_name",
                  "Allocated Amt. ($)" = "Practice_Obligations"),
                  # "Make histogram (of Y)" = "hist"),
            width="95%"
          ),
          selectInput(
            inputId="y_axis",
            label="Y axis",
            choices=c(
              "Allocated amt. ($)" = "Practice_Obligations",
              "Total treated ac." = "total_treated_acres" #,
              # "# items / contract" = "number_contracts"
            ),
            width="95%"
          ),
          bsButton("big_chart",
            label="Create graph",
            style="primary",
            size="default"
          ),
          hr(),
          fluidRow(
            br(),
            column(6,
              bsButton(
                "get_started",
                label="Getting Started",
                style="success"
              )
            ),
            column(6,
              bsButton(
                "give_limits",
                label="Limitations",
                style="warning"
              )
            )
          )
        )
      )
    )
  )
}
