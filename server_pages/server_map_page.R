# Server-side code for the WLFW interactive map.
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


###########################################################################
# Server-side code for the section 7 app basic single-view page
###########################################################################
server_map_page <- function(input, output, selected, session) {
    #######################################################################
    # Get the list of available work practices for selector
    output$practice_choose <- renderUI({
        get_practs <- function(x) {
            if (x == "all") {
                as.character(levels(droplevels(full$practice_name)))
            } else {
                as.character(levels(droplevels(full[full$species == x, ]$practice_name)))
            }
        }

        pract_list <- switch(input$cur_species,
                          "All" = get_practs("all"),
                          "botu" = get_practs("Bog turtle"),
                          "goto" = get_practs("Gopher tortoise"),
                          "grsg" = get_practs("Greater Sage-Grouse"),
                          "gwwa" = get_practs("Golden-winged Warbler"),
                          "lpch" = get_practs("Lesser Prairie-Chicken"),
                          "neco" = get_practs("New England cottontail"),
                          "swfl" = get_practs("Southwestern Willow Flycatcher"))

        selectInput(
            inputId="cur_practice",
            label="Work type (practice)",
            choices=c("All", pract_list),
            width="95%")
    })
    
    #######################################################################
    # Some reactive values needed in several places for making the map 
    cur_zoom <- reactive({
        if (!is.null(input$map_zoom)) {
            input$map_zoom
        } else {
            4
        }
    })

    cur_poly <- reactive({
        switch(input$cur_species,
            "All" = allsp_shp,
            "botu" = BOTU_shp,
            "goto" = GOTO_shp,
            "grsg" = GRSG_shp,
            "gwwa" = GWWA_shp,
            "lpch" = LPCH_shp,
            "neco" = NECO_shp,
            "swfl" = SWFL_shp)
    })

    cur_ext <- reactive({
        switch(input$cur_species,
            "All" = allsp_ext,
            "botu" = BOTU_ext,
            "goto" = GOTO_ext,
            "grsg" = GRSG_ext,
            "gwwa" = GWWA_ext,
            "lpch" = LPCH_ext,
            "neco" = NECO_ext,
            "swfl" = SWFL_ext)
    })

    cur_bnds <- reactive({
        switch(input$cur_species,
            "all" = allsp_ext,
            "botu" = BOTU_ext,
            "goto" = GOTO_ext,
            "grsg" = GRSG_ext,
            "gwwa" = GWWA_ext,
            "lpch" = LPCH_ext,
            "neco" = NECO_ext,
            "swfl" = SWFL_ext)
    })

    cur_data <- reactive({
        make_alt_map_df(selected(), cur_poly(), input$cur_response)
    })

    cur_output_num <- reactive({
        switch(input$cur_response,
               "Total spending" = 19,
               "Acres treated" = 20,
               "Dollars / acre treated" = 21,
               "# contracts" = 22)
    })

    #######################################################################
    # Now to make the map
    output$map <- renderLeaflet({ 
		cur_map <- leaflet() %>%
                   setView(lng=allsp_ext$xmid, 
                           lat=allsp_ext$ymid, 
                           zoom = 4) %>%
                   mapOptions(zoomToLimits = "first")
        return(cur_map)
    })

    # proxy to add/change the basemap
    observe({ 
        leafletProxy("map") %>% 
            clearTiles() %>% 
            addProviderTiles(input$map_tile) 
    })

    # proxy to add/change polygons
    observe({ 
        popupFormat <- paste0("<strong>", cur_data()@data[[5]], " Co.</strong>", 
                              "<br>Expenditures: $", 
                              prettyNum(cur_data()@data[[19]], big.mark=","),
                              "<br>Acres treated: ", 
                              cur_data()@data[[20]],
                              "<br>Dollars / ac.: $", 
                              prettyNum(round(cur_data()@data[[21]], 0), 
                                        big.mark=","),
                              " / ac.",
                              "<br># Contracts: ", 
                              cur_data()@data[[22]]
        )

        leafletProxy("map", data=cur_data()) %>%
            clearShapes() %>%
            addPolygons(
                fillColor = ~colorNumeric(
                                palette="RdYlBu", 
                                domain=range(cur_data()@data[[cur_output_num()]], na.rm=T)
                            )(cur_data()@data[[cur_output_num()]]),
                fillOpacity = 0.6, 
                stroke = TRUE, 
                weight = 1.5, 
                color = "#ffffff", 
                popup = popupFormat
            )
    })

    # proxy to add/change the legend, conditioned on viz selection
    observe({
        make_legend_title <- function(sp, res) {
            lede <- "<p style='text-align:center;'>"
            paste(lede, res, "for<br>", sp, "</p>")
        }

        cur_title <- switch(input$cur_species,
            "All" = make_legend_title("all species", input$cur_response),
            "botu" = make_legend_title("bog turtle", input$cur_response),
            "goto" = make_legend_title("gopher tortoise", input$cur_response),
            "grsg" = make_legend_title("Greater Sage-Grouse", input$cur_response),
            "gwwa" = make_legend_title("Golden-winged Warbler", input$cur_response),
            "lpch" = make_legend_title("Lesser Prairie-Chicken", input$cur_response),
            "neco" = make_legend_title("New England cottontail", input$cur_response),
            "swfl" = make_legend_title("SW Willow Flycatcher", input$cur_response)
        )

        cur_prefix <- switch(input$cur_response,
            "Total spending" = "  $",
            "Acres treated" = "  ",
            "Dollars / acre" = "  $",
            "# contracts" = "  "
        )

        cur_suffix <- switch(input$cur_response,
            "Total spending" = "",
            "Acres treated" = " ac.",
            "Dollars / acre" = " / ac.",
            "# contracts" = " contracts"
        )

        range_vals <- range(cur_data()@data[[cur_output_num()]], na.rm=T)
        values <- cur_data()@data[[cur_output_num()]]
        leafletProxy("map", data=cur_data()) %>%
            clearControls() %>%
            addLegend("bottomleft",
                      pal=colorNumeric(palette="RdYlBu", 
                                       domain=range_vals),
                      values=~values,
                      title=cur_title,
                      labFormat=labelFormat(prefix=cur_prefix,
                                            suffix=cur_suffix),
                      opacity=0.8)
    })

    observe({
        leafletProxy("map", data=cur_data()) %>%
            fitBounds(cur_bnds()$xmin, cur_bnds()$ymin,
                      cur_bnds()$xmax, cur_bnds()$ymax)
    })

    output$large_chart <- renderGvis({
        x <- input$x_axis
        y <- input$y_axis
        if (x %in% c("fy", "STATE", "cnt_st", "practice_name")) {
            make_bargraph(selected(), x, y)
        } else if (x == "hist") {
            make_hist(selected(), y)
        } else {
            make_scatterp(selected(), x, y)
        }
    })

    # output$summary_data <- 

}

