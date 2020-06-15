#' Leafdown R6 Class
#'
#' @description
#' This class acts as a wrapper around the leaflet map. It allows the user to:
#' - select shapes
#' - drill down on these selected shapes
#' - drill up
#' - convenient functions to connect the map to graphs
#'
#' @importFrom magrittr "%>%"
#' @export
Leafdown <- R6::R6Class("Leafdown",
  private = list(
    #' @field spdfs_list The spdfs of all map levels. This is set in \code{initialize} and cannot be changed afterwards.
    .spdfs_list = NULL,
    # map_proxy The proxy from the leaflet map. Used for smoother redrawing.
    .map_proxy = NULL,
    #' @field map_output_id The id from the shiny-ui used in the \code{leafletOutput("<<id>>")}. Used to observe for _shape_click events.
    .map_output_id = NULL,

    #' @field curr_data The spdf-metadata from the currently used shapes AND the corresponding values.
    .curr_data = NULL,
    #' @field curr_map_level The current map level.
    #' This corresponds to the position of the shapes in the \code{spdfs_list}.
    #' (i.e The highest-level is 1, the next is 2 and so on...)
    .curr_map_level = NULL,
    #' @field curr_selection The selected shapes of the current level. They will be highlighted on the map.
    #' Calling \code{drill_down} will drill down on these selected shapes.
    .curr_selection = NULL,
    #' @field curr_spdf The spdfs of the current map level.
    .curr_spdf = NULL,
    #' curr_poly_ids The ids of all polygons of the current map level.
    .curr_poly_ids = NULL,

    # selected_parents The selected shapes from the higher level.
    .selected_parents = NULL,
    # unselected_parents All shapes from the higher level which are not selected. They will be drawn in gray.
    .unselected_parents = NULL,

    #' @description
    #' Initializes the observer for the maps _shape_click events. This is needed for the selection.
    #' Once a shape is clicked, it is added to the \code{.curr_selection} (or removed from it).
    #' The outline of selected shapes is highlighted via the showGroup (hideGroup) functions.
    init_click_observer = function(input, map_output_id) {
      shiny::observeEvent(input[[paste0(map_output_id, "_shape_click")]], {
        clicked_id <- input[[paste0(map_output_id, "_shape_click")]]$id
        req(clicked_id)
        curr_selection <- private$.curr_selection[[private$.curr_map_level]]
        if (clicked_id %in% curr_selection) {
          private$.map_proxy %>% hideGroup(clicked_id)
          private$.curr_selection[[private$.curr_map_level]] <- curr_selection[!curr_selection == clicked_id]
        } else {
          private$.map_proxy %>% showGroup(clicked_id)
          private$.curr_selection[[private$.curr_map_level]] <- c(curr_selection, clicked_id)
        }
      })
    }
  ),
  active = list(
    spdfs_list = function(value) {
      if (missing(value)) {
        private$.spdfs_list
      } else {
        stop("`$spdfs_list` is read only", call. = FALSE)
      }
    },
    curr_selection = function(value) {
      if (missing(value)) {
        private$.curr_selection[[private$.curr_map_level]]
      } else {
        stop("`$.curr_selection` is read only", call. = FALSE)
      }
    },
    map_output_id = function(value) {
      if (missing(value)) {
        private$.map_output_id
      } else {
        stop("`$map_output_id` is read only", call. = FALSE)
      }
    },
    curr_data = function(value) {
      if (missing(value)) {
        private$.curr_data
      } else {
        stop("`$.curr_data` is read only", call. = FALSE)
      }
    },
    curr_map_level = function(value) {
      if (missing(value)) {
        private$.curr_map_level
      } else {
        stop("`$curr_map_level` is read only", call. = FALSE)
      }
    },
    curr_spdf = function(value) {
      if (missing(value)) {
        private$.curr_spdf
      } else {
        stop("`$curr_spdf` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Initializes the leafdown object.
    #' This will not draw the map. Add data first and call \code{draw_leafdown}}to draw the map.
    #' @param spdfs_list The spdfs of all map levels. This cannot be changed later
    #' @param map_output_id The id from the shiny-ui used in the \code{leafletOutput("<<id>>")}. Used to observe for _shape_click events.
    #' @param input The \code{input} from the shiny app
    initialize = function(spdfs_list, map_output_id, input) {
      # TODO: check spdfs_list

      # check map_output_id
      checkmate::assert_character(map_output_id, min.chars = 1)

      # check 'input' reactive vals from shiny
      if(!shiny::is.reactivevalues(input)) {
        stop("The given 'input' argument must be the 'input' from the shiny app")
      }

      private$.curr_map_level <- 1
      private$.curr_selection <- list(c())
      private$.selected_parents <- c()

      private$.spdfs_list <- spdfs_list
      private$.map_output_id <- map_output_id
      private$.curr_spdf <- private$.spdfs_list[[private$.curr_map_level]]

      private$init_click_observer(input, map_output_id)
    },
    #' @description
    #' Draws the leaflet map on the current map level. All unselected parents will be drawn in gray.
    #' @param ... Additional arguments given to \code{leaflet::addPolygons}
    draw_leafdown = function(...) {
      curr_spdf <- private$.curr_spdf
      curr_spdf@data <- private$.curr_data
      private$.map_proxy <- leaflet::leafletProxy(private$.map_output_id)
      all_poly_ids <- c()
      for (pol in curr_spdf@polygons) {
        all_poly_ids <- c(all_poly_ids, pol@ID)
      }
      private$.curr_poly_ids <- all_poly_ids
      map <- leaflet::leaflet(curr_spdf) %>%
        leaflet::addPolygons(layerId = ~all_poly_ids, ...) %>%
        addPolylines(
          group = all_poly_ids, stroke = TRUE, weight = 4, color = "#FFCC00",
          highlight = highlightOptions(bringToFront = T, weight = 4)
        )
      if (private$.curr_map_level != 1) {
        # If there are unselected parent polygons then draw them as gray background
        if (length(private$.unselected_parents@polygons) > 0) {
          map <- map %>%
            addPolylines(
              data = private$.unselected_parents,
              stroke = F, weight = 2, color="#929292",
              highlight = highlightOptions(bringToFront = T)
            ) %>%
            addPolygons(
              data = private$.unselected_parents,
              fillOpacity = 0.4,
              color = "#A4A4A5",
              weight = 1
            )
        }
      }
      private$.map_proxy %>%
        hideGroup(all_poly_ids) %>%
        showGroup(private$.curr_selection[[private$.curr_map_level]])
      map
    },
    #' @description
    #' Returns the metadata of the shapes from the current maplevel.
    #' This may differ from what is displayed if \code{drill_down} was called but \code{draw_leafdown} wasn't (yet)
    #'
    #' @return The current meta-data
    get_current_metadata = function() {
      private$.curr_spdf@data
    },
    #' @description
    #' Sets the data of the current shapes. This includes the meta-data AND the values given by the user.
    #' These values can be used to draw differently colored shapes.
    #' @param data The new data existing of the meta-data and the values to display in the map(color)
    add_data = function(data) {
      # check if the given data contains the correct metadata:
      # - The metadata has to be the same as the old metadata
      # - Optionally value column(s) can be added
      if(!is.list(data)) {
        stop("The given data must be a list")
      }
      if(!all(names(private$.curr_spdf@data) %in% names(data))) {
        stop("You cannot remove columns from the existing meta-data. Only add to it")
      }
      if(!isTRUE(all.equal(data[, names(private$.curr_spdf@data)], private$.curr_spdf@data, check.attributes = FALSE))) {
        stop("You cannot change the existing meta-data. Only add to it")
      }

      private$.curr_data <- data
    },
    #' @description
    #' Drills down to the lower level if:
    #' - there is a lower level (for now there are only two levels)
    #' - at least one shape is selected to drill down on
    #' !This will not redraw the map! Also call \code{add_data} to add data for the new level and then
    #'   \code{draw_leafdown} to redraw the map on the new level
    drill_down = function() {
      # check whether we can drill_down further (just 2 levels for now)
      if(private$.curr_map_level == 2) {
        shinyjs::alert("The lowest level is reached. Cannot drill lower!")
        req(FALSE)
      }
      # check for selection (we can only drill_down if there are shapes selected)
      if(is.null(private$.curr_selection[[private$.curr_map_level]])) {
        shinyjs::alert("Please select the area to drill down!")
        req(FALSE)
      }

      # Information about parent polygons
      parents <- private$.spdfs_list[[private$.curr_map_level]]
      all_poly_ids_parents <- private$.curr_poly_ids
      curr_selection_parents <- private$.curr_selection[[private$.curr_map_level]]
      index_sel_parents <- all_poly_ids_parents %in% curr_selection_parents
      private$.selected_parents <- parents[index_sel_parents, ]
      private$.unselected_parents <- parents[!index_sel_parents, ]

      # spdf_new contains the child polygons of the selected parents
      spdf_new <- private$.spdfs_list[[private$.curr_map_level + 1]]
      spdf_new <- spdf_new[spdf_new@data$GID_1 %in% private$.selected_parents@data$GID_1, ]

      # Update leafdown object
      private$.curr_spdf <- spdf_new
      private$.curr_map_level <- private$.curr_map_level + 1
      private$.curr_selection[[private$.curr_map_level]] <- character(0)
    },
    #' @description
    #' Drills up to the higher level if:
    #' - there is a higher level (for now there are only two levels)
    #' !This will not redraw the map! Also call \code{add_data} to add data for the new level and then
    #'   \code{draw_leafdown} to redraw the map on the new level
    drill_up = function() {
      # check whether we can drill_up further
      if(private$.curr_map_level <= 1) {
        shinyjs::alert("The highest level is reached. Cannot drill higher!")
        req(FALSE)
      }
      # Update leafdown object
      private$.curr_spdf <- private$.spdfs_list[[private$.curr_map_level - 1]]
      private$.curr_map_level <- private$.curr_map_level - 1
      private$.unselected_parents <- NULL
    }
  )
)
