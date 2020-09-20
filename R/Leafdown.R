#' Leafdown R6 Class
#'
#' @description
#' This class acts as a wrapper around the leaflet map. A leaflet map allows the user of the shiny app to:
#' \itemize{
#'   \item select shapes / regions
#'   \item drill down on these selected shapes
#'   \item drill up
#' }
#'
#' The active bindings of the class provide convenient interfaces between the leafdown map
#' and other elements in the shiny app e.g. graphs, tables etc.
#' A leafdown object can \strong{only} be used in a shiny app.
#'
#' @importFrom magrittr "%>%"
#' @import leaflet
#' @export
Leafdown <- R6::R6Class("Leafdown",
  private = list(
    #' @field spdfs_list The spdfs of all map levels. This is set in \code{initialize} and cannot be changed afterwards.
    #' At the moment only two map levels are possible.
    .spdfs_list = NULL,
    # map_proxy The proxy from the leaflet map. Used for smoother redrawing.
    .map_proxy = NULL,
    #' @field map_output_id The id from the shiny ui used in the \code{leafletOutput("<<id>>")}. Used to observe for _shape_click events.
    .map_output_id = NULL,
    #' @field curr_data The metadata and (if available) the corresponding values of all currently displayed shapes.
    .curr_data = NULL,
    #' @field curr_sel_data A \code{reactiveValue} containing a data.frame with
    #' the metadata and (if available) the corresponding values of all currently selected shapes.
    .curr_sel_data = NULL,
    #' @field curr_map_level Index of the current map level.
    #' This corresponds to the position of the shapes in the \code{spdfs_list}.
    #' (i.e The highest-level is 1, the next is 2 and so on...).
    #' At the moment only two map levels are possible.
    .curr_map_level = NULL,
    #' curr_sel_ids The ids of the selected shapes of the current level. They will be highlighted on the map.
    #' Calling \code{drill_down}, the drill down functionality is executed for these shapes.
    .curr_sel_ids = NULL,
    #' @field curr_spdf The spdfs of the current map level.
    .curr_spdf = NULL,
    #' curr_poly_ids The ids of all polygons of the current map level.
    .curr_poly_ids = NULL,
    #' selected_parents The selected spdf shapes from the higher level. (Subset of spdfs_list)
    .selected_parents = NULL,
    #' unselected_parents All spdf shapes from the higher level which are not selected. They will be drawn in gray.
    #' (Subset of spdfs_list)
    .unselected_parents = NULL,

    #' @description
    #' Initializes the observer for the maps _shape_click events. This is needed for the shape selection.
    #' Once a shape is clicked, it is added to (or removed from) \code{.curr_sel_ids}.
    #' The outline of selected shapes is highlighted via the showGroup (hideGroup) functions.
    init_click_observer = function(input, map_output_id) {
      shiny::observeEvent(input[[paste0(map_output_id, "_shape_click")]], {
        clicked_id <- input[[paste0(map_output_id, "_shape_click")]]$id
        req(clicked_id)
        self$toggle_shape_select(clicked_id)
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
    curr_sel_data = function(value) {
      if (missing(value)) {
        private$.curr_sel_data
      } else {
        stop("`$curr_sel_data` is read only", call. = FALSE)
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
    #' This will not draw the map. First add data and then call \code{draw_leafdown} to draw the map.
    #' @param spdfs_list The spdfs of all map levels. This cannot be changed later
    #' @param map_output_id The id from the shiny-ui used in the \code{leafletOutput("<<id>>")}. Used to observe for _shape_click events.
    #' @param input The \code{input} from the shiny app
    initialize = function(spdfs_list, map_output_id, input) {
      if(!is.list(spdfs_list)) {
        stop("The given spdfs_list must be a list")
      }
      if(length(spdfs_list) > 2) {
        stop("Leafdown currently supports only two map levels. The given spdf_list can therefore only contain two elements.")
      }
      for(i in length(spdfs_list)) {
        # Check whether the given spdf_element is an s4 class of type SpatialPolygonsDataFrame.
        is_valid <- check_s4_spdf(spdfs_list[[i]])
        if(!is_valid) {
          stop("The given spdfs_list must contain s4 classes of type SpatialPolygonsDataFrame")
        }
      }
      # check map_output_id
      checkmate::assert_character(map_output_id, min.chars = 1)

      # check 'input' reactive vals from shiny
      if(!shiny::is.reactivevalues(input)) {
        stop("The given 'input' argument must be the 'input' from the shiny app")
      }

      private$.curr_map_level <- 1
      private$.curr_sel_ids <- list(c())
      private$.selected_parents <- c()
      private$.curr_sel_data <- reactiveVal(data.frame())

      private$.spdfs_list <- spdfs_list
      private$.map_output_id <- map_output_id
      private$.curr_spdf <- private$.spdfs_list[[private$.curr_map_level]]

      private$init_click_observer(input, map_output_id)
    },
    #' @description
    #' Draws the leaflet map on the current map level. All unselected parents will be drawn in gray.
    #' @param ... Additional arguments given to \code{leaflet::addPolygons}
    draw_leafdown = function(...) {
      # Checks arguments in ellipsis for undesired inputs such as 'layerId' which may
      # collide with internal structure of leafdown and returns a "cleaned" version of
      # the arguments by removing or redefining problematic inputs.
      # e.g. 'layerId' is removed from arg_list when set
      arg_list <- check_draw_ellipsis(...)
      curr_spdf <- private$.curr_spdf
      curr_spdf@data <- private$.curr_data
      # Using proxy to avoid redrawing of map when highlighting
      private$.map_proxy <- leafletProxy(private$.map_output_id)
      # ids of all polygons in the current spdf
      all_poly_ids <- c()
      for (pol in curr_spdf@polygons) {
        all_poly_ids <- c(all_poly_ids, pol@ID)
      }
      private$.curr_poly_ids <- all_poly_ids
      map <- leaflet(curr_spdf)
      arg_list[["map"]] <- map
      arg_list[["layerId"]] <- ~all_poly_ids
      arg_list[["highlight"]][["bringToFront"]] <- FALSE
      # Add polygons (with "cleaned" version of the arguments) and polylines to basic map
      map <- do.call(addPolygons, arg_list)
      map <- addPolylines(
        map = map,
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
      # On drill up highlight the polylines which were selected before drill down
      private$.map_proxy %>%
        hideGroup(all_poly_ids) %>%
        showGroup(private$.curr_sel_ids[[private$.curr_map_level]])
      map
    },
    #' @description
    #' Returns the metadata of the shapes from the current maplevel.
    #' This may differ from what is displayed if \code{drill_down} has been called but \code{draw_leafdown}
    #' has not been (yet).
    #'
    #' @return The current meta-data
    get_current_metadata = function() {
      private$.curr_spdf@data
    },
    #' @description
    #' Adds the data to the currently displayed shapes.
    #' This includes the meta-data AND the values to be visualized in the map.
    #' @param data The new data existing of the meta-data and the values to display in the map(color)
    add_data = function(data) {
      # check if the given data contains the correct metadata:
      # - The metadata has to be the same as the old metadata
      # - Optionally value column(s) can be added
      if(!is.data.frame(data)) {
        stop("The given data must be a data.frame")
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
      if(private$.curr_map_level == length(private$.spdfs_list)) {
        shinyjs::alert("The lowest level is reached. Cannot drill lower!")
        req(FALSE)
      }
      # check for selection (we can only drill_down if there are shapes selected)
      if(length(private$.curr_sel_ids[[private$.curr_map_level]]) < 1) {
        shinyjs::alert("Please select the area to drill down!")
        req(FALSE)
      }

      # Information about parent polygons
      parents <- private$.spdfs_list[[private$.curr_map_level]]
      all_poly_ids_parents <- private$.curr_poly_ids
      curr_sel_parents <- private$.curr_sel_ids[[private$.curr_map_level]]
      index_sel_parents <- all_poly_ids_parents %in% curr_sel_parents
      private$.selected_parents <- parents[index_sel_parents, ]
      private$.unselected_parents <- parents[!index_sel_parents, ]

      # spdf_new contains the child polygons of the selected parents
      spdf_new <- private$.spdfs_list[[private$.curr_map_level + 1]]
      spdf_new <- spdf_new[spdf_new@data$GID_1 %in% private$.selected_parents@data$GID_1, ]

      # Update leafdown object
      private$.curr_spdf <- spdf_new
      private$.curr_map_level <- private$.curr_map_level + 1
      private$.curr_sel_ids[[private$.curr_map_level]] <- character(0)
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
    },
    #' @description
    #' Selects the shape with the given shape id, or unselects it if it was already selected.
    #' @param shape_id the id of the shape to select, has to be a character and in the current map-level.
    toggle_shape_select = function(shape_id) {
      checkmate::assert_character(shape_id, min.chars = 1)
      if(!shape_id %in% private$.curr_poly_ids) {
        stop("Please make sure the selected shape_id is in the current level")
      }

      # Ids of selected polygons before click
      curr_sel_ids <- private$.curr_sel_ids[[private$.curr_map_level]]
      if (shape_id %in% curr_sel_ids) {
        private$.map_proxy %>% hideGroup(shape_id)
        # Remove id of unselected polygon
        curr_sel_ids <- curr_sel_ids[!curr_sel_ids == shape_id]
      } else {
        private$.map_proxy %>% showGroup(shape_id)
        # Add id of newly selected polygon
        curr_sel_ids <- c(curr_sel_ids, shape_id)
      }

      # Update data with regards to currently selected polygons (after click)
      is_selected <- private$.curr_poly_ids %in% curr_sel_ids
      curr_sel_data <- subset(private$.curr_data, is_selected)
      # Update leafdown object
      private$.curr_sel_ids[[private$.curr_map_level]] <- curr_sel_ids
      private$.curr_sel_data(curr_sel_data) # update reactiveVal
    }
  )
)
