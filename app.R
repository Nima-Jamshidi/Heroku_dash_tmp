## YOUR SOLUTION HERE

# author: Nima Jamshidi
# date: Aug 1st, 2022

"This script is the main file that creates a Dash app for Evo Idle Fleet.

Usage: app.R
"

library(dash)
# library(dashCoreComponents)
# library(dashHtmlComponents)
library(dashBootstrapComponents)

source('dash_functions.R')
source('dash_components.R')

app <- dash_app(
  suppress_callback_exceptions = T) %>% 
  add_stylesheet(list(
    dbcThemes$MORPH,
    "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
  )
)

div_header <- div(list(heading_title,
                           heading_subtitle))

div_sidebar <- div(
  list(
    h2(html$b(toupper('Nima Jamshidi'))),
    br(),
    html$h6(html$i(
      "M.Sc. in Resources, Environment and Sustainability, UBC"
    )),
    br(),
    a(
      id = "LinkedIn",
      className = "fa fa-linkedin fa-2x",
      href = "https://www.linkedin.com/in/nima-jamshidi-991711131/",
      style = list("text-decoration" = "none"),
      target="_blank",
      rel="noopener noreferrer"
    ),
    "  ",
    a(
      id = "GitHub",
      className = "fa fa-github fa-2x",
      href = "https://github.com/Nima-Jamshidi",
      style = list("text-decoration" = "none"),
      target="_blank",
      rel="noopener noreferrer"
    )#,
    # h2(id = "test","test")
  ),
  style = list(
    'background-color' = '#BBCFF1',
    'padding' = 10,
    'position' = 'sticky',
    'top' = 20,
    'bottom' = 50
  )
)

div_main <-
  list(dbcRow(list(
    dbcCol(div(map_text), width = 6),
    dbcCol(
      dbcCard(
        div(list(
          graph_map,
          dbcAlert(
            html$h5(
              html$b(
                "Check out Vancouver Map! Hover over the map to see neighbourhoods. Click on them to see their fleet density through out the week."
              )
            ),
            id = "alert-map1",
            is_open = T,
            dismissable = F,
            fade = F,
            color = "primary",
            style = list(top = 10)
          ),
          dbcAlert(
            id = "alert-map2",
            is_open = F,
            duration = 6000,
            style = list(top = 10)
          )
        )),
        className = "position-sticky right",
        style = list(
          top = 10,
          bottom = 10,
          `margin-top` = 10 ,
          `margin-bottom` = 10
        )
      ),
      ,
      width = 6
    )
  )),
  dbcRow(list(
    dbcCol(
      dbcCard(
        div(list(
          graph_tile,
          dbcCard(list(
            h4("Settings:", style = list("padding" = 5)),
            dbcCol(zscoreDropdown, width = 6)
          )),
          dbcAlert(
            html$h5(
              html$b(
                "This graph shows Hourly Aggregate Fleet Idle Time (HAFIT). Hover over the graph to see the values for different hours of a day of the week. The redder or bluer the block the higher or lower the density of idle cars at that hour."
              )
            ),
            id = "alert-tile1",
            is_open = T,
            dismissable = F,
            fade = F,
            color = "primary",
            style = list(top = 10)
          ),
          dbcAlert(
            id = "alert-tile2",
            is_open = F,
            duration = 6000,
            style = list(top = 10)
          )
        )),
        className = "position-sticky right",
        style = list(
          top = 10,
          bottom = 10,
          `margin-top` = 10 ,
          `margin-bottom` = 10
        )
      ),
      width = 6
    ),
    dbcCol(div(tile_text), width = 6)
  )),
  dbcRow(list(
    dbcCol(div(arv_tile_text), width = 6),
    dbcCol(
      dbcCard(
        div(list(
          dccLoading(graph_arv_tile, type = "circle"),
          dbcCard(list(
            h4("Settings:", style = list("padding" = 5)), dbcCol(slider, width = 12)
          )),
          dbcAlert(
            html$h5(
              html$b(
                "You can see the density of idle cars with an idle duration between 3 hrs and 6 hrs."
              )
            ),
            id = "alert-arv-tile1",
            is_open = T,
            dismissable = F,
            fade = F,
            color = "primary",
            style = list(top = 10)
          ),
          dbcAlert(
            id = "alert-arv-tile2",
            is_open = F,
            duration = 6000,
            style = list(top = 10)
          )
        )),
        className = "position-sticky right",
        style = list(
          top = 10,
          bottom = 10,
          `margin-bottom` = 10
        )
      ),
      width = 6
      )
    )
    )
  )

app %>% set_layout(dbcContainer(list(
  dbcRow(dbcCol(div_header), className = "navbar navbar-expand-lg navbar-light bg-light"),
  dbcRow(list(
    dbcCol(div_sidebar,
           width = 2),
    dbcCol(div_main, width = 10)
  ))
), fluid = T))


# app$callback(output = output(id = "fade", property = "is_in"),
#              params = list(input(id = "Nima-div", property = "n_clicks")),
#              function(n1) {
#                prevent_update(is.null(n1[[1]]))
#                return(F)
#              })

app %>% add_callback(output = list(
    output("alert-map1", "is_open"),
    output("alert-map2", "is_open"),
    output("alert-map2", "children")
  ),
  params = list(input("map-graph", "clickData")),
  function(clickdata) {
    prevent_update(is_null(clickdata$points[[1]]))
    return(list(F, T, html$h5(html$b(
      paste0(
        "Scroll down to see the fleet density for ",
        location_finder(as.integer(clickdata$points[[1]][[1]])),
        "."
      )
    ))))
  })

app$callback(
  output = list(output("alert-tile1", "is_open"),
                output("alert-tile2", "is_open"),
                output("alert-tile2", "children")),
  params = list(input("tile-graph", "clickData")),

  function(tile_clickdata){
    prevent_update(is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    return(list(F,T,html$h5(html$b(paste0("Scroll down to see the fleet density throughout the week for the cars that arrive at ",as.integer(tile_clickdata$points[[1]][3])," on ",c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday",
      "H/ Monday"
    )[as.integer(tile_clickdata$points[[1]][2])],"."
                                        )))))
  })

app$callback(output = list(
  output("alert-arv-tile1", "is_open"),
  output("alert-arv-tile2", "is_open"),
  output("alert-arv-tile1", "children"),
  output("alert-arv-tile2", "children")
  ),
  params = list(
    input(id = "arv_tile_slider", property = "value")
    ),
  function(period_list) {
    return(list(T, T, html$h5(html$b(
      paste0(
        "You can see the density of idle cars with an idle duration ",
        ifelse(
          period_list[[2]] == 600,
          paste0("more than ", periods$label[periods$value ==
                                               period_list[[1]]], "."),
          paste0("between ", periods$label[periods$value ==
                                             period_list[[1]]], " and ", periods$label[periods$value == period_list[[2]]], ".")
        )
      )
    )),
    html$h5(html$b(
      paste0("Scroll up to choose other neighbourhoods and times.")
    ))))
  })

app %>% add_callback(
  output = output(id = "map-graph", property = "clickData"),
  params = list(input(id = "Downtown Vancouver link", property = "n_clicks"),
                input(id = "False Creek link", property = "n_clicks"),
                input(id = "Grouse Mountain link", property = "n_clicks"),
                input(id = "DT 8-9AM  Mondays link", property = "n_clicks"),
                input(id = "YL 8-9AM  Mondays link", property = "n_clicks")),
  function(n1,n2,n3,n4,n5){
    ctx <- callback_context()
    prevent_update(is.null(ctx$triggered$prop_id))
    if (ctx$triggered$prop_id %in% c("Downtown Vancouver link.n_clicks","DT 8-9AM  Mondays link.n_clicks")){
      return(list(points = 68))
    } else if (ctx$triggered$prop_id == "False Creek link.n_clicks"){
      return(list(points = 72))
    } else if (ctx$triggered$prop_id == "Grouse Mountain link.n_clicks"){
      return(list(points = 77))
    } else if (ctx$triggered$prop_id == "YL 8-9AM  Mondays link.n_clicks"){
      return(list(points = 113))
    }
  }
)

app %>% add_callback(
  output = list(output(id = "tile-graph", property = "clickData"),
                output(id = "arv_tile_slider", property = "value")),
  params = list(input(id = "DT 8-9AM  Mondays link", property = "n_clicks"),
                input(id = "YL 8-9AM  Mondays link", property = "n_clicks")),
  function(n1,n2){
    ctx <- callback_context()
    prevent_update(is.null(ctx$triggered$prop_id))
    if (ctx$triggered$prop_id == "DT 8-9AM  Mondays link.n_clicks"){
      return(list(
        list(points = list(c(NA,'1','8'))),
        list(0,120)))
    } else if (ctx$triggered$prop_id == "YL 8-9AM  Mondays link.n_clicks"){
      return(list(
        list(points = list(c(NA,'1','8'))),
        list(0,120)))
    }
  }
)

app %>% add_callback(
  output = output(id = "tile-graph", property = "figure"),
  params = list(input(id = 'map-graph',property = 'clickData'),
                input(id = "zscore-type", property = "value")),
  function(clickdata,zscore){
    prevent_update(is_null(clickdata$points[[1]]))
      make_tile_graph(curve_number = as.integer(clickdata$points[[1]][[1]]), zscore_type = zscore)
  }
)

app %>% add_callback(
  output = output(id = "arv-tile-graph", property = "figure"),
  params=list(input(id = 'map-graph', property='clickData'),
              input(id = 'tile-graph', property='clickData'),
              input(id = 'zscore-type', property='value'),
              input(id = "arv_tile_slider", property = "value")),
  function(map_clickdata,tile_clickdata,zscore,period_list) {
    prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
      make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]][[1]]),
                                       zscore_type = zscore,
                                       Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
                                       Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
                                       Period_cat = period_list)
  }
)

# app %>% add_callback(
#     output=output(id = 'test', property='children'),
#     params=list(input(id = 'tile-graph', property='clickData')),
#     function(data){
#       paste0(data$points[[1]][3])
#     })
# 
# 
# app %>% add_callback(
#   output=output(id = 'test2', property='children'),
#   params = list(input(id = "zscore-type", property = "value")),
#   function(data){
#     paste0((data))
#   })

app %>% add_callback(
  output = output("offcanvas-scrollable", "is_open"),
  params = list(input("open-offcanvas-scrollable", "n_clicks"),
                state("offcanvas-scrollable", "is_open")),
  function(n1, is_open){
    if (n1>0){
      return(!is_open)
    }
    return(is_open)
  }
)

app %>% add_callback(
  output = output("offcanvas-scrollable2", "is_open"),
  params = list(input("open-offcanvas-scrollable2", "n_clicks"),
                state("offcanvas-scrollable2", "is_open")),
  function(n1, is_open){
    if (n1>0){
      return(!is_open)
    }
    return(is_open)
  }
)

# app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))

# app$run_server(debug=TRUE)
app %>% run_app(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))
# app %>% run_app(
#   host = Sys.getenv("DASH_HOST", Sys.getenv("HOST", "0.0.0.0")),
#   port = as.numeric(Sys.getenv("DASH_PORT", Sys.getenv("PORT", 8050)))
# )
# app %>% run_app()
# app

# command to add dash app in Rstudio viewer:
# rstudioapi::viewer("http://127.0.0.1:8050")