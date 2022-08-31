## YOUR SOLUTION HERE

# author: YOUR NAME
# date: THE DATE

"This script is the main file that creates a Dash app for cm110 on the gapminder dataset.

Usage: app.R
"

## Load libraries
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(dashBootstrapComponents)

## Important! Source the files (order matters!)
source('dash_functions.R')
source('dash_components.R')

## Create Dash instance

app <- Dash$new(suppress_callback_exceptions = T,
                external_stylesheets = dbcThemes$COSMO)

## Specify layout elements

div_header <- htmlDiv(
  list(heading_title,
       heading_subtitle
  ), #### THIS IS NEW! Styles added
  style = list(
    backgroundColor = '#337DFF',
    textAlign = 'center',
    color = 'white',
    margin = 5,
    marginTop = 0
  )
)


div_sidebar <- htmlDiv(
  list(htmlLabel('Select y-axis metric:'),
       htmlBr(),
       # zscoreDropdown,
       htmlLabel('Select y scale : '),
       htmlBr(),
       logbutton,
       sources,
       htmlH2(as.character("Hello"),id = "test"
              # ,className = "output-example-loading"
              ),
       htmlH2(as.character("Hello"),id = "test2"),
       offcanvas
       # dccLoading(loading_state = list(is_loading = T))
  ), #### THIS IS NEW! Styles added
  style = list('background-color' = '#BBCFF1',
               'padding' = 10,
               # 'flex-basis' = '20%'#,
               # 'position' = 'fixed',
               'top' = 20,
               # 'left' = 0,
               'bottom' = 50
               # 'width' = '16rem',
               # 'padding' = '2rem 1rem'
  )#,
  # className = "sidebar"
  # className = "position-sticky left"
  # className = "position-absolute left"
)

div_main <- #htmlDiv(
  list(
    dbcRow(
      list(
        dbcCol(
          htmlDiv(map_text),width = 6),
        dbcCol(
          dbcCard(graph_map,className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-top` = 10 ,`margin-bottom` = 10)),width = 6)
        )
      ),
    dbcRow(
      list(
        dbcCol(
          dbcCard(htmlDiv(list(graph_tile,dbcCard(list(htmlH4("Settings:"),dbcCol(zscoreDropdown,width = 6))))),className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-top` = 10 ,`margin-bottom` = 10)),width = 6),
        dbcCol(
          htmlDiv(tile_text),width = 6)
        # dbcCol(
        #   htmlDiv(zscoreDropdown),width = 2)
      )
    ),
    dbcRow(
      list(
        dbcCol(
          htmlDiv(text_template),width = 6),
        dbcCol(
          dbcCard(htmlDiv(list(dccLoading(graph_arv_tile,type = "circle"),dbcCard(list(htmlH4("Settings:"),dbcCol(slider,width = 12))))),className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-bottom` = 10)),width = 6)
        # dbcCol(
        #   htmlDiv(slider,className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-bottom` = 10)),width = 2)
      )
    )
    # dbcRow(
    #   list(
    #     dbcCol(
    #       htmlDiv(text_template),width = 3),
    #     dbcCol(htmlDiv(
    #       list(dbcCard(graph_tile))),width = 7
    #     )
    #     )
    #   ),
    # dbcRow(
    #   list(
    #     dbcCol(
    #       htmlDiv("text_template"),width = 3),
    #     dbcCol(
    #       dbcCard(
    #         htmlDiv(
    #               list(htmlLabel('Select y-axis metric:'),
    #                    htmlBr(),
    #                    htmlLabel('Select y-axis metric:'),
    #                    htmlBr(),
    #                    htmlLabel('Select y-axis metric:'),
    #                    htmlBr(),
    #                    htmlLabel('Select y-axis metric:'),
    #                    htmlBr(),
    #                    htmlLabel('Select y-axis metric:'),
    #                    htmlBr()),
    #               style = list('background-color' = '#BBCFF1',
    #                            'padding' = 10,
    #                            # 'flex-basis' = '20%'#,
    #                            # 'position' = 'fixed',
    #                            'top' = 20,
    #                            # 'left' = 0,
    #                            'bottom' = 50
    #                            # 'width' = '16rem',
    #                            # 'padding' = '2rem 1rem'
    #               ),
    #               # className = "sidebar"
    #               className = "position-sticky left"
    #               )
    #       ),width = 5),
    #     dbcCol(
    #       htmlDiv(slider,className = "position-sticky right",style = list(top = 0)),width = 2)
    #       # slider,width = 2)
    #     )
    #   ),
    # dbcRow(
    #   list(
    #     dbcCol(
    #       htmlDiv(text_template),width = 3),
    #     dbcCol(
    #       dbcCard(graph_tile),width = 5),
    #     dbcCol(
    #       htmlDiv("Test Test Test"),width = 2)
    #   )
    # )
    )
    # graph,
    # htmlDiv(id = "tile-wrapper"),
    # htmlDiv(id = "loader-wrapper", className="loader-wrapper",children = dccLoading(id="loading",children = htmlDiv(slider,id = "arv-tile-wrapper",style = list(visibility = "visible")),type = "circle"),style = list("margin-top" = "15px"))
    # htmlDiv(id = "loader-wrapper", className="loader-wrapper",children = dccLoading(id="loading",children = htmlDiv(id = "arv-tile-wrapper"),type = "circle"),style = list("margin-top" = "15px"))   
    
    # ,htmlDiv(list(
    #   htmlDiv('Convert Temperature'),
    #   'Celsius',
    #   dccInput(
    #     id="celsius",
    #     value=0.0,
    #     type="number"
    #   ),
    #   ' = Fahrenheit',
    #   dccInput(
    #     id="fahrenheit",
    #     value=32.0,
    #     type="number",
    #   )
    # ))
  # )
  # style = list('flex-basis' = '80%')




## Specify App layout

# app %>% set_layout(
# 	div_header,
# 	htmlDiv(
# 		list(
# 			div_sidebar,
# 			div_main
# 		), #### THIS IS NEW! Styles added
# 			style = list('display' = 'flex',
# 								 'justify-content'='center')
# 	)
# )

app %>% set_layout(dbcContainer(list(
  dbcRow(div_header),
  dbcRow(
    list(dbcCol(div_sidebar,
                width = 2),
         dbcCol(div_main, width = 10))
    # dbcCol(dbcRow(list(dbcCol(div_main,width = 7),dbcCol(htmlDiv("Test Test Test"),width = 3))),
    #        width = 10)),
    #### THIS IS NEW! Styles added
    # style = list('display' = 'flex',
    #              'justify-content' = 'center')
  )
  # ,
  # dbcRow(
  #   list(dbcCol(htmlDiv(
  #     list(htmlLabel('Select y-axis metric:'),
  #          htmlBr()),
  #     style = list('background-color' = '#BBCFF1',
  #                  'padding' = 10,
  #                  # 'flex-basis' = '20%'#,
  #                  # 'position' = 'fixed',
  #                  'top' = 20,
  #                  # 'left' = 0,
  #                  'bottom' = 50
  #                  # 'width' = '16rem',
  #                  # 'padding' = '2rem 1rem'
  #     ),
  #     # className = "sidebar"
  #     className = "position-sticky left"
  #     ),
  #     width = 2
  # ),
  # dbcCol(htmlDiv(list(htmlDiv(id = "tile-wrapper"),
  #                     htmlDiv(id = "loader-wrapper", className="loader-wrapper",children = dccLoading(id="loading",children = htmlDiv(id = "arv-tile-wrapper"),type = "circle"),style = list("margin-top" = "15px"))   )),
  #        width = 10)
)))

## App Callbacks


# 
# 
app$callback(
  output = output(id = "map-graph", property = "clickData"),
  # output=output(id = 'test2', property='children'),
  params = list(input(id = "Downtown Vancouver link", property = "n_clicks"),
                input(id = "False Creek link", property = "n_clicks")),
  function(n1,n2){
    # Sys.sleep(1)
    ctx <- callback_context()
    prevent_update(is.null(ctx$triggered$prop_id))
    # paste0(ctx$triggered$prop_id)
    if (ctx$triggered$prop_id == "Downtown Vancouver link.n_clicks"){
      return(list(points = 68))
    } else if (ctx$triggered$prop_id == "False Creek link.n_clicks"){
      return(list(points = 72))
    }
  }
)


app$callback(
  output = output(id = "tile-graph", property = "figure"),
  params = list(input(id = 'map-graph',property = 'clickData'),
                input(id = "zscore-type", property = "value")),
  function(clickdata,zscore){
    prevent_update(is_null(clickdata$points[[1]]))
    # if (is_null(clickdata$points[[1]])){
    #   make_tile_graph(zscore_type = zscore)
    # } else {
      # graph = dccGraph(
      # id = 'tile-graph',
      make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore)
      # style = list(visibility = "visible"))
      # return(dbcRow(list(dbcCol(dbcCard(graph),width = 7),dbcCol(htmlDiv("Test Test Test"),width = 3))))
    # }
  }
)


# app$callback(
#   output = output(id = "tile-graph", property = "figure"),
#   params = list(input(id = 'map-graph',property = 'clickData'),
#                 input(id = "zscore-type", property = "value")),
#   function(clickdata,zscore){
#     # prevent_update(is_null(clickdata$points[[1]]))
#     if (is_null(clickdata$points[[1]])){
#       make_tile_graph(zscore_type = zscore)
#     } else {
#     # graph = dccGraph(
#       # id = 'tile-graph',
#       make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore)
#       # style = list(visibility = "visible"))
#     # return(dbcRow(list(dbcCol(dbcCard(graph),width = 7),dbcCol(htmlDiv("Test Test Test"),width = 3))))
#     }
#   }
# )


app$callback(
  output = output(id = "arv-tile-graph", property = "figure"),
  params=list(input(id = 'map-graph', property='clickData'),
              input(id = 'tile-graph', property='clickData'),
              input(id = 'zscore-type', property='value'),
              input(id = "arv_tile_slider", property = "value")),
  function(map_clickdata,tile_clickdata,zscore,period_list) {
    prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    # graph = dccGraph(
    #   id = 'arv_tile_graph',
      make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
                                       zscore_type = zscore,
                                       Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
                                       Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
                                       Period_cat = period_list)
      # style = list(visibility = "visible")
    # )
    # return(
    #   graph
    # )
  }
)

#####



# app$callback(
#   output = output(id = "map-graph", property = "clickData"),
#   params = list(input(id = "False Creek link", property = "n_clicks")),
#   function(n1){
#     prevent_update(is.null(n1[[1]]))
#     # if(n1>0){
#     return(list(points = 72))
#     # }
#   }
# )
# app$callback(
#   output = output(id = "zscore-type", property = "value"),
#   params = list(input(id = "daily normalized link", property = "n_clicks")),
#   function(n1){
#     prevent_update(is.null(n1[[1]]))
#     # if(n1>0){
#     return("daily")
#     # }
#   }


# app$callback(
#   output = output(id = "zscore-type", property = "value"),
#   params = list(input(id = "daily normalized link", property = "n_clicks")),
#   function(n1){
#     prevent_update(is.null(n1[[1]]))
#     # if (n1>1){
#     return("daily")
#     # }
#   }
# )
# # 




# app$callback(
#   output = output(id = "tile-wrapper", property = "children"),
#   params = list(input(id = 'map-graph',property = 'clickData'),
#                 input(id = "zscore-type", property = "value")),
#   function(clickdata,zscore){
#     graph = dccGraph(
#       id = 'tile-graph',
#       figure=make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),
#       style = list(visibility = "visible"))
#     return(dbcRow(list(dbcCol(dbcCard(graph),width = 7),dbcCol(htmlDiv("Test Test Test"),width = 3))))
#   }
# )
# # 
# 
# 
# 
# app$callback(
#   output = output(id = "arv-tile-wrapper", property = "children"),
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'tile-graph', property='clickData'),
#               input(id = 'zscore-type', property='value')
#               # input(id = "arv_tile_slider", property = "value")
#   ),
#   function(map_clickdata,tile_clickdata,zscore) {
#     # prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     # graph = dccGraph(
#     #   id = 'arv_tile_graph',
#     #   figure = make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
#     #                                    zscore_type = zscore,
#     #                                    Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
#     #                                    Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
#     #                                    Period_cat = period_list),
#     #   style = list(visibility = "visible")
#     # )
#     return(
#       dbcRow(list(dbcCol(dbcCard(id = "arv-tile-graph"),width = 7),dbcCol(slider,width = 3)))
#     )
#   }
# )
# 
# app$callback(
#   output = output(id = "arv-tile-graph", property = "children"),
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'tile-graph', property='clickData'),
#               input(id = 'zscore-type', property='value'),
#               input(id = "arv_tile_slider", property = "value")),
#   function(map_clickdata,tile_clickdata,zscore,period_list) {
#     # prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     graph = dccGraph(
#       id = 'arv_tile_graph',
#       figure = make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
#                                        zscore_type = zscore,
#                                        Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
#                                        Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
#                                        Period_cat = period_list),
#       style = list(visibility = "visible")
#     )
#     return(
#       graph
#     )
#   }
# )
# 
# 

app$callback(
    output=output(id = 'test', property='children'),
    params=list(input(id = 'map-graph', property='clickData')),
    function(data){
      paste0(data$points[[1]])
    })


app$callback(
  output=output(id = 'test2', property='children'),
  # params=list(input(id = 'zscore-type', property='value')),
  params = list(input(id = "zscore-type", property = "value")),
  function(data){
    paste0((data))
  })

# app$callback(
#   output = output(id = "map-graph", property = "clickData"),
#   # output=output(id = 'test2', property='children'),
#   params = list(input(id = "Downtown Vancouver link", property = "n_clicks"),
#                 input(id = "False Creek link", property = "n_clicks")),
#   function(n1,n2){
#     ctx <- callback_context()
#     prevent_update(is.null(ctx))
#     paste0(ctx$triggered$prop_id)
#     if (ctx$triggered$prop_id == "Downtown Vancouver link.n_clicks"){
#       return(list(points = 68))
#     } else if (ctx$triggered$prop_id == "False Creek link.n_clicks"){
#       return(list(points = 72))
#     }
#   }
# )
# app$callback(
#   output=output(id = 'test', property='children'),
#   params=list(input(id = 'open-offcanvas-scrollable', property='n_clicks')),
#   # input(id = 'tile-graph', property='clickData'),
#   # input(id = 'zscore-type', property='value'),
#   # input(id = "arv_tile_slider", property = "value")),
#   function(n1#,
#            # tile_clickdata,zscore,period_list
#   ) {
#     # prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     # paste0(as.character(map_clickdata$points[[1]]),
#     #        "-\n",
#     #        zscore,
#     #        "-\n",
#     #        as.character(tile_clickdata$points[[1]][2]),
#     #        "-\n",
#     #        as.character(tile_clickdata$points[[1]][3]),
#     #        "-\n",
#     #        as.character(period_list[[1]]),
#     #        "--",
#     #        as.character(period_list[[2]]))
#     paste0(n1)
#   })
# 
# app$callback(
#   output = output("test2", property = "children"),
#   params = list(input("open-offcanvas-scrollable", "n_clicks"),
#                 state("offcanvas-scrollable", "is_open")),
#   function(n1, is_open){
#     paste0(
#       (n1),
#       "-\n",
#       as.character(is_open)
#     )
#     # if (n1){
#     #   return(!is_open)
#     # }
#     # return(is_open)
#   }
# )
# 
app$callback(
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
## Run app

# app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050)) # NEW: MUST CHANGE FOR DEPLOYMENT
app$run_server(debug=TRUE)
# app

# command to add dash app in Rstudio viewer:
# rstudioapi::viewer("http://127.0.0.1:8050")