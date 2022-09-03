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
# library(dashTable)
library(dashBootstrapComponents)

## Important! Source the files (order matters!)
source('dash_functions.R')
source('dash_components.R')

## Create Dash instance

app <- Dash$new(suppress_callback_exceptions = T,
                external_stylesheets = c(dbcThemes$MORPH,
                                            "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"))

## Specify layout elements

div_header <- htmlDiv(
  list(heading_title,
       heading_subtitle
  )#, #### THIS IS NEW! Styles added
  # style = list(
  #   backgroundColor = '#337DFF',
  #   textAlign = 'center',
  #   color = 'white',
  #   margin = 5,
  #   marginTop = 0
  # )
)


# div_sidebar <- htmlDiv(
#   list(htmlLabel('Select y-axis metric:'),
#        htmlBr(),
#        # zscoreDropdown,
#        # htmlLabel('Select y scale : '),
#        htmlBr(),
#        logbutton,
#        sources,
#        htmlH2(as.character("Hello"),id = "test"
#               # ,className = "output-example-loading"
#               ),
#        htmlH2(as.character("Hello"),id = "test2"),
#        offcanvas,
#        offcanvas2
#        # dccLoading(loading_state = list(is_loading = T))
#   ), #### THIS IS NEW! Styles added
#   style = list('background-color' = '#BBCFF1',
#                'padding' = 10,
#                # 'flex-basis' = '20%'#,
#                # 'position' = 'fixed',
#                'top' = 20,
#                # 'left' = 0,
#                'bottom' = 50
#                # 'width' = '16rem',
#                # 'padding' = '2rem 1rem'
#   )#,
#   # className = "sidebar"
#   # className = "position-sticky left"
#   # className = "position-absolute left"
# )

div_sidebar <- htmlDiv(
  list(htmlH2(htmlB(toupper('Nima Jamshidi'))),
       htmlBr(),
       htmlH6(htmlI("M.Sc. in Resources, Environment and Sustainability, UBC")),
       htmlBr(),
       htmlA(
         id = "LinkedIn",
         className = "fa fa-linkedin fa-2x",
         # children = "LinkedIn",
         href = "https://www.linkedin.com/in/nima-jamshidi-991711131/",
         style = list("text-decoration" = "none")
       ),
       # htmlBr(),
       # htmlH2(" "),
       "  ",
       htmlA(
         id = "GitHub",
         # children = "GitHub",
         className = "fa fa-github fa-2x",
         href = "https://github.com/Nima-Jamshidi",
         style = list("text-decoration" = "none")
       )
       # zscoreDropdown,
       # htmlLabel('Select y scale : '),
       # htmlBr(),
       # logbutton,
       # sources,
       # htmlH2(as.character("Hello"),id = "test"
       #        # ,className = "output-example-loading"
       # ),
       # htmlH2(as.character("Hello"),id = "test2"),
       # offcanvas,
       # offcanvas2
       # dccLoading(loading_state = list(is_loading = T))
  ), #### THIS IS NEW! Styles added
  style = list('background-color' = '#BBCFF1',
               'padding' = 10,
               # 'flex-basis' = '20%'#,
               'position' = 'sticky',
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
          dbcCard(list(
            graph_map,
            dbcAlert(
              htmlH5(htmlB("Check out Vancouver Map! Hover over the map to see neighbourhoods. Click on them to see their fleet density through out the week.")),
              id="alert-map1",
              is_open=T,
              dismissable = F,
              fade = F,
              color="primary"
              # duration=4000
              # className = "position-sticky right",
              # style = list("margin-top" = 50)
            ),
            dbcAlert(
              id="alert-map2",
              is_open=F,
              duration=4000
              # className = "position-sticky right",
              # style = list("margin-top" = 50)
          )),className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-top` = 10 ,`margin-bottom` = 10)),
          ,width = 6)
        )
      ),
    dbcRow(
      list(
        dbcCol(
          dbcCard(htmlDiv(list(graph_tile,
                               dbcCard(list(htmlH4("Settings:",style = list("padding"=5)),dbcCol(zscoreDropdown,width = 6))),
                               dbcAlert(
                                 htmlH5(htmlB("This graph shows Hourly Aggregate Fleet Idle Time (HAFIT). Hover over the graph to see the values for different hours of a day of the week. The redder or bluer the block the higher or lower the density of idle cars at that hour.")),
                                 id="alert-tile1",
                                 is_open=T,
                                 dismissable = F,
                                 fade = F,
                                 color="primary"
                                 # duration=4000
                                 # className = "position-sticky right",
                                 # style = list("margin-top" = 50)
                               ),
                               dbcAlert(
                                 id="alert-tile2",
                                 is_open=F,
                                 duration=4000
                                 # className = "position-sticky right",
                                 # style = list("margin-top" = 50)
                               ))),className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-top` = 10 ,`margin-bottom` = 10)),width = 6),
        dbcCol(
          htmlDiv(tile_text),width = 6)
        # dbcCol(
        #   htmlDiv(zscoreDropdown),width = 2)
      )
    ),
    dbcRow(
      list(
        dbcCol(
          htmlDiv(arv_tile_text),width = 6),
        dbcCol(
          dbcCard(htmlDiv(list(dccLoading(graph_arv_tile,type = "circle"),dbcCard(list(htmlH4("Settings:",style = list("padding"=5)),dbcCol(slider,width = 12))),
                               dbcAlert(
                                 id="alert-arv-tile1",
                                 is_open=T,
                                 dismissable = F,
                                 fade = F,
                                 color="primary"
                                 # duration=4000
                                 # className = "position-sticky right",
                                 # style = list("margin-top" = 50)
                               ),
                               dbcAlert(
                                 id="alert-arv-tile2",
                                 is_open=F,
                                 duration=4000
                                 # className = "position-sticky right",
                                 # style = list("margin-top" = 50)
                               ))),className = "position-sticky right",style = list(top = 10, bottom = 10, `margin-bottom` = 10)),width = 6)
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

# app %>% set_layout(dccGraph(
#   id = 'map-graph',
#   figure=make_map_plot(),
#   style = list(height = '100vh',
#                width = '100vh',
#                position = 'absolute'),
#   config = list(
#     # displayModeBar = T,
#     modeBarButtonsToRemove = list('toggleSpikelines','lasso2d','select2d','hoverClosestCartesian','hoverCompareCartesian','autoScale')),
#   clickData = list(points = 84)
# ))



app %>% set_layout(dbcContainer(list(
  dbcRow(dbcCol(div_header),className = "navbar navbar-expand-lg navbar-light bg-light"),
  dbcRow(
    list(dbcCol(div_sidebar,
                width = 2),
         dbcCol(div_main, width = 10))
  )#,
  # dbcRow(list(
  #       dbcCard("Hello",id = "card-test1",
  #               style=list(
  #                 "height" = "100vh",
  #                 "width" = "25vw",
  #                 # "float" = "left",
  #                 # 'display' = 'inline-block'
  #                 'position' = 'absolute',
  #                 'left'= 0#,
  #                 # 'visibility' = 'visible'
  #               )
  #       ),
  #       dbcFade(dbcCard(htmlDiv("Nima",id = "Nima-div"),id = "card-test2",
  #                       style=list(
  #                         "height" = "100vh",
  #                         "width" = "25vw",
  #                         # "float" = "left",
  #                         # 'display' = 'inline-block'
  #                         'position' = 'absolute',
  #                         'left'= 0,
  #                         'opacity' = 0.5
  #                         # 'visibility' = 'visible'
  #                       )
  #                       ),
  #               id="fade",
  #               is_in=T,
  #               appear=F#,
  #               # style=list(
  #               #   "height" = "100vh",
  #               #   "width" = "25vw",
  #               #   # "float" = "left",
  #               #   # 'display' = 'inline-block'
  #               #   'position' = 'absolute',
  #               #   'left'= 0
  #               #   # 'visibility' = 'visible'
  #               # )
  #       )))
),fluid = T))

## App Callbacks

app$callback(
  output = output(id = "fade", property = "is_in"),
  # output=output(id = 'test2', property='children'),
  params = list(input(id = "Nima-div", property = "n_clicks")),
  function(n1){
    prevent_update(is.null(n1[[1]]))
    # prevent_update(n_clicks!=1)
    return(F)
  }
)

app$callback(
  output = list(output("alert-map1", "is_open"),
                output("alert-map2", "is_open"),
                output("alert-map2", "children")),
  params = list(input("map-graph", "clickData")),

function(clickdata){
  prevent_update(is_null(clickdata$points[[1]]))
  return(list(F,T,htmlH5(htmlB(paste0("Scroll down to see the fleet density for ",location_finder(as.integer(clickdata$points[[1]])),".")))))
})


app$callback(
  output = list(output("alert-tile1", "is_open"),
                output("alert-tile2", "is_open"),
                output("alert-tile2", "children")),
  params = list(input("tile-graph", "clickData")),
  
  function(tile_clickdata){
    prevent_update(is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    return(list(F,T,htmlH5(htmlB(paste0("Scroll down to see the fleet density throughout the week for the cars that arrive at ",as.integer(tile_clickdata$points[[1]][3])," on ",c(
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

app$callback(
  output = list(output("alert-arv-tile1", "is_open"),
                output("alert-arv-tile2", "is_open"),
                output("alert-arv-tile1", "children"),
                output("alert-arv-tile2", "children")),
  params = list(input(id = "arv_tile_slider", property = "value"),
                state(id = "arv_tile_slider", property = "value")),
  
  function(period_list){
    # prevent_update(is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    
    
    return(list(T,T,htmlH5(htmlB(paste0("You can see the density of idle cars with an idle duration ",ifelse(period_list[[2]]==600,
                                                                                                      paste0("more than ",periods$label[periods$value==period_list[[1]]],"."),
                                                                                                      paste0("between ",periods$label[periods$value==period_list[[1]]]," and ",periods$label[periods$value==period_list[[2]]],"."))))),
                htmlH5(htmlB(paste0("Scroll up to choose other neighbourhoods and times.")))))
  })
# 
# 
app$callback(
  output = output(id = "map-graph", property = "clickData"),
  # output=output(id = 'test2', property='children'),
  params = list(input(id = "Downtown Vancouver link", property = "n_clicks"),
                input(id = "False Creek link", property = "n_clicks"),
                input(id = "Grouse Mountain link", property = "n_clicks"),
                input(id = "DT 8-9AM  Mondays link", property = "n_clicks"),
                input(id = "YL 8-9AM  Mondays link", property = "n_clicks")),
  function(n1,n2,n3,n4,n5){
    # Sys.sleep(1)
    ctx <- callback_context()
    prevent_update(is.null(ctx$triggered$prop_id))
    # paste0(ctx$triggered$prop_id)
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


# app$callback(
#   output = list(output(id = "map-graph", property = "clickData"),
#                 output(id = "tile-graph", property = "clickData"),
#                 output(id = "arv_tile_slider", property = "value")),
#   # output=output(id = 'test2', property='children'),
#   params = list(input(id = "DT 8-9AM  Mondays link", property = "n_clicks")
#                 # input(id = "False Creek link", property = "n_clicks"),
#                 # input(id = "Grouse Mountain link", property = "n_clicks")
#                 ),
#   function(n1){
#     # Sys.sleep(1)
#     ctx <- callback_context()
#     prevent_update(is.null(ctx$triggered$prop_id))
#     # paste0(ctx$triggered$prop_id)
#     if (ctx$triggered$prop_id == "DT 8-9AM  Mondays link.n_clicks"){
#       print("hey")
#       return(list(list(points = 68),list(points = list(c(NA,'1','8'))),list(0,120)))
#     # } else if (ctx$triggered$prop_id == "False Creek link.n_clicks"){
#       # return(list(points = 72))
#     # } else if (ctx$triggered$prop_id == "Grouse Mountain link.n_clicks"){
#       # return(list(points = 77))
#     }
#   }
# )


app$callback(
  output = list(output(id = "tile-graph", property = "clickData"),
                output(id = "arv_tile_slider", property = "value")),
  params = list(input(id = "DT 8-9AM  Mondays link", property = "n_clicks"),
                input(id = "YL 8-9AM  Mondays link", property = "n_clicks")),
  function(n1,n2){
    ctx <- callback_context()
    prevent_update(is.null(ctx$triggered$prop_id))
    # paste0(ctx$triggered$prop_id)
    if (ctx$triggered$prop_id == "DT 8-9AM  Mondays link.n_clicks"){
      # print("hey")
      return(list(
        # 1,
        list(points = list(c(NA,'1','8'))),
        list(0,120)))
    } else if (ctx$triggered$prop_id == "YL 8-9AM  Mondays link.n_clicks"){
      # print("hey")
      return(list(
        # 1,
        list(points = list(c(NA,'1','8'))),
        list(0,120)))
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
      # prevent_update(is.null(data$points[[1]]))
      # paste0(location_finder(data$points[[1]]))
      # location_finder(curve_number = data$points[[1]])
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

app$callback(
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
## Run app

# app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050)) # NEW: MUST CHANGE FOR DEPLOYMENT
app$run_server(debug=TRUE)
# app

# command to add dash app in Rstudio viewer:
# rstudioapi::viewer("http://127.0.0.1:8050")