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

## Important! Source the files (order matters!)
source('dash_functions.R')
source('dash_components.R')

## Create Dash instance

app <- Dash$new(suppress_callback_exceptions = T)

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
			 zscoreDropdown,
			 htmlLabel('Select y scale : '),
			 htmlBr(),
			 logbutton,
			 sources,
			 htmlH2(as.character("Hello"),id = "test",className = "output-example-loading"),
			 htmlH2(as.character("Hello"),id = "test2"),
			 dccLoading(loading_state = list(is_loading = T))#,
			 # dccLoading(id = "loading",type = "circle",children = htmlDiv(htmlDiv(id = "loading-output",children = "TEST")))
	), #### THIS IS NEW! Styles added
	  style = list('background-color' = '#BBCFF1',
								 'padding' = 10,
								 'flex-basis' = '20%')
)

div_main <- htmlDiv(
	list(
  # htmlButton(
	#   children = "submit",
	#   id="input",
	#   # options=list(
	#   #   list("label" = "ABCD", "value" = "ABCD.html"),
	#   #   list("label" = "XYZ", "value" = "XYZ.html")),
	#   # value="ABCD.html",
	#   n_clicks = 0),
	  # graph,
	  # graph_country,
	  # htmlIframe(id = "leaflet",
	  #            src="assets/m.html",
	  #            style=list("height" = "500px", "width" = "100%"),
	  #            n_clicks = 0),
	  # slider,
	  graph,
	  # dccLoading(id="loading",children = graph_tile,type = "graph"),
	  # htmlDiv(children = graph_tile, id = "loading-block",style = list( visibility="hidden")),
	  # htmlDiv(id = "loading-tile",children = "click on map"),
	  # graph_tile,
	  htmlDiv(id = "tile-wrapper"),
	  slider,
	  htmlDiv(id = "arv-tile-wrapper")
	  # graph_arv_tile,
	  # dccRangeSlider(
	  #   id = "testest",
	  #   min=0,
	  #   max=600,
	  #   marks = periods_list,
	  #   value = list(0,120),
	  #   count = 2,
	  #   pushable = T,
	  #   allowCross = F,
	  #   step = NA,
	  #   # included=F
	  # )
	),
	style = list('flex-basis' = '80%')
)

## Specify App layout

app %>% set_layout(
	div_header,
	htmlDiv(
		list(
			div_sidebar,
			div_main
		), #### THIS IS NEW! Styles added
			style = list('display' = 'flex',
								 'justify-content'='center')
	)
)

## App Callbacks


# app$callback(
# 	#update figure of gap-graph
# 	output=output(id = 'test', property='children'),
# 	#based on values of year, continent, y-axis components
# 	params=list(input(id = 'map-graph', property='clickData')),
# 	#this translates your list of params into function arguments
# 	function(clickdata) {
# 	  # Sys.sleep(1)
# 		is.null(clickdata$points[[1]])
# 	})

# app$callback(
# 	#update figure of gap-graph
# 	output=output(id = 'test', property='loading_state'),
# 	#based on values of year, continent, y-axis components
# 	params=list(input(id = 'map-graph', property='clickData')),
# 	#this translates your list of params into function arguments
# 	function(clickdata) {
# 	  # Sys.sleep(1)
# 		list(is_loading = !(is.null(clickdata$points[[1]])),prop_name = "children")
# 	})



# app$callback(
#   #update figure of gap-graph
#   output=output(id = 'tile-graph', property='loading_state'),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData')),
#   #this translates your list of params into function arguments
#   function(clickdata,zscore) {
#     return(is.null((clickdata$points[[1]])))
#     # make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore)
#   })

# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'tile-graph', property='figure'),
#               output(id = 'tile-graph', property='style')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'zscore-type', property='value')),
#   #this translates your list of params into function arguments
#   function(clickdata,zscore) {
#     prevent_update(is.null((clickdata$points[[1]])))
#     return(list(make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),list(display = "block")))
#   })

# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'loading-tile', property='children')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData')),
#   #this translates your list of params into function arguments
#   function(clickdata) {
#     prevent_update(is.null((clickdata$points[[1]])))
#     # new_loading_style = loading_style
#     # return(list(make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),list(visibility = "visible")))
#     return(graph_tile)
#   })

app$callback(
  output = output(id = "tile-wrapper", property = "children"),
  params = list(input(id = 'map-graph',property = 'clickData'),
                input(id = "zscore-type", property = "value")),
  function(clickdata,zscore){
    return(dccGraph(
      id = 'tile-graph',
      figure=make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),#,
      # loading_state = list(is_loading = F)# gets initial data using argument defaults
      # style=list(display = "inline-block")
      style = list(visibility = "visible")))
  }
)


# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'tile-graph', property='figure'),
#               output(id = 'tile-graph', property='style')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'zscore-type', property='value')),
#   #this translates your list of params into function arguments
#   function(clickdata,zscore) {
#     # prevent_update(is.null((clickdata$points[[1]])))
#     # new_loading_style = loading_style
#     return(list(make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),list(visibility = "visible")))
#   })

# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'tile-graph', property='style')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'zscore-type', property='value')),
#   #this translates your list of params into function arguments
#   function(clickdata,zscore) {
#     # prevent_update(is.null((clickdata$points[[1]])))
#     return(list(display = "inline"))
#   })

# app$callback(
#   #update figure of gap-graph
#   output=output(id = 'test2', property='children'),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'tile-graph', property='clickData')),
#   #this translates your list of params into function arguments
#   function(clickdata) {
#     paste0(
#       as.integer(clickdata$points[[1]][2])#,
#       # "\n",
#       # as.character(clickdata$points[[1]][3])
#       )
#     # as.character(clickdata$points[[1]])
#     # as.character(clickdata[[1]][[1]])
#   })

app$callback(
  output = output(id = "arv-tile-wrapper", property = "children"),
  params=list(input(id = 'map-graph', property='clickData'),
              input(id = 'tile-graph', property='clickData'),
              input(id = 'zscore-type', property='value'),
              input(id = "arv_tile_slider", property = "value")),
  function(map_clickdata,tile_clickdata,zscore,period_list) {
    prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    return(
      dccGraph(
        id = 'arv_tile_graph',
        figure = make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
                                         zscore_type = zscore, 
                                         Weekday_arv = as.integer(tile_clickdata$points[[1]][2]), 
                                         Hour_arv = as.integer(tile_clickdata$points[[1]][3]), 
                                         Period_cat = period_list),
        style = list(visibility = "visible")#,
        # animate = T
      ))
  }
)

# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'arv_tile_graph', property='figure'),
#               output(id = 'arv_tile_graph', property='style')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'tile-graph', property='clickData'),
#               input(id = 'zscore-type', property='value'),
#               input(id = "arv_tile_slider", property = "value")),
#   #this translates your list of params into function arguments
#   function(map_clickdata,tile_clickdata,zscore,period_list) {
#     # as.character(clickdata$points[[1]])
#     prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     return(list(make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
#                                         zscore_type = zscore,
#                                         Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
#                                         Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
#                                         Period_cat = period_list),list(visibility = "visible")))
#   })
# 
# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'arv_tile_graph', property='figure'),
#               output(id = 'arv_tile_graph', property='style')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'tile-graph', property='clickData'),
#               input(id = 'zscore-type', property='value'),
#               input(id = "arv_tile_slider", property = "value")),
#   #this translates your list of params into function arguments
#   function(map_clickdata,tile_clickdata,zscore,period_list) {
#     # as.character(clickdata$points[[1]])
#     prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     return(list(make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
#                             zscore_type = zscore, 
#                             Weekday_arv = as.integer(tile_clickdata$points[[1]][2]), 
#                             Hour_arv = as.integer(tile_clickdata$points[[1]][3]), 
#                             Period_cat = period_list),list(visibility = "visible")))
#   })


app$callback(
  #update figure of gap-graph
  output=output(id = 'test', property='children'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'map-graph', property='clickData'),
              input(id = 'tile-graph', property='clickData'),
              input(id = 'zscore-type', property='value'),
              input(id = "arv_tile_slider", property = "value")),
  #this translates your list of params into function arguments
  function(map_clickdata,tile_clickdata,zscore,period_list) {
    # as.character(clickdata$points[[1]])
    prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    paste0(as.character(map_clickdata$points[[1]]),
           "-\n",
           zscore,
           "-\n",
           as.character(tile_clickdata$points[[1]][2]),
           "-\n",
           as.character(tile_clickdata$points[[1]][3]),
           "-\n",
           as.character(period_list[[1]]),
           "--",
           as.character(period_list[[2]]))
    # make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]), zscore_type = zscore, Weekday_arv = as.integer(tile_clickdata$points[[1]][2]), Hour_arv = as.integer(tile_clickdata$points[[1]][3]))
  })

# app$callback(
#   #update figure of gap-graph
#   output=list(output(id = 'loading-output', property='children')),
#   #based on values of year, continent, y-axis components
#   params=list(input(id = 'map-graph', property='clickData')),
#   #this translates your list of params into function arguments
#   function(clickdata) {
#     prevent_update(((clickdata$points[[1]]))==59)
#     # new_loading_style = loading_style
#     return(NA)
#   })

# app$callback(
# 	#update figure of gap-graph
# 	output=list(id = 'gap-graph', property='figure'),
# 	#based on values of year, continent, y-axis components
# 	params=list(input(id = 'y-axis', property='value'),
# 							input(id = 'yaxis-type', property='value')),
# 	#this translates your list of params into function arguments
# 	function(yaxis_value, yaxis_scale) {
# 		make_plot(yaxis_value, yaxis_scale)
# 	})
# 
# ## Updates our second graph using linked interactivity
# app$callback(output = list(id = 'gap-graph-country', property = 'figure'),
# 						 params = list(input(id='y-axis', property='value'),
# 						 							# Here's where we check for graph interactions!
# 						 							input(id='gap-graph', property='clickData')),
# 						 function(yaxis_value, clickData) {
# 						 	# clickData contains $x, $y and $customdata
# 						 	# you can't access these by gapminder column name!
# 						 	country_name = clickData$points[[1]]$customdata
# 						 	make_country_graph(country_name, yaxis_value)
# 						 })

## Run app

# app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050)) # NEW: MUST CHANGE FOR DEPLOYMENT
app$run_server(debug=TRUE)
# app

# command to add dash app in Rstudio viewer:
# rstudioapi::viewer("http://127.0.0.1:8050")