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

offcanvas = htmlDiv(list(
  dbcButton(
    "Open scrollable offcanvas",
    id="open-offcanvas-scrollable",
    n_clicks=0,
  ),
  dbcOffcanvas(
    htmlP("The contents on the main page are now scrollable."),
    id="offcanvas-scrollable",
    scrollable=T,
    title="Scrollable Offcanvas",
    is_open=T,
    placement = "end"
  ))
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
			 offcanvas
			 # dccLoading(loading_state = list(is_loading = T))
	), #### THIS IS NEW! Styles added
	  style = list('background-color' = '#BBCFF1',
								 'padding' = 10#,
								 # 'flex-basis' = '20%'#,
								 # 'position' = 'fixed',
								 # 'top' = 0,
								 # 'left' = 0,
								 # 'bottom' = 0,
								 # 'width' = '16rem',
								 # 'padding' = '2rem 1rem'
								 ),
	className = "sidebar"
)

div_main <- htmlDiv(
	list(
	  # htmlIframe(id = "leaflet",
	  #            src="assets/m.html",
	  #            style=list("height" = "500px", "width" = "100%"),
	  #            n_clicks = 0),
	  dbcCard(graph),
	  # graph,
	  htmlDiv(id = "tile-wrapper"),
	  slider,
	  htmlDiv(id = "loader-wrapper", className="loader-wrapper",children = dccLoading(id="loading",children = htmlDiv(id = "arv-tile-wrapper"),type = "circle"),style = list("margin-top" = "15px"))  
	),
	# style = list('flex-basis' = '80%')
)



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
                width = 3),
         dbcCol(div_main,
                width = 7)),
    #### THIS IS NEW! Styles added
    # style = list('display' = 'flex',
    #              'justify-content' = 'center')
  )
)))

## App Callbacks


# 
# 


app$callback(
  output = output(id = "tile-wrapper", property = "children"),
  params = list(input(id = 'map-graph',property = 'clickData'),
                input(id = "zscore-type", property = "value")),
  function(clickdata,zscore){
    return(dccGraph(
      id = 'tile-graph',
      figure=make_tile_graph(curve_number = as.integer(clickdata$points[[1]]), zscore_type = zscore),
      style = list(visibility = "visible")))
  }
)
# 
app$callback(
  output = output(id = "arv-tile-wrapper", property = "children"),
  params=list(input(id = 'map-graph', property='clickData'),
              input(id = 'tile-graph', property='clickData'),
              input(id = 'zscore-type', property='value'),
              input(id = "arv_tile_slider", property = "value")),
  function(map_clickdata,tile_clickdata,zscore,period_list) {
    # prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    return(
      dccGraph(
        id = 'arv_tile_graph',
        figure = make_arrival_tile_graph(curve_number = as.integer(map_clickdata$points[[1]]),
                                         zscore_type = zscore,
                                         Weekday_arv = as.integer(tile_clickdata$points[[1]][2]),
                                         Hour_arv = as.integer(tile_clickdata$points[[1]][3]),
                                         Period_cat = period_list),
        style = list(visibility = "visible")
      ))
  }
)
# 
# app$callback(
#   output=output(id = 'test', property='children'),
#   params=list(input(id = 'map-graph', property='clickData'),
#               input(id = 'tile-graph', property='clickData'),
#               input(id = 'zscore-type', property='value'),
#               input(id = "arv_tile_slider", property = "value")),
#   function(map_clickdata,tile_clickdata,zscore,period_list) {
#     prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
#     paste0(as.character(map_clickdata$points[[1]]),
#            "-\n",
#            zscore,
#            "-\n",
#            as.character(tile_clickdata$points[[1]][2]),
#            "-\n",
#            as.character(tile_clickdata$points[[1]][3]),
#            "-\n",
#            as.character(period_list[[1]]),
#            "--",
#            as.character(period_list[[2]]))
#   })


app$callback(
  output=output(id = 'test', property='children'),
  params=list(input(id = 'open-offcanvas-scrollable', property='n_clicks')),
              # input(id = 'tile-graph', property='clickData'),
              # input(id = 'zscore-type', property='value'),
              # input(id = "arv_tile_slider", property = "value")),
  function(n1#,
           # tile_clickdata,zscore,period_list
           ) {
    # prevent_update(is_null(map_clickdata$points[[1]]),is.null(tile_clickdata$points[[1]][2]),is.null(tile_clickdata$points[[1]][3]))
    # paste0(as.character(map_clickdata$points[[1]]),
    #        "-\n",
    #        zscore,
    #        "-\n",
    #        as.character(tile_clickdata$points[[1]][2]),
    #        "-\n",
    #        as.character(tile_clickdata$points[[1]][3]),
    #        "-\n",
    #        as.character(period_list[[1]]),
    #        "--",
    #        as.character(period_list[[2]]))
    paste0(n1)
  })

app$callback(
  output = output("test2", property = "children"),
  params = list(input("open-offcanvas-scrollable", "n_clicks"),
                state("offcanvas-scrollable", "is_open")),
  function(n1, is_open){
    paste0(
      (n1),
      "-\n",
      as.character(is_open)
    )
    # if (n1){
    #   return(!is_open)
    # }
    # return(is_open)
  }
)

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