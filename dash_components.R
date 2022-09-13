heading_title <- h1(id="header","EVO CAR SHARE IDLE FLEET",style=list("margin-left"=10))
heading_subtitle <- h2('Looking at Neighbourhood Data Interactively in Time and Space',style = list("padding" = 10))



zscoreDropdown <- dccDropdown(
	id = "zscore-type",
	options = map(
		1:nrow(zscore_type), function(i){
			list(label=zscore_type$label[i], value=zscore_type$value[i])
		}),
	value = "weekly",
	clearable=F
)

scaleDropdown <- dccDropdown(
  id = "scale-type",
  options = map(
    1:nrow(scale_list), function(i){
      list(label=scale_list$label[i], value=scale_list$value[i])
    }),
  value = "3",
  clearable=F
)




slider <- div(dashCoreComponents::dccRangeSlider(
  id = "arv_tile_slider",
  min=0,
  max=600,
  marks = periods_list,
  value = list(180,360),
  count = 2,
  pushable = T,
  allowCross = F,
  step = NA,
  vertical = F
  # className = "position-sticky right"
  ),
  style = list(
    "padding" = 10
  ))



offcanvas = div(list(
  dbcButton(
    "Open scrollable offcanvas",
    id="open-offcanvas-scrollable",
    n_clicks=0,
  ),
  dbcOffcanvas(
    p("The contents on the main page are now scrollable."),
    id="offcanvas-scrollable",
    scrollable=T,
    title="Scrollable Offcanvas",
    is_open=F,
    placement = "end"
  ))
)

offcanvas2 = div(list(
  dbcButton(
    "Open scrollable offcanvas2",
    id="open-offcanvas-scrollable2",
    n_clicks=0,
  ),
  dbcOffcanvas(
    p("Hellooo"),
    id="offcanvas-scrollable2",
    scrollable=T,
    title="Scrollable Offcanvas2",
    is_open=F,
    placement = "end"
  ))
)

graph_map <- dccGraph(
	id = 'map-graph',
	figure=make_map_plot(),
	style = list(width = "100%",
	             `aspect-ratio`= "2140 / 1326"
	             # height= "10vh"
	             # `padding-bottom`: "30%"
	  # ,height = '45vh'
	  ),
	config = list(
	  modeBarButtonsToRemove = list('toggleSpikelines','lasso2d','select2d','hoverClosestCartesian','hoverCompareCartesian','autoScale2d','zoomIn2d', 'zoomOut2d')),
	clickData = list(points = 84)
)

graph_tile <- dccGraph(
  id = 'tile-graph',
  figure=make_tile_graph(),
  config = list(
    # displayModeBar = T,
                modeBarButtonsToRemove = list('toggleSpikelines','lasso2d','select2d','hoverClosestCartesian','hoverCompareCartesian')),
  clickData = list(points = list(c(NA,'1','15')))
)

graph_arv_tile <- dccGraph(
  id = 'arv-tile-graph',
  figure=make_arrival_tile_graph(),
  config = list(
    # displayModeBar = F,
    modeBarButtonsToRemove = list('toggleSpikelines','lasso2d','select2d','hoverClosestCartesian','hoverCompareCartesian'))
)

#sources <- dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
