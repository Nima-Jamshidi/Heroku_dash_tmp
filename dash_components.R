heading_title <- h1(id="header","EVO CAR SHARE IDLE FLEET IN TIME AND SPACE")
heading_subtitle <- h2('Looking at neighbourhood data interactively',style = list("padding" = 10))



zscoreDropdown <- dccDropdown(
	id = "zscore-type",
	options = map(
		1:nrow(zscore_type), function(i){
			list(label=zscore_type$label[i], value=zscore_type$value[i])
		}),
	value = "weekly",
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

# slider = dccRangeSlider(
#   min=0,
#   max=10,
#   marks=list(
#     "0" = "0°F",
#     "3" = "3°F",
#     "5" = "5°F",
#     "7.65" = "7.65°F",
#     "10" = "10°F"
#   ),
#   value=list(3, 7.65),
#   step = NULL
# )


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
	style = list(height = '45vh'),
	config = list(
	  # displayModeBar = T,
	              modeBarButtonsToRemove = list('toggleSpikelines','lasso2d','select2d','hoverClosestCartesian','hoverCompareCartesian','autoScale')),
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
