## Assign components to variables

heading_title <- htmlH1(id="header",'Gapminder Dash Demo')
heading_subtitle <- htmlH2('Looking at country data interactively')

### Create the dropdown
zscoreDropdown <- dccDropdown(
	id = "zscore-type",
	options = map(
		1:nrow(zscore_type), function(i){
			list(label=zscore_type$label[i], value=zscore_type$value[i])
		}),
	value = "weekly"
)

### Create the button 
logbutton <- dccRadioItems(
	id = 'yaxis-type',
	options = list(list(label = 'Linear', value = 'linear'),
								 list(label = 'Log', value = 'log')),
	value = 'linear'
)

slider <- dccRangeSlider(
  id = "arv_tile_slider",
  min=0,
  max=600,
  marks = periods_list,
  value = list(0,120),
  count = 2,
  pushable = T,
  allowCross = F,
  step = NA,
  # included=F
  )

graph <- dccGraph(
	id = 'map-graph',
	figure=make_map_plot() # gets initial data using argument defaults
)

# graph <- dl$Map(
#   # id = 'gap-graph',
#   make_map_plot() # gets initial data using argument defaults
# )



### Create graph components

# graph_tile <- dccGraph(
# 	id = 'tile-graph',
# 	figure=make_tile_graph(),#,
# 	# loading_state = list(is_loading = F)# gets initial data using argument defaults
# 	# style=list(display = "inline-block")
# 	style = list(visibility = "hidden")
# )

# graph_arv_tile <- dccGraph(
#   id = 'arv_tile_graph',
#   figure = make_arrival_tile_graph(),
#   style = list(visibility = "hidden")#,
#   # animate = T
# )

sources <- dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
