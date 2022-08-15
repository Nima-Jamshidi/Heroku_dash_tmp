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

graph <- dccGraph(
	id = 'map-graph',
	figure=make_map_plot() # gets initial data using argument defaults
)

# graph <- dl$Map(
#   # id = 'gap-graph',
#   make_map_plot() # gets initial data using argument defaults
# )



### Create graph components

graph_tile <- dccGraph(
	id = 'tile-graph',
	figure=make_tile_graph() # gets initial data using argument defaults
)

sources <- dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
