heading_title <- htmlH1(id="header",'Gapminder Dash Demo')
heading_subtitle <- htmlH2('Looking at country data interactively')

zscoreDropdown <- dccDropdown(
	id = "zscore-type",
	options = map(
		1:nrow(zscore_type), function(i){
			list(label=zscore_type$label[i], value=zscore_type$value[i])
		}),
	value = "weekly"
)


zscoreDropdown_boot <- dbcDropdownMenu(children = list(dbcDropdownMenuItem("")))

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
  step = NA
  )

graph <- dccGraph(
	id = 'map-graph',
	figure=make_map_plot()
)

sources <- dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
