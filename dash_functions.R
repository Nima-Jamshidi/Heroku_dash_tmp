suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyverse))
# library(gapminder)
library(leaflet)

# Storing the labels/values as a tibble means we can use this both 
# to create the dropdown and convert colnames -> labels when plotting
# yaxisKey <- tibble(label = c("GDP Per Capita", "Life Expectancy", "Population"),
# 									 value = c("gdpPercap", "lifeExp", "pop"))

## Make plot

make_map_plot <- function(){
	
	# gets the label matching the column value
	# y_label <- yaxisKey$label[yaxisKey$value==yaxis]
	
	#filter our data based on the year/continent selections
	data <- readRDS("data/map plot/sf_neighborhoods_t.rds")
	# make the plot!
	
	### the customdata mapping adds country to the tooltip and allows
	# its selection using clickData.
	plot <- ggmap(google_map, darken = c(0.4, "white")) +
	  geom_sf(
	    data = data,
	    inherit.aes = FALSE,
	    aes(fill="pink",
	        text = paste0("</br>",
	                      location)),
	    # alpha = 0.5,
	    colour = 'white',
	    size = 0.3,
	    alpha= 0.6
	  ) #+
	  # geom_sf_label(data = data,
	  #               inherit.aes = FALSE,aes(label = location))+
	  # geom_sf(
	  #   data = sf_neighborhoods_t,
	  #   inherit.aes = FALSE,
	  #   aes(fill=as.factor(company_car2go)),
	  #   # alpha = 0.5,
	  #   colour = 'white',
	  #   size = 0.3,
	  #   alpha= 0.6
	  # ) +
	  # scale_fill_manual(values = c(scales::alpha("#AADDCC", 0.5), "#FFEEBB")) +
	  scale_fill_manual(values=c("#b35db8FF")) +
	  # scale_fill_manual(values = c("magenta", "cyan")) +
	  # geom_text_repel(data = n7,
	  #                 aes(x = center_long, y = center_lat, label = location),
	  #                 size = 1,
	  #                 # label.padding = 0.35,
	  #                 hjust=0.5) +
	  # geom_text_repel(data = n7,
	  #                 aes(x = center_long, y = center_lat, label = location),
	  #                 size = 1,
	  #                 label.padding = 0.15,
	  #                 point.padding = 0,
	#                 box.padding = .15,
	#                 hjust=0.5) +
	# geom_text(data = n7,
	#                 aes(x = center_long, y = center_lat, label = location),
	#                 size = 1,
	#                 # label.padding = 0.15,
	#                 hjust=0.5) +
	# geom_point(data=LongLatToUTM(as.numeric(unlist(evo_data %>% filter(location==Name) %>% dplyr::select(lon))),as.numeric(unlist(evo_data %>% filter(location==Name) %>% dplyr::select(lat)))),aes(X,Y),alpha=0.05,size=0.005)+
	# scale_x_continuous(name = "Longitude") +
	# scale_y_continuous(name = "Latitude") +
	# coord_fixed(ratio = 1) +
	theme_bw() +
	  theme(
	    # legend.position = "none",
	    panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank(),
	    axis.ticks = element_blank(),
	    axis.text = element_blank(),
	    axis.title = element_blank(),
	    legend.position="none"
	  )
	
	ggplotly(plot,tooltip = "text")
	# ggplotly(plot)
	# p <- ggplot(data, aes(x = year, y = !!sym(yaxis), 
	# 											colour = continent, 
	# 											customdata=country)) +
	# 	geom_jitter(alpha=0.6) +
	# 	scale_color_manual(name = 'Continent', values = continent_colors) +
	# 	scale_x_continuous(breaks = unique(data$year))+
	# 	xlab("Year") +
	# 	ylab(y_label) +
	# 	ggtitle(paste0("Change in ", y_label, " over time (Scale : ", scale, ")")) +
	# 	theme_bw()
	
	# if (scale == 'log'){
	# 	p <- p + scale_y_continuous(trans='log10')
	# }
	# 
	# ggplotly(p) %>%
	# 	### this is optional but changes how the graph appears on click
	# 	# more layout stuff: https://plotly-r.com/improving-ggplotly.html
	# 	layout(clickmode = 'event+select')
	
}

### Create the line graph

make_country_graph <- function(country_select="Canada",
															 yaxis="gdpPercap"){
	
	# gets the label matching the column value
	y_label <- yaxisKey$label[yaxisKey$value==yaxis]
	
	#filter our data based on the year/continent selections
	data <- gapminder %>%
		filter(country == country_select)
	
	# make the plot
	p <- ggplot(data, aes(x=year, y=!!sym(yaxis), colour=continent)) +
		geom_line() +
		scale_color_manual(name="Continent", values=continent_colors) +
		scale_x_continuous(breaks = unique(data$year))+
		xlab("Year") +
		ylab(y_label) +
		ggtitle(paste0("Change in ", y_label, " Over Time: ", 
									 country_select)) +
		theme_bw()
	
	ggplotly(p)
}
