suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyverse))
library(gapminder)
library(leaflet)
library(ggmap)
library(base64enc)

zscore_type <- tibble(label = c("Weekly normalized", "Daily normalized"),
									 value = c("weekly", "daily"))


periods <- tibble(label = c("0 min","10 min","30 min","1 hr","2 hr","3 hr","6 hr","6+ hr"),
                  value = c(0,10,30,60,120,180,360,600)) 

periods_list = as.list(periods$label)
names(periods_list) = periods$value

make_map_plot <- function(){

  	data <- readRDS("data/map plot/sf_neighborhoods_t.rds")
	google_map <- dataURI(file = "data/map plot/google_map.png")
	bbox_sf <- readRDS("data/map plot/bbox_sf.rds")
	
	plot_ly(type = "scatter",data, split = ~location, showlegend = F,
	        hoverlabel = list(namelength = 0)
	) %>% layout(clickmode = "event+select") %>% 
	  layout(xaxis = list(range = c((bbox_sf[1,]))),
	         yaxis = list(range = c((bbox_sf[2,])))) %>%
	  layout(
	    images = list(
	      list(
	        source =  google_map,
	        xref = "x",
	        yref = "y",
	        x = bbox_sf[1,1],
	        y = bbox_sf[2,2],
	        sizex = bbox_sf[1,2]-bbox_sf[1,1],
	        sizey = bbox_sf[2,2]-bbox_sf[2,1],
	        sizing = "stretch",
	        opacity = 0.4,
	        layer = "over"
	      )
	    )
	  ) %>% 
	  highlight(on = "plotly_click", color = "blue")
	
}


make_tile_graph <- function(curve_number=117,zscore_type = "weekly"){
	data <- readRDS("data/hourly tile plot/hourly_tile_plot_data.rds")
	
  locations <-
    data %>% distinct(location) %>% arrange(location)
  j=curve_number-58
  Name <- locations$location[j]
  zscore = paste0("zscore_",tolower(zscore_type))
  mean = paste0("mean_",tolower(zscore_type))
  std = paste0("std_",tolower(zscore_type))
   
    data_plot <- data %>% filter(location == Name)
    data_plot$weekday <-
      fct_relevel(
        data_plot$weekday,
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
        "H/ Monday"
      )
    
    hweek <- 
      ggplot(data_plot , aes(weekday, hour)) +
      geom_raster(
        aes(fill = !!sym(zscore),
            text = paste0("</br><b>",
                          hour,
                          ":00-",
                          hour+1,
                          ":00 ",
                          weekday,
                          "</b></br>Hourly aggregate fleet idle time = ",
                          round(number,1),
                          "</br>    Z-score = ",
                          round(!!sym(zscore),1),
                          "</br>    mean = ",
                          round(!!sym(mean),1),
                          "</br>    std = ",
                          round(!!sym(std),1))),
        hjust = 0.5,
        vjust = 0.5,
        interpolate = FALSE
      ) +
      scale_fill_gradient2(
        name = "Z-score",
        low = "blue",
        mid = "white",
        high = "red",
        na.value = "grey50",
      ) +
    coord_cartesian(expand = FALSE, clip = "off") +
      theme_bw() +
      theme(plot.margin = margin(20, 2, 2, 2, unit = "pt"),plot.title = element_text(vjust=7,size=10)) +
      scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 23),
                         labels = as.character(c(0, 4, 8, 12, 16, 20, 23)))+
      labs(x="",
           y="Hour of Day"
      )

  plotly::ggplotly(hweek, tooltip = "text") %>% layout(clickmode = "event+select")
}

make_arrival_tile_graph <-
  function(curve_number = 63,
           zscore_type = "weekly",
           Weekday_arv = 2,
           Hour_arv = 8,
           Period_cat = list(60,360)) {
    data <- readRDS("data/arrival tile plot/arrival_tile_plot_data.rds")
  
    locations <-
      data %>% distinct(location) %>% arrange(location)
    j = curve_number - 58
    Name <- locations$location[j]
    zscore = paste0("zscore_", tolower(zscore_type))
    mean = paste0("mean_", tolower(zscore_type))
    std = paste0("std_", tolower(zscore_type))
    
    Weekday_arv = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday",
      "H/ Monday"
    )[Weekday_arv]
    
    periods <- tibble(
      start = c(0, 10, 30, 60, 120, 180, 360),
      end = c(10, 30, 60, 120, 180, 360, 600),
      period = levels(cut(
        0, c(0, 10, 30, 60, 120, 180, 360, Inf), right = F
      ))
    )

    data_plot <-
      data %>% filter(
        location == Name,
        weekday_arv == Weekday_arv,
        hour_arv == Hour_arv,
        period_cat %in% periods$period[seq(
          which(periods$start == Period_cat[[1]]),
          which(periods$end == Period_cat[[2]])
        )]
      ) %>% 
      group_by(weekday_int,hour_int) %>% 
      summarise(number = sum(number))
    
    data_plot$weekday_int <-
      fct_relevel(
        data_plot$weekday_int ,
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
        "H/ Monday"
      )
    x_levels <- levels(data_plot$weekday_int)
    data_plot$hour_int <-
      factor(data_plot$hour_int ,
             0:23)

    data_plot <- data_plot %>% ungroup() %>% complete(weekday_int,hour_int)
    
    data_plot$weekday_int <- as.integer(data_plot$weekday_int)
    data_plot$hour_int <- as.integer(as.character(data_plot$hour_int))
    
    hweek <-
      ggplot(data_plot , aes(weekday_int, hour_int)) +
      geom_raster(
        aes(
          fill = number,
          text = paste0(
            "</br>",
            hour_int,
            ":00-",
            as.numeric(as.character(hour_int)) + 1,
            ":00 ",
            weekday_int,
            "</br>Hourly aggregate fleet idle time = ",
            round(number, 2)
          )),
          hjust = 0.5,
          vjust = 0.5,
          interpolate = FALSE
        ) +
          scale_fill_gradient2(
            name = "Number",
            low = "blue",
            mid = "white",
            high = "red",
            na.value = "grey50",
          ) +
          coord_cartesian(
            clip = "off") +
          theme_bw() +
          theme(
            plot.margin = margin(20, 2, 2, 2, unit = "pt"),
            plot.title = element_text(vjust = 7, size = 10)
          ) +
          scale_y_continuous(
            breaks = c(0, 4, 8, 12, 16, 20, 23),
            labels = as.character(c(0, 4, 8, 12, 16, 20, 23)),
            limits = c(0,23),
            minor_breaks = c(),
            expand = expansion(add = 0.5)
          ) +
          scale_x_continuous(
            limits = c(1,8),
            breaks = 1:8,
            labels = x_levels,
            minor_breaks = c(),
            expand = expansion(add = .5)
            ) +
          labs(x = "",
               y = "Hour of Day")

        ggplotly(hweek, tooltip = "text") %>% layout(clickmode = "event+select")
  }
