suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyverse))
library(gapminder)
library(leaflet)
library(ggmap)
library(base64enc)


# Storing the labels/values as a tibble means we can use this both 
# to create the dropdown and convert colnames -> labels when plotting
zscore_type <- tibble(label = c("Weekly normalized", "Daily normalized"),
									 value = c("zscore_weekly", "zscore_daily"))

## Make plot

make_map_plot <- function(){
	
	# gets the label matching the column value
	# y_label <- yaxisKey$label[yaxisKey$value==yaxis]
	
	#filter our data based on the year/continent selections
	data <- readRDS("data/map plot/sf_neighborhoods_t.rds")
	google_map <- dataURI(file = "data/map plot/google_map.png")
	bbox_sf <- readRDS("data/map plot/bbox_sf.rds")
	# make the plot!
	
	plot_ly(type = "scatter",data, split = ~location, showlegend = F
	        # ,
	        # hovertemplate = paste0('</br>')
	) %>% layout(clickmode = "event+select") %>% 
	  layout(xaxis = list(range = c((bbox_sf[1,]))),
	         yaxis = list(range = c((bbox_sf[2,])))) %>%
	  # Add trace
	  layout(
	    images = list(
	      list(
	        # Add images
	        # source =  "https://img-prod-cms-rt-microsoft-com.akamaized.net/cms/api/am/imageFileData/RE4wEag?ver=1497",
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
	  )
	
	
	### the customdata mapping adds country to the tooltip and allows
	# its selection using clickData.
	# p_popup <- paste0("<strong>", data$location,"</strong>")
	# 
	# leaflet(data) %>%
	#   addPolygons(
	#     stroke = T,
	#     color = "white",
	#     weight = 1,
	#     opacity = .7,
	#     fillColor = "#b35db8FF",
	#     fillOpacity = 0.8, smoothFactor = 0.5,
	#     popup = p_popup) %>%
	#   addTiles()
	
	# plot <- ggmap(google_map, darken = c(0.4, "white")) +
	#   geom_sf(
	#     data = data,
	#     inherit.aes = FALSE,
	#     aes(fill="pink",
	#         text = paste0("</br>",
	#                       location)),
	#     # alpha = 0.5,
	#     colour = 'white',
	#     size = 0.3,
	#     alpha= 0.6
	#   ) +
	#   # geom_sf_label(data = sf_neighborhoods_t,
	#   #               inherit.aes = FALSE,aes(label = location))+
	#   # geom_sf(
	#   #   data = sf_neighborhoods_t,
	#   #   inherit.aes = FALSE,
	#   #   aes(fill=as.factor(company_car2go)),
	#   #   # alpha = 0.5,
	#   #   colour = 'white',
	#   #   size = 0.3,
	#   #   alpha= 0.6
	#   # ) +
	#   # scale_fill_manual(values = c(scales::alpha("#AADDCC", 0.5), "#FFEEBB")) +
	#   scale_fill_manual(values=c("#b35db8FF")) +
	#   # scale_fill_manual(values = c("magenta", "cyan")) +
	#   # geom_text_repel(data = n7,
	#   #                 aes(x = center_long, y = center_lat, label = location),
	#   #                 size = 1,
	#   #                 # label.padding = 0.35,
	#   #                 hjust=0.5) +
	#   # geom_text_repel(data = n7,
	#   #                 aes(x = center_long, y = center_lat, label = location),
	#   #                 size = 1,
	#   #                 label.padding = 0.15,
	#   #                 point.padding = 0,
	# #                 box.padding = .15,
	# #                 hjust=0.5) +
	# # geom_text(data = n7,
	# #                 aes(x = center_long, y = center_lat, label = location),
	# #                 size = 1,
	# #                 # label.padding = 0.15,
	# #                 hjust=0.5) +
	# # geom_point(data=LongLatToUTM(as.numeric(unlist(evo_data %>% filter(location==Name) %>% dplyr::select(lon))),as.numeric(unlist(evo_data %>% filter(location==Name) %>% dplyr::select(lat)))),aes(X,Y),alpha=0.05,size=0.005)+
	# # scale_x_continuous(name = "Longitude") +
	# # scale_y_continuous(name = "Latitude") +
	# # coord_fixed(ratio = 1) +
	# theme_bw() +
	#   theme(
	#     # legend.position = "none",
	#     panel.grid.major = element_blank(),
	#     panel.grid.minor = element_blank(),
	#     axis.ticks = element_blank(),
	#     axis.text = element_blank(),
	#     axis.title = element_blank(),
	#     legend.position="none"
	#   )
	# 
	# plotly::ggplotly(plot,tooltip = "text")
	# plotly::ggplotly(plot)
# 	p <- ggplot(data, aes(x = year, y = !!sym(yaxis),
# 												colour = continent,
# 												customdata=country)) +
# 		geom_jitter(alpha=0.6) +
# 		scale_color_manual(name = 'Continent', values = continent_colors) +
# 		scale_x_continuous(breaks = unique(data$year))+
# 		xlab("Year") +
# 		ylab(y_label) +
# 		ggtitle(paste0("Change in ", y_label, " over time (Scale : ", scale, ")")) +
# 		theme_bw()
# 
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

make_tile_graph <- function(curve_number=63,zscore = "zscore_weekly"){
	data <- readRDS("data/hourly tile plot/hourly_tile_plot_data.rds")
	# gets the label matching the column value
  locations <-
    data %>% distinct(location) %>% arrange(location)
  j=curve_number-62
  Name <- locations$location[j]
  # zscore = paste0("zscore_",tolower(zscore_type))  
    # temp3 <- gather(
    #   data6_weekday %>%
    #     filter(location == Name,
    #            Company == "evo"),
    #   3:(ncol(data6_weekday)),
    #   key = "hour" ,
    #   value = "zscore"
    # ) %>%
    #   ungroup() %>%
    #   cbind(
    #     gather(
    #       data3_weekday %>%
    #         dplyr::select(-Holiday) %>%
    #         filter(location == Name,
    #                Company == "evo"),
    #       3:(ncol(data6_weekday)),
    #       key = "hour" ,
    #       value = "number"
    #     ) %>%
    #       ungroup() %>%
    #       mutate(zero = ifelse(number == 0, NA, FALSE)) %>%
    #       dplyr::select(zero, number)
    #   ) %>%
    #   mutate(zscore = ifelse(is.na(zero), NA, zscore)) %>%
    #   dplyr::select(-zero)
    # 
    # temp_week <-
    #   factor(
    #     c(
    #       rep("Monday", 24),
    #       rep("Tuesday", 24),
    #       rep("Wednesday", 24),
    #       rep("Thursday", 24),
    #       rep("Friday", 24)
    #     ),
    #     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    #   )
    # temp_hour <- rep(0:23, 5)
    # temp4 <-
    #   cbind(
    #     temp3 %>% dplyr::select(-hour, -location, -Company),
    #     weekday = temp_week,
    #     hour = temp_hour
    #   )
    # 
    #   temp5 <- gather(
    #     data6_holiday %>%
    #       filter(location == Name,
    #              Company == "evo"),
    #     3:(ncol(data6_holiday)),
    #     key = "hour" ,
    #     value = "zscore"
    #   ) %>%
    #     ungroup() %>%
    #     cbind(
    #       gather(
    #         data3_holiday %>%
    #           dplyr::select(-Holiday) %>%
    #           filter(location == Name,
    #                  Company == "evo"),
    #         3:(ncol(data6_holiday)),
    #         key = "hour" ,
    #         value = "number"
    #       ) %>%
    #         ungroup() %>%
    #         mutate(zero = ifelse(number == 0, NA, FALSE)) %>%
    #         dplyr::select(zero, number)
    #     ) %>%
    #     mutate(zscore = ifelse(is.na(zero), NA, zscore)) %>%
    #     dplyr::select(-zero)
    #   temp_week <-
    #     factor(c(rep("H/ Monday", 24), rep("Saturday", 24), rep("Sunday", 24)),
    #            levels = c("H/ Monday", "Saturday", "Sunday"))
    #   temp_hour <- rep(0:23, 3)
    #   temp6 <-
    #     cbind(
    #       temp5 %>% dplyr::select(-hour, -location, -Company),
    #       weekday = temp_week,
    #       hour = temp_hour
    #     )
    #   temp6$weekday <-
    #     fct_relevel(temp6$weekday, "Saturday", "Sunday", "H/ Monday")
    #   
    #   temp7 <- rbind(temp6, temp4)
    
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
    
    
    # temp7_labs <- temp7 %>%
    #   group_by(weekday) %>%
    #   summarise(mean = mean(number, na.rm = TRUE), sd = pop.sd(number)) %>%
    #   left_join(duration %>%
    #               filter(Company == "evo",location == Name)) %>%
    #   mutate(lab = paste0("Mean = ", round(mean / count , 1)),
    #          lab2 = paste0("SD = ", round(sd / count, 1)))
    
    
    
    
    hweek <- 
      ggplot(data_plot , aes(weekday, hour)) +
      geom_raster(
        aes(fill = !!sym(zscore),
            text = paste0("</br>",
                          hour,
                          ":00-",
                          hour+1,
                          ":00 ",
                          weekday,
                          "</br>Hourly aggregate fleet idle time = ",
                          round(number,2),
                          "</br>Z-score = ",
                          round(!!sym(zscore),1))),
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
        # limits = c(-2.2, 2.2),
        # oob = scales::squish
      ) +
      # geom_text(
      #   data = temp7_labs,
      #   aes(weekday, y = Inf, label = lab),
      #   vjust = -1.7,
      #   size = 3
      # )+
      # geom_text(
      #   data = temp7_labs,
      #   aes(weekday, y = Inf, label = lab2),
      #   vjust = -0.4,
      #   size = 3
    # ) +
    coord_cartesian(expand = FALSE, clip = "off") +
      theme_bw() +
      # labs(title = str_c("Distribution of Hourly Aggregate Fleet Idle Time in ", Name,", Evo Fleet"))+
      theme(plot.margin = margin(20, 2, 2, 2, unit = "pt"),plot.title = element_text(vjust=7,size=10)) +
      # theme(plot.margin = margin(20, 2, 2, 2, unit = "pt")) +
      scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 23),
                         labels = as.character(c(0, 4, 8, 12, 16, 20, 23)))+
      labs(x="",
           y="Hour of Day"
      )
    
    
    # ggsave(str_c("Figure/evo_",Name,"_idle car distribution.png"),width = 7.3,height = 7.3)  
  
  
  plotly::ggplotly(hweek, tooltip = "text")
}
