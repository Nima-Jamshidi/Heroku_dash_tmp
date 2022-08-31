suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyverse))
# library(gapminder)
# library(leaflet)
# library(ggmap)
library(base64enc)

text_template = "updatemode (a value equal to: 'mouseup' or 'drag'; default 'mouseup'): Determines when the component should update its value property. If mouseup (the default) then the slider will only trigger its value when the user has finished dragging the slider. If drag, then the slider will update its value continuously as it is being dragged. If you want different actions during and after drag, leave updatemode as mouseup and use drag_value for the continuously updating value.

vertical (boolean; optional): If True, the slider will be vertical.

verticalHeight (number; default 400): The height, in px, of the slider if it is vertical.

className (string; optional): Additional CSS class for the root DOM node.

id (string; optional): The ID of this component, used to identify dash components in callbacks. The ID needs to be unique across all of the components in an app.

loading_state (dict; optional): Object that holds the loading state object coming from dash-renderer.

loading_state is a dict with keys:

component_name (string; optional): Holds the name of the component that is loading.

is_loading (boolean; optional): Determines if the component is loading or not.

prop_name (string; optional): Holds which property is loading.

persistence (boolean | string | number; optional): Used to allow user interactions in this component to be persisted when the component - or the page - is refreshed. If persisted is truthy and hasn't changed from its previous value, a value that the user has changed while using the app will keep that change, as long as the new value also matches what was given originally. Used in conjunction with persistence_type."


map_text = list("For my graduate research project I had access to Evo carsharing service's fleet data between February 2017 and August 2018. 
                Evo is a free-floating (+ station-based) carsharing service that has been operating in Vancouver, BC since March 2015.
                The other carsharing services in Vancouver at the time of the study belonged to Car2Go, 
                also free-floating but ceased to continue operating in 2020, and Modo, a co-op providing round-trip carsharing service.",htmlBr(),
                "During the data collection period the fleet consisted of 1050 Toyota Prius Hybrids on average. 
                The data had information about the location of each carsharing car while they were idle and not reserved by the users 
                with a 5-minute frequency. After wrangling the data, we were able to identidy trips and cancelled reservations. 
                On average an Evo car was used 5.8 times per day with each trip (+ reservation period) averageing around an hour in duration.",
                htmlBr(),"In a part of my thesis I explored the spatio-temporal usage of the cars and through that identified usage patterns
                among different neighbourhoods. The usage of the carsharing service follows the general daily pattern of other transportation 
                modes with peaks during the commute hours. But how does it plays out in the service boundary?",
                htmlBr(),"Here You can find 57 neighborhoods throughout Metro Vancouver in which Evo has been present. 
                Hover over the map and see the neighbourhoods' names.",htmlBr(),"In order to show the usage pattern in the neighbourhoods,
                the periods that a car was idle in a location waiting for a user to book and drive it are analyzed.",
                htmlBr(),"If you click on a neighbourhood, you can see the hourly aggregate fleet idle time distribution figure below.
                That is the number of vehicles idle in that neighbourhood during any hour of the day of the week.
                To be more precise, the aggregate idle time (hour unit) of vehicles for each hour is calculated.")


map_text = list(htmlP("For my graduate research project I had access to Evo carsharing service's fleet data between February 2017 and August 2018. 
                Evo is a free-floating (+ station-based) carsharing service that has been operating in Vancouver, BC since March 2015.
                The other carsharing services in Vancouver at the time of the study belonged to Car2Go, 
                also free-floating but ceased to continue operating in 2020, and Modo, a co-op providing round-trip carsharing service."),
                htmlP("During the data collection period the fleet consisted of 1050 Toyota Prius Hybrids on average. 
                The data had information about the location of each carsharing car while they were idle and not reserved by the users 
                with a 5-minute frequency. After wrangling the data, we were able to identidy trips and cancelled reservations. 
                On average an Evo car was used 5.8 times per day with each trip (+ reservation period) averageing around an hour in duration."),
                htmlP("In a part of my thesis I explored the spatio-temporal usage of the cars and through that identified usage patterns
                among different neighbourhoods. The usage of the carsharing service follows the general daily pattern of other transportation 
                modes with peaks during the commute hours. But how does it plays out in the service boundary?"),htmlP("Here You can find
                57 neighborhoods throughout Metro Vancouver in which Evo has been present. 
                Hover over the map and see the neighbourhoods' names. In order to show the usage pattern in the neighbourhoods,
                the periods that a car was idle at a location waiting for a user to book and drive it are analyzed."),
                htmlP("If you click on a neighbourhood, you can see the hourly aggregate fleet idle time distribution (HAFIT) figure below.
                That is the number of vehicles idle in that neighbourhood during any hour of the day of the week.
                To be more precise, the aggregate idle time (hour unit) of vehicles for each hour is calculated."))


tile_text = list(
  htmlP(
    'HAFIT, tells you when and where cars get piled up in the morning, leave in the afternoon,
                       and where the parties happen on the weekend nights! The system on the long weekend Mondays would experience a usage
                       pattern different from regular weekday Mondays, hence a separate column for "H/ Mondays".
                       Hover over the figure to see HAFIT and the respective normalized values for each hour of the week.
                       The coloring of the figure is to convey when the number of idle cars are high (red) or low (blue).'
  ),
  htmlP(
    list(
      "If you click on ",
      htmlA(
        id = "Downtown Vancouver link",
        children = "Downtown Vancouver",
        href = "#!"
      ),
      ", you can see that this neighbourhood is a popular destination for carsharing users in the morning on weekdays.
                             The HAFIT level stays the same until afternoon around 4PM when the majority of the cars are taken by the users gradually
                             and leave the neighbourhood. The neighbourhood is not as popular in the weekends with lighter red colors.
                             To have better view of the daily patterns, you can change the settings of the figure to show the daily
      normalized data instead of weekly. This normalization helps better with identifying neighbourhood classes.
      Opposed to Downtown Vancouver, ",
      htmlA(
        id = "False Creek link",
        children = "False Creek",
        href = "#!"
      ),
      " on Friday nights is as busy as weekday middays. 
      The daily normalization show a more similar pattern for False Creek and Downtown Vancouver. 
      Both being destinations for work during weekdays and entertainment on weekend nights. The result of a clustering analisys 
      on these figures are presented in my thesis."
    )
  )
)
                 


zscore_type <- tibble(label = c("Weekly normalized", "Daily normalized"),
									 value = c("weekly", "daily"))


periods <- tibble(label = c("0 min","10 min","30 min","1 hr","2 hr","3 hr","6 hr","6+ hr"),
                  value = c(0,10,30,60,120,180,360,600)) 

periods_list = as.list(periods$label)
names(periods_list) = periods$value

periods_list = list(`0` = list(label = "0 min", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `10` = list(label = "10 min", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `30` = list(label = "30 min", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `60` = list(label = "1 hr", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `120` = list(label = "2 hr", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `180` = list(label = "3 hr", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `360` = list(label = "6 hr", style = list(transform = "translateX(-50%) translateY(20%) rotate(-90deg)")),
                    `600` = list(label = "6+ hr", style = list(transform = "translateX(-100%)")))

# periods_list = list(`0` = list(label = "0 min"),
#                     `10` = list(label = "10 min"),
#                     `30` = list(label = "30 min"),
#                     `60` = list(label = "1 hr"),
#                     `120` = list(label = "2 hr"),
#                     `180` = list(label = "3 hr"),
#                     `360` = list(label = "6 hr"),
#                     `600` = list(label = "6+ hr"))


make_map_plot <- function() {
  data <- readRDS("data/map plot/sf_neighborhoods_t.rds")
  google_map <- dataURI(file = "data/map plot/google_map.png")
  bbox_sf <- readRDS("data/map plot/bbox_sf.rds")
  
  # plot_ly(type = "scatter",data, split = ~location, showlegend = F,
  #         hoverlabel = list(namelength = 0)
  # ) %>% layout(clickmode = "event+select") %>%
  #   layout(xaxis = list(range = c((bbox_sf[1,]))),
  #          yaxis = list(range = c((bbox_sf[2,])))) %>%
  #   layout(
  #     images = list(
  #       list(
  #         source =  google_map,
  #         xref = "x",
  #         yref = "y",
  #         x = bbox_sf[1,1],
  #         y = bbox_sf[2,2],
  #         sizex = bbox_sf[1,2]-bbox_sf[1,1],
  #         sizey = bbox_sf[2,2]-bbox_sf[2,1],
  #         sizing = "stretch",
  #         opacity = 0.4,
  #         layer = "over"
  #       )
  #     )
  #   ) %>%
  #   highlight(on = "plotly_click", color = "blue")
  #
  
  # plot_ly(type = "scatter",data, split = ~location, showlegend = F,
  #         hoverlabel = list(namelength = 0),
  #         width=800, height=495
  # ) %>% layout(clickmode = "event+select") %>%
  #   layout(xaxis = list(range = c((bbox_sf[1,]))),
  #          yaxis = list(range = c((bbox_sf[2,])))) %>%
  #   layout(
  #     images = list(
  #       list(
  #         source =  google_map,
  #         xref = "x",
  #         yref = "y",
  #         x = bbox_sf[1,1],
  #         y = bbox_sf[2,2],
  #         sizex = bbox_sf[1,2]-bbox_sf[1,1],
  #         sizey = bbox_sf[2,2]-bbox_sf[2,1],
  #         # sizing = "stretch",
  #         opacity = 0.4,
  #         layer = "over"
  #       )
  #     )
  #   ) %>%
  #   layout(margin=list(
  #     l=0,
  #     r=0,
  #     b=0,
  #     t=0,
  #     pad=0
  #   ))
  
  
  plot_ly(
    type = "scatter",
    data,
    split = ~ location,
    showlegend = F,
    hoverlabel = list(namelength = 0)#,
    # width=800, height=495
  ) %>% layout(clickmode = "event+select") %>%
    layout(xaxis = list(range = c((bbox_sf[1,]))),
           yaxis = list(range = c((bbox_sf[2,])))) %>%
    layout(images = list(
      list(
        source =  google_map,
        xref = "x",
        yref = "y",
        x = bbox_sf[1, 1],
        y = bbox_sf[2, 2],
        sizex = bbox_sf[1, 2] - bbox_sf[1, 1],
        sizey = bbox_sf[2, 2] - bbox_sf[2, 1],
        sizing = "stretch",
        opacity = 0.4,
        layer = "over"
      )
    )) %>%
    layout(margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    ))
}


make_tile_graph <- function(curve_number=80,zscore_type = "weekly"){
	data <- readRDS("data/hourly tile plot/hourly_tile_plot_data.rds")
	
  locations <-
    data %>% distinct(location) %>% arrange(location)
  j=curve_number-56
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
                          Name,
                          "</b>",
                          "</br><b>",
                          hour,
                          ":00-",
                          hour+1,
                          ":00 ",
                          weekday,
                          "</b></br>Hourly aggregate fleet idle time = ",
                          sprintf("%.1f",round(number,1)),
                          "</br>    Z-score = ",
                          sprintf("%.1f",round(!!sym(zscore),1)),
                          "</br>    mean = ",
                          sprintf("%.1f",round(!!sym(mean),1)),
                          "</br>    std = ",
                          sprintf("%.1f",round(!!sym(std),1)))),
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
      )+
    theme(axis.text.x = element_text(
      angle = 45,
      # vjust = 1,
      # size = 5,
      # hjust = 0
      ))

  plotly::ggplotly(hweek, tooltip = "text")  %>% layout(clickmode = "event+select",
                                                        xaxis = list(fixedrange = TRUE), 
                                                        yaxis = list(fixedrange = TRUE),
                                                        title=Name) #%>% 
    # config(displayModeBar = FALSE,
    #        modeBarButtonsToRemove = c('zoom2d','pan2d','hoverClosestGl2d','lasso2d'))
}

make_arrival_tile_graph <-
  function(curve_number = 80,
           zscore_type = "weekly",
           Weekday_arv = 2,
           Hour_arv = 8,
           Period_cat = list(60,360)) {
    data <- readRDS("data/arrival tile plot/arrival_tile_plot_data.rds")
  
    locations <-
      data %>% distinct(location) %>% arrange(location)
    j = curve_number - 56
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
    
    color_limits <- data %>% filter(location == Name) %>% group_by(weekday_arv, hour_arv, weekday_int,hour_int) %>% summarise(number = sum(number))
    color_limits <- c(0,max(color_limits$number))
    
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
          text = paste0("</br><b>",
                        Name,
                        "</b>",
                        "<b></br>",
                        hour_int,
                        ":00-",
                        hour_int + 1,
                        ":00 ",
                        x_levels[weekday_int],
                        "</b></br>Hourly aggregate fleet idle time = ",
                        sprintf("%.2f",round(number, 2))
          )),
          hjust = 0.5,
          vjust = 0.5,
          interpolate = FALSE,
          color = "grey"
        ) +
          # scale_fill_gradient2(
          #   name = "Number",
          #   low = "blue",
          #   mid = "white",
          #   high = "red",
          #   na.value = "grey50",
          #   limits = color_limits
          # ) +
      scale_fill_gradient(
        name = "Number",
        low = "#DCDCDC",
        # mid = "white",
        high = "#FFFF00",
        na.value = "grey50",
        limits = color_limits
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
               y = "Hour of Day")+
      theme(axis.text.x = element_text(
        angle = 45,
        # vjust = 1,
        # size = 5,
        # hjust = 0
      ))

        ggplotly(hweek, tooltip = "text") %>% layout(clickmode = "event+select",
                                                     title=paste0(
                                                       Name,
                                                       # "</b>",
                                                       "<br>",
                                                       "Arrivals between ",
                                                       Hour_arv,
                                                       "-",
                                                       Hour_arv + 1,
                                                       ":00 ",
                                                       Weekday_arv
                                                     ),
                                                     margin = list(
                                                       l = 0,
                                                       r = 0,
                                                       b = 0,
                                                       t = 70,
                                                       pad = 0
                                                     ),
                                                     xaxis = list(fixedrange = TRUE), 
                                                     yaxis = list(fixedrange = TRUE)) #%>% 
          # config(displayModeBar = F,
          #        modeBarButtonsToRemove = list('zoom2d','pan2d','hoverClosestGl2d','lasso2d'),
          #        displaylogo = F)
  }
