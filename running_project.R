setwd("C:\\Users\\Plush\\OneDrive\\Desktop\\Runs\\")


library(tidyverse)
library(ggmap)
library(XML)
library(methods)
library(stringr)
library(gganimate)
library(gapminder)
library(ggthemes)
library(lubridate)
library(RColorBrewer)
library(ggimage)
library(png)

#creating a function to read in all my runs more efficiently
read_in_LJ <- function(number) {
  
  #gets the file path of a run at Lake Johnson
  path <- xmlParse(str_c('run', (number), 'LJ.tcx'))
  #gets the nodes for the tcx to read it in
  nodes <- getNodeSet(path, "//ns:Trackpoint", "ns")
  #saves and returns the data with na values removed
  dataset <- na.omit(xmlToDataFrame(nodes))
  #returning the list of unnamed runs 
  return(dataset)
  
}


#function to name all of the read in data sets something useful
naming_LJ <- function(run_list, prefix = 'run_') {
  
  #gives every run a name of "run_(number)" and returns the list with each run named
  names(run_list) <- str_c(prefix, seq_along(run_list))
  #returning the named list
  return(run_list)
  
}


#this carries out all the cleaning needed for a run data frame
clean_LJ_data <- function(run_df) {
  
  #setting the number of decimal places used in the data frame
  options(digits = 14)
  #cleaning the longitude and latitude variables
  #splitting the position variable to separate longitude and latitude
  run_df[c('Latitude_Chr', 'Longitude_Chr')] <- str_split_fixed(run_df$Position, '-', 2)
  #converting longitude and latitude to numeric
  run_df$Longitude <- as.numeric(run_df$Longitude_Chr)*(-1)
  run_df$Latitude <- as.numeric(run_df$Latitude_Chr)
  #cleaning up the time variable
  run_df$Time <- as.POSIXct(run_df$Time, format = '%Y-%m-%dT%H:%M:%OS')
  run_df$Time <- as.POSIXct(as.numeric(run_df$Time - 5*3600), origin = '1970-01-01')
  #creating a variable that states what minute in the run it is
  run_df$Minute <- as.integer(as.numeric(run_df$Time - min(run_df$Time)) / 60)
  
  #cleaning up the altitude variable
  run_df$Altitude <- as.numeric(run_df$AltitudeMeters)
  
  #creating a variable that states if my altitude has increased or decreased since the last measurement
  differences <- diff(run_df$Altitude)
  #I means it has increased, D means it has decreased, N means it has stayed the same.
  inc_dec <- c(0, ifelse(differences > 0, "I", ifelse(differences < 0, "D", "N")))
  run_df$Altitude_Change <- inc_dec
  
  #selecting the final variables
  run <- run_df %>% 
    select(Longitude, Latitude, Minute, Altitude, Time, Altitude_Change)
  run
  #returning final cleaned df list
  return(run)
  
}


#reading in my data
run_list <- lapply(1:8, read_in_LJ)
run_list <- naming_LJ(run_list)
run_list <- lapply(run_list, clean_LJ_data)

#creating a function to combine all the data frames into one data frame
combine_runs <- function(df_list) {
  
  #creating an empty data frame to combine the data frames into
  final_df <- data.frame()
  #using a for loop to row bind the each data frame in the list
  for (df in df_list) {
    final_df <- rbind(final_df, df)
  }
  
  
  return(final_df)
}

#combining all the runs into one data frame for a later gif
combined <- combine_runs(run_list)
combined

#setting the API key to use for ggmaps
#deleted API key for display purposes
key <- 'xxx'
register_google(key)


#creating the map background that will be plotted on
run_map <- get_map(location = c(mean(run_list[[1]]$Longitude), mean(run_list[[1]]$Latitude)), maptype = 'terrain', zoom = 15)


#setting image for my run gif
N_image <- "https://www.pinclipart.com/picdir/big/115-1154923_clip-art-transparent-stick-figure-icon-png-for.png"
I_image <- "red_stickfigure.png"
D_image <- "blue_stickfigure.png"


#creating a data frame for my first gif
first_run <- run_list$run_1 %>% 
  mutate(Image = ifelse(Altitude_Change == "I", I_image, ifelse(Altitude_Change == "D", D_image, N_image))) %>% 
  select(Time, Longitude, Latitude, Image)



#plotting my run onto the map
map <- ggmap(run_map) + 
  geom_point(data = first_run, aes(x = Longitude, y = Latitude), size = 2) +
  labs(x = "Longitude", y = "Latitude", title = "Animation of My First Run at Lake Johnson, March 3, 2023", caption = "Created by Robbie Goss | Data from mapmyfitness.com | Collected with MapMyWalk App", ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) + 
  geom_image(data = first_run, aes(x = Longitude, y = Latitude, image = Image), size = 0.05)

#animating my route onto the map
map.animation <- map + 
  transition_states(first_run$Time)

#creating the animation
first_run <- animate(map.animation)
first_run

#saving the gif
save_animation(first_run, 'first_run_altitude_change.gif')


#creating a gif that goes through all the routes of my runs
combined_plotter <- combined %>% 
  mutate(Date = as.factor(floor_date(Time, unit = "day"))) %>% 
  group_by(Date)


#creating the map to plot my different runs on
combined_map <- get_map(location = c(mean(combined_plotter$Longitude), mean(combined_plotter$Latitude)), maptype = 'terrain', zoom = 15)

#creating the map with all of my runs plotted
map2 <- ggmap(run_map) + 
  geom_point(data = combined_plotter, aes(x = Longitude, y = Latitude, color = Date), size = 0.2) + 
  scale_color_brewer(palette = "OrRd", guide = "none") +
  labs(x = "Longitude", y = "Latitude", title = "Animation of All of My Runs at Lake Johnson to Date", caption = "Created by Robbie Goss | Data from mapmyfitness.com | Collected with MapMyWalk App") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


#creating the animation transitioning between runs
map2.animation <- map2 +
  transition_states(as.Date(combined_plotter$Date))

#creating the animation
combined_runs <- animate(map2.animation, nframes = 100)
combined_runs

#saving the gif
save_animation(combined_runs, 'combined_runs.gif')