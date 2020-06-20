##
## Tutorial: make animated histograms like https://infographics.economist.com/2017/first-child-age/
## 


# install.packages(c("eurostat", "dplyr","ggplot2","gganimate", "gifski", "png"))
library(eurostat) # for easy access to the source 
library(dplyr) # for manipulating data, like filtering and summarising data
library(ggplot2)
library(gganimate) # for animating the ggplot visualization
library(gifski) #  for rendering the animation to a gif file
library(png) # for reading and writing png files, which is also needed during rendering


## The color palette
cols <- c("#71445E", "#9B292E", "#235889")

## The selected countries. Germany, France and Sweden are not included here, because they have missing data for some years
country <- c("CZ", "EE", "PL", "RO", "BG", "HU", "EL", "ES", "PT", "BE", "NL", "AT")
countryname <- c("Czechia", "Estonia", "Poland", "Roumania", "Bulgaria", "Hungary", "Greece", "Spain", "Portugal", "Belgium", "Netherlands", "Austria")
region <- c("Eastern Europe", "Eastern Europe", "Eastern Europe", "Eastern Europe", "Eastern Europe", "Eastern Europe", "Southern Europe", "Southern Europe", "Southern Europe", "Western Europe", "Western Europe", "Western Europe")

countries <- data.frame(country, countryname, region)

## Static histogram
## Get the full data set from Eurostat. This data set contains the age of women at which they had their first, second, third and fourth child, for all EU countries over time
fertility.order.age <- get_eurostat("demo_fordagec", time_format = "num")

## Create a vector to filter out the ages classes for single year ages. The age classes are encoded as "Y16", "Y17", ... 
ages <- sprintf("Y%s",16:49)

## Filter out the relevant data. ord_brth == 1 are the rows containing the age of women at which they first gave birth
mother.age <- filter(fertility.order.age, 
                     ord_brth == 1,
                     geo %in% countries$country,
                     time == 1995 | time == 2018,
                     age %in% ages)

## Remove the "Y" from the age classes and convert to a number
mother.age$age <- as.numeric(gsub("Y", "", mother.age$age))

mother.age.perc <- group_by(mother.age, time, geo) %>%
  ## Calculate the percentages for all age classes, in each year in each country
  mutate(perc = round(values/sum(values)*100,1)) %>%
  ## Join the country names and regions
  left_join(countries, by = c("geo" = "country")) %>%
  ## Order the levels of the countrynames, so that they appear in the right order in the facetted plot
  mutate(countryname = factor(countryname, levels = countries$countryname))

## Subset of the values for 2018
mother.age.perc.18 <- filter(mother.age.perc, time == 2018)
## Get the highest values for each country in 2018, to add as labels on the histograms
mode.18 <- group_by(mother.age.perc.18, geo) %>%
  ## Get the highest value
  top_n(1, perc) %>%
  ## Remove the duplicates (those are present because there are some ties). .keep_all makes sure all columns are kept
  distinct(perc, .keep_all = TRUE)

## Subset of the values for 1995, to draw the silhouette histograms
mother.age.perc.95 <- filter(mother.age.perc, time == 1995)

## The ggplot
## An cheap trick to simulate the graduated effect of the bars in the original is to map the percentages to alpha
histograms <- ggplot(mother.age.perc.18, aes(x = age, y = perc, fill = region, alpha = perc)) +
  geom_col() +
  ## The silhouettes with the data for 1995
  geom_col(data = mother.age.perc.95, color = "#000000", alpha = 0, size = 0.1) +
  ## The text labels with the highest percentage for each country
  geom_text(data = mode.18, aes(label = paste(perc, "%", sep="")), vjust = 0, nudge_y = 1, hjust = 0.5) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    ## Make the the title of each plot bold and right aligned with hjust
    strip.text = element_text(size = 12, face = "bold", hjust = 1)
  ) +
  xlab("") + ylab("") +
  ## The colors for each region
  scale_fill_manual(values = cols) +
  ## Breaks on the x axis are 5 years apart
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  ## Facet the plot by countryname to get a small multiple for each country. The x scales are independent of each other, so they are repeated for each small multiple. Because the input data for x (the ages) are identical for all countries, the x scales are also the same
  facet_wrap(~countryname, ncol = 3, scales = "free_x") +
  ggtitle('Thirty is the new twenty', subtitle = 'Ages of women at first birth, selected countries, 1995 vs 2018')
  
ggsave("histograms-static.png", plot = histograms, units = "cm", width = 18, height = 24)
histograms + transition_states(time)

### Animated
mother.age.all <- filter(fertility.order.age, 
                     ord_brth == 1,
                     geo %in% countries$country,
                     ## Instead of only 1995 and 2018, the animation needs the data for all years
                     time > 1994,
                     age %in% ages)

mother.age.all$age <- as.numeric(gsub("Y", "", mother.age.all$age))

mother.age.all.perc <- group_by(mother.age.all, time, geo) %>%
  mutate(perc = round(values/sum(values)*100,1)) %>%
  left_join(countries, by = c("geo" = "country")) %>%
  mutate(countryname = factor(countryname, levels = countries$countryname))

mode.all <- group_by(mother.age.all.perc, geo, time) %>%
  top_n(1, perc) %>%
  ## Remove the duplicates (ties) for each combination of country (geo) and year
  distinct(geo, time, perc, .keep_all = TRUE)

## Subset of the data for the year 1995
mother.age.perc.95 <- filter(mother.age.all.perc, time == 1995)

## Construct a dataframe that contains the data for 1995 duplicated for each year between 1995 and 2018. In that way, you can draw the silhouettes for the 1995 histogram on all frames of the dataset
mother.age.perc.95.all <- mother.age.perc.95
for(yr in 1996:2018){
  yrdata <- mother.age.perc.95
  yrdata$time <- yr
  mother.age.perc.95.all <- rbind(mother.age.perc.95.all, yrdata)
}

walking.histograms <- ggplot(mother.age.all.perc, aes(x = age, y = perc, fill = region, alpha = perc)) +
  geom_col() +
  geom_col(data = mother.age.perc.95.all, color = "#000000", alpha = 0, size = 0.1) +
  geom_text(data = mode.all, aes(label = paste(perc, "%", sep="")), vjust = 0, nudge_y = 1, hjust = 0.5) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold", hjust = 1)
  ) +
  xlab("") + ylab("") +
  scale_fill_manual(values = cols, name = "") +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  facet_wrap(~countryname, ncol = 3, scales = "free_x") +
  ## Only these last two functions use gganimate. Note the {closest_state} in the subtitle: this will contain the year for the current frame in the animation
  ggtitle('Thirty is the new twenty', subtitle = 'Ages of women at first birth, selected countries, 1995 vs {closest_state}') +
  ## transition_states specifies which column in the data should be used for the animation
  transition_states(time)

## Use animate() to render the animation in the Viewer. This will take a while
animate(walking.histograms)

## Use anim_save to save an animation. The animation to save can be a gganimate object (like below), it can be the last rendered animation (this is the default), or it can be a rendered gganimate object
anim_save("final-animation.gif", animation = walking.histograms, width = 900, height = 1200, res = 150)

## animate() has some options to control the animation. If you want a smoother animation, you can set the number of frames per second to a higher value (default is 10), you can set the total duration (default is 10), set the number of frames the first and last frame should be repeated. With the rewind option, you can rewind the animation
animate(walking.histograms, fps = 20, start_pause = 20, end_pause = 20, rewind = TRUE)

## You can render an animation first, and then save it
rendered.animation <- animate(walking.histograms, fps = 20, duration = 8, start_pause = 5, end_pause = 5, rewind = TRUE, width = 900, height = 1200, res = 150)
anim_save(filename = "rewindinghistograms.gif", animation = rendered.animation)
