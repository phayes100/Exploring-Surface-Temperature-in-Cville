library(raster) # reading in and wrangling landsat data
library(sf) # reading in and wrangling contextual data
library(tidyverse)
library(geojsonio)
library(tidycensus) # connect and return ACS and Decennial tibbles
library(ggpubr)
library(gstat)
library(rgdal)
library(scales)
library(RColorBrewer)

# Load geojson files of temperature data from Regional Equity Atlas as spatial dataframes
# https://equity-atlas-uvalibrary.opendata.arcgis.com/search?tags=
morning_temps <- st_read("Urban_Heat_Data_2021_Morning_Traverse.geojson")
afternoon_temps <- st_read("Urban_Heat_Data_2021_Afternoon_Traverse.geojson")
evening_temps <- st_read("Urban_Heat_Data_2021_Evening_Traverse.geojson")
nhoods <- st_read("Planning_Neighborhood_Area.geojson")


# api call for getting median hh income
  
tracts <- get_acs(geography = "tract",
                          variables = c(med_hh_inc = "S1901_C01_012", pct_white = "DP05_0037P", pct_over_65 = "DP05_0024P"), 
                          state = "51", 
                          county = c("540"),
                          year = c(2020),
                          geometry = F)
# pivot wider for separate variable columns 
tracts <- tracts %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe))


# need to do a separate api call to get the geometry column and join to tracts data
geom <- get_acs(geography = "tract",
                variables = c(med_hh_inc = "S1901_C01_012"),
                state = "51", 
                county = c("540"),
                year = c(2020),
                geometry = T)
# join to tracts to add geometry column and select only relevant variables
tracts <- left_join(tracts, geom) %>% 
  mutate(pct_poc = 100 - estimate_pct_white) #%>% # get % of POC (pct of people not White, one race) 
  #select(GEOID, NAME, estimate_med_hh_inc, estimate_pct_over_65, estimate_pct_white, pct_poc, geometry) 
  
tracts <- st_sf(tracts) # geom_sf needs the df to be an sf object


# set the crs for each st table to 4269 to match tracts to allow st_join
morning_temps <- st_set_crs(morning_temps, 4269)
afternoon_temps <- st_set_crs(afternoon_temps, 4269)
evening_temps <- st_set_crs(evening_temps, 4269)
nhoods <- st_set_crs(nhoods, 4269)

# join to assign tract data to each corresponding temperature record
joined <- st_join(tracts, afternoon_temps)

# visualize relationship b/t temperature and hh income
ggscatter(joined, x = "temp_f", y = "estimate_med_hh_inc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature(F)", ylab = "Median HH Income")

# record correlation coeff of income and temperature
income_cor <- cor(joined$estimate_med_hh_inc, joined$temp_f, use = "complete.obs")


tracts_with_temps <- joined %>% 
  group_by(GEOID, estimate_med_hh_inc, estimate_pct_over_65, pct_poc) %>% 
  summarise("AVG Temp" = mean(temp_f))



names(tracts_with_temps)[names(tracts_with_temps) == 'estimate_med_hh_inc'] <- 'Median Household Income'

tracts_with_temps$`Median Household Income` <- as.double(tracts_with_temps$`Median Household Income`)

tracts_with_temps <- st_sf(tracts_with_temps)





nhoods_with_temps <- st_join(nhoods, afternoon_temps) %>% 
  group_by(NAME) %>% 
  summarise("Temp" = mean(temp_f))


ggplot() +
  geom_sf(data = nhoods_with_temps, aes(fill = Temp), show.legend = T) + 
  scale_fill_gradient(low = "blue", high = "red") +
  geom_sf_text(data = nhoods_with_temps, aes(label = NAME)) +
  theme_void()

ggplot() +
  geom_sf(data = tracts_with_temps, aes(fill = `Median Household Income`), show.legend = T)  +
  scale_fill_gradientn(colours = colorspace::heat_hcl(7),
                       values=rescale(c(20000,70000,100000)),
                       limits = c(20000, 96300),
                       labels = scales::dollar_format()) +
  theme_void()

ggplot() +
  geom_sf(data = tracts_with_temps, aes(fill = `AVG Temp`), show.legend = T) +
  scale_fill_gradientn(colours = rev(colorspace::heat_hcl(7)), name = "Average Temperature\n (?F)") +
  theme_void()


aft_temps <- afternoon_temps %>%
  mutate(lat = unlist(map(afternoon_temps$geometry,1)),
         long = unlist(map(afternoon_temps$geometry,2)))


rast <- raster("n38_w079_1arc_v3.tif")


#rdf <- as.data.frame(rast, xy = T)

#rdf <- rdf %>% 
  #filter(x > min(aft_temps$lat) & x < max(aft_temps$lat))

pts <- geojson_read("Urban_Heat_Data_2021_Afternoon_Traverse.geojson", what = "sp")

aft_temps$z <- raster::extract(rast, pts)

aft_temps_df <- as.data.frame(aft_temps)

sp_temps <- sp::SpatialPointsDataFrame(
  coords = aft_temps_df[,c("lat", "long")],
  data = aft_temps_df[,c("temp_f", "lat", "long")],
  proj4string = CRS("+init=epsg:4326")
)

city_bounds <- rgdal::readOGR(dsn='City Boundaries\\SHP', layer = 'municipal_boundary_area_09_03_2020')

city_bounds <- spTransform(city_bounds, CRS(proj4string(rast)))

fitmax <- gstat::gstat(formula = temp_f ~ 1, data = sp_temps, nmax = 2, set = list(idp = 2.0))

mean_temp <- raster::interpolate(rast, model = fitmax, ext = city_bounds)

#plot(mean_temp)

fmean <- function(x) mean(x, na.rm = T)

mean_temp_m <- raster::focal(mean_temp, w = matrix(1, 19, 19), fmean, pad = T)

mean_temp_m <- raster::mask(mean_temp_m, city_bounds)

plot(mean_temp_m, col=rev(brewer.pal(n = 11, name = 'RdBu')), ext = city_bounds, box = FALSE, axes = FALSE, main = 'Afternoon Area-Wide')
for(i in 1:nrow(nhoods)) plot(nhoods[i,]$geometry, border=rgb(0,0,0,alpha=0.12), lwd=2, add=TRUE)

df_temp <- as.data.frame(mean_temp_m, xy = T)
colnames(df_temp) <- c("x", "y", "Temperature")


ggplot() +
  geom_tile(data = df_temp, aes(x = x, y = y, fill = Temperature)) +
  scale_fill_distiller(palette = 'RdBu', na.value = "white") +
  geom_sf(data = nhoods_with_temps, show.legend = T, fill = NA) +
  geom_sf_text(data = nhoods_with_temps, aes(label = NAME), alpha = 0.5, size = 2.5) +
  theme_void()
  


