
# title: "CO emission & CO2 emission - Industrial production level assessment"


  
# 1. Data Visualization - CO/CO2 emission ratio
# Set the working directory for emission dataset
CO2_emission_work_path <- "D:/19. PhD-原始数据-2022/13. CO2 data/CO2 Emission Inventory - Tsinghua University/CO2 - Yearly"
CO_emission_work_path <- "D:/19. PhD-原始数据-2022/13. CO2 data/CO2 Emission Inventory - Tsinghua University/CO - Yearly"

# Upload the useful library
library(sp)
library(raster)
library(reshape)
library(ggplot2)
library(rgdal)
library(dplyr)

# Raster stacking & geographical coordination setting
CO2_emission_file <- list.files(CO2_emission_work_path, full.names = TRUE, pattern = ".asc") # ESRI grid data
CO_emission_file <- list.files(CO_emission_work_path, full.names = TRUE, pattern = ".asc") # ESRI grid data

CO2_emission_stack <- stack(CO2_emission_file)
CO_emission_stack <- stack(CO_emission_file)

crs(CO2_emission_stack) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
crs(CO_emission_stack) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"

# Upload shape-file of administrative boundary of China
admin_boundary <- readOGR("D:/19. PhD-原始数据-2022/8. 中国行政边界/全国数据shp格式/Lat_Long.shp")
admin_boundary_2 <- readOGR("D:/19. PhD-原始数据-2022/8. 中国行政边界/Province_2.shp")

admin_boundary <- spTransform(admin_boundary, CRS("+init=epsg:4326"))
admin_boundary_2 <- spTransform(admin_boundary_2, CRS("+init=epsg:4326"))

CO2_emission_stack <- crop(CO2_emission_stack, extent(admin_boundary), snap = "out")
CO_emission_stack <- crop(CO_emission_stack, extent(admin_boundary), snap = "out")

CO_emission_stack <- stack(CO_emission_stack)
CO2_emission_stack <- stack(CO2_emission_stack)

CO_CO2_emission_ratio_stack <- CO_emission_stack / CO2_emission_stack
CO_CO2_emission_ratio_stack <- stack(CO_CO2_emission_ratio_stack)
CO_CO2_emission_ratio_stack

# To generate the dataframe of each raster stacks
CO_CO2_emission_ratio_stack_df <- as.data.frame(CO_CO2_emission_ratio_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))
CO_CO2_emission_ratio_stack_df

# To summarize the statistic characteristic of CO_CO2_ratio raster stack
summary(CO_CO2_emission_ratio_stack_df)

quantile(CO_CO2_emission_ratio_stack_df$value, probs = c(.25, .5, .75, .80, .85, .90, .95, .99, .998, .999, 1), na.rm = TRUE)

CO_CO2_emission_ratio_stack_df$cuts <- cut(CO_CO2_emission_ratio_stack_df$value, breaks = c(0.005, 0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.07,0.09))

CO_CO2_emission_ratio_stack_df

# Data visualization - CO_CO2_ratio emission
ggplot()+
  geom_raster(data = CO_CO2_emission_ratio_stack_df, aes(x = x, y = y, fill = cuts)) + 
  scale_fill_brewer("Legend_title",type = "seq", palette = "PiYG") +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~variable)

ggsave("CO_CO2_ratio_yearly_change.png", scale = 2, width = 5, height = 3)


# 2. Time series trend analysis - CO & CO2 emission 
## 2.1 Approach 1 - Pixel-wies regression analysis between CO and CO2 emission

# Time-series analysis --- Approach 1
# Trend analysis of the change in CO_CO2_ratio

library(rtsa)
library(trend)

# Linear regression analysis - pixel-wise regression between two raster time series
library(raster)
library(stats)
CO_CO2_emission_stack <- stack(CO_emission_stack, CO2_emission_stack)

fun_linear_slope = function(x) { if (is.na(x[1])){ NA } else { lm(x[1:10] ~ x[11:20])$coefficients[2] }}

fun_linear_intercept = function(x) { if (is.na(x[1])){ NA } else { lm(x[1:10] ~ x[11:20])$coefficients[1] }}

fun_linear_p_value = function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:10] ~ x[11:20]);summary(m)$coefficients[8] }}

fun_linear_r_squared = function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:10] ~ x[11:20]);summary(m)$r.squared }}

linear_slope <- calc(CO_CO2_emission_stack, fun_linear_slope)

linear_intercept <- calc(CO_CO2_emission_stack, fun_linear_intercept)

linear_r_squared <- calc(CO_CO2_emission_stack, fun_linear_r_squared)

linear_p_value <- calc(CO_CO2_emission_stack, fun_linear_p_value)

library(RColorBrewer)

cols <- brewer.pal(11, "RdYlGn")

pal <- colorRampPalette(cols)

# Data visualization - Linear results
plot(linear_slope, main = "Slope _ Pixel-wise  regression analysis between CO and CO2 emission ", col = pal(7))

plot(admin_boundary_2, add=TRUE)

plot(linear_intercept, main = "Intercept _ Pixel-wise regression analysis between CO and CO2 emission ")

plot(admin_boundary_2, add=TRUE)

plot(linear_r_squared, main = "R2 _ Pixel-wise  regression analysis between CO and CO2 emission ")

plot(admin_boundary_2, add=TRUE)

plot(linear_p_value, main = "P_value_ Pixel-wise regression analysis between CO and CO2 emission ")

plot(admin_boundary_2, add=TRUE)

# Mask p_value > 0.05 and get a confidence level of 95% - p值越小，拒绝原假设的理由越充分
m = c(0, 0.05, 1, 0.05, 1, 0)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask = reclassify(linear_p_value, rclmat)

fun = function(x) { x[x<1] <- NA; return(x)}
p.mask.NA = calc(p.mask, fun)

trend.sig = mask(linear_slope, p.mask.NA)

plot(trend.sig, main="Slope_statistically significant", col = pal(7))

plot(admin_boundary_2, add=TRUE)

summary(trend.sig)

#breaks = c(-1,-0.5,0, 0.5,1), col = topo.colors(4)


## 2.2 Approach 2 - Time-series-stack trend analysis between CO and CO2 emission - Pearson & Kendall analysis with statistically significance
### 2.2.1 Pearson

CO2_emission_stack_df <- as.data.frame(CO2_emission_stack, xy = TRUE, na.rm = TRUE)

CO2_emission_stack_df

CO_emission_stack_df <- as.data.frame(CO_emission_stack, xy = TRUE, na.rm = TRUE)

CO_emission_stack_df

location_CO2_emission_df <- CO2_emission_stack_df[,1:2] 

location_CO_emission_df <- CO_emission_stack_df[,1:2]

location_CO2_emission_df

location_CO_emission_df

identical(location_CO2_emission_df, location_CO_emission_df)

# Data reshaping
library(tibble)

CO2_emission_stack_df <- tibble::rownames_to_column(CO2_emission_stack_df, "pixel_index")

CO2_emission_stack_df

CO_emission_stack_df <- tibble::rownames_to_column(CO_emission_stack_df, "pixel_index")

CO_emission_stack_df

# To convert long to wide
CO2_emission_stack_df_1<-as.data.frame(t(CO2_emission_stack_df), as.numeric)

CO_emission_stack_df_1<-as.data.frame(t(CO_emission_stack_df))

names(CO2_emission_stack_df_1) <- data.frame(lapply(CO2_emission_stack_df_1[1, ], as.numeric))

CO2_emission_stack_df_1 <- CO2_emission_stack_df_1[-1, ] 

names(CO_emission_stack_df_1) <- data.frame(lapply(CO_emission_stack_df_1[1, ], as.numeric))

CO_emission_stack_df_1 <- CO_emission_stack_df_1[-1, ]

CO2_emission_stack_df_1 <- CO2_emission_stack_df_1[-c(1:2),]

CO_emission_stack_df_1 <- CO_emission_stack_df_1[-c(1:2),]

CO2_emission_stack_df_1 <- data.frame(lapply(CO2_emission_stack_df_1,as.numeric))

CO_emission_stack_df_1 <- data.frame(lapply(CO_emission_stack_df_1,as.numeric))

CO2_emission_stack_df_1

CO_emission_stack_df_1

# Try - single pair-wise correlation
library(psych)
a <- CO2_emission_stack_df_1[1]
b <-CO_emission_stack_df_1[1]
c <- corr.test(a,b,method = 'pearson')
c$r
c$p
c$p.adj

library(psych)

cor_trend_result <- data.frame()
for (i in 1:16047){
  column_CO2 <- CO2_emission_stack_df_1[i]
  column_CO <- CO_emission_stack_df_1[i]
  
  cor_trend <- corr.test(column_CO2, column_CO, method = "pearson")
  cor_trend_result[1,i] <- cor_trend$r
  cor_trend_result[2,i] <- cor_trend$p
}
cor_trend_result

# Data preparation: for the generation of raster of correlation 
library(dplyr)

colnames(cor_trend_result) <- colnames(CO_emission_stack_df_1)

cor_trend_result <- t(cor_trend_result)

cor_trend_result <- as.data.frame(cor_trend_result)

colnames(cor_trend_result) <- c("corr","p.value")
cor_trend_result

cor_merge <- cbind(cor_trend_result, location_CO_emission_df)

cor_merge

colnames(cor_merge) <- c("corr","p.value","lon","lat")

cor_merge

cor_merge <- cor_merge[, c(3,4,1,2)]

cor_merge

# Raster of correlation coefficient

cor_merge_corr <- cor_merge[, c(1,2,3)]

corr_pearson_corr <- rasterFromXYZ(cor_merge_corr) 

plot(corr_pearson_corr, main = "Coefficients for pearson correlation analysis")

plot(admin_boundary_2, add = TRUE)

# Raster of correlation - p_value

cor_merge_p_value <- cor_merge[, c(1,2,4)]

cor_merge_p_value_S <- cor_merge_p_value[cor_merge_p_value$p.value<=0.05,]

cor_merge_p_value_S <- rasterFromXYZ(cor_merge_p_value_S) 

plot(cor_merge_p_value_S, main = "P_value for pearson correlation analysis")

plot(admin_boundary_2, add = TRUE)

# To mask coefficient map by p_value map
corr_pearson_sig <- mask(corr_pearson_corr, cor_merge_p_value_S)

plot(corr_pearson_sig, main = "Pearson coefficient_statistically significant")

plot(admin_boundary_2, add = TRUE)

summary(corr_pearson_sig)


### 2.2.2 Kendall


# Try - single pair-wise correlation
library(psych)
a_1 <- CO2_emission_stack_df_1[1]
b_1 <-CO_emission_stack_df_1[1]
c_1 <- corr.test(a,b,method = 'kendall')
c_1$r
c_1$p
c_1$p.adj

library(psych)

cor_trend_kendall_result <- data.frame()
for (i in 1:16047){
  column_CO2 <- CO2_emission_stack_df_1[i]
  column_CO <- CO_emission_stack_df_1[i]
  
  cor_trend <- corr.test(column_CO2, column_CO, method = "kendall")
  cor_trend_kendall_result[1,i] <- cor_trend$r
  cor_trend_kendall_result[2,i] <- cor_trend$p
}
cor_trend_kendall_result

# Data preparation: for the generation of raster of correlation 
library(dplyr)

colnames(cor_trend_kendall_result) <- colnames(CO_emission_stack_df_1)

cor_trend_kendall_result <- t(cor_trend_kendall_result)

cor_trend_kendall_result <- as.data.frame(cor_trend_kendall_result)

colnames(cor_trend_kendall_result) <- c("corr","p.value")
cor_trend_kendall_result

cor_merge_kendall <- cbind(cor_trend_kendall_result, location_CO_emission_df)

cor_merge_kendall

colnames(cor_merge_kendall) <- c("corr","p.value","lon","lat")

cor_merge_kendall

cor_merge_kendall <- cor_merge_kendall[, c(3,4,1,2)]

cor_merge_kendall

# Raster of correlation coefficient

cor_merge_corr_kendall <- cor_merge_kendall[, c(1,2,3)]

corr_kendall_corr <- rasterFromXYZ(cor_merge_corr_kendall) 

plot(corr_kendall_corr, main = "Coefficients for kendall correlation analysis")

plot(admin_boundary_2, add = TRUE)

# Raster of correlation - p_value

cor_merge_p_value_kendall <- cor_merge_kendall[, c(1,2,4)]

cor_merge_p_value_S_kendall <- cor_merge_p_value_kendall[cor_merge_p_value_kendall$p.value<=0.05,]

cor_merge_p_value_S_kendall <- rasterFromXYZ(cor_merge_p_value_S_kendall) 

plot(cor_merge_p_value_S_kendall, main = "P_value for kendall correlation analysis")

plot(admin_boundary_2, add = TRUE)




# To mask coefficient map by p_value map
corr_kendall_sig <- mask(corr_kendall_corr, cor_merge_p_value_S_kendall)

plot(corr_kendall_sig, main = "Kendall coefficient_statistically significant")

plot(admin_boundary_2, add = TRUE)

summary(corr_kendall_sig)


plot(corr_kendall_sig)
plot(corr_pearson_sig)
plot(trend.sig)

summary(corr_kendall_sig)
summary(corr_pearson_sig)
summary(trend.sig)
```

# 3. Time series trend analysis - CO / CO2 emission ratio - TREND

CO_CO2_emission_ratio_stack <- stack(CO_CO2_emission_ratio_stack)

CO_CO2_emission_ratio_stack

CO_CO2_emission_ratio_stack_df


# 3.1 Approach 1: Linear analysis [ratio ~ year]

library(raster)
library(stats)

year = seq(2008,2017,1)

fun_linear_trend_slope = function(x) { if (is.na(x[1])){ NA } else { lm(x[1:10]~year)$coefficients[2] }}

fun_linear_trend_intercept = function(x) { if (is.na(x[1])){ NA } else { lm(x[1:10]~year)$coefficients[1] }}

fun_linear_trend_p_value = function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:10]~year);summary(m)$coefficients[8] }}

fun_linear_trend_r_squared = function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:10]~year);summary(m)$r.squared }}

linear_trend_slope <- calc(CO_CO2_emission_stack, fun_linear_trend_slope)

linear_trend_intercept <- calc(CO_CO2_emission_stack, fun_linear_trend_intercept)

linear_trend_r_squared <- calc(CO_CO2_emission_stack, fun_linear_trend_r_squared)

linear_trend_p_value <- calc(CO_CO2_emission_stack, fun_linear_trend_p_value)

# Data visualization - Linear trend results
plot(linear_trend_slope , main = "Slope_Trend analysis of CO & CO2 ratio")
plot(admin_boundary_2, add = TRUE)

plot(linear_trend_intercept, main = "Intercept_Trend analysis of CO & CO2 ratio")
plot(admin_boundary_2, add = TRUE)

plot(linear_trend_r_squared, main = "R2_Trend analysis of CO & CO2 ratio")
plot(admin_boundary_2, add = TRUE)

plot(linear_trend_p_value, main = "P_value_Trend analysis of CO & CO2 ratio")
plot(admin_boundary_2, add = TRUE)

# Mask p_value > 0.05 and get a confidence level of 95% - p值越小，拒绝原假设的理由越充分
m = c(0, 0.05, 1, 0.05, 1, 0)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask_trend = reclassify(linear_trend_p_value, rclmat)

fun_trend = function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_trend = calc(p.mask_trend, fun_trend)

trend.sig_trend = mask(linear_trend_slope, p.mask.NA_trend)

plot(trend.sig_trend, main="Slope_Trend analysis_statistically significant", col = pal(6))
plot(admin_boundary_2, add = TRUE)

summary(trend.sig_trend)

###################### Summary ###############################
# To perform zonal statistic for each provinces
library(raster)
library(rgdal)

linear_trend_CO_CO2_ratio_city <- extract(linear_trend_slope, admin_boundary, fun = mean, na.rm = TRUE, df = TRUE)

linear_trend_CO_CO2_ratio_city_df <- data.frame(linear_trend_CO_CO2_ratio_city)

linear_trend_CO_CO2_ratio_city_df

#### City ####
linear_trend_CO_CO2_ratio_city_df$Province <- admin_boundary@data$FIRST_省

linear_trend_CO_CO2_ratio_city_df$City <- admin_boundary@data$地市

linear_trend_CO_CO2_ratio_city_df$Province[368:385] <- "香港特别行政区"

linear_trend_CO_CO2_ratio_city_df$Province[386:393] <- "澳门特别行政区"

linear_trend_CO_CO2_ratio_city_df$Province[394:415] <- "台湾省"

linear_trend_CO_CO2_ratio_city_df

write.csv(linear_trend_CO_CO2_ratio_city_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/linear_trend_CO_CO2_ratio_city.csv")

#### Province ####
library(dplyr)

linear_trend_CO_CO2_ratio_province_df = linear_trend_CO_CO2_ratio_city_df %>% group_by(Province) %>%
  summarise(slope = mean(layer), .groups = 'drop')

linear_trend_CO_CO2_ratio_province_df

write.csv(linear_trend_CO_CO2_ratio_province_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/linear_trend_CO_CO2_ratio_province.csv")

#### Regions Calculation ####

# Statistical calculation

linear_trend_slope_CO_CO2_ratio_city <- read.csv(file = "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/linear_trend_CO_CO2_ratio_regions.csv", encoding = "UTF-8")
linear_trend_slope_CO_CO2_ratio_city

library(tidyverse)
linear_trend_slope_CO_CO2_ratio_region <- linear_trend_slope_CO_CO2_ratio_city %>% 
  group_by(Regions) %>% 
  summarise(
    mean = mean(layer),
    sd = sd(layer),
    n = n(), na.rm = FALSE
  ) %>% 
  mutate(se = sd/sqrt(n),
         Regions = factor(Regions, levels = c('BTH', 'CYD', 'YRD', 'PRD', 'FWP', 'Others')))
linear_trend_slope_CO_CO2_ratio_region

########## Region statistic #########
library(ggplot2)
library(gghalves)
library(ggpubr)
library(ggsignif)
library(ggsci)

ggplot(linear_trend_slope_CO_CO2_ratio_city, aes(x = Regions, y = layer, fill = Regions)) +
  geom_half_violin(aes(fill = Regions),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Regions) - 0.1,
                 y = layer,color = Regions),
             position = position_jitter(width = .05),size = .25, shape = 20)+
  geom_boxplot(aes(x = Regions, y = layer, fill = Regions),
               outlier.shape = NA,
               width = .05,
               color = "black") +
  geom_errorbar(data = linear_trend_slope_CO_CO2_ratio_region,
                aes(x = Regions, y = mean, group = Regions, colour = Regions,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  labs(y = "Slope_Linear Trend", x = "Regions")+
  scale_color_jco() +
  scale_fill_jco() +
  geom_signif(comparisons = list(c("BTH", "CYD"),
                                 c("BTH", "FWP"),
                                 c("BTH", "PRD"),
                                 c("BTH", "YRD"),
                                 c("CYD", "FWP"),
                                 c("CYD", "PRD"),
                                 c("CYD", "YRD"),
                                 c("FWP", "PRD"),
                                 c("FWP", "YRD"),
                                 c("PRD", "YRD")),
              map_signif_level = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
              textsize=2,test=wilcox.test,step_increase=0.05)


# 3.2 Approach 2: Kendall analysis [Kendall tau statistic and the Theil-Sen slope modification - 95% CL] 

# Calculates a nonparametric statistic for a monotonic trend based on the Kendall tau statistic and the Theil-Sen slope modification - 95% confidence level
# Output:
## raster layer 1 slope for trend, always returned
## raster layer 2 intercept for trend if intercept TRUE
## raster layer 3 p value for trend fit if p.value TRUE
## raster layer 4 z value for trend fit if z.value TRUE
## raster layer 5 lower confidence level at 95 pct, if confidence TRUE
## raster layer 6 upper confidence level at 95 pct, if confidence TRUE
## raster layer 7 Kendall's tau two-sided test, reject null at 0, if tau TRUE

library(spatialEco)
library(raster)

kendall_raster_ratio_trend <- raster.kendall(CO_CO2_emission_ratio_stack, p.value=TRUE, z.value=TRUE, 
                                             intercept=TRUE, confidence=TRUE, 
                                             tau=TRUE)

summary(kendall_raster_ratio_trend)

plot(kendall_raster_ratio_trend)

plot(kendall_raster_ratio_trend$slope, main = "CO_CO2_ratio_slope")
plot(admin_boundary_2, add = TRUE)
plot(kendall_raster_ratio_trend$intercept, main = "CO_CO2_ratio_intercept")
plot(admin_boundary_2, add = TRUE)
plot(kendall_raster_ratio_trend$p.value, main = "CO_CO2_ratio_p_value")
plot(admin_boundary_2, add = TRUE)
plot(kendall_raster_ratio_trend$z.value, main = "CO_CO2_ratio_z_value")
plot(admin_boundary_2, add = TRUE)
plot(kendall_raster_ratio_trend$tau, main = "CO_CO2_ratio_tau")
plot(admin_boundary_2, add = TRUE)

summary(kendall_raster_ratio_trend$slope)
summary(kendall_raster_ratio_trend$p.value)
summary(kendall_raster_ratio_trend$z.value)
summary(kendall_raster_ratio_trend$tau)

# To mask slope map + tau map by z_score map
# Mask p_value > 0.05 and get a confidence level of 95% - p值越小，拒绝原假设的理由越充分
z.value_Mann_Sen <- kendall_raster_ratio_trend$p.value
slope_Mann_Sen <- kendall_raster_ratio_trend$slope
tau_Mann_Sen <- kendall_raster_ratio_trend$tau

m = c(-4, -1.96, 1, -1.96, 1.96, 0, 1.96,4,1)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask_Mann_Sen_trend = reclassify(z.value_Mann_Sen, rclmat)

fun_trend = function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_Mann_Sen_trend = calc(p.mask_Mann_Sen_trend, fun_trend)

trend.sig_Mann_Sen_trend_tau = mask(tau_Mann_Sen, p.mask.NA_Mann_Sen_trend)
trend.sig_Mann_Sen_trend_slope = mask(slope_Mann_Sen, p.mask.NA_Mann_Sen_trend)

plot(trend.sig_Mann_Sen_trend_tau, main="Tau_Trend analysis_statistically significant")
plot(admin_boundary_2, add = TRUE)
plot(trend.sig_Mann_Sen_trend_slope, main="Slope_Trend analysis_statistically significant")
plot(admin_boundary_2, add = TRUE)

summary(trend.sig_Mann_Sen_trend_tau)
summary(trend.sig_Mann_Sen_trend_slope)

###################### Summary SLOPE ###############################
######################## Error #################################
# To perform zonal statistic for each provinces
library(raster)
library(rgdal)

slope_Mann_Sen_city <- raster::extract(slope_Mann_Sen, admin_boundary, fun = mean, na.rm = TRUE, df = TRUE)

slope_Mann_Sen_city_df <- data.frame(slope_Mann_Sen_city)

slope_Mann_Sen_city_df

#### City ####
slope_Mann_Sen_city_df$Province <- admin_boundary@data$FIRST_省

slope_Mann_Sen_city_df$City <- admin_boundary@data$地市

slope_Mann_Sen_city_df$Province[368:385] <- "香港特别行政区"

slope_Mann_Sen_city_df$Province[386:393] <- "澳门特别行政区"

slope_Mann_Sen_city_df$Province[394:415] <- "台湾省"

slope_Mann_Sen_city_df

write.csv(slope_Mann_Sen_city_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/slope_Mann_Sen_city.csv")

#### Province ####
library(dplyr)

slope_Mann_Sen_province_df = slope_Mann_Sen_city_df %>% group_by(Province) %>%
  summarise(slope = mean(slope), .groups = 'drop')

slope_Mann_Sen_province_df

write.csv(slope_Mann_Sen_province_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/slope_Mann_Sen_province.csv")

#### Regions Calculation ####

# Statistical calculation

slope_Mann_Sen_city_CO_CO2_ratio <- read.csv(file = "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/slope_Mann_Sen_regions.csv", encoding = "UTF-8")
slope_Mann_Sen_city_CO_CO2_ratio

library(tidyverse)
slope_Mann_Sen_region_CO_CO2_ratio <- slope_Mann_Sen_city_CO_CO2_ratio %>% 
  group_by(Regions) %>% 
  summarise(
    mean = mean(slope),
    sd = sd(slope),
    n = n(), na.rm = FALSE
  ) %>% 
  mutate(se = sd/sqrt(n),
         Regions = factor(Regions, levels = c('BTH', 'CYD', 'YRD', 'PRD', 'FWP', 'Others')))
slope_Mann_Sen_region_CO_CO2_ratio

########## Region statistic #########
library(ggplot2)
library(gghalves)
library(ggpubr)
library(ggsignif)
library(ggsci)

ggplot(slope_Mann_Sen_city_CO_CO2_ratio, aes(x = Regions, y = slope, fill = Regions)) +
  geom_half_violin(aes(fill = Regions),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Regions) - 0.1,
                 y = slope,color = Regions),
             position = position_jitter(width = .05),size = .25, shape = 20)+
  geom_boxplot(aes(x = Regions, y = slope, fill = Regions),
               outlier.shape = NA,
               width = .05,
               color = "black") +
  geom_errorbar(data = slope_Mann_Sen_region_CO_CO2_ratio,
                aes(x = Regions, y = mean, group = Regions, colour = Regions,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  labs(y = "Slope_Mann_Kendall Trend", x = "Regions")+
  scale_color_jco() +
  scale_fill_jco() +
  geom_signif(comparisons = list(c("BTH", "CYD"),
                                 c("BTH", "FWP"),
                                 c("BTH", "PRD"),
                                 c("BTH", "YRD"),
                                 c("CYD", "FWP"),
                                 c("CYD", "PRD"),
                                 c("CYD", "YRD"),
                                 c("FWP", "PRD"),
                                 c("FWP", "YRD"),
                                 c("PRD", "YRD")),
              map_signif_level = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
              textsize=2,test=wilcox.test,step_increase=0.05)


###################### Summary TAU ###############################
######################## Error #################################
# To perform zonal statistic for each provinces
library(raster)
library(rgdal)

tau_Mann_Sen_city <- raster::extract(tau_Mann_Sen, admin_boundary, fun = mean, na.rm = TRUE, df = TRUE)

tau_Mann_Sen_city_df <- data.frame(tau_Mann_Sen_city)

tau_Mann_Sen_city_df

#### City ####
tau_Mann_Sen_city_df$Province <- admin_boundary@data$FIRST_省

tau_Mann_Sen_city_df$City <- admin_boundary@data$地市

tau_Mann_Sen_city_df$Province[368:385] <- "香港特别行政区"

tau_Mann_Sen_city_df$Province[386:393] <- "澳门特别行政区"

tau_Mann_Sen_city_df$Province[394:415] <- "台湾省"

tau_Mann_Sen_city_df

write.csv(tau_Mann_Sen_city_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/tau_Mann_Sen_city.csv")

#### Province ####
library(dplyr)

tau_Mann_Sen_province_df = tau_Mann_Sen_city_df %>% group_by(Province) %>%
  summarise(tau = mean(tau), .groups = 'drop')

tau_Mann_Sen_province_df

write.csv(tau_Mann_Sen_province_df, "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/tau_Mann_Sen_province.csv")

#### Regions Calculation ####

# Statistical calculation

tau_Mann_Sen_city_CO_CO2_ratio <- read.csv(file = "D:/25. Ratster_Output_EMISSION/Ratio_Analysis/tau_Mann_Sen_regions.csv", encoding = "UTF-8")
tau_Mann_Sen_city_CO_CO2_ratio

library(tidyverse)
tau_Mann_Sen_region_CO_CO2_ratio <- tau_Mann_Sen_city_CO_CO2_ratio %>% 
  group_by(Regions) %>% 
  summarise(
    mean = mean(tau),
    sd = sd(tau),
    n = n(), na.rm = FALSE
  ) %>% 
  mutate(se = sd/sqrt(n),
         Regions = factor(Regions, levels = c('BTH', 'CYD', 'YRD', 'PRD', 'FWP', 'Others')))

########## Region statistic #########
library(ggplot2)
library(gghalves)
library(ggpubr)
library(ggsignif)
library(ggsci)

ggplot(tau_Mann_Sen_city_CO_CO2_ratio, aes(x = Regions, y = tau, fill = Regions)) +
  geom_half_violin(aes(fill = Regions),
                   position = position_nudge(x = .15, y = 0),
                   adjust=1.5, trim=FALSE, colour=NA, side = 'r') +
  geom_point(aes(x = as.numeric(Regions) - 0.1,
                 y = tau,color = Regions),
             position = position_jitter(width = .05),size = .25, shape = 20)+
  geom_boxplot(aes(x = Regions, y = tau, fill = Regions),
               outlier.shape = NA,
               width = .05,
               color = "black") +
  geom_errorbar(data = tau_Mann_Sen_region_CO_CO2_ratio,
                aes(x = Regions, y = mean, group = Regions, colour = Regions,
                    ymin = mean-se, ymax = mean+se),
                width=.05,
                position=position_nudge(x = .1, y = 0)
  ) +
  labs(y = "tau_Mann_Kendall Trend", x = "Regions")+
  scale_color_jco() +
  scale_fill_jco() +
  geom_signif(comparisons = list(c("BTH", "CYD"),
                                 c("BTH", "FWP"),
                                 c("BTH", "PRD"),
                                 c("BTH", "YRD"),
                                 c("CYD", "FWP"),
                                 c("CYD", "PRD"),
                                 c("CYD", "YRD"),
                                 c("FWP", "PRD"),
                                 c("FWP", "YRD"),
                                 c("PRD", "YRD")),
              map_signif_level = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
              textsize=2,test=wilcox.test,step_increase=0.05)


# 3.3 Approach 3: Manne-Kendall trend analysis


# Time-series analysis --- Approach 3 (calculate trends on time series in raster data) --- Manne-Kendall trend analysis
# Hypothesis - H0: There is no discernible pattern in the data
# Hypothesis - H1: There is a trend in the data. (This could indicate a positive or negative trend.)

library(raster)

library(Kendall)

fun_Manne_kendall <- function(x){ return(unlist(MannKendall(x)))}

Manne_kendall_CO_CO2_ratio_result <- calc(CO_CO2_emission_ratio_stack,fun_Manne_kendall)

# Output
## tau: Kendall's tau statistic
## sl: two-sided p-value
## S: Kendall Score
## D: denominator, tau=S/D
##varS: variance of S
summary(Manne_kendall_CO_CO2_ratio_result)

plot(Manne_kendall_CO_CO2_ratio_result)
plot(admin_boundary_2, add=TRUE)

plot(Manne_kendall_CO_CO2_ratio_result$tau, main = "Manne_Kendall_tau")
plot(admin_boundary_2, add=TRUE)

plot(Manne_kendall_CO_CO2_ratio_result$sl, main = "Manne_Kendall_two_side_p_value")
plot(admin_boundary_2, add=TRUE)

# To mask slope map by sens_p_value map
# Mask p_value > 0.05 and get a confidence level of 95% - p值越小，拒绝原假设的理由越充分
p.value_Mann <- Manne_kendall_CO_CO2_ratio_result$sl
tau_Mann <- Manne_kendall_CO_CO2_ratio_result$tau

m = c(0, 0.025, 1, 0.025, 0.975, 0, 0.975,1,1)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask_Mann_trend = reclassify(p.value_Mann, rclmat)

fun_trend = function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_Mann_trend = calc(p.mask_Mann_trend, fun_trend)

trend.sig_Mann_trend = mask(tau_Mann, p.mask.NA_Mann_trend)

plot(trend.sig_Mann_trend, main="Tau_Trend analysis_statistically significant")
plot(admin_boundary_2, add=TRUE)

summary(trend.sig_Mann_trend)


# 3.4 Approach 4: Sen's slope trend analysis - Computes Sen's slope for linear rate of change and corresponding confidence intervalls

# Sen  time-series analysis --- CO&CO2_ratio: sEN 斜率用于计算趋势值 - Computes Sen's slope for linear rate of change and corresponding confidence intervals

fun_Sen <- function(y){
  if(length(na.omit(y)) <10) return(c(NA, NA, NA))   #删除数据不连续含有NA的像元
  
  Sen_estimate <- sens.slope(ts(na.omit(y), start = 2008, end = 2017, frequency = 1), conf.level = 0.95) #Sen斜率估计
  
  Zs <- Sen_estimate$statistic
  
  slope <- Sen_estimate$estimate
  
  p_value <- Sen_estimate$p.value
  
  return(c(Zs, slope, p_value))
}

CO_CO2_ratio_sen <- calc(CO_CO2_emission_ratio_stack, fun_Sen)   #栅格计算
CO_CO2_ratio_Zs <- subset(CO_CO2_ratio_sen,1)  #提取Z统计量
CO_CO2_ratio_slope <- subset(CO_CO2_ratio_sen,2)   #提取sen斜率
CO_CO2_ratio_p_value <- subset(CO_CO2_ratio_sen,3)   #提取p值

plot(CO_CO2_ratio_Zs, main = "Sen_Z_score")
plot(admin_boundary_2, add=TRUE)
plot(CO_CO2_ratio_slope, main = "Sen_slope")
plot(admin_boundary_2, add=TRUE)
plot(CO_CO2_ratio_p_value, main = "Sen_p_value")
plot(admin_boundary_2, add=TRUE)

summary(CO_CO2_ratio_sen)
summary(CO_CO2_ratio_Zs)
summary(CO_CO2_ratio_slope)
summary(CO_CO2_ratio_p_value)

# To mask slope map by sens_p_value map
# Mask p_value > 0.05 and get a confidence level of 95% - p值越小，拒绝原假设的理由越充分
m = c(0, 0.05, 1, 0.05, 1, 0)
rclmat = matrix(m, ncol=3, byrow=TRUE)
p.mask_sens_trend = reclassify(CO_CO2_ratio_p_value, rclmat)

fun_trend = function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_sens_trend = calc(p.mask_sens_trend, fun_trend)

trend.sig_sens_trend = mask(CO_CO2_ratio_slope, p.mask.NA_sens_trend)

plot(trend.sig_sens_trend, main="Slope_Trend analysis_statistically significant")
plot(admin_boundary_2, add = TRUE)

summary(trend.sig_sens_trend)














