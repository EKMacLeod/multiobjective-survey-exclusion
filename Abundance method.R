## Cell removal by Abundance ##
## E MacLeod ##
## March 2026 ##

rm(list = ls())

## setwd and libraries -----

library (DATRAS)
library (sf)
library (tidyverse)
library (rnaturalearth)
library (marmap)
library (viridis)
library (reshape2)
library (surveyIndex)
library (icesDatras)

run <- "Haddock"
grid_res <- 0.5 # options of 0.05, 0.1, 0.2, 0.25 and 0.5
# quarter <- "Both Quarters"

dat.file <- "C:/Users/EM11976/Documents/Papers/OWD Surveys 2023/data/Haddock survey index"
# out.file <- paste0("C:/Users/EM11976/Documents/Papers/OWD Surveys 2023/output/Optimisation outputs/",
#                    run, "/", quarter, "/", grid_res, "/NSGAIIsimop_seed117_pop200_gens200_th0.5")

## functions -----
compare_indices_allages <- function(q, new_index) {
  
  # if (!(q %in% problem$quarters)) {
  #   stop("Invalid value for q. Expected 'q1' or 'q3'.")
  # }
  
  true_index <- if (1 %in% q) Q1_index_true else Q3_index_true
  
  real <- true_index
  
  # Fix column names
  colnames(real) <- gsub("X", "", colnames(real))
  
  # Align years
  # real <- real[match(rownames(new_index), real$year), ]
  
  # Align age columns
  real <- real[, colnames(new_index)]
  
  # real <- as.matrix(real)
  new  <- new_index
  
  impacts <- colSums(abs(new / real - 1))
  return(-mean(impacts))
}

index_reshape <- function(x, source) {
  x <- as.data.frame(x)
  
  # reshape data frames
  if (source == "WGNSSK") {
    x <- x %>% 
      rename (year = "...1")
  }
  
  if (source == "Index") {
    x$year <- rownames(x)
    x <- x %>% 
      rename ("8+" = "8")
  }
  
  x <- melt(x, id.vars = "year")
  x <- x %>% 
    rename (Age = variable, index = value) %>% 
    mutate (Source = source)
}

ALK_plots <- function (data, minage, maxage) {
  
  print(unique (data$Year))
  split <- as.character(unique(data$Year))
  
  fit <- d.ALK[[split]]
  
  windows (15, 8)
  plotALKraw(data, minAge = minage, maxAge = maxage)
  plotALKfit(fit, row = 1, add = TRUE)
  savePlot(filename = paste0(out.file, "/Quarter", paste (q, collapse = "_"),"/ALKs/", split, "_", exclusion), type = "png")
  dev.off()
}

add.ALK<-function(d){
  
  d.ysplit = split(d,d$Year)
  
  d.ALK= lapply(d.ysplit,
                fitALK,
                minAge = min (had.ages),
                maxAge = max (had.ages),
                method = 3, # spatial ALK
                gamma= NA,
                autoChooseK = TRUE,
                useBIC = TRUE,
                varCof = FALSE, # changed to TRUE - using varying coefficients for spatial effect (wide area)
                maxK = 50)
  
  d.Nage=lapply(d.ALK,predict)
  
  # lapply (d.ysplit, ALK_plots, minage = min(cod.ages), maxage = max(cod.ages))
  
  for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]];
  dd <- do.call("c",d.ysplit)
  
  dd
}

## spatial information -----

load ("C:/Users/EM11976/OneDrive - Robert Gordon University/WP6/Code/MASTS 2023/data/OWD data/wind_developments.Rdata")
rm (remove_rows, countries_near_Scotland, countries_near_UK)

ices_areas <- "data/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp"
ices_subareas <- c("3.a.20", "4.b", "4.c", "4.a", "7.d", "6.a")

countries <- ne_countries (type = "countries", continent = "Europe", scale = "large")
countries <- st_as_sf (countries)

bathy = getNOAA.bathy(lon1 = -20, lon2 = 20, lat1 = 40, lat2 = 65, 
                      resolution = 1)
bathy = fortify.bathy(bathy)

spatial_grid_cells <- readRDS (paste0 (dat.file, "/", grid_res, " csquare_grid.Rdata" ))

## load ICES data -----

ices_data <- readRDS (file = paste0 (dat.file, "/Optimisation data/Optimisation Data Haddock.Rdata"))
ices_data <- subset (ices_data, Area_27 %in% ices_subareas)
hauls2 <- readRDS (file = paste0 (dat.file, "/Optimisation data/hauls2.Rdata"))

## For each quarter, calculate the total abundance (N) in each grid cell and create order based on this -----

Abundance_gridCell <- Abundance_plot %>% 
  group_by (SURVEY, cs_code0.5) %>% 
  summarise (Total_Abundance = sum (Abundance))

# which grid cells are in Q1 and not Q34 and vice versa
length (unique (Abundance_gridCell$cs_code0.5)) # 388 grid cells
length (Abundance_gridCell$cs_code0.5[!(duplicated(Abundance_gridCell$cs_code0.5)|duplicated(Abundance_gridCell$cs_code0.5, fromLast=TRUE))])
# 44 of these cells are in one quarter and not in the other

# for each grid cell which quarter has the highest abundance?
Quarter_highest_abundance <- Abundance_gridCell %>% 
  group_by (cs_code0.5) %>% 
  mutate (max_Col = which.max(Total_Abundance)) %>% 
  mutate (max_col_name = NA) %>% 
  group_by (cs_code0.5, max_Col, max_col_name) %>% 
  summarise (n =n())

Quarter_highest_abundance$max_col_name[Quarter_highest_abundance$max_Col == 1] <- "Quarter 1"
Quarter_highest_abundance$max_col_name[Quarter_highest_abundance$max_Col == 2] <- "Quarter 3+4"

summary (as.factor (Quarter_highest_abundance$max_col_name))
# in 173 cells abundance is higher in quarter 1, in 215 cells abundance is higher in quarter 3+4
rm (Quarter_highest_abundance)

# difference in total abundance between the two quarters
Abundance_gridCell %>% 
  group_by (SURVEY) %>% 
  summarise (TOTAL_abundance = sum (Total_Abundance))
# Q1 total abundance = 1807352
# Q3+4 total abundance = 2639234

# Calculate the total abundance in each grid cell
Abundance_gridCell <- Abundance_gridCell %>% 
  group_by (cs_code0.5) %>% 
  summarise (Total_Abundance = sum (Total_Abundance))

# order cells based on this and assign a number
Abundance_gridCell <- Abundance_gridCell %>% 
  arrange (-Total_Abundance) %>% 
  mutate (n_excluded = c(1:nrow(Abundance_gridCell))) %>% 
  as.data.frame()

# plot that compares this to the greedy outputs
Abundance_gridCell <- Abundance_gridCell %>% select (-Total_Abundance) %>%  mutate (method = "Raw Abundance")
  
greedy_plot_data <- readRDS ("output/Reviewer comments/Greedy plot data.Rdata")
greedy_plot_data <- greedy_plot_data %>% 
  filter (!is.na (sum_impact)) %>% 
  select (-id, -sum_impact, -grid_id_removed2) %>%
  mutate (method = "Greedy")

abundance_plot_data <- st_as_sf (merge (Abundance_gridCell, spatial_grid_cells, all.x = TRUE))
abundance_plot_data <- abundance_plot_data %>% arrange (n_excluded)

head (abundance_plot_data)
head (greedy_plot_data)

comparison_plot <- rbind (greedy_plot_data, abundance_plot_data[1:157,])

ggplot () + 
  geom_sf (data = comparison_plot, aes (fill = n_excluded)) +
  facet_wrap (~method) +
  scale_fill_gradientn (colours = colorspace::diverge_hcl(5), name = "Order of\nCells Removed", 
                        breaks = seq (0, 160, 40), labels = c("0", "40", "80", "120", "160")) +
  # geom_contour(data = bathy, aes(x=x, y=y, z=z),breaks=c(-50, -100, -150, -500, -1000),
  #              linewidth=c(0.1), colour="lightblue") +
  geom_sf (data = countries, aes (), colour = "grey20", fill = "grey70") +
  # geom_sf (data = offshore_windfarms, aes (), fill = NA, colour = "blue", linewidth = 0.1) +
  coord_sf (xlim = c(-12, 12), ylim = c(49, 62)) +
  theme_bw () + theme (panel.background = element_rect(fill = "aliceblue")) +
  xlab ("Longitude") + ylab ("Latitude") 

ggsave ("output/Reviewer comments/Greedy vs Abundance plot_cropped.png", width = 12, height = 8)

## Run a Kendall Tau looking at the same order as Greedy -----

greedy_plot_data <- greedy_plot_data %>% arrange (n_excluded)

comparison <- greedy_plot_data %>% 
  as.data.frame () %>% 
  select (cs_code0.5, n_excluded, -x) %>% 
  rename (greedy_order = n_excluded)

Abundance_join <- abundance_plot_data %>% 
  as.data.frame () %>% 
  select (cs_code0.5, n_excluded, -x) %>% 
  rename (abundance_order = n_excluded)

comparison <- left_join (comparison, Abundance_join)

cor(comparison$greedy_order, comparison$abundance_order, method = "kendall")
cor.test(comparison$greedy_order, comparison$abundance_order, method = "kendall")

write_csv (comparison, file = "output/Reviewer comments/greedy_abundance ranks.csv")

## loop through the order that abundance method would remove cells and calculate the % difference for these solutions -----

head (Abundance_gridCell)

# Abundance_gridCell <- Abundance_gridCell[1:2,]

ices_data_complete <- ices_data

# read in the 'true' indices
Q1_index_true <- read.csv("data/Haddock survey index/Optimisation data/Haddock Index Q1.csv")
Q3_index_true <- read.csv("data/Haddock survey index/Optimisation data/Haddock Index Q3.csv")

for (a in 1:nrow (Abundance_gridCell)) {
  
  print (a)
  
  cs_code <- Abundance_gridCell$cs_code0.5[a]
  
  print (cs_code)
  
  ## create loop that adds on an extra cell each time
  
  if (a == 1) {cells_exclude = cs_code}
  if (a != 1) {cells_exclude = c(cs_code, cells_exclude)}
  
  ## Remove that cell from the ices data -----
  ices_data_cellRm <- subset(ices_data_complete, cs_code0.5 %notin% c(cells_exclude)) ### CHANGE HERE WHEN CHANGING RESOLUTION
  
  ## Quarter 1 run model and calculate the % impact -----
  message ("running Q1")
  
  q <- 1
  
  if (1 %in% q) {had.ages <- c(1:8)}
  if (3 %in% q) {had.ages <- c(0:8)}
  
  # filter_data
  ices_data <- subset (ices_data_cellRm,
                       Species == "Melanogrammus aeglefinus", # haddock only
                       HaulVal == "V", # valid hauls only
                       !is.na(lon), !is.na(lat), # hauls with spatial information
                       Depth < 201, # remove very deep tows
                       !is.na (Depth),
                       Quarter %in% q, # ensure correct quarter
                       Area_27 %in% ices_subareas, # subareas we are interested in
                       StdSpecRecCode == 1
  )
  
  
  ices_data[["HH"]] <- left_join (ices_data[["HH"]], hauls2)
  # ices_data[["HL"]] <- left_join (ices_data[["HL"]], hauls2)
  # ices_data[["CA"]] <- left_join (ices_data[["CA"]], hauls2)
  ices_data$haul.id <- as.factor (ices_data$haul.id)
  
  # test <- ices_data[["HH"]]
  # test <- test %>% select (haul.id, haul.id2, Gear, Gear2)
  # rm (test)
  
  ices_data[["HH"]]$Gear <- ices_data[["HH"]]$Gear2
  ices_data$Gear <- as.factor (ices_data$Gear)
  
  # number at length, weight by haul and number at age
  ices_data <- addSpectrum(ices_data, by = 1)
  attr(ices_data, "cm.breaks")
  
  head(ices_data$N)
  
  ices_data <- addWeightByHaul(ices_data)
  
  print(paste0("Adding ALK ", Sys.time()))
  
  had.modelling <- add.ALK (ices_data)
  
  print(paste0("ALK done ", Sys.time()))
  
  # Set up for modelling
  
  # make ctime
  had.modelling$ctime = as.numeric(as.character(had.modelling$Year))
  
  # create grid
  grid = getGrid(had.modelling, nLon = 40)
  plot(grid)
  gridd <- subset(had.modelling[[2]], haul.id %in% grid[[3]])
  
  ## Delta GAM modelling
  
  mP <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
  s(lon, lat, bs='ds', k=120, m=c(1, 0.5)) +
  s(lon, lat, bs='ds', m=c(1,0.5), k=9, by=Year, id=1) +
  s(Depth, bs='ds', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + 
  s(timeOfYear, bs='ds', k=6) + 
  offset(log(HaulDur))",
            length(had.ages))

  
  mZ <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
  s(lon, lat, bs='ds', k=80, m=c(1, 0.5)) +
  s(lon, lat, bs='ds', k=7, m=c(1, 0.5), by=Year, id=1) +
  s(Depth, bs='ds', k=6) + 
  s(TimeShotHour, bs='cc', k=6)+ 
  s(timeOfYear, bs='ds', k=6) + 
  offset(log(HaulDur))",
            length(had.ages))
  
  print(paste0("Running Surveyindex Model ", Sys.time()))
  
  SI = getSurveyIdx(had.modelling,
                    ages = had.ages,
                    myids=NULL,
                    predD=gridd,
                    cutOff = 0.1,
                    fam = "LogNormal",
                    # kvecP = kvP,
                    # kvecZ = kvZ,
                    mc.cores = 1,
                    modelZ = mZ,
                    modelP = mP, 
                    control=list(trace=TRUE,maxit=10)
  )
  
  print(paste0("Surveyindex Model Completed, saving output ", Sys.time()))

  index <- 2*SI$idx[, ]/length(grid[[3]])
  new_index <- as.data.frame(index)
  
  perc_diff_Q1 <- compare_indices_allages(1, new_index)
  
  ## Quarter 3+4 run model and calculate the % impact -----
  
  message ("running Q3+4")
  
  q <- c(3, 4)
  
  if (3 %in% q) {had.ages <- c(0:8)}
  
  # filter_data
  ices_data <- subset (ices_data_cellRm,
                       Species == "Melanogrammus aeglefinus", # haddock only
                       HaulVal == "V", # valid hauls only
                       !is.na(lon), !is.na(lat), # hauls with spatial information
                       Depth < 201, # remove very deep tows
                       !is.na (Depth),
                       Quarter %in% q, # ensure correct quarter
                       Area_27 %in% ices_subareas, # subareas we are interested in
                       StdSpecRecCode == 1
  )
  
  
  ices_data[["HH"]] <- left_join (ices_data[["HH"]], hauls2)
  # ices_data[["HL"]] <- left_join (ices_data[["HL"]], hauls2)
  # ices_data[["CA"]] <- left_join (ices_data[["CA"]], hauls2)
  ices_data$haul.id <- as.factor (ices_data$haul.id)
  
  # test <- ices_data[["HH"]]
  # test <- test %>% select (haul.id, haul.id2, Gear, Gear2)
  # rm (test)
  
  ices_data[["HH"]]$Gear <- ices_data[["HH"]]$Gear2
  ices_data$Gear <- as.factor (ices_data$Gear)
  
  # number at length, weight by haul and number at age
  ices_data <- addSpectrum(ices_data, by = 1)
  attr(ices_data, "cm.breaks")
  
  head(ices_data$N)
  
  ices_data <- addWeightByHaul(ices_data)
  
  print(paste0("Adding ALK ", Sys.time()))
  
  had.modelling <- add.ALK (ices_data)
  
  print(paste0("ALK done ", Sys.time()))
  
  # Set up for modelling
  
  # make ctime
  had.modelling$ctime = as.numeric(as.character(had.modelling$Year))
  
  # create grid
  grid = getGrid(had.modelling, nLon = 40)
  plot(grid)
  gridd <- subset(had.modelling[[2]], haul.id %in% grid[[3]])
  
  ## Delta GAM modelling
    mP <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
    s(lon, lat, bs='ds', k=120, m=c(1,0.5)) + 
    s(lon, lat, bs='ds', k=9, m=c(1,0.5), by=Year, id=1) + 
    s(log(Depth), bs='ts', k=6) + 
    s(timeOfYear, bs='ts', k=6) +   
    s(TimeShotHour, bs='cc', k=6) + 
    s(TimeShotHour, bs='cc', k=6, by= Quarter) + 
    offset(log(HaulDur))",
              length(had.ages))
    
    mZ <- rep("Year + Gear + s(Ship, bs='re', by=dum) +
    s(lon, lat, bs='ds', k=80, m=c(1,0.5)) + 
    s(lon, lat, bs='ds', k=7, m=c(1,0.5), by=Year, id=1) +
    s(log(Depth), bs='ts', k=6) + s(timeOfYear, bs='ts', k=6) +   
    s(TimeShotHour, bs='cc', k=6) + 
    s(TimeShotHour, bs='cc', k=6, by= Quarter) + 
    offset(log(HaulDur))",
              length(had.ages))
  
  print(paste0("Running Surveyindex Model ", Sys.time()))
  
  SI = getSurveyIdx(had.modelling,
                    ages = had.ages,
                    myids=NULL,
                    predD=gridd,
                    cutOff = 0.1,
                    fam = "LogNormal",
                    # kvecP = kvP,
                    # kvecZ = kvZ,
                    mc.cores = 1,
                    modelZ = mZ,
                    modelP = mP, 
                    control=list(trace=TRUE,maxit=10)
  )
  
  print(paste0("Surveyindex Model Completed, saving output ", Sys.time()))
  
  index <- 2*SI$idx[, ]/length(grid[[3]])
  new_index <- as.data.frame(index)
  
  perc_diff_Q3_4 <- compare_indices_allages(c(3, 4), new_index)
  
  
  ## create df compilating the % difference -----
  
  message ("compiling percentage differences")

  abundance_solutions <- data.frame (n_removed = a, 
              cs_code_rm = paste(cells_exclude, collapse = ";"), 
              impact_on_q1 = perc_diff_Q1, 
              impact_on_q3 = perc_diff_Q3_4)
  
  # calculate the sum impact
  abundance_solutions <- abundance_solutions %>% 
    mutate (sum_impact = sum (impact_on_q1, impact_on_q3))
  
  # if is the first iteration save as csv file
  if (a == 1) {
  write_csv (abundance_solutions, 
             file = "output/Reviewer comments/Abundance solutions/abundance solutions_1.csv")  
  }
  
  if (a != 1) {
  prev_solutions <- read_csv (paste0("output/Reviewer comments/Abundance solutions/abundance solutions_", (a-1), ".csv"))

  abundance_solutions <- rbind (prev_solutions, abundance_solutions)
  
  write_csv (abundance_solutions, 
             file = paste0("output/Reviewer comments/Abundance solutions/abundance solutions_", a, ".csv")) 
  }
  
  # remove things -----
  rm (abundance_solutions, perc_diff_Q1, perc_diff_Q3_4)
  rm (gridd, grid, had.modelling, ices_data, index, SI, mP, mZ)
  
}