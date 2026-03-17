## Re create North Sea haddock Survey index from Stock Annex ##
## E MacLeod ##
## January 2024 ##
## Updated February 2025. Model now includes "gear" ##

rm(list = ls())

## libraries and setwd -----

# library (maptools)
library(DATRAS)
library(surveyIndex)
library(icesDatras)
library (tidyverse)
#indexlibrary (rnaturalearth)
#library (sf)
#library (reshape2)

input_data <- readLines("stdin", n = 3)
csquare <- input_data[1]
csquare <- jsonlite::fromJSON(csquare)
id <- input_data[2]
quarter <- input_data[3]

# csquare <- readLines("stdin", n = 1)
# haul_id <- as.numeric(input_data[1])

# id <- 0
# csquare <- "7501:140:3"


print(csquare)
print(id)
print(quarter)

`%notin%` <- Negate(`%in%`)

current_year <- substring(Sys.time(), 1, 4)

# dat.file <- "C:/Users/tiwonge/OneDrive - Robert Gordon University/HTCondor/mop_fish/"
# out.file <- "C:/Users/tiwonge/OneDrive - Robert Gordon University/HTCondor/mop_fish/"

dat.file <- "./"
out.file <- "./"

ices_data <- readRDS(paste0(dat.file, "Optimisation_Data_Haddock", ".Rdata")) ### CHANGE DATA FILE HERE
print(length(ices_data)) # before
ices_subareas <- readRDS (file = paste0 (dat.file, "ICES_subareas.Rdata")) ### CHANGE DATA FILE HERE
ices_data <- subset(ices_data, cs_code0.5 %notin% c(csquare)) ### CHANGE HERE WHEN CHANGING RESOLUTION
print(length(ices_data)) # after
## functions -----

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

## set quarter and ages -----
if (quarter == "q1") {
  q <- c(1)
} else if (quarter == "q3") {
  q <- c(3, 4)
}

# quarter <- c("1", paste0("3,4"))

# ices_data2 <- ices_data # save data with both quarters before gets subset in loop

# for (i in 1:length (unique (quarter))) {
  
  # ices_data <- ices_data2 # data previously subset in loop so just getting back to full dat
  
#   print (quarter[i])
  
if (1 %in% q) {had.ages <- c(1:8)}
if (3 %in% q) {had.ages <- c(0:8)}

ices_data <- subset (ices_data,
                     Species == "Melanogrammus aeglefinus", # haddock only
                     HaulVal == "V", # valid hauls only
                     !is.na(lon), !is.na(lat), # hauls with spatial information
                     Depth < 201, # remove very deep tows
                     !is.na (Depth),
                     Quarter %in% q, # ensure correct quarter
                     Area_27 %in% ices_subareas, # subareas we are interested in
                     StdSpecRecCode == 1
)


    ### Added

    hauls2 <- readRDS (paste0 (dat.file, "hauls2.Rdata"))

    ices_data[["HH"]] <- left_join (ices_data[["HH"]], hauls2)
    # ices_data[["HL"]] <- left_join (ices_data[["HL"]], hauls2)
    # ices_data[["CA"]] <- left_join (ices_data[["CA"]], hauls2)
    ices_data$haul.id <- as.factor (ices_data$haul.id)

    # test <- ices_data[["HH"]]
    # test <- test %>% select (haul.id, haul.id2, Gear, Gear2)
    # rm (test)

    ices_data[["HH"]]$Gear <- ices_data[["HH"]]$Gear2
    ices_data$Gear <- as.factor (ices_data$Gear)

    ices_data <- addSpectrum(ices_data, by = 1)
    attr(ices_data, "cm.breaks")
    head(ices_data$N)

      # windows (12,12)
    # plot (ices_data, col = "red") # plot length distributions onto trawls
    # savePlot (filename = paste0(out.file, "/Quarter", paste (q, collapse = "_"), "/DATRAS length distributions_", exclusion), type = "png")
    # dev.off()
    
    # windows (14,8)
    # plotVBG(ices_data,scale=2,ylim=c(0,60),col=c(2,4),lwd=2,by=paste(Year,Quarter)) # cohort plot - evolution of the length distribution of the stock over time
    # savePlot (filename = paste0(out.file, "/Quarter", paste (q, collapse = "_"), "/DATRAS cohort plot_", exclusion), type = "png")
    # dev.off()
    
    ices_data <- addWeightByHaul(ices_data) # estimate weight of haddock caught in each haul
    
    # basic haul weight plot
    # ggplot () +
    #   geom_sf (data = countries, fill = "olivedrab4", colour = "grey30") +
    #   geom_point (data = ices_data[["HH"]],
    #               aes (x = lon, y = lat, size = HaulWgt, colour = Survey)) +
    #   coord_sf (xlim = c(-10, 12), ylim = c(50, 62)) +
    #   theme_bw () + xlab ("Longitude") + ylab ("Latitude") +
    #   scale_color_brewer (palette = "Paired", direction = -1) +
    #   scale_size_continuous (range = c(0.1,8), name = "Haul Weight (kg)") +
    #   ggtitle ("Haddock biomass")
    #
    # ggsave (paste0(out.file, "/Quarter", paste (q, collapse = "_"), "/DATRAS haul weight_", exclusion, ".png"), width = 9, height = 7)
    #
    # ices_data2 <- ices_data
    # ices_data2$HaulWgt[ices_data2$HaulWgt >5000] <- 5000
    #
    # ggplot () +
    #   geom_sf (data = countries, fill = "grey50", colour = "grey30") +
    #   geom_point (data = subset(ices_data[["HH"]], HaulWgt >0),
    #               aes (x = lon, y = lat, size = HaulWgt, colour = windfarm)) +
    #   geom_sf(data = offshore_windfarms, aes (), colour = "black", fill = NA) +
    #   coord_sf (xlim = c(-10, 12), ylim = c(50, 62)) +
    #   theme_bw () + xlab ("Longitude") + ylab ("Latitude") +
    #   scale_color_brewer (palette = "Set1", direction = -1) +
    #   scale_size_continuous (range = c(0.1,8), name = "Haul Weight (kg)") +
    #   ggtitle ("Haddock biomass")
    #
    # ggsave (paste0(out.file, "/Quarter", paste (q, collapse = "_"), "/DATRAS haul weight_windfarms ", exclusion, ".png"), width = 8, height = 6)
    
    ##' Spatial ALK -----
    
    print(paste0("Adding ALK ", Sys.time()))
    
    had.modelling <- add.ALK (ices_data)
    
    print(paste0("ALK done ", Sys.time()))

      ## Set up for modelling -----
    
    # make ctime
    had.modelling$ctime = as.numeric(as.character(had.modelling$Year))
    
    # create grid
    grid = getGrid(had.modelling, nLon = 40)
    plot(grid)
    gridd <- subset(had.modelling[[2]], haul.id %in% grid[[3]])
    
    ## Delta GAM modelling -----
    
    # positive part of the model- HAD TO REMOVE GEAR TO INCORPORATE DIFFERENT YEARS
    # mP <- rep("offset (log (HaulDur)) +
    #             s (lon, lat, bs = 'tp', k = 120, m = c(1, 0.5)) +
    #             Year +
    #             s (Depth, bs = 'ts', m = c(1, 0.5), k = 6) +
    #             s (Ship, bs = 're') +
    #             s (TimeShotHour, bs = 'cc', k = 6) +
    #             s (ctime, bs = 'ts',  m = c(1, 0.5), k = 6)",
    #             length(had.ages))
    
    mP <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
  s(lon, lat, bs='ds', k=120, m=c(1, 0.5)) +
  s(lon, lat, bs='ds', m=c(1,0.5), k=9, by=Year, id=1) +
  s(Depth, bs='ds', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + 
  s(timeOfYear, bs='ds', k=6) + 
  offset(log(HaulDur))",
              length(had.ages))
    
    
    # presence/absesnce part of the model - HAD TO REMOVE GEAR TO INCORPORATE DIFFERENT YEARS
    # mZ <- rep("offset (log (HaulDur)) +
    #             s (lon, lat, bs = 'tp', k = 120, m = c(1, 0.5)) +
    #             Year +
    #             s (Depth, bs = 'ts', m = c(1, 0.5), k = 6) +
    #             s (Ship, bs = 're') +
    #             s (TimeShotHour, bs = 'cc', k = 6) +
    #             s (ctime, bs = 'ts',  m = c(1, 0.5), k = 6)",
    #             length(had.ages))
    
    mZ <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
  s(lon, lat, bs='ds', k=80, m=c(1, 0.5)) +
  s(lon, lat, bs='ds', k=7, m=c(1, 0.5), by=Year, id=1) +
  s(Depth, bs='ds', k=6) + 
  s(TimeShotHour, bs='cc', k=6)+ 
  s(timeOfYear, bs='ds', k=6) + 
  offset(log(HaulDur))",
              length(had.ages))
    
    # Q3/4 model has an added effect
    if (3 %in% q) {
    
    # positive part of the model- HAD TO REMOVE GEAR TO INCORPORATE DIFFERENT YEARS
    # mP <- rep("offset (log (HaulDur)) +
    #             s (lon, lat, bs = 'tp', k = 120, m = c(1, 0.5)) +
    #             Year +
    #             s (Depth, bs = 'ts', m = c(1, 0.5), k = 6) +
    #             s (Ship, bs = 're') +
    #             s (ctime, bs = 'ts',  m = c(1, 0.5), k = 6) +
    #             s (TimeShotHour, bs = 'cc', k = 6)",
    #             length(had.ages))
    
      mP <- rep("Year + Gear + s(Ship, bs='re', by=dum) + 
    s(lon, lat, bs='ds', k=120, m=c(1,0.5)) + 
    s(lon, lat, bs='ds', k=9, m=c(1,0.5), by=Year, id=1) + 
    s(log(Depth), bs='ts', k=6) + 
    s(timeOfYear, bs='ts', k=6) +   
    s(TimeShotHour, bs='cc', k=6) + 
    s(TimeShotHour, bs='cc', k=6, by= Quarter) + 
    offset(log(HaulDur))",
                length(had.ages))
      
    # presence/absesnce part of the model- HAD TO REMOVE GEAR TO INCORPORATE DIFFERENT YEARS
    # mZ <- rep("offset (log (HaulDur)) +
    #           s (lon, lat, bs = 'tp', k = 120, m = c(1, 0.5)) +
    #           Year +
    #           s (Depth, bs = 'ts', m = c(1, 0.5), k = 6) +
    #           s (Ship, bs = 're') +
    #           s (ctime, bs = 'ts',  m = c(1, 0.5), k = 6) +
    #           s (TimeShotHour, bs = 'cc', k = 6)",
    #         length(had.ages))
      
      mZ <- rep("Year + Gear + s(Ship, bs='re', by=dum) +
    s(lon, lat, bs='ds', k=80, m=c(1,0.5)) + 
    s(lon, lat, bs='ds', k=7, m=c(1,0.5), by=Year, id=1) +
    s(log(Depth), bs='ts', k=6) + s(timeOfYear, bs='ts', k=6) +   
    s(TimeShotHour, bs='cc', k=6) + 
    s(TimeShotHour, bs='cc', k=6, by= Quarter) + 
    offset(log(HaulDur))",
                length(had.ages))
    }
    
    print(paste0("Running Surveyindex Model ", Sys.time()))
  
    # SI = getSurveyIdx(had.modelling,
    #                 ages = had.ages,
    #                 myids = grid[[3]],
    #                 cutOff = 0.1,
    #                 fam = "LogNormal",
    #                 # kvecP = kvP,
    #                 # kvecZ = kvZ,
    #                 mc.cores = 1,
    #                 modelZ = mZ,
    #                 modelP = mP)
    
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

    # save.image (paste0(dat.file, "/Year ranges2/index_quarter", y, " ", paste (q, collapse = "_"), "_", exclusion, ".Rdata"))
    
#     index <- SI$idx
index <- 2*SI$idx[, ]/length(grid[[3]])
index <- as.data.frame(index)

write.csv(index, file = paste0(out.file, "/Index_for_solution_", id, "_", quarter, ".csv"))


  







