########### Title - Pine marten multi-scale occupancy model 
########### Date - 2022.08.12
########### Author - Angoh; Thorsen; Hofmeester


rm(list = ls()) # Clear the workspace
setwd("C:/PineMarten")
##### Import and structure the data ######
y <- readRDS("dataarray.full.rds")
# Inspect the data
dim(y)
dimnames(y)

##### Import the covariates and order them in the order of the new name labels #####
cov.grid <- readRDS("cov.grid.sd.rds")
cov.loc <- readRDS("cov.loc.sd.rds")
cov.tempo <- readRDS("cov.tempo.sd.rds") # tempo as in temporal
cov.grid.clearcut <- readRDS("cov.grid.clearcut.sd.rds")
cov.grid.old <- readRDS("cov.grid.old_forest.sd.rds")
cov.loc.clearcut <- readRDS("cov.loc.clearcut.rds")
cov.loc.old <- readRDS("cov.loc.old_forest.rds")


##### Create object with all the relations between ct, grid and covs ################
labels <- readRDS("Grid_loc_newloc_relation.rds")
# Finding new grid cells and camera traps
new.grid.label <- 
  data.frame(old_name = unlist(dimnames(y)[2]),
             new_grid_id = 1:length(unlist(dimnames(y)[2]))) #Add new_grid_id column 
# This object now has all the relations between ct, grid and covs
labels <- merge(labels, new.grid.label, by.x = "ind.id", by.y = "old_name") 

# Renaming the dimensions in the ct data
dimnames(y)[2] <- list(new.grid.label$new_grid_id) # Grid ids
dimnames(y) # Good. The Object new.grid.label now has the relation between old and new names.
# The original was correct since the old grid IDs were sorted by increasing grid ids


##### cov.grid - can be supplied as a matrix or array with two dimensions #######
# The order of the covariates here is given by the ordinal order of the columns order in cov.loc
# 1 = forest.coverS
# Make sure the order in the covariates are correct
colnames(cov.grid)
cov.grid <-
  merge(
    cov.grid,
    unique(labels[, c("id", "new_grid_id")]),
    by = "id",
    all.x = TRUE,
    all.y = FALSE
  )
cov.grid <- cov.grid[order(cov.grid$new_grid_id), ]
rownames(cov.grid) <- 1:nrow(cov.grid)


cov.grid.mat <-
  as.matrix(cov.grid[, c("forest.coverS", "num.area")])

##### Temporally (year) varying clearcut and old forest covariates at grid cell level #####
######## 1. Re-index clearcut and add to array ####################################
# Supply this data as an array with 3 dimensions: covariate, grid, year
# First organize the clearcut data for re-indexing before adding to the model
cov.grid_id <-
  cov.grid.clearcut$id # Extract location_id, make sure it is in correct order
library(tidyverse)
cov.grid.clearcut <-
  gather(cov.grid.clearcut) # Make column into rows with associated value
cov.grid.clearcut$id <-
  cov.grid_id # Add location id to each year-clearcut value

library(dplyr)
cov.grid.clearcut <-
  cov.grid.clearcut %>% filter(!grepl("id", key)) # Remove all rows with grid id
cov.grid.clearcut <-
  rename(cov.grid.clearcut, year = key) # Rename year column
cov.grid.clearcut <-
  rename(cov.grid.clearcut, clearcutS = value) # Rename clearcut column
cov.grid.clearcut$year[cov.grid.clearcut$year == "Yr2018S"] <-
  "1" # Replace Yr2018S with 2018
cov.grid.clearcut$year[cov.grid.clearcut$year == "Yr2019S"] <-
  "2" # Replace Yr2018S with 2019
cov.grid.clearcut$year[cov.grid.clearcut$year == "Yr2020S"] <-
  "3" # Replace Yr2018S with 2020
cov.grid.clearcut$year[cov.grid.clearcut$year == "Yr2021S"] <-
  "4" # Replace Yr2018S with 2021

cov.grid.clearcut$year <- as.integer(cov.grid.clearcut$year) # Converting to an integer

#save re-organised clearcut.grid
saveRDS(cov.grid.clearcut, file = "cov.grid.clearcut.re-index.rds")

##
cov.grid.clearcut <-
  merge(cov.grid.clearcut, labels[, c("id", "new_grid_id")], by =
          "id")

#Make sure the order in the covariates are correct
cov.grid.clearcut <- cov.grid.clearcut[order(cov.grid.clearcut$new_grid_id), ]

n.cov.grid.clearcut = 1 # Number of temporally (year) varying covariates at the grid level (clearcut)

cov.grid.clearcut.arr <-
  array(
    NA,
    dim = c(
      n.cov.grid.clearcut,
      max(cov.grid.clearcut$new_grid_id),
      max(cov.grid.clearcut$year)
    ),
    dimnames = list(
      1:n.cov.grid.clearcut,
      1:max(cov.grid.clearcut$new_grid_id),
      1:max(cov.grid.clearcut$year)
    )
  )


# Filling up the array with information
for (i in 1:nrow(cov.grid.clearcut)) {
  this.grid.clearcut <- cov.grid.clearcut[i,]$new_grid_id #120L
  this.year <- cov.grid.clearcut[i,]$year #1L
  cov.grid.clearcut.arr[,this.grid.clearcut, this.year] <-
    as.numeric(cov.grid.clearcut[i, 3]) # Here we need to supply the columns for the covariate
}

#Check
#cov.grid.clearcut.arr[1, , ]


######## 2. Re-index old forest and add to array###################################
# Supply this data as an array with 3 dimensions: covariate, grid, year
# First organize the clearcut data for re-indexing before adding to the model
cov.grid_id <-
  cov.grid.old$id # Extract location_id, make sure it is in correct order
library(tidyverse)
cov.grid.old <-
  gather(cov.grid.old) # Make column into rows with associated value
cov.grid.old$id <-
  cov.grid_id # Add location id to each year-old forest value

library(dplyr)
cov.grid.old <-
  cov.grid.old %>% filter(!grepl("id", key)) # Remove all rows with grid id
cov.grid.old <-
  rename(cov.grid.old, year = key) # Rename year column
cov.grid.old <-
  rename(cov.grid.old, oldS = value) # Rename clearcut column
cov.grid.old$year[cov.grid.old$year == "Yr2018S"] <-
  "1" # Replace Yr2018S with 2018
cov.grid.old$year[cov.grid.old$year == "Yr2019S"] <-
  "2" # Replace Yr2018S with 2019
cov.grid.old$year[cov.grid.old$year == "Yr2020S"] <-
  "3" # Replace Yr2018S with 2020
cov.grid.old$year[cov.grid.old$year == "Yr2021S"] <-
  "4" # Replace Yr2018S with 2021

cov.grid.old$year <- as.integer(cov.grid.old$year) # Converting to an integer

#save re-organised clearcut.grid
saveRDS(cov.grid.old, file = "cov.grid.old.re-index.rds")

##
cov.grid.old <-
  merge(cov.grid.old, labels[, c("id", "new_grid_id")], by =
          "id")

#Make sure the order in the covariates are correct
cov.grid.old <- cov.grid.old[order(cov.grid.old$new_grid_id), ]

n.cov.grid.old = 1 # Number of temporally (year) varying covariates at the grid level (old forest)

cov.grid.old.arr <-
  array(
    NA,
    dim = c(
      n.cov.grid.old,
      max(cov.grid.old$new_grid_id),
      max(cov.grid.old$year)
    ),
    dimnames = list(
      1:n.cov.grid.old,
      1:max(cov.grid.old$new_grid_id),
      1:max(cov.grid.old$year)
    )
  )


# Filling up the array with information
for (i in 1:nrow(cov.grid.old)) {
  this.grid.old <- cov.grid.old[i,]$new_grid_id #120L
  this.year <- cov.grid.old[i,]$year #1L
  cov.grid.old.arr[,this.grid.old, this.year] <-
    as.numeric(cov.grid.old[i, 3]) # Here we need to supply the columns for the covariate
}


##### cov.loc: Stationary camera trap covariates ####################################
# Supplying the data as an array with the following 3 dimensions; covariate, grid id, camera trap id
# The order of the covariates here is given by the ordinal order of the columns order in cov.loc
# 1 = TRI_50mS, 2 = num.feature
cov.loc <-
  merge(cov.loc, labels[, c("location_id", "ct_loc_label", "new_grid_id")], by =
          "location_id")

#Make sure the order in the covariates are correct
cov.loc <- cov.loc[order(cov.loc$new_grid_id), ]

n.covs.loc = 2 # Number of stationary covariates at the ct level
cov.loc.arr <-
  array(
    NA,
    dim = c(
      n.covs.loc,
      max(cov.loc$new_grid_id),
      max(cov.loc$ct_loc_label)
    ),
    dimnames = list(
      1:n.covs.loc,
      1:max(cov.loc$new_grid_id),
      1:max(cov.loc$ct_loc_label)
    )
  )

# Fill up the array with information
for (i in 1:nrow(cov.loc)) {
  this.grid <- cov.loc[i, ]$new_grid_id #120L???
  this.loc <- cov.loc[i, ]$ct_loc_label #2L??
  cov.loc.arr[, this.grid, this.loc] <-
    as.numeric(cov.loc[i, 2:3]) # Supply the columns for the covariates
}

#Check
#cov.loc.arr[2, , ]
# 2 covariate, 323 grid with cameras, max 8 camera traps in a grid cell
#view(cov.loc.arr)

##### Temporally (year) varying clearcut and old forest covariates at camera trap level #####
######## 1. Re-index clearcut and add to array ####################################
# Supply this data as an array with 4 dimensions: covariate, grid, camera trap, year
# First organize the clearcut data for re-indexing before adding to the model
cov.location_id <-
  cov.loc.clearcut$location_id # Extract location_id, make sure it is in correct order
library(tidyverse)
cov.loc.clearcut <-
  gather(cov.loc.clearcut) # Make column into rows with associated value
cov.loc.clearcut$location_id <-
  cov.location_id # Add location id to each year-clearcut value

library(dplyr)
cov.loc.clearcut <-
  cov.loc.clearcut %>% filter(!grepl("location_id", key)) # Remove all rows with location_id
cov.loc.clearcut <-
  rename(cov.loc.clearcut, year = key) # Rename year column
cov.loc.clearcut <-
  rename(cov.loc.clearcut, clearcutC = value) # Rename clearcut column
cov.loc.clearcut$year[cov.loc.clearcut$year == "Yr2018C"] <-
  "1" # Replace Yr2018S with 1
cov.loc.clearcut$year[cov.loc.clearcut$year == "Yr2019C"] <-
  "2" # Replace Yr2018S with 2
cov.loc.clearcut$year[cov.loc.clearcut$year == "Yr2020C"] <-
  "3" # Replace Yr2018S with 3
cov.loc.clearcut$year[cov.loc.clearcut$year == "Yr2021C"] <-
  "4" # Replace Yr2018S with 4

cov.loc.clearcut$year <- as.integer(cov.loc.clearcut$year) # Converting to an integer

#save re-organised clearcut.loc
saveRDS(cov.loc.clearcut, file = "cov.loc.clearcut.re-index.rds")

cov.loc.clearcut <-
  merge(cov.loc.clearcut, labels[, c("location_id", "ct_loc_label", "new_grid_id")], by =
          "location_id")

#Make sure the order in the covariates are correct
cov.loc.clearcut <- cov.loc.clearcut[order(cov.loc.clearcut$new_grid_id), ]

n.cov.loc.clearcut = 1 # Number of temporally (year) varying covariates at the ct level (clearcut)

cov.loc.clearcut.arr <-
  array(
    NA,
    dim = c(
      n.cov.loc.clearcut,
      max(cov.loc.clearcut$new_grid_id),
      max(cov.loc.clearcut$ct_loc_label),
      max(cov.loc.clearcut$year)
    ),
    dimnames = list(
      1:n.cov.loc.clearcut,
      1:max(cov.loc.clearcut$new_grid_id),
      1:max(cov.loc.clearcut$ct_loc_label),
      1:max(cov.loc.clearcut$year)
    )
  )

# Filling up the array with information
for (i in 1:nrow(cov.loc.clearcut)) {
  this.grid.clearcut <- cov.loc.clearcut[i,]$new_grid_id #120L
  this.loc.clearcut <- cov.loc.clearcut[i,]$ct_loc_label #2L
  this.year <- cov.loc.clearcut[i,]$year #1L
  cov.loc.clearcut.arr[,this.grid.clearcut, this.loc.clearcut, this.year] <-
    as.numeric(cov.loc.clearcut[i, 3]) # Here we need to supply the columns for the covariate
}

######## 2. Re-index old forest and add to array ####################################
# Supply this data as an array with 4 dimensions: covariate, grid, camera trap, year
# First organize the old forest data for re-indexing before adding to the model
cov.location_id <-
  cov.loc.old$location_id # Extract location_id, make sure it is in correct order
library(tidyverse)
cov.loc.old <-
  gather(cov.loc.old) # Make column into rows with associated value
cov.loc.old$location_id <-
  cov.location_id # Add location id to each year-old forest value

library(dplyr)
cov.loc.old <-
  cov.loc.old %>% filter(!grepl("location_id", key)) # Remove all rows with location_id
cov.loc.old <-
  rename(cov.loc.old, year = key) # Rename year column
cov.loc.old <-
  rename(cov.loc.old, oldC = value) # Rename old forest column
cov.loc.old$year[cov.loc.old$year == "Yr2018C"] <-
  "1" # Replace Yr2018S with 1
cov.loc.old$year[cov.loc.old$year == "Yr2019C"] <-
  "2" # Replace Yr2018S with 2
cov.loc.old$year[cov.loc.old$year == "Yr2020C"] <-
  "3" # Replace Yr2018S with 3
cov.loc.old$year[cov.loc.old$year == "Yr2021C"] <-
  "4" # Replace Yr2018S with 4

cov.loc.old$year <- as.integer(cov.loc.old$year) # Converting to an integer

#save re-organised old.loc
saveRDS(cov.loc.old, file = "cov.loc.old.re-index.rds")

cov.loc.old <-
  merge(cov.loc.old, labels[, c("location_id", "ct_loc_label", "new_grid_id")], by =
          "location_id")

#Make sure the order in the covariates are correct
cov.loc.old <- cov.loc.old[order(cov.loc.old$new_grid_id), ]

n.cov.loc.old = 1 # Number of temporally (year) varying covariates at the ct level (old forest)

cov.loc.old.arr <-
  array(
    NA,
    dim = c(
      n.cov.loc.old,
      max(cov.loc.old$new_grid_id),
      max(cov.loc.old$ct_loc_label),
      max(cov.loc.old$year)
    ),
    dimnames = list(
      1:n.cov.loc.old,
      1:max(cov.loc.old$new_grid_id),
      1:max(cov.loc.old$ct_loc_label),
      1:max(cov.loc.old$year)
    )
  )

# Filling up the array with information
for (i in 1:nrow(cov.loc.old)) {
  this.grid.old <- cov.loc.old[i,]$new_grid_id #120L
  this.loc.old <- cov.loc.old[i,]$ct_loc_label #2L
  this.year <- cov.loc.old[i,]$year #1L
  cov.loc.old.arr[,this.grid.old, this.loc.old, this.year] <-
    as.numeric(cov.loc.old[i, 3]) # Here we need to supply the columns for the covariate
}

#Check
#cov.loc.clearcut.arr[1, , , ]
#cov.loc.old.arr[1, , , ]
#cov.loc.arr[1, , ]
#cov.tempo.arr[1, ,, ]


##### Temporally (period) varying camera trap covariates for detection probability #####
# Supplying also this data as an array, now with three dimensions: grid, camera trap, period/survey
# Fill in array with covariate
cov.tempo <- merge(cov.tempo, labels[, c("location_id", "ct_loc_label", "new_grid_id")], by =
                     "location_id")

#Make sure the order in the covariates are correct
cov.tempo <- cov.tempo[order(cov.tempo$new_grid_id), ]

n.covs.tempo = 2 # Number of temporally varying covariates at the ct level (mean_temp, mean_snow)
cov.tempo.arr <-
  array(
    NA,
    dim = c(
      n.covs.tempo,
      max(cov.tempo$new_grid_id),
      max(cov.tempo$ct_loc_label),
      max(cov.tempo$period)
    ),
    dimnames = list(
      1:n.covs.tempo,
      1:max(cov.tempo$new_grid_id),
      1:max(cov.tempo$ct_loc_label),
      1:max(cov.tempo$period)
    )
  )

# Filling up the array with information
for (i in 1:nrow(cov.tempo)) {
  this.grid.tempo <- cov.tempo[i,]$new_grid_id #120L
  this.loc.tempo <- cov.tempo[i,]$ct_loc_label #2L
  this.survey <- cov.tempo[i,]$period #292L
  cov.tempo.arr[, this.grid.tempo, this.loc.tempo, this.survey] <-
    as.numeric(cov.tempo[i, 3:4]) # Here we need to supply the columns for the covariates
}
# Again, the order is the same as the order of the column
# 1 = mean_tempS, 2 = mean_snowS

# Create the vector which tells the model how many camera traps there is in the grid cell
table(labels$new_grid_id)
n.traps <- as.numeric(table(labels$new_grid_id))
n.traps

# Put everything into the data object
data <- list(
  y = y,
  n.years = dim(y)[1],
  n.cells = dim(y)[2],
  n.period = dim(y)[4],
  n.traps = n.traps,
  cov.grid = cov.grid.mat,
  cov.grid.clearcut = cov.grid.clearcut.arr,
  cov.grid.old = cov.grid.old.arr,
  cov.loc = cov.loc.arr,
  cov.loc.clearcut = cov.loc.clearcut.arr,
  cov.loc.old = cov.loc.old.arr,
  cov.tempo = cov.tempo.arr
)


#########################################################################
## The multi-scale occupancy model
#########################################################################

cat(file="PM-Mocc-20220812.txt", 
    "model{
  # Priors and model for params
  for(yy in 1:n.years){
    psi0[yy]~dunif(0,1) # all prob are between 0 and 1 
    theta0[yy] ~ dunif(0,1)
    p0[yy] ~ dunif(0,1)
    int.psi[yy]<-logit(psi0[yy])            #Intercepts for occupancy probability at grid cell level
    int.theta[yy] <- logit(theta0[yy])        #Intercepts for site-use probability at the camera trap level
    int.p[yy] <- logit(p0[yy])                #Intercepts for detection probability
    
  }
  
 prior.test ~ dnorm(0,0.2) #add parameters for flat prior and use this to plot with posteriors later 
   
  #Occupancy prob. at grid cell
  #####Continuous covariates - add slopes
  beta.focov.psi ~ dnorm(0,0.2) #slope for covatiate (forest cover)
  beta.clearcut.psi ~ dnorm(0,0.2) #slope for covatiate (clearcut) 
  beta.old.psi ~ dnorm(0,0.2) #slope for covatiate (mature/old forest) 
  
  ##### Categorical covariates
  beta.area.psi[1] <- 0       # First level is reference category
  for(i in 2:5) {                    #Agdtel = 1, Hedmark = 2, Nomøst =3, NorlandS =4, Troms=5
    beta.area.psi[i] ~ dnorm(0,0.2) } # Difference between each study area and other 
    
  
  #Site-use prob. at camera trap level 
  ##### Continuous covariates - add slopes
  beta.rug.theta ~ dnorm(0,0.2) #slope for covariate (rug)
  
  ##### Categorical covariates
  beta.clearcut.theta[1] <- 0       # First level is reference category
  for(i in 2) {                    # within 100m radius: no clearcut = 1, clearcut =2
    beta.clearcut.theta[i] ~ dnorm(0,0.2) } # Difference between each cover type and other 
    
  beta.old.theta[1] <- 0       # First level is reference category
  for(i in 2) {                    # within 100m radius: no old forest = 1, old forest =2
    beta.old.theta[i] ~ dnorm(0,0.2) } # Difference between each cover type and other 
  
  
  #Detection prob. at camera trap level 
  ##### Continuous covariates - add slopes
  beta.temp.p ~ dnorm(0,0.2) #slope for covariate (temp) 
  beta.snow.p ~ dnorm(0,0.2) #slope for covariate (snow) #relationship with snow and non clearcut sites only
  beta.int.snow.clearcut.p ~ dnorm(0,0.2) #relationship with snow and clearcut sites only
  
  ##### Categorical covariates
  beta.clearcut.p[1] <- 0       # First level is reference category
  for(i in 2) {                    # within 100m radius: no clearcut = 1, clearcut =2
    beta.clearcut.p[i] ~ dnorm(0,0.2) } # Difference between no clearcut and clearcut 
    
  beta.feature.p[1] <- 0       # First level is reference category, 
  for(i in 2:4) {              # boulders = 1, dense = 2, open =3, sparse = 4
    beta.feature.p[i] ~ dnorm(0,0.2) } # Difference between each feature type and the reference feature
    

  #Likelihood (for model structure)
  for(yy in 1:n.years){
    for(i in 1:n.cells){
      #Occupancy in grid cell i for year yy
      z[yy,i] ~ dbern(psi[yy,i])
      logit(psi[yy,i]) <- int.psi[yy] 
      + beta.focov.psi*cov.grid[i,1]
      + beta.clearcut.psi*cov.grid.clearcut[1,i,yy] 
      + beta.old.psi*cov.grid.old[1,i,yy] 
      + beta.area.psi[cov.grid[i,2]] 
      
        for(j in 1:n.traps[i]){         # n.traps is a vector with the number of camera traps in each cell
        # Site-use at camera location j for year yy
        z.su[yy,i,j] ~ dbern(mu.su[yy,i,j])
        mu.su[yy,i,j] <- z[yy,i] * theta[yy,i,j]
        logit(theta[yy,i,j]) <- int.theta[yy] 
        + beta.rug.theta*cov.loc[1,i,j]                         # controlling covariate 
        + beta.clearcut.theta[cov.loc.clearcut[1,i,j,yy]]       # no clearcut, clearcut
        + beta.old.theta[cov.loc.old[1,i,j,yy]]                 # no old forest, old forest
      
          for(k in 1:n.period){ 
          # detection probability at camera location j in period k and year yy
          y[yy,i,j,k] ~ dbern(mu.y[yy,i,j,k])
          mu.y[yy,i,j,k] <- z.su[yy,i,j] * p[yy,i,j,k]
          logit(p[yy,i,j,k]) <- int.p[yy]
          + beta.feature.p[cov.loc[2,i,j]]      # correction factor
          + beta.temp.p*cov.tempo[1,i,j,k]      # correction factor ability to detect depending on biology and camera performance
          + beta.snow.p*cov.tempo[2,i,j,k]      # relationship with snow and non clearcut sites only
          + beta.clearcut.p[cov.loc.clearcut[1,i,j,yy]] 
          + beta.int.snow.clearcut.p*equals(cov.loc.clearcut[1,i,j,yy],2)*cov.tempo[2,i,j,k]      #Sites with clearcut only*continuous snow
         
          
        } # End period
      } # End camera trap
    } # End grid cell
  } # End year
} # End model
")


# MCMC settings
ni <- 60000; nt <- 10; nb <- 20000; nc <- 3

# Initial values
inits <- function() {
  list(
    z = apply(y[, , , ], c(1, 2), function(x) {
      ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
    }),
    z.su = apply(y[, , , ], c(1, 2, 3), function(x) {
      ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
    })
  )
}

# Parameters monitored
params <- c("int.psi", "int.theta", "int.p", 
            "beta.focov.psi",
            "beta.clearcut.psi",
            "beta.old.psi",
            "beta.area.psi",
            "beta.rug.theta",
            "beta.clearcut.theta",
            "beta.old.theta",
            "beta.feature.p",
            "beta.temp.p",
            "beta.snow.p",
            "beta.clearcut.p",
            "beta.int.snow.clearcut.p",
            "prior.test")

#########################################################################
## Fit the model
a <- Sys.time()
cat("Start Time \n ")
Sys.time()


library(jagsUI)
outJ.Mocc <- jagsUI::jags(data, inits, params, "PM-Mocc-20220812.txt", n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb, parallel=T)
## Save model output
save(outJ.Mocc,file="outJ-Mocc-20220812.RData")
#traceplot(b)


cat("End Time \n ")
Sys.time()

cat("Duration of Analysis \n ")
Sys.time()-a
