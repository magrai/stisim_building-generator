
# Resources and initialising ----------------------------------------------

#library(xlsx)
library(dplyr)
library(ggplot2)

set <- c()
vis <- c()
dir_data <- "data"

set.seed(42)


# Settings ----------------------------------------------------------------

## Object group
set$event <- "BLDG" # "BLDG". "A", "V", "CT"

## AB DIESER DISTANZ WERDEN DIE GEBÄUDE IN DIE WELT GEBAUT (EGAL WO)
set$onDist <- 0 # long. position at which object group initially appear

## SO LANG SOLL DIE OBJEKTGRUPPE SEIN
set$space4Obj <- 300 # length of object group
set$roadSide <- "left" # road side: left vs. rights
set$dir <- "longitudinal" # direction into which object group is built: longitudinal vs. lateral

## When object direction == longitudinal
## AN DIESER POSITION BEGINNT DIE OBJEKT-GRUPPE
set$longPos4Onset <- 300 # long. position at which object group begin

## When object direction == lateral: 
set$latDir <- "left" #left vs. right

## WO IST DIE KREUZUNG?
set$longPosIntersect <- 930 # long. distance to align object group to
set$latSpaceAdjust <- 15

## Which object classes should be considered?
### Business B. (B), General B. (G), Houses (H), Skyscrapers (S), Utility (U)
set$classes <- c("S")
set$modelIndices2remove < c() # exclusion of object codes

## Range of distances between objects
set$dist2nextObj_min <- 0 # 5
set$dist2nextObj_max <- 10  #10
set$dist2nextObj_step <- 0.1

## Roadway
set$road_laneWidth <- 3.66
set$road_laneNumber <- 6
set$road_laneNumberRight <- 3

## Sidewalk
set$sidewalk_width <- 10 # width of pedestrian way



# Settings intern (do not change) -----------------------------------------

## Number of left hand side lanes
set$road_laneNumberLeft <- set$road_laneNumber - set$road_laneNumberRight


## Distances between buildings
set$dist2nextObj <- 
  seq(set$dist2nextObj_min, set$dist2nextObj_max, set$dist2nextObj_step) 


## Settings for object placement parallel to road

if (set$roadSide == "left")  
  set$dist2RoadDivLine_min <- 
    (set$road_laneNumberLeft * set$road_laneWidth + set$sidewalk_width)

if (set$roadSide == "right")
  set$dist2RoadDivLine_min <- 
    set$road_laneNumberRight * set$road_laneWidth + set$sidewalk_width

set$dist2RoadDivLine_max <- set$dist2RoadDivLine_min + 1
set$dist2RoadDivLine_step <- 0.1 

set$dist2RoadDivLine <-
  seq(set$dist2RoadDivLine_min, 
      set$dist2RoadDivLine_max, 
      set$dist2RoadDivLine_step) 


## Heading angle (rotation) when building in longitudinal direction
if (set$dir == "longitudinal" & 
    set$roadSide == "right") headingAngle <- 0
if (set$dir == "longitudinal" & 
    set$roadSide == "left") headingAngle <- 180

## Heading angle (rotation) when building in lateral direction
if (set$dir == "lateral" & 
    set$latDir == "left" & 
    set$roadSide == "left") headingAngle <- 270

if (set$dir == "lateral" & 
    set$latDir == "left" & 
    set$roadSide == "right") headingAngle <- 90

if (set$dir == "lateral" & 
    set$latDir == "right" & 
    set$roadSide == "left") headingAngle <- 90

if (set$dir == "lateral" & 
    set$latDir == "right" & 
    set$roadSide == "right") headingAngle <- 270




# Settings for vehicles (additional) --------------------------------------

if (set$event %in% c("A", "V", "CT")) {
  set$dist2RoadDivLine <- set$dist2RoadDivLine - set$sidewalk_width
}

if (set$event == "CT" & set$latDir == "left" & set$roadSide == "right")
  set$CTdir <- "R"
if (set$event == "CT" & set$latDir == "left" & set$roadSide == "left")
  set$CTdir <- "L"
if (set$event == "CT" & set$latDir == "right" & set$roadSide == "left")
  set$CTdir <- "R"
if (set$event == "CT" & set$latDir == "right" & set$roadSide == "right")
  set$CTdir <- "L"




# Adjust dist2RoadDivLine by lateral side ---------------------------------

if (set$dir == "longitudinal" & set$roadSide == "left")
  set$dist2RoadDivLine <- set$dist2RoadDivLine * -1




# Prepare data ------------------------------------------------------------

if (set$event == "BLDG") {
  
  ## Load and adjust data variable type
  file_path <- file.path(dir_data, paste0(set$event, ".csv"))
  data4Obj <- read.csv(file_path)
  #data4Obj <- read.xlsx(paste(set$event, ".xlsx", sep = ""), header = T, 1)
  data4Obj$model_index <- as.character(data4Obj$model_index)
  
  ## Convert feet into metres
  data4Obj[, c("width", "length", "height")] <- 
    data4Obj[, c("width", "length", "height")] * 0.3048
}

if (set$event %in% c("A", "V", "CT")) {
  data4Obj <- data.frame(
    model_index = c(1:3),
    width = 2,
    length = 3)
  
  temp <- data4Obj$width
  data4Obj$width <- data4Obj$length
  data4Obj$length <- temp
  rm(temp)
}

## Create list of possible model indices
possModelIndices <- data4Obj$model_index

## Remove unwanted buildings
possModelIndices <- possModelIndices[!possModelIndices %in% set$codes2remove]



# Create block of objects -------------------------------------------------

objNr <- 0 ## Object counter
usedSpace <- 0
objTable <- c()

while (usedSpace < set$space4Obj) {
  
  ## Increment object counter
  objNr <- objNr + 1
  
  ## Choose random model
  modelIndex <- sample(possModelIndices, 1)
  
  ## Choose random distance to next object
  dist2nextObj <- sample(set$dist2nextObj, 1)
  
  ## Compute used space
  modelWidth <- data4Obj$width[data4Obj$model_index == modelIndex]
  usedSpace <- usedSpace + modelWidth + dist2nextObj
  
  ## Create feature vector
  objVector <- data.frame(objNr, modelIndex)
  objVector <- left_join(objVector, data4Obj, 
                         by = c("modelIndex" = "model_index"))
  objVector <- cbind(objVector, dist2nextObj, usedSpace)
  
  objTable <- rbind(objTable, objVector)
  
}

## Remove last object, due to while loop
objTable <- objTable[-nrow(objTable), ]
 


# Set longitudinal position -----------------------------------------------

## When building in longitudinal direction
if (set$dir == "longitudinal") {
  
  ## First object
  objTable$longPos[1] <- 
    set$longPos4Onset + set$sidewalk_width + 
    objTable$width[1]/2
  
  ## Last object
  objTable$longPos[max(objTable$objNr)] <- 
    set$longPos4Onset + set$space4Obj - set$sidewalk_width - 
    objTable$width[max(objTable$objNr)]/2
  
  if (max(objTable$objNr) > 2)
  ## All objects between first and last object
  for (objNr in 2:(max(objTable$objNr) - 1)) {
    objTable$longPos[objNr] <- 
      objTable$longPos[objNr - 1] + objTable$width[objNr - 1]/2 +
      objTable$dist2nextObj[objNr - 1] +
      objTable$width[objNr]/2
  }
}


## When building in lateral direction ...
## ... add deviation to longitudinal position
if (set$dir == "lateral")
  positionBlur <- sample(set$dist2RoadDivLine, max(objTable$objNr), replace = T)

## When building in lateral direction to the left
if (set$dir == "lateral" & set$latDir == "left") {
  
  if (set$roadSide == "left")
    objTable$longPos <- set$longPosIntersect - positionBlur
  if (set$roadSide == "right")
    objTable$longPos <- set$longPosIntersect + positionBlur
}

## INFO: WHEN ROTATION ON LEFT SIDE NO ADJUSTMENT FOR LENGTH NEEDED!
## (no clue why ...: must be dependent on pos/neg lateral position)

## When building in lateral direction to the right
if (set$dir == "lateral" & set$latDir == "right") {
  if (set$roadSide == "left") {
    objTable$longPos <- set$longPosIntersect + positionBlur + objTable$length
    if (set$event %in% c("A", "V", "CT"))
      objTable$longPos <- set$longPosIntersect + positionBlur
  }
  if (set$roadSide == "right") {
    objTable$longPos <- set$longPosIntersect - positionBlur - objTable$length
    if (set$event %in% c("A", "V", "CT"))
      objTable$longPos <- set$longPosIntersect - positionBlur
  }
}
 

## Adjust longitudinal position to on-distance
objTable$longPos <- objTable$longPos - set$onDist



# Set lateral position ----------------------------------------------------

## When building in longitudinal direction
if (set$dir == "longitudinal") {
  objTable$latPos <- 
    sample(set$dist2RoadDivLine, max(objTable$objNr), replace = T)
  
  if (set$event == "BLDG" & set$roadSide == "left")
    objTable$latPos <- objTable$latPos - objTable$length
}


## When building in lateral direction

if (set$dir == "lateral" & set$latDir == "left") {
  
  ## First object
  objTable$latPos[1] <- 
    (set$latSpaceAdjust + objTable$width[1]/2) * -1
  
  ## Last object
  objTable$latPos[max(objTable$objNr)] <- 
    (set$space4Obj - set$latSpaceAdjust - 
    objTable$width[max(objTable$objNr)]/2) * -1
  
  if (max(objTable$objNr) > 2)
    ## All objects between first and last object
    for (objNr in 2:(max(objTable$objNr) - 1)) {
      objTable$latPos[objNr] <- 
        objTable$latPos[objNr - 1] - objTable$width[objNr - 1]/2 - 
        objTable$dist2nextObj[objNr - 1] - 
        objTable$width[objNr]/2
    }
}


if (set$dir == "lateral" & set$latDir == "right") {
  
  ## First object
  objTable$latPos[1] <- 
    set$latSpaceAdjust + objTable$width[1]/2
  
  ## Last object
  objTable$latPos[max(objTable$objNr)] <- 
    set$space4Obj - set$latSpaceAdjust - objTable$width[max(objTable$objNr)]/2
  
  if (max(objTable$objNr) > 2)
    ## All objects between first and last object
    for (objNr in 2:(max(objTable$objNr) - 1)) {
      objTable$latPos[objNr] <- 
        objTable$latPos[objNr - 1] + objTable$width[objNr - 1]/2 + 
        objTable$dist2nextObj[objNr - 1] + 
        objTable$width[objNr]/2
    }
}



# Variables for visualisation ---------------------------------------------

## When building in longitudinal direction
if (set$dir == "longitudinal") {
  
  ## For visualisation of roadway
  vis$roadEdgeLeft <- 0 - set$road_laneNumberLeft * set$road_laneWidth
  vis$roadEdgeRight <- 0 + set$road_laneNumberRight * set$road_laneWidth
  
  ## For visualisation of sidewald
  vis$sidewalkLeft <- 
    0 - set$road_laneNumberLeft * set$road_laneWidth - set$sidewalk_width
  vis$sidewalkRight <- 
    0 + set$road_laneNumberRight * set$road_laneWidth + set$sidewalk_width
  
  ## For visualisation of striping
  for (lane in c(1:set$road_laneNumberLeft) - 1) {
    vis$stripingLeft <- 0 - lane * set$road_laneWidth
  }
  for (lane in c(1:set$road_laneNumberRight) - 1) {
    vis$stripingRight <- 0 + lane * set$road_laneWidth
  }
  
  ## For visualisation of objects
  if (set$roadSide == "left") {
    objTable$vis_xmin <- objTable$latPos + objTable$length
    objTable$vis_xmax <- objTable$latPos
  }
  if (set$roadSide == "right") {
    objTable$vis_xmin <- objTable$latPos
    objTable$vis_xmax <- objTable$latPos + objTable$length
  }
  objTable$vis_ymin <- objTable$longPos - objTable$width/2
  objTable$vis_ymax <- objTable$longPos + objTable$width/2
  
  ## For visualisation of position limits
  vis$onset <- set$longPos
  vis$onsetRange <- set$longPos + set$sidewalk_width
  vis$ending <- set$longPos + set$space4Obj
  vis$endingRange <- set$longPos + set$space4Obj - set$sidewalk_width
  
  
}


## When building in lateral direction

## For visualisation of roadway deviding line
if (set$dir == "lateral") vis$roadDiv <- set$longPosIntersect

if (set$dir == "lateral" & set$latDir == "left") {
  
  ## For visualisation of roadway
  vis$roadEdgeLeft <- 
    vis$roadDiv - set$road_laneNumberLeft * set$road_laneWidth
  vis$roadEdgeRight <- 
    vis$roadDiv + set$road_laneNumberRight * set$road_laneWidth
  
  ## For visualisation of sidewalk
  vis$sidewalkLeft <- vis$roadEdgeLeft - set$sidewalk_width
  vis$sidewalkRight <- vis$roadEdgeRight + set$sidewalk_width
  
  ## For visualisation of striping
  for (lane in c(1:set$road_laneNumberLeft) - 1) {
    vis$stripingLeft <- 
      vis$roadDiv - lane * set$road_laneWidth
  }
  for (lane in c(1:set$road_laneNumberRight) - 1) {
    vis$stripingRight <- 
      vis$roadDiv + lane * set$road_laneWidth
  }
  
  ## For visualisation of position limits
  vis$onset <- 0
  vis$onsetRange <- 0 - set$latSpaceAdjust
  vis$ending <- 0 - set$space4Obj
  vis$endingRange <- 0 - set$space4Obj + set$latSpaceAdjust
  
  ## For visualisation of objects
  if (set$roadSide == "left") {
    objTable$vis_xmin <- objTable$latPos + objTable$width/2
    objTable$vis_xmax <- objTable$latPos - objTable$width/2
    objTable$vis_ymin <- objTable$longPos
    objTable$vis_ymax <- objTable$longPos - objTable$length
  }
  if (set$roadSide == "right") {
    objTable$vis_xmin <- objTable$latPos + objTable$width/2
    objTable$vis_xmax <- objTable$latPos - objTable$width/2
    objTable$vis_ymin <- objTable$longPos
    objTable$vis_ymax <- objTable$longPos + objTable$length
  }
  
}


if (set$dir == "lateral" & set$latDir == "right") {
  
  ## For visualisation of roadway
  vis$roadEdgeLeft <- vis$roadDiv + set$road_laneNumberLeft * set$road_laneWidth
  vis$roadEdgeRight <- vis$roadDiv - set$road_laneNumberLeft * set$road_laneWidth
  
  ## For visualisation of sidewalk
  vis$sidewalkLeft <- vis$roadEdgeLeft + set$sidewalk_width
  vis$sidewalkRight <- vis$roadEdgeRight - set$sidewalk_width
  
  ## For visualisation of striping
  for (lane in c(1:set$road_laneNumberLeft) - 1) {
    vis$stripingLeft <- 
      vis$roadDiv + lane * set$road_laneWidth
  }
  for (lane in c(1:set$road_laneNumberRight) - 1) {
    vis$stripingRight <- 
      vis$roadDiv - lane * set$road_laneWidth
  }
  
  ## For visualisation of position limits
  vis$onset <- 0
  vis$onsetRange <- 0 + set$latSpaceAdjust
  vis$ending <- 0 + set$space4Obj
  vis$endingRange <- 0 + set$space4Obj - set$latSpaceAdjust
  
  ## For visualisation of objects
  if (set$roadSide == "left") {
    objTable$vis_xmin <- objTable$latPos - objTable$width/2
    objTable$vis_xmax <- objTable$latPos + objTable$width/2
    objTable$vis_ymin <- objTable$longPos
    objTable$vis_ymax <- objTable$longPos - objTable$length
  }
  if (set$roadSide == "right") {
    objTable$vis_xmin <- objTable$latPos - objTable$width/2
    objTable$vis_xmax <- objTable$latPos + objTable$width/2
    objTable$vis_ymin <- objTable$longPos
    objTable$vis_ymax <- objTable$longPos + objTable$length
  }
}



# Visualisation of objects ------------------------------------------------

plotData <- 
  ggplot() + 
  geom_rect(data = objTable,
            aes(xmin = vis_xmin,
                xmax = vis_xmax,
                ymin = vis_ymin,
                ymax = vis_ymax,
                group = objNr))



# Visualisation area ------------------------------------------------------

if (set$dir == "longitudinal")
  plotData <- 
    plotData + 
    coord_cartesian(xlim = c(-250, 250),
                    ylim = c(set$longPos4Onset - 50, 
                             set$longPos4Onset + set$space4Obj + 50))

if (set$dir == "lateral" & set$latDir == "left")
  plotData <- 
    plotData + 
    coord_cartesian(xlim = c((set$space4Obj + 100) * -1, 50),
                    ylim = c(set$longPosIntersect - 100, 
                             set$longPosIntersect + 100))

if (set$dir == "lateral" & set$latDir == "right")
  plotData <- 
    plotData + 
    coord_cartesian(xlim = c(-50, (set$space4Obj + 100)),
                    ylim = c(set$longPosIntersect - 100, 
                             set$longPosIntersect + 100))


# Visualisation of position limits ----------------------------------------

if (set$dir == "longitudinal")
  
  plotData <- 
    plotData +
    ## Onset and ending
    geom_hline(aes(yintercept = vis$onset), colour = "red") +
    geom_hline(aes(yintercept = vis$ending), colour = "red") +
    ## Range at onset and ending
    geom_hline(aes(yintercept = vis$onsetRange), 
               colour = "red", linetype = "dotted") + 
    geom_hline(aes(yintercept = vis$endingRange), 
               colour = "red", linetype = "dotted")

if (set$dir == "lateral")
  
  plotData <- 
    plotData +
    ## Onset and ending
    geom_vline(aes(xintercept = vis$onset), colour = "red") +
    geom_vline(aes(xintercept = vis$ending), colour = "red") +
    ## Range at onset and ending
    geom_vline(aes(xintercept = vis$onsetRange), 
               colour = "red", linetype = "dotted") + 
    geom_vline(aes(xintercept = vis$endingRange), 
               colour = "red", linetype = "dotted")



# Visualisation of road ---------------------------------------------------

if (set$dir == "longitudinal") {
  
  plotData <- 
    plotData +  
    ## Roadway deviding line
    geom_vline(aes(xintercept = 0)) + 
    ## Road way edge line
    geom_vline(aes(xintercept = vis$roadEdgeLeft), size = 1) + 
    geom_vline(aes(xintercept = vis$roadEdgeRight), size = 1) +  
    ## Sidewalk edge line left
    geom_vline(aes(xintercept = vis$sidewalkLeft), colour = "green4") + 
    geom_vline(aes(xintercept = vis$sidewalkRight), colour = "green4") + 
    ## Draw striping
    geom_vline(aes(xintercept = vis$stripingLeft), linetype = "dashed") +
    geom_vline(aes(xintercept = vis$stripingRight), linetype = "dashed")
}

if (set$dir == "lateral") {
  
  plotData <- 
    plotData +  
    ## Roadway deviding line
    geom_hline(aes(yintercept = vis$roadDiv)) + 
    ## Road way edge line on left hand side
    geom_hline(aes(yintercept = vis$roadEdgeLeft), size = 1) + 
    geom_hline(aes(yintercept = vis$roadEdgeRight), size = 1) + 
    ## Sidewalk edge line
    geom_hline(aes(yintercept = vis$sidewalkLeft), colour = "green4") + 
    geom_hline(aes(yintercept = vis$sidewalkRight), colour = "green4") +
    ## Draw striping
    geom_hline(aes(yintercept = vis$stripingLeft), linetype = "dashed") + 
    geom_hline(aes(yintercept = vis$stripingRight), linetype = "dashed")
}



# Plot output -------------------------------------------------------------

plot(plotData)



# Output for STISIM -------------------------------------------------------

## Select necessary variables
scriptVars <- c("longPos", "latPos", "modelIndex")

## Create object table with only script relevant data
if (set$event == "BLDG")
  objTable4Script <- data.frame(
    onDist = rep(set$onDist, max(objTable$objNr)),
    event = rep(set$event, max(objTable$objNr)),
    P1_longPos = objTable$longPos,
    P2_latPos = objTable$latPos,
    P3_modelIndex = objTable$modelIndex,
    P4_headingAngle = rep(headingAngle, max(objTable$objNr))
  )

if (set$event == "V")
  objTable4Script <- data.frame(
    onDist = rep(set$onDist, max(objTable$objNr)),
    event = rep(set$event, max(objTable$objNr)),
    P1_speed = rep(0, max(objTable$objNr)),
    P2_longPos = objTable$longPos,
    P3_lat = objTable$latPos,
    P4_brakingLight = rep(0, max(objTable$objNr)),
    P5_modelIndex = objTable$modelIndex
  )

if (set$event == "A")
  objTable4Script <- data.frame(
    onDist = rep(set$onDist, max(objTable$objNr)),
    event = rep(set$event, max(objTable$objNr)),
    P1_speed = rep(0, max(objTable$objNr)),
    P2_longPos = objTable$longPos,
    P3_latPos = objTable$latPos,
    P4_modelIndex = objTable$modelIndex
  )

if (set$event == "CT")
  objTable4Script <- data.frame(
    onDist = rep(set$onDist, max(objTable$objNr)),
    event = rep(set$event, max(objTable$objNr)),
    P1_longPos = objTable$longPos,
    P2_displayOptions = 5,
    P3_latPos = objTable$latPos,
    P4_speed = 0,
    P5_direction = rep(set$CTdir, max(objTable$objNr)),
    P6_modelIndex = objTable$modelIndex,
    P7_intelligent = 0
  )

## Create string
scriptBlock <- apply(objTable4Script, 1, paste, collapse = ", ")

## Commentary for STISIM script
cat(paste(
  paste("-1 Object block for length of ", 
        set$space4Obj, " m ", 
        sep = ""),
  paste("-1 ... begins at long. position: ", 
        set$longPos4Onset, 
        " m", sep = ""),
  paste("-1 ... in direction: ", 
        set$dir, 
        sep = ""),
  paste("-1 ... at road side: ", 
        set$roadSide, 
        sep = ""),
  sep = "\n"))
cat("\n")

if (set$dir == "lateral")
  cat(paste("-1 ... in lateral direction: ", set$latDir, sep = ""))

cat("\n")

## Output for copy-and-paste
cat(scriptBlock, sep = "\n")