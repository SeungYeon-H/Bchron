#possible bayesian age-depth modeling, putting 'position' 
#positionthicknes... is it worth using this? 
ages = c(2500, 2300, 2000, 1800)
ageSds = c(30, 30, 30, 30)
 
positions = c(4, NA, NA, 1)  # Baeysian order -  'NA' where the position is unknown
 
positions[is.na(positions)] <- seq(min(positions, na.rm = TRUE), max(positions, na.rm = TRUE), length.out = sum(is.na(positions) + 2))[2:(sum(is.na(positions)) + 1)]
 
calCurves = rep("IntCal20", 4)

 
modelResults <- Bchronology(
  ages = ages,
  ageSds = ageSds,
  calCurves = calCurves,
  positions = positions,
)
