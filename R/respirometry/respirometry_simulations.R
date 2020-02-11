# Respirometry simulation
# Copyright (c) 2020 Ph. Grosjean (Philippe.Grosjean@umons.ac.be)
# 
# There functions allows to simulate the effect of size and time parameters on
# the output of a respirometer given respiration, net photosynthesis and mass of
# an organism

SciViews::R
library(simecol)

# respire -----------------------------------------------------------------
# Simple model of a respirometer : linear increase/decrease of oxygen with time
# Here are the data:
## Respirometer characteristics
RespVol    <- 1.5   # Volume of the respirometer [L]
O2Ini      <- 8     # Initial O2 concentration in the respirometer [mg/L]
RespFlow   <- 0.03  # Water change rate when respirometer in open [L/min]
RespTime   <- 60    # Length in time of open-closed phase [min]
O2Flow     <- O2Ini # O2 concentration in change water [mg/L]
## Organism characteristics
RespRate   <- 0.05   # Respiration rate [mg O2/g/min].
PhotRate   <- 0.12   # Photosynthesis rate [mg O2/g/min]
Mass       <- 0.5    # Mass of the organism [g] 
## Day/night characteristic
LightTime  <- 12*60  # Length of light phase (on 24h)
## Min-max tolerated O2 values
O2Range    <- c(6, 12) # Min-max tolerated O2 concentration [mg/L]
## Time info
Times     <- c(from = 0, to = 52 * 60, by = 1) # Time [min]

# The model
respire <- new("odeModel",
  main = function(time, init, parms) {
    Vol <- init["Vol"]
    O2  <- init["O2"]
    with(as.list(parms), {
      dVol <- 0   # Volume of the respirometer does not change
      O2Rate <- RespRate
      # Is light on?
      LightOn <- time %% (24 * 60) <= LightTime
      # If yes, also consider photosynthesis
      if (LightOn) O2Rate <- RespRate - PhotRate
      dO2 <- -O2Rate / Vol
      # Is the respirometer closed?
      RespOpen <- as.logical(time %/% RespTime %% 2)
      if (is.na(RespOpen)) RespOpen <- FALSE
      # If the respirometer is open, also consider water change
      if (RespOpen) dO2 <- dO2 + RespFlow * (O2Flow - O2) / Vol
      # Avoid negative values for O2
      #if (O2 + dO2 < 0) dO2 <- -O
      list(c(dVol, dO2))
    })
  },
  equations = list(),
  times = Times,
  parms = c(RespRate = RespRate * Mass, PhotRate = PhotRate * Mass,
    RespTime = RespTime, RespFlow = RespFlow, O2Flow = O2Flow,
    LightTime = LightTime),
  init = c(Vol = RespVol, O2 = O2Ini),
  solver = "lsoda"
)

respire <- sim(respire)
res <- out(respire)

# O2 plot
chart(data = res, O2 ~ time) +
  geom_line() +
  geom_hline(yintercept = O2Range, col = "red", lty = 2) +
  xlab("Time [min]") +
  ylab("O2 [mg/L]")

# When do we reach O2Max or O2Min (in hours)?
(res$time[res$O2 > O2Range[2]][1]) / 60
(res$time[res$O2 < O2Range[1]][1]) / 60
