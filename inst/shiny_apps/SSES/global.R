

library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

BoatRes<<-c("No boats","Car top","Trailer")
MotorRes<<-c("Any","Less than 10hp","Elec. only")
GearRes<<-c("None","Bait ban","Barbless","Fly only")
TakeLim<<-c("No take","1 fish","4 fish","5+ fish")

mcols<<-c("black","darkgrey","orange","green","brown","darkgreen","red","blue")
