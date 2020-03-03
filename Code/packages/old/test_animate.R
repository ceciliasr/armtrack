library(ggplot2)
library(dplyr)
install.packages("animation")
library(animation)
install.packages("gganimate")
library(gganimate)
## Create data frame with values for both parametric equations ##
heartdf = data_frame(
  t = seq(0, 2*pi, pi/60),
  x = 16*sin(t)^3,
  y = 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t),
  x2 = 3*sqrt(2)*cos(t)/(sin(t)^2 + 1),
  y2 = 20 + 4*sqrt(2)*cos(t)*sin(t)/(sin(t)^2 + 1)
)
# Create frame variable for text #
textdf = data_frame(t = max(heartdf$t) + 1:25)
# Create plot #
p = ggplot(data = heartdf, 
           aes(x, y, frame = round(t, 1), cumulative = TRUE)) + 
  geom_path(aes(group = 1)) +
  geom_path(aes(group = 2, x = x2, y= y2), size = 2, colour = "red") +
  geom_polygon(aes(group = 1), fill = "red") +
  geom_text(aes(x = 0, y = 0, label = "Happy Valentine's Day \n Marioly!!!", 
                frame = t), data = textdf, size = 10, colour = "white")
ani.options(interval = 0.06) #animation speed, seconds per frame
gganimate(p, title_frame = FALSE)
