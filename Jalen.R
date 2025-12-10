library(babynames)
library(mdsr)
library(ggplot2)
library(dplyr)
BabynamesDist <- make_babynames_dist()
jalen <- BabynamesDist %>%
  filter(name == "Jalen" & sex == "M")
jaylen <- BabynamesDist %>%
  filter(name == "Jaylen" & sex == "M")
jaylin <- BabynamesDist %>%
  filter(name == "Jaylin" & sex == "M")
jaylon <- BabynamesDist %>%
  filter(name == "Jaylon" & sex == "M")
jaelen <- BabynamesDist %>%
  filter(name == "Jaelen" & sex == "M")
jailen <- BabynamesDist %>%
  filter(name == "Jailen" & sex == "M")
jalin <- BabynamesDist %>%
  filter(name == "Jalin" & sex == "M")
all <- rbind(jalen, jaylin, jaylon, jaylen, jaelen, jailen, jalin)
total <- all%>%
  group_by(year)%>%
  summarise(
    n=sum(n)
  )
all <- all[,c(1,3,4)]
total$name <- rep('Total', 35)
all <- rbind(all,total)
name_plot <- ggplot(data = all, aes(x = year))  # basic ggplot object (with x-axis) to build on
## adding line plot with estimated number of Jalens BORN each year
name_plot <- name_plot +
  geom_line(aes(y = n, color = name), size = 1.5)
## Adding a y-axis label
name_plot <- name_plot +
  ylab("Number of People") +
  xlab(NULL)
## Some text to add to plots:
context <- tribble(
  ~year, ~num_people, ~label,
  1989.5, 3500, "Jalen Rose debuts\nat Michigan",
  1994, 5000, "Jalen Rose drafted\nby the Denver Nuggets",
  2005, 3550, "Jalen Rose wins the\nNBAs Most Improved\nPlayer Award"
)
## Placing the text created above onto the graph (Fig. 3.22):
name_plot +
  ggtitle("Number of Jalen's Born Each Year Based on Different Spellings") +
  geom_text(
    data = context,
    aes(y = num_people, label = label),
    size = 6
  ) +
  geom_curve(
    x = 1989.5, xend = 1991, y = 3200, yend = 150,
    arrow = arrow(length = unit(0.5, "cm")), curvature = 0.3
  ) +
  geom_curve(
    x = 2002.5, xend = 2000.2, y = 3575, yend = 3575,
    arrow = arrow(length = unit(0.5, "cm")), curvature = 0
  ) +
  geom_curve(
    x = 1994, xend = 1994, y = 4700, yend = 1750,
    arrow = arrow(length = unit(0.5, "cm")), curvature = 0
  ) +
  ylim(0, 6500) +
  xlim(1987, 2010)
