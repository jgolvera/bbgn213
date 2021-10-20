#'--- 
#' Class 05: Data Visualization
#' Author: Jocelyn Olvera
#' Date: October 13 2021
#' Description: Today we are going to use ggplot2 package
#'---

# First we need to loud the package!
# Install.packages("ggplot2")
library(ggplot2)

# We will use this inbuilt "cars" dataset first
# "head" allows viewing of only a small subset of the data 
head(cars)

# All ggplots have at least 3 layers
# data + aes + geoms

ggplot(data=cars) +
  aes(x=speed, y=dist) + 
  geom_point()+ 
# Side-Note: geom_line()+ draws lines connecting data points  
  geom_smooth(method="lm")+
  labs(title="Stopping Distance of Old Cars",
    x="Speed(MPH)",
    y="Stopping Distance (ft)")

# Side-Note: ggplot is not the only graphics system
# A very popular is a good old "base" R graphics
plot(cars)
plot(cars$speed, cars$speed)

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# Q1.how many genes are in this dataset?
nrow(genes)

# Q2. how many genes are "up"?
# this function list all genes: genes$State
table(genes$State)

# Q3. What % are up?
# this updates as data updates, adding sig-figs is the #3
# round=rounding up percentage
round(table(genes$State)/nrow(genes)*100,2)

# Lets make a figure
# Sates are the down/up/unchanging gene expressions
p <- ggplot(genes) +
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()

p

# I like it but not the default colors, lets change them
# reminder to re-assign p in orde to keep the color change
p <- p + scale_colour_manual(values=c("blue", "orange", "red"))
p
# Changing the main title and titles of the X and Y axis
p + labs(title="Gene Expression Changes Upon Drug Treatment", x="Control (no drug)", y="Drug Treatment")

# Lets explore the gapminder dataset
# install.packages("gapminder")
library(gapminder)
head(gapminder)

# Lets make a new plot of year vs lifeExp
# running first 2 lines just displays plot layout
# alpha is transparency
# reminder "aes" aesthetics 
ggplot(gapminder) + 
  aes(x=year, y=lifeExp, col=continent) +
  geom_jitter(width=0.3, alpha=0.4) +
#  geom_boxplot(aes(group=year), alpha=0.4)
# mean line = quantiles
  geom_violin(aes(group=year),alpha=0.2, 
              draw_quantiles = 0.5)

# Install the plotly
# install.packages("plotly")
# plotly is an interactive plot function (AMAZING!)
# adding (#) to ggplotly to be able to save as PDF

# library(plotly)
# ggplotly()



