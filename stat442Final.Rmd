---
title: "STAT 442 Final Project"
subtitle: "Joyce Wangsa"
output:
  pdf_document:
    keep_tex: yes
    number_sections: no
  html_document:
    toc: yes
  word_document: default
urlcolor: blue
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(plotly)
library(treemapify)
library(ggplot2)
library(countrycode)
#opts_chunk$set(tidy.opts=list(width.cutoff=70),tidy=TRUE)
```

## Data Cleaning 
```{r}
dat <- read.csv(file = "~/Downloads/layoffs_data.csv", header = TRUE)

#Dropping duplicate rows (companies)
df <- dat[!duplicated(dat$Company), ]

#Assigning continents for respective countries
df$Continent <- countrycode(sourcevar = df[, "Country"],
                            origin = "country.name",
                            destination = "continent")
```

\newpage

## 2D/3D Continuous - Treemap
```{r}
#Dropping NA values of Funds_Raised
x1 <- df %>% drop_na(Funds_Raised)
x <- x1 %>% drop_na(Percentage)

#Grouping the rows in the data by the Location column
a <- group_by(x, Location)

#Summarizing the sum of funds raised per location, alphabetical order by default
b1 <- summarise(a, sum(Funds_Raised))
b2 <- summarise(a, mean(Percentage))
b1b2 <- merge(b1, b2)

#Deriving the top 20 locations with largest sum of funds raised
c <- b1b2[order(b1b2$`sum(Funds_Raised)`, decreasing = TRUE),][1:20,] 

#Listing the countries the companies belong to
countri <- c("United States", "United States", "United States","United States",
             "United States", "India", "United Kingdom", "Singapore", "United States",
             "Indonesia", "Germany", "Brazil", "United States", "United States",
             "United States", "Canada", "United States", "India", "United States",
             "Israel") 

#Binding respective countries to the rightmost column of the top 20 list
y <- cbind(c, countri) 
```

```{r}
#Plotting the Treemap
z <- ggplot(y, aes(area = `sum(Funds_Raised)`, label = Location, 
                   fill = `mean(Percentage)`, subgroup = countri))+
      geom_treemap(colour="#7E7E7E")+
      geom_treemap_text(colour = "white", place = "bottomleft", reflow = T,
                        fontface="bold")+
      geom_treemap_subgroup_border(colour = "#7E7E7E") +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, 
                                 colour = "black", fontface = "bold.italic", 
                                 min.size = 0)+
      scale_fill_viridis_c(option="magma", direction=-1)

z
```

\newpage

## Categorical - Violin Plot

```{r}
x <- df %>% drop_na(Laid_Off_Count)
```

```{r}
p <- ggplot(x, aes(x = Continent, y = Laid_Off_Count, fill = Continent)) + 
  geom_violin(trim = F) + 
  geom_boxplot(width=0.075, fill="white") +
  ylim(-250, 1250) + 
  theme(legend.position="none")+
  scale_fill_manual(values=c("#FFD275", "#E8AE68", "#A57F60", "#E3A587", "#DB5A42"))

p
```

\newpage

## Homebrew - Skewed Mosaic

```{r eval=FALSE}
#Tabulating the count of values for the columns Industry and Stage 
table(df$Industry)[order(table(df$Industry), decreasing = TRUE)]
table(df$Stage)[order(table(df$Stage), decreasing = TRUE)]
```

```{r}
#Taking the top 6 categories from each attribute, excluding Unknown and subsetting the data accordingly
x0 = subset(df, Industry %in% c("Finance","Retail","Healthcare", "Marketing", "Food", "Transportation"))
y0 = subset(df, Stage %in% c("Series A","Series B","Series C", "Series D","IPO", "Acquired"))

mxy <- merge(x0, y0)

x <- mxy$Industry
y <- mxy$Stage
N = nrow(mxy)

prop.table(table(x, y), 2)
```

```{r}
skewed.mosaic = function(x,y,skew=TRUE)
{
gap = 0.02
if(skew){gap = 0}

# Establish bounds
plot(0:1, 0:1, type="n", 
     xlim=c(-0.2, 1 + 1*skew), ylim=c(-0.2,1), 
     axes=FALSE, ylab="", xlab="")

# Get column widths and positions
mp_width = table(x) / N
mp_x = c(0, cumsum(mp_width))

# Get text positions and apply text for columns
text_x_pos = (mp_x[-1] + mp_x[-length(mp_x)])/2
text_x_lab = names(mp_width)
text(x=text_x_pos-0.125, y=-0.11, labels=text_x_lab, srt = 22.5, cex=0.78)

# Get text positions and apply text for rows
mp_height = table(y)/N
mp_y = c(0, cumsum(mp_height))
text_y_pos = (mp_y[-1] + mp_y[-length(mp_y)])/2
text_y_lab = names(mp_height)
text(x=-0.17 + 1*skew*text_y_pos, y=text_y_pos, labels=text_y_lab, cex=0.78)

# For each column, draw the parallelograms (skew) or rectangles (no skew)
for(k in 1:length(mp_width))
{
  
  # Get the polygon heights
	mp_height = prop.table(table(x,y), 2)[,k]
	mp_y = c(0, cumsum(mp_height))

	# For each polygon in this row
	for(j in 1:length(mp_height))
	{
	  # If it's nonzero height
		if(mp_height[j] != 0 & mp_width[k] != 0)
		{

		# Draw a rectangle for no skew	
		if(!skew)
		{
		polygon( x=mp_x[k + c(0,1,1,0)] + gap*c(1,-1,-1,1), 
				 y=mp_y[j + c(0,0,1,1)] + gap*c(1,1,-1,-1), 
				 col="#D0B8A8", border="#85586F", lwd=2)
		}	 
			
		# Or a parallelogram for skew
		if(skew)
		{
		polygon( x=mp_x[k + c(0,1,1,0)] + mp_y[j + c(0,0,1,1)], 
		 y=mp_y[j + c(0,0,1,1)] , 
		 col="#D0B8A8", border="#85586F", lwd=2)
		}
		}
	}
}
}

skewed.mosaic(x,y)
```

\newpage

Distribution of layoffs 

## Wildcard - Circular Barplot

```{r}
#Dropping NA values of percentage laid off
p <- df %>% drop_na(Laid_Off_Count)
p <- df %>% drop_na(Funds_Raised)

#Want to know the industries with highest funds raised
agg <- aggregate(p$Funds_Raised, by=list(Category=p$Industry), FUN=sum)
agg[order(agg$x, decreasing = TRUE),][1:4,]

#Subsetting data, only want rows from top 4 industries in terms of total funds raised
p1 <- subset(p, Industry %in% c("Media", "Transportation", "Consumer", "Finance"))

#Taking the top 60 largest companies in terms of number of employees
dt <- p1[order(-(p1$Laid_Off_Count)),][3:62,]
dt <- dt[c("Company", "Industry", "Laid_Off_Count")]
```

```{r}
dt$Industry <- as.factor(dt$Industry)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar*nlevels(dt$Industry), ncol(dt)))
colnames(to_add) <- colnames(dt)
to_add$Industry <- rep(levels(dt$Industry), each=empty_bar)
dt <- rbind(dt, to_add)
dt <- dt %>% arrange(Industry)
dt$id <- seq(1, nrow(dt))
 
# Get the name and the y position of each label
label_data <- dt
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# prepare a data frame for base lines
base_data <- dt %>% 
  group_by(Industry) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
 
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
```

```{r}
q <- ggplot(dt, aes(x=as.factor(id), y=Laid_Off_Count, fill=Industry)) +       
  
  geom_bar(stat="identity", alpha=0.5) +
  
  geom_segment(data=grid_data, aes(x = end, y = 1600, xend = start, yend = 1600), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1200, xend = start, yend = 1200), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  annotate("text", x = rep(max(dt$id),4), y = c(400,800,1200,1600), 
           label = c("400", "800", "1200", "1600") , color="black", size=1.7 , 
           angle=0, fontface="bold", hjust=1) +
  ylim(-400,1800) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(-1.5,-1.5,-1.5,-1.5, "cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=Laid_Off_Count+25, label=Company, 
                                 hjust=hjust),
            color="black", fontface="bold",alpha=0.6, size=1.9, 
            angle=label_data$angle, inherit.aes = FALSE) +

  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), 
               colour = "black", alpha=0.37, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Industry), 
            hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=1.9, 
            fontface="bold", inherit.aes = FALSE, nudge_y = c(0,-100,0,0))+
  scale_fill_manual(values=c("#6C698D", "#BFAFA6", "#6E6A6F", "#AA968A"))
q
```


