---
title: "Regression Models - Week-2"
author: "Saul Lugo"
date: "December 20, 2015"
output: html_document
---

#Building a Linear Model


```r
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha=0.2)
g = g + geom_point(size = 5, colour="blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour="black")
g
```

![plot of chunk lm_diamonds_example](figure/lm_diamonds_example-1.png) 
