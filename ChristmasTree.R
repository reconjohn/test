#' ---
#' title: "Happy B Day for D"
#' author: "Yohan"
#' date: "Nov. 21, 2019"
#' output:
#'   html_document:
#'     toc: true
#'     keep_md: true
#' ---

#+ r setup, echo= F, warning=F, message=F
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#+ echo= F
library(plotrix)

candle = function(pos)
{
  x=pos[1]
  y=pos[2]
  rect(x,y,x+.2,y+2,col="red")
  polygon(c(x+.05,x-.1,x+.1,x+.3,x+.15,x+0.05), c(y+2,y+2.3,y+2.6,y+2.3,y+2,y+2),col="#EECF08")
  
}

plot(c(0,10), c(0,10),type="n", bty="n",xaxt="n",yaxt="n", main="Happy B Day! D! for your 30 years old!", xlab="God's love always! - Yohan, Nov. 21, 2019",ylab="")
draw.ellipse(5,2,col="#73EA71",a=4.4,b=1.7,border=1)
draw.ellipse(5,2,col="#BAE5FF",a=4,b=1.4,border=1)
rect(1,2,9,5,col="#BAE5FF",border="#BAE5FF")
lines(c(1,1),c(2,5))
lines(c(9,9),c(2,5))
draw.ellipse(5,5,col="#FCD2DE",a=4,b=1.4)

# for(i in runif(30)*7+1.3){
#   for(j in runif(30)*2+3.5){
#     candle(c(i,j))
#   }
# }

candle(c(2.5,4.5))
candle(c(4,4.5))
candle(c(7,5.2))


ChristmasTree <- read.csv("https://raw.githubusercontent.com/t-redactyl/Blog-posts/master/Christmas%20tree%20base%20data.csv")

names(ChristmasTree)[1] <- "Tree.X"
save(ChristmasTree, file = "ChristmasTree.Rdata")

library(ggplot2)
tree <- ggplot() +
  geom_tile(data = ChristmasTree, aes(x = Tree.X, y = Tree.Y, fill = Tree.Colour)) +
  scale_fill_identity() +
  theme_bw() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "To D - Yohan, Nov. 21, 2019", y = "")

Desired.Lights <- 50
Total.Lights <- sum(round(Desired.Lights * 0.35) + round(Desired.Lights * 0.20) +
                      round(Desired.Lights * 0.17) + round(Desired.Lights * 0.13) +
                      round(Desired.Lights * 0.10) + round(Desired.Lights * 0.05))

Lights <- data.frame(Lights.X = c(
  round(runif(round(Desired.Lights * 0.35), 4, 18), 0),
  round(runif(round(Desired.Lights * 0.20), 5, 17), 0),
  round(runif(round(Desired.Lights * 0.17), 6, 16), 0),
  round(runif(round(Desired.Lights * 0.13), 7, 15), 0),
  round(runif(round(Desired.Lights * 0.10), 8, 14), 0),
  round(runif(round(Desired.Lights * 0.05), 10, 12), 0)
))
Lights$Lights.Y <- c(
  round(runif(round(Desired.Lights * 0.35), 4, 6), 0),
  round(runif(round(Desired.Lights * 0.20), 7, 8), 0),
  round(runif(round(Desired.Lights * 0.17), 9, 10), 0),
  round(runif(round(Desired.Lights * 0.13), 11, 12), 0),
  round(runif(round(Desired.Lights * 0.10), 13, 14), 0),
  round(runif(round(Desired.Lights * 0.05), 15, 17), 0)
)
Lights$Lights.Colour <- c(round(runif(Total.Lights, 1, 4), 0))

tree <- tree +
  geom_point(
    data = Lights, aes(x = Lights.X, y = Lights.Y, alpha = Lights.Colour),
    colour = "lightgoldenrodyellow", shape = 16
  ) +
  theme(legend.position = "none")

Baubles <- data.frame(Bauble.X = c(
  6, 9, 15, 17, 5, 13, 16, 7, 10, 14, 7, 9, 11,
  14, 8, 14, 9, 12, 11, 12, 14, 11, 17, 10
))
Baubles$Bauble.Y <- c(
  4, 5, 4, 4, 5, 5, 5, 6, 6, 6, 8, 8, 8, 8, 10,
  10, 11, 11, 12, 13, 10, 16, 7, 14
)
Baubles$Bauble.Colour <- factor(c(
  1, 2, 2, 3, 2, 3, 1, 3, 1, 1, 1, 2, 1, 2,
  3, 3, 2, 1, 3, 2, 1, 3, 3, 1
))
Baubles$Bauble.Size <- c(
  1, 3, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3,
  2, 3, 1, 1, 2, 2, 3, 3, 2
)



tree <- tree +
  geom_point(
    data = Baubles, aes(
      x = Bauble.X, y = Bauble.Y,
      colour = Bauble.Colour, size = Bauble.Size
    ),
    shape = 16
  ) +
  scale_colour_manual(values = c("firebrick2", "gold", "dodgerblue3")) +
  scale_size_area(max_size = 12)

tree <- tree +
  geom_segment(aes(x = 2.5, xend = 4.5, y = 1.5, yend = 1.5), colour = "blueviolet", size = 2) +
  geom_segment(aes(x = 5.5, xend = 8.5, y = 1.5, yend = 1.5), colour = "dodgerblue3", size = 2) +
  geom_segment(aes(x = 13.5, xend = 16.5, y = 1.5, yend = 1.5), colour = "blueviolet", size = 2) +
  geom_segment(aes(x = 17.5, xend = 19.5, y = 1.5, yend = 1.5), colour = "dodgerblue3", size = 2) +
  geom_segment(aes(x = 3.5, xend = 3.5, y = 0.5, yend = 2.5), colour = "blueviolet", size = 2) +
  geom_segment(aes(x = 7.0, xend = 7.0, y = 0.5, yend = 2.5), colour = "dodgerblue3", size = 2) +
  geom_segment(aes(x = 15.0, xend = 15.0, y = 0.5, yend = 2.5), colour = "blueviolet", size = 2) +
  geom_segment(aes(x = 18.5, xend = 18.5, y = 0.5, yend = 2.5), colour = "dodgerblue3", size = 2)

# library(extrafont)
# font_import()
# loadfonts()

tree1 <- tree +
  annotate("text",
           x = 11, y = 20, label = "JIA YOU D! Today, The NEW DAY!",
           family = "Luminari", size = 8
  )

tree1

#' ![A Tree of Hope](tree.jpg){width=700}
#' 
#' ![Happy Life Journey to Come](bike.jpg){width=700}
#' 
#' > * ## How do I get the best out of life?
#' * ## Face your past without regret.
#' * ## Handle your present with confidence.
#' * ## Prepare for the future without fear.
#' 
#' > * ## Keep the faith and drop the fear.
#' * ## Do not believe your doubts
#' * ## and never doubt your belief.
#' 
#' > * ## Trust God.
#' * ## Life is wonderful,
#' * ## if you know how to live.
#' 

