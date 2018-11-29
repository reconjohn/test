#' ---
#' title: "Data visualization"
#' author: "Yohan"
#' output: 
#'   html_document:
#'     preserve_yaml: true
#'     toc: true
#'     toc_float: true
#'     keep_md: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)

#+ r
## gapminder
str(gapminder)
ggplot(data = gapminder, aes(x = year, y = lifeExp,
                             group = country, color = continent)) +
  geom_line(alpha = 0.5) + 
  facet_wrap( ~ continent) +
  xlab("Year") + ylab("Life expectancy") +
  ggtitle("Life expectancy over time") + theme_bw()

ggplot(data = gapminder, aes(x = continent, y = year, color = continent)) +
  geom_point()

ggplot(data = gapminder, aes(x = continent, y = year, color = continent)) +
  geom_point(position = position_jitter(width = 0.5, height = 2))

ggplot(data = gapminder, aes(x = year, y = lifeExp, group = country)) +
  geom_line(alpha = 0.5, aes(color = "Country", size = "Country")) +
  geom_line(stat = "smooth", method = "loess",
            aes(group = continent, color = "Continent", size = "Continent"), 
            alpha = 0.5) +
  facet_wrap(~ continent, nrow = 2) +
  scale_color_manual(name = "Life Exp. for:",
                     values = c("Country" = "black", "Continent" = "dodgerblue1")) +
  scale_size_manual(name = "Life Exp. for:", 
                    values = c("Country" = 0.25, "Continent" = 3)) +
  theme_minimal(base_size = 14) + 
  ylab("Years") + xlab("") + 
  ggtitle("Life Expectancy, 1952-2007", subtitle = "By continent and country") +
  theme(legend.position=c(0.75, 0.2), axis.text.x = element_text(angle = 45))

gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap)) %>% 
  ggplot(aes(x = continent, y = medianGdpPercap)) +
  geom_col()

gapminder %>%
  filter(year == 1952) %>% 
  ggplot(aes(x = pop)) +
  geom_histogram() +
  scale_x_log10()

gapminder %>%
  filter(year == 1952) %>% 
  ggplot(aes(x = continent, y = gdpPercap, color = continent)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparing GDP per capita across continents")

## tidy 
str(iris)
iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\.")

iris$Flower <- 1:nrow(iris)

iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)


ggplot(iris.wide, aes(x = Length, y = Width, color = Part)) +
  geom_jitter() +
  facet_grid(. ~ Species)


## cluster analysis
ggpairs(iris[,-6],mapping=aes(color=Species))

ggpairs(iris, columns = 1:4, 
        aes(color=Species, alpha=0.4), 
        title="Scatterplot Matrix",
        upper=list(continuous="density", combo="box"),
        lower=list(continuous="smooth", combo="dot")) +
  theme_light() +
  theme(plot.title=element_text(size=10))

ggplot(iris, aes(x=Petal.Length, y=Sepal.Width, colour=Species) ) +
  geom_point(size= 2.5) +
  geom_smooth(method="lm") +             
  labs(title="Aggregated Data")

set.seed(123)
cluster=kmeans(iris[,1:4],3)
iris$cluster=as.factor(cluster$cluster)
ggpairs(iris,columns = 1:5, mapping=aes(color=cluster))

set.seed(456)
performance=c()
for (i in rep(1:100,times=30)) {
  clust=kmeans(iris[,1:4],i)
  performance=c(performance,1-clust$tot.withinss/clust$totss)
}
perf_df=data.frame(metrics=performance,number_of_center=rep(1:100,times=30))
ggplot(perf_df,aes(x=number_of_center,y=metrics)) +
  geom_point(alpha=0.2) +
  geom_vline(xintercept = 3,color='red')


## point 
data(diamonds)
str(diamonds)
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.4)+
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.4)+
  geom_smooth(aes(color = clarity))

ggplot(diamonds, aes(x = clarity, y = carat, color = price)) +
  geom_point(alpha = 0.5)

ggplot(diamonds, aes(x = clarity, y = carat, color = price)) +
  geom_point(alpha = 0.5, position = "jitter")


data("mtcars")
str(mtcars)
mtcars <- mtcars %>% 
  mutate_at(vars(cyl, am), factor)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})

legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)

## aesthetics 
### x, y, color, fill, size, alpha, labels, linetype, shape
### variable: continuous, discrete
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl, col = am)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

ggplot(mtcars, aes(x = wt, y = mpg, size = cyl)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, alpha = cyl)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, shape = cyl)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) +
  geom_text()

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_text(label = rownames(mtcars), color = 'red')


ggplot(mtcars, aes(x = mpg, y = qsec, col = cyl, shape = am, 
                   size = (hp/wt))) +
  geom_point()


ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter() +
  scale_y_continuous(limits = c(-2,2))


ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter(width = 0.1, alpha = 0.6, shape = 1)

ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_point(position = position_jitter(0.1))

### histogram 
ggplot(mtcars, aes(mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#377EB8")

ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1)

ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "dodge")

ggplot(mtcars, aes(mpg, color = cyl)) +
  geom_freqpoly(binwidth = 1)

ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)

### bar
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "stack")

ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()

ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = position_dodge(0.2), alpha = 0.6)

### qplot
qplot(wt, mpg, data = mtcars)

qplot(wt, mpg, data = mtcars, size = cyl)

qplot(wt, mpg, data = mtcars, color = hp)

qplot(cyl, factor(vs), data = mtcars)
      
qplot(cyl, factor(vs), data = mtcars, geom = "jitter")

ggplot(mtcars, aes(cyl, wt, col = am)) +
  geom_point(position = position_jitter(0.2, 0))

ggplot(mtcars, aes(cyl, wt, fill = am)) +
  geom_dotplot(stackdir = "center", binaxis = "y")

qplot(
  cyl, wt,
  data = mtcars,
  fill = am,
  geom = "dotplot",
  binaxis = "y",
  stackdir = "center"
)



head(ChickWeight)
ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_line(aes(group = Chick))

ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
  geom_line(aes(group = Chick))

ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
  geom_line(aes(group = Chick), alpha = 0.3) +
  geom_smooth(lwd = 2, se = FALSE)

