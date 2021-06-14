######## DESCRIPTIVE STATISTICS

library(gapminder)
data <- gapminder 

mean(gapminder$lifeExp) # Mean not adequate if you've NA value
mean(gapminder$lifeExp, na.rm=TRUE)

median(gapminder$lifeExp)
var(gapminder$lifeExp) # Variance
sd(gapminder$lifeExp) # Standard deviation
max(gapminder$lifeExp) # Max value
min(gapminder$lifeExp) # Min value
range(gapminder$lifeExp) # Range
quantile(gapminder$lifeExp) # Quantiles 25%
quantile(gapminder$lifeExp, c(.3,.6,.9)) # Customized your quantiles

t.test(lifeExp, conf.level = 0.9) # Intervalle de confiance
wilcox.test(lifeExp,conf.int=TRUE)$conf.int

## Boxplot

#1
boxplot(gapminder$lifeExp)

#2
boxplot(gapminder$lifeExp,
        main = "Life Expectancy",
        xlab = "Number of years",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)




