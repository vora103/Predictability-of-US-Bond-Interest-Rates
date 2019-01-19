library(readxl)
url <- "file:///C:/Users/shivu/Downloads/file_show (2).xlsx"
destfile <- "file_show_1_.xlsx"
curl::curl_download(url, destfile)
InterestRates <- read_excel(destfile)

#install.packages('plotly')
library(plotly)
library(ggplot2)
#ggplot(data = InterestRates, x = Year, y1=BAA, y2 = Real_Interest_Rates, y3 = MPi_YoY) + geom_point(aes(size=1, shape=16, color = "red") + 
#geom_line())

Recession <- ggplot(InterestRates, aes(x=Quarter)) + geom_point(aes(y=BAA, size=1, shape=20, color = "red")) +
geom_point(aes(y = Effective_Fed_Funds, size=1, shape=20, color = "blue")) + scale_shape_identity() + 
geom_point(aes(y = MPI_YoY, size=1, shape=20, color = "green")) + geom_line(aes(y=BAA, color = "red")) +
geom_line(aes(y=Effective_Fed_Funds, color = "blue")) + geom_line(aes(y=MPI_YoY, color = "green")) + 
geom_vline(xintercept=1990.5, size=9, alpha =.5, color = "purple") + geom_vline(xintercept=2001, size =5, alpha =.5, color = "violet") + 
geom_vline(xintercept=2008, size=12, alpha =.5, color = "yellow")  + scale_colour_discrete(name  ="variables", 
breaks=c("blue","green","red"), labels=c("BAA", "MPI_YoY", "Effective_Fed_Funds")) + ggtitle("Bond Rates and Recession")

Recession <- Recession + theme(panel.background = element_rect(fill = 'pink'))
Recession

ggplotly(Recession, tooltip = c("BAA","MPI_YoY", "Real_Interest_Rates"))




Recession2 <- ggplot(InterestRates, aes(x=Quarter)) + scale_shape_identity() + geom_line(aes(y=BAA, color = "red")) +
geom_line(aes(y=Effective_Fed_Funds, color = "blue")) + geom_line(aes(y=MPI_YoY, color = "green")) + 
geom_vline(xintercept=1990.5, size=9, alpha =.5, color = "purple") + geom_vline(xintercept=2001, size =5, alpha =.5, color = "violet") + 
geom_vline(xintercept=2008, size=12, alpha =.5, color = "yellow")  + scale_colour_discrete(name  ="variables", 
breaks=c("blue","green","red"), labels=c("BAA", "MPI_YoY", "Effective_Fed_Funds")) + ggtitle("Bond Rates and Recession")

Recession2 <- Recession2 + theme(panel.background = element_rect(fill = 'pink'))
Recession2

ggplotly(Recession2, tooltip = c("BAA","MPI_YoY", "Real_Interest_Rates"))


#install.packages('quantmod')
library(quantmod)

InterestRates$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


InterestRates <- InterestRates%>%
  accumulate_by(~ID)



a <- ggplotly(a) %>%
  layout(
    title = "AAA Bond Yields Compared to Inflation Rates",
    yaxis = list(
      title = "AAA Bond Yield",
      zeroline = F,
      tickprefix = "$"
    ),
    xaxis = list(
      title = "Year",
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Year"
    )
  )
a

install.packages('gapminder')
library(gapminder)


gganim2 <- a %>% 
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
gganim2


library('ISLR')
#install.packages('factoextra')
library('factoextra')
library('cluster')
#install.packages('tidyverse')
library('tidyverse')
library('stats')


kmeans10 <- kmeans(InterestRates[, 3:25], centers = 3, nstart = 25)
str(kmeans10)
kmeans10 
plot(kmeans10)
fviz_cluster(kmeans10, data = InterestRates, show_labels = TRUE, repel = TRUE, geom = "text", main = "Corporate Bond Rate Clustering")



a <- ggplot(InterestRates, aes(x = Year, y = AAA)) + geom_point(aes(color = Pres_Party)) + theme_classic() +
  labs(x="Year", y="AAA Bond Yield", title="AAA Bond Yields Compared to Inflation Rates")

ggplotly(a)

b <- ggplot(InterestRates, aes(x = Year , y = AAA, color = Pres_Party)) + geom_point() + theme_classic() +
  labs(x="Year", y="AAA Bond Yield", title="AAA Bond Yields - Colored by President's Party")

ggplotly(b)

c <- ggplot(InterestRates, aes(x =  Defense_Spending, y = AAA, color = Pres_Party)) + geom_point() + theme_classic() +
  labs(x="Defense Spending", y="AAA Bond Yield", title="AAA Bond Yields as a Function of Defense Spending")

ggplotly(c)
                                        