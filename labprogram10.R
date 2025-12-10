library(plotly)
library(gapminder)
library(dplyr)

data("gapminder")

scatter_plot <- gapminder%>%
  plot_ly(x=~gdpPercap,y=~lifeExp,color=~continent,size=~pop,hoverinfo='text',text=~paste("Country",country,"<br>",gdpPercap),
          type="scatter",mode="marker")%>%
  layout(title="GDP vs Life Expectancy")
scatter_plot

bar_chart <- gapminder%>%
  filter(year==2007)%>%
  plot_ly(x=~country,y=~lifeExp,type='bar',hoverinfo='text',text=~paste(country,"\t",lifeExp))%>%
  layout(title="Life Expectancy in 2007")

bar_chart

line_chart <- gapminder%>%
  filter(continent=="Asia")%>%
  plot_ly(x=~year,y=~lifeExp,color=~country,type="scatter",mode="lines")%>%
  layout(title="Life Expectancy in Asia")

line_chart

dashboard <- subplot(scatter_plot,bar_chart,line_chart,nrows=1)%>%
  layout(title="Gapminder Data Visualization")

dashboard