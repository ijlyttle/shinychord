This is a demonstration of how to use a [shinychord](http://ijlyttle.github.io/shinychord/) to put a [dygraph](https://rstudio.github.io/dygraphs/) into a shiny app.

Dygraphs are used to plot time-series, although we are using a dataframe to supply the data.

For this app, we use a static dataframe, `wx_ames`, that describes the weather in Ames, Iowa, for the month of January 2014. This data was obtained from the [Weather Underground API](http://www.wunderground.com/weather/api), using Alex Shum's [rwunderground package](https://cran.r-project.org/web/packages/rwunderground/index.html).

The code for the function `ch_dygraph`, used to generate the shinychord, can be found at  [Github](https://github.com/ijlyttle/shinychord/blob/master/R/ch_dygraph.R), as can its package [shinychord](http://github.com/ijlyttle/shinychord/). 


