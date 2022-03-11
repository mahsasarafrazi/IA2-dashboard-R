library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
 
 #reading data from data

data = read.csv(file = "data/processed/clean_df.csv")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot_line'),
      dccDropdown(
        id='rating-select',
        options = data$rating %>%  purrr::map(function(rating,pop) list(label = rating, value = rating)) , 
        value=list("TV-G","TV-MA", "TV-14","TV-Y7"),
        multi=TRUE),
      dccRangeSlider(
        id='my-range-slider',
        min=1942,
        max=2020,
        marks = list(
          '1942' = '1942',
          '1960' = '1960',
          '1980' = '1980',
          '2000' = '2000',
          '2020' = '2020'
          
        ),
        value=list(2003, 2020)
      )
    )
  )
)

app$callback(
  output('plot_line', 'figure'),
  list(input('rating-select', 'value'),
       input('my-range-slider','value')),
  function(ratings_range,year_range) {
    df <- na.omit(data) %>% 
      filter(release_year > year_range[1],release_year < year_range[2]) %>%
      filter(rating %in% ratings_range) %>%
      group_by(release_year,rating) %>% 
      summarise(count = length(rating))
    
    plot  <- ggplot(df ,aes(x = release_year, y = count, color = rating)) +
      geom_line()+      
      scale_size(range = c(2, 12)) +
      ggtitle('Movie rating in Netflix in different years') +
      labs(x = 'Years', y= "Number of movie") +
      theme_bw() +
      theme(text =  element_text(size = 10)) +
      ggthemes::scale_color_tableau() 
    
    ggplotly(plot)
  }
)

app$run_server(host = '0.0.0.0')


