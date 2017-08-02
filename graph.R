library(plotly)

temp = seq(as.Date("2000/1/1"), as.Date("2105/1/1"), by = "day")

temp = temp[1:13340]
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

d <- txhousing %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area")) %>%
  accumulate_by(~date)

d$date2 = paste0(format(as.Date(temp, format='%Y-%m-%d'), '%Y'),'.', format(as.Date(temp, format='%Y-%m-%d'), 'D')) 

p <- d %>%
  plot_ly(
    x = ~date2, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Date",
      zeroline = F
    ),
    yaxis = list(
      title = "Median",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = T
    ) %>%
  animation_button(
    x = 100, xanchor = "right", y = 0, yanchor = "bottom"
  )

p

