# Prepping Penguins
penguin_prep <- function(data, species){
    data |>
      # remove NAs
      filter(!is.na(sex)) |>
      # order factor
      mutate(Year = factor(year, levels = c("2007", "2008", "2009"))) 
  }


# fit linear regression
fit_model <- function(data, response, predictor){
  mod <- lm(as.formula(paste(response, "~", predictor)), data)
  mod
}

# make a figure
make_figure <- function(data){
  ggplot(data, aes(x = species, y = flipper_len)) +
    geom_boxplot(fill = c("grey80", "darkgreen", "blue")) +
    labs(x = "", y = expression(Flipper~length~cm)) +
    theme_bw()
}
