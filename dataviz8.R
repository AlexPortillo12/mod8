library(gapminder)
library(ggplot2)
data2007 <- gapminder[gapminder$year == 2007, ]


gdp_box <- ggplot(data2007, aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot() +
  labs(title = "GDP Per Capita by Continent in 2007", y = "GDP per Capita", x = "Continent") +
  theme_minimal()


life_box <- ggplot(data2007, aes(x = continent, y = lifeExp, fill = continent)) +
  geom_boxplot() +
  labs(title = "Life Expectancy by Continent in 2007", y = "Life Expectancy", x = "Continent") +
  theme_minimal()

ggsave("gdp_box.png", gdp_box)
ggsave("life_box.png", life_box)


mean_data <- gapminder %>%
  group_by(year, continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap),
            mean_lifeExp = mean(lifeExp), .groups = 'drop')


gdp_time_series <- ggplot(mean_data, aes(x = year, y = mean_gdpPercap, color = continent)) +
  geom_line() +
  labs(title = "Mean GDP Per Capita Over Time by Continent", y = "Mean GDP per Capita", x = "Year") +
  theme_minimal()


life_time_series <- ggplot(mean_data, aes(x = year, y = mean_lifeExp, color = continent)) +
  geom_line() +
  labs(title = "Mean Life Expectancy Over Time by Continent", y = "Mean Life Expectancy", x = "Year") +
  theme_minimal()


ggsave("gdp_time_series.png", gdp_time_series)
ggsave("life_time_series.png", life_time_series)


scatter_plot <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(aes(color = continent), alpha = 0.5) +
  scale_size(range = c(1, 20), guide = 'none') +
  facet_wrap(~ year) +
  scale_x_log10() +
  labs(title = "Relationship Between Life Expectancy and GDP Per Capita Over Time",
       x = "GDP per Capita (log scale)", y = "Life Expectancy") +
  theme_minimal()


ggsave("scatter_plot.png", scatter_plot)

