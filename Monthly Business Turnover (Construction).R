# https://www.rba.gov.au/statistics/tables/
# https://www.abs.gov.au/statistics/economy/business-indicators/monthly-business-turnover-indicator/nov-2024

library(tidyverse)
library(readxl)
library(snakecase)
library(showtext)
library(scales)

font_add_google("Libre Franklin", "franklin")
showtext_auto()

df1 <- read_excel("F1.1 INTEREST RATES AND YIELDS – MONEY MARKET (2010-).xlsx",
                  sheet = "Data",
                  range = "A1:B189",
                  col_names = TRUE)

df2 <- read_excel("5681005_National - Table 05. Business turnover indicator, Construction - Monthly percentage change and index.xlsx",
                  sheet = "Data1",
                  range = "E1:E189",
                  col_names = TRUE)

construction <- bind_cols(df1, df2)


extract_colname <- function(col_name) {
  match <- str_match(col_name, ";\\s*([^;]+)\\s*;")[, 2] # Match ; + whitespace + text + whitespace + ;
  return(str_trim(match)) # Trim whitespace
}


construction <- construction |> 
  filter(!row_number() %in% 1:9) |>
  mutate(
    date = floor_date(as.Date(as.numeric(date), origin = "1899-12-30"), "month"),
    across(where(is.character), as.numeric)
  ) |>
  rename_with(extract_colname, .cols = -c(1,2)) |> 
  rename(cash_rate = "Cash Rate Target")

names(construction) <- to_snake_case(names(construction))


construction <- construction |>
  mutate(
    cash_rate_target_scaled = rescale(cash_rate, to = c(0,1)),
    construction_scaled = rescale(construction, to = c(0,1)),
  )

range_cash_rate <- range(construction$cash_rate, na.rm = TRUE)
range_construction <- range(construction$construction, na.rm = TRUE)

reverse_rescale <- function(x, range) { 
  scales::rescale(x, to = range, from = c(0,1)) # Reverse rescale() from (0,1) to original range
  }

construction |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cash_rate_target_scaled, colour = "Cash Rate")) +
  geom_line(aes(y = construction_scaled, colour = "Construction Turnover Index")) +
  scale_color_manual(values = c("Cash Rate" = "tomato2", "Construction Turnover Index" = "navy")) +
  scale_x_date(breaks = seq(as.Date("2010-01-01"),as.Date("2025-01-01"), by = "3 years"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(
    name = "Cash Rate",
    labels = function(x) paste0(round(reverse_rescale(x, range_cash_rate), 1), "%"),
    sec.axis = sec_axis(
      name = "Construction Turnover Index",
      function(x) reverse_rescale(x, range_construction))
  ) +
  theme_classic() +
  labs(
    title = "Monthly Business Turnover (Construction) in Australia",
    subtitle = "Observing the relationship between the RBA cash rate and construction turnover.",
    caption = "Source: ABS Monthly Business Turnover Indicator - Table 05: Business turnover indicator, Construction - Monthly percentage change and index"
  ) +
  theme(
    text = element_text(family = "franklin"),
    plot.title = element_text(colour = "tomato2", size = 40, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "grey25", size = 28, lineheight = 0.35),
    plot.caption = element_text(colour = "grey25", size = 18, hjust = 0, vjust = -1, lineheight = 0.35),
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "grey95"),
    panel.border = element_rect(colour = "grey40", fill = NA, size = 0.5),
    panel.grid.major.x = element_line(size = 0.5),
    panel.grid.minor.x = element_line(size = 0.5),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(colour = "grey25", size = 30, vjust = 2),
    axis.title.y.right = element_text(colour = "grey25", size = 30, vjust = 3),
    axis.text = element_text(size = 22),
    axis.line = element_line(colour = "grey40", size = 0.1),
    axis.ticks = element_line(colour = "grey40", size = 0.5),
    legend.text = element_text(colour = "grey25", size = 22),
    legend.title = element_blank(),
    legend.position = "top", 
    legend.justification = c(0, 1),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.margin = margin(-5, 0, -10, 0)
  )

ggsave("Monthly Business Turnover (Construction).png", width = 6, heigh = 4.34)
