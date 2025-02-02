# https://www.rba.gov.au/statistics/tables/
# https://www.abs.gov.au/statistics/economy/finance/lending-indicators/sep-2024

library(tidyverse)
library(readxl)
library(snakecase)
library(showtext)
library(scales)

font_add_google("Libre Franklin", "franklin")
showtext_auto()

df1 <- read_excel("F1.1 INTEREST RATES AND YIELDS – MONEY MARKET.xlsx",
                  sheet = "Data",
                  range = "A1:B277",
                  col_names = TRUE)

df2 <- read_excel("560101_National - Table 1. Households; Housing finance; Total housing; By property purpose; New loan commitments; Values.xlsx",
                  sheet = "Data1",
                  range = "H1:J277",
                  col_names = TRUE)


new_lending <- bind_cols(df1, df2)


extract_colname <- function(col_name) {
  match <- str_match(col_name, "(?:[^;]+\\s*;){2}\\s*([^;]+)\\s*;")[, 2] # Match (ignore first 2 ; separated fields incl. whitespace) + whitespace + text + whitespace + ;  
  return(str_trim(match)) # Trim whitespace
}


new_lending <- new_lending |> 
  filter(!row_number() %in% 1:9) |>
  mutate(
    date = floor_date(as.Date(as.numeric(date), origin = "1899-12-30"), "month"),
    across(where(is.character), as.numeric)
  ) |>
  rename_with(extract_colname, .cols = -c(1,2)) |> 
  rename(
    total_housing = "Total housing excluding refinancing",
    cash_rate = "Cash Rate Target"
    )

names(new_lending) <- to_snake_case(names(new_lending))


new_lending <- new_lending |>
  mutate(
    cash_rate_target_scaled = rescale(cash_rate, to = c(0,1)),
    total_housing_scaled = rescale(total_housing, to = c(0,1))
  )

range_cash_rate <- range(new_lending$cash_rate, na.rm = TRUE)
range_total_housing <- range(new_lending$total_housing, na.rm = TRUE)


reverse_rescale <- function(x, range) { 
  scales::rescale(x, to = range, from = c(0,1)) # Reverse rescale() from (0,1) to original range
}


new_lending |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cash_rate_target_scaled, colour = "Cash Rate")) +
  geom_line(aes(y = total_housing_scaled, colour = "New Loan Commitments")) +
  scale_color_manual(values = c("Cash Rate" = "tomato2", "New Loan Commitments" = "navy")) +
  scale_x_date(breaks = seq(as.Date("2002-01-01"),as.Date("2024-01-01"), by = "3 years"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(
    name = "Cash Rate",
    labels = function(x) paste0(round(reverse_rescale(x, range_cash_rate), 1), "%"),
    breaks = seq(0, 3, by = 0.2),
    sec.axis = sec_axis(
      name = "New Loan Commitments ($m)",
      function(x) reverse_rescale(x, range_total_housing),
      labels = scales::comma,
      breaks = scales::pretty_breaks(n = 5),
      ),
    expand = c(0.1,0.05)
  ) +
  theme_classic() +
  labs(
    title = "New Home Loan Commitments in Australia",
    subtitle = "Observing the relationship between the RBA cash rate and new home loan commitments.",
    caption = "Source: ABS Lending Indicators - Table 1. Households; Housing finance; Total housing; By property purpose; New loan commitments; Values"
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

ggsave("New Loan Commitments.png", width = 6, heigh = 4.34)
