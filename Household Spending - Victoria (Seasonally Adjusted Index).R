# https://www.abs.gov.au/statistics/economy/finance/monthly-household-spending-indicator/jan-2024

library(tidyverse)
library(readxl)
library(snakecase)
library(showtext)

font_add_google("Libre Franklin", "franklin")
showtext_auto()

lockdowns <- tribble(
  ~start_date,~end_Date,
  '2020-03-26','2020-05-12',
  '2020-07-08','2020-10-27',
  '2021-02-12','2021-02-17',
  '2021-05-27','2021-06-10',
  '2021-07-15','2021-07-27',
  '2021-08-05','2021-10-21'
) |> mutate(across(everything(), ~as.Date(.)))


df1 <- read_excel("5682001_Victoria - Table 01. Experimental estimates of Household Spending, Australia.xlsx",
                  sheet = "Data1",
                  range = "A1:I70",
                  col_names = TRUE)

df2 <- read_excel("5682001_Victoria - Table 01. Experimental estimates of Household Spending, Australia.xlsx",
                  sheet = "Data1",
                  range = "O1:O70",
                  col_names = TRUE)

hh_spending <- bind_cols(df1, df2)


extract_colname <- function(col_name) {
  match <- str_match(col_name, ";\\s*([^;]+)\\s*;")[, 2] # Match ; + whitespace + text + whitespace + ;
  return(str_trim(match)) # Trim whitespace
}


process_time_series <- function(df, date_col, frequency = 12, start_date = c(2019, 1)) {
  df |> 
    mutate(across(where(is.numeric), ~ ts(.x, frequency = frequency, start = start_date), .names = "{.col}_ts")) |> # Convert numeric to ts
    mutate(across(ends_with("_ts"), ~ stl(.x, s.window = "periodic")$time.series[, "trend"], .names = "{.col}_adj")) # Perform STL & extract trend
}

hh_spending <- hh_spending |> 
  filter(!row_number() %in% 1:9) |>
  mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    across(where(is.character), as.numeric)
  ) |> 
  rename_with(extract_colname, .cols = -1)

hh_spending_adj <- process_time_series(hh_spending, date_col = "Date", frequency = 12, start_date = c(2029, 1)) |> 
  select(Date, ends_with("adj")) |> 
  rename_with(~ str_extract(., "^[^_]+"), .cols = -1) |>
  rename("All Household Spending Categories" = "Total (Household Spending Categories)") |> 
  pivot_longer(
    cols = c(2:10), 
    names_to = "category",
    values_to = "index_value"
  )
  
names(hh_spending_adj) <- to_snake_case(names(hh_spending_adj))


hh_spending_adj |> 
  ggplot() + 
  geom_rect(data = lockdowns,
            aes(xmin = start_date, xmax = end_Date,
                ymin = 50, ymax = 160,
                fill = "fill_colour"),
            position = "identity") +
  geom_line(aes(x = date, y = index_value, group = category, colour = category),
            size = 1, alpha = 0.4, show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    breaks = "fill_colour",
                    label = "Lockdown Period*",
                    values = "grey95") +
  scale_x_date(breaks = seq(as.Date("2019-01-01"),as.Date("2024-01-01"), by = "1 year"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = seq(75, 150, by = 25),
                     expand = c(0, 0)) +
  facet_wrap(~category, scales = "free_x") +
  theme_classic() +
  labs(
    title = "Monthly Household Spending in Victoria",
    subtitle = "Observing the impact of the coronavirus pandemic on Victoria's spending habits, using\nthe seasonally adjusted household spending index.",
    caption = "Source: ABS Monthly Household Spending Indicator - Table 01. Experimental estimates of Household Spending, Victoria\n*Lockdown periods applicable to Melbourne only, does not include regional Victoria."
  ) +
  theme(
    text = element_text(family = "franklin"),
    plot.title = element_text(colour = "tomato2", size = 40, face = "bold"),
    plot.subtitle = element_text(colour = "grey25", size = 28, lineheight = 0.35),
    plot.caption = element_text(colour = "grey25", size = 16, hjust = 0, vjust = -1, lineheight = 0.35),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "grey98"),
    strip.text = element_text(colour = "grey30", size = 20.5, face= "bold", hjust = 0),
    strip.background.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.1),
    panel.border = element_rect(colour = "grey40", fill = NA, size = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_line(colour = "grey40", size = 0.1),
    axis.ticks = element_line(colour = "grey40", size = 0.1),
    legend.text = element_text(colour = "grey25", size = 20.5, face = "bold", family = "franklin"),
    legend.key = element_rect(colour = "grey25", linewidth = 0.5),
    legend.key.height = unit(9, "pt"),
    legend.position = "inside", 
    legend.position.inside = c(0.9, 1.1),
    legend.background = element_blank()
  )

ggsave("Household Spending - Victoria (Seasonally Adjusted Index).png", width = 6, heigh = 4.34)
