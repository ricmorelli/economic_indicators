# https://www.abs.gov.au/statistics/economy/finance/monthly-household-spending-indicator/jan-2024

library(tidyverse)
library(readxl)
library(snakecase)
library(showtext)
library(RColorBrewer)

font_add_google("Libre Franklin", "franklin")
showtext_auto()


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


hh_spending <- hh_spending |> 
  filter(!row_number() %in% 1:9) |>
  mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    across(where(is.character), as.numeric)
  ) |> 
  rename_with(extract_colname, .cols = -1) |>
  rename("All Household Spending Categories" = "Total (Household Spending Categories)") |> 
  mutate(across(where(is.numeric), ~ (.x / lag(.x, 12))-1)) |> 
  drop_na() |> 
  pivot_longer(
    cols = c(2:10), 
    names_to = "category",
    values_to = "py_change"
  )

names(hh_spending) <- to_snake_case(names(hh_spending))


hh_spending |> 
  ggplot() + 
  geom_col(aes(x = date, y = py_change, group = category, fill = category),
           color = "grey30", size = 0, alpha = 0.4, show.legend = FALSE) +
  scale_x_date(breaks = seq(as.Date("2019-01-01"),as.Date("2024-01-01"), by = "1 year"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1,2.15, by = 0.5)) +
  facet_wrap(~category, scales = "free_x") +
  theme_classic() +
  labs(
    title = "Year-on-year Changes in Victorian Monthly Household Spending",
    subtitle = "Tracking changes (vs. same period previous year) in Victorian household spending habits since 2020.",
    caption = "ABS: Monthly Household Spending Indicator - Table 01. Experimental estimates of Household Spending, Victoria"
  ) +
  theme(
    text = element_text(family = "franklin"),
    plot.title = element_text(colour = "tomato2", size = 40, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "grey25", size = 26, lineheight = 0.35),
    plot.caption = element_text(colour = "grey25", size = 16, hjust = 0, vjust = -1, lineheight = 0.35),
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "grey95"),
    strip.text = element_text(colour = "grey30", size = 20.5, face= "bold", hjust = 0),
    strip.background.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.1),
    panel.border = element_rect(colour = "grey40", fill = NA, size = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_line(colour = "grey40", size = 0.1),
    axis.ticks = element_line(colour = "grey40", size = 0.1),
    legend.text = element_text(colour = "grey25", size = 16, face = "bold", family = "franklin"),
    legend.key.height = unit(9, "pt"),
    legend.position = "inside", 
    legend.position.inside = c(0.915, 1.03),
    legend.background = element_blank()
  )

ggsave("Household Spending - Victoria (Year-on-year).png", width = 6, heigh = 4.34)


