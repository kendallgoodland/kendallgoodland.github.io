
req_pack <- c("readr", "dplyr", "tidyr", "ggplot2", "scales", "stringr")
install_under <- setdiff(req_pack, rownames(installed.packages()))
if (length(install_under)) install.packages(install_under, quiet = TRUE)
invisible(lapply(req_pack, library, character.only = TRUE))

INPUT_CSV  <- "affective_polarization_data.csv"  
OUTPUT_PNG <- "affective_polarization_figure.png"

my_data <- readr::read_csv(INPUT_CSV, show_col_types = FALSE)

needed <- c("year", "party_of_rater", "pct_cold")
stopifnot(all(needed %in% names(my_data)))
if (!"pct_very_cold" %in% names(my_data)) my_data$pct_very_cold <- NA_real_

my_data <- my_data %>%
  mutate(
    party_of_rater = str_to_lower(party_of_rater),
    party_of_rater = case_when(
      party_of_rater %in% c("dem", "democrat", "democratic") ~ "Dem",
      party_of_rater %in% c("rep", "republican") ~ "Rep",
      TRUE ~ party_of_rater
    ),
    year = as.integer(year)
  ) %>%
  filter(party_of_rater %in% c("Dem", "Rep")) %>%
  arrange(year, party_of_rater)

# legend
pretty_party <- function(x) dplyr::recode(
  x, "Dem"="Democrats rating Republicans", "Rep"="Republicans rating Democrats", .default = x
)

# cold layers
cold_df <- my_data %>% select(year, party_of_rater, value = pct_cold) %>% mutate(series = "Cold (<50)")
very_df <- my_data %>% select(year, party_of_rater, value = pct_very_cold) %>% mutate(series = "Very cold (<25)")

plot_df <- bind_rows(cold_df, very_df)

has_very <- any(!is.na(plot_df$value[plot_df$series == "Very cold (<25)"]))

# my plot
p <- ggplot(plot_df, aes(x = year, y = value, color = party_of_rater)) +
  # Solid lines: Cold (<50)
  geom_line(
    data = subset(plot_df, series == "Cold (<50)" & !is.na(value)),
    linewidth = 1
  ) +
  geom_point(
    data = subset(plot_df, series == "Cold (<50)" & !is.na(value)),
    size = 2
  ) +
  # Dashed lines: Very cold (<25) if available
  {
    if (has_very) list(
      geom_line(
        data = subset(plot_df, series == "Very cold (<25)" & !is.na(value)),
        linewidth = 0.9, linetype = 2
      ),
      geom_point(
        data = subset(plot_df, series == "Very cold (<25)" & !is.na(value)),
        size = 1.8, shape = 1
      )
    )
  } +
  scale_color_discrete(labels = pretty_party) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Affective Polarization Among U.S. Partisans Over Time",
    subtitle = if (has_very)
      "Solid: % rating the opposing party as Cold (<50) • Dashed: % Very Cold (<25) / Very Unfavorable"
    else
      "Lines show % of partisans rating the opposing party as Cold (<50)",
    x = "Year",
    y = "Percent of partisans",
    color = "Series by rater",
    caption = "Source: ANES time series & Pew Research Center (compiled from published summaries)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p

# cleaned export table for your appendix, if needed
export <- dat %>% arrange(year, party_of_rater)
readr::write_csv(export, "affective_polarization_series_export.csv")
message("Exported data table: affective_polarization_series_export.csv")
