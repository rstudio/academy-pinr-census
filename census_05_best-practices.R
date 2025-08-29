library(tidyverse)

read_year <- function(file) {
  file |>
    read_csv(show_col_types = FALSE) |>
    # convert to percentages of occupied housing
    mutate(
      owner_occupied = owner_occupied / occupied_housing,
      renter_occupied = renter_occupied / occupied_housing,
      gas_heat = gas_heat / occupied_housing,
      electric_heat = electric_heat / occupied_housing,
      no_heat = no_heat / occupied_housing
    ) |>
    # convert to percentages of housing units
    mutate(
      occupied_housing = occupied_housing / housing_units,
      mortgaged_housing = mortgaged_housing / housing_units
    ) |>
    # reorganize columns
    relocate(
      med_home_value,
      med_gross_rent,
      med_year_built,
      household_size,
      occupied_housing,
      renter_occupied,
      owner_occupied,
      mortgaged_housing,
      .after = housing_units
    )
}

read_years <- function(years = 2005:2019, dir = "data") {
  filenames <-
    list.files(
      path = dir,
      pattern = ".csv",
      full.names = TRUE
    ) |>
    str_subset(paste0(years, collapse = "|"))

  filenames |>
    map(read_year) |>
    list_rbind()
}

summarize_divisions <- function(df, var = "total_pop") {
  df |>
    select(division, where(is.numeric), -year) |>
    pivot_longer(-division) |>
    filter(name == var) |>
    group_by(division, name) |>
    summarize(
      value = sum(value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(value)) |>
    pivot_wider()
}

plot_var_by <- function(data = acs, var = total_pop, by = division) {

  col_var <- data |>
    pull({{ var }})

  if (is.numeric(col_var)) {
    data |>
      mutate({{ by }} := fct_reorder({{ by }}, {{ var }}, .na_rm = TRUE)) |>
      ggplot(aes(x = {{ var }}, y = {{ by }})) +
      geom_boxplot()
  } else {
    data |>
      ggplot(aes(y = {{ var }}, fill = {{ var }})) +
      geom_bar(
        position = position_dodge(),
        show.legend = FALSE
      ) +
      facet_wrap(vars({{ by }}))
  }
}

plot_over_time <- function(measure, df = acs, by = sex, facet = region) {
  measure_long <-
    measure |>
    pivot_longer(cols = -c(year:total)) |>
    mutate(
      sex = str_remove_all(name, " .*"),
      age = fct_inorder(str_remove_all(name, "^[a-z]+ ")),
      proportion = value / total
    )

  df |>
    select(year, where(is.character)) |>
    left_join(measure_long, by = c("year", "state")) |>
    group_by(year, state, {{ facet }}, {{ by }}) |>
    summarize(proportion = sum(proportion)) |>
    ungroup() |>
    ggplot(aes(year,proportion, color = {{ by }})) +
    geom_smooth() +
    facet_wrap(vars({{ facet }}))
}
