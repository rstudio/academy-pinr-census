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

summarize_divisions <- function(data, var = "total_pop") {
  data |>
    select(division, where(is.numeric), -year) |>
    pivot_longer(-division) |>
    filter(name == var) |>
    group_by(division, name) |>
    summarize(value = sum(value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(value)) |>
    pivot_wider()
}
