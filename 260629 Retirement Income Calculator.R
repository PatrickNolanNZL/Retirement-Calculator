library(shiny)

# =============================================================================
# RETIREMENT INCOME CALCULATOR (R SHINY)
# =============================================================================

# =============================================================================
# SECTION 1: UTILITY FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# 1A. GENERAL HELPERS
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

parse_currency <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || is.na(x)) return(0)
  suppressWarnings(as.numeric(gsub(",", "", as.character(x)))) %||% 0
}

fmt_money <- function(x, digits = 0) {
  x <- ifelse(is.na(x), 0, x)
  paste0(
    "$",
    format(
      round(x, digits),
      big.mark = ",",
      scientific = FALSE,
      nsmall = digits,
      trim = TRUE
    )
  )
}

# -----------------------------------------------------------------------------
# 1B. DATE HELPERS
# -----------------------------------------------------------------------------

age_from_dob <- function(dob, ref_date) {
  as.numeric(as.Date(ref_date) - as.Date(dob)) / 365.25
}

birthday_this_calendar_year <- function(dob, ref_date = Sys.Date()) {
  as.Date(sprintf(
    "%s-%02d-%02d",
    format(as.Date(ref_date), "%Y"),
    as.integer(format(as.Date(dob), "%m")),
    as.integer(format(as.Date(dob), "%d"))
  ))
}

get_model_start_date <- function(dob, model_start_choice) {
  today0 <- as.Date(Sys.Date())

  if (identical(model_start_choice, "birthdayThisYear")) {
    return(max(today0, birthday_this_calendar_year(dob, today0)))
  }

  today0
}

get_retirement_date <- function(dob, withdrawal_age) {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(withdrawal_age)) return(as.Date(NA))

  as.Date(sprintf(
    "%s-%02d-%02d",
    as.integer(format(dob, "%Y")) + round(withdrawal_age),
    as.integer(format(dob, "%m")),
    as.integer(format(dob, "%d"))
  ))
}

get_years_total_date_horizon <- function(
    dob,
    withdrawal_age,
    model_start_choice = "today"
) {
  start_date <- get_model_start_date(dob, model_start_choice)
  ret_date <- get_retirement_date(dob, withdrawal_age)

  if (is.na(start_date) || is.na(ret_date)) return(NA_real_)

  as.numeric(ret_date - start_date) / 365.25
}

get_years_to_specific_age <- function(
    dob,
    target_age,
    model_start_choice = "today"
) {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(target_age)) return(NA_real_)

  start_date <- get_model_start_date(dob, model_start_choice)
  target_date <- as.Date(sprintf(
    "%s-%02d-%02d",
    as.integer(format(dob, "%Y")) + round(target_age),
    as.integer(format(dob, "%m")),
    as.integer(format(dob, "%d"))
  ))

  if (is.na(start_date) || is.na(target_date)) return(NA_real_)

  as.numeric(target_date - start_date) / 365.25
}

# =============================================================================
# SECTION 2: POLICY PARAMETERS & CONFIGURATION
# =============================================================================

CONFIG <- list(
  esct_brackets = data.frame(
    threshold = c(0, 18721, 64201, 93721, 216001),
    rate = c(10.5, 17.5, 30, 33, 39)
  ),
  govt_defaults = list(
    rate = 25,
    threshold = 180000,
    cap = 260.72
  ),
  returns_by_pir = list(
    conservative = c(`10.5` = 3.0, `17.5` = 2.7, `28` = 2.5),
    balanced     = c(`10.5` = 4.1, `17.5` = 3.8, `28` = 3.5),
    growth       = c(`10.5` = 5.2, `17.5` = 4.9, `28` = 4.5),
    aggressive   = c(`10.5` = 6.3, `17.5` = 6.0, `28` = 5.5)
  ),
  personal_tax_brackets_percent = data.frame(
    threshold = c(0, 15600, 53500, 78100, 180000),
    rate = c(10.5, 17.5, 30, 33, 39)
  ),
  personal_tax_brackets_decimal = data.frame(
    threshold = c(15600, 53500, 78100, 180000, Inf),
    rate = c(0.105, 0.175, 0.30, 0.33, 0.39)
  ),
  home_withdrawal_retain = 1000,
  drawdown_target_age = 90
)

# =============================================================================
# SECTION 3: TAX, INPUT AND VALIDATION HELPERS
# =============================================================================

# -----------------------------------------------------------------------------
# 3A. TAX HELPERS
# -----------------------------------------------------------------------------

get_pir_rate_from_income <- function(annual_income) {
  inc <- annual_income %||% 0
  if (inc <= 15600) return(10.5)
  if (inc <= 53500) return(17.5)
  28
}

get_esct_rate_for_income <- function(gross_income) {
  income <- gross_income %||% 0
  rate <- CONFIG$esct_brackets$rate[1]

  for (i in seq_len(nrow(CONFIG$esct_brackets))) {
    if (income >= CONFIG$esct_brackets$threshold[i]) {
      rate <- CONFIG$esct_brackets$rate[i]
    } else {
      break
    }
  }

  rate
}

calculate_net_income_with_percent_brackets <- function(gross) {
  gross_income <- gross %||% 0
  if (gross_income <= 0) return(0)

  remaining <- gross_income
  tax <- 0

  for (i in nrow(CONFIG$personal_tax_brackets_percent):1) {
    threshold <- CONFIG$personal_tax_brackets_percent$threshold[i]
    rate <- CONFIG$personal_tax_brackets_percent$rate[i] / 100

    if (remaining > threshold) {
      tax <- tax + (remaining - threshold) * rate
      remaining <- threshold
    }
  }

  max(0, gross_income - tax)
}

calculate_net_income_with_decimal_brackets <- function(gross) {
  gross_income <- gross %||% 0
  if (gross_income <= 0) return(0)

  tax <- 0
  previous_threshold <- 0

  for (i in seq_len(nrow(CONFIG$personal_tax_brackets_decimal))) {
    threshold <- CONFIG$personal_tax_brackets_decimal$threshold[i]
    rate <- CONFIG$personal_tax_brackets_decimal$rate[i]

    if (gross_income <= previous_threshold) break

    upper <- if (is.infinite(threshold)) gross_income else min(gross_income, threshold)
    taxable <- upper - previous_threshold

    if (taxable > 0) tax <- tax + taxable * rate

    previous_threshold <- if (is.infinite(threshold)) gross_income else threshold
  }

  max(0, gross_income - tax)
}

# -----------------------------------------------------------------------------
# 3B. INPUT READERS
# -----------------------------------------------------------------------------

read_breaks <- function(input, suffix = "") {
  br <- list()

  for (k in 1:3) {
    sA <- suppressWarnings(as.numeric(input[[paste0("break", k, "StartAge", suffix)]]))
    eA <- suppressWarnings(as.numeric(input[[paste0("break", k, "EndAge", suffix)]]))
    pause_type <- input[[paste0("break", k, "PauseType", suffix)]] %||% "pause-member"
    rr <- suppressWarnings(as.numeric(input[[paste0("break", k, "ReducedRate", suffix)]]))
    enabled <- isTRUE(input[[paste0("break", k, "TopUpEnabled", suffix)]])
    amt <- suppressWarnings(as.numeric(input[[paste0("break", k, "TopUpAmount", suffix)]]))

    if (is.finite(sA) && is.finite(eA) && eA > sA) {
      br[[length(br) + 1]] <- list(
        startAge = sA,
        endAge = eA,
        pauseType = pause_type,
        reducedRate = if (is.finite(rr)) max(0, rr) else NA_real_,
        topUpEnabled = enabled,
        topUpAmount = max(0, round(amt %||% 0))
      )
    }
  }

  if (length(br) > 1) {
    br <- br[order(vapply(br, function(x) x$startAge, numeric(1)))]
  }

  br
}

read_voluntary_contributions <- function(input, suffix = "") {
  items <- list()

  for (k in 1:2) {
    type <- input[[paste0("voluntary", k, "Type", suffix)]] %||% "none"
    if (identical(type, "none")) next

    start_age <- suppressWarnings(as.numeric(input[[paste0("voluntary", k, "StartAge", suffix)]]))
    end_age <- suppressWarnings(as.numeric(input[[paste0("voluntary", k, "EndAge", suffix)]]))
    amount <- max(
      0,
      suppressWarnings(as.numeric(input[[paste0("voluntary", k, "Amount", suffix)]])) %||% 0
    )

    if (!is.finite(start_age) || amount <= 0) next

    if (identical(type, "recurring")) {
      if (!is.finite(end_age)) end_age <- start_age + 1
      if (end_age <= start_age) next
    } else {
      end_age <- NA_real_
    }

    items[[length(items) + 1]] <- list(
      type = type,
      startAge = start_age,
      endAge = end_age,
      amount = amount,
      applied = FALSE
    )
  }

  if (length(items) > 1) {
    items <- items[order(vapply(items, function(x) x$startAge, numeric(1)))]
  }

  items
}

read_home_withdrawal <- function(input, suffix = "") {
  list(
    enabled = isTRUE(input[[paste0("homeWithdrawalEnabled", suffix)]]),
    age = suppressWarnings(as.numeric(input[[paste0("homeWithdrawalAge", suffix)]]))
  )
}

read_person_inputs <- function(input, suffix = "") {
  dob <- as.Date(input[[if (nzchar(suffix)) paste0("dob", suffix) else "dob"]])
  model_start_choice <- input$modelStartChoice %||% "today"
  withdrawal_age <- suppressWarnings(
    as.numeric(input[[if (nzchar(suffix)) paste0("withdrawalAge", suffix) else "withdrawalAge"]])
  )
  start_date <- get_model_start_date(dob, model_start_choice)

  list(
    dob = dob,
    model_start_choice = model_start_choice,
    start_age = age_from_dob(dob, start_date),
    withdrawal_age = withdrawal_age,
    projection_years = get_years_total_date_horizon(dob, withdrawal_age, model_start_choice),
    current_balance = parse_currency(input[[if (nzchar(suffix)) paste0("currentBalance", suffix) else "currentBalance"]]),
    annual_income = parse_currency(input[[if (nzchar(suffix)) paste0("annualIncome", suffix) else "annualIncome"]]),
    member_rate = suppressWarnings(as.numeric(input[[if (nzchar(suffix)) paste0("memberRate", suffix) else "memberRate"]])),
    employer_rate = suppressWarnings(as.numeric(input[[if (nzchar(suffix)) paste0("employerRate", suffix) else "employerRate"]])),
    strategy = input[[if (nzchar(suffix)) paste0("strategy", suffix) else "strategy"]] %||% "balanced",
    breaks = read_breaks(input, suffix),
    voluntary_contributions = read_voluntary_contributions(input, suffix),
    home_withdrawal = read_home_withdrawal(input, suffix)
  )
}

read_scenario_settings <- function(input) {
  couple_mode <- identical(input$mode, "couple")

  list(
    is_couple_mode = couple_mode,
    display_real = identical(if (couple_mode) input$displayModeShared else input$displayMode, "real"),
    fiscal_drag = identical(if (couple_mode) input$fiscalDragShared else input$fiscalDrag, "yes"),
    retirement_return = suppressWarnings(as.numeric(if (couple_mode) input$retirementReturnShared else input$retirementReturn)),
    retirement_return_decimal = (suppressWarnings(as.numeric(if (couple_mode) input$retirementReturnShared else input$retirementReturn)) %||% 0) / 100,
    wage_growth = suppressWarnings(as.numeric(if (couple_mode) input$wageGrowthShared else input$wageGrowth)),
    inflation_rate = suppressWarnings(as.numeric(if (couple_mode) input$inflationRateShared else input$inflationRate)),
    inflation_rate_decimal = (suppressWarnings(as.numeric(if (couple_mode) input$inflationRateShared else input$inflationRate)) %||% 0) / 100,
    nz_super_age = suppressWarnings(as.numeric(if (couple_mode) input$nzSuperAgeShared else input$nzSuperAge)),
    nz_super_annual_single = parse_currency(input$nzSuperAnnual),
    nz_super_annual_shared = parse_currency(input$nzSuperAnnualShared),
    nz_super_discount_rate = (suppressWarnings(as.numeric(if (couple_mode) input$nzSuperDiscountRateShared else input$nzSuperDiscountRate)) %||% 4.5) / 100,
    nz_super_npv_horizon = round(suppressWarnings(as.numeric(if (couple_mode) input$nzSuperNpvHorizonShared else input$nzSuperNpvHorizon)) %||% 25),
    nz_super_indexation = if (couple_mode) (input$nzSuperIndexationShared %||% "hybrid") else (input$nzSuperIndexation %||% "hybrid"),
    nz_super_income_test_enabled = isTRUE(if (couple_mode) input$nzSuperIncomeTestEnabledShared else input$nzSuperIncomeTestEnabled),
    nz_super_income_test_threshold = parse_currency(if (couple_mode) input$nzSuperIncomeTestThresholdShared else input$nzSuperIncomeTestThreshold),
    nz_super_income_test_abatement_rate = (suppressWarnings(as.numeric(if (couple_mode) input$nzSuperIncomeTestAbatementRateShared else input$nzSuperIncomeTestAbatementRate)) %||% 0) / 100,
    nz_super_income_test_stop_age = suppressWarnings(as.numeric(if (couple_mode) input$nzSuperIncomeTestStopAgeShared else input$nzSuperIncomeTestStopAge)) %||% 0,
    income_rule = if (couple_mode) (input$incomeRuleShared %||% "4percent") else (input$incomeRule %||% "4percent"),
    govt_rate = suppressWarnings(as.numeric(if (couple_mode) input$sharedGovtRate else input$govtRate)) %||% CONFIG$govt_defaults$rate,
    govt_threshold = parse_currency(if (couple_mode) input$sharedGovtThreshold else input$govtThreshold),
    govt_cap = parse_currency(if (couple_mode) input$sharedGovtCap else input$govtCap),
    nz_super_income_basis = if (couple_mode) "combined employment income (projected)" else "employment income (projected)"
  )
}

# -----------------------------------------------------------------------------
# 3C. VALIDATION
# -----------------------------------------------------------------------------

validate_person_inputs <- function(person_inputs, label = "Person") {
  req_ok <- is.finite(person_inputs$start_age) &&
    is.finite(person_inputs$withdrawal_age) &&
    is.finite(person_inputs$annual_income) &&
    nzchar(person_inputs$strategy)

  if (!req_ok) {
    return(list(
      valid = FALSE,
      reason = "incomplete",
      message = sprintf("%s details are incomplete", label)
    ))
  }

  if (
    person_inputs$current_balance < 0 ||
    person_inputs$annual_income < 0 ||
    person_inputs$member_rate < 0 ||
    person_inputs$employer_rate < 0
  ) {
    return(list(
      valid = FALSE,
      reason = "invalid",
      message = sprintf("%s contains one or more negative values", label)
    ))
  }

  if (person_inputs$member_rate > 100 || person_inputs$employer_rate > 100) {
    return(list(
      valid = FALSE,
      reason = "invalid",
      message = sprintf("%s contribution rates must be between 0 and 100", label)
    ))
  }

  if (
    person_inputs$withdrawal_age < 60 ||
    person_inputs$withdrawal_age <= person_inputs$start_age
  ) {
    return(list(
      valid = FALSE,
      reason = "invalid",
      message = sprintf("%s has an invalid age range", label)
    ))
  }

  hw <- person_inputs$home_withdrawal
  if (isTRUE(hw$enabled)) {
    if (!is.finite(hw$age)) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s first-home withdrawal needs a valid withdrawal age", label)
      ))
    }

    if (hw$age < person_inputs$start_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s first-home withdrawal age cannot be before the model start age", label)
      ))
    }

    if (hw$age > person_inputs$withdrawal_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s first-home withdrawal age cannot be after the KiwiSaver withdrawal age", label)
      ))
    }
  }

  prev_end <- NULL
  for (i in seq_along(person_inputs$breaks)) {
    b <- person_inputs$breaks[[i]]

    if (b$startAge < person_inputs$start_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s break %s cannot start before the model start age", label, i)
      ))
    }

    if (b$endAge > person_inputs$withdrawal_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s break %s cannot end after the KiwiSaver withdrawal age", label, i)
      ))
    }

    if (!is.null(prev_end) && b$startAge < prev_end) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s contribution breaks cannot overlap", label)
      ))
    }

    if (
      b$pauseType %in% c("reduce-member", "reduce-both") &&
      !(is.finite(b$reducedRate) && b$reducedRate >= 0 && b$reducedRate <= 100)
    ) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s break %s needs a reduced rate between 0 and 100", label, i)
      ))
    }

    if (isTRUE(b$topUpEnabled) && !(is.finite(b$topUpAmount) && b$topUpAmount >= 0)) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s break %s needs a non-negative government top-up amount", label, i)
      ))
    }

    prev_end <- b$endAge
  }

  for (i in seq_along(person_inputs$voluntary_contributions)) {
    item <- person_inputs$voluntary_contributions[[i]]

    if (item$startAge < person_inputs$start_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s voluntary contribution %s cannot start before the model start age", label, i)
      ))
    }

    if (item$startAge > person_inputs$withdrawal_age) {
      return(list(
        valid = FALSE,
        reason = "invalid",
        message = sprintf("%s voluntary contribution %s cannot start after the KiwiSaver withdrawal age", label, i)
      ))
    }

    if (identical(item$type, "recurring")) {
      if (!(is.finite(item$endAge) && item$endAge > item$startAge)) {
        return(list(
          valid = FALSE,
          reason = "invalid",
          message = sprintf("%s recurring voluntary contribution %s needs an end age after the start age", label, i)
        ))
      }

      if (item$endAge > person_inputs$withdrawal_age) {
        return(list(
          valid = FALSE,
          reason = "invalid",
          message = sprintf("%s recurring voluntary contribution %s cannot end after the KiwiSaver withdrawal age", label, i)
        ))
      }
    }
  }

  list(valid = TRUE, reason = NULL, message = NULL)
}

# =============================================================================
# SECTION 4: CALCULATION LOGIC
# =============================================================================

# -----------------------------------------------------------------------------
# 4A. BREAK AND VOLUNTARY CONTRIBUTION HELPERS
# -----------------------------------------------------------------------------

get_break_at_age <- function(age, breaks) {
  if (!length(breaks)) return(NULL)

  for (b in breaks) {
    if (age >= b$startAge && age < b$endAge) return(b)
  }

  NULL
}

get_voluntary_contribution_for_period <- function(
    age_at_start,
    age_at_end,
    items,
    periods_per_year
) {
  contribution <- 0
  if (!length(items)) return(list(amount = 0, items = items))

  for (i in seq_along(items)) {
    item <- items[[i]]

    if (identical(item$type, "recurring")) {
      if (age_at_start >= item$startAge && age_at_start < item$endAge) {
        contribution <- contribution + item$amount / periods_per_year
      }
    } else if (identical(item$type, "oneoff")) {
      if (!isTRUE(item$applied) && age_at_start < item$startAge && age_at_end >= item$startAge) {
        contribution <- contribution + item$amount
        items[[i]]$applied <- TRUE
      }
    }
  }

  list(amount = contribution, items = items)
}

# -----------------------------------------------------------------------------
# 4B. KIWISAVER ACCUMULATION ENGINE
# -----------------------------------------------------------------------------

calc_kiwisaver <- function(
    start_age,
    withdrawal_age,
    current_balance,
    annual_income,
    member_rate,
    employer_rate,
    strategy,
    wage_growth,
    years_total_override = NA_real_,
    fiscal_drag = FALSE,
    govt_rate = CONFIG$govt_defaults$rate,
    govt_threshold = CONFIG$govt_defaults$threshold,
    govt_cap = CONFIG$govt_defaults$cap,
    breaks = list(),
    voluntary_contributions = list(),
    home_withdrawal_enabled = FALSE,
    home_withdrawal_age = NA_real_
) {

  years_total <- if (
    is.finite(years_total_override) && years_total_override > 0
  ) {
    years_total_override
  } else {
    withdrawal_age - start_age
  }

  if (!(is.finite(years_total) && years_total > 0)) {
    return(list(
      nom = current_balance,
      mTot = 0,
      vTot = 0,
      eTot = 0,
      gTot = 0,
      rTot = 0,
      hTot = 0
    ))
  }

  pir <- get_pir_rate_from_income(annual_income)
  annual_return_rate <- CONFIG$returns_by_pir[[strategy]][as.character(pir)] / 100
  annual_return_rate <- as.numeric(annual_return_rate %||% 0)
  wage_growth_rate <- (wage_growth %||% 0) / 100

  periods_per_year <- 12
  total_periods <- max(0, round(years_total * periods_per_year))

  if (total_periods == 0) {
    return(list(
      nom = current_balance,
      mTot = 0,
      vTot = 0,
      eTot = 0,
      gTot = 0,
      rTot = 0,
      hTot = 0
    ))
  }

  monthly_multiplier <- (1 + annual_return_rate)^(1 / periods_per_year)

  balance <- current_balance
  total_member <- 0
  total_voluntary <- 0
  total_employer <- 0
  total_govt <- 0
  total_returns <- 0
  total_home <- 0

  home_done <- FALSE
  current_tax_year <- 0
  member_sum_this_year <- 0
  income_exceeded_this_year <- FALSE
  vc_state <- voluntary_contributions

  get_tax_year_number <- function(period_number) {
    if (period_number == 1) 0 else ceiling((period_number - 1) / periods_per_year)
  }

  is_last_period_of_tax_year <- function(period_number) {
    if (period_number == 1) {
      TRUE
    } else {
      get_tax_year_number(period_number) != get_tax_year_number(period_number + 1)
    }
  }

  get_salary_for_period <- function(period_number) {
    annual_income * (1 + wage_growth_rate)^(ceiling(period_number / periods_per_year) - 1)
  }

  for (period_number in seq_len(total_periods)) {
    opening_balance <- balance
    annual_salary <- get_salary_for_period(period_number)
    balance_after_interest <- opening_balance * monthly_multiplier
    interest_earned <- balance_after_interest - opening_balance

    esct_rate <- if (isTRUE(fiscal_drag)) {
      get_esct_rate_for_income(annual_salary)
    } else {
      get_esct_rate_for_income(annual_income)
    }

    age_at_start <- start_age + (period_number - 1) / periods_per_year
    age_at_end <- start_age + period_number / periods_per_year
    eligible_age <- age_at_start >= 16 && age_at_start < 65

    active_break <- get_break_at_age(age_at_start, breaks)
    member_rate_used <- member_rate / 100
    employer_rate_used <- employer_rate / 100

    if (!is.null(active_break)) {
      if (identical(active_break$pauseType, "pause-member")) member_rate_used <- 0

      if (identical(active_break$pauseType, "pause-both")) {
        member_rate_used <- 0
        employer_rate_used <- 0
      }

      if (identical(active_break$pauseType, "reduce-member") && is.finite(active_break$reducedRate)) {
        member_rate_used <- active_break$reducedRate / 100
      }

      if (identical(active_break$pauseType, "reduce-both") && is.finite(active_break$reducedRate)) {
        member_rate_used <- active_break$reducedRate / 100
        employer_rate_used <- active_break$reducedRate / 100
      }
    }

    member_contribution <- (annual_salary * member_rate_used) / periods_per_year

    vc <- if (eligible_age) {
      get_voluntary_contribution_for_period(
        age_at_start,
        age_at_end,
        vc_state,
        periods_per_year
      )
    } else {
      list(amount = 0, items = vc_state)
    }

    voluntary_contribution <- vc$amount
    vc_state <- vc$items

    employer_gross <- (annual_salary * employer_rate_used) / periods_per_year
    employer_contribution <- employer_gross * (1 - esct_rate / 100)

    tax_year_number <- get_tax_year_number(period_number)
    if (tax_year_number != current_tax_year) {
      current_tax_year <- tax_year_number
      member_sum_this_year <- 0
      income_exceeded_this_year <- FALSE
    }

    if (eligible_age) {
      member_sum_this_year <- member_sum_this_year + member_contribution + voluntary_contribution
      if (annual_salary > govt_threshold) income_exceeded_this_year <- TRUE
    }

    govt_contribution <- 0
    pay_govt_now <- is_last_period_of_tax_year(period_number) || period_number == total_periods

    if (pay_govt_now && eligible_age && !income_exceeded_this_year) {
      govt_contribution <- min(govt_cap, member_sum_this_year * (govt_rate / 100))
    }

    next_age_at_start <- start_age + period_number / periods_per_year
    next_break <- get_break_at_age(next_age_at_start, breaks)
    is_last_period_in_break <- !is.null(active_break) && (is.null(next_break) || !identical(next_break, active_break))

    top_up <- if (is_last_period_in_break && isTRUE(active_break$topUpEnabled)) {
      max(0, round(active_break$topUpAmount %||% 0))
    } else {
      0
    }

    closing_balance <- balance_after_interest +
      member_contribution +
      voluntary_contribution +
      employer_contribution +
      govt_contribution +
      top_up

    total_returns <- total_returns + interest_earned
    total_member <- total_member + member_contribution
    total_voluntary <- total_voluntary + voluntary_contribution
    total_employer <- total_employer + employer_contribution
    total_govt <- total_govt + govt_contribution + top_up

    if (pay_govt_now) {
      current_tax_year <- get_tax_year_number(period_number + 1)
      member_sum_this_year <- 0
      income_exceeded_this_year <- FALSE
    }

    post_balance <- closing_balance
    if (
      isTRUE(home_withdrawal_enabled) &&
      !home_done &&
      is.finite(home_withdrawal_age) &&
      age_at_start < home_withdrawal_age &&
      age_at_end >= home_withdrawal_age
    ) {
      withdrawal_amount <- max(0, post_balance - CONFIG$home_withdrawal_retain)

      if (withdrawal_amount > 0) {
        total_home <- total_home + withdrawal_amount
        post_balance <- post_balance - withdrawal_amount
      }

      home_done <- TRUE
    }

    balance <- post_balance
  }

  list(
    nom = balance,
    mTot = total_member,
    vTot = total_voluntary,
    eTot = total_employer,
    gTot = total_govt,
    rTot = total_returns,
    hTot = total_home
  )
}

# ----------------------------------------------------------------------------
# 4C. PERSON-LEVEL PROJECTION
# ----------------------------------------------------------------------------

run_person_projection <- function(person_inputs, scenario_settings, is_partner = FALSE) {
  projection_years <- person_inputs$projection_years
  if (!(is.finite(projection_years) && projection_years > 0)) {
    projection_years <- person_inputs$withdrawal_age - person_inputs$start_age
  }

  p <- calc_kiwisaver(
    person_inputs$start_age,
    person_inputs$withdrawal_age,
    person_inputs$current_balance,
    person_inputs$annual_income,
    person_inputs$member_rate,
    person_inputs$employer_rate,
    person_inputs$strategy,
    scenario_settings$wage_growth,
    projection_years,
    scenario_settings$fiscal_drag,
    scenario_settings$govt_rate,
    scenario_settings$govt_threshold,
    scenario_settings$govt_cap,
    person_inputs$breaks,
    person_inputs$voluntary_contributions,
    person_inputs$home_withdrawal$enabled,
    person_inputs$home_withdrawal$age
  )

  display_inflation_factor <- if (scenario_settings$display_real) {
    (1 + scenario_settings$inflation_rate_decimal)^(max(0, round(projection_years * 12)) / 12)
  } else {
    1
  }

  home_withdrawal_years <- if (
    isTRUE(person_inputs$home_withdrawal$enabled) &&
    is.finite(person_inputs$home_withdrawal$age)
  ) {
    get_years_to_specific_age(
      person_inputs$dob,
      person_inputs$home_withdrawal$age,
      person_inputs$model_start_choice %||% "today"
    )
  } else {
    NA_real_
  }

  home_withdrawal_factor <- if (
    scenario_settings$display_real &&
    is.finite(home_withdrawal_years) &&
    home_withdrawal_years > 0
  ) {
    (1 + scenario_settings$inflation_rate_decimal)^(max(0, round(home_withdrawal_years * 12)) / 12)
  } else {
    1
  }

  p$display_inflation_factor <- display_inflation_factor
  p$displayed_balance <- if (scenario_settings$display_real) p$nom / display_inflation_factor else p$nom
  p$displayed_home_withdrawal <- if (scenario_settings$display_real) p$hTot / home_withdrawal_factor else p$hTot

  p
}

# ----------------------------------------------------------------------------
# 4D. RETIREMENT DRAWDOWN SCHEDULE
# ----------------------------------------------------------------------------


simulate_ks_decumulation_schedule <- function(
    balance_at_withdrawal_nom,
    withdraw_age,
    start_age,
    rule,
    inflation_rate_decimal,
    retirement_return_decimal
) {
  target_age <- CONFIG$drawdown_target_age
  ages <- 65:target_age
  out <- setNames(vector("list", length(ages)), as.character(ages))
  
  zero_row <- function() {
    list(
      incomeNom = 0,
      incomeRealToday = 0,
      endBalNom = 0,
      endBalRealToday = 0
    )
  }
  
  withdraw_age <- round(withdraw_age)
  if (!is.finite(withdraw_age) || withdraw_age > target_age) {
    for (age in ages) out[[as.character(age)]] <- zero_row()
    return(out)
  }
  
  for (age in ages[ages < withdraw_age]) {
    out[[as.character(age)]] <- zero_row()
  }
  
  if (!(is.finite(balance_at_withdrawal_nom) &&
        balance_at_withdrawal_nom > 0 &&
        is.finite(retirement_return_decimal))) {
    for (age in ages[ages >= withdraw_age]) out[[as.character(age)]] <- zero_row()
    return(out)
  }
  
  # --------------------------------------------------------------------------
  # Fixed-date rule:
  # --------------------------------------------------------------------------
  fixed_pay_nom <- 0
  fixed_pay_real_today <- 0
  
  if (identical(rule, "fixed-date")) {
    n <- max(1, target_age - withdraw_age)
    
    # Years from model start to withdrawal age
    years_to_retirement <- max(0, withdraw_age - start_age)
    
    # Convert retirement balance to today's dollars
    balance_at_withdrawal_real_today <- balance_at_withdrawal_nom /
      ((1 + inflation_rate_decimal)^years_to_retirement)
    
    # Real return after retirement
    real_return_decimal <- ((1 + retirement_return_decimal) /
                              (1 + inflation_rate_decimal)) - 1
    
    # Solve annuity-due in real terms (payments shown from age x)
    if (abs(real_return_decimal) < 1e-12) {
      fixed_pay_real_today <- balance_at_withdrawal_real_today / n
    } else {
      fixed_pay_real_today <- balance_at_withdrawal_real_today *
        real_return_decimal /
        ((1 - (1 + real_return_decimal)^(-n)) * (1 + real_return_decimal))
    }
  }
  
  base_pct <- if (identical(rule, "4percent")) 0.04 else if (identical(rule, "6percent")) 0.06 else 0
  base_drawdown_nom <- if (base_pct > 0) balance_at_withdrawal_nom * base_pct else 0
  bal_nom <- balance_at_withdrawal_nom
  
  for (age in ages[ages >= withdraw_age]) {
    if (identical(rule, "fixed-date") && age >= target_age) {
      out[[as.character(age)]] <- zero_row()
      bal_nom <- 0
      next
    }
    
    start_bal <- bal_nom
    if (!(start_bal > 0)) {
      out[[as.character(age)]] <- zero_row()
      bal_nom <- 0
      next
    }
    
    years_since_ret <- max(0, age - withdraw_age)
    years_from_start <- max(0, age - start_age)
    
    drawdown_nom <- if (rule %in% c("4percent", "6percent")) {
      base_drawdown_nom * (1 + inflation_rate_decimal)^years_since_ret
    } else if (identical(rule, "fixed-date")) {
      # Real annuity in today's dollars, re-inflated to nominal dollars for this age
      fixed_pay_real_today * (1 + inflation_rate_decimal)^years_from_start
    } else {
      balance_at_withdrawal_nom * 0.04 * (1 + inflation_rate_decimal)^years_since_ret
    }
    
    drawdown_nom <- max(0, min(start_bal, drawdown_nom))
    end_bal <- max(0, (start_bal - drawdown_nom) * (1 + retirement_return_decimal))
    deflator <- (1 + inflation_rate_decimal)^years_from_start
    
    out[[as.character(age)]] <- list(
      incomeNom = drawdown_nom,
      incomeRealToday = drawdown_nom / deflator,
      endBalNom = end_bal,
      endBalRealToday = end_bal / deflator
    )
    
    bal_nom <- end_bal
  }
  
  out
}

# ----------------------------------------------------------------------------
# 4E. RETIREMENT OUTPUTS
# ----------------------------------------------------------------------------

compute_retirement_outputs <- function(
    primary_inputs,
    primary_projection,
    scenario_settings,
    partner_inputs = NULL,
    partner_projection = NULL
) {
  couple_mode <- scenario_settings$is_couple_mode

  nz_base_single_now <- scenario_settings$nz_super_annual_single
  nz_base_couple_now <- scenario_settings$nz_super_annual_shared
  nz_disc_rate <- scenario_settings$nz_super_discount_rate
  nz_horizon <- max(1, scenario_settings$nz_super_npv_horizon)
  indexation <- scenario_settings$nz_super_indexation
  inf <- scenario_settings$inflation_rate
  wg <- scenario_settings$wage_growth
  iR <- scenario_settings$inflation_rate_decimal
  rR <- scenario_settings$retirement_return_decimal
  nza <- scenario_settings$nz_super_age

  primary_start_age <- primary_inputs$start_age
  partner_start_age <- if (!is.null(partner_inputs)) partner_inputs$start_age else NA_real_

  get_income <- function(current_income, years_from_start) {
    if (!(is.finite(current_income) && current_income > 0)) return(0)
    current_income * (1 + wg / 100)^max(0, years_from_start)
  }

  get_assessable_income <- function(years_from_start) {
    x <- get_income(primary_inputs$annual_income, years_from_start)
    if (couple_mode && !is.null(partner_inputs)) {
      x <- x + get_income(partner_inputs$annual_income, years_from_start)
    }
    x
  }

  apply_income_test <- function(base_nzs_annual, age_for_test, years_from_start) {
    if (!(isTRUE(scenario_settings$nz_super_income_test_enabled) && scenario_settings$nz_super_income_test_stop_age > scenario_settings$nz_super_age)) {
      return(base_nzs_annual)
    }

    if (!(age_for_test >= scenario_settings$nz_super_age && age_for_test < scenario_settings$nz_super_income_test_stop_age)) {
      return(base_nzs_annual)
    }

    abatement <- max(
      0,
      get_assessable_income(years_from_start) - scenario_settings$nz_super_income_test_threshold
    ) * scenario_settings$nz_super_income_test_abatement_rate

    max(0, base_nzs_annual - abatement)
  }

  full_years <- function(x) max(0, floor((x %||% 0) + 1e-9))

  index_factor_non_hybrid <- function(years) {
    years <- max(0, years)

    if (identical(indexation, "none")) return(1)
    if (identical(indexation, "cpi")) return((1 + max(inf / 100, 0))^years)
    if (identical(indexation, "wage")) return((1 + wg / 100)^years)
    1
  }

  net_aotwe_annual_now_default <- if (nz_base_couple_now > 0) nz_base_couple_now / 0.66 else 0

  indexed_couple_nzs_annual_hybrid <- function(years) {
    years <- max(0, years)
    cpi_indexed <- nz_base_couple_now * (1 + max(inf / 100, 0))^years
    net_aotwe_at_year <- net_aotwe_annual_now_default * (1 + wg / 100)^years
    min(max(cpi_indexed, 0.66 * net_aotwe_at_year), 0.725 * net_aotwe_at_year)
  }

  nzs_payment_annual <- function(years_from_start, eligible_count) {
    if (eligible_count <= 0) return(0)

    years <- full_years(years_from_start)

    if (identical(indexation, "hybrid")) {
      couple_rate <- indexed_couple_nzs_annual_hybrid(years)
      single_living_alone <- 0.65 * couple_rate
      partner_rate <- couple_rate / 2

      if (eligible_count == 2) return(couple_rate)
      return(if (couple_mode) partner_rate else single_living_alone)
    }

    factor <- index_factor_non_hybrid(years)
    if (eligible_count == 2) return(nz_base_couple_now * factor)

    if (eligible_count == 1) {
      return(if (couple_mode) (nz_base_couple_now * factor) / 2 else nz_base_single_now * factor)
    }

    0
  }

  years_to_elig_primary <- max(0, nza - primary_start_age)
  years_to_elig_partner <- if (couple_mode && is.finite(partner_start_age)) {
    max(0, nza - partner_start_age)
  } else {
    NA_real_
  }

  first_eligibility <- if (couple_mode && is.finite(years_to_elig_partner)) {
    min(years_to_elig_primary, years_to_elig_partner)
  } else {
    years_to_elig_primary
  }

  nzs_pv_at_elig <- 0
  for (t in 0:(nz_horizon - 1)) {
    years_from_start <- first_eligibility + t

    eligible_count <- if (!couple_mode) {
      1
    } else {
      pa <- primary_start_age + years_from_start
      pb <- partner_start_age + years_from_start
      (ifelse(pa >= nza, 1, 0)) + (ifelse(is.finite(partner_start_age) && pb >= nza, 1, 0))
    }

    payment_base <- nzs_payment_annual(years_from_start, eligible_count)
    payment_age_for_test <- if (!couple_mode) {
      primary_start_age + years_from_start
    } else {
      max(primary_start_age, partner_start_age %||% primary_start_age) + years_from_start
    }

    payment <- apply_income_test(payment_base, payment_age_for_test, years_from_start)
    nzs_pv_at_elig <- nzs_pv_at_elig + payment / (1 + nz_disc_rate)^t
  }

  infl_to_elig <- (1 + iR)^full_years(first_eligibility)
  nzs_displayed <- if (scenario_settings$display_real) nzs_pv_at_elig / infl_to_elig else nzs_pv_at_elig

  ks_schedule_you <- simulate_ks_decumulation_schedule(
    primary_projection$nom,
    primary_inputs$withdrawal_age,
    primary_inputs$start_age,
    scenario_settings$income_rule,
    iR,
    rR
  )

  ks_schedule_partner <- if (couple_mode && !is.null(partner_projection)) {
    simulate_ks_decumulation_schedule(
      partner_projection$nom,
      partner_inputs$withdrawal_age,
      partner_inputs$start_age,
      scenario_settings$income_rule,
      iR,
      rR
    )
  } else {
    NULL
  }

  age_today_int <- floor(age_from_dob(primary_inputs$dob, Sys.Date()))
  years_to_ret_age <- max(0, primary_inputs$withdrawal_age - age_today_int)

  net_at_ret_age <- calculate_net_income_with_decimal_brackets(
    primary_inputs$annual_income * (1 + wg / 100)^years_to_ret_age
  )

  if (couple_mode && !is.null(partner_inputs)) {
    partner_age_today_int <- floor(age_from_dob(partner_inputs$dob, Sys.Date()))
    partner_years_to_ret <- max(0, partner_inputs$withdrawal_age - partner_age_today_int)

    net_at_ret_age <- net_at_ret_age + calculate_net_income_with_decimal_brackets(
      partner_inputs$annual_income * (1 + wg / 100)^partner_years_to_ret
    )
  }

  eligible_count_year1 <- if (!couple_mode) {
    ifelse(primary_inputs$withdrawal_age >= nza, 1, 0)
  } else {
    partner_age_now <- if (!is.null(partner_inputs)) {
      partner_inputs$start_age + years_to_ret_age
    } else {
      NA_real_
    }

    (ifelse(primary_inputs$withdrawal_age >= nza, 1, 0)) +
      (ifelse(is.finite(partner_age_now) && partner_age_now >= nza, 1, 0))
  }

  base_index_years <- max(0, round(nza) - age_today_int)
  year1_nzsuper_base <- if (eligible_count_year1 > 0) {
    if (identical(indexation, "hybrid")) {
      couple_rate <- indexed_couple_nzs_annual_hybrid(base_index_years)

      if (eligible_count_year1 == 2) {
        couple_rate
      } else if (couple_mode) {
        couple_rate / 2
      } else {
        0.65 * couple_rate
      }
    } else {
      factor <- index_factor_non_hybrid(base_index_years)

      if (eligible_count_year1 == 2) {
        nz_base_couple_now * factor
      } else if (couple_mode) {
        (nz_base_couple_now * factor) / 2
      } else {
        nz_base_single_now * factor
      }
    }
  } else {
    0
  }

  year1_nzsuper <- apply_income_test(
    year1_nzsuper_base,
    if (!couple_mode) {
      primary_inputs$withdrawal_age
    } else {
      max(primary_inputs$withdrawal_age, partner_inputs$withdrawal_age %||% primary_inputs$withdrawal_age)
    },
    years_to_ret_age
  )

  year1_ks <- (ks_schedule_you[[as.character(round(primary_inputs$withdrawal_age))]] %||% list(incomeNom = 0))$incomeNom
  if (couple_mode && !is.null(ks_schedule_partner)) {
    year1_ks <- year1_ks + (ks_schedule_partner[[as.character(round(partner_inputs$withdrawal_age))]] %||% list(incomeNom = 0))$incomeNom
  }

  replacement_rate <- if (net_at_ret_age > 0) {
    (year1_ks + year1_nzsuper) / net_at_ret_age
  } else {
    NA_real_
  }

  income_rows <- lapply(65:90, function(age) {
    years_from_start <- max(0, age - primary_inputs$start_age)

    you_row <- ks_schedule_you[[as.character(age)]] %||% list(incomeNom = 0, incomeRealToday = 0)
    ks_nom <- you_row$incomeNom
    ks_real <- you_row$incomeRealToday

    if (couple_mode && !is.null(ks_schedule_partner) && !is.null(partner_inputs)) {
      partner_age_now <- partner_inputs$start_age + (age - primary_inputs$start_age)

      partner_row <- if (is.finite(partner_age_now) && partner_age_now >= 65 && partner_age_now <= 90) {
        ks_schedule_partner[[as.character(round(partner_age_now))]] %||% list(incomeNom = 0, incomeRealToday = 0)
      } else {
        list(incomeNom = 0, incomeRealToday = 0)
      }

      ks_nom <- ks_nom + partner_row$incomeNom
      ks_real <- ks_real + partner_row$incomeRealToday
    }

    eligible_count <- if (!couple_mode) {
      ifelse(age >= nza, 1, 0)
    } else {
      partner_age_now <- partner_inputs$start_age + (age - primary_inputs$start_age)
      (ifelse(age >= nza, 1, 0)) +
        (ifelse(is.finite(partner_age_now) && partner_age_now >= nza, 1, 0))
    }

    years_rr <- if (age < round(nza)) {
      full_years(age - primary_inputs$start_age)
    } else {
      max(0, round(nza) - floor(age_from_dob(primary_inputs$dob, Sys.Date()))) + (age - round(nza))
    }

    nzs_nom_base <- if (eligible_count > 0) nzs_payment_annual(years_rr, eligible_count) else 0

    age_for_income_test <- if (!couple_mode) {
      age
    } else {
      max(age, partner_inputs$start_age + (age - primary_inputs$start_age))
    }

    nzs_nom <- apply_income_test(nzs_nom_base, age_for_income_test, years_from_start)
    nzs_real <- if (years_from_start > 0) nzs_nom / (1 + iR)^years_from_start else nzs_nom

    data.frame(
      Age = age,
      KiwiSaver = if (scenario_settings$display_real) ks_real else ks_nom,
      NZSuper = if (scenario_settings$display_real) nzs_real else nzs_nom,
      Total = if (scenario_settings$display_real) ks_real + nzs_real else ks_nom + nzs_nom,
      ReplacementRate = replacement_rate,
      stringsAsFactors = FALSE
    )
  })

  list(
    nz_super_value = nzs_displayed,
    nz_super_details = paste0(
      if (identical(indexation, "none")) {
        "Static"
      } else if (identical(indexation, "cpi")) {
        "Indexed (CPI only)"
      } else {
        "Indexed (CPI + wage band)"
      },
      if (isTRUE(scenario_settings$nz_super_income_test_enabled)) {
        paste0(" + income-tested (", scenario_settings$nz_super_income_basis, ")")
      } else {
        ""
      },
      if (scenario_settings$display_real) {
        " — lump-sum equivalent at eligibility age (today’s $, in advance)"
      } else {
        " — lump-sum equivalent at eligibility age (nominal $, in advance)"
      }
    ),
    replacement_rate = replacement_rate,
    income_table = do.call(rbind, income_rows)
  )
}

# =============================================================================
# SECTION 5: UI COMPONENT HELPERS
# =============================================================================

# -----------------------------------------------------------------------------
# 5A. SHARED DISPLAY HELPERS
# -----------------------------------------------------------------------------

static_display <- function(label, value) {
  tagList(tags$label(label), tags$div(class = "readonly-field", value))
}

# -----------------------------------------------------------------------------
# 5B. SCENARIO INPUT PANELS
# -----------------------------------------------------------------------------

break_inputs_ui <- function(suffix = "") {
  tagList(
    checkboxInput(paste0("homeWithdrawalEnabled", suffix), "Withdraw KiwiSaver for first home", FALSE),
    numericInput(paste0("homeWithdrawalAge", suffix), "Age at withdrawal", value = NA, min = 0, max = 120, step = 0.1),
    lapply(1:3, function(k) {
      tags$div(
        class = "mini-card",
        tags$h5(sprintf("Break %s", k)),
        fluidRow(
          column(6, numericInput(paste0("break", k, "StartAge", suffix), "Start age", value = NA, min = 0, max = 120, step = 0.1)),
          column(6, numericInput(paste0("break", k, "EndAge", suffix), "End age", value = NA, min = 0, max = 120, step = 0.1))
        ),
        fluidRow(
          column(4, selectInput(
            paste0("break", k, "PauseType", suffix),
            "Contribution change",
            choices = c(
              "Pause member only" = "pause-member",
              "Pause member + employer" = "pause-both",
              "Reduce member only" = "reduce-member",
              "Reduce member + employer" = "reduce-both"
            ),
            selected = "pause-member"
          )),
          column(4, numericInput(paste0("break", k, "ReducedRate", suffix), "Reduced rate (%)", value = 3, min = 0, max = 100, step = 0.1)),
          column(
            4,
            checkboxInput(paste0("break", k, "TopUpEnabled", suffix), "Apply govt top-up", FALSE),
            numericInput(paste0("break", k, "TopUpAmount", suffix), "Top-up amount ($)", value = 1000, min = 0, step = 1)
          )
        )
      )
    })
  )
}

voluntary_inputs_ui <- function(suffix = "") {
  tagList(
    tags$p(
      class = "muted-note",
      "Optional extra contributions outside standard member and employer rates. Recurring amounts are annual and spread across the year."
    ),
    lapply(1:2, function(k) {
      tags$div(
        class = "mini-card",
        tags$h5(sprintf("Voluntary contribution %s", k)),
        fluidRow(
          column(4, selectInput(
            paste0("voluntary", k, "Type", suffix),
            "Type",
            choices = c("None" = "none", "One-off" = "oneoff", "Recurring" = "recurring"),
            selected = "none"
          )),
          column(4, numericInput(paste0("voluntary", k, "StartAge", suffix), "Start age", value = NA, min = 0, max = 120, step = 0.1)),
          column(4, numericInput(paste0("voluntary", k, "Amount", suffix), "Amount ($)", value = 0, min = 0, step = 1))
        ),
        fluidRow(
          column(6, numericInput(paste0("voluntary", k, "EndAge", suffix), "End age (recurring only)", value = NA, min = 0, max = 120, step = 0.1)),
          column(6, static_display("Counts for govt subsidy", "Yes, if eligible"))
        )
      )
    })
  )
}

person_inputs_ui <- function(title, suffix = "") {
  dob_id <- if (nzchar(suffix)) paste0("dob", suffix) else "dob"
  withdrawal_age_id <- if (nzchar(suffix)) paste0("withdrawalAge", suffix) else "withdrawalAge"
  current_balance_id <- if (nzchar(suffix)) paste0("currentBalance", suffix) else "currentBalance"
  annual_income_id <- if (nzchar(suffix)) paste0("annualIncome", suffix) else "annualIncome"
  member_rate_id <- if (nzchar(suffix)) paste0("memberRate", suffix) else "memberRate"
  employer_rate_id <- if (nzchar(suffix)) paste0("employerRate", suffix) else "employerRate"
  strategy_id <- if (nzchar(suffix)) paste0("strategy", suffix) else "strategy"
  age_output <- if (nzchar(suffix)) paste0("currentAgeUI", suffix) else "currentAgeUI"

  tags$details(
    tags$summary(title),
    div(
      class = "details-body",
      fluidRow(
        column(6, dateInput(dob_id, "Date of birth", value = "1991-01-03")),
        column(
          6,
          if (!nzchar(suffix)) {
            selectInput(
              "modelStartChoice",
              "Model start date",
              choices = c("Today" = "today", "Birthday this calendar year" = "birthdayThisYear"),
              selected = "today"
            )
          } else {
            tags$div()
          }
        )
      ),
      fluidRow(
        column(6, uiOutput(age_output)),
        column(6, numericInput(withdrawal_age_id, "Withdrawal age", value = 65, min = 60, max = 100, step = 1))
      ),
      fluidRow(
        column(6, textInput(current_balance_id, "KiwiSaver balance ($)", value = "5,000")),
        column(6, textInput(annual_income_id, "Annual gross income ($)", value = "80,000"))
      ),
      fluidRow(
        column(6, numericInput(member_rate_id, "Member contribution (%)", value = 4, min = 0, max = 100, step = 0.1)),
        column(6, numericInput(employer_rate_id, "Employer contribution (%)", value = 4, min = 0, max = 100, step = 0.1))
      ),
      selectInput(
        strategy_id,
        "Investment strategy",
        choices = c(
          "Conservative" = "conservative",
          "Balanced" = "balanced",
          "Growth" = "growth",
          "Aggressive" = "aggressive"
        ),
        selected = "balanced"
      )
    )
  )
}

# -----------------------------------------------------------------------------
# 5C. SHARED SETTINGS PANELS
# -----------------------------------------------------------------------------

govt_inputs_ui <- function(shared = FALSE) {
  prefix <- if (shared) "shared" else ""

  tags$details(
    tags$summary("KiwiSaver Government Subsidy"),
    div(
      class = "details-body",
      fluidRow(
        column(6, numericInput(paste0(prefix, if (shared) "GovtRate" else "govtRate"), "Subsidy rate (%)", value = 25, min = 0, max = 100, step = 0.1)),
        column(6, textInput(paste0(prefix, if (shared) "GovtThreshold" else "govtThreshold"), "Income threshold ($)", value = "180,000"))
      ),
      fluidRow(
        column(6, textInput(paste0(prefix, if (shared) "GovtCap" else "govtCap"), "Annual cap ($)", value = "260.72"))
      )
    )
  )
}

nz_super_inputs_ui <- function(shared = FALSE) {
  suf <- if (shared) "Shared" else ""
  annual_default <- if (shared) "44,550" else "28,950"
  income_test_id <- paste0("nzSuperIncomeTestEnabled", suf)

  tags$details(
    tags$summary("NZ Super"),
    div(
      class = "details-body",
      fluidRow(
        column(6, numericInput(paste0("nzSuperAge", suf), "Eligibility age", value = 65, min = 60, max = 80, step = 1)),
        column(6, textInput(paste0("nzSuperAnnual", suf), "Annual rate ($)", value = annual_default))
      ),
      fluidRow(
        column(6, numericInput(paste0("nzSuperDiscountRate", suf), "NPV discount rate (%)", value = 4.5, step = 0.1)),
        column(6, numericInput(paste0("nzSuperNpvHorizon", suf), "NPV horizon (years)", value = 25, min = 1, max = 60, step = 1))
      ),
      selectInput(
        paste0("nzSuperIndexation", suf),
        "Indexation",
        choices = c("None" = "none", "CPI" = "cpi", "CPI + wage band" = "hybrid"),
        selected = "hybrid"
      ),
      checkboxInput(income_test_id, "Apply income testing", FALSE),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", income_test_id),
        fluidRow(
          column(6, textInput(paste0("nzSuperIncomeTestThreshold", suf), "Income threshold ($)", value = "0")),
          column(6, numericInput(paste0("nzSuperIncomeTestAbatementRate", suf), "Abatement rate (%)", value = 50, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput(paste0("nzSuperIncomeTestStopAge", suf), "Income testing stops at age", value = 70, min = 0, max = 120, step = 1)),
          column(6, static_display("Assessable income basis", if (shared) "Combined employment income (projected)" else "Employment income (projected)"))
        )
      )
    )
  )
}

decum_inputs_ui <- function(shared = FALSE) {
  id <- if (shared) "incomeRuleShared" else "incomeRule"

  tags$details(
    tags$summary("Decumulation Strategy"),
    div(
      class = "details-body",
      selectInput(
        id,
        "Withdrawal rule",
        choices = c("4% rule" = "4percent", "6% rule" = "6percent", "Real Annuity to Age 90" = "fixed-date"),
        selected = "4percent"
      )
    )
  )
}

invest_inputs_ui <- function(shared = FALSE) {
  suf <- if (shared) "Shared" else ""
  display_id <- if (shared) "displayModeShared" else "displayMode"
  fiscal_id <- if (shared) "fiscalDragShared" else "fiscalDrag"

  tags$details(
    tags$summary("Investment Assumptions"),
    div(
      class = "details-body",
      fluidRow(
        column(4, numericInput(paste0("retirementReturn", suf), "Return after 65 (%)", value = 2.0, step = 0.1)),
        column(4, numericInput(paste0("wageGrowth", suf), "Annual wage growth (%)", value = 3.5, step = 0.1)),
        column(4, numericInput(paste0("inflationRate", suf), "Inflation rate (%)", value = 2.0, step = 0.1))
      ),
      fluidRow(
        column(6, radioButtons(display_id, "Display results", choices = c("Real" = "real", "Nominal" = "nominal"), selected = "real", inline = TRUE)),
        column(6, radioButtons(fiscal_id, "Fiscal Drag (ESCT)", choices = c("Yes" = "yes", "No" = "no"), selected = "no", inline = TRUE))
      )
    )
  )
}

# =============================================================================
# SECTION 6: STYLING AND MAIN UI
# =============================================================================

# -----------------------------------------------------------------------------
# 6A. APPLICATION STYLING
# -----------------------------------------------------------------------------

app_styles <- HTML("
  *{box-sizing:border-box}
  :root{--sorted-orange:#f5660a;--sorted-grey:#e9eaf1;--sorted-red:#ff0540;--navy-1:#0a1f63;--navy-2:#112c78;--text-on-navy:#ffffff;--card-ink:#16213a;--panel-bg:#f3f3f3;--panel-inner:#f1f2f7;--line:#dddddd;--muted:#748096}
  body{background:linear-gradient(180deg,var(--navy-2) 0%,var(--navy-1) 100%);min-height:100vh;padding:20px 14px 28px;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
  .container-fluid{max-width:1840px;margin:0 auto}
  .app-header{text-align:center;margin-bottom:18px;padding-top:4px}
  .app-title-main{color:var(--text-on-navy);font-size:30px;line-height:1.08;font-weight:700;margin:0 0 26px;font-family:Georgia,'Times New Roman',serif}
  .mode-toggle-wrap{max-width:none;margin:0 auto 22px}
  .mode-toggle-wrap .form-group{margin-bottom:0}
  .mode-toggle-wrap .shiny-options-group{display:flex;gap:16px;width:100%}
  .mode-toggle-wrap .radio-inline{flex:1 1 0;margin:0!important;padding:0!important;position:relative;display:flex!important;align-items:center;justify-content:center;min-height:76px;border-radius:18px;background:#2b447f;border:3px solid rgba(255,255,255,.18);color:#ffffff;font-size:18px;font-weight:700;text-align:center;transition:all .15s ease}
  .mode-toggle-wrap .radio-inline input{position:absolute;opacity:0;pointer-events:none;width:0;height:0}
  .mode-toggle-wrap .radio-inline:has(input:checked){background:var(--sorted-orange);border-color:var(--sorted-orange);box-shadow:none}
  .mode-toggle-wrap .radio-inline:hover{filter:brightness(1.03)}
  .left-column,.right-column{padding-left:16px!important;padding-right:16px!important}
  .main-card{background:var(--panel-bg);border-radius:26px;padding:36px 42px 34px;box-shadow:none;border:none}
  .section-head{color:var(--card-ink);font-size:18px;font-weight:500;text-transform:uppercase;letter-spacing:1px;margin:0 0 28px;border-bottom:3px solid var(--line);padding-bottom:16px}
  .well{background:transparent;border:none;box-shadow:none;padding:0;margin:0}
  details{border:none;background:transparent;padding:0;margin-bottom:18px}
  details>summary{cursor:pointer;user-select:none;display:flex;align-items:center;gap:8px;background:var(--sorted-grey);padding:18px 20px;border-radius:14px;margin-bottom:0;color:var(--card-ink);font-size:17px;font-weight:600;list-style:none}
  details>summary::-webkit-details-marker{display:none}
  details>summary::marker{display:none}
  .details-body{padding:14px 4px 4px}
  .mini-card{border:1px solid #e3e4ea;border-radius:14px;padding:14px;background:#f9fafe;margin-bottom:12px}
  .mini-card h5{margin:0 0 12px;font-size:16px;color:var(--card-ink)}
  label{display:block;margin-bottom:6px;color:var(--card-ink);font-weight:600;font-size:13px}
  input[type='text'],input[type='number'],input[type='date'],select{width:100%;padding:11px 12px;border:1px solid #d8dbe5;border-radius:10px;font-size:14px;color:var(--card-ink);min-height:44px;background:#ffffff;box-shadow:none}
  .shiny-input-container{width:100%}
  .radio label,.checkbox label{font-weight:500}
  .summary-box{background:var(--panel-inner);border-radius:20px;padding:26px 30px;box-shadow:none;border:1px solid #e4e5ea;margin-bottom:20px}
  .accent-card{border-left:6px solid var(--sorted-orange)}
  .summary-label{color:#667788;font-size:13px;text-transform:uppercase;letter-spacing:.5px;font-weight:700;margin-bottom:10px}
  .summary-value{color:var(--card-ink);font-size:24px;font-weight:700;line-height:1.05;margin-top:0}
  .muted-note{color:var(--muted);font-size:14px;line-height:1.45}
  .readonly-field{min-height:44px;padding:11px 12px;background:#f8f9fc;border:1px solid #d8dbe5;border-radius:10px;color:var(--card-ink)}
  .error-box{background:#ffebee;border-left:4px solid var(--sorted-red);padding:12px 14px;border-radius:10px;color:#c62828;margin-bottom:16px}
  .info-box{background:#f8f9fc;border-left:4px solid var(--sorted-orange);padding:12px 14px;border-radius:10px;color:var(--muted);margin-top:14px;font-size:13px;line-height:1.45}
  .placeholder-box{text-align:center;padding:40px 20px;color:#7f8698}
  .balance-card h4,.results-table h4{font-size:14px;color:var(--card-ink);margin:0 0 14px;font-weight:600}
  table{width:100%;margin-top:8px;border-collapse:collapse;font-size:14px;background:transparent}
  th{background:transparent;padding:10px 8px;text-align:right;font-weight:700;color:var(--card-ink);border-bottom:2px solid #d7d9e1}
  th:first-child,td:first-child{text-align:left}
  td{padding:10px 8px;border-bottom:1px solid #d7d9e1;text-align:right;color:var(--card-ink)}
  ul.compact-list{list-style:none;padding-left:0;margin:0}
  ul.compact-list li{display:flex;justify-content:space-between;gap:20px;padding:12px 0;border-bottom:1px solid #d7d9e1;font-size:15px;color:var(--card-ink)}
  ul.compact-list li:last-child{border-bottom:none}
  .shared-group details{margin-bottom:12px}
  @media (max-width:1100px){.app-title-main{font-size:30px}.mode-toggle-wrap .radio-inline{font-size:20px;min-height:68px}.main-card{padding:28px 24px}}
")

# -----------------------------------------------------------------------------
# 6B. APPLICATION LAYOUT
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("Retirement Income Calculator (R Shiny)"),
    tags$style(app_styles)
  ),
  div(
    class = "app-header",
    tags$h1(class = "app-title-main", "Prototype Retirement Income Calculator"),
    div(
      class = "mode-toggle-wrap",
      radioButtons(
        "mode",
        NULL,
        choices = c("Individual" = "individual", "Couple" = "couple"),
        selected = "individual",
        inline = TRUE
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      class = "left-column",
      div(
        class = "main-card",
        tags$h2(class = "section-head", "Details"),
        wellPanel(
          conditionalPanel(
            "input.mode == 'individual'",
            person_inputs_ui("Key information", suffix = ""),
            tags$details(
              tags$summary("Contribution breaks"),
              div(class = "details-body", break_inputs_ui(""))
            ),
            tags$details(
              tags$summary("Voluntary contributions"),
              div(class = "details-body", voluntary_inputs_ui(""))
            ),
            govt_inputs_ui(FALSE),
            nz_super_inputs_ui(FALSE),
            decum_inputs_ui(FALSE),
            invest_inputs_ui(FALSE)
          ),
          conditionalPanel(
            "input.mode == 'couple'",
            person_inputs_ui("Your details", suffix = ""),
            tags$details(
              tags$summary("Your contribution breaks"),
              div(class = "details-body", break_inputs_ui(""))
            ),
            tags$details(
              tags$summary("Your voluntary contributions"),
              div(class = "details-body", voluntary_inputs_ui(""))
            ),
            person_inputs_ui("Partner details", suffix = "Partner"),
            tags$details(
              tags$summary("Partner contribution breaks"),
              div(class = "details-body", break_inputs_ui("Partner"))
            ),
            tags$details(
              tags$summary("Partner voluntary contributions"),
              div(class = "details-body", voluntary_inputs_ui("Partner"))
            ),
            tags$details(
              tags$summary("Shared settings"),
              div(
                class = "details-body shared-group",
                govt_inputs_ui(TRUE),
                nz_super_inputs_ui(TRUE),
                decum_inputs_ui(TRUE),
                invest_inputs_ui(TRUE)
              )
            )
          )
        )
      )
    ),
    column(
      width = 6,
      class = "right-column",
      div(
        class = "main-card",
        tags$h2(class = "section-head", "Results"),
        uiOutput("errorUI"),
        uiOutput("placeholderUI"),
        uiOutput("headlineUI"),
        conditionalPanel("input.mode == 'individual'", uiOutput("breakdownUI")),
        conditionalPanel("input.mode == 'couple'", uiOutput("coupleBalancesUI")),
        uiOutput("nzSuperUI"),
        div(
          class = "summary-box results-table",
          h4("Annual Retirement Income"),
          tableOutput("incomeTable"),
          div(
            class = "info-box",
            HTML("<strong>Estimates only</strong> — Actual returns vary by market performance. Replacement rate compares retirement income with net income at the withdrawal age (same-year dollars).<br><br><strong>Model date: 7 June 2026. Please treat all outputs as the result of a prototype model. Results may change as the model is developed.</strong> Final parity should still be validated with benchmark scenarios.")
          )
        )
      )
    )
  )
)

# =============================================================================
# SECTION 7: SERVER AND OUTPUT LOGIC
# =============================================================================

# -----------------------------------------------------------------------------
# 7A. REACTIVE SCENARIO STATE
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  output$currentAgeUI <- renderUI({
    dob_val <- input$dob
    dob_date <- suppressWarnings(as.Date(dob_val))
    static_display(
      "Current age",
      if (is.null(dob_val) || length(dob_date) == 0 || is.na(dob_date)) {
        ""
      } else {
        round(age_from_dob(dob_date, Sys.Date()), 1)
      }
    )
  })

  output$currentAgeUIPartner <- renderUI({
    dob_val <- input$dobPartner
    dob_date <- suppressWarnings(as.Date(dob_val))
    static_display(
      "Current age",
      if (is.null(dob_val) || length(dob_date) == 0 || is.na(dob_date)) {
        ""
      } else {
        round(age_from_dob(dob_date, Sys.Date()), 1)
      }
    )
  })

  scenario_result <- reactive({
    ss <- read_scenario_settings(input)
    primary_inputs <- read_person_inputs(input, "")
    partner_inputs <- if (ss$is_couple_mode) read_person_inputs(input, "Partner") else NULL

    v1 <- validate_person_inputs(primary_inputs, "Your")
    if (!isTRUE(v1$valid)) return(list(error = v1))

    if (ss$is_couple_mode) {
      v2 <- validate_person_inputs(partner_inputs, "Partner")
      if (!isTRUE(v2$valid)) return(list(error = v2))
    }

    primary_projection <- run_person_projection(primary_inputs, ss, is_partner = FALSE)
    partner_projection <- if (ss$is_couple_mode) run_person_projection(partner_inputs, ss, is_partner = TRUE) else NULL
    ro <- compute_retirement_outputs(primary_inputs, primary_projection, ss, partner_inputs, partner_projection)

    list(
      error = NULL,
      scenario_settings = ss,
      primary_inputs = primary_inputs,
      partner_inputs = partner_inputs,
      primary_projection = primary_projection,
      partner_projection = partner_projection,
      retirement_outputs = ro
    )
  })

  output$errorUI <- renderUI({
    res <- scenario_result()

    if (!is.null(res$error) && !identical(res$error$reason, "incomplete")) {
      div(class = "error-box", paste("⚠", res$error$message))
    } else {
      NULL
    }
  })

  output$placeholderUI <- renderUI({
    res <- scenario_result()

    if (!is.null(res$error) && identical(res$error$reason, "incomplete")) {
      div(class = "summary-box placeholder-box", "Fill in your details and results will appear here.")
    } else {
      NULL
    }
  })

  output$headlineUI <- renderUI({
    res <- scenario_result()
    if (!is.null(res$error)) return(NULL)

    ss <- res$scenario_settings
    pp <- res$primary_projection

    headline_value <- pp$displayed_balance
    headline_label <- "Projected KiwiSaver Balance"
    subtext <- paste("At age", round(res$primary_inputs$withdrawal_age))

    if (ss$is_couple_mode && !is.null(res$partner_projection)) {
      headline_value <- headline_value + res$partner_projection$displayed_balance
      headline_label <- "Joint KiwiSaver Balance"
      subtext <- "At each partner's withdrawal age"
    }

    div(
      class = "summary-box accent-card",
      div(class = "summary-label", headline_label),
      div(class = "summary-value", fmt_money(headline_value, 0)),
      div(class = "muted-note", subtext)
    )
  })

  output$breakdownUI <- renderUI({
    res <- scenario_result()
    if (!is.null(res$error)) return(NULL)

    ss <- res$scenario_settings
    pi <- res$primary_inputs
    pp <- res$primary_projection
    infl <- pp$display_inflation_factor %||% 1

    s <- if (ss$display_real) pi$current_balance / infl else pi$current_balance
    m <- if (ss$display_real) pp$mTot / infl else pp$mTot
    v <- if (ss$display_real) pp$vTot / infl else pp$vTot
    e <- if (ss$display_real) pp$eTot / infl else pp$eTot
    g <- if (ss$display_real) pp$gTot / infl else pp$gTot
    r <- if (ss$display_real) pp$rTot / infl else pp$rTot
    h <- if (ss$display_real) pp$displayed_home_withdrawal else pp$hTot

    div(
      class = "summary-box balance-card",
      h4("Balance Breakdown"),
      tags$ul(
        class = "compact-list",
        tags$li(tags$span("Starting"), tags$span(fmt_money(s, 2))),
        tags$li(tags$span("+ Member contributions"), tags$span(fmt_money(m, 2))),
        tags$li(tags$span("+ Voluntary contributions"), tags$span(fmt_money(v, 2))),
        tags$li(tags$span("+ Employer contributions"), tags$span(fmt_money(e, 2))),
        tags$li(tags$span("+ Government contributions"), tags$span(fmt_money(g, 2))),
        tags$li(tags$span("+ Returns"), tags$span(fmt_money(r, 2))),
        tags$li(tags$span("− Home withdrawals"), tags$span(fmt_money(h, 2)))
      )
    )
  })

  output$coupleBalancesUI <- renderUI({
    res <- scenario_result()
    if (!is.null(res$error) || is.null(res$partner_projection)) return(NULL)

    div(
      class = "summary-box balance-card",
      h4("Individual KiwiSaver balances"),
      tags$ul(
        class = "compact-list",
        tags$li(tags$span("You"), tags$span(fmt_money(res$primary_projection$displayed_balance, 2))),
        tags$li(tags$span("You — home withdrawals"), tags$span(fmt_money(res$primary_projection$displayed_home_withdrawal, 2))),
        tags$li(tags$span("Partner"), tags$span(fmt_money(res$partner_projection$displayed_balance, 2))),
        tags$li(tags$span("Partner — home withdrawals"), tags$span(fmt_money(res$partner_projection$displayed_home_withdrawal, 2)))
      )
    )
  })

  output$nzSuperUI <- renderUI({
    res <- scenario_result()
    if (!is.null(res$error)) return(NULL)

    div(
      class = "summary-box accent-card",
      div(class = "summary-label", "NZ Super (NPV at eligibility age)"),
      div(class = "summary-value", fmt_money(res$retirement_outputs$nz_super_value, 0)),
      div(class = "muted-note", res$retirement_outputs$nz_super_details)
    )
  })

  output$incomeTable <- renderTable({
    res <- scenario_result()
    if (!is.null(res$error)) return(NULL)

    df <- res$retirement_outputs$income_table
    out <- data.frame(
      Age = df$Age,
      KiwiSaver = vapply(df$KiwiSaver, fmt_money, character(1), digits = 0),
      NZSuper = vapply(df$NZSuper, fmt_money, character(1), digits = 0),
      Total = vapply(df$Total, fmt_money, character(1), digits = 0),
      stringsAsFactors = FALSE
    )

    if (!res$scenario_settings$is_couple_mode) {
      out$`Repl %` <- ifelse(
        is.finite(res$retirement_outputs$replacement_rate),
        paste0(round(res$retirement_outputs$replacement_rate * 100, 1), "%"),
        "—"
      )
    }

    out
  }, striped = FALSE, bordered = FALSE, spacing = "s", align = "r")
}

# =============================================================================
# SECTION 8: LAUNCH THE APP
# =============================================================================

shinyApp(ui, server)
