library(shiny)

# =============================================================================
# SECTION 1: UTILITY FUNCTIONS
# =============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || identical(x, "") || (length(x) == 1 && is.na(x))
}

parse_currency <- function(x, blank_as_zero = TRUE) {
  if (is_blank(x)) return(if (blank_as_zero) 0 else NA_real_)
  cleaned <- gsub("[$, ]", "", as.character(x))
  out <- suppressWarnings(as.numeric(cleaned))
  if (!is.finite(out)) return(NA_real_)
  out
}

fmt_money <- function(x, digits = 0) {
  x <- ifelse(is.na(x), 0, x)
  paste0("$", format(round(x, digits), big.mark = ",", scientific = FALSE, nsmall = digits, trim = TRUE))
}

num_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  out <- suppressWarnings(as.numeric(x[1]))
  if (!is.finite(out)) return(NA_real_)
  out
}

safe_anniversary_date <- function(dob, target_year) {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(target_year)) return(as.Date(NA))
  month <- as.integer(format(dob, "%m"))
  day <- as.integer(format(dob, "%d"))
  target <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", as.integer(target_year), month, day)))
  if (is.na(target) && month == 2 && day == 29) target <- as.Date(sprintf("%s-02-28", as.integer(target_year)))
  target
}

age_from_dob <- function(dob, ref_date) {
  dob <- as.Date(dob)
  ref_date <- as.Date(ref_date)
  if (is.na(dob) || is.na(ref_date)) return(NA_real_)
  as.numeric(ref_date - dob) / 365.25
}

age_to_date <- function(dob, age) {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(age)) return(as.Date(NA))
  if (abs(age - round(age)) < 1e-9) {
    return(safe_anniversary_date(dob, as.integer(format(dob, "%Y")) + round(age)))
  }
  dob + round(age * 365.25)
}

add_months_safe <- function(date, n) {
  date <- as.Date(date)
  if (is.na(date) || !is.finite(n)) return(as.Date(NA))
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  d <- as.integer(format(date, "%d"))
  total_m <- (y * 12 + (m - 1)) + as.integer(n)
  new_y <- total_m %/% 12
  new_m <- total_m %% 12 + 1
  first_next <- if (new_m == 12) as.Date(sprintf("%04d-01-01", new_y + 1)) else as.Date(sprintf("%04d-%02d-01", new_y, new_m + 1))
  last_day <- as.integer(format(first_next - 1, "%d"))
  as.Date(sprintf("%04d-%02d-%02d", new_y, new_m, min(d, last_day)))
}

govt_contribution_year_end <- function(date) {
  date <- as.Date(date)
  if (is.na(date)) return(NA_integer_)
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  if (m >= 7) y + 1L else y
}

next_govt_contribution_boundary <- function(date) {
  date <- as.Date(date)
  if (is.na(date)) return(as.Date(NA))
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  boundary_year <- if (m >= 7) y + 1L else y
  as.Date(sprintf("%04d-07-01", boundary_year))
}

get_model_start_date <- function(dob, model_start_choice = "today") as.Date(Sys.Date())

get_retirement_date <- function(dob, withdrawal_age) {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(withdrawal_age)) return(as.Date(NA))
  age_to_date(dob, withdrawal_age)
}

get_years_total_date_horizon <- function(dob, withdrawal_age, model_start_choice = "today") {
  start_date <- get_model_start_date(dob, model_start_choice)
  ret_date <- get_retirement_date(dob, withdrawal_age)
  if (is.na(start_date) || is.na(ret_date)) return(NA_real_)
  as.numeric(ret_date - start_date) / 365.25
}

get_years_to_specific_age <- function(dob, target_age, model_start_choice = "today") {
  dob <- as.Date(dob)
  if (is.na(dob) || !is.finite(target_age)) return(NA_real_)
  start_date <- get_model_start_date(dob, model_start_choice)
  target_date <- age_to_date(dob, target_age)
  if (is.na(start_date) || is.na(target_date)) return(NA_real_)
  as.numeric(target_date - start_date) / 365.25
}

# =============================================================================
# SECTION 2: POLICY PARAMETERS AND CONFIGURATION
# =============================================================================

CONFIG <- list(
  esct_brackets = data.frame(threshold = c(0, 18721, 64201, 93721, 216001), rate = c(10.5, 17.5, 30, 33, 39)),
  govt_defaults = list(rate = 25, threshold = 180000, cap = 260.72),
  returns_by_pir = list(
    conservative = c(`10.5` = 3.0, `17.5` = 2.7, `28` = 2.5),
    balanced     = c(`10.5` = 4.1, `17.5` = 3.8, `28` = 3.5),
    growth       = c(`10.5` = 5.2, `17.5` = 4.9, `28` = 4.5),
    aggressive   = c(`10.5` = 6.3, `17.5` = 6.0, `28` = 5.5)
  ),
  personal_tax_brackets_decimal = data.frame(threshold = c(15600, 53500, 78100, 180000, Inf), rate = c(0.105, 0.175, 0.30, 0.33, 0.39)),
  home_withdrawal_retain = 1000,
  drawdown_target_age = 90,
  acc_earner_levy = list(rate = 0.0175, max_liable_earnings = 156641),
  date_limits = list(min_dob = as.Date("1900-01-01"), max_future_dob_days = 0, max_partner_age_gap = 40),
  money_limits = list(max_balance = 10000000, max_annual_income = 5000000, max_voluntary_contribution = 1000000, max_break_top_up = 1000000, max_nz_super_annual = 200000, max_govt_threshold = 5000000, max_govt_cap = 100000),
  model_version_label = "prototype produced July 2026 - date-based event engine"
)

# =============================================================================
# SECTION 3: TAX, INPUT AND VALIDATION HELPERS
# =============================================================================

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
    if (income >= CONFIG$esct_brackets$threshold[i]) rate <- CONFIG$esct_brackets$rate[i] else break
  }
  rate
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

calculate_acc_earner_levy <- function(gross_income) {
  gross_income <- gross_income %||% 0
  if (!(is.finite(gross_income) && gross_income > 0)) return(0)
  min(gross_income, CONFIG$acc_earner_levy$max_liable_earnings) * CONFIG$acc_earner_levy$rate
}

calculate_disposable_income_with_acc <- function(gross_income) {
  gross_income <- gross_income %||% 0
  if (!(is.finite(gross_income) && gross_income > 0)) return(0)
  max(0, calculate_net_income_with_decimal_brackets(gross_income) - calculate_acc_earner_levy(gross_income))
}

input_id <- function(base, suffix = "") if (nzchar(suffix)) paste0(base, suffix) else base

read_breaks <- function(input, suffix = "") {
  br <- list()
  for (k in 1:3) {
    enabled_id <- paste0("break", k, "Enabled", suffix)
    enabled_value <- input[[enabled_id]]
    sA <- num_or_na(input[[paste0("break", k, "StartAge", suffix)]])
    eA <- num_or_na(input[[paste0("break", k, "EndAge", suffix)]])
    if (!is.null(enabled_value)) {
      if (!isTRUE(enabled_value)) next
    } else {
      if (!is.finite(sA) && !is.finite(eA)) next
    }
    br[[length(br) + 1]] <- list(
      startAge = sA,
      endAge = eA,
      pauseType = input[[paste0("break", k, "PauseType", suffix)]] %||% "pause-member",
      reducedRate = num_or_na(input[[paste0("break", k, "ReducedRate", suffix)]]),
      topUpEnabled = isTRUE(input[[paste0("break", k, "TopUpEnabled", suffix)]]),
      topUpAmount = num_or_na(input[[paste0("break", k, "TopUpAmount", suffix)]])
    )
  }
  if (length(br) > 1) br <- br[order(vapply(br, function(x) if (is.finite(x$startAge)) x$startAge else Inf, numeric(1)))]
  br
}

read_voluntary_contributions <- function(input, suffix = "") {
  items <- list()
  for (k in 1:2) {
    type <- input[[paste0("voluntary", k, "Type", suffix)]] %||% "none"
    if (identical(type, "none")) next
    items[[length(items) + 1]] <- list(
      type = type,
      startAge = num_or_na(input[[paste0("voluntary", k, "StartAge", suffix)]]),
      endAge = if (identical(type, "recurring")) num_or_na(input[[paste0("voluntary", k, "EndAge", suffix)]]) else NA_real_,
      amount = num_or_na(input[[paste0("voluntary", k, "Amount", suffix)]]),
      applied = FALSE
    )
  }
  if (length(items) > 1) items <- items[order(vapply(items, function(x) x$startAge %||% Inf, numeric(1)))]
  items
}

read_home_withdrawal <- function(input, suffix = "") {
  list(enabled = isTRUE(input[[paste0("homeWithdrawalEnabled", suffix)]]), age = num_or_na(input[[paste0("homeWithdrawalAge", suffix)]]))
}

read_person_inputs <- function(input, suffix = "") {
  dob <- as.Date(input[[input_id("dob", suffix)]])
  model_start_choice <- "today"
  withdrawal_age <- num_or_na(input[[input_id("withdrawalAge", suffix)]])
  start_date <- get_model_start_date(dob, model_start_choice)
  list(
    dob = dob,
    model_start_choice = model_start_choice,
    start_age = age_from_dob(dob, start_date),
    withdrawal_age = withdrawal_age,
    projection_years = get_years_total_date_horizon(dob, withdrawal_age, model_start_choice),
    current_balance = parse_currency(input[[input_id("currentBalance", suffix)]]),
    annual_income = parse_currency(input[[input_id("annualIncome", suffix)]]),
    member_rate = num_or_na(input[[input_id("memberRate", suffix)]]),
    employer_rate = num_or_na(input[[input_id("employerRate", suffix)]]),
    strategy = input[[input_id("strategy", suffix)]] %||% "balanced",
    breaks = read_breaks(input, suffix),
    voluntary_contributions = read_voluntary_contributions(input, suffix),
    home_withdrawal = read_home_withdrawal(input, suffix)
  )
}

read_scenario_settings <- function(input) {
  couple_mode <- identical(input$mode, "couple")
  suf <- if (couple_mode) "Shared" else ""
  list(
    is_couple_mode = couple_mode,
    display_real = identical(input[[if (couple_mode) "displayModeShared" else "displayMode"]], "real"),
    fiscal_drag = identical(input[[if (couple_mode) "fiscalDragShared" else "fiscalDrag"]], "yes"),
    retirement_return = num_or_na(input[[paste0("retirementReturn", suf)]]),
    retirement_return_decimal = (num_or_na(input[[paste0("retirementReturn", suf)]]) %||% 0) / 100,
    wage_growth = num_or_na(input[[paste0("wageGrowth", suf)]]),
    inflation_rate = num_or_na(input[[paste0("inflationRate", suf)]]),
    inflation_rate_decimal = (num_or_na(input[[paste0("inflationRate", suf)]]) %||% 0) / 100,
    nz_super_age = num_or_na(input[[paste0("nzSuperAge", suf)]]),
    nz_super_annual_single = parse_currency(input$nzSuperAnnual),
    nz_super_annual_shared = parse_currency(input$nzSuperAnnualShared),
    nz_super_discount_rate = (num_or_na(input[[paste0("nzSuperDiscountRate", suf)]]) %||% NA_real_) / 100,
    nz_super_npv_horizon = round(num_or_na(input[[paste0("nzSuperNpvHorizon", suf)]]) %||% NA_real_),
    nz_super_indexation = input[[paste0("nzSuperIndexation", suf)]] %||% "hybrid",
    nz_super_income_test_enabled = isTRUE(input[[paste0("nzSuperIncomeTestEnabled", suf)]]),
    nz_super_income_test_threshold = parse_currency(input[[paste0("nzSuperIncomeTestThreshold", suf)]]),
    nz_super_income_test_abatement_rate = (num_or_na(input[[paste0("nzSuperIncomeTestAbatementRate", suf)]]) %||% NA_real_) / 100,
    nz_super_income_test_stop_age = num_or_na(input[[paste0("nzSuperIncomeTestStopAge", suf)]]) %||% 0,
    income_rule = input[[if (couple_mode) "incomeRuleShared" else "incomeRule"]] %||% "4percent",
    govt_rate = num_or_na(input[[if (couple_mode) "sharedGovtRate" else "govtRate"]]) %||% NA_real_,
    govt_threshold = parse_currency(input[[if (couple_mode) "sharedGovtThreshold" else "govtThreshold"]]),
    govt_cap = parse_currency(input[[if (couple_mode) "sharedGovtCap" else "govtCap"]]),
    nz_super_income_basis = if (couple_mode) "combined employment income (projected)" else "employment income (projected)"
  )
}

validation_result <- function(message, reason = "invalid") list(valid = FALSE, reason = reason, message = message)
valid_result <- function() list(valid = TRUE, reason = NULL, message = NULL)

validate_dob <- function(dob, label = "Person") {
  dob <- as.Date(dob)
  max_dob <- Sys.Date() + CONFIG$date_limits$max_future_dob_days
  if (is.na(dob)) return(sprintf("%s needs a valid date of birth", label))
  if (dob < CONFIG$date_limits$min_dob) return(sprintf("%s date of birth is outside the supported range", label))
  if (dob > max_dob) return(sprintf("%s date of birth cannot be in the future", label))
  NULL
}

validate_money <- function(value, label, min_value = 0, max_value = Inf) {
  if (!is.finite(value)) return(sprintf("%s needs a valid dollar amount", label))
  if (value < min_value) return(sprintf("%s must be at least %s", label, fmt_money(min_value, 0)))
  if (value > max_value) return(sprintf("%s must be no more than %s", label, fmt_money(max_value, 0)))
  NULL
}

validate_couple_age_gap <- function(primary_inputs, partner_inputs) {
  if (is.null(partner_inputs)) return(NULL)
  age_gap <- abs(primary_inputs$start_age - partner_inputs$start_age)
  if (is.finite(age_gap) && age_gap > CONFIG$date_limits$max_partner_age_gap) return(sprintf("The partner age gap is outside the supported range of %s years", CONFIG$date_limits$max_partner_age_gap))
  NULL
}

validate_scenario_settings <- function(ss) {
  checks <- list(
    validate_money(ss$govt_threshold, "Government subsidy income threshold", 0, CONFIG$money_limits$max_govt_threshold),
    validate_money(ss$govt_cap, "Government subsidy annual cap", 0, CONFIG$money_limits$max_govt_cap),
    validate_money(ss$nz_super_annual_single, "NZ Super single annual rate", 0, CONFIG$money_limits$max_nz_super_annual),
    validate_money(ss$nz_super_annual_shared, "NZ Super couple annual rate", 0, CONFIG$money_limits$max_nz_super_annual),
    validate_money(ss$nz_super_income_test_threshold, "NZ Super income-test threshold", 0, CONFIG$money_limits$max_annual_income)
  )
  for (msg in checks) if (!is.null(msg)) return(validation_result(msg))
  if (!(is.finite(ss$govt_rate) && ss$govt_rate >= 0 && ss$govt_rate <= 100)) return(validation_result("Government subsidy rate must be between 0% and 100%"))
  if (!(is.finite(ss$nz_super_npv_horizon) && ss$nz_super_npv_horizon >= 1 && ss$nz_super_npv_horizon <= 50)) return(validation_result("NZ Super NPV horizon must be between 1 and 50 years"))
  if (!(is.finite(ss$nz_super_discount_rate) && ss$nz_super_discount_rate >= 0 && ss$nz_super_discount_rate <= 0.15)) return(validation_result("NZ Super discount rate must be between 0% and 15%"))
  if (!(is.finite(ss$nz_super_income_test_abatement_rate) && ss$nz_super_income_test_abatement_rate >= 0 && ss$nz_super_income_test_abatement_rate <= 1)) return(validation_result("NZ Super abatement rate must be between 0% and 100%"))
  if (!(is.finite(ss$inflation_rate) && ss$inflation_rate >= -2 && ss$inflation_rate <= 10)) return(validation_result("Inflation must be between -2% and 10%"))
  if (!(is.finite(ss$wage_growth) && ss$wage_growth >= -2 && ss$wage_growth <= 10)) return(validation_result("Wage growth must be between -2% and 10%"))
  if (!(is.finite(ss$retirement_return) && ss$retirement_return >= -10 && ss$retirement_return <= 15)) return(validation_result("Return after 65 must be between -10% and 15%"))
  if (!(is.finite(ss$nz_super_age) && ss$nz_super_age >= 60 && ss$nz_super_age <= 80)) return(validation_result("NZ Super eligibility age must be between 60 and 80"))
  valid_result()
}

validate_person_inputs <- function(person_inputs, label = "Person") {
  dob_error <- validate_dob(person_inputs$dob, label)
  if (!is.null(dob_error)) return(validation_result(dob_error))
  req_ok <- is.finite(person_inputs$start_age) && is.finite(person_inputs$withdrawal_age) && is.finite(person_inputs$annual_income) && is.finite(person_inputs$current_balance) && is.finite(person_inputs$member_rate) && is.finite(person_inputs$employer_rate) && nzchar(person_inputs$strategy)
  if (!req_ok) return(validation_result(sprintf("%s details are incomplete", label), "incomplete"))
  checks <- list(validate_money(person_inputs$current_balance, sprintf("%s KiwiSaver balance", label), 0, CONFIG$money_limits$max_balance), validate_money(person_inputs$annual_income, sprintf("%s annual gross income", label), 0, CONFIG$money_limits$max_annual_income))
  for (msg in checks) if (!is.null(msg)) return(validation_result(msg))
  if (person_inputs$member_rate < 0 || person_inputs$employer_rate < 0 || person_inputs$member_rate > 100 || person_inputs$employer_rate > 100) return(validation_result(sprintf("%s contribution rates must be between 0 and 100", label)))
  if (person_inputs$withdrawal_age < 65 || person_inputs$withdrawal_age > CONFIG$drawdown_target_age || person_inputs$withdrawal_age <= person_inputs$start_age) return(validation_result(sprintf("%s has an invalid age range. Withdrawal age must be between 65 and %s and after the model start age", label, CONFIG$drawdown_target_age)))
  hw <- person_inputs$home_withdrawal
  if (isTRUE(hw$enabled)) {
    if (!is.finite(hw$age)) return(validation_result(sprintf("%s first-home withdrawal needs a valid withdrawal age", label)))
    if (hw$age < person_inputs$start_age) return(validation_result(sprintf("%s first-home withdrawal age cannot be before the model start age", label)))
    if (hw$age > person_inputs$withdrawal_age) return(validation_result(sprintf("%s first-home withdrawal age cannot be after the KiwiSaver withdrawal age", label)))
  }
  prev_end <- NULL
  for (i in seq_along(person_inputs$breaks)) {
    b <- person_inputs$breaks[[i]]
    if (!(is.finite(b$startAge) && is.finite(b$endAge) && b$endAge > b$startAge)) return(validation_result(sprintf("%s break %s needs a valid start and end age", label, i)))
    if (b$startAge < person_inputs$start_age) return(validation_result(sprintf("%s break %s cannot start before the model start age", label, i)))
    if (b$endAge > person_inputs$withdrawal_age) return(validation_result(sprintf("%s break %s cannot end after the KiwiSaver withdrawal age", label, i)))
    if (!is.null(prev_end) && b$startAge < prev_end) return(validation_result(sprintf("%s contribution breaks cannot overlap", label)))
    if (b$pauseType %in% c("reduce-member", "reduce-both")) {
      if (!(is.finite(b$reducedRate) && b$reducedRate >= 0 && b$reducedRate <= 100)) return(validation_result(sprintf("%s break %s needs a reduced rate between 0 and 100", label, i)))
      if (b$reducedRate > person_inputs$member_rate) return(validation_result(sprintf("%s break %s reduced member rate cannot exceed the normal member contribution rate", label, i)))
      if (identical(b$pauseType, "reduce-both") && b$reducedRate > person_inputs$employer_rate) return(validation_result(sprintf("%s break %s reduced employer rate cannot exceed the normal employer contribution rate", label, i)))
    }
    if (isTRUE(b$topUpEnabled)) {
      msg <- validate_money(b$topUpAmount, sprintf("%s break %s government top-up", label, i), 0, CONFIG$money_limits$max_break_top_up)
      if (!is.null(msg)) return(validation_result(msg))
    }
    prev_end <- b$endAge
  }
  for (i in seq_along(person_inputs$voluntary_contributions)) {
    item <- person_inputs$voluntary_contributions[[i]]
    msg <- validate_money(item$amount, sprintf("%s voluntary contribution %s amount", label, i), 0, CONFIG$money_limits$max_voluntary_contribution)
    if (!is.null(msg)) return(validation_result(msg))
    if (item$amount <= 0) return(validation_result(sprintf("%s voluntary contribution %s amount must be positive", label, i)))
    if (!is.finite(item$startAge)) return(validation_result(sprintf("%s voluntary contribution %s needs a start age", label, i)))
    if (item$startAge < person_inputs$start_age) return(validation_result(sprintf("%s voluntary contribution %s cannot start before the model start age", label, i)))
    if (item$startAge > person_inputs$withdrawal_age) return(validation_result(sprintf("%s voluntary contribution %s cannot start after the KiwiSaver withdrawal age", label, i)))
    if (identical(item$type, "recurring")) {
      if (!(is.finite(item$endAge) && item$endAge > item$startAge)) return(validation_result(sprintf("%s recurring voluntary contribution %s needs an end age after the start age", label, i)))
      if (item$endAge > person_inputs$withdrawal_age) return(validation_result(sprintf("%s recurring voluntary contribution %s cannot end after the KiwiSaver withdrawal age", label, i)))
    }
  }
  valid_result()
}

policy_warning_messages <- function(primary_inputs, partner_inputs, ss) {
  out <- c()
  check_person <- function(p, label) {
    w <- c()
    if (is.finite(p$member_rate) && is.finite(p$employer_rate) && p$member_rate > 0 && p$employer_rate == 0) w <- c(w, sprintf("%s has member contributions but no employer contribution. This may not reflect current employee settings.", label))
    if (is.finite(p$member_rate) && is.finite(p$employer_rate) && p$member_rate > 0 && p$employer_rate > 0 && p$employer_rate < 3) w <- c(w, sprintf("%s has an employer contribution below 3%%. This may not reflect current employee settings.", label))
    w
  }
  out <- c(out, check_person(primary_inputs, "Your scenario"))
  if (isTRUE(ss$is_couple_mode) && !is.null(partner_inputs)) out <- c(out, check_person(partner_inputs, "Partner scenario"))
  out
}

# =============================================================================
# SECTION 4: DATE-BASED CALCULATION LOGIC
# =============================================================================

between_dates <- function(x, start_date, end_date, include_end = TRUE) {
  x <- as.Date(x); start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (is.na(x) || is.na(start_date) || is.na(end_date)) return(FALSE)
  if (include_end) x >= start_date && x <= end_date else x >= start_date && x < end_date
}

active_break_on_date <- function(segment_start, breaks, dob) {
  if (!length(breaks)) return(NULL)
  for (b in breaks) {
    b_start <- age_to_date(dob, b$startAge)
    b_end <- age_to_date(dob, b$endAge)
    if (!is.na(b_start) && !is.na(b_end) && segment_start >= b_start && segment_start < b_end) return(b)
  }
  NULL
}

build_projection_dates <- function(dob, model_start_date, withdrawal_date, breaks = list(), voluntary_contributions = list(), home_withdrawal_age = NA_real_) {
  dates <- c(as.Date(model_start_date), as.Date(withdrawal_date))
  # Monthly contribution checkpoints retain the familiar contribution rhythm, while event dates give exact age/date boundaries.
  cursor <- add_months_safe(model_start_date, 1)
  while (!is.na(cursor) && cursor < withdrawal_date) {
    dates <- c(dates, cursor)
    cursor <- add_months_safe(cursor, 1)
  }
  # Government contribution years end on 30 June, represented by the next day's boundary, 1 July.
  boundary <- next_govt_contribution_boundary(model_start_date)
  while (!is.na(boundary) && boundary < withdrawal_date) {
    if (boundary > model_start_date) dates <- c(dates, boundary)
    boundary <- next_govt_contribution_boundary(boundary + 1)
  }
  # Exact age eligibility boundaries.
  for (age in c(16, 65)) {
    d <- age_to_date(dob, age)
    if (between_dates(d, model_start_date, withdrawal_date)) dates <- c(dates, d)
  }
  # Contribution breaks.
  for (b in breaks) {
    for (age in c(b$startAge, b$endAge)) {
      d <- age_to_date(dob, age)
      if (between_dates(d, model_start_date, withdrawal_date)) dates <- c(dates, d)
    }
  }
  # Voluntary contributions.
  for (item in voluntary_contributions) {
    d_start <- age_to_date(dob, item$startAge)
    if (between_dates(d_start, model_start_date, withdrawal_date)) dates <- c(dates, d_start)
    if (identical(item$type, "recurring")) {
      d_end <- age_to_date(dob, item$endAge)
      if (between_dates(d_end, model_start_date, withdrawal_date)) dates <- c(dates, d_end)
    }
  }
  # First-home withdrawal.
  home_date <- age_to_date(dob, home_withdrawal_age)
  if (between_dates(home_date, model_start_date, withdrawal_date)) dates <- c(dates, home_date)
  sort(unique(as.Date(dates[!is.na(dates)])))
}

calc_kiwisaver <- function(dob, start_age, withdrawal_age, current_balance, annual_income, member_rate, employer_rate,
                           strategy, wage_growth, years_total_override = NA_real_, fiscal_drag = FALSE,
                           govt_rate = CONFIG$govt_defaults$rate, govt_threshold = CONFIG$govt_defaults$threshold,
                           govt_cap = CONFIG$govt_defaults$cap, breaks = list(), voluntary_contributions = list(),
                           home_withdrawal_enabled = FALSE, home_withdrawal_age = NA_real_, model_start_date = Sys.Date()) {
  dob <- as.Date(dob)
  model_start_date <- as.Date(model_start_date)
  if (is.na(model_start_date)) model_start_date <- Sys.Date()
  withdrawal_date <- get_retirement_date(dob, withdrawal_age)
  if (is.na(withdrawal_date) && is.finite(years_total_override)) withdrawal_date <- model_start_date + round(years_total_override * 365.25)
  if (is.na(withdrawal_date) || withdrawal_date <= model_start_date) {
    return(list(nom = current_balance, mTot = 0, vTot = 0, eTot = 0, gTot = 0, rTot = 0, hTot = 0, model_start_date = model_start_date, model_run_date = Sys.Date()))
  }

  pir <- get_pir_rate_from_income(annual_income)
  annual_return_rate <- as.numeric(CONFIG$returns_by_pir[[strategy]][as.character(pir)] %||% 0) / 100
  wage_growth_rate <- (wage_growth %||% 0) / 100

  event_dates <- build_projection_dates(dob, model_start_date, withdrawal_date, breaks, voluntary_contributions, home_withdrawal_age)
  if (length(event_dates) < 2) {
    return(list(nom = current_balance, mTot = 0, vTot = 0, eTot = 0, gTot = 0, rTot = 0, hTot = 0, model_start_date = model_start_date, model_run_date = Sys.Date()))
  }

  balance <- current_balance
  total_member <- total_voluntary <- total_employer <- total_govt <- total_returns <- total_home <- 0
  member_sum_this_govt_year <- 0
  income_exceeded_this_govt_year <- FALSE
  oneoff_applied <- rep(FALSE, length(voluntary_contributions))
  home_done <- FALSE

  # If a first-home withdrawal is set exactly at the model start date, apply it before the first segment.
  home_withdrawal_date <- age_to_date(dob, home_withdrawal_age)
  if (isTRUE(home_withdrawal_enabled) && is.finite(home_withdrawal_age) && !is.na(home_withdrawal_date) && home_withdrawal_date <= model_start_date) {
    withdrawal_amount <- max(0, balance - CONFIG$home_withdrawal_retain)
    if (withdrawal_amount > 0) {
      total_home <- total_home + withdrawal_amount
      balance <- balance - withdrawal_amount
    }
    home_done <- TRUE
  }

  annual_salary_at <- function(date) {
    elapsed_years <- max(0, floor(as.numeric(as.Date(date) - model_start_date) / 365.25 + 1e-9))
    annual_income * (1 + wage_growth_rate)^elapsed_years
  }

  # Use exact dates for Government contribution age eligibility.
  # This avoids decimal-age rounding issues at the 16th and 65th birthdays.
  eligibility_start_date <- age_to_date(dob, 16)
  eligibility_end_date <- age_to_date(dob, 65)

  for (i in seq_len(length(event_dates) - 1)) {
    segment_start <- event_dates[i]
    segment_end <- event_dates[i + 1]
    if (segment_end <= segment_start) next

    year_fraction <- as.numeric(segment_end - segment_start) / 365.25
    opening_balance <- balance

    # One-off voluntary contributions that occur exactly at the segment start are added before returns for that segment.
    oneoff_at_start <- 0
    if (length(voluntary_contributions)) {
      for (j in seq_along(voluntary_contributions)) {
        item <- voluntary_contributions[[j]]
        if (identical(item$type, "oneoff") && !isTRUE(oneoff_applied[j])) {
          contribution_date <- age_to_date(dob, item$startAge)
          if (!is.na(contribution_date) &&
              contribution_date == segment_start &&
              !is.na(eligibility_start_date) &&
              !is.na(eligibility_end_date) &&
              contribution_date >= eligibility_start_date &&
              contribution_date < eligibility_end_date) {
            oneoff_at_start <- oneoff_at_start + item$amount
            oneoff_applied[j] <- TRUE
          }
        }
      }
    }
    if (oneoff_at_start > 0) {
      balance <- balance + oneoff_at_start
      total_voluntary <- total_voluntary + oneoff_at_start
      member_sum_this_govt_year <- member_sum_this_govt_year + oneoff_at_start
    }

    balance_after_interest <- balance * (1 + annual_return_rate)^year_fraction
    interest_earned <- balance_after_interest - balance
    total_returns <- total_returns + interest_earned

    annual_salary <- annual_salary_at(segment_start)
    esct_rate <- if (isTRUE(fiscal_drag)) get_esct_rate_for_income(annual_salary) else get_esct_rate_for_income(annual_income)
    eligible_age <- !is.na(eligibility_start_date) &&
      !is.na(eligibility_end_date) &&
      segment_start >= eligibility_start_date &&
      segment_start < eligibility_end_date

    active_break <- active_break_on_date(segment_start, breaks, dob)
    member_rate_used <- member_rate / 100
    employer_rate_used <- employer_rate / 100
    if (!is.null(active_break)) {
      if (identical(active_break$pauseType, "pause-member")) member_rate_used <- 0
      if (identical(active_break$pauseType, "pause-both")) { member_rate_used <- 0; employer_rate_used <- 0 }
      if (identical(active_break$pauseType, "reduce-member") && is.finite(active_break$reducedRate)) member_rate_used <- active_break$reducedRate / 100
      if (identical(active_break$pauseType, "reduce-both") && is.finite(active_break$reducedRate)) { member_rate_used <- active_break$reducedRate / 100; employer_rate_used <- active_break$reducedRate / 100 }
    }

    member_contribution <- if (eligible_age) annual_salary * member_rate_used * year_fraction else 0
    recurring_voluntary <- 0
    if (eligible_age && length(voluntary_contributions)) {
      for (item in voluntary_contributions) {
        if (identical(item$type, "recurring")) {
          start_date <- age_to_date(dob, item$startAge)
          end_date <- age_to_date(dob, item$endAge)
          if (!is.na(start_date) && !is.na(end_date) && segment_start >= start_date && segment_start < end_date) {
            recurring_voluntary <- recurring_voluntary + item$amount * year_fraction
          }
        }
      }
    }
    employer_gross <- if (eligible_age) annual_salary * employer_rate_used * year_fraction else 0
    employer_contribution <- employer_gross * (1 - esct_rate / 100)

    govt_contribution <- 0
    if (eligible_age) {
      member_sum_this_govt_year <- member_sum_this_govt_year + member_contribution + recurring_voluntary
      if (annual_salary > govt_threshold) income_exceeded_this_govt_year <- TRUE
    }

    is_govt_boundary <- identical(segment_end, next_govt_contribution_boundary(segment_start))
    is_final_segment <- identical(segment_end, withdrawal_date)
    if (is_govt_boundary || is_final_segment) {
      if (!income_exceeded_this_govt_year) govt_contribution <- min(govt_cap, member_sum_this_govt_year * (govt_rate / 100))
      member_sum_this_govt_year <- 0
      income_exceeded_this_govt_year <- FALSE
    }

    top_up <- 0
    if (!is.null(active_break) && isTRUE(active_break$topUpEnabled)) {
      break_end_date <- age_to_date(dob, active_break$endAge)
      if (!is.na(break_end_date) && identical(segment_end, break_end_date)) top_up <- max(0, round(active_break$topUpAmount %||% 0))
    }

    balance <- balance_after_interest + member_contribution + recurring_voluntary + employer_contribution + govt_contribution + top_up
    total_member <- total_member + member_contribution
    total_voluntary <- total_voluntary + recurring_voluntary
    total_employer <- total_employer + employer_contribution
    total_govt <- total_govt + govt_contribution + top_up

    if (isTRUE(home_withdrawal_enabled) && !home_done && is.finite(home_withdrawal_age) && !is.na(home_withdrawal_date) && identical(segment_end, home_withdrawal_date)) {
      withdrawal_amount <- max(0, balance - CONFIG$home_withdrawal_retain)
      if (withdrawal_amount > 0) {
        total_home <- total_home + withdrawal_amount
        balance <- balance - withdrawal_amount
      }
      home_done <- TRUE
    }
  }

  list(nom = balance, mTot = total_member, vTot = total_voluntary, eTot = total_employer, gTot = total_govt, rTot = total_returns, hTot = total_home, model_start_date = model_start_date, model_run_date = Sys.Date())
}

run_person_projection <- function(person_inputs, scenario_settings, is_partner = FALSE) {
  projection_years <- person_inputs$projection_years
  if (!(is.finite(projection_years) && projection_years > 0)) projection_years <- person_inputs$withdrawal_age - person_inputs$start_age
  p <- calc_kiwisaver(person_inputs$dob, person_inputs$start_age, person_inputs$withdrawal_age, person_inputs$current_balance, person_inputs$annual_income, person_inputs$member_rate, person_inputs$employer_rate, person_inputs$strategy, scenario_settings$wage_growth, projection_years, scenario_settings$fiscal_drag, scenario_settings$govt_rate, scenario_settings$govt_threshold, scenario_settings$govt_cap, person_inputs$breaks, person_inputs$voluntary_contributions, person_inputs$home_withdrawal$enabled, person_inputs$home_withdrawal$age, model_start_date = get_model_start_date(person_inputs$dob, person_inputs$model_start_choice %||% "today"))
  display_inflation_factor <- if (scenario_settings$display_real) (1 + scenario_settings$inflation_rate_decimal)^(max(0, projection_years)) else 1
  p$display_inflation_factor <- display_inflation_factor
  p$displayed_balance <- if (scenario_settings$display_real) p$nom / display_inflation_factor else p$nom
  p$displayed_home_withdrawal <- if (scenario_settings$display_real) p$hTot / display_inflation_factor else p$hTot
  p
}

build_balance_breakdown <- function(person_inputs, projection) {
  final_deflator <- projection$display_inflation_factor %||% 1
  if (!(is.finite(final_deflator) && final_deflator > 0)) final_deflator <- 1
  rows <- data.frame(
    Component = c("Starting balance", "Member contributions", "Voluntary contributions", "Employer contributions", "Government contributions", "Investment returns", "Home withdrawals"),
    Nominal = c(person_inputs$current_balance, projection$mTot, projection$vTot, projection$eTot, projection$gTot, projection$rTot, -projection$hTot),
    stringsAsFactors = FALSE
  )
  rows$Real <- rows$Nominal / final_deflator
  attr(rows, "nominal_total") <- sum(rows$Nominal, na.rm = TRUE)
  attr(rows, "real_total") <- sum(rows$Real, na.rm = TRUE)
  attr(rows, "nominal_balance") <- projection$nom
  attr(rows, "real_balance") <- projection$nom / final_deflator
  rows
}

simulate_ks_decumulation_schedule <- function(balance_at_withdrawal_nom, withdraw_age, start_age, rule, inflation_rate_decimal, retirement_return_decimal) {
  target_age <- CONFIG$drawdown_target_age
  ages <- 65:target_age
  out <- setNames(vector("list", length(ages)), as.character(ages))
  zero_row <- function() list(incomeNom = 0, incomeRealToday = 0, endBalNom = 0, endBalRealToday = 0)
  withdraw_age <- round(withdraw_age)
  if (!is.finite(withdraw_age) || withdraw_age > target_age) { for (age in ages) out[[as.character(age)]] <- zero_row(); return(out) }
  for (age in ages[ages < withdraw_age]) out[[as.character(age)]] <- zero_row()
  if (!(is.finite(balance_at_withdrawal_nom) && balance_at_withdrawal_nom > 0 && is.finite(retirement_return_decimal))) { for (age in ages[ages >= withdraw_age]) out[[as.character(age)]] <- zero_row(); return(out) }
  fixed_pay_real_today <- 0
  if (identical(rule, "fixed-date")) {
    n <- max(1, target_age - withdraw_age)
    years_to_retirement <- max(0, withdraw_age - start_age)
    balance_real <- balance_at_withdrawal_nom / ((1 + inflation_rate_decimal)^years_to_retirement)
    real_return <- ((1 + retirement_return_decimal) / (1 + inflation_rate_decimal)) - 1
    fixed_pay_real_today <- if (abs(real_return) < 1e-12) balance_real / n else balance_real * real_return / ((1 - (1 + real_return)^(-n)) * (1 + real_return))
  }
  base_pct <- if (identical(rule, "4percent")) 0.04 else if (identical(rule, "6percent")) 0.06 else 0
  base_drawdown_nom <- if (base_pct > 0) balance_at_withdrawal_nom * base_pct else 0
  bal_nom <- balance_at_withdrawal_nom
  for (age in ages[ages >= withdraw_age]) {
    if (identical(rule, "fixed-date") && age >= target_age) { out[[as.character(age)]] <- zero_row(); bal_nom <- 0; next }
    start_bal <- bal_nom
    if (!(start_bal > 0)) { out[[as.character(age)]] <- zero_row(); bal_nom <- 0; next }
    years_since_ret <- max(0, age - withdraw_age)
    years_from_start <- max(0, age - start_age)
    drawdown_nom <- if (rule %in% c("4percent", "6percent")) base_drawdown_nom * (1 + inflation_rate_decimal)^years_since_ret else if (identical(rule, "fixed-date")) fixed_pay_real_today * (1 + inflation_rate_decimal)^years_from_start else balance_at_withdrawal_nom * 0.04 * (1 + inflation_rate_decimal)^years_since_ret
    drawdown_nom <- max(0, min(start_bal, drawdown_nom))
    end_bal <- max(0, (start_bal - drawdown_nom) * (1 + retirement_return_decimal))
    deflator <- (1 + inflation_rate_decimal)^years_from_start
    out[[as.character(age)]] <- list(incomeNom = drawdown_nom, incomeRealToday = drawdown_nom / deflator, endBalNom = end_bal, endBalRealToday = end_bal / deflator)
    bal_nom <- end_bal
  }
  out
}

compute_retirement_outputs <- function(primary_inputs, primary_projection, scenario_settings, partner_inputs = NULL, partner_projection = NULL) {
  couple_mode <- scenario_settings$is_couple_mode
  iR <- scenario_settings$inflation_rate_decimal
  rR <- scenario_settings$retirement_return_decimal
  wg <- scenario_settings$wage_growth
  nza <- scenario_settings$nz_super_age
  indexation <- scenario_settings$nz_super_indexation
  nz_base_single_now <- scenario_settings$nz_super_annual_single
  nz_base_couple_now <- scenario_settings$nz_super_annual_shared
  nz_disc_rate <- scenario_settings$nz_super_discount_rate
  nz_horizon <- max(1, scenario_settings$nz_super_npv_horizon)
  primary_start_age <- primary_inputs$start_age
  partner_start_age <- if (!is.null(partner_inputs)) partner_inputs$start_age else NA_real_
  full_years <- function(x) max(0, floor((x %||% 0) + 1e-9))
  indexed_couple_nzs_annual_hybrid <- function(years) {
    years <- max(0, years)
    cpi_indexed <- nz_base_couple_now * (1 + max(scenario_settings$inflation_rate / 100, 0))^years
    net_aotwe_now <- if (nz_base_couple_now > 0) nz_base_couple_now / 0.66 else 0
    net_aotwe_year <- net_aotwe_now * (1 + wg / 100)^years
    min(max(cpi_indexed, 0.66 * net_aotwe_year), 0.725 * net_aotwe_year)
  }
  nzs_payment_annual <- function(years_from_start, eligible_count) {
    if (eligible_count <= 0) return(0)
    years <- full_years(years_from_start)
    if (identical(indexation, "hybrid")) {
      couple_rate <- indexed_couple_nzs_annual_hybrid(years)
      if (eligible_count == 2) return(couple_rate)
      return(if (couple_mode) couple_rate / 2 else 0.65 * couple_rate)
    }
    factor <- if (identical(indexation, "none")) 1 else if (identical(indexation, "cpi")) (1 + max(scenario_settings$inflation_rate / 100, 0))^years else 1
    if (eligible_count == 2) return(nz_base_couple_now * factor)
    if (eligible_count == 1) return(if (couple_mode) (nz_base_couple_now * factor) / 2 else nz_base_single_now * factor)
    0
  }
  get_income <- function(current_income, years_from_start) if (!(is.finite(current_income) && current_income > 0)) 0 else current_income * (1 + wg / 100)^max(0, years_from_start)
  get_assessable_income <- function(years_from_start) {
    x <- get_income(primary_inputs$annual_income, years_from_start)
    if (couple_mode && !is.null(partner_inputs)) x <- x + get_income(partner_inputs$annual_income, years_from_start)
    x
  }
  apply_income_test <- function(base_nzs_annual, age_for_test, years_from_start) {
    if (!(isTRUE(scenario_settings$nz_super_income_test_enabled) && scenario_settings$nz_super_income_test_stop_age > scenario_settings$nz_super_age)) return(base_nzs_annual)
    if (!(age_for_test >= scenario_settings$nz_super_age && age_for_test < scenario_settings$nz_super_income_test_stop_age)) return(base_nzs_annual)
    abatement <- max(0, get_assessable_income(years_from_start) - scenario_settings$nz_super_income_test_threshold) * scenario_settings$nz_super_income_test_abatement_rate
    max(0, base_nzs_annual - abatement)
  }
  years_to_elig_primary <- max(0, nza - primary_start_age)
  years_to_elig_partner <- if (couple_mode && is.finite(partner_start_age)) max(0, nza - partner_start_age) else NA_real_
  first_eligibility <- if (couple_mode && is.finite(years_to_elig_partner)) min(years_to_elig_primary, years_to_elig_partner) else years_to_elig_primary
  nzs_pv_at_elig <- 0
  for (t in 0:(nz_horizon - 1)) {
    years_from_start <- first_eligibility + t
    eligible_count <- if (!couple_mode) 1 else ifelse(primary_start_age + years_from_start >= nza, 1, 0) + ifelse(is.finite(partner_start_age) && partner_start_age + years_from_start >= nza, 1, 0)
    payment_base <- nzs_payment_annual(years_from_start, eligible_count)
    age_for_test <- if (!couple_mode) primary_start_age + years_from_start else max(primary_start_age, partner_start_age %||% primary_start_age) + years_from_start
    nzs_pv_at_elig <- nzs_pv_at_elig + apply_income_test(payment_base, age_for_test, years_from_start) / (1 + nz_disc_rate)^t
  }
  nzs_displayed <- if (scenario_settings$display_real) nzs_pv_at_elig / ((1 + iR)^full_years(first_eligibility)) else nzs_pv_at_elig
  ks_schedule_you <- simulate_ks_decumulation_schedule(primary_projection$nom, primary_inputs$withdrawal_age, primary_inputs$start_age, scenario_settings$income_rule, iR, rR)
  ks_schedule_partner <- if (couple_mode && !is.null(partner_projection)) simulate_ks_decumulation_schedule(partner_projection$nom, partner_inputs$withdrawal_age, partner_inputs$start_age, scenario_settings$income_rule, iR, rR) else NULL
  age_today_int <- floor(age_from_dob(primary_inputs$dob, Sys.Date()))
  years_to_ret_age <- max(0, primary_inputs$withdrawal_age - age_today_int)
  net_at_ret_age <- calculate_disposable_income_with_acc(primary_inputs$annual_income * (1 + wg / 100)^years_to_ret_age)
  base_index_years <- max(0, round(nza) - age_today_int)
  year1_nzsuper_base <- if (!couple_mode && primary_inputs$withdrawal_age >= nza) { if (identical(indexation, "hybrid")) 0.65 * indexed_couple_nzs_annual_hybrid(base_index_years) else nz_base_single_now } else 0
  year1_nzsuper <- apply_income_test(year1_nzsuper_base, primary_inputs$withdrawal_age, years_to_ret_age)
  year1_ks <- (ks_schedule_you[[as.character(round(primary_inputs$withdrawal_age))]] %||% list(incomeNom = 0))$incomeNom
  replacement_rate <- if (!couple_mode && round(primary_inputs$withdrawal_age) == 65 && net_at_ret_age > 0) (year1_ks + year1_nzsuper) / net_at_ret_age else NA_real_
  replacement_rate_note <- if (couple_mode) "Replacement rate is not shown in couple mode because a single household replacement rate can be misleading when partners have different ages or withdrawal ages." else if (round(primary_inputs$withdrawal_age) != 65) "Replacement rate is only shown when KiwiSaver withdrawal age is 65." else "First-year retirement income at age 65 as a share of projected disposable employment income at age 65, after income tax and ACC earners' levy."
  income_rows <- lapply(65:90, function(age) {
    years_from_start <- max(0, age - primary_inputs$start_age)
    you_row <- ks_schedule_you[[as.character(age)]] %||% list(incomeNom = 0, incomeRealToday = 0)
    ks_nom <- you_row$incomeNom
    ks_real <- you_row$incomeRealToday
    if (couple_mode && !is.null(ks_schedule_partner) && !is.null(partner_inputs)) {
      partner_age_now <- partner_inputs$start_age + (age - primary_inputs$start_age)
      partner_row <- if (is.finite(partner_age_now) && partner_age_now >= 65 && partner_age_now <= 90) ks_schedule_partner[[as.character(round(partner_age_now))]] %||% list(incomeNom = 0, incomeRealToday = 0) else list(incomeNom = 0, incomeRealToday = 0)
      ks_nom <- ks_nom + partner_row$incomeNom
      ks_real <- ks_real + partner_row$incomeRealToday
    }
    eligible_count <- if (!couple_mode) ifelse(age >= nza, 1, 0) else {
      partner_age_now <- partner_inputs$start_age + (age - primary_inputs$start_age)
      ifelse(age >= nza, 1, 0) + ifelse(is.finite(partner_age_now) && partner_age_now >= nza, 1, 0)
    }
    years_rr <- if (age < round(nza)) full_years(age - primary_inputs$start_age) else max(0, round(nza) - floor(age_from_dob(primary_inputs$dob, Sys.Date()))) + (age - round(nza))
    nzs_nom_base <- if (eligible_count > 0) nzs_payment_annual(years_rr, eligible_count) else 0
    age_for_income_test <- if (!couple_mode) age else max(age, partner_inputs$start_age + (age - primary_inputs$start_age))
    nzs_nom <- apply_income_test(nzs_nom_base, age_for_income_test, years_from_start)
    nzs_real <- if (years_from_start > 0) nzs_nom / (1 + iR)^years_from_start else nzs_nom
    data.frame(Age = age, KiwiSaver = if (scenario_settings$display_real) ks_real else ks_nom, NZSuper = if (scenario_settings$display_real) nzs_real else nzs_nom, Total = if (scenario_settings$display_real) ks_real + nzs_real else ks_nom + nzs_nom, stringsAsFactors = FALSE)
  })
  list(
    nz_super_value = nzs_displayed,
    nz_super_details = paste0(if (identical(indexation, "none")) "Static" else if (identical(indexation, "cpi")) "Indexed (CPI only)" else "Indexed (CPI + wage band)", if (isTRUE(scenario_settings$nz_super_income_test_enabled)) paste0(" + income-tested (", scenario_settings$nz_super_income_basis, ")") else "", if (scenario_settings$display_real) " - lump-sum equivalent at eligibility age (today's $, in advance)" else " - lump-sum equivalent at eligibility age (nominal $, in advance)"),
    replacement_rate = replacement_rate,
    replacement_rate_note = replacement_rate_note,
    income_table = do.call(rbind, income_rows)
  )
}

# =============================================================================
# SECTION 5: UI COMPONENT HELPERS
# =============================================================================

static_display <- function(label, value) tagList(tags$label(label), tags$div(class = "readonly-field", value))

break_inputs_ui <- function(suffix = "") {
  tagList(
    checkboxInput(paste0("homeWithdrawalEnabled", suffix), "Withdraw KiwiSaver for first home", FALSE),
    numericInput(paste0("homeWithdrawalAge", suffix), "Age at withdrawal", value = NA, min = 0, max = 120, step = 0.1),
    lapply(1:3, function(k) tags$div(class = "mini-card", tags$h5(sprintf("Break %s", k)), checkboxInput(paste0("break", k, "Enabled", suffix), "Use this contribution break", FALSE), tags$p(class = "muted-note", "Start and end age are required only if this break is enabled."), fluidRow(column(6, numericInput(paste0("break", k, "StartAge", suffix), "Start age", value = NA, min = 0, max = 120, step = 0.1)), column(6, numericInput(paste0("break", k, "EndAge", suffix), "End age", value = NA, min = 0, max = 120, step = 0.1))), fluidRow(column(4, selectInput(paste0("break", k, "PauseType", suffix), "Contribution change", choices = c("Pause member only" = "pause-member", "Pause member + employer" = "pause-both", "Reduce member only" = "reduce-member", "Reduce member + employer" = "reduce-both"), selected = "pause-member")), column(4, numericInput(paste0("break", k, "ReducedRate", suffix), "Reduced rate (%)", value = 3, min = 0, max = 100, step = 0.1)), column(4, checkboxInput(paste0("break", k, "TopUpEnabled", suffix), "Apply govt top-up", FALSE), numericInput(paste0("break", k, "TopUpAmount", suffix), "Top-up amount ($)", value = 1000, min = 0, max = CONFIG$money_limits$max_break_top_up, step = 1)))))
  )
}

voluntary_inputs_ui <- function(suffix = "") {
  tagList(tags$p(class = "muted-note", "Optional extra contributions outside standard member and employer rates. Recurring amounts are annual and spread exactly across active days."), lapply(1:2, function(k) tags$div(class = "mini-card", tags$h5(sprintf("Voluntary contribution %s", k)), fluidRow(column(4, selectInput(paste0("voluntary", k, "Type", suffix), "Type", choices = c("None" = "none", "One-off" = "oneoff", "Recurring" = "recurring"), selected = "none")), column(4, numericInput(paste0("voluntary", k, "StartAge", suffix), "Start age", value = NA, min = 0, max = 120, step = 0.1)), column(4, numericInput(paste0("voluntary", k, "Amount", suffix), "Amount ($)", value = 0, min = 0, max = CONFIG$money_limits$max_voluntary_contribution, step = 1))), fluidRow(column(6, numericInput(paste0("voluntary", k, "EndAge", suffix), "End age (required if recurring)", value = NA, min = 0, max = 120, step = 0.1)), column(6, static_display("Counts for govt subsidy", "Yes, if eligible"))))))
}

person_inputs_ui <- function(title, suffix = "") {
  dob_id <- input_id("dob", suffix)
  withdrawal_age_id <- input_id("withdrawalAge", suffix)
  current_balance_id <- input_id("currentBalance", suffix)
  annual_income_id <- input_id("annualIncome", suffix)
  member_rate_id <- input_id("memberRate", suffix)
  employer_rate_id <- input_id("employerRate", suffix)
  strategy_id <- input_id("strategy", suffix)
  age_output <- input_id("currentAgeUI", suffix)
  tags$details(open = TRUE, tags$summary(title), div(class = "details-body", fluidRow(column(6, dateInput(dob_id, "Date of birth", value = "1991-01-03", min = CONFIG$date_limits$min_dob, max = Sys.Date())), column(6, static_display("Model start date", "Today"))), fluidRow(column(6, uiOutput(age_output)), column(6, numericInput(withdrawal_age_id, "Withdrawal age", value = 65, min = 65, max = CONFIG$drawdown_target_age, step = 1))), fluidRow(column(6, textInput(current_balance_id, "KiwiSaver balance ($)", value = "5,000")), column(6, textInput(annual_income_id, "Annual gross income ($)", value = "80,000"))), fluidRow(column(6, numericInput(member_rate_id, "Member contribution (%)", value = 4, min = 0, max = 100, step = 0.1)), column(6, numericInput(employer_rate_id, "Employer contribution (%)", value = 4, min = 0, max = 100, step = 0.1))), selectInput(strategy_id, "Investment strategy", choices = c("Conservative" = "conservative", "Balanced" = "balanced", "Growth" = "growth", "Aggressive" = "aggressive"), selected = "balanced")))
}

govt_inputs_ui <- function(shared = FALSE) {
  prefix <- if (shared) "shared" else ""
  tags$details(tags$summary("KiwiSaver Government Subsidy"), div(class = "details-body", fluidRow(column(6, numericInput(paste0(prefix, if (shared) "GovtRate" else "govtRate"), "Subsidy rate (%)", value = 25, min = 0, max = 100, step = 0.1)), column(6, textInput(paste0(prefix, if (shared) "GovtThreshold" else "govtThreshold"), "Income threshold ($)", value = "180,000"))), fluidRow(column(6, textInput(paste0(prefix, if (shared) "GovtCap" else "govtCap"), "Annual cap ($)", value = "260.72"))), tags$p(class = "muted-note", "Government subsidy is calculated over contribution years ending 30 June. Final partial contribution years are included.")))
}

nz_super_inputs_ui <- function(shared = FALSE) {
  suf <- if (shared) "Shared" else ""
  annual_default <- if (shared) "44,550" else "28,950"
  income_test_id <- paste0("nzSuperIncomeTestEnabled", suf)
  tags$details(tags$summary("NZ Super"), div(class = "details-body", fluidRow(column(6, numericInput(paste0("nzSuperAge", suf), "Eligibility age", value = 65, min = 60, max = 80, step = 1)), column(6, textInput(paste0("nzSuperAnnual", suf), "Annual rate ($)", value = annual_default))), fluidRow(column(6, numericInput(paste0("nzSuperDiscountRate", suf), "NPV discount rate (%)", value = 4.5, min = 0, max = 15, step = 0.1)), column(6, numericInput(paste0("nzSuperNpvHorizon", suf), "NPV horizon (years)", value = 25, min = 1, max = 50, step = 1))), selectInput(paste0("nzSuperIndexation", suf), "Indexation", choices = c("None" = "none", "CPI" = "cpi", "CPI + wage band" = "hybrid"), selected = "hybrid"), checkboxInput(income_test_id, "Apply income testing", FALSE), conditionalPanel(condition = sprintf("input['%s'] == true", income_test_id), fluidRow(column(6, textInput(paste0("nzSuperIncomeTestThreshold", suf), "Income threshold ($)", value = "0")), column(6, numericInput(paste0("nzSuperIncomeTestAbatementRate", suf), "Abatement rate (%)", value = 50, min = 0, max = 100, step = 0.1))), fluidRow(column(6, numericInput(paste0("nzSuperIncomeTestStopAge", suf), "Income testing stops at age", value = 70, min = 0, max = 120, step = 1)), column(6, static_display("Assessable income basis", if (shared) "Combined employment income (projected)" else "Employment income (projected)"))))))
}

decum_inputs_ui <- function(shared = FALSE) {
  id <- if (shared) "incomeRuleShared" else "incomeRule"
  tags$details(tags$summary("Decumulation Strategy"), div(class = "details-body", selectInput(id, "Withdrawal rule", choices = c("4% rule" = "4percent", "6% rule" = "6percent", "Real annuity to age 90" = "fixed-date"), selected = "4percent")))
}

invest_inputs_ui <- function(shared = FALSE) {
  suf <- if (shared) "Shared" else ""
  display_id <- if (shared) "displayModeShared" else "displayMode"
  fiscal_id <- if (shared) "fiscalDragShared" else "fiscalDrag"
  tags$details(tags$summary("Investment Assumptions"), div(class = "details-body", fluidRow(column(4, numericInput(paste0("retirementReturn", suf), "Return after 65 (%)", value = 2.0, min = -10, max = 15, step = 0.1)), column(4, numericInput(paste0("wageGrowth", suf), "Annual wage growth (%)", value = 3.5, min = -2, max = 10, step = 0.1)), column(4, numericInput(paste0("inflationRate", suf), "Inflation rate (%)", value = 2.0, min = -2, max = 10, step = 0.1))), fluidRow(column(6, radioButtons(display_id, "Display results", choices = c("Real" = "real", "Nominal" = "nominal"), selected = "real", inline = TRUE)), column(6, radioButtons(fiscal_id, "Fiscal Drag (ESCT)", choices = c("Yes" = "yes", "No" = "no"), selected = "no", inline = TRUE)))))
}

# =============================================================================
# SECTION 6: STYLING AND UI
# =============================================================================

app_styles <- HTML("
  *{box-sizing:border-box}
  :root{--sorted-orange:#6b8db5;--sorted-grey:#eef1f5;--sorted-red:#c84d4d;--text-on-navy:#2c3e50;--card-ink:#1f2937;--panel-bg:#ffffff;--panel-inner:#f8fafc;--line:#dde3ea;--muted:#64748b}
  body{background:#f5f7fa;min-height:100vh;padding:20px 14px 28px;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
  .container-fluid{max-width:1840px;margin:0 auto}.app-header{text-align:center;margin-bottom:26px;padding-top:4px}.app-title-main{color:var(--text-on-navy);font-size:30px;line-height:1.08;font-weight:700;margin:0 0 18px;font-family:Georgia,'Times New Roman',serif}
  .top-controls{display:grid;grid-template-columns:160px 160px 160px;gap:14px;justify-content:center;align-items:stretch;margin:0 auto}.mode-toggle-wrap,.mode-toggle-wrap .form-group,.mode-toggle-wrap .shiny-input-container,.mode-toggle-wrap .shiny-options-group{display:contents}.mode-toggle-wrap .radio{display:contents;margin:0}.mode-toggle-wrap .radio label,.reset-wrap .btn{width:160px;height:58px;margin:0!important;padding:0!important;border-radius:14px;font-size:16px;font-weight:600;text-align:center;display:flex!important;align-items:center;justify-content:center;box-shadow:none;transition:all .15s ease}.mode-toggle-wrap .radio label{position:relative;background:#eef2f7;border:2px solid #d5dce5;color:#2c3e50;cursor:pointer}.mode-toggle-wrap .radio label input{position:absolute;opacity:0;pointer-events:none;width:0;height:0}.mode-toggle-wrap .radio label:hover{background:#e4ebf3;border-color:#c7d2df}.mode-toggle-wrap .radio label:has(input:checked){background:var(--sorted-orange);border-color:var(--sorted-orange);color:#ffffff}.reset-wrap{margin:0;display:contents}.reset-wrap .btn{background:#ffffff;border:1px solid #c7ccd4;color:var(--card-ink)}.reset-wrap .btn:hover{background:#f8fafc;border-color:#aeb8c5;color:var(--card-ink)}
  .left-column,.right-column{padding-left:16px!important;padding-right:16px!important}.main-card{background:var(--panel-bg);border-radius:26px;padding:36px 42px 34px;box-shadow:none;border:none}.section-head{color:var(--card-ink);font-size:18px;font-weight:500;text-transform:uppercase;letter-spacing:1px;margin:0 0 28px;border-bottom:3px solid var(--line);padding-bottom:16px}.well{background:transparent;border:none;box-shadow:none;padding:0;margin:0}
  details{border:none;background:transparent;padding:0;margin-bottom:18px}details>summary{cursor:pointer;user-select:none;display:flex;align-items:center;gap:8px;background:var(--sorted-grey);padding:18px 20px;border-radius:14px;margin-bottom:0;color:var(--card-ink);font-size:17px;font-weight:600;list-style:none}details>summary::-webkit-details-marker{display:none}details>summary::marker{display:none}.details-body{padding:14px 4px 4px}.mini-card{border:1px solid #e3e4ea;border-radius:14px;padding:14px;background:#f9fafe;margin-bottom:12px}.mini-card h5{margin:0 0 12px;font-size:16px;color:var(--card-ink)}label{display:block;margin-bottom:6px;color:var(--card-ink);font-weight:600;font-size:13px}input[type='text'],input[type='number'],input[type='date'],select{width:100%;padding:11px 12px;border:1px solid #d8dbe5;border-radius:10px;font-size:14px;color:var(--card-ink);min-height:44px;background:#ffffff;box-shadow:none}.shiny-input-container{width:100%}.radio label,.checkbox label{font-weight:500}
  .summary-box{background:var(--panel-inner);border-radius:20px;padding:26px 30px;box-shadow:none;border:1px solid #e4e5ea;margin-bottom:20px}.accent-card{border-left:6px solid var(--sorted-orange)}.summary-label{color:#667788;font-size:13px;text-transform:uppercase;letter-spacing:.5px;font-weight:700;margin-bottom:10px}.summary-value{color:var(--card-ink);font-size:24px;font-weight:700;line-height:1.05;margin-top:0}.muted-note{color:var(--muted);font-size:14px;line-height:1.45}.readonly-field{min-height:44px;padding:11px 12px;background:#f8f9fc;border:1px solid #d8dbe5;border-radius:10px;color:var(--card-ink)}.error-box{background:#ffebee;border-left:4px solid var(--sorted-red);padding:12px 14px;border-radius:10px;color:#c62828;margin-bottom:16px}.info-box{background:#f8f9fc;border-left:4px solid var(--sorted-orange);padding:12px 14px;border-radius:10px;color:var(--muted);margin-top:14px;margin-bottom:16px;font-size:13px;line-height:1.45}.placeholder-box{text-align:center;padding:40px 20px;color:#7f8698}.balance-card h4,.results-table h4{font-size:14px;color:var(--card-ink);margin:0 0 18px;font-weight:600}
  .results-table{padding:24px 18px;overflow-x:hidden}.results-table .shiny-table,.results-table table{width:100%!important;max-width:100%!important;margin-top:10px;border-collapse:collapse;table-layout:fixed;font-size:13.5px;background:transparent}.results-table th{background:transparent;padding:11px 8px;text-align:right;font-weight:700;color:var(--card-ink);border-bottom:2px solid #d7d9e1;white-space:nowrap}.results-table td{padding:10px 8px;border-bottom:1px solid #d7d9e1;text-align:right;color:var(--card-ink);white-space:nowrap;font-variant-numeric:tabular-nums}.results-table th:first-child,.results-table td:first-child{text-align:left;width:12%}.results-table th:nth-child(2),.results-table td:nth-child(2){width:30%}.results-table th:nth-child(3),.results-table td:nth-child(3){width:30%}.results-table th:nth-child(4),.results-table td:nth-child(4){width:28%}
  table{width:100%;margin-top:8px;border-collapse:collapse;font-size:14px;background:transparent}th{background:transparent;padding:10px 8px;text-align:right;font-weight:700;color:var(--card-ink);border-bottom:2px solid #d7d9e1}th:first-child,td:first-child{text-align:left}td{padding:10px 8px;border-bottom:1px solid #d7d9e1;text-align:right;color:var(--card-ink)}ul.compact-list{list-style:none;padding-left:0;margin:0}ul.compact-list li{display:flex;justify-content:space-between;gap:20px;padding:12px 0;border-bottom:1px solid #d7d9e1;font-size:15px;color:var(--card-ink)}ul.compact-list li:last-child{border-bottom:none}.shared-group details{margin-bottom:12px}.mode-note{margin-top:-8px;margin-bottom:14px;color:var(--muted);font-size:13px;line-height:1.4}@media (max-width:1100px){.app-title-main{font-size:30px}.main-card{padding:28px 24px}}@media (max-width:620px){.top-controls{grid-template-columns:minmax(0,1fr);width:min(100%,340px);gap:10px}.mode-toggle-wrap .radio label,.reset-wrap .btn{width:100%;height:54px;font-size:16px}.results-table{padding:20px 12px}.results-table .shiny-table,.results-table table{font-size:12.5px}.results-table th,.results-table td{padding:9px 5px}}
")

ui <- fluidPage(
  tags$head(tags$title("Retirement Income Calculator (R Shiny)"), tags$style(app_styles)),
  div(class = "app-header", tags$h1(class = "app-title-main", "Prototype Retirement Income Calculator"), div(class = "top-controls", div(class = "mode-toggle-wrap", radioButtons("mode", NULL, choices = c("Individual" = "individual", "Couple" = "couple"), selected = "individual", inline = FALSE)), div(class = "reset-wrap", actionButton("resetInputs", "Reset defaults", class = "btn btn-default")))),
  fluidRow(
    column(width = 6, class = "left-column", div(class = "main-card", tags$h2(class = "section-head", "Details"), wellPanel(person_inputs_ui("Your details", suffix = ""), tags$details(tags$summary("Your contribution breaks"), div(class = "details-body", break_inputs_ui(""))), tags$details(tags$summary("Your voluntary contributions"), div(class = "details-body", voluntary_inputs_ui(""))), conditionalPanel("input.mode == 'individual'", govt_inputs_ui(FALSE), nz_super_inputs_ui(FALSE), decum_inputs_ui(FALSE), invest_inputs_ui(FALSE)), conditionalPanel("input.mode == 'couple'", tags$p(class = "mode-note", "Partner details and shared couple settings are used only in couple mode. Individual-mode assumptions remain separate so scenarios do not silently overwrite each other."), person_inputs_ui("Partner details", suffix = "Partner"), tags$details(tags$summary("Partner contribution breaks"), div(class = "details-body", break_inputs_ui("Partner"))), tags$details(tags$summary("Partner voluntary contributions"), div(class = "details-body", voluntary_inputs_ui("Partner"))), tags$details(tags$summary("Shared couple settings"), div(class = "details-body shared-group", govt_inputs_ui(TRUE), nz_super_inputs_ui(TRUE), decum_inputs_ui(TRUE), invest_inputs_ui(TRUE))))))),
    column(width = 6, class = "right-column", div(class = "main-card", tags$h2(class = "section-head", "Results"), uiOutput("errorUI"), uiOutput("warningUI"), uiOutput("placeholderUI"), uiOutput("headlineUI"), conditionalPanel("input.mode == 'individual'", uiOutput("breakdownUI")), conditionalPanel("input.mode == 'couple'", uiOutput("coupleBalancesUI")), uiOutput("nzSuperUI"), uiOutput("replacementRateUI"), div(class = "summary-box results-table", h4("Annual Retirement Income"), tableOutput("incomeTable"), div(class = "info-box", HTML(paste0("<strong>Estimates only</strong> - Actual returns vary by market performance. Replacement rate is shown only for individual scenarios where KiwiSaver withdrawal age is 65.<br><br><strong>Model version: ", CONFIG$model_version_label, ".</strong> <strong>Model run date: ", format(Sys.Date(), "%d %B %Y"), ".</strong> Calculations use the date on which the model is run. KiwiSaver government subsidy is assessed over contribution years ending 30 June, with final partial years included. Contribution and event timing in the accumulation model is date-based, with monthly contribution checkpoints and exact event-date boundaries. Please treat all outputs as the result of a prototype model. Results may change as the model is developed."))))))
  )
)

# =============================================================================
# SECTION 7: SERVER AND OUTPUT LOGIC
# =============================================================================

server <- function(input, output, session) {
  output$currentAgeUI <- renderUI({ dob_date <- suppressWarnings(as.Date(input$dob)); static_display("Current age", if (is.na(dob_date)) "" else round(age_from_dob(dob_date, Sys.Date()), 1)) })
  output$currentAgeUIPartner <- renderUI({ dob_date <- suppressWarnings(as.Date(input$dobPartner)); static_display("Current age", if (is.na(dob_date)) "" else round(age_from_dob(dob_date, Sys.Date()), 1)) })
  observeEvent(input$resetInputs, {
    updateRadioButtons(session, "mode", selected = "individual")
    for (suffix in c("", "Partner")) {
      updateDateInput(session, paste0("dob", suffix), value = "1991-01-03")
      updateNumericInput(session, paste0("withdrawalAge", suffix), value = 65)
      updateTextInput(session, paste0("currentBalance", suffix), value = "5,000")
      updateTextInput(session, paste0("annualIncome", suffix), value = "80,000")
      updateNumericInput(session, paste0("memberRate", suffix), value = 4)
      updateNumericInput(session, paste0("employerRate", suffix), value = 4)
      updateSelectInput(session, paste0("strategy", suffix), selected = "balanced")
      updateCheckboxInput(session, paste0("homeWithdrawalEnabled", suffix), value = FALSE)
      updateNumericInput(session, paste0("homeWithdrawalAge", suffix), value = NA)
      for (k in 1:3) {
        updateCheckboxInput(session, paste0("break", k, "Enabled", suffix), value = FALSE)
        updateNumericInput(session, paste0("break", k, "StartAge", suffix), value = NA)
        updateNumericInput(session, paste0("break", k, "EndAge", suffix), value = NA)
        updateSelectInput(session, paste0("break", k, "PauseType", suffix), selected = "pause-member")
        updateNumericInput(session, paste0("break", k, "ReducedRate", suffix), value = 3)
        updateCheckboxInput(session, paste0("break", k, "TopUpEnabled", suffix), value = FALSE)
        updateNumericInput(session, paste0("break", k, "TopUpAmount", suffix), value = 1000)
      }
      for (k in 1:2) {
        updateSelectInput(session, paste0("voluntary", k, "Type", suffix), selected = "none")
        updateNumericInput(session, paste0("voluntary", k, "StartAge", suffix), value = NA)
        updateNumericInput(session, paste0("voluntary", k, "Amount", suffix), value = 0)
        updateNumericInput(session, paste0("voluntary", k, "EndAge", suffix), value = NA)
      }
    }
    updateNumericInput(session, "govtRate", value = 25); updateTextInput(session, "govtThreshold", value = "180,000"); updateTextInput(session, "govtCap", value = "260.72")
    updateNumericInput(session, "sharedGovtRate", value = 25); updateTextInput(session, "sharedGovtThreshold", value = "180,000"); updateTextInput(session, "sharedGovtCap", value = "260.72")
    for (suf in c("", "Shared")) {
      updateNumericInput(session, paste0("nzSuperAge", suf), value = 65)
      updateTextInput(session, paste0("nzSuperAnnual", suf), value = if (suf == "Shared") "44,550" else "28,950")
      updateNumericInput(session, paste0("nzSuperDiscountRate", suf), value = 4.5)
      updateNumericInput(session, paste0("nzSuperNpvHorizon", suf), value = 25)
      updateSelectInput(session, paste0("nzSuperIndexation", suf), selected = "hybrid")
      updateCheckboxInput(session, paste0("nzSuperIncomeTestEnabled", suf), value = FALSE)
      updateTextInput(session, paste0("nzSuperIncomeTestThreshold", suf), value = "0")
      updateNumericInput(session, paste0("nzSuperIncomeTestAbatementRate", suf), value = 50)
      updateNumericInput(session, paste0("nzSuperIncomeTestStopAge", suf), value = 70)
      updateNumericInput(session, paste0("retirementReturn", suf), value = 2.0)
      updateNumericInput(session, paste0("wageGrowth", suf), value = 3.5)
      updateNumericInput(session, paste0("inflationRate", suf), value = 2.0)
    }
    updateSelectInput(session, "incomeRule", selected = "4percent"); updateSelectInput(session, "incomeRuleShared", selected = "4percent")
    updateRadioButtons(session, "displayMode", selected = "real"); updateRadioButtons(session, "fiscalDrag", selected = "no")
    updateRadioButtons(session, "displayModeShared", selected = "real"); updateRadioButtons(session, "fiscalDragShared", selected = "no")
  })
  scenario_result <- reactive({
    ss <- read_scenario_settings(input)
    v0 <- validate_scenario_settings(ss)
    if (!isTRUE(v0$valid)) return(list(error = v0))
    primary_inputs <- read_person_inputs(input, "")
    partner_inputs <- if (ss$is_couple_mode) read_person_inputs(input, "Partner") else NULL
    v1 <- validate_person_inputs(primary_inputs, "Your")
    if (!isTRUE(v1$valid)) return(list(error = v1))
    if (ss$is_couple_mode) {
      v2 <- validate_person_inputs(partner_inputs, "Partner")
      if (!isTRUE(v2$valid)) return(list(error = v2))
      gap_error <- validate_couple_age_gap(primary_inputs, partner_inputs)
      if (!is.null(gap_error)) return(list(error = validation_result(gap_error)))
    }
    primary_projection <- run_person_projection(primary_inputs, ss, is_partner = FALSE)
    partner_projection <- if (ss$is_couple_mode) run_person_projection(partner_inputs, ss, is_partner = TRUE) else NULL
    ro <- compute_retirement_outputs(primary_inputs, primary_projection, ss, partner_inputs, partner_projection)
    warnings <- policy_warning_messages(primary_inputs, partner_inputs, ss)
    list(error = NULL, warnings = warnings, scenario_settings = ss, primary_inputs = primary_inputs, partner_inputs = partner_inputs, primary_projection = primary_projection, partner_projection = partner_projection, retirement_outputs = ro)
  })
  output$errorUI <- renderUI({ res <- scenario_result(); if (!is.null(res$error) && !identical(res$error$reason, "incomplete")) div(class = "error-box", paste("Warning:", res$error$message)) else NULL })
  output$warningUI <- renderUI({ res <- scenario_result(); if (!is.null(res$error) || !length(res$warnings)) return(NULL); div(class = "info-box", tags$strong("Scenario warning"), tags$ul(lapply(res$warnings, tags$li))) })
  output$placeholderUI <- renderUI({ res <- scenario_result(); if (!is.null(res$error) && identical(res$error$reason, "incomplete")) div(class = "summary-box placeholder-box", "Fill in your details and results will appear here.") else NULL })
  output$headlineUI <- renderUI({
    res <- scenario_result(); if (!is.null(res$error)) return(NULL)
    ss <- res$scenario_settings; pp <- res$primary_projection
    headline_value <- pp$displayed_balance; headline_label <- "Projected KiwiSaver Balance"; subtext <- paste("At age", round(res$primary_inputs$withdrawal_age))
    if (ss$is_couple_mode && !is.null(res$partner_projection)) { headline_value <- headline_value + res$partner_projection$displayed_balance; headline_label <- "Joint KiwiSaver Balance"; subtext <- "At each partner's withdrawal age" }
    div(class = "summary-box accent-card", div(class = "summary-label", headline_label), div(class = "summary-value", fmt_money(headline_value, 0)), div(class = "muted-note", subtext))
  })
  output$breakdownUI <- renderUI({
    res <- scenario_result()
    if (!is.null(res$error)) return(NULL)
    rows <- build_balance_breakdown(res$primary_inputs, res$primary_projection)
    display_rows <- lapply(seq_len(nrow(rows)), function(i) {
      tags$tr(tags$td(rows$Component[i]), tags$td(fmt_money(rows$Nominal[i], 2)), tags$td(fmt_money(rows$Real[i], 2)))
    })
    div(class = "summary-box balance-card results-table", h4("Balance Breakdown"), tags$table(tags$thead(tags$tr(tags$th("Component"), tags$th("Nominal"), tags$th("Real"))), tags$tbody(display_rows)))
  })
  output$coupleBalancesUI <- renderUI({
    res <- scenario_result(); if (!is.null(res$error) || is.null(res$partner_projection)) return(NULL)
    div(class = "summary-box balance-card", h4("Individual KiwiSaver balances"), tags$ul(class = "compact-list", tags$li(tags$span("You"), tags$span(fmt_money(res$primary_projection$displayed_balance, 2))), tags$li(tags$span("You - home withdrawals"), tags$span(fmt_money(res$primary_projection$displayed_home_withdrawal, 2))), tags$li(tags$span("Partner"), tags$span(fmt_money(res$partner_projection$displayed_balance, 2))), tags$li(tags$span("Partner - home withdrawals"), tags$span(fmt_money(res$partner_projection$displayed_home_withdrawal, 2)))))
  })
  output$nzSuperUI <- renderUI({ res <- scenario_result(); if (!is.null(res$error)) return(NULL); div(class = "summary-box accent-card", div(class = "summary-label", "NZ Super (NPV at eligibility age)"), div(class = "summary-value", fmt_money(res$retirement_outputs$nz_super_value, 0)), div(class = "muted-note", res$retirement_outputs$nz_super_details)) })
  output$replacementRateUI <- renderUI({ res <- scenario_result(); if (!is.null(res$error)) return(NULL); rr <- res$retirement_outputs$replacement_rate; div(class = "summary-box", div(class = "summary-label", "Replacement rate"), div(class = "summary-value", if (is.finite(rr)) paste0(round(rr * 100, 1), "%") else "Not shown"), div(class = "muted-note", res$retirement_outputs$replacement_rate_note)) })
  output$incomeTable <- renderTable({
    res <- scenario_result(); if (!is.null(res$error)) return(NULL)
    df <- res$retirement_outputs$income_table
    data.frame(Age = df$Age, KiwiSaver = vapply(df$KiwiSaver, fmt_money, character(1), digits = 0), NZSuper = vapply(df$NZSuper, fmt_money, character(1), digits = 0), Total = vapply(df$Total, fmt_money, character(1), digits = 0), stringsAsFactors = FALSE)
  }, striped = FALSE, bordered = FALSE, spacing = "s", align = "lrrr")
}

# =============================================================================
# SECTION 7A: BASIC MODEL TESTS
# =============================================================================

run_calculator_tests <- function() {
  message("Running calculator tests...")
  assert_close <- function(actual, expected, tolerance = 1e-6, label = "value") {
    if (!is.finite(actual) || abs(actual - expected) > tolerance) stop(sprintf("%s failed: expected %s, got %s", label, expected, actual), call. = FALSE)
    TRUE
  }
  assert_true <- function(value, label = "condition") {
    if (!isTRUE(value)) stop(sprintf("%s failed", label), call. = FALSE)
    TRUE
  }
  assert_true(identical(get_model_start_date(as.Date("1991-01-03")), Sys.Date()), "Model start date should default to Sys.Date()")
  gross <- 100000
  assert_true(calculate_disposable_income_with_acc(gross) < calculate_net_income_with_decimal_brackets(gross), "Disposable income with ACC should be lower than income-tax-only net income")

  govt_test <- calc_kiwisaver(dob = as.Date("1961-01-01"), start_age = 64, withdrawal_age = 65, current_balance = 0, annual_income = 120000, member_rate = 4, employer_rate = 0, strategy = "balanced", wage_growth = 0, years_total_override = 1, fiscal_drag = FALSE, govt_rate = 25, govt_threshold = 999999, govt_cap = 260.72, breaks = list(), voluntary_contributions = list(), home_withdrawal_enabled = FALSE, home_withdrawal_age = NA_real_, model_start_date = as.Date("2025-01-01"))
  assert_close(govt_test$gTot, 521.44, tolerance = 0.05, label = "Government contribution across two partial government years")

  govt_threshold_test <- calc_kiwisaver(dob = as.Date("1961-01-01"), start_age = 64, withdrawal_age = 65, current_balance = 0, annual_income = 120000, member_rate = 4, employer_rate = 0, strategy = "balanced", wage_growth = 0, years_total_override = 1, fiscal_drag = FALSE, govt_rate = 25, govt_threshold = 1000, govt_cap = 260.72, breaks = list(), voluntary_contributions = list(), home_withdrawal_enabled = FALSE, home_withdrawal_age = NA_real_, model_start_date = as.Date("2025-01-01"))
  assert_close(govt_threshold_test$gTot, 0, tolerance = 0.01, label = "Government contribution should be zero above the income threshold")

  turns_65_mid_year_test <- calc_kiwisaver(
    dob = as.Date("1962-01-01"),
    start_age = 64.5,
    withdrawal_age = 66,
    current_balance = 0,
    annual_income = 50000,
    member_rate = 4,
    employer_rate = 0,
    strategy = "balanced",
    wage_growth = 0,
    years_total_override = NA_real_,
    fiscal_drag = FALSE,
    govt_rate = 25,
    govt_threshold = 999999,
    govt_cap = 260.72,
    breaks = list(),
    voluntary_contributions = list(),
    home_withdrawal_enabled = FALSE,
    home_withdrawal_age = NA_real_,
    model_start_date = as.Date("2026-07-01")
  )

  assert_close(
    turns_65_mid_year_test$gTot,
    251.88,
    tolerance = 0.05,
    label = "Government contribution should stop exactly on the 65th birthday"
  )

  already_65_test <- calc_kiwisaver(
    dob = as.Date("1961-07-01"),
    start_age = 65,
    withdrawal_age = 66,
    current_balance = 0,
    annual_income = 50000,
    member_rate = 4,
    employer_rate = 0,
    strategy = "balanced",
    wage_growth = 0,
    years_total_override = NA_real_,
    fiscal_drag = FALSE,
    govt_rate = 25,
    govt_threshold = 999999,
    govt_cap = 260.72,
    breaks = list(),
    voluntary_contributions = list(),
    home_withdrawal_enabled = FALSE,
    home_withdrawal_age = NA_real_,
    model_start_date = as.Date("2026-07-01")
  )

  assert_close(
    already_65_test$gTot,
    0,
    tolerance = 0.01,
    label = "Government contribution should be zero when already 65 at model start"
  )

  oneoff_start_test <- calc_kiwisaver(dob = as.Date("1986-01-01"), start_age = 40, withdrawal_age = 41, current_balance = 0, annual_income = 0, member_rate = 0, employer_rate = 0, strategy = "balanced", wage_growth = 0, years_total_override = 1, fiscal_drag = FALSE, govt_rate = 0, govt_threshold = 999999, govt_cap = 0, breaks = list(), voluntary_contributions = list(list(type = "oneoff", startAge = 40, endAge = NA_real_, amount = 1000, applied = FALSE)), home_withdrawal_enabled = FALSE, home_withdrawal_age = NA_real_, model_start_date = as.Date("2026-01-01"))
  assert_close(oneoff_start_test$vTot, 1000, tolerance = 0.01, label = "One-off voluntary contribution at model start age")

  home_start_test <- calc_kiwisaver(dob = as.Date("1986-01-01"), start_age = 40, withdrawal_age = 41, current_balance = 10000, annual_income = 0, member_rate = 0, employer_rate = 0, strategy = "balanced", wage_growth = 0, years_total_override = 1, fiscal_drag = FALSE, govt_rate = 0, govt_threshold = 999999, govt_cap = 0, breaks = list(), voluntary_contributions = list(), home_withdrawal_enabled = TRUE, home_withdrawal_age = 40, model_start_date = as.Date("2026-01-01"))
  assert_true(home_start_test$hTot > 0, "First-home withdrawal at model start age should be applied")

  ss <- list(display_real = TRUE, inflation_rate_decimal = 0.02, wage_growth = 0, fiscal_drag = FALSE, govt_rate = 25, govt_threshold = 999999, govt_cap = 260.72)
  person <- list(dob = as.Date("1961-01-01"), model_start_choice = "today", start_age = 64, withdrawal_age = 65, projection_years = 1, current_balance = 10000, annual_income = 80000, member_rate = 4, employer_rate = 4, strategy = "balanced", breaks = list(), voluntary_contributions = list(), home_withdrawal = list(enabled = FALSE, age = NA_real_))
  projection <- run_person_projection(person, ss)
  breakdown <- build_balance_breakdown(person, projection)
  assert_close(attr(breakdown, "nominal_total"), attr(breakdown, "nominal_balance"), tolerance = 0.01, label = "Nominal balance breakdown should reconcile")
  assert_close(attr(breakdown, "real_total"), attr(breakdown, "real_balance"), tolerance = 0.01, label = "Real balance breakdown should reconcile")

  bad_person <- person
  bad_person$voluntary_contributions <- list(list(type = "recurring", startAge = 64.2, endAge = NA_real_, amount = 1000, applied = FALSE))
  validation <- validate_person_inputs(bad_person, "Test person")
  assert_true(!isTRUE(validation$valid), "Recurring voluntary contribution with missing end age should fail validation")

  bad_break_person <- person
  bad_break_person$breaks <- list(list(startAge = 64.1, endAge = 64.5, pauseType = "reduce-member", reducedRate = 99, topUpEnabled = FALSE, topUpAmount = 0))
  validation_break <- validate_person_inputs(bad_break_person, "Test person")
  assert_true(!isTRUE(validation_break$valid), "Reduced contribution rate above normal member rate should fail validation")

  message("All calculator tests passed.")
  invisible(TRUE)
}

# =============================================================================
# SECTION 8: LAUNCH APP
# =============================================================================

shinyApp(ui, server)
