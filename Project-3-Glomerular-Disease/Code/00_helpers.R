survfitms_to_survfit <- function (fit, event = "Progression") {
  event.state <- which(fit$states == event)
  fit$surv <-
    1 - fit$pstate[, event.state]
  fit$std.err <-
    fit$std.err[, event.state]
  lower <- fit$lower
  lower[is.na(lower)] <- 0
  upper <- fit$upper
  upper[is.na(upper)] <- 0
  fit$lower <-
    1 - upper[, event.state]
  fit$upper <-
    1 - lower[, event.state]
  fit$n.risk <-
    fit$n.risk[, fit$states == "(s0)"]
  fit$n.event <-
    fit$n.event[, event.state]
  fit$cumhaz <-
    fit$cumhaz[, paste(1, event.state, sep = ".")]
  fit$type <- "right"
  fit$pstate <- NULL
  fit$p0 <- NULL
  fit$sp0 <- NULL
  fit$transitions <- NULL
  fit$states <- NULL
  class(fit) <- "survfit"
  fit
}

glmer_pool_and_estimate <- function (obj) {
  obj %>%
    pool() %>%
    summary() %>%
    mutate(p.value = 2 * pnorm(abs(statistic), lower = FALSE),
           ci.lower = estimate - std.error * qnorm(0.975),
           ci.upper = estimate + std.error * qnorm(0.975)) %>%
    mutate_at(vars(estimate, ci.lower, ci.upper), exp) %>%
    filter(!term %in% "(Intercept)")
}

coxme_pool_and_estimate <- function (obj) {
  obj %>%
    pool() %>%
    summary() %>%
    mutate(p.value = 2 * pnorm(abs(statistic), lower = FALSE),
           ci.lower = estimate - std.error * qnorm(0.975),
           ci.upper = estimate + std.error * qnorm(0.975)) %>%
    mutate_at(vars(estimate, ci.lower, ci.upper), exp)
}

format_pval <- function (p, eps = 1e-16) {
  case_when(is.na(p) ~ "",
            round(p, 2) >= 0.1 ~ sprintf("%.2f", p),
            p >= eps ~ sprintf("%.1g", p),
            TRUE ~ sprintf("<%.1g", eps))
}
