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
