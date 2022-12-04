##
##  Construct VAR a la Bernanke Blinder
##  Time Period: 1959 - 1979
##
##  Matthew DeHaven
##  2022 11 14
##
require(data.table)
require(lubridate)
require(stringr)
require(vars)
require(svars)
require(ggplot2)

## Read in data
data <- readRDS("./data/uiff_data.RDS")
data <- data[,.(date, series_id, value)]
data[, date := ymd(date)]

## Convert to wide
data <- data |> dcast(date ~ series_id)

## Transform
data[, pi := log(pi)]


## Trim to time period
data <- data[date %between% ymd(c("1959-07-01", "1979-09-30"))]


## Ordering (for Choleskey Decomposition)
order <- c("date", "sff", "ff", "u", "pi")
data <- data[,..order]
  
## Test BIC
vars::VARselect(data[,.(ff,u,pi)])
## The paper uses 6, but the BIC (SC) here says we should use 2

## Set up VAR
myvar <- vars::VAR(data[,.(ff,u,pi)], p = 6, type = "none")

## Choleskey ID
mysvar <-svars::id.chol(myvar)

## Impulse response
irf_vals <- irf(mysvar)$irf |> data.table()
irf_vals <- irf_vals |> melt(id = "V1")
names(irf_vals) <- c("t", "variable", "value")
irf_vals[, shock_var := str_extract(variable, "(?<=\\[ )\\w*(?= \\])")]
irf_vals[, response_var := str_extract(variable, "(?<= )\\w*$")]

## Shock sizes
u_init_shock <- irf_vals[t==0&shock_var=="u"&response_var=="u", value]*100 ## In Basis Points
pi_init_shock <- irf_vals[t==0&shock_var=="pi"&response_var=="pi", value]
pi_init_shock <- ((1 + pi_init_shock)^12-1)*100*100 ## Annualized rate, in Basis Points

## Chart the IRF
irf_chart <- 
irf_vals[shock_var != "ff" & response_var == "ff"] |>
  ggplot(aes(
    x = t,
    y = value,
    color = shock_var
  )) +
  geom_hline(yintercept = 0) + 
  geom_line() +
  scale_y_continuous(limits = c(-0.31, 0.5), expand = c(0,0), breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) +
  labs(
    title = "Response of Funds Rate to \nU and I Shocks",
    x = "Horizon (Months)",
    y = "Percentage Points",
    caption = paste0(
      "Initial Shocks:  ", round(u_init_shock, 0), " bps (u), ", round(pi_init_shock, 0), " bps (pi)."
    ) 
  ) +
  theme_bw() +
  theme(legend.position = c(0.8,0.5), legend.key = element_blank(), legend.background = element_blank(), legend.title = element_blank())

ggsave("./output/charts/BernankeBlinderVARchart.png", irf_chart, height = 4, width = 3, units = "in")

