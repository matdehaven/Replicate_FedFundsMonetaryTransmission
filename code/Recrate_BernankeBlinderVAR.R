##
##  Construct VAR a la Bernanke Blinder
##  Time Period: 
##
##  Matthew DeHaven
##  2022 11 14
##
require(data.table)
require(lubridate)
require(stringr)
require(vars)
require(svars)

## Read in data
data <- readRDS("./data/uiff_data.RDS")
data[, date := ymd(date)]

## Convert to wide
data <- data |> dcast(date ~ series_id)

## Trim to time period
data <- data[date %between% ymd(c("1959-07-01", "1979-09-30"))]

## Transform
data[, i := log(i)]

## Ordering (for Choleskey Decomposition)
order <- c("date", "ff", "u", "i")
data <- data[,..order]
  
## Test BIC
vars::VARselect(data[,-"date"])
## The paper uses 6, but the BIC (SC) here says we should use 2

## Set up VAR
myvar <- vars::VAR(data[,-"date"], p = 6, type = "none",)

## Choleskey ID
mysvar <-svars::id.chol(myvar)

## Impulse response
irf_vals <- irf(mysvar)$irf |> data.table()
irf_vals <- irf_vals |> melt(id = "V1")
names(irf_vals) <- c("t", "variable", "value")
irf_vals[, shock_var := str_extract(variable, "(?<=\\[ )\\w*(?= \\])")]
irf_vals[, response_var := str_extract(variable, "(?<= )\\w*$")]

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
  theme_bw() +
  theme(legend.position = c(0.8,0.5), legend.background = element_blank(), legend.title = element_blank())

ggsave("./output/charts/BernankeBlinderVARchart.png", irf_chart, height = 4, width = 3, units = "in")

