# THIS SCRIPT ESTIMATES COST SHARES FOR PA DISASTERS
# ENSURE THAT pa_adjusted FROM THE pa_data.R FILE IS LOADED IN THE WORKSPACE
# ESTIMATES IMPACTS FROM SETTING PA COST SHARE AT 75 PERCENT FOR ALL DISASTERS

library(dplyr)
library(writexl)

# Select relevant columns and adjust for inflation
pa_cost_share <- pa_adjusted |>
  select(-federal_share, -total_amount, -mitigationAmt, -adj_T_amount, -adj_mitigation) |>
  mutate(federal_share = federalShareObligated * deflator) |>
  mutate(recipient_amount = adj_amount - federal_share)

# Summarize to disaster level and create cost share columns
pa_cost_share <- pa_cost_share |>
  group_by(disasterNumber, year, state) |>
  summarize(total_funding = sum(adj_amount), total_federal = sum(federal_share),
            total_recipient = sum(recipient_amount)) |>
  mutate(cost_share = round(total_federal / total_funding, 2)) |>
  mutate(cost_share = ifelse(cost_share < 0.75, 0.75, cost_share)) |>
  mutate(proposed_federal = total_federal * 0.75, proposed_recipient = total_funding * .25) |>
  mutate(cs_difference = proposed_recipient - total_recipient)

# Normalize results for 75 percent cost share
pa_cost_share$cs_difference <- ifelse(pa_cost_share$cost_share == 0.75, 0, pa_cost_share$cs_difference)

# Estimate changes to cost share amounts and summarize additional recipient cost
cost_share_summary <- pa_cost_share |>
  filter(cost_share > 0.75) |>
  group_by(year) |>
  summarize(count = n(), recipient_cost = sum(cs_difference))

# Export results to Excel
write_xlsx(list('Cost Share Summary' = cost_share_summary, 'Cost Share by Disaster' = pa_cost_share), 'PA_Cost_Share.xlsx')