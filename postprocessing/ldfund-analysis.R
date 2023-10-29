# Analysis of GCAM results on loss and damage fund modeling
#
# Hassan Niazi, May 2023

library(tidyverse)
library(rgcam)
library(rmap)
library(ggpubr)

# load already queried project, if not then source("load-results.R")
ldfund <- loadProject(proj = 'ldfund_remote.dat')    # choose from local or remote
listScenarios(ldfund)     # or names(ldfund)
listQueries(ldfund)       # or names(ldfund[[1]])

# get queried data ----
df_co2e <- getQuery(ldfund, "CO2 emissions by region")
df_co2c <- getQuery(ldfund, "CO2 concentrations")
df_forcing <- getQuery(ldfund, "total climate forcing")
df_temp <- getQuery(ldfund, "global mean temperature")
df_gdp <- getQuery(ldfund, "GDP MER by region")
df_pcgdp <- getQuery(ldfund, "GDP per capita PPP by region")
df_pricesSector <- getQuery(ldfund, "prices by sector")
df_pricesMarkets <- getQuery(ldfund, "prices of all markets")
df_demandMarkets <- getQuery(ldfund, "demand of all markets")
df_inputsSector <- getQuery(ldfund, "inputs by sector")
df_outputsSector <- getQuery(ldfund, "outputs by sector")
#df_ <- getQuery(ldfund, "")

gcam_iso_region <- read_csv("gcam_iso_region.csv")
ldGDPchange_gcam <- read_csv("ldGDPchange_gcam.csv")
cntry_gdp_adjust_all <- read_csv("cntry_gdp_adjust_all.csv")
pools_gcam_region <- read_csv("pools_gcam_region.csv")

# plot format ----
g <- ggplot() + theme_bw() + theme(strip.background = element_blank(),
                                   strip.text = element_text(face = "bold"),
                                   legend.position = "bottom",
                                   plot.title = element_text(hjust = 0.5),
                                   panel.grid.major = element_blank())
colors <- c("green4","orange2", "yellow2", "darkred")
colors_5scen <- c("green4","orange2", "yellow2", "darkred", "black")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GDP ----
## raw from code ----


# gdp change by region
df_gdp_change_input <- ldGDPchange_gcam %>%
  tidyr::pivot_longer(cols = ends_with("C"), names_to = "temp_change", values_to = "gdp_change")

g +
  geom_point(data = df_gdp_change_input, aes(x = gdp_change, y = temp_change, colour = temp_change)) +
  facet_wrap(~region, ncol=8) +
  scale_color_manual(values = colors) +
  labs(x = "Percent Change in GDP due to Climate Damages", y = "Temperature Change", color = "")
# ggsave(filename = "figures/gdp_change_input.png", width = 16, height = 9, units = "in")

# gdp trajectories by region as an input to gcam from gcamdata
df_gdp_input <- cntry_gdp_adjust_all %>%
  group_by(scenario, temp_change, GCAM_region_ID, year) %>%
  summarise(gdp = sum(gdp)) %>%
  #filter(temp_change == "4C", scenario == "SSP5") %>%
  filter(scenario == "SSP2") %>%
  left_join(gcam_iso_region, by = c("GCAM_region_ID")) %>% ungroup()
#%>% tidyr::pivot_longer(cols = ends_with("C"), names_to = "temp_change", values_to = "gdp_change")

g +
  geom_line(data = df_gdp_input,
            aes(x = year, y = gdp, group = temp_change, colour = temp_change), size = 1) +
  #geom_point(aes(colour = temp_change)) +
  facet_wrap(~region, ncol=8, scales="free_y") +
  scale_color_manual(values = colors) +
  labs(x = "Year", y = "GDP Change due to Climate Damages", color = c(""))
# ggsave(filename = "figures/gdp_trajectories_gcamdata.png", width = 16, height = 9, units = "in")

## gcam results ----
g +
  geom_line(data = df_gdp %>% filter(scenario != "Reference"),
            aes(x = year, y = value, group = scenario, color = scenario), size = 1) +
  facet_wrap(~region, nrow = 4, ncol = 8, scales = "free_y") +
  xlim(1975, 2100) +
  # scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "GDP MER [Million 1990$]", color = "Scenarios")
# ggsave(filename = "figures/gdp_difference_scenarios.png", width = 16, height = 9, units = "in")

# global GDP stayed the same
df_gdp %>% select(!Units) %>% group_by(scenario, year) %>%
  summarise(global_gdp = sum(value)) %>%
  ungroup() -> globalgdp

g +
  geom_line(data = globalgdp,
            aes(x = year, y = global_gdp, color = scenario, linetype = scenario), size = 1.25) +
  xlim(1975, 2100) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Global GDP", x = "Year", y = "GDP MER [Million 1990$]" , color  = "", linetype = "", shape = ""
  )
# ggsave(filename = "figures/gdp_global_same.png", width = 7, height = 6, units = "in")

### GDP changes in 3CgSSP2 ----
df_gdp %>%
  pivot_wider(names_from = "scenario", values_from = "value") %>%
  mutate(gdp_change = (`3CgSSP2` - Reference)/Reference) -> gdpchange

# total gdp reallocated
sum((gdpchange)$`3CgSSP2`)
sum((gdpchange)$Reference)
# reallocated absolute gdp
sum((gdpchange %>% filter(gdp_change < 0))$`3CgSSP2`) - sum((gdpchange %>% filter(gdp_change < 0))$Reference)
# reallocated gddp as a percent of global
(sum((gdpchange %>% filter(gdp_change < 0))$`3CgSSP2`) - sum((gdpchange %>% filter(gdp_change < 0))$Reference)) * 100 / sum((gdpchange)$Reference)

# gdp changes in senders and receivers in 3CgSSP2 scenario
g +
  geom_line(data = gdpchange,
            aes(x = year, y = gdp_change,
                colour = after_stat(if_else(y > 0, "Receivers", if_else(y < 0, "Senders", "Unchanged")))),
            size = 1.25) +
  facet_wrap(~region, nrow = 4, ncol = 8, scales = "free_y") +
  xlim(2020, 2100) +
  scale_color_manual(values = c("green4", "red3", "black")) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "GDP MER Change [%]", color = c("")) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/gdp_changes_senderVSreceiver_3C.png", width = 16, height = 9, units = "in")

# only changed GDPs
g +
  geom_line(data = gdpchange %>% filter(gdp_change != 0),
            aes(x = year, y = gdp_change), size = 1.25) +
  facet_wrap(~region, nrow = 8, ncol = 4, scales = "free_y") +
  scale_y_continuous(labels = scales::percent)

### GDP changes in all scenarios ----
df_gdp %>%
  left_join(df_gdp %>%
              filter(scenario == "Reference") %>%
              select(region, year, value_ref = value), by = c("region", "year")) %>%
  mutate(gdp_change = (value - value_ref) / value_ref) %>%
  select(!Units) %>%
  arrange(gdp_change) %>%
  mutate(pool = if_else(gdp_change > 0, "Receivers", if_else(gdp_change < 0, "Senders", "Unchanged"))) -> gdp_change

# one time run to create the pools file, pools might change in different sender vs reciever scenarios
# gdp_change %>% filter(scenario != "Reference", year > 2015) %>%
#   select(region, pool) %>%  distinct() %>%
#   left_join(gcam_iso_region %>% select(region, GCAM_region_ID) %>% distinct()) -> pools_gcam_region
# write_csv(pools_gcam_region, "pools_gcam_region.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Forcing ----

# global forcign stayed the same
# (probably wrong as forcing should be consistent with the damages sceanrios)
g +
  geom_line(data = df_forcing, aes(x = year, y = value, color = scenario), size = 1.2) +
  facet_wrap(~scenario, ncol = 5) +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "Radiative Forcing [W/m2]", color = "")
# ggsave(filename = "figures/forcing.png", width = 16, height = 5, units = "in")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CO2 Emissions ----
df_co2e %>% filter(Units == "MTC") %>%
  mutate(value = value *(44/12)) %>%
  left_join(pools_gcam_region, by = "region") -> df_CO2 # convert carbon to CO2

## regional emissions ----
# emission trajectories, not useful so plotting changes
g +
  geom_line(data = df_CO2, aes(x = year, y = value, color = scenario)) +
  facet_wrap(~region, ncol = 8) +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "Carbon dioxide Emissions [Million metric tonnes of CO2]", color = "") # Mt CO2, mega metric tonnes of CO2

# CO2 change
df_CO2 %>%
  left_join(df_CO2 %>%
              filter(scenario == "Reference") %>%
              select(region, year, value_ref = value),
            by = c("region", "year")) %>%
  mutate(co2_change = (value - value_ref) * 100 / value_ref) %>%
  select(!Units) %>%
  filter(year >= 2020) %>%     # drop unchanged years
  arrange(desc(co2_change)) -> co2_change

# regional changes in emissions across scenarios
g +
  # geom_line(data = co2_change %>% left_join(pools_gcam_region, by = "region"),
  #           aes(x = year, y = co2_change/100, color = pool, linetype = scenario), size = 1.1) +
  geom_point(data = co2_change,
            aes(x = year, y = co2_change/100, color = pool, shape = scenario), size = 1.1) +
  facet_wrap(~region, ncol = 8) + #, scales="free_y"
  scale_color_manual(values = c("red4", "green4", "black")) +
  labs(x = "Year", y = "CO2 Emissions Change relative to Reference [%]", color = "", shape = "") +  # Mt CO2, mega metric tonnes of CO2
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/emissions_regional_change_scenarios.png", width = 16, height = 9, units = "in")


# map of emissions changes: one scenario, 2015 vs 2100 (2015 is the same as ref)
df_CO2 %>% filter(scenario == "3CgSSP2", year %in% c("2015", "2020", "2050", "2070", "2100")) %>% select(!Units) -> df_CO2_3C_rmap
co2_change %>% filter(scenario == "3CgSSP2", year %in% c("2020", "2100")) -> df_CO2_3C_change_rmap

rmap_co2_data = data.frame(subRegion = df_CO2_3C_rmap$region, value = df_CO2_3C_rmap$value, year = df_CO2_3C_rmap$year)
rmap_co2_change_data = data.frame(subRegion = df_CO2_3C_change_rmap$region, value = df_CO2_3C_change_rmap$co2_change, year = df_CO2_3C_change_rmap$year)


mapE <- rmap::map(data = rmap_co2_data,
                  underLayer = rmap::mapGCAMReg32,
                  legendFixedBreaks = c(0, 500, 2000, 3500, 13500), # use palette with breaks
                  # palette = "OrRd",
                  palette = c("gray", "green4", "gold", "orange2", "darkred"),
                  # palette = c('gray', "forestgreen", "darkolivegreen3",'gold1', 'darkred'),
                  # folder = "figures/rmaps/CO2_3C",
                  save = F, show = F)

ggarrange(
mapE[[1]] + ggplot2::theme(legend.position = "bottom") # 2015
,
mapE[[5]] + ggplot2::theme(legend.position = "bottom") # 2100
, common.legend = T, legend = "bottom", ncol = 1)
# ggsave(filename = "figures/emissions_map_2015_2100.png", width = 9, height = 8.25, units = "in")

mapC <- rmap::map(data = rmap_co2_change_data,
                  underLayer = rmap::mapGCAMReg32,
                  legendFixedBreaks = c(-5, -2, 0, 0.1, 2, 15), # use palette with breaks
                  # palette = c("green4", "darkolivegreen3", "gold", "orange2", "darkred"),
                  # folder = "figures/rmaps/CO2_3C_change",
                  save = F, show = F)
mapC[[2]] + ggplot2::theme(legend.position = "bottom")
# ggsave(filename = "figures/emissions_map_2100_3C.jpg", width = 9, height = 5, units = "in")


## global CO2 emissions ----

# global emissions stayed the same
df_CO2 %>% select(!Units) %>% group_by(scenario, year) %>%
  summarise(global_emissions = sum(value)) %>%
  ungroup() -> global_emissions

g +
  geom_line(data = global_emissions,
            aes(x = year, y = global_emissions, group = scenario, color = scenario, linetype = scenario), size = 1.25) +
  xlim(1975, 2100) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Global CO2 Emissions", x = "Year", y = "Global CO2 Emissions [Mt CO2]" , color  = "", linetype = "", shape = ""
  )
# ggsave(filename = "figures/emissions_global_same.png", width = 7, height = 6, units = "in")

# global emissions by pools
df_CO2 %>% select(!Units) %>%
  group_by(scenario, pool, year) %>%
  summarise(global_emissions_bypool = sum(value)) %>%
  ungroup() -> global_emissions_bypool

g +
  geom_line(data = global_emissions_bypool,
            aes(x = year, y = global_emissions_bypool, color = scenario, linetype = scenario),
            size = 1.25) +
  xlim(1975, 2100) +
  facet_wrap(~pool) +
  # scale_color_manual(values = colors_5scen) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Global CO2 Emissions [Mt CO2]" , color  = "", linetype = "", shape = "")
# ggsave(filename = "figures/emissions_global_same_pools.png", width = 12, height = 5, units = "in")

# changes in CO2 emissions
co2_change %>%
  group_by(scenario, year) %>%
  summarise(global = sum(value),
            global_ref = sum(value_ref)) %>%
  mutate(global_co2change = (global - global_ref) * 100 / global_ref) %>%
  arrange(desc(global_co2change)) -> global_co2_change

# global emissions stayed the same,
ggarrange(
# always less than 0.6% relative to reference
g +
  geom_point(data = global_co2_change, aes(x = 1:nrow(global_co2_change), y = global_co2change/100, color = scenario)) +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Index (number of points)", y = "Global CO2 Emissions Change relative to Reference [%]", color = "") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("a") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03))
,
# across all scenarios
g +
  geom_line(data = global_co2_change, aes(x = year, y = global_co2change/100, color = scenario), size = 1) +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "Global CO2 Emissions Change relative to Reference [%]", color = "") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("b") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03))
,
common.legend = T, legend = "bottom", label.y = "Global CO2 Emissions Change relative to Reference [%]"
)
# ggsave(filename = "figures/emissions_global_change.jpg", width = 10, height = 5, units = "in")


# gdp change trajectories over time across scenarios
g +
  geom_line(data = gdp_change,
            aes(x = year, y = gdp_change, colour = scenario),
            size = 1.25) +
  facet_wrap(~region, nrow = 4, ncol = 8, scales = "free_y") +
  xlim(2020, 2100) +
  # scale_color_manual(values = c("green4", "red3", "black")) +
  scale_color_manual(values = colors_5scen) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "GDP MER Change [%]", color = c(""), linetype = c("")) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/gdp_change_scenarios.png", width = 16, height = 9, units = "in")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prices by Sector ----
df_pricesSector %>%
  left_join(df_pricesSector %>%
              filter(scenario == "Reference") %>%
              select(region, sector, year, value_ref = value),
            by = c("region", "sector", "year")) %>%
  mutate(prices_bysector_change = (value - value_ref) * 100 / value_ref) %>%
  select(!Units) %>%
  filter(year >= 2020) %>%     # drop unchanged years
  arrange(desc(prices_bysector_change)) %>%
  left_join(pools_gcam_region, by = "region") -> prices_bysector_change

# check, should be zero: sum((prices_bysector_change %>% filter(scenario == "Reference"))$prices_bysector_change, na.rm = T)


## global top moving sectors ----

# top moving sectors irrespective of time, region, or scenario
rbind(prices_bysector_change %>% top_n(5, prices_bysector_change),
      prices_bysector_change %>% top_n(-5, prices_bysector_change))

# top moving sectors in 2100 irrespective region or scenario
rbind(prices_bysector_change %>% filter(year == 2100) %>% top_n(5, prices_bysector_change),
      prices_bysector_change %>% filter(year == 2100) %>% top_n(-5, prices_bysector_change)) -> top_moving_glob_2100
# write_csv(top_moving_glob_2100, "top_moving_sectors_glob_2100.csv")

# top moving sectors based on averages changes over the 21st century
prices_bysector_change %>%
  group_by(scenario, region, sector, pool) %>%
  summarize(avg_value = mean(value),
            avg_value_ref = mean(value_ref)) %>%
  mutate(avg_prices_bysector_change = (avg_value - avg_value_ref) * 100 / avg_value_ref) %>%
  ungroup() -> prices_bysector_change_avg

rbind(prices_bysector_change_avg %>% top_n(5, avg_prices_bysector_change),
      prices_bysector_change_avg %>% top_n(-5, avg_prices_bysector_change)) -> top_moving_glob
# write_csv(top_moving_glob, "top_moving_sectors_glob.csv")

top_moving_glob %>%
  left_join(prices_bysector_change, by = c("scenario", "region", "sector", "pool")) -> top_moving_glob_traj

# key moving sectors
g +
  geom_line(data = top_moving_glob_traj,
            aes(x = year, y = prices_bysector_change/100, linetype = scenario,
                colour = after_stat(if_else(y > 0, "Increase in Prices", if_else(y < 0, "Decrease in Prices", "Unchanged")))),
            size = 1) +
  facet_wrap(~sector, nrow = 2, ncol = 3, scales = "free_y") +
  xlim(2020, 2100) +
  scale_color_manual(values = c("green4", "red3", "black")) +
  # scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c('dotted','solid')) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Price Changes relative to Reference Scenario [%]", color = c(""), linetype = c("")) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/top_moving_sectors_glob.png", width = 10, height = 6, units = "in")

## regional top moving sectors ----
prices_bysector_change %>%
  filter(region == "Africa_Western", year == 2100, scenario == "3CgSSP2") -> reg_prices_sectors
# write_csv(reg_sectorprices, "prices_bysector_change_AW_2100_3C.csv")

rbind(reg_prices_sectors %>% top_n(5, prices_bysector_change),
      reg_prices_sectors %>% top_n(-5, prices_bysector_change)) -> top_moving_reg

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prices of all markets ----

# split region and market names
gcamRegions <- c("global", pools_gcam_region$region)
df_pricesMarkets_split <- df_pricesMarkets %>% mutate(region = "GetLost") %>% rename(reg_market = market)

for (reg in gcamRegions) {
  df_pricesMarkets_split %>%
    mutate(region = if_else(grepl(reg, reg_market), reg, region)) -> df_pricesMarkets_split
}

df_pricesMarkets_split %>%
  filter(region == "GetLost") %>%
  separate(reg_market, c("region", "market"), sep = "_") -> df_pricesBasins_split

df_pricesMarkets_split %>%
  filter(region != "GetLost") %>%
  mutate(market = substr(reg_market, start = nchar(region) + 1, stop = nchar(reg_market))) %>%
  select(!reg_market) -> df_pricesMarkets_split

df_pricesMarkets_split %>% rbind(df_pricesBasins_split) -> df_pricesMarkets_split
# checks: unique(df_pricesMarkets_split$region) unique(df_pricesMarkets_split$market)

# price changes relative to reference
df_pricesMarkets_split %>%
  filter(year >= 2020) %>%     # drop unchanged years
  select(!Units) %>%
  left_join(df_pricesMarkets_split %>%
              filter(scenario == "Reference") %>%
              select(region, market, year, value_ref = value),
            by = c("region", "market", "year")) %>%
  mutate(prices_markets_change = (value - value_ref) * 100 / value_ref) %>%
  arrange(desc(prices_markets_change))  %>%
  left_join(pools_gcam_region, by = "region") %>%
  mutate(pool = if_else(is.na(pool), "Basin", pool)) -> prices_markets_change

# unique(prices_markets_change$pool)

## top moving markets based on averages changes over the 21st century ----
prices_markets_change %>%
  group_by(scenario, region, market, pool) %>%
  summarize(avg_value = mean(value),
            avg_value_ref = mean(value_ref)) %>%
  mutate(avg_prices_market_change = (avg_value - avg_value_ref) * 100 / avg_value_ref) %>%
  ungroup() %>% drop_na() -> prices_markets_change_avg

rbind(prices_markets_change_avg %>% top_n(5, avg_prices_market_change),
      prices_markets_change_avg %>% top_n(-5, avg_prices_market_change)) -> top_moving_markets_glob
# write_csv(top_moving_markets_glob, "top_moving_markets_glob.csv")

top_moving_markets_glob %>%
  left_join(prices_markets_change, by = c("scenario", "region", "market", "pool")) -> top_moving_market_glob_traj

# key moving sectors
g +
  geom_line(data = top_moving_market_glob_traj,
            aes(x = year, y = prices_markets_change/100, linetype = scenario,
                colour = after_stat(if_else(y > 0, "Increase in Prices", if_else(y < 0, "Decrease in Prices", "Unchanged")))),
            size = 1) +
  facet_wrap(~market, nrow = 2, ncol = 3, scales = "free_y") +
  xlim(2020, 2100) +
  scale_color_manual(values = c("green4", "red3", "black")) +
  # scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c('dotted','solid')) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Price Changes relative to Reference [%]", color = c(""), linetype = c("")) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/top_moving_markets_glob.png", width = 10, height = 6, units = "in")

## water prices ----
prices_markets_change %>%
  filter(grepl("water_td_irr_", market)) %>%
  group_by(scenario, region, year, pool) %>%
  summarize(avg_value = mean(value),
            avg_value_ref = mean(value_ref)) %>%
  mutate(avg_prices_markets_change_water = (avg_value - avg_value_ref) * 100 / avg_value_ref) %>%
  ungroup() -> avg_prices_markets_change_water

g +
  geom_smooth(data = avg_prices_markets_change_water %>% filter(scenario == "3CgSSP2"),
            aes(x = year, y = avg_prices_markets_change_water/100,
                colour = after_stat(if_else(y > 0, "Increase in Prices", if_else(y < 0, "Decrease in Prices", "Unchanged")))),
            size = 1.25, se = F) +
  facet_wrap(~regions, nrow = 4, ncol = 8, scales = "free_y") +
  # facet_wrap(~pool, nrow = 4, ncol = 8, scales = "free_y") +
  scale_color_manual(values = c("green4", "red3", "black")) +
  # scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "Water Price Changes relative to Reference [%]", color = c(""), linetype = c("")) +
  theme(legend.position = "bottom", plot.title.position = "plot") +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/water_prices_R.png", width = 16, height = 9, units = "in")


## food prices ----
# pick "interesting" food markets using average changes in prices
prices_markets_change_avg %>% filter(scenario == "4CgSSP2", region == "Africa_Western")

prices_markets_change %>%
  filter(market %in% c("NutsSeeds", "Pasture", "biomass", "Beef", "Legumes", "OilCrop", "Forest", "Dairy")) -> prices_markets_change_ag

# average global Ag prices
prices_markets_change_ag %>%
  group_by(scenario, market, pool, year) %>%
  summarize(avg_value = mean(value),
            avg_value_ref = mean(value_ref)) %>%
  mutate(avg_prices_markets_change_ag = (avg_value - avg_value_ref) * 100 / avg_value_ref) %>%
  ungroup() -> avg_prices_markets_change_ag

# global Ag prices by pool
g +
  geom_line(data = avg_prices_markets_change_ag,
              aes(x = year, y = avg_prices_markets_change_ag/100, color = scenario), size = 1) +
  facet_grid(market~pool, scales = "free") +
  labs(x = "Year", y = "Change in Ag-Land-Food Prices relative to Reference [%]", color = c(""), linetype = c("")) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = colors_5scen)
# ggsave(filename = "figures/ag_prices_change_pools.png", width = 8, height = 10, units = "in")

## energy prices ----
prices_markets_change %>%
  filter(market %in% c("crude oil", "natural gas", "coal")) -> prices_markets_change_en

# average global energy prices
prices_markets_change_en %>%
  group_by(scenario, market, pool, year) %>%
  summarize(avg_value = mean(value),
            avg_value_ref = mean(value_ref)) %>%
  mutate(avg_prices_markets_change_en = (avg_value - avg_value_ref) * 100 / avg_value_ref) %>%
  ungroup() -> avg_prices_markets_change_en

# global energy prices by pool
g +
  geom_smooth(data = avg_prices_markets_change_en,
            aes(x = year, y = avg_prices_markets_change_en/100, color = scenario), size = 1, se = F) +
  facet_grid(market~pool, scales = "free") +
  labs(x = "Year", y = "Change in Energy Prices relative to Reference [%]", color = c(""), linetype = c("")) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = colors_5scen)
# ggsave(filename = "figures/energy_prices_change_pools.png", width = 8, height = 6.5, units = "in")

# energy prices all scenarios, all regions
g +
  geom_smooth(data = prices_markets_change_en,
            aes(x = year, y = prices_markets_change/100, linetype = market, color = scenario), size = 1, se = FALSE) +
  facet_wrap(~region, ncol = 8, scales = "free_y") +
  scale_color_manual(values = colors_5scen) +
  labs(x = "Year", y = "Change in Energy Prices relative to Reference [%]", color = c(""), linetype = c("")) +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/energy_prices_change_allscenario.png", width = 16, height = 9, units = "in")

# energy prices one scenario, all regions
g +
  geom_smooth(data = prices_markets_change_en %>% filter(scenario == "3CgSSP2"),
              aes(x = year, y = prices_markets_change/100, color = market), size = 1.2, se = FALSE) +
  facet_wrap(~region, ncol = 8, scales = "free_y") +
  # scale_color_manual(values = colors_5scen) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Change in Energy Prices relative to Reference [%]", color = c(""), linetype = c("")) +
  scale_y_continuous(labels = scales::percent)
# ggsave(filename = "figures/energy_prices_change_3C_R.png", width = 16, height = 9, units = "in")










# prices in a scenario, year, and a region
reg_ch <- c("Africa_Western", "USA")
prices_markets_change %>%
  filter(year == 2100, scenario == "3CgSSP2", region %in% reg_ch) %>%
  drop_na() -> reg_prices_markets



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Inputs by sector ----


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Outputs by sector ----
