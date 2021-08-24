#' This code holds plotting and analysis scripts for Kikstra et al. (2021), Nat. Energy, "Climate mitigation scenarios with persistent COVID-19 related energy demand changes" 
#' Author: Jarmo S. Kikstra 
#' 
#' Instructions: 
#' - to reproduce, please run parts 0 to 2 first, and then select and run the specific figure you would like to create  
#' 
#' Index:
#' 0. Loading packages.
#' 1. Locating and reading all data used for analysis.
#' 2. Define auxiliary functions. 
#' 3. Main text figures.
#' 4. Supplementary figures.
#' 
#' 


# Part 0: loading packages ====
# uncomment lines below in case the packages are not yet installed.
# pkg.list <- c("vroom",
#               "readxl",
#               "ggsci",
#               "hrbrthemes",
#               "ggrepel",
#               "RColorBrewer",
#               "ggpubr",
#               "gridExtra",
#               "colorspace",
#               "patchwork",
#               "tidyverse",
#               "plotly",
#               "maps",
#               "rworldmap",
#               "mapproj",
#               "rgdal",
#               "here",
#               "tidyverse")
# install.packages(pkg.list)
# load packages
library(vroom) # load CSV files, fast, as tibble.
library(readxl) # load Excel files
library(ggsci) # scientific colour schemes
library(hrbrthemes) # more themes
library(ggrepel) # for non-overlapping labels
library(RColorBrewer) # for colours
library(ggpubr) # for ggarrange
library(gridExtra) # for ggarrange
library(colorspace) # hsv colorspace manipulations
library(patchwork) # for arrangement with much easier syntax than ggarrange 
library(plotly) # for interactive plots
library(maps) # for maps, only supplementary figure
library(rworldmap) # for maps, only supplementary figure
library(mapproj) # for maps, only supplementary figure
library(rgdal) # for maps, only supplementary figure 
library(here) # for easily specifying relative paths
library(tidyverse) # for all data manipulation, plotting, etc.



# Part 1: locating and reading data (COVID scenarios, GDP sensitivities, SR15 data, and some auxiliary files) ====

# Part 1.0: Set some global parameters used throughout ====
year.start <- 2015 # start year for timeseries figure
year.end <- 2035 # start year for timeseries figure

# Part 1.1: Setting paths ====

# # set working directory to this file
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
# base path 
here::i_am("README.md")
base.path <- paste0(here(), "/")
out.path <- paste0(base.path, "figures/")

# Part 1.1.1: Setting paths: inputs ====
# main data file
data.file <- "covid-scenarios-data.xlsx"
# sheets
# main scenario data from this study, output of MESSAGEix-GLOBIOM
f.covid <- "scenario_data"
# Collated activity assumptions data
f.activity <- "2025vs2019"
# Extra data with estimated upstream CO2 emissions of demand end-use sectors
f.upstream <- "upstream_data"
# Colour settings by scenario, for plotting
f.colormarkers <- "color_markers"
# Regional definitions for MESSAGEix
f.MESSAGEregion <- "iso_MESSAGEix"
# Aggregate structure and activity changes related to energy demand changes
f.aggregate.energy.demand <- "demand_change"

# SR15 database data files
# SR15 data on a global level, with climate categories
f.sr15 <- paste0(base.path,"sr15.csv") 
# SR15 data on a five-regional level, wide format
f.sr15.r5 <- paste0(base.path,"iamc15_scenario_data_all_regions_r2.0.xlsx") 


# Part 1.2: Load COVID scenario base data ====
df_full <- read_excel(paste0(base.path,data.file), sheet=f.covid)
df <- pivot_longer(df_full,`2000`:`2100`, names_to = "year") %>% na.omit()
df.cov <- df %>% rename(model=Model, scenario=Scenario, unit=Unit, region=Region, variable=Variable) %>% select(-unit)

# assign colours and categories
color_markers = read_excel(paste0(base.path,data.file), sheet=f.colormarkers)
# Function for desaturating colors by specified proportion
desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}
# adapt saturation differences for colorblind-safeness
color_markers_n <- color_markers %>%
  mutate(color=ifelse(scenario=="baseline",desat(color,1),color)) %>%
  mutate(color=ifelse(scenario=="restore",desat(color,1),color)) %>%
  mutate(color=ifelse(scenario=="self reliance",desat(color,0.6),color)) %>%
  mutate(color=ifelse(scenario=="smart use",desat(color,0.3),color)) %>%
  mutate(color=ifelse(scenario=="green push",desat(color,1),color))
# apply to data
df.cov <- df.cov %>% mutate(category=ifelse(
  grepl("1000", scenario, fixed = FALSE),"Below 2.0C",
  ifelse(grepl("550", scenario, fixed = FALSE)|grepl("(Same climate policy 1.5)", scenario, fixed = FALSE),"1.5C no or low OS",
         "Above 2.0C")
)) %>% mutate(color = ifelse(
  grepl("Baseline-no-COVID", scenario, fixed = FALSE),(color_markers %>% filter(scenario=="baseline"))$color,
  ifelse(
    grepl("Smart Use", scenario, fixed = FALSE),(color_markers %>% filter(scenario=="smart use"))$color,
    ifelse(
      grepl("Green Push", scenario, fixed = FALSE),(color_markers %>% filter(scenario=="green push"))$color,
      ifelse(
        grepl("Self-Reliance", scenario, fixed = FALSE),(color_markers %>% filter(scenario=="self reliance"))$color,
        ifelse(
          grepl("Restore", scenario, fixed = FALSE),(color_markers %>% filter(scenario=="restore"))$color, "black"
        ))))))
# separate colormarkers for handling of colours outside of dataframe
color_markers_n <- color_markers_n %>%
  mutate_if(is.character, str_replace_all, pattern="baseline", replacement="Baseline-no-COVID") %>%
  mutate_if(is.character, str_replace_all, pattern="smart use", replacement="Smart Use") %>%
  mutate_if(is.character, str_replace_all, pattern="green push", replacement="Green Push") %>%
  mutate_if(is.character, str_replace_all, pattern="self reliance", replacement="Self-Reliance") %>%
  mutate_if(is.character, str_replace_all, pattern="restore", replacement="Restore")
color_markers_n.550 <- color_markers_n %>% mutate(scenario=paste0(scenario," (550)"))
color_markers_n.samepolicy <- color_markers_n %>% mutate(scenario=paste0(scenario," (Same climate policy 1.5)"))
color_markers_n.1000 <- color_markers_n %>% mutate(scenario=paste0(scenario," (1000)"))
color_markers_n <- color_markers_n %>%
  bind_rows(color_markers_n.550) %>%
  bind_rows(color_markers_n.samepolicy) %>%
  bind_rows(color_markers_n.1000)
mark_cols = color_markers_n$color
# set names, for use in ggplot manual colors
names(mark_cols) = color_markers_n$scenario

# split out GDP sensitivity runs
df.gdp <- df.cov %>% filter(grepl("sens_med.SHK_R", scenario, fixed = FALSE)) # select GDP sensitivity scenarios
df.cov <- df.cov %>% filter(!grepl("sens_med.SHK_R", scenario, fixed = FALSE)) # deselect GDP sensitivity scenarios

# create smaller dataframes with only the World region
df.cov.w <- df.cov %>% filter(region=="World") %>% mutate(year = as.numeric(year)) # just world, to save processing time where possible
gdp <- df.gdp %>% filter(region=="World")

# Part 1.2.1: Load GDP-specific data ====
gdp.var <- "GDP|MER" # alternatively, "GDP|PPP" could be used
gdp <- df.gdp %>% filter(region=="World")


# markers
gdp.marker <- df.cov.w %>% filter(variable==gdp.var) %>% filter(category=="Above 2.0C") %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
  mutate(min=min(value), q10=quantile(value,0.1), q25=quantile(value,0.25), med=median(value), q75=quantile(value,0.75), q90=quantile(value,0.9), max=max(value)) %>%
  filter(year>=year.start & year<=year.end)
gdp.marker.end <- gdp.marker %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
gdp.marker <- gdp.marker %>% left_join(gdp.marker.end, by=c("model", "scenario"))
gdp.marker <- gdp.marker %>% filter(scenario=="Green Push") # can be any of the covid scenarios

# separate no-covid scenario
gdp.no.covid <- df.cov.w %>% filter(variable==gdp.var) %>% filter(category=="Above 2.0C") %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
  mutate(min=min(value), q10=quantile(value,0.1), q25=quantile(value,0.25), med=median(value), q75=quantile(value,0.75), q90=quantile(value,0.9), max=max(value)) %>%
  filter(year>=year.start & year<=year.end)
gdp.no.covid.end <- gdp.no.covid %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
gdp.no.covid <- gdp.no.covid %>% left_join(gdp.no.covid.end, by=c("model", "scenario"))
gdp.no.covid <- gdp.no.covid %>% filter(scenario=="Baseline-no-COVID") # can be any of the covid scenarios

# set 2020 value
gdp.2020 <- gdp %>% filter(year==2020, variable==gdp.var) %>% mutate(value=
                                                                       (gdp.marker%>% filter(year==2020, variable==gdp.var))$value)
# add historical years (before reporting fix)
gdp.2015.val <- 57709*1.10774 # from summarized_data excel file, multiplied by scalar to go from 2005 to 2010 usd
gdp.2019.val <- 66912*1.10774 # from summarized_data excel file, multiplied by scalar to go from 2005 to 2010 usd 
gdp.2019 <- gdp.2020 %>% mutate(value=gdp.2019.val, year=2019)
gdp.2015 <- gdp.2020 %>% mutate(value=gdp.2015.val, year=2015)



# combine
gdp <- gdp %>% filter(year!=2020) %>% bind_rows(gdp.2020)
gdp.gdp <- gdp %>% filter(variable==gdp.var) %>% mutate(year=as.numeric(year)) %>% group_by(year) %>% arrange(scenario,year) %>%
  mutate(min=min(value), q10=quantile(value,0.1), q25=quantile(value,0.25), med=median(value), q75=quantile(value,0.75), q90=quantile(value,0.9), max=max(value)) %>%
  filter(year>=year.start & year<=year.end)
gdp.gdp.end <- gdp.gdp %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
gdp.gdp <- gdp.gdp %>% left_join(gdp.gdp.end, by=c("model", "scenario"))

# make a df for cumulative co2 emissions (consider moving these lines to below)
# covid scens:
df.co2 <- df.cov %>% filter(variable=="Emissions|CO2") %>% filter(region=="World") %>% rename(co2=value)
df.co2.c <- df.co2 %>% filter(!grepl('0',scenario,fixed=FALSE)) %>% select(scenario, year, co2) %>% mutate(year=as.numeric(year))
co22019 <- (df.co2 %>% filter(year==2019,scenario=="Baseline-no-COVID"))$co2
co22020 <- (df.co2 %>% filter(year==2020,scenario=="Restore"))$co2

gdp <- df.gdp %>% filter(region=="World")

# gdp:
gdp.v <- gdp %>% filter(variable=="Emissions|CO2") %>% mutate(year=as.numeric(year)) %>% group_by(year) %>% pivot_wider(scenario,year) %>%
  mutate(`2019`=co22019, `2020`=co22020) %>%
  pivot_longer(`2000`:`2100`, names_to="year") %>%
  arrange(scenario,year) %>% group_by(year) %>%
  mutate(min=min(value), q10=quantile(value,0.1), q25=quantile(value,0.25), med=median(value), q75=quantile(value,0.75), q90=quantile(value,0.9), max=max(value))
df.co2 <- df.cov %>% filter(variable=="Emissions|CO2") %>% filter(region=="World") %>% rename(co2=value) %>% select(c(scenario,year,co2)) %>%
  bind_rows(gdp.v %>% select(scenario,year,value) %>% rename(co2=value)) %>%
  arrange(year) %>% arrange(scenario) %>% mutate(year=as.numeric(year))


y2 <- unique(df.co2$year)[c(2:length(unique(df.co2$year)))]
y1 <- c(2000,y2[y2 != 2100])
# duration of each time period (will be used later also to calculate cumulative results)
duration <- data.frame(year = y2,
                       dur = y2-y1)


# Part 1.3: load SR15 data ====
# sr15 data (world only - just for categories now...)
df.15 <- vroom(f.sr15) %>% select(-c(colnames(.)[1])) %>% select(-c(exclude, baseline))
# sr15 data (world and r5 - no categories)
df.15.r5 <- read_excel(f.sr15.r5, sheet="data")
years<-c("2010", "2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050")
df.15.r5 <- df.15.r5 %>% select(Model,Scenario,Region,Variable,Unit,years) %>%
  pivot_longer(years,names_to="year",values_to="value") %>%
  rename(model=Model, scenario=Scenario, variable=Variable, region=Region, unit=Unit) %>%
  mutate(year=as.numeric(year))
df.15.r5 <- df.15.r5 %>%
  left_join(
    df.15 %>% select(model,scenario,category) %>% distinct()
  )
df.15 <- df.15.r5
# redo SR15 categories.
df.15 <- df.15 %>% mutate(category = ifelse(category=="Below 1.5C"|category=="1.5C low overshoot", "1.5C no or low OS",
                                            ifelse(category=="1.5C high overshoot"|category=="Lower 2C"|category=="Higher 2C", "Below 2.0C", category))) %>%
  mutate(color = ifelse(
    grepl("1.5C no or low OS", category, fixed = TRUE),"#1e9583",
    ifelse(
      grepl("Below 2.0C", scenario, fixed = TRUE),"#63bce4",
      "#e78731")))






# Part 1.4: Loading activity data ====
df.act <- read_excel(paste0(base.path, data.file), sheet="2025v2019") %>% rename(`Smart Use`=GL, `Self-Reliance`=SR, `Green Push`=GP) %>%
  pivot_longer(`Smart Use`:`Green Push`, values_to="Relative Change 2019-2025", names_to="Scenario")


# Part 1.5: Load upstream co2 emissions data ====
yr.upstream <- 2030 # select focus year for later visualizations


co2.upstream <- read_excel(paste0(base.path, data.file), sheet="upstream_data") %>%
  rename(model=Model, scenario=Scenario, unit=Unit, region=Region, variable=Variable) %>%
  select(-unit) %>%
  pivot_longer(`2010`:`2100`, names_to="year", values_to="value") %>% mutate(year=as.numeric(year)) %>%
  mutate(scenario=ifelse(scenario=='baseline_y_macro_clone','Baseline-no-COVID',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='green_learn_marker_mpa_adjusted_macro','Smart Use',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='green_push_marker_mpa_adjusted_macro','Green Push',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='NRC_GDP_marker_mpa_adjusted_macro','Restore',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='self_reliance_marker_mpa_adjusted_macro','Self-Reliance',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='green_push_550fp','Green Push (Same climate policy 1.5)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='NRC_GDP_550fp','Restore (Same climate policy 1.5)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_baseline2_550','Baseline-no-COVID (550)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_green_learn_550','Smart Use (550)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_green_push_550','Green Push (550)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_NRC_GDP_550','Restore (550)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_self_reliance_550','Self-Reliance (550)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_baseline2_1000','Baseline-no-COVID (1000)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_green_learn_1000','Smart Use (1000)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_green_push_1000','Green Push (1000)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_NRC_GDP_1000','Restore (1000)',scenario)) %>% #rename scenarios
  mutate(scenario=ifelse(scenario=='EN_self_reliance_1000','Self-Reliance (1000)',scenario)) #rename scenarios


df.cov.demand.co2 <- df.cov.w %>% filter(variable%in%c("Emissions|CO2|Energy|Demand|Residential and Commercial", "Emissions|CO2|Energy|Demand|Transportation", "Emissions|CO2|Energy|Demand|Industry"))
df.co2.demand.only <- df.cov.demand.co2 %>%
  mutate(sector=NA) %>%
  mutate(sector=ifelse(grepl(pattern='Transport',x=variable,fixed=T),'Transport',sector)) %>%
  mutate(sector=ifelse(grepl(pattern='Residential',x=variable,fixed=T),'Buildings',sector)) %>%
  mutate(sector=ifelse(grepl(pattern='Industr',x=variable,fixed=T),'Industry',sector)) %>%
  mutate(type="End-use") %>%
  group_by(year,scenario,sector,region,type) %>% summarise(value=sum(value)) %>%
  filter(!grepl('sens',scenario,fixed=T)) %>%
  mutate(category=ifelse(
    grepl("1000", scenario, fixed = FALSE),"Below 2.0C",
    ifelse(grepl("550", scenario, fixed = FALSE)|grepl("(Same climate policy 1.5)", scenario, fixed = FALSE),
           "1.5C",
           "Above 2.0C")
  ))


df.co2.sector.incl.upstream <- df.cov.demand.co2 %>%
  bind_rows(co2.upstream) %>%
  mutate(sector=NA) %>%
  mutate(sector=ifelse(grepl(pattern='Transport',x=variable,fixed=T),'Transport',sector)) %>%
  mutate(sector=ifelse(grepl(pattern='Residential',x=variable,fixed=T),'Buildings',sector)) %>%
  mutate(sector=ifelse(grepl(pattern='Industr',x=variable,fixed=T),'Industry',sector)) %>%
  mutate(type="Total") %>%
  bind_rows(
    df.cov.w %>% filter(variable=="Emissions|CO2") %>%
      mutate(sector="All CO2") %>%
      mutate(type="Total")
  ) %>%
  group_by(year,scenario,sector,region,type) %>% summarise(value=sum(value)) %>%
  filter(!grepl('sens',scenario,fixed=T)) %>%
  mutate(category=ifelse(
    grepl("1000", scenario, fixed = FALSE),"Below 2.0C",
    ifelse(grepl("550", scenario, fixed = FALSE)|grepl("(Same climate policy 1.5)", scenario, fixed = FALSE),
           "1.5C",
           "Above 2.0C")
  ))


df.upstream <- bind_rows(
  df.co2.demand.only,
  df.co2.sector.incl.upstream
) %>%
  filter(category%in%c('1.5C','Below 2.0C','Above 2.0C'))
df.upstream$cat.ordered = factor(df.upstream$category, levels=c('1.5C','Below 2.0C','Above 2.0C'))

# wedges
df.upstream.wedge <- df.upstream %>%
  filter(type=="Total") %>%
  filter(scenario%in%c('Green Push','Restore')) %>%
  pivot_wider(names_from=scenario, values_from=value)
df.upstream.wedge.diff <- df.upstream.wedge %>%
  mutate(value=`Restore`-`Green Push`) %>%
  select(year,region,sector,value) %>%
  pivot_wider(names_from = sector, values_from=value)
df.wedge <- df.upstream.wedge.diff %>%
  left_join(df.upstream.wedge %>% filter(sector=="All CO2", type=="Total"))
df.wedge.sectorcomparison <- df.wedge %>% filter(year%in%c(2025,2030)) %>% select(year,Buildings,Industry,Transport) %>%
  mutate(Buildings=Buildings/Transport, Industry=Industry/Transport, Transport=Transport/Transport)

df.upstream.wedge.15 <- df.upstream %>%
  filter(type=="Total") %>%
  filter(scenario%in%c('Green Push (Same climate policy 1.5)','Restore (Same climate policy 1.5)')) %>%
  pivot_wider(names_from=scenario, values_from=value)
df.upstream.wedge.diff.15 <- df.upstream.wedge.15 %>%
  mutate(value=`Restore (Same climate policy 1.5)`-`Green Push (Same climate policy 1.5)`) %>%
  select(year,region,sector,value) %>%
  pivot_wider(names_from = sector, values_from=value)
df.wedge.15 <- df.upstream.wedge.diff.15 %>%
  left_join(df.upstream.wedge.15 %>% filter(sector=="All CO2", type=="Total"))
df.wedge.15.sectorcomparison <- df.wedge.15 %>% filter(year%in%c(2025,2030)) %>% select(year,Buildings,Industry,Transport) %>%
  mutate(Buildings=Buildings/Transport, Industry=Industry/Transport, Transport=Transport/Transport)


# Part 1.6: Loading demand change data ====
act.change.df = read_excel(paste0(base.path, data.file), sheet="demand_change") %>% 
  gather(key = 'scenario', value = 'value',2:5) %>% 
  mutate(sector = if_else(sector == 'Industrial activity', 'Industry', sector))

# Part 1.7: MESSAGE and SR15 5-regional aggregation data preparation ====
variables <- c("Emissions|CO2",
               "Final Energy",
               "Final Energy|Residential and Commercial",
               "Final Energy|Transportation",
               "Final Energy|Industry"
)
plot.category <- "Above 2.0C" # select the category of covid scenarios to show, alternative e.g. "1.5C no or low OS"
plot.category.sr15 <- "1.5C no or low OS" # compare with this category from SR15 pathways


# Part 1.7.1: MESSAGEix 5 regional aggregation ====
oecd <- c("NAM", "WEU", "PAO")
afr <- c("AFR", "MEA")
lam <- c("LAM")
asia <- c("SAS", "PAS", "CPA")
ref <- c("EEU", "FSU")
regs <- c(
  "R5OECD",
  "R5MAF",
  "R5LAM",
  "R5ASIA",
  "R5REF"
)

# note: unit is dropped here
r5.sum.cov <- df.cov %>% filter(variable%in%variables, region%in%c(oecd,afr,lam,asia,ref)) %>%
  mutate(r5=case_when(
    region %in% oecd ~ "R5OECD",
    region %in% afr ~ "R5MAF",
    region %in% lam ~ "R5LAM",
    region %in% asia ~ "R5ASIA",
    region %in% ref ~ "R5REF"
  )) %>%
  drop_na(r5) %>%
  group_by(model,scenario,variable,year,category,color,r5) %>%
  summarise(value=sum(value)) %>%
  rename(region=r5)
r5.sum.gdp <- df.gdp %>% filter(variable%in%variables, region%in%c(oecd,afr,lam,asia,ref)) %>%
  mutate(r5=case_when(
    region %in% oecd ~ "R5OECD",
    region %in% afr ~ "R5MAF",
    region %in% lam ~ "R5LAM",
    region %in% asia ~ "R5ASIA",
    region %in% ref ~ "R5REF"
  )) %>%
  drop_na(r5) %>%
  group_by(model,scenario,variable,year,category,r5) %>%
  summarise(value=sum(value)) %>%
  rename(region=r5)

df.cov.vars <- df.cov %>% filter(variable%in%variables) %>%
  bind_rows(r5.sum.cov)
df.gdp.vars <- df.gdp %>% filter(variable%in%variables) %>%
  bind_rows(r5.sum.gdp)
df.15.vars <- df.15 %>% filter(variable%in%variables) %>%
  mutate(region=ifelse(region=="R5OECD90+EU","R5OECD",region)) %>%
  filter(region!="R5ROWO")

# select the required variable from the covid scenarios
df.cov.selected.regional <- df.cov.vars %>% filter(category==plot.category) %>% mutate(year=as.numeric(year)) %>% group_by(year, category, region, variable) %>%
  drop_na(value) %>%
  mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
  filter(year>=year.start & year<=year.end)
df.cov.selected.end <- df.cov.selected.regional %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
df.cov.selected.regional <- df.cov.selected.regional %>% left_join(df.cov.selected.end, by=c("model", "scenario"))
df.cov.selected.regional <- df.cov.selected.regional %>% filter(region%in%regs)

# get GDP uncertainty range for specific variable
gdp.regional <- df.gdp.vars %>% mutate(year=as.numeric(year)) %>% filter(category==plot.category) %>% group_by(year, category, region, variable) %>%
  drop_na(value) %>%
  mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
  filter(year>=year.start & year<=year.end)
gdp.w.end <- gdp.regional %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
gdp.regional <- gdp.regional %>% left_join(gdp.w.end, by=c("model", "scenario"))
gdp.regional <- gdp.regional %>% filter(region%in%regs)

# Part 1.7.2: SR15 ====

# get selected sr15 ranges for the required variable
df.regional.sr15 <- df.15.vars %>% filter(category==plot.category.sr15) %>% group_by(year, category, region, variable) %>%
  drop_na(value) %>%
  mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
  ungroup() %>%
  distinct(year, category, q10,med,q90, .keep_all = TRUE) %>% mutate(scenario=plot.category.sr15, model="SR15") %>%
  filter(year>=year.start & year<=year.end)
df.regional.end.sr15 <- df.regional.sr15 %>% filter(year==year.end) %>%
  rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
  ungroup() %>%
  select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
df.regional.sr15 <- df.regional.sr15 %>% left_join(df.regional.end.sr15)
df.regional.sr15 <- df.regional.sr15 %>% filter(region%in%regs)




# Part 2: Define auxiliary functions ====

# Part 2.1: function for main text figure 2 ====
do_main_timeseries_plot <- function(df.cov.selected, var, varname, varlims, year.end=2035,
                                    df.15=df.15.selected, cat.sr15="1.5C no or low OS", cat.covid="Above 2.0C",
                                    downsize=1, range=5,
                                    df.gdp.sel=df.gdp.selected,
                                    y.unit = "GtCO2/yr",
                                    plotlabels=TRUE, plotranges=TRUE, justpathways=FALSE, plotgdpsensitivity=TRUE){
  
  # get selected sr15 ranges for the required variable
  df.v.sr15 <- df.15.selected %>% filter(variable==var) %>% filter(category==cat.sr15) %>% group_by(year, category) %>%
    drop_na(value) %>%
    mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
    ungroup() %>%
    distinct(year, category, q10,med,q90, .keep_all = TRUE) %>% mutate(scenario=cat.sr15, model="SR15") %>%
    filter(year>=year.start & year<=year.end)
  df.v.end.sr15 <- df.v.sr15 %>% filter(year==year.end) %>%
    rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
    ungroup() %>%
    select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
  df.v.sr15 <- df.v.sr15 %>% left_join(df.v.end.sr15)
  print(df.v.sr15)
  
  # get GDP uncertainty range for specific variable
  gdp.v <- df.gdp.sel %>% filter(variable==var) %>% mutate(year=as.numeric(year)) %>% group_by(year) %>%
    drop_na(value) %>%
    mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
    filter(year>=year.start & year<=year.end)
  gdp.w.end <- gdp.v %>% filter(year==year.end) %>%
    rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
    ungroup() %>%
    select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
  gdp.v <- gdp.v %>% left_join(gdp.w.end, by=c("model", "scenario"))
  
  print("min and max emissions in 2020 for full gdp range")
  print(gdp.v %>% filter(year==2025) %>% select(scenario, min, max))
  
  # select the required variable from the covid scenarios
  df.cov.selected.v <- df.cov.selected %>% filter(variable==var) %>% filter(category%in%cat.covid) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
    drop_na(value) %>%
    mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
    filter(year>=year.start & year<=year.end)
  df.cov.selected.end <- df.cov.selected.v %>% filter(year==year.end) %>%
    rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
    ungroup() %>%
    select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
  df.cov.selected.v <- df.cov.selected.v %>% left_join(df.cov.selected.end, by=c("model", "scenario"))
  
  
  
  # do main plot
  geom.text.size  <- 4/downsize
  theme.size <- (14/5) * geom.text.size
  formatter1000 <- function(){
    function(x)x/1000
  }
  formatter.standard <- function(){
    function(x)x
  }
  
  if (justpathways==TRUE){
    p.v <- ggplot(df.cov.selected %>% filter(variable==var) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
                    drop_na(value) %>%
                    mutate(min=min(na.rm=T, value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(na.rm=T,value)) %>%
                    filter(year>=year.start & year<=year.end) ,
                  aes(x=year, group=scenario)) +
      # add 0 and net-zero
      geom_rect(aes(xmin=year.start, xmax=year.end, ymin=-50000, ymax=0), fill="#ffebcd", alpha=0.08) +
      
      
      # add gdp uncertainty range
      geom_ribbon(data=gdp.v %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model), fill=ifelse(plotgdpsensitivity==TRUE,'black',NULL), alpha=0.1) +
      
      # add covid scenarios
      geom_line(data= df.cov.selected.v,aes(y=value, colour=scenario), linetype="solid", size=1/downsize) +
      geom_point(data= df.cov.selected.v %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
                 aes(y=value, colour=scenario), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      coord_cartesian(xlim=c(year.start, year.end+2),
                      ylim=varlims) +
      
      ggtitle(varname) +
      theme_classic() +
      scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      theme(legend.position="none",
            title=element_text(size=theme.size),
            text = element_text(size=theme.size)
      ) +
      xlab(NULL) +
      ylab(y.unit) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0),
                         labels = ifelse(y.unit=="GtCO2/yr",formatter1000(),formatter.standard()))
    if (plotlabels){
      p.v <- p.v +
        coord_cartesian(xlim=c(year.start,year.end+5),ylim=varlims) + 
        # add labels to scenarios
        geom_text_repel(data=df.cov.selected.v %>% filter(year==year.end),
                        size=geom.text.size/(downsize+0.5),
                        xlim=c(year.end+1,year.end+5),
                        aes(x=year.end,
                            y=value,
                            group=scenario, label=scenario, colour=scenario),  direction="y", hjust = 0, force=5)
      return(p.v)
    }
    
  }
  
  if (range=="minmax"){
    p.v <- ggplot(df.cov.selected %>% filter(variable==var) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
                    drop_na(value) %>%
                    mutate(min=min(na.rm=T, value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(na.rm=T,value)) %>%
                    filter(year>=year.start & year<=year.end) ,
                  aes(x=year, group=scenario)) +
      # add 0 and net-zero
      geom_rect(aes(xmin=year.start, xmax=year.end, ymin=-50000, ymax=0), fill="#ffebcd", alpha=0.08) +
      
      # add sr 15 timeseries category
      geom_ribbon(data=df.v.sr15, aes(ymin=min, ymax=max, fill=category), alpha=0.1) +
      geom_ribbon(data=df.v.sr15, aes(ymin=q25, ymax=q75, fill=category), alpha=0.3) +
      geom_line(data=df.v.sr15, aes(y=min, colour=category), size=0.1) +
      geom_line(data=df.v.sr15, aes(y=max, colour=category), size=0.1) +
      geom_line(data=df.v.sr15, aes(y=med, colour=category), linetype="dashed", size=1.5/downsize) +
      geom_point(data=df.v.sr15, aes(y=med, colour=category), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      # add gdp uncertainty range
      geom_ribbon(data=gdp.v %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model), fill='black', alpha=0.1) +

      # add covid scenarios
      geom_line(data= df.cov.selected.v,aes(y=value, colour=scenario), linetype="solid", size=1/downsize) +
      geom_point(data= df.cov.selected.v %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
                 aes(y=value, colour=scenario), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      coord_cartesian(xlim=c(year.start, year.end+2), ylim=varlims) +
      
      ggtitle(varname) +
      theme_classic() +
      scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      theme(legend.position="none",
            title=element_text(size=theme.size),
            text = element_text(size=theme.size)
      ) +
      xlab(NULL) +
      ylab(y.unit) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), labels = ifelse(y.unit=="GtCO2/yr",formatter1000(),formatter.standard()))
  } else if (range==25){
    p.v <- ggplot(df.cov.selected.v,
                  aes(x=year, group=scenario)) +
      # add 0 and net-zero
      geom_rect(aes(xmin=year.start, xmax=year.end, ymin=-50000, ymax=0), fill="#ffebcd", alpha=0.08) +
      
      # add sr 15 timeseries category
      geom_ribbon(data=df.v.sr15, aes(ymin=q25, ymax=q75, fill=category), alpha=0.3) +
      geom_line(data=df.v.sr15, aes(y=med, colour=category), linetype="dashed", size=1.5/downsize) +
      geom_point(data=df.v.sr15, aes(y=med, colour=category), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      # add gdp uncertainty range
      geom_ribbon(data=gdp.v %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model), fill='black', alpha=0.1) +
      
      # add covid scenarios
      geom_line(data= df.cov.selected.v,aes(y=value, colour=scenario), linetype="solid", size=1/downsize) +
      geom_point(data= df.cov.selected.v %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
                 aes(y=value, colour=scenario), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      coord_cartesian(xlim=c(year.start, year.end+2), ylim=varlims) +
      
      ggtitle(varname) +
      theme_classic() +
      scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      theme(legend.position="none",
            title=element_text(size=theme.size),
            text = element_text(size=theme.size)
      ) +
      xlab(NULL) +
      ylab(y.unit) +
      scale_x_continuous(expand = c(0,0), limits = c(year.start,year.end+5)) +
      scale_y_continuous(expand = c(0,0), labels = ifelse(y.unit=="GtCO2/yr",formatter1000(),formatter.standard()))
  }
  
  if (plotlabels){
    p.v <- p.v +
      coord_cartesian(xlim=c(year.start,year.end+5),ylim=varlims) + #c(year.start, year.end+2+(8*(downsize))), ylim=varlims) +
      # add labels to scenarios
      geom_text_repel(data=df.cov.selected.v %>% filter(year==year.end),
                      size=geom.text.size/(downsize+0.5),
                      xlim=c(year.end+1,year.end+5),#c(year.end+2-10+10*downsize, year.end+5+(3*(downsize-1))-10+15*downsize ),
                      aes(x=year.end,
                          y=value,
                          # fontface = "bold",
                          group=scenario, label=scenario, colour=scenario),  direction="y", hjust = 0, force=5)
  }
  
  
  # calculate combined_ranges
  if (plotranges){
    NPi_str <- "CD-LINKS_NPi"
    df.npi.sr15 <- df.15.selected %>% filter(variable==var) %>%
      filter(scenario==NPi_str) %>%
      filter(year==year.end)
    df.npi.sr15$category <- "NPi"
    df.npi.sr15 <- df.npi.sr15 %>%
      group_by(year, category) %>%
      mutate(min=min(na.rm=T, value), q10=quantile(na.rm=T,x=value,0.1), med=median(value), q90=quantile(na.rm=T,x=value,0.9), max=max(na.rm=T, value)) %>%
      ungroup() %>%
      distinct(year, category, q10,med,q90, .keep_all = TRUE) %>%
      select(-c(model,value)) %>%
      filter(year>=year.start & year<=year.end)
    
    
    NDC_str <- "CD-LINKS_INDCi"
    df.ndc.sr15 <- df.15.selected %>% filter(variable==var) %>%
      filter(scenario==NDC_str)
    df.ndc.sr15$category <- "INDCi"
    df.ndc.sr15 <- df.ndc.sr15 %>%
      group_by(year, category) %>%
      mutate(min=min(na.rm=T,value), q10=quantile(na.rm=T,x=value,0.1), med=median(value), q90=quantile(na.rm=T,x=value,0.9), max=max(na.rm=T, value)) %>%
      ungroup() %>%
      distinct(year, category, q10,med,q90, .keep_all = TRUE) %>%
      select(-c(model,value)) %>%
      filter(year>=year.start & year<=year.end)
    
    t15_str <- "1.5C no or low OS"
    df.t15.sr15 <- df.15.selected %>% filter(variable==var) %>%
      filter(category==t15_str)
    df.t15.sr15$scenario <- t15_str
    df.t15.sr15 <- df.t15.sr15 %>%
      group_by(year, category) %>%
      mutate(min=min(na.rm=T,value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(na.rm=T,value)) %>%
      ungroup() %>%
      distinct(year, category, q10,q25,med,q75,q90, .keep_all = TRUE) %>%
      select(-c(model,value)) %>%
      filter(year>=year.start & year<=year.end)
    
    
    cov_range <- df.cov.selected %>% filter(variable==var & scenario!="Baseline-no-COVID") %>%
      filter(category%in%cat.covid) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
      mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), med=median(value), q90=quantile(na.rm=T,x=value,0.9), max=max(value))%>%
      filter(year==year.end) %>%  mutate(scenario='Covid recoveries') %>% select(-model)
    
    combined_ranges <- bind_rows(df.npi.sr15, df.ndc.sr15) %>% bind_rows(df.t15.sr15) %>% bind_rows(cov_range)
    print(combined_ranges)
    
    p.r <- ggplot(combined_ranges %>% filter(year==year.end), aes(x=year.end))+
      geom_linerange(aes(x=year,xmin=year,
                         xmax=year,
                         y=min,ymin=min,
                         ymax=max,
                         group=scenario,
                         colour=scenario),
                     position = position_dodge(width = .1),
                     size=2) +
      geom_text(aes(x=year,y=max+350,
                    label=paste0(as.character(round(max/1000))),
                    group=scenario,
                    colour=scenario),
                position = position_dodge(width = .1),
                size=2.5) +
      geom_text(aes(x=year,y=min-350,
                    label=paste0(as.character(round(min/1000))),
                    group=scenario,
                    colour=scenario),
                position = position_dodge(width = .1),
                size=2.5) +
      guides(colour=guide_legend("Scenario family"),
             label=FALSE) +
      theme_void()+
      theme(axis.line.x=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
      ) +
      scale_color_manual(name = "Scenario family",
                         labels = c("1.5C (no or low OS)", "NDCs (CD-LINKS)", "National Policies (CD-LINKS)", "Covid recoveries"),
                         values = c(mark_cols,
                                    "1.5C no or low OS"= "#1e9583",
                                    "Below 2.0C"="#63bce4",
                                    "Above 2.0C"="#e78731",
                                    "INDCi"="#8491B4FF",
                                    "NPi"="#4DBBD5FF",
                                    "Covid recoveries"="red",
                                    "CD-LINKS_NPi"="#4DBBD5FF",
                                    "CD-LINKS_INDCi"="#8491B4FF"
                         ))+
      xlab(NULL) +
      ylab(y.unit) +
      scale_y_continuous(limits = varlims,
                         expand = c(0,0))
    
    return(p.r)
  } else {
    return(p.v)
  }
  
  
}


# Part 2.1.1: simpler function that is alike main text figure 2 ====
do_main_timeseries_plot_simple <- function(df.cov.selected, var, varname, varlims, year.end=2035,
                                           df.15=df.15.selected, cat.sr15="1.5C no or low OS", cat.covid="Above 2.0C",
                                           downsize=1, range=5,
                                           df.gdp.sel=df.gdp.selected,
                                           y.unit = "GtCO2/yr",
                                           plotlabels=TRUE, plotranges=TRUE, justpathways=FALSE, plotgdpsensitivity=TRUE){
  
  
  # get GDP uncertainty range for specific variable
  gdp.v <- df.gdp.sel %>% filter(variable==var) %>% mutate(year=as.numeric(year)) %>% group_by(year) %>%
    drop_na(value) %>%
    mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
    filter(year>=year.start & year<=year.end)
  gdp.w.end <- gdp.v %>% filter(year==year.end) %>%
    rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
    ungroup() %>%
    select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
  gdp.v <- gdp.v %>% left_join(gdp.w.end, by=c("model", "scenario"))
  
  # select the required variable from the covid scenarios
  df.cov.selected.v <- df.cov.selected %>% filter(variable==var) %>% filter(category%in%cat.covid) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
    drop_na(value) %>%
    mutate(min=min(value), q10=quantile(na.rm=T,x=value,0.1), q25=quantile(na.rm=T,x=value,0.25), med=median(value), q75=quantile(na.rm=T,x=value,0.75), q90=quantile(na.rm=T,x=value,0.9), max=max(value)) %>%
    filter(year>=year.start & year<=year.end)
  df.cov.selected.end <- df.cov.selected.v %>% filter(year==year.end) %>%
    rename(min.end=min, q10.end=q10, q25.end=q25, med.end=med, q75.end=q75, q90.end=q90, max.end=max) %>%
    ungroup() %>%
    select(model, scenario, min.end, q10.end, q25.end, med.end, q75.end, q90.end, max.end)
  df.cov.selected.v <- df.cov.selected.v %>% left_join(df.cov.selected.end, by=c("model", "scenario"))
  
  
  
  # do main plot
  geom.text.size  <- 4/downsize
  theme.size <- (14/5) * geom.text.size
  formatter1000 <- function(){
    function(x)x/1000
  }
  formatter.standard <- function(){
    function(x)x
  }
  
  if (justpathways==TRUE){
    p.v <- ggplot(df.cov.selected %>% filter(variable==var) %>% mutate(year=as.numeric(year)) %>% group_by(year, category) %>%
                    drop_na(value) %>%
                    mutate(min=min(na.rm=T, value),
                           q10=quantile(na.rm=T,x=value,0.1),
                           q25=quantile(na.rm=T,x=value,0.25),
                           med=median(value),
                           q75=quantile(na.rm=T,x=value,0.75),
                           q90=quantile(na.rm=T,x=value,0.9),
                           max=max(na.rm=T,value)) %>%
                    filter(year>=year.start & year<=year.end) ,
                  aes(x=year, group=scenario)) +
      
      # add gdp uncertainty range
      geom_ribbon(data=gdp.v %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model),
                  fill=ifelse(plotgdpsensitivity==TRUE,'black','white'), alpha=0.1) +
      
      # add covid scenarios
      geom_line(data= df.cov.selected.v,aes(y=value, colour=scenario), linetype="solid", size=1/downsize) +
      geom_point(data= df.cov.selected.v %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
                 aes(y=value, colour=scenario), shape=21, size=3/downsize, fill="white", stroke = 1/downsize) +
      
      coord_cartesian(xlim=c(year.start, year.end+2),
                      ylim=varlims) +
      
      ggtitle(varname) +
      theme_classic() +
      scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
      theme(legend.position="none",
            title=element_text(size=theme.size),
            text = element_text(size=theme.size)
      ) +
      xlab(NULL) +
      ylab(y.unit) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0),
                         labels = ifelse(y.unit=="GtCO2/yr",formatter1000(),formatter.standard()))
    if (plotlabels){
      p.v <- p.v +
        coord_cartesian(xlim=c(year.start,year.end+5),ylim=varlims) + #c(year.start, year.end+2+(8*(downsize))), ylim=varlims) +
        # add labels to scenarios
        geom_text_repel(data=df.cov.selected.v %>% filter(year==year.end),
                        size=geom.text.size/(downsize+0.5),
                        xlim=c(year.end+1,year.end+5),#c(year.end+2-10+10*downsize, year.end+5+(3*(downsize-1))-10+15*downsize ),
                        aes(x=year.end,
                            y=value,
                            # fontface = "bold",
                            group=scenario, label=scenario, colour=scenario),  direction="y", hjust = 0, force=5)
      return(p.v)
    }
    
  }
  
  
}


# Part 2.2: Functions for data processing to analyse mitigation scenarios, for main text figure 3 ====

# Part 2.2.1: Define decarbonisation functions ====
decarb_sector <- function(df.co2, c.scen=comp.scen){
  df.co2.rec <- df.co2 %>% filter(year%in%c(2021,2025)) %>% pivot_wider(names_from = year, values_from = co2) %>%
    mutate(decarb_pace_rec=(`2025`-`2021`)/`2021`*100/4) %>%
    select(-c(`2025`,`2021`))
  df.co2.rec <- df.co2.rec %>%
    mutate(ref_rec = ifelse(grepl("550", scenario, fixed = FALSE),
                            (df.co2.rec %>% filter(scenario==paste0(c.scen, " (550)")))$decarb_pace_rec,
                            ifelse(grepl("1000", scenario, fixed = FALSE),
                                   (df.co2.rec %>% filter(scenario==paste0(c.scen, " (1000)")))$decarb_pace_rec,
                                   NA)))
  df.co2.post.rec <- df.co2 %>% filter(year%in%c(2025,2040)) %>% pivot_wider(names_from = year, values_from = co2) %>%
    mutate(decarb_pace_post_rec=(`2040`-`2025`)/`2025`*100/16) %>%
    select(-c(`2040`,`2025`))
  df.co2.post.rec <- df.co2.post.rec %>%
    mutate(ref_post_rec = ifelse(grepl("550", scenario, fixed = FALSE),
                                 (df.co2.post.rec %>% filter(scenario==paste0(c.scen, " (550)")))$decarb_pace_post_rec,
                                 ifelse(grepl("1000", scenario, fixed = FALSE),
                                        (df.co2.post.rec %>% filter(scenario==paste0(c.scen, " (1000)")))$decarb_pace_post_rec,
                                        NA)))
  df.co2.deco2 <- left_join(df.co2.rec,df.co2.post.rec) %>%
    mutate(postpone.ambition.ratio=decarb_pace_post_rec/decarb_pace_rec) %>%
    mutate(ref_ratio_rec=decarb_pace_rec/ref_rec) %>%
    mutate(ref_ratio_post_rec=decarb_pace_post_rec/ref_post_rec) %>%
    filter(grepl('550',scenario,fixed=FALSE)|grepl('1000',scenario,fixed=FALSE)) %>%
    mutate(clim=ifelse(grepl('550',scenario,fixed=FALSE),'1.5C','below 2C'))
  decarb.sector <- df.co2.deco2 %>% select(scenario,variable,decarb_pace_rec,decarb_pace_post_rec) %>% filter(!grepl('FP',scenario,fixed=FALSE))
  return(decarb.sector)
}
calculate_decarb <- function(dfl, region.sel = "World", comp.scen = 'Restore', only.total=FALSE){
  
  if (only.total){
    df.co2 <- dfl %>% filter(variable=="Emissions|CO2") %>%
      filter(region==region.sel) %>% rename(co2=value)
    decarb.total <<- decarb_sector(df.co2) %>% mutate(variable="Emissions|CO2")
    return(
      decarb.total %>%
        mutate(clim=ifelse(grepl('550',scenario,fixed=FALSE), '1.5', '2.0'))
    )
  }
  
  # CO2
  df.co2.t <- dfl %>% filter(variable=="Emissions|CO2|Energy|Demand|Transportation") %>%
    filter(region==region.sel) %>% rename(co2=value)
  df.co2.b <- dfl %>% filter(variable=="Emissions|CO2|Energy|Demand|Residential and Commercial") %>%
    filter(region==region.sel) %>% rename(co2=value)
  df.co2.i <- dfl %>% filter(variable=="Emissions|CO2|Energy|Demand|Industry") %>%
    filter(region==region.sel) %>% rename(co2=value)
  
  decarb.t <<- decarb_sector(df.co2.t, c.scen=comp.scen) %>% mutate(variable="Emissions|CO2|Energy|Demand|Transportation")
  decarb.b <<- decarb_sector(df.co2.b, c.scen=comp.scen) %>% mutate(variable="Emissions|CO2|Energy|Demand|Residential and Commercial")
  decarb.i <<- decarb_sector(df.co2.i, c.scen=comp.scen) %>% mutate(variable="Emissions|CO2|Energy|Demand|Industry")
  
  # combine decarbonisation paces in one dataframe
  decarb <- decarb.t %>% bind_rows(decarb.i) %>% bind_rows(decarb.b) %>%
    mutate(clim=ifelse(grepl('550',scenario,fixed=FALSE), '1.5', '2.0'))
  return(decarb)
}
get_relative_decarb_paces <- function(decarb, comp.scen = 'Restore'){
  out.df.for.radar <- decarb %>% select(scenario,variable,decarb_pace_post_rec) %>%
    # mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2"&grepl('550',scenario,fixed=TRUE),
    #                                      decarb_pace_post_rec/(decarb.total %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    # mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2"&grepl('1000',scenario,fixed=TRUE),
    #                                      decarb_pace_post_rec/(decarb.total %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Transportation"&grepl('550',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.t %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Transportation"&grepl('1000',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.t %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"&grepl('550',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.b %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"&grepl('1000',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.b %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Industry"&grepl('550',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.i %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    mutate(decarb_pace_post_rec = ifelse(variable=="Emissions|CO2|Energy|Demand|Industry"&grepl('1000',scenario,fixed=TRUE),
                                         decarb_pace_post_rec/(decarb.i %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$decarb_pace_post_rec,decarb_pace_post_rec)) %>%
    rename(value=decarb_pace_post_rec) %>%
    mutate(variable=ifelse(variable=="Emissions|CO2|Energy|Demand|Transportation","CO2 Transport",
                           ifelse(variable=="Emissions|CO2|Energy|Demand|Residential and Commercial","CO2 Buildings",
                                  ifelse(variable=="Emissions|CO2|Energy|Demand|Industry","CO2 Industry",
                                         variable))))
  return(out.df.for.radar)
}


# Part 2.2.2 function for producing all radar data ====
produce_radar_data <- function(dfcv=df.cov.all, compare.scens = c("Restore", "Baseline-no-COVID", "Green Push"), regions.to.plot=regions.to.plot, duration=duration){
  radar.all <- NULL
  # Part 2.2.2.1: variable selection and categorisation for region aggregation ====
  list.of.variables.for.circle.additive <- c(
    "Emissions|CO2", # general decarbonization
    "Emissions|CO2|Energy|Demand|Transportation", # decarbonizing transport
    "Emissions|CO2|Energy|Demand|Residential and Commercial", # decarbonizing buildings
    "Emissions|CO2|Energy|Demand|Industry", # decarbonizing industry
    "Capacity|Electricity|Coal", # coal phase-out
    "Emissions|Kyoto Gases (AR5-GWP100)", # for calculating aggregated carbon costs
    "Final Energy|Transportation|Electricity", # for electrification transport
    "Final Energy|Transportation", # for electrification transport
    "Investment|Energy Supply" # energy investment
  )
  list.of.variables.weights <- c(
    "Secondary Energy|Electricity", # for aggregating solar-wind share
    "Emissions|Kyoto Gases (AR5-GWP100)" # for calculating aggregated carbon costs
  )
  
  list.of.variables.for.circle.weighted <- c(
    "Secondary Energy|Electricity|Solar-Wind share", # electricity generation
    "Price|Carbon" # for calculating aggregated carbon costs
  )
  
  # Part 2.2.2.2: Do region aggregation for regional plots ====
  df <- dfcv %>% filter(variable%in%c(list.of.variables.for.circle.additive, list.of.variables.for.circle.weighted, list.of.variables.weights))
  
  # aggregate to R5 level
  oecd <- c("NAM", "WEU", "PAO")
  afr <- c("AFR", "MEA")
  lam <- c("LAM")
  asia <- c("SAS", "PAS", "CPA")
  ref <- c("EEU", "FSU")
  
  # note: unit will be dropped
  r5.sum <- df %>% filter(variable%in%list.of.variables.for.circle.additive, region%in%c(oecd,afr,lam,asia,ref)) %>%
    mutate(r5=case_when(
      region %in% oecd ~ "R5OECD",
      region %in% afr ~ "R5MAF",
      region %in% lam ~ "R5LAM",
      region %in% asia ~ "R5ASIA",
      region %in% ref ~ "R5REF"
    )) %>%
    drop_na(r5) %>%
    group_by(model,scenario,variable,year,category,color,r5) %>%
    summarise(value=sum(value)) %>%
    rename(region=r5)
  
  r5.calculate.weights <- df %>% filter(variable%in%list.of.variables.weights, region%in%c(oecd,afr,lam,asia,ref)) %>%
    mutate(r5=case_when(
      region %in% oecd ~ "R5OECD",
      region %in% afr ~ "R5MAF",
      region %in% lam ~ "R5LAM",
      region %in% asia ~ "R5ASIA",
      region %in% ref ~ "R5REF"
    )) %>% ungroup() %>%
    drop_na(r5) %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    group_by(model,scenario,year,category,color,r5) %>%
    summarise(`Carbon.sum`=sum(`Emissions|Kyoto Gases (AR5-GWP100)`),
              `Electricity.sum`=sum(`Secondary Energy|Electricity`))
  
  r5.weighted.sum <- df %>% filter(variable%in%c(list.of.variables.for.circle.weighted,list.of.variables.weights), region%in%c(oecd,afr,lam,asia,ref)) %>%
    mutate(r5=case_when(
      region %in% oecd ~ "R5OECD",
      region %in% afr ~ "R5MAF",
      region %in% lam ~ "R5LAM",
      region %in% asia ~ "R5ASIA",
      region %in% ref ~ "R5REF"
    )) %>% ungroup() %>%
    drop_na(r5) %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    left_join(r5.calculate.weights) %>%
    group_by(model,scenario,year,category,color,r5) %>%
    summarise(`Price|Carbon`=sum(`Price|Carbon`*`Emissions|Kyoto Gases (AR5-GWP100)`/`Carbon.sum`),
              `Secondary Energy|Electricity|Solar-Wind share`=sum(`Secondary Energy|Electricity|Solar-Wind share`*`Secondary Energy|Electricity`/`Electricity.sum`)) %>%
    pivot_longer(cols=`Price|Carbon`:`Secondary Energy|Electricity|Solar-Wind share`, names_to="variable", values_to="value") %>%
    rename(region=r5)
  r5.weighted.sum
  
  df <- df %>% bind_rows(
    r5.sum,
    r5.weighted.sum
  )
  
  # aggregate to R2 level
  GN <- c(oecd, ref)
  GS <- c(afr, lam, asia)
  
  # note: unit will be dropped
  r2.sum <- df %>% filter(variable%in%list.of.variables.for.circle.additive, region%in%c(GN,GS)) %>%
    mutate(r2=case_when(
      region %in% GN ~ "Global North",
      region %in% GS ~ "Global South"
    )) %>%
    drop_na(r2) %>%
    group_by(model,scenario,variable,year,category,color,r2) %>%
    summarise(value=sum(value)) %>%
    rename(region=r2)
  r2.calculate.weights <- df %>% filter(variable%in%list.of.variables.weights, region%in%c(oecd,afr,lam,asia,ref)) %>%
    mutate(r2=case_when(
      region %in% GN ~ "Global North",
      region %in% GS ~ "Global South"
    )) %>% ungroup() %>%
    drop_na(r2) %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    group_by(model,scenario,year,category,color,r2) %>%
    summarise(`Carbon.sum`=sum(`Emissions|Kyoto Gases (AR5-GWP100)`),
              `Electricity.sum`=sum(`Secondary Energy|Electricity`))
  
  r2.weighted.sum <- df %>% filter(variable%in%c(list.of.variables.for.circle.weighted,list.of.variables.weights), region%in%c(oecd,afr,lam,asia,ref)) %>%
    mutate(r2=case_when(
      region %in% GN ~ "Global North",
      region %in% GS ~ "Global South"
    )) %>% ungroup() %>%
    drop_na(r2) %>%
    pivot_wider(values_from = value, names_from = variable) %>%
    left_join(r2.calculate.weights) %>%
    group_by(model,scenario,year,category,color,r2) %>%
    summarise(`Price|Carbon`=sum(`Price|Carbon`*`Emissions|Kyoto Gases (AR5-GWP100)`/`Carbon.sum`),
              `Secondary Energy|Electricity|Solar-Wind share`=sum(`Secondary Energy|Electricity|Solar-Wind share`*`Secondary Energy|Electricity`/`Electricity.sum`)) %>%
    pivot_longer(cols=`Price|Carbon`:`Secondary Energy|Electricity|Solar-Wind share`, names_to="variable", values_to="value") %>%
    rename(region=r2)
  r2.weighted.sum
  
  df <- df %>% bind_rows(
    r2.sum,
    r2.weighted.sum
  )
  
  # Part 2.2.2.3-: Calculate all radar indicators ====
  for (comp.scen in compare.scens){
    for (r in regions.to.plot){
      region.for.circle <- r
      
      # Part 2.2.2.3: Emissions indicators ====
      decarb <- calculate_decarb(df %>% filter(region==region.for.circle), region.sel = region.for.circle)
      radar.emissions <- get_relative_decarb_paces(decarb, comp.scen = comp.scen)
      
      
      # Part 2.2.2.4: Supply-side indicators ====
      df.coal.capacity <- df %>% filter(variable=="Capacity|Electricity|Coal") %>% rename(coal.c=value)
      coal.w.2020.2030 <- df.coal.capacity %>% left_join(duration %>% mutate(year=as.character(year))) %>% filter(region==region.for.circle, year>=2020, year<=2030) %>%
        group_by(scenario) %>% mutate(coal.c = cumsum(coal.c*dur)) %>%
        filter(year==2030) %>% select(-dur)
      radar.supply.coal <- coal.w.2020.2030 %>% select(scenario,variable,coal.c) %>%
        filter(grepl('1000',scenario,fixed=TRUE)|grepl('550',scenario,fixed=TRUE),!grepl('FP',scenario,fixed=TRUE)) %>%
        mutate(coal.c = ifelse(variable=="Capacity|Electricity|Coal"&grepl('550',scenario,fixed=TRUE),
                               coal.c/(coal.w.2020.2030 %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$coal.c,coal.c)) %>%
        mutate(coal.c = ifelse(variable=="Capacity|Electricity|Coal"&grepl('1000',scenario,fixed=TRUE),
                               coal.c/(coal.w.2020.2030 %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$coal.c,coal.c)) %>%
        rename(value=coal.c) %>%
        mutate(variable="Coal Phase-out") %>%
        mutate(value=1+(1-value)) # higher coal capacity = lower phase-out challenge
      df.ren.share <- df %>% filter(variable=="Secondary Energy|Electricity|Solar-Wind share") %>% rename(ren.share=value) %>% filter(region==region.for.circle)
      ren.share.2030 <- df.ren.share %>% filter(region==region.for.circle, year==2030)
      radar.supply.ren <- ren.share.2030 %>% select(scenario,variable,ren.share) %>%
        filter(grepl('1000',scenario,fixed=TRUE)|grepl('550',scenario,fixed=TRUE),!grepl('FP',scenario,fixed=TRUE)) %>%
        mutate(ren.share = ifelse(variable=="Secondary Energy|Electricity|Solar-Wind share"&grepl('550',scenario,fixed=TRUE),
                                  ren.share/(ren.share.2030 %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$ren.share,ren.share)) %>%
        mutate(ren.share = ifelse(variable=="Secondary Energy|Electricity|Solar-Wind share"&grepl('1000',scenario,fixed=TRUE),
                                  ren.share/(ren.share.2030 %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$ren.share,ren.share)) %>%
        rename(value=ren.share) %>%
        mutate(variable="Non-Biomass Renewables Share")
      
      # Part 2.2.2.5: Demand-side indicator ====
      df.transport.elec <- df %>% filter(variable=="Final Energy|Transportation|Electricity") %>% rename(transport.e=value) %>% filter(region==region.for.circle)
      df.transport.fe <- df %>% filter(variable=="Final Energy|Transportation") %>% rename(transport=value) %>% filter(region==region.for.circle)
      df.transport <- left_join(df.transport.fe,df.transport.elec %>% select(scenario,year,transport.e)) %>% mutate(elec.rate=transport.e/transport) %>% select(-c(transport.e,transport))
      transport.elec.2030 <- df.transport %>% filter(region==region.for.circle, year==2030)
      radar.demand <- transport.elec.2030 %>% select(scenario,variable,elec.rate) %>%
        filter(grepl('1000',scenario,fixed=TRUE)|grepl('550',scenario,fixed=TRUE),!grepl('FP',scenario,fixed=TRUE)) %>%
        mutate(elec.rate = ifelse(variable=="Final Energy|Transportation"&grepl('550',scenario,fixed=TRUE),
                                  elec.rate/(transport.elec.2030 %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$elec.rate,elec.rate)) %>%
        mutate(elec.rate = ifelse(variable=="Final Energy|Transportation"&grepl('1000',scenario,fixed=TRUE),
                                  elec.rate/(transport.elec.2030 %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$elec.rate,elec.rate)) %>%
        rename(value=elec.rate) %>%
        mutate(variable="Electrification Transport")
      
      # Part 2.2.2.6: Economic indicators ====
      df.cp <- df %>% filter(variable=="Price|Carbon") %>% filter(region==region.for.circle) %>% rename(c.price=value)
      df.kyoto <- df %>% filter(variable=="Emissions|Kyoto Gases (AR5-GWP100)") %>% filter(region==region.for.circle) %>% rename(kyoto=value)
      df.disc <- df.kyoto %>% select(-kyoto) %>%
        mutate(year=as.numeric(year))%>% mutate(disc.factor=(1/(1+0.05))^(year-2020)) %>% filter(year>=2020) %>% mutate(year=as.character(year))
      df.npv <- left_join(df.cp,df.kyoto %>% select(scenario,year,kyoto)) %>% left_join(df.disc%>% select(scenario,year,disc.factor)) %>%
        na.omit() %>% group_by(scenario) %>% mutate(npv=cumsum(kyoto*c.price*disc.factor/1000000)) %>%
        select(-c(kyoto,c.price,disc.factor)) %>% mutate(Unit="NPV carbon emissions in trillion US$2010")
      npv.2030 <- df.npv %>% filter(region==region.for.circle, year==2030)
      radar.econ.npv <- npv.2030 %>% select(scenario,variable,npv) %>%
        filter(grepl('1000',scenario,fixed=TRUE)|grepl('550',scenario,fixed=TRUE),!grepl('FP',scenario,fixed=TRUE)) %>%
        mutate(npv = ifelse(variable=="Price|Carbon"&grepl('550',scenario,fixed=TRUE),
                            npv/(npv.2030 %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$npv,npv)) %>%
        mutate(npv = ifelse(variable=="Price|Carbon"&grepl('1000',scenario,fixed=TRUE),
                            npv/(npv.2030 %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$npv,npv)) %>%
        rename(value=npv) %>%
        mutate(variable="Carbon Costs")
      df.inv <- df %>% filter(variable=="Investment|Energy Supply") %>% rename(inv=value) %>% filter(region==region.for.circle)
      inv.2020.2030 <- df.inv %>% left_join(duration %>% mutate(year=as.character(year))) %>% filter(region==region.for.circle, year>=2020, year<=2030) %>%
        group_by(scenario) %>% mutate(inv = cumsum(inv*dur)) %>%
        # still need to control/subtract from baseline? or is that fine now we do 2020-2030?
        filter(year==2030) %>% select(-dur)
      radar.econ.inv <- inv.2020.2030 %>% select(scenario,variable,inv) %>%
        filter(grepl('1000',scenario,fixed=TRUE)|grepl('550',scenario,fixed=TRUE),!grepl('FP',scenario,fixed=TRUE)) %>%
        mutate(inv = ifelse(variable=="Investment|Energy Supply"&grepl('550',scenario,fixed=TRUE),
                            inv/(inv.2020.2030 %>% filter(grepl(paste0(comp.scen, ' (550)'),scenario,fixed=TRUE)))$inv,inv)) %>%
        mutate(inv = ifelse(variable=="Investment|Energy Supply"&grepl('1000',scenario,fixed=TRUE),
                            inv/(inv.2020.2030 %>% filter(grepl(paste0(comp.scen, ' (1000)'),scenario,fixed=TRUE)))$inv,inv)) %>%
        rename(value=inv)%>%
        mutate(variable="Energy Investments")
      
      
      # Part 2.2.2.7: Combine indicators in one dataframe ====
      
      radar <- radar.emissions %>% filter(variable!="Emissions|CO2") %>%
        bind_rows(radar.supply.coal) %>%
        bind_rows(radar.supply.ren) %>%
        bind_rows(radar.demand) %>%
        bind_rows(radar.econ.npv) %>%
        bind_rows(radar.econ.inv)
      
      # filter out values that are 1 to not freak out log
      radar <- radar %>%
        filter(value!=1)
      
      # invert if Green Push:
      if (comp.scen=="Green Push"){
        radar<- radar %>%
          mutate(value=(value-1)*-1)
      }
      
      radar.all <- radar.all %>%
        bind_rows(
          radar %>% mutate(compare=comp.scen) %>% mutate(region=r)
        )
      
      
      
    }
  }
  radar.out <- radar.all
  
  return(radar.out)
}



# Part 2.3: function for plotting main text figure 3 ====
make_radar_v2 <- function(df,
                          scenario.ambition.level="(550)",
                          scenarios=c("Restore", "Green Push"),
                          regions=c("R5ASIA","R5LAM","R5MAF","R5OECD","R5REF")){
  radar.data <- df%>% filter(compare%in%scenarios) %>%
    filter(grepl(scenario.ambition.level,scenario,fixed=TRUE)) %>%
    mutate(scenario=ifelse(grepl('Self',scenario,fixed=TRUE), 'Self-Reliance',
                           ifelse(grepl('Green Push',scenario,fixed=TRUE), 'Green Push',
                                  ifelse(grepl('Restore',scenario,fixed=TRUE), 'Restore',
                                         ifelse(grepl('Smart Use',scenario,fixed=TRUE),'Smart Use',
                                                'Baseline-no-COVID'))))) %>%
    filter(grepl('Push',scenario,fixed=TRUE)|
             grepl('Rest',scenario,fixed=TRUE)|
             grepl('Use',scenario,fixed=TRUE)|
             grepl('Self',scenario,fixed=TRUE)) %>%
    distinct() # drop duplicates for world

  p <- ggplot(radar.data,
              aes(x=variable,
                  y=(value)*100,
                  fill=scenario
              ))+
    geom_hline(yintercept = seq(-20, 10, by = 10), colour = "grey90", size = 0.2) +
    geom_bar(data = . %>% filter(region=="World") ,
             aes(colour=scenario),
             width=1.75,
             size=1,
             stat="identity",
             position = position_dodge(width=0.3), # for >2 bars
             alpha=0.2) +
    geom_point(data = . %>% filter(region%in%regions),
               aes(shape=region, color=scenario),
               size=2,
               position = position_dodge(width=0.3)) +
    geom_text(data = . %>% filter(region%in%regions),
              aes(y=25, label=ifelse(value>0.3,round((value)*100),NA), color=scenario),
              size=3,
              position = position_dodge(width=0.3)) +
    geom_hline(yintercept = 0,  colour="#51883E", 
               linetype="solid",
               size=1) +
    xlab("") + ylab("") +
    scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731")) +
    scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731")) +
    scale_alpha_manual(values=c(mark_cols)) +
    theme_ipsum() +
    scale_y_continuous(
      limits = c((min((radar.data %>% filter(region%in%regions))$value))*100*1.3, # some slack/space in the middle of the circle plot
                 (max((radar.data %>% filter(region%in%regions))$value))*100)
    ) +
    coord_polar() +
    theme(
      axis.text.x = element_text(angle=0,size=8),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    guides(color=guide_legend("Scenario"),
           fill=guide_legend("Scenario"),
           shape=guide_legend("Region")
    )
  
  return(p)
}

# Part 2.3.1: simpler function for exploring mitigation results, in bar graph style ====
make_bar_simple <- function(df,
                            scenario.ambition.level="(550)",
                            compare.scenarios=c("Restore", "Green Push"),
                            plot.scenarios=c("Green Push (550)", "Self-Reliance (550)", "Smart Use (550)"),
                            regions=c("R5ASIA","R5LAM","R5MAF","R5OECD","R5REF"),
                            variables=c("CO2 Transport", "CO2 Industry", "CO2 Buildings", "Coal Phase-out", "Non-Biomass Renewables Share", "Electrification Transport", "Carbon Costs", "Energy Investments")){
  radar.data <- df %>% filter(scenario%in%plot.scenarios) %>% filter(compare%in%compare.scenarios) %>% filter(variable%in%variables) %>%
    filter(grepl(scenario.ambition.level,scenario,fixed=TRUE)) %>%
    mutate(scenario=ifelse(grepl('Self',scenario,fixed=TRUE), 'Self-Reliance',
                           ifelse(grepl('Green Push',scenario,fixed=TRUE), 'Green Push',
                                  ifelse(grepl('Restore',scenario,fixed=TRUE), 'Restore',
                                         ifelse(grepl('Smart Use',scenario,fixed=TRUE),'Smart Use',
                                                'Baseline-no-COVID'))))) %>%
    filter(grepl('Push',scenario,fixed=TRUE)|
             grepl('Rest',scenario,fixed=TRUE)|
             grepl('Use',scenario,fixed=TRUE)|
             grepl('Self',scenario,fixed=TRUE)) %>%
    mutate(value = (value-1)) %>%
    distinct() # drop duplicates for world
  p <- ggplot(radar.data,
              aes(x=0,
                  y=(value)*100,
                  fill=scenario
              ))+
    geom_hline(yintercept = 0) +
    geom_bar(data = . %>% filter(region=="World") ,
             aes(colour=scenario),
             width=1,
             size=1,
             stat="identity",
             position = position_dodge(width=0), # for 1-2 bars. use `position_dodge(width=0.3)`, # for >2 bars
             alpha=0.2) +
    geom_point(data = . %>% filter(region%in%regions),
               aes(shape=region, color=scenario),
               size=2,
               position = position_dodge(width=0.3)
    ) +
    xlab(NULL) + ylab("% diff to Restore") +
    scale_x_continuous(expand = c(0.4,0.4)) +
    scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731")) +
    scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731")) +
    scale_alpha_manual(values=c(mark_cols)) +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    guides(color=guide_legend("Scenario"),
           fill=guide_legend("Scenario"),
           shape=guide_legend("Region")
    )
  
  return(p)
}


# Part 2.4: function for plotting world map ====
print_map <- function(df,col,val_low,val_high,stepsize,title, savestring,colouring){
  colorp <- brewer.pal(n = length(seq(from = val_low, to = val_high, by = stepsize))-1, name = colouring)
  # pdf
  pdf(file = paste0(out.path, savestring,".pdf"),
      bg = "transparent", height = 8, width = 12, pointsize = 20)
  # create a spatialpolygonsdataframe with a specific projection
  sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica'),] # (using rworldmap)
  sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84")) # transform to robinson projection (using rgdal)
  # merge the dataframe with the projection
  polydata <- merge(sPDF, df, by.x="ISO3", by.y="iso")
  # plot the map (using rworldmap)
  maps <- mapCountryData(polydata,
                         catMethod = seq(from = val_low, to = val_high, by = stepsize),
                         mapTitle = title,
                         nameColumnToPlot=col,
                         colourPalette=colorp,
                         lwd = 0.1,
                         addLegend='TRUE')
  maps
  dev.off()
  return(maps)
}


# Part 3: Main text figures ====


# Part 3.1: Figure 1 ====

p.act.bar.fig1 <- ggplot()+
  geom_bar(data = act.change.df,
           aes(x = factor(sector,levels = c( 'Industry','Buildings','Freight','Mobility' )),
                                    y = value, fill = scenario), 
           stat = "identity", position = "dodge", width = 0.5)+
  facet_wrap(~factor(scenario,levels = c("Self-Reliance","Green Learn", "Restore" , "Green Push")) )+
  geom_bar(data = act.change.df %>% filter(scenario == 'Restore') %>% select(-scenario),
           aes(x = factor(sector,levels = c( 'Industry','Buildings','Freight','Mobility' )),
               y = value), color = 'black', alpha = 0.0, size = 0.3, width = 0.5,
           stat = "identity", position = "dodge")+
  geom_hline(yintercept = 0,size = 0.5)+
  coord_flip()+
  theme_bw()+theme(axis.title = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major.y = element_blank(),
                   legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values = mark_cols)+
  ggtitle('Energy services changes between 2019 and 2025')

ggsave(paste0(out.path, "figure1.pdf"), device=cairo_pdf,
       p.act.bar.fig1,
       width=150,
       height=150,
       unit="mm",
       dpi=700)

# Part 3.2: Figure 2 ====
# Part 3.2.1: Fig. 2 A-C, global sectoral final energy and D global total co2 emissions ====
plot.category <- "Above 2.0C" # select the category of covid scenarios to show, alternative e.g. "1.5C no or low OS"
plot.category.sr15 <- "1.5C no or low OS" # compare with this category from SR15 pathways
variables <- c("Emissions|CO2",
               "Final Energy",
               "Final Energy|Residential and Commercial",
               "Final Energy|Transportation",
               "Final Energy|Industry"
               )
df.cov.vars <- df.cov %>% filter(variable%in%variables)
df.15.vars <- df.15 %>% filter(variable%in%variables) %>%
  mutate(region=ifelse(region=="R5OECD90+EU","R5OECD",region)) %>%
  filter(region!="R5ROWO")
df.gdp.vars <- df.gdp %>% filter(variable%in%variables)
for (r in c("World")){
  df.cov.selected <- df.cov.vars %>% filter(region==r)
  df.15.selected <- df.15.vars %>% filter(region==r)
  df.gdp.selected <- df.gdp.vars %>% filter(region==r)
  
  limits.scalar <- ifelse(r=="World",1,0.5)
  
  # Part 1.3.2: totals ===
  plot.co2.total <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2", varname="Annual CO2 Emissions", varlims=limits.scalar*c(-5000, 55000),
                                            df.15 = df.15.selected,
                                            downsize=1, range="minmax",
                                            cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                            plotlabels=TRUE, plotranges=FALSE)
  plot.co2.total.small <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2", varname="Annual CO2 Emissions", varlims=limits.scalar*c(-5000, 55000),
                                                  df.15 = df.15.selected,
                                                  downsize=1.7, range="minmax",
                                                  cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                  plotlabels=FALSE, plotranges=FALSE)
  if(r=="World"){
    plot.co2.total.range <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2", varname="Annual CO2 Emissions", varlims=limits.scalar*c(-5000, 55000),
                                                    df.15 = df.15.selected,
                                                    downsize=1, range="minmax",
                                                    cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                    plotlabels=TRUE, plotranges=TRUE)
  }
  
  plot.fe.total <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Final Energy", varname="Annual Final Energy Demand", varlims=limits.scalar*c(150, 600),
                                           df.15 = df.15.selected,
                                           downsize=1, range="minmax",
                                           cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                           y.unit = "EJ/yr",
                                           plotlabels=TRUE, plotranges=FALSE)
  if(r=="World"){
    plot.fe.total.range <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Final Energy", varname="Annual Final Energy Demand", varlims=limits.scalar*c(150, 600),
                                                   df.15 = df.15.selected,
                                                   downsize=1, range="minmax",
                                                   cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                   y.unit = "EJ/yr",
                                                   plotlabels=TRUE, plotranges=TRUE)
  }
  # buildings
  if ("Emissions|CO2|Energy|Demand|Residential and Commercial"%in%variables){
    plot.co2.buildings <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2|Energy|Demand|Residential and Commercial", varname="Buildings CO2", varlims=limits.scalar*c(0, 5500),
                                                  df.15 = df.15.selected,
                                                  downsize=2, range=25,
                                                  cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                  plotlabels=FALSE, plotranges=FALSE)
  }
  
  plot.fe.buildings <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Final Energy|Residential and Commercial", varname="Buildings Energy", varlims=limits.scalar*c(60, 250),
                                               df.15 = df.15.selected,
                                               downsize=2, range=25,
                                               y.unit = "EJ/yr",
                                               cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                               plotlabels=FALSE, plotranges=FALSE)
  # transport
  if ( "Emissions|CO2|Energy|Demand|Transportation"%in%variables ){
    plot.co2.transport <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2|Energy|Demand|Transportation", varname="Transport CO2", varlims=limits.scalar*c(0, 13000),
                                                  df.15 = df.15.selected,
                                                  downsize=2, range=25,
                                                  cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                  plotlabels=FALSE, plotranges=FALSE)
  }
  
  
  plot.fe.transport <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Final Energy|Transportation", varname="Transport Energy", varlims=limits.scalar*c(60, 250),
                                               df.15 = df.15.selected,
                                               downsize=2, range=25,
                                               y.unit = "EJ/yr",
                                               cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                               plotlabels=FALSE, plotranges=FALSE)
  # industry
  if ( "Emissions|CO2|Energy|Demand|Industry"%in%variables ){
    plot.co2.industry <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Emissions|CO2|Energy|Demand|Industry", varname="Industry CO2", varlims=limits.scalar*c(0, 3000),
                                                 df.15 = df.15.selected,
                                                 downsize=2, range=25,
                                                 cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                                 plotlabels=FALSE, plotranges=FALSE)
  }
  
  plot.fe.industry <- do_main_timeseries_plot(df.cov.selected=df.cov.selected, var="Final Energy|Industry", varname="Industry Energy", varlims=limits.scalar*c(60, 250),
                                              df.15 = df.15.selected,
                                              downsize=2, range=25,
                                              y.unit = "EJ/yr",
                                              cat.sr15=plot.category.sr15, cat.covid=plot.category,
                                              plotlabels=FALSE, plotranges=FALSE)
}

# Part 3.2.2: Fig. 2 E, comparing 2025 sectoral energy to national policies based forecasts ====
r <- "World"
year.npi.range <- c(2025) #c(2020,2025)
df.baseline.point <- df.cov.vars %>% filter(region==r, category=="Above 2.0C") %>% filter(year%in%year.npi.range) %>%
  filter(grepl("Baseline",scenario,fixed=T)) %>%
  ungroup %>% group_by(variable,year) %>% summarise(value=value) %>%
  mutate(`Scenario family`="Baseline-no-COVID")
df.cov.npi.range <- df.cov.vars %>% filter(region==r, category=="Above 2.0C") %>% filter(year%in%year.npi.range) %>%
  filter(!grepl("Baseline",scenario,fixed=T)) %>%
  ungroup %>% group_by(variable,year) %>% summarise(max=max(value, na.rm=T), min=min(value, na.rm=T)) %>%
  mutate(`Scenario family`="COVID scenarios")
df.15.npi.range <- df.15.vars %>% filter(region==r, scenario=="CD-LINKS_NPi") %>% filter(year%in%year.npi.range) %>%
  ungroup %>% group_by(variable,year) %>% summarise(max=max(value, na.rm=T), min=min(value, na.rm=T)) %>%
  mutate(`Scenario family`="National Policies")
# NPi-CDLINKS harmonized to baseline in 2020 to remove starting year differences for this comparison figure
df.15.npi.range.harmonized <- df.15.vars %>% filter(region==r, scenario=="CD-LINKS_NPi") %>% filter(year%in%c(2020,year.npi.range)) %>% mutate(year=as.character(year)) %>%
  left_join(df.cov.vars %>% filter(region==r, category=="Above 2.0C") %>% filter(year%in%c(2020,year.npi.range)) %>%
              filter(grepl("Baseline",scenario,fixed=T)) %>%
              ungroup %>% group_by(variable,year) %>% summarise(baseline.value=value)) %>%
  pivot_wider(names_from=year, values_from=c(value, baseline.value)) %>%
  mutate(diff.to.baseline.2020=value_2020-baseline.value_2020) %>%
  mutate(diff.to.baseline.2025=value_2025-baseline.value_2025) %>%
  mutate(harmonized.2020=value_2020 - (diff.to.baseline.2020)) %>%
  mutate(harmonized.2025=value_2025 - (diff.to.baseline.2020)) %>%
  ungroup %>% group_by(variable) %>% summarise(max=max(harmonized.2025, na.rm=T), min=min(harmonized.2025, na.rm=T)) %>%
  mutate(`Scenario family`="National Policies", year=2025)
# NPi-CDLINKS harmonized to baseline in 2020 to remove starting year differences for this comparison figure
df.15.npi.range.harmonized.models <- df.15.vars %>% filter(region==r, scenario=="CD-LINKS_NPi") %>% filter(year%in%c(2020,year.npi.range)) %>% mutate(year=as.character(year)) %>%
  left_join(df.cov.vars %>% filter(region==r, category=="Above 2.0C") %>% filter(year%in%c(2020,year.npi.range)) %>%
              filter(grepl("Baseline",scenario,fixed=T)) %>%
              ungroup %>% group_by(variable,year) %>% summarise(baseline.value=value)) %>%
  pivot_wider(names_from=year, values_from=c(value, baseline.value)) %>%
  mutate(diff.to.baseline.2020=value_2020-baseline.value_2020) %>%
  mutate(diff.to.baseline.2025=value_2025-baseline.value_2025) %>%
  mutate(harmonized.2020=value_2020 - (diff.to.baseline.2020)) %>%
  mutate(harmonized.2025=value_2025 - (diff.to.baseline.2020)) %>%
  ungroup %>% group_by(model,variable) %>% summarise(harmonized.value=harmonized.2025) %>%
  mutate(`Scenario family`="National Policies", year=2025) %>%
  mutate(model.letter=ifelse(model=="AIM/CGE 2.1","A",ifelse(model=="IMAGE 3.0.1","I",ifelse(model=="MESSAGEix-GLOBIOM 1.0","M",ifelse(model=="POLES CD-LINKS","P",ifelse(model=="REMIND-MAgPIE 1.7-3.0","R",ifelse(model=="WITCH-GLOBIOM 4.4","W",NA)))))))
df.gdp.npi.range <- df.gdp.vars %>% filter(region==r) %>% filter(year%in%year.npi.range) %>%
  ungroup %>% group_by(variable,year) %>% summarise(max=max(value), min=min(value)) %>%
  mutate(`Scenario family`="COVID scenarios \nwith GDP uncertainty")

ranges.vars <- c("Final Energy|Industry", "Final Energy|Residential and Commercial", "Final Energy|Transportation")

df.npi.ranges <- df.cov.npi.range %>% mutate(year=as.numeric(year)) %>%
  bind_rows(df.15.npi.range.harmonized) %>%
  bind_rows(df.gdp.npi.range %>% mutate(year=as.numeric(year))) %>%
  filter(variable%in%ranges.vars)


p.npi.ranges <- ggplot(df.npi.ranges,
                       aes(x=variable, colour=`Scenario family`)) +
  geom_linerange(data=. %>% filter(grepl("GDP",`Scenario family`,fixed=T)),
                 aes(ymin=min, ymax=max), size = 2.5, position = position_nudge(x = -0.1, y = 0) ) +
  geom_linerange(data=. %>% filter(grepl("scenarios",`Scenario family`,fixed=T)) %>% filter(!grepl("GDP",`Scenario family`,fixed=T)),
                 aes(ymin=min, ymax=max), size = 2.5, position = position_nudge(x = -0.1, y = 0) ) +
  geom_linerange(data=. %>% filter(grepl("National",`Scenario family`,fixed=T)),
                 aes(ymin=min, ymax=max), size = 2.5, position = position_nudge(x = 0.1, y = 0) ) +
  
  geom_point(data=df.baseline.point %>% filter(variable%in%ranges.vars) %>% filter(grepl("Baseline",`Scenario family`,fixed=T)),
             size=4, aes(y=value), shape=18, position = position_nudge(x = -0.1, y = 0) ) +
  
  geom_text(data=df.15.npi.range.harmonized.models %>% filter(variable%in%ranges.vars),
            aes(y=harmonized.value, x=variable, label=model.letter), position = position_nudge(x = 0.1, y = 0), colour="black", size=1.5) +
  
  
  ggtitle("2025 Energy versus Forecasts") +
  theme_classic() +
  scale_color_manual(breaks = c("Baseline-no-COVID", "COVID scenarios", "COVID scenarios \nwith GDP uncertainty", "National Policies"),
                     values=c("black", pal_npg("nrc")(2)[1], "grey", pal_npg("nrc")(2)[2]))+
  theme(
    axis.text.x = element_text(size=7),
    axis.text.y = element_text(size=10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0,210), expand = c(0,0)) +
  xlab(NULL) +
  ylab("EJ/yr") +
  scale_x_discrete(breaks=ranges.vars, labels=c("Industry", "Buildings", "Transport"))

p.npi.ranges

# Part 3.2.3: Fig. 2 F, cumulative CO2 emissions timeseries ====
# calculate cumulative emisssions
co2_cum_mark = df.co2 %>% left_join(duration) %>% ungroup() %>%
  group_by(scenario,year) %>%
  mutate(cum_co2_per = cumsum(co2*dur)/1000) %>% ungroup() %>%
  filter(year!=2000) %>%
  group_by(scenario) %>%
  mutate(cum_co2 = cumsum(cum_co2_per)) %>%
  mutate(Unit = 'Gt CO2')

co2cum2019 <- ((co2_cum_mark %>% filter(scenario=="Baseline-no-COVID",year==2019))$cum_co2)

co2_cum_mark %>% filter(year==2030) %>% mutate(cum_co2=(cum_co2-co2cum2019)) %>% select(scenario,year,cum_co2)

# taken from baseline emissions
co2.2018 <- 43
# ranges from SR15, given by Joeri.
range.15.50.min <- 230 - co2.2018
range.15.50.central <- 480 - co2.2018
range.15.50.max <- 730 - co2.2018

range.20.66.min <- 830 - co2.2018
range.20.66.central <- 1070 - co2.2018
range.20.66.max <- 1320 - co2.2018


max.min.co2_cum.2030 <- bind_rows(
  co2_cum_mark %>% filter(year==2030)
) %>%
  select(c(scenario, cum_co2, year)) %>%
  arrange(cum_co2)

max.min.co2_cum.2030.nogdp <- max.min.co2_cum.2030 %>% filter(!grepl("sens",scenario,fixed=T)) %>% mutate(cum_co2=cum_co2-(max.min.co2_cum.2030 %>% filter(scenario=="Baseline-no-COVID") %>% pull(cum_co2)))

p.cumulative <- ggplot(co2_cum_mark %>% filter(year>=2019&year<=2045) %>% filter(!grepl(' (',scenario,fixed=TRUE)) %>% filter(!grepl('sens',scenario,fixed=TRUE)),
                aes(y=cum_co2-co2cum2019, x=year, group=scenario)) +

  geom_ribbon(aes(ymin=range.20.66.min, ymax=range.20.66.max), fill="#ffebcd", alpha=0.08) +
  annotate("text", x=2024.5,y=range.20.66.central+50, label="Likely 2C")+
  geom_segment(aes(x=2019,xend=2045,
                   y=range.20.66.central,yend=range.20.66.central),
               color = "black", size=1) +
  
  geom_ribbon(aes(ymin=range.15.50.min, ymax=range.15.50.max), fill="#ffebcd", alpha=0.08) +
  annotate("text", x=2021.5,y=range.15.50.central+50, label="1.5C")+
  geom_segment(aes(x=2019,xend=2045,
                   y=range.15.50.central,yend=range.15.50.central),
               color = "black", size=1) +

    geom_ribbon(data=co2_cum_mark %>% filter(year>=2019&year<=2045) %>% filter(grepl('sens',scenario,fixed=TRUE)) %>%
                group_by(year) %>%
                mutate(min=min(cum_co2), q10=quantile(cum_co2,0.1), q25=quantile(cum_co2,0.25), med=median(cum_co2), q75=quantile(cum_co2,0.75), q90=quantile(cum_co2,0.9), max=max(cum_co2)),
              aes(ymin=min-co2cum2019, ymax=max-co2cum2019), fill="grey", alpha=0.2) +
  
  geom_line(aes(colour=scenario),linetype="solid",size=0.8) +
  geom_point(data= co2_cum_mark %>% filter(year>=2020&year<=2045) %>% filter(!grepl(' (',scenario,fixed=TRUE)) %>% filter(!grepl('_',scenario,fixed=FALSE))%>% filter(year!=2021,year!=2022,year!=2023,year!=2024),
             aes(colour=scenario), shape=21, size=2, fill="white", stroke = 1) +

  ylab("GtCO2") +
  xlab(NULL) +
  ggtitle("Cumulative CO2 Emissions")+
  scale_color_manual(values=c(mark_cols,
                              "1.5C no or low OS"= "#1e9583",
                              "Below 2.0C"="#63bce4",
                              "Above 2.0C"="#e78731",
                              "INDCi"="#8491B4FF",
                              "NPi"="#4DBBD5FF",
                              "Covid recoveries"="red",
                              "CD-LINKS_NPi"="#4DBBD5FF",
                              "CD-LINKS_INDCi"="#8491B4FF"
  ))+
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(xlim=c(2019, 2046), ylim= c(0,1500)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

p.cumulative
# Part 3.2.4: Fig. 2 G, Global GDP/yr indexed timeseries ====
p.gdp.range <- ggplot(gdp.gdp %>%filter(year>=year.start&year<=year.end), aes(x=year)) +
  geom_ribbon(aes(ymin=min/gdp.2019.val*100, ymax=max/gdp.2019.val*100),alpha=0.2)+
  geom_line(aes(y=min/gdp.2019.val*100), size=0.5) +
  geom_line(aes(y=max/gdp.2019.val*100), size=0.5) +
  
  
  geom_line(data=gdp.marker%>% bind_rows(gdp.2019) %>% bind_rows(gdp.2015), aes(y=value/gdp.2019.val*100), size=2) +
  geom_point(data=gdp.marker%>% bind_rows(gdp.2019) %>% bind_rows(gdp.2015)%>% filter(year!=2019,year!=2021,year!=2022,year!=2023,year!=2024), aes(y=value/gdp.2019.val*100), shape=21, size=3, fill="white", stroke = 1) +
  
  geom_line(data=gdp.no.covid%>% bind_rows(gdp.2019) %>% bind_rows(gdp.2015), aes(y=value/gdp.2019.val*100), size=1, linetype="longdash") +
  geom_point(data=gdp.no.covid%>% bind_rows(gdp.2019) %>% bind_rows(gdp.2015)%>% filter(year!=2019,year!=2021,year!=2022,year!=2023,year!=2024), aes(y=value/gdp.2019.val*100), shape=21, size=3, fill="white", stroke = 1) +
  
  theme_classic()+
  ggtitle("Global GDP")+
  ylab("Indexed to 2019") +
  xlab(NULL)
p.gdp.range

# Part 3.2.5: Fig. 2 combined ====
p.figure2.combined <- ggarrange(
  ggarrange(
    ggarrange(plot.fe.buildings+theme(axis.text=element_text(size=9),
                                      axis.title=element_text(size=12),
                                      title = element_text(size=12)) + coord_cartesian(xlim=c(2015,2035.5), ylim=c(60, 250)),
              plot.fe.transport+theme(axis.text=element_text(size=9),
                                      axis.title=element_text(size=12),
                                      title = element_text(size=12)) + coord_cartesian(xlim=c(2015,2035.5), ylim=c(60, 250)),
              plot.fe.industry+theme(axis.text=element_text(size=9),
                                     axis.title=element_text(size=12),
                                     title = element_text(size=12)) + coord_cartesian(xlim=c(2015,2035.5), ylim=c(60, 250)),
              nrow = 3,
              ncol = 1,
              font.label = list(size = 14, color = "black", face = "bold", family = NULL)
    ),
    ggarrange(plot.co2.total + coord_cartesian(xlim=c(2015,2042),ylim=c(-5000,55000)) + annotation_custom(ggplotGrob(plot.co2.total.range +
                                                                                                                       theme(legend.position = "none")),
                                                                                                          xmin=2035.3,xmax=2036.8,
                                                                                                          ymin=-5000, ymax=55000)+theme(axis.text=element_text(size=9),
                                                                                                                                        axis.title=element_text(size=12),
                                                                                                                                        title = element_text(size=14)),
              nrow = 1,
              ncol = 1,
              font.label = list(size = 14, color = "black", face = "bold", family = NULL)
    ),
    nrow = 1,
    ncol = 2,
    widths = c(4,8),
    font.label = list(size = 14, color = "black", face = "bold", family = NULL)
  ),
  ggarrange(p.npi.ranges+theme(axis.text=element_text(size=9),
                               axis.title=element_text(size=12),
                               panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
                               title = element_text(size=12),
                               legend.position = "none"),
            p.cumulative+theme(axis.text=element_text(size=9),
                        axis.title=element_text(size=12),
                        title = element_text(size=12)),
            p.gdp.range+theme(axis.text=element_text(size=9),
                              axis.title=element_text(size=12),
                              title = element_text(size=14)),
            nrow = 1,
            ncol = 3,
            widths = c(4,4,4),
            font.label = list(size = 14, color = "black", face = "bold", family = NULL)
  ),
  nrow = 2,
  ncol = 1,
  heights = c(8,4),
  font.label = list(size = 14, color = "black", face = "bold", family = NULL)
)

p.figure2.combined

ggsave(paste0(out.path, "figure2.pdf"), device=cairo_pdf,
       p.figure2.combined,
       width=250,
       height=300,
       unit="mm",
       dpi=700)


# Part 3.3: Figure 3 ====
regions.for.polar.figure <- c(
  "World",
  "Global North",
  "Global South"
)
radar.all <- produce_radar_data(dfcv = df.cov,
                                compare.scens = c("Restore", "Baseline-no-COVID", "Green Push"),
                                regions.to.plot=regions.for.polar.figure,
                                duration=duration)

p.radar.r2.15 <- make_radar_v2(radar.all, scenarios=c("Green Push"), regions = c("Global North", "Global South")) +
  theme(legend.position = "none")
p.radar.r2.15

ggsave(paste0(out.path, "figure3.pdf"), device=cairo_pdf,
       p.radar.r2.15,
       width=250,
       height=300,
       unit="mm",
       dpi=400)







# Part 4: Selected supplementary information figures ====

# Part 4.1: Supplementary Figure 12 ====
p.act <- ggplot(df.act  %>%
                  filter(`Sub-sector`!="Buildings (all)") %>%
                  arrange(Indicator) %>% 
                  mutate(Region=ifelse(Region=="GN", "Global North",
                                       ifelse(Region=="GS", "Global South",NA))),
                aes(y=-100*(1-`Relative Change 2019-2025`))) +
  facet_grid(`Sub-sector`~Scenario) +
  geom_hline(yintercept = 0, color="black") +
  geom_col(aes(x=Region, fill=Indicator), color="black", position=position_dodge2(width = 1, padding=0.5, preserve = "single")) +
  geom_text(aes(x=Region,
                y=ifelse(is.na(`Relative Change 2019-2025`),3,-100*(1-`Relative Change 2019-2025`)+ifelse((`Relative Change 2019-2025`)>1,+3,-3)),
                label=ifelse(is.na(`Relative Change 2019-2025`),"NA",round(-100*(1-`Relative Change 2019-2025`)))), color="black", position=position_dodge2(width = 0.9)) +
  theme_classic2() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
    panel.grid.minor.y = element_blank()
  ) +
  ylab("2019-2025 change (%)") +
  scale_fill_npg()
p.act

ggsave(paste0(out.path, "SI_fig12.pdf"),
       p.act,
       width=300,
       height=300,
       unit="mm",
       limitsize = FALSE,
       dpi=400)


# Part 4.2: Supplementary Figure 13 ====
ue.all <- df.cov %>% filter(grepl("Useful Energy|",variable,fixed=T)) %>% filter(!grepl("|Input",variable,fixed=T)) %>% filter(!grepl("(",scenario,fixed=T)) %>% mutate(year=as.numeric(year))

p.useful.all.world <- ggplot(ue.all %>% filter(year>=2015,year<=2030) %>% filter(region=="World"),
                             aes(x=year, y=value, colour=variable, group=variable)) +
  facet_wrap(~scenario, scales = "free") +
  geom_line(lwd=1) +
  ylab("Useful Energy (EJ/yr)") +
  theme_ipsum() +
  ggtitle("Global energy demand model input") +
  scale_y_continuous(expand=c(0,0)) +
  guides(
    color=guide_legend("Model input variable")
  ) +
  scale_x_continuous(breaks = c(2020,2030), labels = c("2020","2030")) +
  xlab(NULL) +
  scale_color_npg()
p.useful.all.world

ggsave(paste0(out.path, "SI_fig13.pdf"),device = cairo_pdf,
       p.useful.all.world,
       width=300,
       height=150,
       unit="mm",
       dpi=400)


# Part 4.2: Supplementary Figure 16 ====
# compare upstream size across all results
p.co2demand.incl.upstream <- ggplot(df.upstream %>%
                                      filter(year==yr.upstream),
                                    aes(y=value)) +
  facet_grid(cat.ordered~region) +
  coord_flip() +
  geom_hline(yintercept = 0, color="black") +
  geom_col(data=. %>% filter(type=="Total"),
           aes(x=sector, fill=sector), alpha=0.5, color="black", position=position_dodge2(width = 1, padding=0.5, preserve = "total")) +
  geom_col(data=. %>% filter(type=="End-use"),
           aes(x=sector, fill=sector), alpha=1, color="black", position=position_dodge2(width = 1, padding=0.5, preserve = "total")) +
  geom_text(data=. %>% filter(type=="Total"),
            aes(x=sector, y=value+500, label=scenario),hjust = 0, size=3, color="black", position=position_dodge2(width = 1)) +
  theme_classic2() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  ylab("MtCO2/yr") +
  xlab(NULL) +
  scale_y_continuous(limits = c(0,56000), expand=c(0,0)) +
  guides(fill=guide_legend(NULL)) +
  scale_fill_manual(breaks=c("All CO2",
                             "Buildings",
                             "Transport",
                             "Industry"),
                    values=c("black",
                             pal_npg("nrc")(9)[1],
                             pal_npg("nrc")(9)[2],
                             pal_npg("nrc")(9)[3])) +
  ggtitle("Direct (full colour) and indirect (transparent) emissions in 2030")

p.co2demand.incl.upstream

ggsave(paste0(out.path, "SI_fig16.pdf"), device=cairo_pdf,
       p.co2demand.incl.upstream,
       width=300,
       height=300,
       unit="mm",
       limitsize = FALSE,
       dpi=400)


# Part 4.3: Supplementary Figure 17 ====
p.wedge.upstream <- ggplot(data=df.wedge %>% filter(year>=2015,year<=2030),
                           aes(x=year)) +
  geom_ribbon(aes(ymin=Restore-Buildings,
                  ymax=Restore), fill=pal_npg("nrc", alpha = 0.7)(9)[1]) +
  geom_ribbon(aes(ymin=Restore-Transport,
                  ymax=Restore), fill=pal_npg("nrc", alpha = 0.7)(9)[2]) +
  geom_ribbon(aes(ymin=Restore-Transport-Industry,
                  ymax=Restore-Transport), fill=pal_npg("nrc", alpha = 0.7)(9)[3]) +
  geom_line(data=df.wedge %>% filter(year>=2015,year<=2020),aes(y=`Restore`), lwd=1, linetype='solid') +
  geom_line(data=df.wedge %>% filter(year>=2015,year<=2020),aes(y=`Green Push`), lwd=1, linetype='solid') +
  geom_line(data=df.wedge %>% filter(year>=2020,year<=2030),aes(y=`Restore`), lwd=1, linetype='dashed') +
  geom_line(data=df.wedge %>% filter(year>=2020,year<=2030),aes(y=`Green Push`), lwd=1, linetype='dashed') +
  
  # add labels in ribbons
  geom_text(x=2023,y=44500, label="Buildings", color=pal_npg("nrc", alpha = 1)(9)[1], fontface="bold") +
  geom_label(x=2025,y=43500, label="Transport", color="white", fill=pal_npg("nrc", alpha = 1)(9)[2], fontface="bold") +
  geom_text(x=2027,y=42500, label="Industry", color=pal_npg("nrc", alpha = 1)(9)[3], fontface="bold") +
  
  # add labels to lines
  geom_text(x=2027,y=45100, label="Restore", color="black") +
  geom_text(x=2029,y=43100, label="Green Push", color="black") +
  
  scale_color_npg() +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(limits = c(40000,46000), expand=c(0,0)) +
  labs(subtitle = "\n\nWithout additional climate policies") +
  ggtitle("CO2 emissions sectoral differences with and without additional climate policies") +
  xlab(NULL) +
  ylab("MtCO2/yr")

p.wedge.upstream

p.wedge.upstream.15 <- ggplot(data=df.wedge.15 %>% filter(year>=2015,year<=2030),
                              aes(x=year)) +
  geom_ribbon(aes(ymin=`Restore (Same climate policy 1.5)`-Buildings,
                  ymax=`Restore (Same climate policy 1.5)`), fill=pal_npg("nrc", alpha = 0.7)(9)[1]) +
  geom_ribbon(aes(ymin=`Restore (Same climate policy 1.5)`-Transport,
                  ymax=`Restore (Same climate policy 1.5)`), fill=pal_npg("nrc", alpha = 0.7)(9)[2]) +
  geom_ribbon(aes(ymin=`Restore (Same climate policy 1.5)`-Transport-Industry,
                  ymax=`Restore (Same climate policy 1.5)`-Transport), fill=pal_npg("nrc", alpha = 0.7)(9)[3]) +
  geom_line(data=df.wedge.15 %>% filter(year>=2015,year<=2020),aes(y=`Restore (Same climate policy 1.5)`), lwd=1, linetype='solid') +
  geom_line(data=df.wedge.15 %>% filter(year>=2015,year<=2020),aes(y=`Green Push (Same climate policy 1.5)`), lwd=1, linetype='solid') +
  geom_line(data=df.wedge.15 %>% filter(year>=2020,year<=2030),aes(y=`Restore (Same climate policy 1.5)`), lwd=1, linetype='dashed') +
  geom_line(data=df.wedge.15 %>% filter(year>=2020,year<=2030),aes(y=`Green Push (Same climate policy 1.5)`), lwd=1, linetype='dashed') +
  scale_color_npg() +
  theme_minimal() +
  
  # add labels in ribbons
  geom_text(x=2021,y=34000, label="Industry", color=pal_npg("nrc", alpha = 1)(9)[3], fontface="bold") +
  geom_label(x=2023,y=32000, label="Transport", color="white", fill=pal_npg("nrc", alpha = 1)(9)[2], fontface="bold") +
  geom_text(x=2025,y=30000, label="Buildings", color=pal_npg("nrc", alpha = 1)(9)[1], fontface="bold") +
  
  # add labels to lines
  geom_text(x=2029.2,y=22000, label="Restore (550)", color="black") +
  geom_text(x=2027,y=18000, label="Green Push (550)", color="black") +
  
  theme(#legend.position="none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
    panel.grid.minor.y = element_blank()
  ) +
  labs(subtitle = "\n\n1.5C") +
  xlab(NULL) +
  ylab("MtCO2/yr")

p.wedge.upstream.15

p.wedge <- ggarrange(
  p.wedge.upstream,
  p.wedge.upstream.15,
  ncol=1,
  nrow=2
)

p.wedge

ggsave(paste0(out.path, "SI_fig17.pdf"), device=cairo_pdf,
       p.wedge,
       width=250,
       height=250,
       unit="mm",
       limitsize = FALSE,
       dpi=400)


# Part 4.4: Supplementary Figure 19  ====
v <- "Emissions|CO2"
p.regional.emissions <- ggplot(df.cov.selected.regional %>% filter(variable==v),
                               aes(x=year, group=scenario)) +
  facet_grid(~region) +
  # add 0 and net-zero
  geom_segment(aes(x = 0, y = year.start, xend = 0, yend = year.end), colour = "black") +
  geom_rect(aes(xmin=year.start, xmax=year.end, ymin=-50000, ymax=0), fill="#ffebcd", alpha=0.08) +
  
  # add sr 15 timeseries category
  geom_ribbon(data=df.regional.sr15 %>% filter(variable==v), aes(ymin=min/1000, ymax=max/1000, fill=category), alpha=0.1) +
  geom_ribbon(data=df.regional.sr15 %>% filter(variable==v), aes(ymin=q25/1000, ymax=q75/1000, fill=category), alpha=0.3) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=min/1000, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=max/1000, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=med/1000, colour=category), linetype="dashed", size=1.5) +
  geom_point(data=df.regional.sr15 %>% filter(variable==v), aes(y=med/1000, colour=category), shape=21, size=3, fill="white", stroke = 1) +
  
  # add gdp uncertainty range
  geom_ribbon(data=gdp.regional %>% filter(variable==v) %>% filter(year>=2021), aes(ymin=min/1000, ymax=max/1000, group=model), fill='black', alpha=0.2) +
  
  # add covid scenarios
  geom_line(data= df.cov.selected.regional %>% filter(variable==v),aes(y=value/1000, colour=scenario), linetype="solid", size=1) +
  geom_point(data= df.cov.selected.regional %>% filter(variable==v) %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
             aes(y=value/1000, colour=scenario), shape=21, size=3, fill="white", stroke = 1) +
  
  coord_cartesian(xlim=c(year.start, year.end+2),
                  ylim=c(-2000/1000,24000/1000)) +
  
  ggtitle("Total CO2 emissions") +
  theme_classic() +
  scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  theme(legend.position = "none",
    axis.text.x = element_text(size=7),
    axis.text.y = element_text(size=10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
    panel.grid.minor.y = element_blank()
  ) +
  guides(color=guide_legend("Scenario"),
         fill=FALSE,
         shape=guide_legend("Scenario")) +
  xlab(NULL) +
  ylab("GtCO2/yr") +
  scale_x_continuous(expand = c(0,0), breaks = c(2020,2030)) +
  scale_y_continuous(expand = c(0,0))

v <- "Final Energy"
p.regional.fe <- ggplot(df.cov.selected.regional %>% filter(variable==v),
                        aes(x=year, group=scenario)) +
  facet_grid(~region) +
  
  # add sr 15 timeseries category
  geom_ribbon(data=df.regional.sr15 %>% filter(variable==v), aes(ymin=min, ymax=max, fill=category), alpha=0.1) +
  geom_ribbon(data=df.regional.sr15 %>% filter(variable==v), aes(ymin=q25, ymax=q75, fill=category), alpha=0.3) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=min, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=max, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable==v), aes(y=med, colour=category), linetype="dashed", size=1.5) +
  geom_point(data=df.regional.sr15 %>% filter(variable==v), aes(y=med, colour=category), shape=21, size=3, fill="white", stroke = 1) +
  
  # add gdp uncertainty range
  geom_ribbon(data=gdp.regional %>% filter(variable==v) %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model), fill='black', alpha=0.2) +
  
  # add covid scenarios
  geom_line(data= df.cov.selected.regional %>% filter(variable==v),aes(y=value, colour=scenario), linetype="solid", size=1) +
  geom_point(data= df.cov.selected.regional %>% filter(variable==v) %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
             aes(y=value, colour=scenario), shape=21, size=3, fill="white", stroke = 1) +
  
  coord_cartesian(xlim=c(year.start, year.end+2),
                  ylim=c(0,250)) +
  
  ggtitle("Total Final Energy") +
  theme_classic() +
  scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  theme(legend.position="none",
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
        panel.grid.minor.y = element_blank()
  ) +
  xlab(NULL) +
  ylab("EJ/yr") +
  scale_x_continuous(expand = c(0,0), breaks = c(2020,2030)) +
  scale_y_continuous(expand = c(0,0))

vs <- c("Final Energy|Residential and Commercial",
        "Final Energy|Transportation",
        "Final Energy|Industry")
vs.labs <- c("Buildings", "Transportation", "Industry")
names(vs.labs) <- vs
p.regional.fe.sectoral <- ggplot(df.cov.selected.regional %>% filter(variable%in%vs),
                                 aes(x=year, group=scenario)) +
  facet_grid(variable~region,
             labeller = labeller(variable = vs.labs)) +
  
  # add sr 15 timeseries category
  geom_ribbon(data=df.regional.sr15 %>% filter(variable%in%vs), aes(ymin=min, ymax=max, fill=category), alpha=0.1) +
  geom_ribbon(data=df.regional.sr15 %>% filter(variable%in%vs), aes(ymin=q25, ymax=q75, fill=category), alpha=0.3) +
  geom_line(data=df.regional.sr15 %>% filter(variable%in%vs), aes(y=min, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable%in%vs), aes(y=max, colour=category), size=0.1) +
  geom_line(data=df.regional.sr15 %>% filter(variable%in%vs), aes(y=med, colour=category), linetype="dashed", size=1.5) +
  geom_point(data=df.regional.sr15 %>% filter(variable%in%vs), aes(y=med, colour=category), shape=21, size=3, fill="white", stroke = 1) +
  
  # add gdp uncertainty range
  geom_ribbon(data=gdp.regional %>% filter(variable%in%vs) %>% filter(year>=2021), aes(ymin=min, ymax=max, group=model), fill='black', alpha=0.2) +
  
  # add covid scenarios
  geom_line(data= df.cov.selected.regional %>% filter(variable%in%vs),aes(y=value, colour=scenario), linetype="solid", size=1) +
  geom_point(data= df.cov.selected.regional %>% filter(variable%in%vs) %>% filter(year!=2019, year!=2021,year!=2022,year!=2023,year!=2024),
             aes(y=value, colour=scenario), shape=21, size=3, fill="white", stroke = 1) +
  
  coord_cartesian(xlim=c(year.start, year.end+2),
                  ylim=c(0,125)) +
  
  ggtitle("Sectoral Final Energy") +
  theme_classic() +
  scale_color_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  scale_fill_manual(values=c(mark_cols,"1.5C no or low OS"= "#1e9583", "Below 2.0C"="#63bce4","Above 2.0C"="#e78731"))+
  theme(legend.position="none",
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="darkgrey" ),
        panel.grid.minor.y = element_blank()
  ) +
  xlab(NULL) +
  ylab("EJ/yr") +
  scale_x_continuous(expand = c(0,0), breaks = c(2020,2030)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,25,50,75,100))

p.regional.emissions
p.regional.fe
p.regional.fe.sectoral

p.regional <- ggarrange(nrow = 3, ncol = 1, heights = c(1,1,3),
                        p.regional.emissions,
                        p.regional.fe,
                        p.regional.fe.sectoral)

p.regional

ggsave(paste0(out.path, "SI_fig19.pdf"), device=cairo_pdf,
       p.regional,
       width=200,
       height=300,
       unit="mm",
       dpi=400)




# Part 4.6: Supplementary Figure 20  ====
regions.for.polar.figure <- c(
  "World",
  "R5OECD",
  "R5MAF",
  "R5LAM",
  "R5ASIA",
  "R5REF"
)
radar.all <- produce_radar_data(dfcv = df.cov,
                                compare.scens = c("Restore", "Baseline-no-COVID", "Green Push"),
                                regions.to.plot=regions.for.polar.figure,
                                duration=duration)
p.radar.r5.15 <- make_radar_v2(radar.all,
                               scenarios=c("Green Push"),
                               regions = c("R5ASIA","R5LAM","R5MAF","R5OECD","R5REF")) +
  ylim(c(-25,16)) + theme(legend.position = "none")
p.radar.r5.15

ggsave(paste0(out.path, "SI_fig20.pdf"), device=cairo_pdf,
       p.radar.r5.15,
       width=250,
       height=250,
       unit="mm",
       limitsize = FALSE,
       dpi=400)

# Part 4.7: Supplementary Figure 21  ====
regions.for.polar.figure <- c(
  "World",
  "R5OECD",
  "R5MAF",
  "R5LAM",
  "R5ASIA",
  "R5REF"
)
radar.all <- produce_radar_data(dfcv = df.cov,
                                compare.scens = c("Restore", "Baseline-no-COVID", "Green Push"),
                                regions.to.plot=regions.for.polar.figure,
                                duration=duration)
p.radar.r5.20 <- make_radar_v2(radar.all,
                               scenario.ambition.level = "(1000)",
                               scenarios=c("Green Push"),
                               regions = c("R5ASIA","R5LAM","R5MAF","R5OECD","R5REF")) +
  ylim(c(-70,30)) + theme(legend.position = "none")
p.radar.r5.20 

ggsave(paste0(out.path, "SI_fig21.pdf"), device=cairo_pdf,
       p.radar.r5.20,
       width=250,
       height=250,
       unit="mm",
       limitsize = FALSE,
       dpi=400)

# Part 4.8: Supplementary Figure 22 ====
# Part 4.8.1: prepare 11-regional data for this plot ==== 
regions <- c("EEU", "FSU", "PAO", "WEU", "NAM","LAM", "SAS", "MEA", "PAS", "CPA", "AFR")
scens <- c("Smart Use (550)","Self-Reliance (550)","Restore (550)")
compare.s <- "Green Push (550)"
decarb.regional <- NULL
re.regional <- NULL
em.regional <- NULL
inv.regional <- NULL
for (reg in regions){
  c.scen <- compare.s
  comp.scen <- compare.s
  decarb.regional <- decarb.regional %>%
    bind_rows(calculate_decarb(dfl=df.cov, 
                               region.sel=reg, 
                               comp.scen = compare.s,
                               only.total=T) %>% mutate(R11.region=reg))
  rm(c.scen)
  rm(comp.scen)
  re.regional <- re.regional %>%
    bind_rows(df.cov %>% filter(variable=="Secondary Energy|Electricity|Solar-Wind share",year==2030,region==reg) %>% mutate(R11.region=reg))
  em.regional <- em.regional %>%
    bind_rows(df.cov %>% filter(variable=="Emissions|CO2",year==2030,region==reg) %>% mutate(R11.region=reg))
  inv.regional <- inv.regional %>%
    bind_rows(df.cov %>% filter(variable=="Investment|Energy Supply",year==2030,region==reg) %>% mutate(R11.region=reg))
}
message.R11 <- read_excel(paste0(base.path,data.file), sheet=f.MESSAGEregion) %>%
  mutate(iso = toupper(iso)) %>%
  rename(R11.region = `MESSAGE-GLOBIOM`) %>% select(-RCP_REG) %>%
  mutate(iso = ifelse(iso=="ROM", "ROU", iso))  # Typo fix for Romania

# Part 4.8.2: plot selected 11-regional data ====
for (s in scens){
  # 2025-240 decarbonization rate (negative diff is higher challenges compared to compare.s)
  map.decarb.15 <- message.R11 %>% left_join( decarb.regional %>% filter(scenario==s, variable=="Emissions|CO2"))
  map.decarb.15.compare <- message.R11 %>% left_join( decarb.regional %>% filter(scenario==compare.s, variable=="Emissions|CO2") %>% mutate(comp=decarb_pace_post_rec) %>% select(R11.region,comp))
  map.decarb.15 <- map.decarb.15 %>% left_join(map.decarb.15.compare) %>%
    mutate(diff=-1*(decarb_pace_post_rec-comp)) # go from emissions reduction to decarbonization pace
  print_map(map.decarb.15,
            "diff",
            -0.6,0.6,1/20,
            paste0("Additional decarbonization pace \nfor 2025-2040 for ", s),
            paste0("SI_fig22-", str_replace_all(str_replace_all(s,"[[:punct:]]","")," ","_"),"decarb"),
            colouring="YlGnBu")
  # solar+wind share (negative diff is higher challenges compared to compare.s)
  map.decarb.15 <- message.R11 %>% left_join( re.regional %>% filter(scenario==s, variable=="Secondary Energy|Electricity|Solar-Wind share"))
  map.decarb.15.compare <- message.R11 %>% left_join( re.regional %>% filter(scenario==compare.s, variable=="Secondary Energy|Electricity|Solar-Wind share") %>% mutate(comp=value) %>% select(R11.region,comp))
  map.decarb.15 <- map.decarb.15 %>% left_join(map.decarb.15.compare) %>%
    mutate(diff=value-comp)
  print_map(map.decarb.15,
            "diff",
            -0.5,0.5,1/20,
            paste0("Additional solar & wind share of electricity \nin 2030 for ", s),
            paste0("SI_fig22-", str_replace_all(str_replace_all(s,"[[:punct:]]","")," ","_"),"renewables"),
            colouring="YlGnBu")
  # annual co2 emissions
  map.decarb.15 <- message.R11 %>% left_join( em.regional %>% filter(scenario==s, variable=="Emissions|CO2"))
  map.decarb.15.compare <- message.R11 %>% left_join( em.regional %>% filter(scenario==compare.s, variable=="Emissions|CO2") %>% mutate(comp=value) %>% select(R11.region,comp))
  map.decarb.15 <- map.decarb.15 %>% left_join(map.decarb.15.compare) %>%
    mutate(diff=value-comp)
  print_map(map.decarb.15,
            "diff",
            -300,300,1/20,
            paste0("Additional CO2 emissions \nin 2030 for ", s),
            paste0("SI_fig22-", str_replace_all(str_replace_all(s,"[[:punct:]]","")," ","_"),"emissions"),
            colouring="YlGnBu")
  
  # annual energy investment
  map.decarb.15 <- message.R11 %>% left_join( inv.regional %>% filter(scenario==s, variable=="Investment|Energy Supply"))
  map.decarb.15.compare <- message.R11 %>% left_join( inv.regional %>% filter(scenario==compare.s, variable=="Investment|Energy Supply") %>% mutate(comp=value) %>% select(R11.region,comp))
  map.decarb.15 <- map.decarb.15 %>% left_join(map.decarb.15.compare) %>%
    mutate(diff=value-comp)
  print_map(map.decarb.15,
            "diff",
            -300,300,1/20,
            paste0("Additional energy supply investment \nin 2030 for ", s),
            paste0("SI_fig22-", str_replace_all(str_replace_all(s,"[[:punct:]]","")," ","_"),"investment"),
            colouring="YlGnBu")
  
}


# Part 4.9: Supplementary Figure 23 ====
R11.co2 <- df.cov %>% mutate(year=as.numeric(year)) %>%
  filter(region!="World", variable=="Emissions|CO2") %>%
  filter(!grepl('\\(',scenario,fixed=FALSE)) %>%
  filter(year>=2019, year<=2035)

p.r11.co2 <- ggplot(R11.co2,
                    aes(x=year, group=scenario)) +
  geom_line(aes(y=value, colour=scenario), linetype="solid", size=1.5) +
  geom_point(data= R11.co2 %>% filter(year!=2021,year!=2022,year!=2023,year!=2024),
             aes(y=value, colour=scenario), shape=21, size=3, fill="white", stroke = 1) +
  ggtitle("Regional CO2 emissions") +
  facet_wrap(~region, scales = "free") +
  theme_classic() +
  scale_color_manual(values=c(mark_cols))+
  scale_x_continuous(breaks = c(2020,2030)) +
  xlab(NULL) +
  ylab("MtCO2/yr") + theme(legend.position = "none")
p.r11.co2
ggsave(paste0(out.path, "SI_fig23.pdf"), device=cairo_pdf,
       p.r11.co2,
       width=300,
       height=200,
       unit="mm",
       dpi=400)

# Part 4.10: Supplementary Figure 24 ====
ghg.variables <- c("Emissions|BC",
                   "Emissions|OC",
                   "Emissions|CH4",
                   "Emissions|CO",
                   "Emissions|NOx",
                   "Emissions|CO2|AFOLU",
                   "Emissions|CO2|Energy and Industrial Processes",
                   "Emissions|HFC",
                   "Emissions|Sulfur",
                   "Emissions|SF6",
                   "Emissions|N2O",
                   "Emissions|VOC")
ghg.variable.names <- c("BC [Mt BC/yr]",
                        "OC [Mt OC/yr]",
                        "CH4 [Mt CH4/yr]",
                        "CO [Mt CO/yr]",
                        "NOx [Mt NOx/yr]",
                        "Land-use CO2 [Mt CO2/yr]",
                        "CO2 Energy and Industry [Mt CO2/yr]",
                        "HFC [kt HFC134a-eq/yr]",
                        "Sulfur [Mt SO2/yr]",
                        "SF6 [kt SF6/yr]",
                        "N2O [kt N2O/yr]",
                        "VOC [Mt VOC/yr]")

ghgs <- df.cov %>% mutate(year=as.numeric(year)) %>%
  filter(region=="World", variable %in% ghg.variables) %>%
  filter(!grepl('\\(',scenario,fixed=FALSE)) %>%
  filter(year>=2019, year<=2035)

i <- 0
for (g in ghg.variables){
  i <- i + 1
  ghgs <- ghgs %>%
    mutate(variable=ifelse(variable==g, ghg.variable.names[[i]], variable))
}


p.ghgs <- ggplot(ghgs,
                 aes(x=year, group=scenario)) +
  geom_line(aes(y=value, colour=scenario), linetype="solid", size=1.5) +
  geom_point(data= ghgs %>% filter(year!=2021,year!=2022,year!=2023,year!=2024),
             aes(y=value, colour=scenario), shape=21, size=3, fill="white", stroke = 1) +
  ggtitle("Global pathways of selected emissions") +
  facet_wrap(~variable, scales = "free") +
  theme_classic() +
  scale_color_manual(values=c(mark_cols))+
  xlab(NULL) +
  ylab(NULL) + theme(legend.position = "none")
p.ghgs
ggsave(paste0(out.path, "SI_fig24.pdf"), device=cairo_pdf,
       p.ghgs,
       width=320,
       height=200,
       unit="mm",
       dpi=500)

