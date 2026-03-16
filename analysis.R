# =============================================================================
# Unequal Beginnings: Spatial Inequality and Structural Determinants of
# Infant Mortality in Türkiye
# =============================================================================
# Author:  Arda Sarıtaş
# Data:    TUIK (Turkish Statistical Institute), WHO Global Health Observatory
# Period:  2009–2023 | 81 provinces
# =============================================================================

# Install dependencies (run once) ---------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
# install.packages("forcats")
# install.packages("skimr")
# install.packages("moments")
# install.packages("knitr")
# install.packages("kableExtra")
# install.packages("ggridges")
# install.packages("patchwork")

# WD setup
setwd("data")  # run script from repo root; data files are in data/
getwd()
file.exists("tuik_i5mr_raw.csv")

# Used Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(reshape2)
library(forcats)
library(skimr)
library(moments)

################################# U5MR #########################################
# Read the data
df_raw <- read.csv("tuik_i5mr_raw.csv", header = FALSE)

# Extract row 5 which contains prefix
row5 <- df_raw[5, 1]

# Remove the prefix "Beş Yaş Altı Ölüm Hızı (Binde)|Ölçüm bazında|"
clean_row5 <- sub("^Beş Yaş Altı Ölüm Hızı \\(Binde\\)\\|Ölçüm bazında\\|", "", row5)

# We put || to retain original format
final_row5 <- paste0("||", clean_row5)

# Replace row 5 in the df with the final
df_raw[5, 1] <- final_row5

# Extract the province names
province_row <- unlist(strsplit(df_raw[2, 1], "\\|"))

# Removes "Satırlar", "", ""
province_names <- province_row[-c(1, 2, 3)]

# Remove metadata to obtain a df with only the data
df_data_only <- df_raw[-c(1, 2, 3, 4, 20), , drop = FALSE]

# Remove leading and trailing whitespace from all rows
df_data <- gsub("^\\s+|\\s+$", "", df_data_only$V1)

# Process the data
split_data <- strsplit(df_data, "\\|")
split_cleaned <- lapply(split_data, function(row) row[-c(1, 2)])
df_split <- do.call(rbind, split_cleaned) %>% as.data.frame(stringsAsFactors = FALSE)

# Rename the columns
colnames(df_split) <- c("year", province_names)

# Pivot to longer format
df_long <- df_split %>%
  pivot_longer(cols = -year, names_to = "province_id", values_to = "U5MR")

# Add a clean Province name by removing suffixes of ids
df_long <- df_long %>%
  mutate(
    year = as.integer(year),
    U5MR = as.numeric(U5MR),
    province = sub("-\\d+$", "", province_id)
  )

# Lastly, fix the ordering of columns
df_long <- df_long %>%
  select(year, province_id, province, U5MR)

u5mr_province_year <- df_long

# Cleaned long-formatted data that shows provinces' u5mr over years 2009 - 2023:
u5mr_province_year # with id
u5mr_province_year <- u5mr_province_year %>% select(year, province, U5MR)

# Filter for a few provinces for readability
selected_provinces <- c("İstanbul", "Ankara", "İzmir", "Hatay", "Diyarbakır", "Urfa")

# Example Plot
u5mr_province_year %>%
  filter(province %in% selected_provinces) %>%
  ggplot(aes(x = year, y = U5MR, color = province, group = province)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Under-5 Mortality Rate Trends by Province (2009–2023)",
    x = "Year",
    y = "U5MR (per 1000 live births)",
    color = "Province"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )


########################### GDP PER CAPITA #####################################
gdp <- read_excel("gpd_per_capita_province.xls")

# Extract the Turkey GDP for later use
gdp_turkey <- gdp %>% select(`23.04.2025`)

# Process the gdp_turkey data

# Omit NA's
gdp_turkey <- gdp_turkey %>% drop_na()

# Change column name
colnames(gdp_turkey) <- c("Türkiye")

# Remove the first row which is "Türkiye"
gdp_turkey <- gdp_turkey[-1, ]

# Refactor the gdp_turkey data as necessary
gdp_turkey <- gdp_turkey %>%
  mutate(
    year = as.character(seq(2009, 2023, 1)),
    Türkiye = as.numeric(Türkiye)
  )
gdp_turkey <- gdp_turkey[ , c("year", "Türkiye")]
gdp_turkey

# Clean and structure the gdp data

# Remove non-data fields
gdp <- gdp[-c(1, 2),  -c(2, 3)]
gdp <- gdp[, -5]
gdp.provinces <- gdp[1, -1]

# Change the column names into year and provinces
colnames(gdp) <- c("year", gdp.provinces)
gdp <- gdp[-1, ]

# Refactor gdp data to be used for plotting
gdp <- gdp %>%
  mutate(
    year = as.integer(year),
    across(-year, ~ as.numeric(.))
    )

# Long format
gdp_long <- gdp %>%
  pivot_longer(cols = -year, names_to = "province", values_to = "gdp")

# For convenience
selected_provinces <- c("İstanbul", "Ankara", "İzmir")

# Example plot
gdp_long %>%
  filter(province %in% selected_provinces) %>%
  ggplot(aes(x = year, y = gdp, color = province)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "GDP Per Capita Trends by Province (2009–2023)",
    x = "Year",
    y = "GDP Per Capita",
    color = "Province"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )


############ Join GPD per Capita and U5MR for correlation analysis #############


gdp_u5mr <- inner_join(u5mr_province_year, gdp_long, by = c("year", "province"))

# For convenience
selected_provinces <- c("İstanbul", "Ankara", "İzmir", "Hatay")

# Correlation, example plot
gdp_u5mr %>%
  filter(province %in% selected_provinces) %>%
  ggplot(aes(x = gdp, y = U5MR)) +
  geom_point(aes(color = province), alpha = 0.7) +

  labs(
    title = "Correlation Between GDP Per Capita and U5MR",
    x = "GDP per Capita",
    y = "Under-5 Mortality Rate"
  ) +
  theme_minimal()

# Linear fitted scatterplot that shows correlation
ggplot(gdp_u5mr, aes(x = gdp, y = U5MR)) +
  geom_point(aes(color = province), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #facet_wrap(~ year) +
  labs(
    title = "Correlation Between GDP Per Capita and U5MR",
    x = "GDP per Capita",
    y = "Under-5 Mortality Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


################################## IMR #########################################
imr <- read_excel("tuik_imr.xls")

# Extract the Turkey GDP for later use
imr_turkey <- imr %>% select(`23.04.2025`)

# Process the gdp_turkey data
imr_turkey <- imr_turkey %>% drop_na()
colnames(imr_turkey) <- c("Türkiye")
imr_turkey <- imr_turkey[-1, ]
imr_turkey <- imr_turkey %>%
  mutate(
    Türkiye = gsub(",", ".", Türkiye),
    Türkiye = as.numeric(Türkiye),
    year = seq(2009, 2023, 1)
  )

# Re-order the columns
imr_turkey <- imr_turkey[ , c("year", "Türkiye")]

# Clean and structure the imr data, same as gdp
imr <- imr[-c(1, 2),  -c(2, 3)]
imr <- imr[, -5]
imr.provinces <- imr[1, -1]
colnames(imr) <- c("year", imr.provinces)
imr <- imr[-1, ]
imr <- imr %>%
  mutate(
    year = as.integer(year),
    across(-year, ~ as.numeric(gsub(",", ".", .)))
  )

# Long format
imr_long <- imr %>%
  pivot_longer(cols = -year, names_to = "province", values_to = "imr")


# For convenience
selected_provinces <- c("İstanbul", "Ankara", "İzmir", "Hatay")

# Example IMR line plot
imr_long %>%
  filter(province %in% selected_provinces) %>%
  ggplot(aes(x = year, y = imr, color = province, group = province)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "IMR Trends by Province (2009–2023)",
    x = "Year",
    y = "IMR Per Capita",
    color = "Province"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )


########### Join GPD per Capita and IMR for correlation analysis ###############

gdp_imr <- inner_join(imr_long, gdp_long, by = c("year", "province"))
imr_u5mr <- inner_join(imr_long, u5mr_province_year, by = c("year", "province"))
imr_u5mr
# Log fitted scatterplot that shows correlation
gdp_imr %>%
  ggplot(aes(x = gdp, y = imr)) +
  geom_point(aes(color = province), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "black") +
  labs(
    title = "GDP vs IMR with Log Fit",
    x = "GDP Per Capita",
    y = "IMR"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# This is a heat map of correlation which I did not put in the docs
heat_data <- gdp_u5mr %>%
  group_by(year) %>%
  summarize(correlation = cor(gdp, U5MR, use = "complete.obs"))

ggplot(heat_data, aes(x = year, y = 1, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Yearly GDP–U5MR Correlation", fill = "Correlation") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

# GDP Per Capita Distribution
gdp_long %>%
  ggplot(aes(x = factor(year), y = gdp)) +
  geom_boxplot() +
  labs(title = "GDP Per Capita Distribution by Year", x = "Year", y = "GDP") +
  theme_minimal()

# Province-Year Heatmap of IMR, seems a bit daunting
imr_long %>%
  ggplot(aes(x = factor(year), y = fct_reorder(province, imr, .fun = mean, .desc = TRUE), fill = imr)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "IMR", option = "D") +
  labs(
    title = "Province-Year Heatmap of Infant Mortality Rate (IMR)",
    x = "Year",
    y = "Province"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

# Make sure 'gdp_imr' is your merged dataset with columns: year, province, gdp, imr, still a bit daunting
gdp_imr %>%
  ggplot(aes(x = factor(year), y = fct_reorder(province, gdp, .fun = mean, .desc = TRUE), fill = gdp)) +
  geom_tile(color = "white") +
  # Optional: overlay IMR as text inside tiles
  geom_text(aes(label = round(imr, 1)), size = 2, color = "black") +
  scale_fill_viridis_c(name = "GDP", option = "C") +
  labs(
    title = "GDP Heatmap by Province and Year (with IMR Labels)",
    x = "Year",
    y = "Province"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )


# Example for a selected province, e.g., Istanbul
gdp_imr %>%
  filter(province == "İstanbul") %>%
  ggplot(aes(x = factor(year), y = imr, fill = gdp)) +
  geom_col() +
  scale_fill_viridis_c(name = "GDP per Capita", option = "C") +
  labs(
    title = "İstanbul: IMR Over Time Colored by GDP",
    x = "Year",
    y = "Infant Mortality Rate (IMR)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# IMR over time by province colored by gdp, facets include all the provinces
gdp_imr %>%
  ggplot(aes(x = factor(year), y = imr, fill = gdp)) +
  geom_col() +
  scale_fill_viridis_c(name = "GDP per Capita (×1000 $)", option = "C") +
  labs(
    x = "Year",
    y = "Infant Mortality Rate (IMR)",
    color = "GDP per Capita (1000$)"
  ) +
  facet_wrap(~ province, scales = "free_y") +
theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

######################## Bed Count per 100k ####################################
bed <- read_excel("bed_count_100k.xls")

# Extract the Turkey GDP for later use
bed_turkey <- bed %>% select(`23.04.2025`)

# Process the bed_turkey data
bed_turkey <- bed_turkey %>% drop_na()
colnames(bed_turkey) <- c("Türkiye")
bed_turkey <- bed_turkey[-1, ]
bed_turkey <- bed_turkey %>%
  mutate(
    Türkiye = gsub(",", ".", Türkiye),
    Türkiye = as.numeric(Türkiye),
    year = seq(2009, 2023, 1)
  )

# Columns
bed_turkey <- bed_turkey[ , c("year", "Türkiye")]
bed_turkey <- bed_turkey %>%
  mutate(Türkiye = Türkiye / 100)

# Clean and structure the gdp data
bed <- bed[-c(1, 2),  -c(2, 3)]
bed <- bed[, -5]
bed.provinces <- bed[1, -1]
colnames(bed) <- c("year", bed.provinces)
bed <- bed[-1, ]
bed <- bed %>%
  mutate(
    year = as.integer(year),
    across(-year, ~ as.numeric(gsub(",", ".", .)))
  )

# Long format
bed_long <- bed %>%
  pivot_longer(cols = -year, names_to = "province", values_to = "bedCount")

# Convert hospital beds per 100,000 population to per 1,000 for matching other data
bed_long <- bed_long %>%
  mutate(bedCount = bedCount / 100)
bed_1k <- bed %>%
  mutate(across(-year, ~ . / 100))

##### Join Bed Count over a thousand people and IMR for correlation analysis ###
bed_imr <- inner_join(imr_long, bed_long, by = c("year", "province"))

# Plot the correlation between bed count and IMR, log fitted again
ggplot(bed_imr, aes(x = bedCount, y = imr)) +
  geom_point(aes(color = province), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "black") +
  labs(
    title = "Logarithmic Fit: Hospital Beds vs IMR",
    x = "Hospital Beds per 1000 People",
    y = "Infant Mortality Rate (IMR)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# Unusable line plot
ggplot(bed_imr, aes(x = year, y = imr, group = province, color = bedCount)) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "IMR Over Time by Province (Colored by Hospital Beds per 1000)",
    x = "Year",
    y = "Infant Mortality Rate (IMR)",
    color = "Beds per 1k"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# Infant Mortality Rate by Province and Year (Fill = Beds per 1k)
ggplot(bed_imr, aes(x = factor(year), y = imr, fill = bedCount)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_c(name = "Beds per 1000 people", option = "plasma") +
  facet_wrap(~ province, scales = "free_y") +
  labs(
    x = "Year",
    y = "IMR",
    fill = "Bed Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

########################### Global Data ########################################

# Read the WHO neonatal mortality data
who_data <- read_csv("A4C49D3_ALL_LATEST.csv")

# Select countries to compare
countries <- c("Türkiye", "Germany", "France", "Italy", "Spain",
               "Poland", "Sweden", "Norway")

# Filter and clean
who_filtered <- who_data %>%
  filter(GEO_NAME_SHORT %in% countries,
         DIM_TIME >= 2009,
         IND_NAME == "Mortality rate (neonetal)") %>%
  select(year = DIM_TIME, country = GEO_NAME_SHORT, mortality = RATE_PER_1000_N)

# Wide format for tables/plots
who_wide <- who_filtered %>%
  pivot_wider(names_from = country, values_from = mortality)

# Order by year
who_wide <- who_wide %>%
  arrange(year)

# Reorder so Türkiye is first
who_wide <- who_wide %>%
  select(year, Türkiye, everything())

who_wide <- who_wide %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

########################### Regional Data ######################################

# Region mapping for each province
province_region <- c(
  "Adana" = "Mediterranean", "Adıyaman" = "Southeast Anatolia", "Afyonkarahisar" = "Aegean", "Ağrı" = "Eastern Anatolia",
  "Aksaray" = "Central Anatolia", "Amasya" = "Black Sea", "Ankara" = "Central Anatolia", "Antalya" = "Mediterranean",
  "Ardahan" = "Eastern Anatolia", "Artvin" = "Black Sea", "Aydın" = "Aegean", "Balıkesir" = "Marmara",
  "Bartın" = "Black Sea", "Batman" = "Southeast Anatolia", "Bayburt" = "Eastern Anatolia", "Bilecik" = "Marmara",
  "Bingöl" = "Eastern Anatolia", "Bitlis" = "Eastern Anatolia", "Bolu" = "Black Sea", "Burdur" = "Mediterranean",
  "Bursa" = "Marmara", "Çanakkale" = "Marmara", "Çankırı" = "Central Anatolia", "Çorum" = "Black Sea",
  "Denizli" = "Aegean", "Diyarbakır" = "Southeast Anatolia", "Düzce" = "Black Sea", "Edirne" = "Marmara",
  "Elazığ" = "Eastern Anatolia", "Erzincan" = "Eastern Anatolia", "Erzurum" = "Eastern Anatolia", "Eskişehir" = "Central Anatolia",
  "Gaziantep" = "Southeast Anatolia", "Giresun" = "Black Sea", "Gümüşhane" = "Black Sea", "Hakkari" = "Southeast Anatolia",
  "Hatay" = "Mediterranean", "Iğdır" = "Eastern Anatolia", "Isparta" = "Mediterranean", "İstanbul" = "Marmara",
  "İzmir" = "Aegean", "Kahramanmaraş" = "Mediterranean", "Karabük" = "Black Sea", "Karaman" = "Central Anatolia",
  "Kars" = "Eastern Anatolia", "Kastamonu" = "Black Sea", "Kayseri" = "Central Anatolia", "Kırıkkale" = "Central Anatolia",
  "Kırklareli" = "Marmara", "Kırşehir" = "Central Anatolia", "Kilis" = "Southeast Anatolia", "Kocaeli" = "Marmara",
  "Konya" = "Central Anatolia", "Kütahya" = "Aegean", "Malatya" = "Eastern Anatolia", "Manisa" = "Aegean",
  "Mardin" = "Southeast Anatolia", "Mersin" = "Mediterranean", "Muğla" = "Aegean", "Muş" = "Eastern Anatolia",
  "Nevşehir" = "Central Anatolia", "Niğde" = "Central Anatolia", "Ordu" = "Black Sea", "Osmaniye" = "Mediterranean",
  "Rize" = "Black Sea", "Sakarya" = "Marmara", "Samsun" = "Black Sea", "Siirt" = "Southeast Anatolia",
  "Sinop" = "Black Sea", "Sivas" = "Central Anatolia", "Şanlıurfa" = "Southeast Anatolia", "Şırnak" = "Southeast Anatolia",
  "Tekirdağ" = "Marmara", "Tokat" = "Black Sea", "Trabzon" = "Black Sea", "Tunceli" = "Eastern Anatolia",
  "Uşak" = "Aegean", "Van" = "Eastern Anatolia", "Yalova" = "Marmara", "Yozgat" = "Central Anatolia",
  "Zonguldak" = "Black Sea"
)

# Add a region column to the imr data
imr_region <- imr_long %>%
  mutate(region = province_region[province])

# Get a long-formatted data w/ columns region year and imr
imr_region_year <- imr_region %>%
  group_by(region, year) %>%
  summarize(imr = mean(imr, na.rm = TRUE), .groups = "drop")

# Regional Trends of IMR
ggplot(imr_region_year, aes(x = year, y = imr, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "IMR (per 1,000 live births)",
    color = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )

# Add a region column to the gdp data
gdp_region <- gdp_long %>%
  mutate(region = province_region[province])

# Get a long-formatted data w/ columns region year and gdp
gdp_region_year <- gdp_region %>%
  group_by(region, year) %>%
  summarize(gdp = mean(gdp, na.rm = TRUE), .groups = "drop")
gdp_region_year

# Regional Trends of GDP per Capita
ggplot(gdp_region_year, aes(x = year, y = gdp, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Regional Trends in Gross Domestic Product Per Capita(GDP Per Capita)",
    x = "Year",
    y = "GDP Per Capita ($1k)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

bed_region
# Add a region column to the bed data
bed_region <- bed_long %>%
  mutate(region = province_region[province])
bed_region$bedCount <- as.numeric(bed_region$bedCount)

# Get a long-formatted data w/ columns region year and gdp
bed_region_year <- bed_region %>%
  group_by(region, year) %>%
  summarize(bed_1k = mean(bedCount, na.rm = TRUE), .groups = "drop")

# Regional Trends of Hospital Bed Count
ggplot(bed_region_year, aes(x = year, y = bed_1k, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "Hospital Beds per 1,000 People",
    color = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )

############################## STATISTICS ######################################

# A function i wrote to get the statistical data for our data
get_stats_by_year <- function(df, value_col) {
  df %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      # n = sum(!is.na(.data[[value_col]])),
      min = round(min(.data[[value_col]], na.rm = TRUE), 2),
      Q1 = round(quantile(.data[[value_col]], probs = 0.25, na.rm = TRUE), 2),
      mean = round(mean(.data[[value_col]], na.rm = TRUE), 2),
      # trimmed_mean = round(mean(.data[[value_col]], trim = 0.1, na.rm = TRUE), 2),
      median = round(median(.data[[value_col]], na.rm = TRUE), 2),
      Q3 = round(quantile(.data[[value_col]], probs = 0.75, na.rm = TRUE), 2),
      max = round(max(.data[[value_col]], na.rm = TRUE), 2),
      std_dev = round(sd(.data[[value_col]], na.rm = TRUE), 2),
      skewness = round(moments::skewness(.data[[value_col]], na.rm = TRUE), 2),
      kurtosis = round(moments::kurtosis(.data[[value_col]], na.rm = TRUE), 2)
    )
}

# Application of the function
stats_u5mr <- get_stats_by_year(u5mr_province_year, "U5MR")
stats_imr  <- get_stats_by_year(imr_long, "imr")
stats_gdp  <- get_stats_by_year(gdp_long, "gdp")
stats_bed  <- get_stats_by_year(bed_long, "bedCount") # or "bed_1k" if scaled

# To show the statistics
stats_u5mr
stats_imr
stats_gdp
stats_bed

library(knitr)
library(kableExtra)


# Create styled table
table_imr <- who_wide%>%
  kable("latex", booktabs = TRUE) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    font_size = 10
  ) %>%
  row_spec(0, bold = TRUE)
table_imr


## Distribution of IMR accross provinces
library(ggridges)
ggplot(imr_long, aes(x = imr, y = factor(year))) +
  geom_density_ridges(fill = "skyblue", alpha = 0.6) +
  labs(title = "Distribution of IMR Across Provinces by Year",
       x = "Infant Mortality Rate", y = "Year") +
  theme_minimal()

##
library(ggridges)

# Distribution holistic
ggplot(imr_region, aes(x = imr, y = factor(year), fill = region)) +
  geom_density_ridges(alpha = 0.7, scale = 3, rel_min_height = 0.01) +
  labs(
    x = "IMR",
    y = "Year",
    fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )

# Regional Distributions of IMR Across Years
ggplot(imr_region, aes(x = imr, y = factor(year), fill = region)) +
  geom_density_ridges(alpha = 0.6) +
  facet_wrap(~ region) +  # Keep facet labels for accessibility
  labs(
    x = "IMR (per 1,000 live births)",
    y = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )




imr_long
imr_region
imr_region_year


# IMR Trends Across Provinces by Region
ggplot(imr_region, aes(x = year, y = imr, group = province, color = region)) +
  geom_line(alpha = 0.4) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black", size = 1.2) +
  labs(
       x = "Year", y = "IMR (per 1,000 live births)") +
  theme_minimal()


# IMR 2023 horizontal col plot
imr_latest <- filter(imr_region, year == 2023)

ggplot(imr_latest, aes(x = reorder(province, imr), y = imr)) +
  geom_col() +
  coord_flip() +
  labs( x = "Province", y = "IMR (per 1,000 live births)") +
  theme_minimal()

# Regional IMR
ggplot(imr_region_year, aes(x = year, y = imr, color = region)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Regional IMR Trends in Türkiye (2009–2023)",
       x = "Year", y = "Mean IMR (per 1,000 live births)") +
  theme_minimal()


# Overlaid area
ggplot(bed_region_year, aes(x = year, y = bed_1k, fill = region)) +
  geom_area(position = "identity") +
  labs(
       x = "Year", y = "Beds per 1,000 People") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )
bed_region_year

library(ggplot2)
library(scales)  # for formatting currency if needed

# title = "GDP Per Capita Distribution Across Provinces (2009–2023)",
gdp_long %>%
  ggplot(aes(x = factor(year), y = gdp)) +
  geom_boxplot(fill = "#4682B4", color = "black", outlier.shape = 16, outlier.size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "darkred") +
  scale_y_continuous(labels = comma) +  # Adds comma formatting to y-axis
  labs(
    x = "Year",
    y = "GDP per Capita (x1000$)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 11)
  )


# Calculate mean IMR for each province across all years
imr_avg_province <- imr_long %>%
  group_by(province) %>%
  summarize(mean_imr = mean(imr, na.rm = TRUE)) %>%
  arrange(mean_imr)

# Top 10 provinces with lowest average IMR (best overall)
top10_best <- imr_avg_province %>%
  slice(1:10)

# Top 10 provinces with highest average IMR (worst overall)
top10_worst <- imr_avg_province %>%
  slice_tail(n = 10)



library(patchwork)

# Top 10 Best
p1 <- ggplot(top10_best, aes(x = reorder(province, mean_imr), y = mean_imr)) +
  geom_segment(aes(xend = province, y = 0, yend = mean_imr), color = "lightgray") +
  geom_point(size = 3, color = "forestgreen") +
  labs(
       x = NULL, y = "IMR") +
  theme_minimal()
p1
# Top 10 Worst
p2 <- ggplot(top10_worst, aes(x = reorder(province, mean_imr), y = mean_imr)) +
  geom_segment(aes(xend = province, y = 0, yend = mean_imr), color = "lightgray") +
  geom_point(size = 3, color = "firebrick") +
  labs(
       x = NULL, y = "IMR") +
  theme_minimal()

p1 / p2  # This stacks p1 on top of p2

who_wide

library(tidyverse)

who_long <- who_wide %>%
  pivot_longer(cols = -year, names_to = "country", values_to = "neonatal_mortality")

library(tidyverse)

# Filter the 2022 data
who_2022 <- who_oecd %>% filter(year == 2022)

# Convert to long format
who_2022_long <- who_2022 %>%
  pivot_longer(cols = -year, names_to = "country", values_to = "neonatal_mortality")

# Plot horizontal bar chart
ggplot(who_long, aes(x = reorder(country, neonatal_mortality), y = neonatal_mortality)) +
  geom_col(fill = "darkgray") +
  coord_flip() +
  labs(
    x = "",
    y = " Total Neonatal Mortality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(hjust = 1),
    axis.text = element_text(size = 11)
  )

# Filter for Türkiye and sum neonatal mortality across years
who_long %>%
  filter(year == 2022)


# Create a 2023 row
who_2023 <- tibble(
  year = 2023,
  Türkiye = 5.00,
  Poland = 2.69,
  France = 2.54,
  Germany = 2.21,
  Italy = 1.57,
  Spain = 1.72,
  Norway = 1.29,
  Sweden = 1.39
)

# Append to existing dataset
who_all <- bind_rows(who_long, who_2023)

who_long <- who_all %>%
  pivot_longer(cols = -year, names_to = "country", values_to = "neonatal_mortality")


who_cumulative <- who_long %>%
  group_by(country) %>%
  summarize(total_mortality = sum(neonatal_mortality, na.rm = TRUE))

ggplot(who_cumulative, aes(x = reorder(country, total_mortality), y = total_mortality)) +
  geom_col(fill = "darkgray") +
  coord_flip() +
  labs(
    x = "",
    y = "Total Neonatal Mortality Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(hjust = 1),
    axis.text = element_text(size = 11)
  )

ggplot(imr_u5mr, aes(x = imr, y = U5MR)) +
  geom_col() +
  labs(
    title = "National Trends in IMR and U5MR Over Time",
    x = "IMR", y = "U5MR"
  ) +
  theme_minimal()

cor(imr_u5mr$imr, imr_u5mr$U5MR, method = "pearson")

# title = "Correlation Between Infant Mortality Rate (IMR) and Under-5 Mortality Rate (U5MR)",
ggplot(imr_u5mr, aes(x = imr, y = U5MR)) +
  geom_point(color = "#2C77B8", size = 2.5, alpha = 0.8) +  # Slight transparency for polish
  geom_smooth(method = "lm", se = FALSE, color = "#D62828", size = 1) +
  labs(
    x = "Infant Mortality Rate (per 1,000 live births)",
    y = "Under-5 Mortality Rate (per 1,000 live births)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )


# Calculate Pearson correlation
r_value <- cor(imr_u5mr$imr, imr_u5mr$U5MR, method = "pearson")
r_value

# Format to 2 decimal places
r_label <- paste0("Pearson r = ", round(r_value, 2))

ggplot(imr_u5mr, aes(x = imr, y = U5MR)) +
  geom_point(color = "#2C77B8", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#D62828", size = 1) +
  annotate("text", x = max(imr_u5mr$imr) * 0.7, y = max(imr_u5mr$U5MR) * 0.9,
           label = r_label, size = 4.5, fontface = "italic") +
  labs(
    x = "Infant Mortality Rate (per 1,000 live births)",
    y = "Under-5 Mortality Rate (per 1,000 live births)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(size = 12)
  )

