


test_0001 <- fromJSON("/Users/lnorth/Desktop/pywatershed-SA-UA-main/Projects/day_EastRiv/obs/SNOTEL/NRCS_data_list.json")

df_0001 <- test_0001$CO$`380`

df_0001$CO$`380`$Date <- as.Date(df_0001$CO$`380`$Date)


max_wteq_per_year <- df_0001 %>%
  group_by(waterYear) %>%
  summarise(max_WTEQ = max(WTEQ, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Plot using ggplot
ggplot(max_wteq_per_year, aes(x = waterYear, y = max_WTEQ)) +
  geom_line(color = "blue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Maximum WTEQ by Water Year",
    x = "Water Year",
    y = "Max WTEQ"
  ) +
  theme_minimal()
