install.packages("scales")

library("dplyr")
library("tidyr")
library("scales")
library("ggplot2")

comtrade1 <- read.csv("comtrade1.csv")
comtrade2 <- read.csv("comtrade2.csv")
comtrade3 <- read.csv("comtrade3.csv")

comtrade1 <- comtrade1 %>% 
  select(Year, Partner, Qty,Trade.Value..US.., Commodity)
comtrade2 <- comtrade2 %>% 
  select(Year, Partner, Qty, Trade.Value..US.., Commodity)
comtrade3 <- comtrade3 %>% 
  select(Year, Partner, Qty, Trade.Value..US.., Commodity)

comtrade <- full_join(comtrade1, comtrade2) %>% 
  full_join(comtrade3) %>% 
  mutate(Price = (Trade.Value..US.. / Qty)) %>% 
  group_by(Year, Partner) %>% 
  summarize(
    Trade.Value..US.. = sum(Trade.Value..US..),
    Qty = sum(Qty),
    Price = sum(Price)
  )
  
topTrade <- comtrade %>% 
  group_by(Partner) %>%
  summarize(
      periodTradeValue = sum(Trade.Value..US..)
  ) %>% 
  arrange(-periodTradeValue)

topTrade <- topTrade[2:11, ]
topTradePartners <- topTrade$Partner
period = unique(comtrade$Year)

comtrade <- filter(comtrade, Partner %in% topTradePartners) %>% 
  rename("tradeValue" = Trade.Value..US..)

# A1) Graph total value of trade over this period by each trading partner
# Q:Should I plot total value over this period, or value by year?
# Q: Is it necessary to have plots for each type of lumber, or is it ok to aggregate?
# Q: Should I express trade value logarithmically?
# Q: How to define top trading partners? I defined it as top 10 with greatest trade value
totalTradeValuePlot <- ggplot(topTrade) +
  geom_col(aes(x = reorder(Partner, -periodTradeValue), y = periodTradeValue)) +
  labs(title = "Total Value of Lumber Imports by Trading Partner, 1997-2011",
         x = "Trading Partner", 
         y = "Trade Value (Billions of USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))

totalTradeValuePlot

# A2) Trade value by year
annualTradeValuePlot <- ggplot(comtrade) +
  geom_line(aes(x = Year, y = tradeValue, color = Partner)) + 
  scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Annual Value of Lumber Imports by Trading Partner",
       y = "Value of Imports (in Billions of USD)")
  
annualTradeValuePlot

# B) Trade Quantity

totalTradeQty <- ggplot(comtrade) +
  geom_line(aes(x = Year, y = Qty, color = Partner)) + 
  scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Quantity of Lumber Imports by Trading Partner",
       y = "Quantity of Imports (in Cubic Meters)")

totalTradeQty

# C) Price
pricePlot <- ggplot(comtrade) +
  geom_line(aes(x = Year, y = Price, color = Partner)) + 
  #scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Price of Lumber Imports by Trading Partner",
       y = "Price of One Cubic Meter of Lumber (in USD)")

pricePlot

#########################
# Canada-specific plots #
#########################
canada_data <- comtrade %>% 
  filter(Partner == "Canada")

# To get separate line for each type of lumber, geom_line(aes(color = Commodity))
canada_data$Commodity[canada_data$Commodity == "Lumber, coniferous (softwood) thickness < 6 mm"] <- 
"Lumber"
canada_data$Commodity[canada_data$Commodity == "Plywood, all softwood, each ply < 6mm thick"] <- 
  "Plywood"
canada_data$Commodity[canada_data$Commodity == "Veneer or ply sheet, coniferous (softwood) <6 mm thick"] <-
  "Veneer"
canada_data$Commodity[canada_data$Commodity == "Plywood consisting solely of sheets of wood, each ply not >6mm thkns. (excl ..."] <- 
  "Plywood"
canada_data$Commodity[canada_data$Commodity == "Sheets for veneering, incl. those obt. by slicing laminated wood, for plywo ..."] <-
  "Veneer"
canada_data$Commodity[canada_data$Commodity == "Wood sawn/chipped length wise, sliced/peeled, whether or not planed, sanded ..."] <-
  "Wood chipped/sawn"
canada_data$Commodity[canada_data$Commodity == "Sheets for veneering (including those obtained by slicing laminated wood), for plywood/for similar laminated wood & other wood, sawn lengthwise, sliced/peeled, whether/not planed, sanded, spliced/end-jointed, of a thickness not> 6 mm,coniferous"] <-
  "Veneer"
canada_data$Commodity[canada_data$Commodity == "Wood sawn/chipped length wise, sliced/peeled, whether/not planed, sanded/end-jointed, of a thickness >6mm, coniferous"] <-
  "Wood chipped/sawn"

# A) Trade value by year
canadaTradeValuePlot <- ggplot(canada_data) +
  geom_line(aes(x = Year, y = tradeValue, color = Partner)) + 
  scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Annual Value of Lumber Imports from Canada",
       y = "Value of Imports (in Billions of USD)") #+
  #geom_vline(xintercept = 2001)

canadaTradeValuePlot

# B) Trade Quantity

canadaTradeQty <- ggplot(canada_data) +
  geom_line(aes(x = Year, y = Qty, color = Partner)) + 
  scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Quantity of Canadian Lumber Imports",
       y = "Quantity of Imports (in Cubic Meters)") #+
  #geom_vline(xintercept = 2001)

canadaTradeQty

# C) Price
canadaPricePlot <- ggplot(canada_data) +
  geom_line(aes(x = Year, y = Price, color = Partner)) + 
  #scale_y_log10(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_x_continuous(breaks = period) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Price of Canadian Lumber Imports to US",
       y = "Price of One Cubic Meter of Lumber (in USD)")

canadaPricePlot
