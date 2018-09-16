

# Install these libraries (only do this once)
install.packages("ChannelAttribution")
install.packages("reshape")
install.packages("ggplot2")

# Load these libraries (every time you start RStudio)
library(ChannelAttribution)
library(reshape)
library(ggplot2)

# This loads the demo data. You can load your own data by importing a dataset or reading in a file
data(PathData)

H <- heuristic_models(Data, 'path', 'total_conversions', var_value='total_conversion_value')
M <- markov_model(Data, 'path', 'total_conversions', var_value='total_conversion_value', order = 1) 

R <- merge(H, M, by='channel_name') 

R1 <- R[, (colnames(R)%in%c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion'))]
colnames(R1) <- c('channel_name', 'first_touch', 'last_touch', 'linear_touch', 'markov_model')

R1 <- melt(R1, id='channel_name')

# Plot the total conversions
ggplot(R1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")
