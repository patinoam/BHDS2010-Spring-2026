# The lines below are run if the packages were not yet been installed.
# Otherwise,they are commented out so the code is not executed.
# install.packages("pastecs")
# install.packages("reshape2")
# install.packages("tidyverse")

# The pastecs package contains functions like `stat.desc` and `by` to compute
# descriptive statistics and stratify data based on a specified categorical
# variable.
library(pastecs)
# The reshape2 package contains the function `melt` to convert data from a wide
# format to a long format.
library(reshape2)
# The tidyverse package contains functions for data wrangling and 
# generating figures, such as `ggplot`, among others.
library(tidyverse)

# The working directory is retrieved and stored as an object. This assumes that 
# the local repository on the user's machine is used.
wd_path <- getwd()

# The data file path is stored as an object based on where the data is stored 
# in the repository and the file name.
data_path <- paste0(wd_path, "/Assignment-3/TextMessages.csv")

# The data is loaded into the RStudio environment.
# Since the file has a .csv extension, the function `read.csv` is used.
# An object is created which contains the imported data from the specified 
# file path `data_path`.
text_data <- read.csv(data_path, header=TRUE)

# The command `names` calls the data frame object and returns the variable
# names from the columns in the data frame.
names(text_data)
# The output confirms that the variable names include "Group", "Baseline",
# "Six_months", and "Participant".

# The command `head` prints the first 5 observations from the data frame object 
# in the console window.
head(text_data)
# The output confirms that all variables contain numeric values. It also
# confirms that the data is in a wide format.

# The data is structured such that each row is an observation. The command 
# `nrow` calls the data frame object and returns the number of rows in 
# the dataset.
nrow(text_data)
# The output confirms there are 50 rows in the dataset.

# The groups are coded as 1 or 2. Having the labels stored as characters is 
# helpful for making the outputs and figures readable and legible. Input 
# arguments for the function `factor` are the data frame column with group 
# coding, defined levels of 1 and 2, and labels for each level.
text_data$Group_Label <- factor(text_data$Group, levels=c(1:2), 
                                labels=c("Group 1", 
                                         "Group 2"))

# The `is.factor` command is run to confirm that "Group_Label"
# is categorical.
is.factor(text_data$Group_Label)
# TRUE is returned which confirms that the variable is categorical.

# The `is.numeric` command is to confirm that "Group", "Baseline","Six_months", 
# and "Participant" are numeric.
is.numeric(text_data$Group)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Baseline)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Six_months)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Participant)
# TRUE is returned which confirms that the variable is numeric.

# The data is reshaped to a long dataset using the function `melt` from the
# package reshape. It is stored as a new object called `long_text_data`.
long_text_data <- melt(text_data, id.vars=c("Participant", "Group_Label"), 
                       measure.vars=c("Baseline", "Six_months"),
                       variable.name="Time", value.name="Value")

# The command `names` calls the data frame object and returns the variable
# names from the columns in the data frame.
names(long_text_data)
# The output confirms that the variable names include "Participant", 
# "Group_Label", "Time", and "Value".

# The command `head` prints the first 5 observations from the data frame object 
# in the console window.
head(long_text_data)
# The output confirms that the data is now in a long format.

# The `is.factor` command is to confirm that "Group_Label" and "Time" are
# categorical.
is.factor(long_text_data$Group_Label)
# TRUE is returned which confirms that the variable is categorical.
is.factor(long_text_data$Time)
# TRUE is returned which confirms that the variable is categorical.

# The `is.numeric` command is to confirm that "Participant" and "Value" 
# are numeric.
is.numeric(long_text_data$Participant)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(long_text_data$Value)
# TRUE is returned which confirms that the variable is numeric.

# TODO add code description
by(text_data$Baseline, text_data$Group_Label, 
   function(x) round(stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE), 4))

by(text_data$Six_months, text_data$Group_Label,
   function(x) round(stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE), 4))

# A faceted bar plot for text message count by group and time is made with 
# the function `ggplot` from the tidyverse package.
# Inputs to `ggplot` include the dataset object and the aesthetics function 
# called `aes`, where we set the categorical variable Time to the 
# x-axis and continuous variable Value to the y-axis. This 
# creates the first layer of the figure with the labeled axes. Next, the bar 
# plot layer is created with the function `stat_summary`, which utilizes the 
# mean rather than the individual observations. Inputs include defining the 
# mean as the function, "bar" as the shape, white as the fill, and black as 
# the outside outline. Another layer is added for the error bars with the 
# function `stat_summary`, which uses each group's 95% confidence interval, 
# as well as the pointrange geom in the color red. The function for the 95% 
# confidence interval is from the package Hmisc. In addition, function 
# `scale_y_continuous` is used to add another layer with user-defined y-axis 
# limits. Tick marks are also specified by the input breaks, using  
# y-axis limit values from 0 to 90 in increments of 2. Another layer is added 
# using the function `facet_grid` to create subplots to separate the data by 
# the categorical variable Group_Label, such that data from each group is 
# displayed in separate columns along the x-axis. Lastly, a layer that specifies 
# the axes labels and figure title is added.
long_text_data %>% ggplot(aes(x=Time, 
                              y=Value)) + 
  stat_summary(fun=mean, geom="bar", fill="White", colour="Black") +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", colour="Red") +
  scale_y_continuous(limits=c(0, 90), breaks=seq(from=0, to=90, by=2)) + 
  facet_grid(. ~ Group_Label) +
  labs(title="Bar Plots of Text Message Count by Group and Time", 
       x="Time", 
       y="Text Message Count")
# The bar charts show the mean value for each time and group along with error  
# bars, where the x-axis is time, the y-axis is the text message count, and the 
# subplot columns are group. In Group 1, the mean text message count decreased 
# from baseline to the six month time point. In Group 2, a similar pattern is 
# observed, however to a lesser degree. The mean and CI at baseline for both 
# groups are similar. However, the mean at six months appears to be smaller for
# Group 1 as compared to Group 2. The CI for the six month data for Group 1 is 
# larger as comapred to Group 2 as well.


#To measure the difference in baseline count of text messages vs count of 
#messages after 6 months, we will be employing a box plot.
#With box plots, the main "box" is comprised of the first/lower and
#third/upper quartiles, representing 25% and 75% of the data respectively.
#The line in the middle of the box is representative of the median in the data.
#Finally, the two lines extending above and below the boxes encompass all data
#within 1.5x the interquartile range (IQR). Generally, the longer these are,
#the greater the spread of data present. Any dots outside of these lines are
#data points that are outliers.

#One benefit of box plots is that they can provide a visualization of the 
#spread of data, making it easier to spot skewness and overall distribution
#shape

ggplot(long_text_data, aes(x=Time, y=Value)) +
  geom_boxplot() +
  facet_wrap(~Group_Label) +
  labs(
    title = "Boxplot of Baseline vs Six Month Text Count",
    x = "Time",
    y = "Text Message Count") 

#We see that for both groups of participants, there is a decrease in the median
#count of texts when comparing their baseline with their six month text count
#However, it is worth noting that in both groups, the 25th percentile of the
#baseline falls within the 75th percentile of the six month mark. This could
#be indicative that the difference in the medians is not statistically 
#significant
#Furthermore, we note that in the first group, there are multiple outliers, 
#particularly in the bottom range, which could have amounted to the 
#lowered median of the first group
