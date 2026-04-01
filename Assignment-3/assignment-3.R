#####
##### Data preparation
#####

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

# The `is.factor` command is run to confirm that Group_Label
# is categorical.
is.factor(text_data$Group_Label)
# TRUE is returned which confirms that the variable is categorical.

# The `is.numeric` command is to confirm that Group, Baseline,Six_months, 
# and Participant are numeric.
is.numeric(text_data$Group)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Baseline)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Six_months)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(text_data$Participant)
# TRUE is returned which confirms that the variable is numeric.

# The data is reshaped to a long format using the function `melt` from the
# package reshape2. It is stored as a new object called `long_text_data`.
long_text_data <- melt(text_data, id.vars=c("Participant", "Group_Label"), 
                       measure.vars=c("Baseline", "Six_months"),
                       variable.name="Time", value.name="Value")

# For the Time variable, the string "Six_months" is changed to "Six Months" to
# improve legibility of figures that will use this label.
long_text_data$Time <- gsub("Six_months", "Six Months", long_text_data$Time)

# The command `names` calls the data frame object and returns the variable
# names from the columns in the data frame.
names(long_text_data)
# The output confirms that the variable names include "Participant", 
# "Group_Label", "Time", and "Value".

# The command `head` prints the first 5 observations from the data frame object 
# in the console window.
head(long_text_data)
# The output confirms that the data is now in a long format.

# The `is.factor` command is to confirm that Group_Label and Time are
# categorical.
is.factor(long_text_data$Group_Label)
# TRUE is returned which confirms that the variable is categorical.
is.factor(long_text_data$Time)
# TRUE is returned which confirms that the variable is categorical.

# The `is.numeric` command is to confirm that Participant and Value 
# are numeric.
is.numeric(long_text_data$Participant)
# TRUE is returned which confirms that the variable is numeric.
is.numeric(long_text_data$Value)
# TRUE is returned which confirms that the variable is numeric.

#####
##### Summary statistics
#####

# To compute summary statistics, the data object with a wide format is used. 
# Here, the continuous variables, Baseline and Six_months, are stored in 
# separate columns. 

# The summary statistics for these continuous variables can be
# stratified by the categorical variable, Group_Label, with the function
# `by` from the pastecs package. The first input argument is the
# continuous variable, the second one is the categorical variable, and the
# third one specifies a function applied to the data. In this case, a
# new function is defined which combines the use of the functions `round` and
# `stat.desc`. Specifically, when the function `round` is called, the first
# input argument is the output from `stat.desc`, and the second input argument
# is the number of decimal places, which is specified as 4 here.

# Inputs to `stat.desc` include the following:
# - basic is set to TRUE to check how many observations are included
# - desc is set to TRUE to check the descriptive statistics values
# - norm is set to FALSE to suppress the normality test output

# The continuous variable, Baseline, is stratified by the categorical
# variable, Group_Label.
by(text_data$Baseline, text_data$Group_Label, 
   function(x) round(stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE), 4))
# For the Baseline data, the output indicates that all 25 observations in each
# group were used to compute the descriptive statistics, since none were
# missing. The Group 1 has a mean of 64.84, median of 64, variance of 114.0567, 
# and standard deviation of 10.6797. The range is 38, with a minimum of 47 and a
# maximum of 85. The Group 2 has a mean of 65.6, median of 65, variance of  
# 117.4167, and standard deviation of 10.8359. The range is 43, with a minimum 
# of 46 and a maximum of 89.

# The mean and median are fairly close and only differ by 0.84 and 0.6 for 
# Group 1 and Group 2, respectively. Thus, using the mean to summarize
# the data is appropriate. Both groups have a similar mean and overall 
# variability.

# The continuous variable, Six_months, is stratified by the categorical
# variable, Group_Label.
by(text_data$Six_months, text_data$Group_Label,
   function(x) round(stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE), 4))
# For the Six Months data, the output indicates that all 25 observations in each
# group were used to compute the descriptive statistics, since none were
# missing. The Group 1 has a mean of 52.96, median of 58, variance of 266.7067, 
# and standard deviation of 16.3312. The range is 69, with a minimum of 9 and a
# maximum of 78. The Group 2 has a mean of 61.84, median of 62, variance of 
# 88.5567, and standard deviation of 9.4105. The range is 33, with a minimum of 
# 46 and a maximum of 79.

# The mean and median for Group 2 are fairly close and differ by 0.16. However, 
# for Group 1, they differ by 5.04, which is a somewhat large gap and may 
# warrant further testing for normality. Overall, using the mean to summarize 
# the data is appropriate. Note that Group 1 has a smaller mean, larger 
# range, and thus more variability.

#####
##### Boxplot
#####

# To measure the difference in baseline count of text messages vs count of 
# messages after 6 months, we will be employing a box plot.
# With box plots, the main "box" is comprised of the first/lower and
# third/upper quartiles, representing 25% and 75% of the data respectively.
# The line in the middle of the box is representative of the median in the data.
# Finally, the two lines extending above and below the boxes encompass all data
# within 1.5x the interquartile range (IQR). Generally, the longer these are,
# the greater the spread of data present. Any dots outside of these lines are
# data points that are outliers.

# One benefit of box plots is that they can provide a visualization of the 
# spread of data, making it easier to spot skewness and overall distribution
# shape

ggplot(long_text_data, aes(x=Time, y=Value, colour=Time)) +
  geom_boxplot() +
  facet_wrap(~Group_Label) +
  labs(
    title = "Boxplot of Text Message Count by Group and Time",
    x = "Time",
    y = "Text Message Count") +
  scale_color_manual(values = c("Baseline" = "orange", 
                                "Six Months" = "darkturquoise"))

# We see that for both groups of participants, there is a decrease in the median
# count of texts when comparing their baseline with their six month text count
# However, it is worth noting that in both groups, the 25th percentile of the
# baseline falls within the 75th percentile of the six month mark. This could
# be indicative that the difference in the medians is not statistically 
# significant
# Furthermore, we note that in the first group, there are multiple outliers, 
# particularly in the bottom range, which could have amounted to the 
# lowered median of the first group

#####
##### Bar chart
#####

# A faceted bar chart for text message count by group and time is made with 
# the function `ggplot` from the tidyverse package.
# Inputs to `ggplot` include the dataset object, the aesthetics function 
# called `aes`, where we set the categorical variable Time to the x-axis and 
# continuous variable Value to the y-axis, and assigning Time for color coding. 
# This creates the first layer of the figure with the labeled axes. Next, the 
# bar chart layer is created with the function `stat_summary`, which utilizes the 
# mean rather than the individual observations. Inputs include defining the 
# mean as the function, "bar" as the shape and white as the fill. 
# Another layer is added for the error bars with the function `stat_summary`, 
# which uses each group's 95% confidence interval. The function for the 95% 
# confidence interval is from the package Hmisc. 
# In addition, function `scale_y_continuous` is used to add another layer 
# with user-defined y-axis limits. Tick marks are also specified by the input 
# breaks, using y-axis limit values from 0 to 90 in increments of 5. 
# Another layer is added using the function `facet_grid` to create subplots to 
# separate the data by the categorical variable Group_Label, such that data 
# from each group is displayed in separate columns along the x-axis. Lastly, a 
# layer that specifies the axes labels and figure title is added.
long_text_data %>% ggplot(aes(x=Time, 
                              y=Value, colour=Time)) + 
  stat_summary(fun=mean, geom="bar", fill="White") +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange") +
  scale_y_continuous(limits=c(0, 90), breaks=seq(from=0, to=90, by=5)) + 
  facet_grid(. ~ Group_Label) +
  labs(title="Bar Charts of Text Message Count by Group and Time", 
       x="Time", 
       y="Text Message Count")+
  scale_color_manual(values = c("Baseline" = "orange", 
                                "Six Months" = "darkturquoise"))
# The bar charts show the mean value for each time and group along with error  
# bars, where the x-axis is time, the y-axis is the text message count, and the 
# subplot columns are group. In Group 1, the mean text message count decreased 
# from baseline to the six month time point. In Group 2, a similar pattern is 
# observed, however to a lesser degree. The mean and CI at baseline for both 
# groups are similar. However, the mean at six months appears to be smaller for
# Group 1 as compared to Group 2. The CI for the six month data for Group 1 is 
# larger as compared to Group 2 as well.
