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
long_text_data %>% ggplot(aes(x=Time, 
                              y=Value)) + 
  stat_summary(fun=mean, geom="bar", fill="White", colour="Black") +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", colour="Red") +
  scale_y_continuous(limits=c(0, 90), breaks=seq(from=0, to=90, by=2)) + 
  facet_grid(. ~ Group_Label) +
  labs(title="Bar Plots of Text Message Count by Group and Time", 
       x="Time", 
       y="Text Message Count")
