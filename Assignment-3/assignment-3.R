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
