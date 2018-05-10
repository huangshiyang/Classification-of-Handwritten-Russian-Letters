#uncomment the 2 lines after to install the package
#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5",suppressUpdates=TRUE)
library(rhdf5)
library(grid)
library(keras)
library(caret)
library(lattice)
library(ggplot2)

rotate <- function(x)
  t(apply(x, 2, rev))
#============================
#Load and Preprocess the Data
#============================
data <- read.csv("letters.csv")
files <- array(data$file)
letters <- array(data$letter)
backgrounds <- array(data$background)

# Create tensors and targets
fname <- "LetterColorImages.h5"
f <- h5ls(fname)
tensors <- h5read(fname, "/images")
targets <- h5read(fname, "/labels")
print ("Tensor shape:")
dim(tensors)
print ("Target shape:")
dim(targets)

# Normalize the tensors
tensors <- tensors / 255

# Read and display a tensor
print("Label:")
letters[101]
col <-
  rgb(rotate(tensors[1, 32:1, , 101]), rotate(tensors[2, 32:1, , 101]), rotate(tensors[3, 32:1, , 101]))
dim(col) <- dim(tensors[1, , , 101])
grid.raster(col, interpolate = FALSE)

# Grayscaled tensors
gray_tensors <- tensors[1, , ,]
for (i in 1:1650) {
  for (j in 1:32) {
    for (k in 1:32) {
      gray_tensors[j, k, i] <-
        tensors[1:3, j, k, i] %*% c(0.299, 0.587, 0.114)
    }
  }
}

# Read and display a grayscaled tensor
print("Label:")
letters[101]
grid.raster(rotate(gray_tensors[32:1, , 101]), interpolate = FALSE)

# Print the target unique values
unique(targets)
print("{а,б,в,г,д,е,ё,ж,з,и,й,к,л,м,н,о,п,р,с,т,у,ф,х,ц,ч,ш,щ,ъ,ы,ь,э,ю,я}")

# One-hot encode the targets, started from the zero label
cat_targets = to_categorical(targets - 1, 33)
dim(cat_targets)

# Split the data
set.seed(100)
#creating indices
trainIndex <- createDataPartition(cat_targets[, 1], p = 0.8, list = FALSE)
x_train <- tensors[, , , trainIndex]
y_train <- cat_targets[trainIndex, ]
x_test <- tensors[, , , -trainIndex]
y_test <- cat_targets[-trainIndex, ]
# Print the shape
print ("Training tensor's shape:")
dim(x_train)
print ("Training target's shape:")
dim(y_train)
print ("Testing tensor's shape:")
dim(x_test)
print ("Testing target's shape:")
dim(y_test)

# Split the grayscaled data
trainIndex <- createDataPartition(cat_targets[, 1], p = 0.8, list = FALSE)
x_train2 <- gray_tensors[, , trainIndex]
y_train2 <- cat_targets[trainIndex, ]
x_test2 <- gray_tensors[, , -trainIndex]
y_test2 <- cat_targets[-trainIndex, ]


# Print the shape
print ("Training grayscaled tensor's shape:")
dim(x_train2)
print ("Training grayscaled target's shape:")
dim(y_train2)
print ("Testing grayscaled tensor's shape:")
dim(x_test2)
print ("Testing grayscaled target's shape:")
dim(y_test2)

