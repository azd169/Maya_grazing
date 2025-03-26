library(magick)

# Otsu's treshold for for green intensity

otsu_threshold <- function(intensities, nbins = 256) {
  # Compute histogram without plotting
  h <- hist(intensities, breaks = nbins, plot = FALSE)
  counts <- h$counts
  bins <- h$mids
  
  total <- sum(counts)
  sum_all <- sum(bins * counts)
  sumB <- 0
  wB <- 0
  max_between <- 0
  threshold <- 0
  
  # Iterate over all bins to find the threshold that maximizes between-class variance
  for (i in seq_along(counts)) {
    wB <- wB + counts[i]
    if (wB == 0) next
    wF <- total - wB
    if (wF == 0) break
    sumB <- sumB + bins[i] * counts[i]
    mB <- sumB / wB
    mF <- (sum_all - sumB) / wF
    between <- wB * wF * (mB - mF)^2
    if (between > max_between) {
      max_between <- between
      threshold <- bins[i]
    }
  }
  return(threshold)
}

# Define the folder path containing the .jpg files (update as needed)
folder_path <- "~/R working directory/Maya photos/"

# List all .jpg files in the folder (full names returned)
files <- list.files(path = folder_path, pattern = "\\.jpg$", full.names = T)

# Initialize a data frame to store the results
results <- data.frame(File = character(0), PercentGreen = numeric(0), stringsAsFactors = F)

# Process each file
for (file in files) {
  cat("Processing file:", file, "\n")
  
  # Load the image
  ndvi_img <- image_read(file)
  
  # Extract RGB pixel data (array with dimensions: channels x width x height)
  img_data <- image_data(ndvi_img, channels = "rgb")
  
  # Convert each channel from character to integer vectors/matrices.
  # Here, each channel is a matrix; as.integer() returns a vector but we only need it for calculations.
  red_channel   <- as.integer(img_data[1, , ])
  green_channel <- as.integer(img_data[2, , ])
  blue_channel  <- as.integer(img_data[3, , ])
  
  # Flatten the green channel into a vector of intensities
  green_values <- as.vector(green_channel)
  
  # Optional: Plot a histogram of the green channel values for visual inspection.
  # hist(green_channel, breaks = 300,
  #      main = paste("Histogram of Green Channel Intensities for", basename(file)),
  #      xlab = "Green Intensity")
  
  # Compute Otsu's threshold for the green channel
  green_intensity_threshold <- otsu_threshold(green_values)
  
  # Identify green pixels: green value is dominant and above the threshold
  green_pixels <- (green_channel > red_channel) &
    (green_channel > blue_channel) &
    (green_channel > green_intensity_threshold)
  
  # Count the number of green pixels
  num_green_pixels <- sum(green_pixels)
  
  # Total number of pixels (using the length of the green channel)
  total_pixels <- length(green_channel)
  
  # Calculate the percentage of green pixels
  percent_green <- (num_green_pixels / total_pixels) * 100
  
  # Append the result to the data frame
  results <- rbind(results, data.frame(File = basename(file),
                                       Percent_Green = percent_green,
                                       stringsAsFactors = F))
}