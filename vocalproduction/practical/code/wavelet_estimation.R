#remember we want to determine the effects of height, age, sex, 'condition' on f0 and formants 
# x=[h,a,s], y=[fn]
#and the effect of f0 on formants 
# x=f0, y=[fn] 


options(connectionObserver=NULL)

#install.packages('wavethresh')
library(wavethresh)
library(ggplot2)

data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')


# discrete wavelet transformation / decomposition of order O(n). 
# uses mallat pyramid algorithm or cohen interval transformation
# wavelet family options for filter selection: DaubExPhase, DaubLeAsymm
# look into Lina-Mayrand complex-valued wavelets in the future
# also look into the 'density() function for kernel density estimation https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/density 

{

# clean the data and enumerate non-numerical values 
data$sex <- replace(data$sex, data$sex == 'F', 1)
data$sex <- replace(data$sex, data$sex == 'M', 0)
data$condition <- replace(data$condition, data$condition == 'small', 0)
data$condition <- replace(data$condition, data$condition == 'normal', 1)
data$condition <- replace(data$condition, data$condition == 'big', 2)

}

# inspecting data output
data

# unlisting data to convert to numbers, just in case we have some stragglers that are strings
numeric_vector <- as.numeric(unlist(data))
data <- as.numeric(numeric_vector)


{


## construct wavelet-based density estimate

# params
filt_num=10
filt_type='DaubLeAsymm'

# squared pad for input to wavelet algorithm
next_power_of_two <- 2^ceiling(log2(length(data)))
padded_data <- c(data, rep(0, next_power_of_two - length(data)))

# filter 
pad_wavelet_decomp <- wd(padded_data, filter.number = filt_num, family = filt_type)

# denoise
thresholded <- threshold(pad_wavelet_decomp,policy='universal') 

# reconstruct

density_estimate <- wr(thresholded) 


}


{## plot density estimation of all column values

# Increase resolution and apply kernel smoothing
x <- seq(min(data), max(data), length.out = length(data))
y <- approx(seq_along(density_estimate), density_estimate, x)$y

# params
bw=diff(range(x))/60 # note! this value constrains geom_line to remove values outside of the scale range. be careful to read console output and adjust as/if necessary

smooth_density <- ksmooth(x, y, kernel = 'normal', bandwidth = bw)

# simple plot, chunky bins
#plot(smooth_density,type='l',main="Smoothed Wavelet-based Density Estimation")

# refine plot variables
ggplot(data.frame(x = smooth_density$x, y = smooth_density$y), aes(x, y)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Smoothed Wavelet-based Density Estimation",
       x = "Data Columns", y = "Density")
}




