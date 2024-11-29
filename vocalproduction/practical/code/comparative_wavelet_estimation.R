#remember we want to determine the effects of height, age, sex, 'condition' on f0 and formants 

# consider the multivariate parameters: 
# x=[h,a,s], y=[fn]

# consider the effect of f0 on formants 
# x=f0, y=[fn] 


{# imports and file mgmt

library(wavethresh)
library(ggplot2)
data<-read.csv('C:/Users/laelu/Documents/mobi24-pc/w12-human/practical/data/human_voice.csv')

}

{## clean

# enumerate non-numerical values 
data$sex <- replace(data$sex, data$sex == 'F', 1)
data$sex <- replace(data$sex, data$sex == 'M', 0)
data$condition <- replace(data$condition, data$condition == 'small', 0)
data$condition <- replace(data$condition, data$condition == 'normal', 1)
data$condition <- replace(data$condition, data$condition == 'big', 2)

# ensure data is num type
numeric_vector <- as.numeric(unlist(data))
data <- as.numeric(numeric_vector)

}



{## construct wavelet-based density estimate. 
  
# use discrete wavelet transformation of order O(n). 
# wd uses mallat pyramid algorithm or cohen interval transformation
# filter type: DaubExPhase or DaubLeAsymm

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

# set resolution
x <- seq(min(data), max(data), length.out = length(data))
y <- approx(seq_along(density_estimate), density_estimate, x)$y

# params
bw=diff(range(x))/60 # adjust bins

# smooth kernel with normal distro
smooth_density <- ksmooth(x, y, kernel = 'normal', bandwidth = bw)

# refine plot variables
ggplot(data.frame(x = smooth_density$x, y = smooth_density$y), aes(x, y)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Smoothed Wavelet-based Density Estimation",
       x = "Data Columns", y = "Density")
}




