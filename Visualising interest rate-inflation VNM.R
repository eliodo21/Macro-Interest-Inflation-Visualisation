# Set working directory to where the data with the interest rates is saved
setwd("C:/Users/thanh/Downloads/Macro")

# Load relevant libraries
library(fredr)

# Set API key for import from FRED (this is necessary to directly import data from FRED)
fredr_set_key("c009550630335a1d1868ec25c1914893")

######## 1) Central bank interest rate

# Read CSV file with interest rate
cbrate=read.csv("cbpol.csv", header= TRUE)

# Find start date
print(cbrate$time[1997])

# Transform central bank into time series object; make sure the start date is correct
cb_rate= ts(cbrate$cb_rate, frequency=1, start = c(1997,1))
print(cb_rate)

# Collapse into quarterly averages
cb_rate_qt=aggregate(cb_rate, nfrequency = 4, FUN = mean)
print(cb_rate_qt)

# Remove all data points before 1998
cb_rate_trim = window(cb_rate, start=c(1998,1))
print(cb_rate_trim)

######## 2) Inflation

#Import quarterly CPI (here for VNM)
CPI= fredr(
  series_id = "VNMPCPIPCPPPT",
  frequency ="a", units="lin")

# Find start date
print(CPI$date[1])

# Transform into time series object
cpi= ts(CPI$value, frequency=1, start = c(1990,1))
print(cpi)

# Transform into annualised growth rate (=inflation) 
infl=((cpi/lag(cpi, k=-1)) - 1)*100
print(infl)

# Remove all data points before 1998 and after 2023
infl_trim = window(infl, start=c(1998,1), end=c(2023,1))
print(infl_trim)

#### 3) Plot

# Plot series in one graph with separate axes
par(mar = c(5, 4, 4, 4) + 0.3)
plot(infl_trim, lty=1, ylab = 'INFL', lwd=2, main="Vietnam", xlab='')
par(new = TRUE)
plot(cb_rate_trim, lty=2, lwd=2, ylab = '', xlab='',  axes=FALSE)
axis(side = 4, at = pretty(range(cb_rate)))
mtext("INTRATE", side = 4, line = 2)
legend("topright", legend=c("INFL", "INTRATE"),
       lty=1:2, cex=0.8, bty = "n", y.intersp=0.5)

# Store image
dev.copy2pdf(file="infl_intrate_vn.pdf", width = 10, height = 7)
