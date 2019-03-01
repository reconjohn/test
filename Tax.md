---
title: "2018 Payroll Tax"
author: "Yohan"
output:
  html_document:
    toc: true
    keep_md: true
  pdf_document: default
---
> This graph is [based on 2018 Payroll Tax Information](https://isc.uw.edu/wp-content/uploads/2018/01/Payroll-Tax-Information-2018.pdf). The amount of wages is the wages after subtracting withholding allowances, which is calculated **$172.90 x number of allowances claimed**. Furthermore, additional **$2,000** is substracted from the wages based on the tax exemption treaty with Korean government such that the tax treaty allows me to be exempt from federal withholding on the first $2,000 of my wages. However, the treaty is not applicable for the Social Security and Medicare taxes (Federal Insurance Contributions Act).


```r
y = c()
pa = c(154, 551, 1767, 3592, 6717, 8488, 20988)
ba = c(39.7, 185.62, 587.12, 1337.12, 1903.84, 6278.84)
pc = c(0.1, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)

for(i in 1:30000){
  if(i < pa[1]){
    y[i] = 0
  } else if(i < pa[2]){
    y[i] = (i - pa[1]) * pc[1]
  } else if(i < pa[3]){
    y[i] = (i - pa[2]) * pc[2] + ba[1]
  } else if(i < pa[4]){
    y[i] = (i - pa[3]) * pc[3] + ba[2]
  } else if(i < pa[5]){
    y[i] = (i - pa[4]) * pc[4] + ba[3]
  } else if(i < pa[6]){
    y[i] = (i - pa[5]) * pc[5] + ba[4]
  } else if(i < pa[7]){
    y[i] = (i - pa[6]) * pc[6] + ba[5]
  } else {
    y[i] = (i - pa[7]) * pc[7] + ba[6]
  }
}

library(ggplot2)
y = data.frame(y)
data = cbind(1:30000, y)
colnames(data) = c("Amount of wages", "Tax withholding")

ggplot(data, aes(x = `Amount of wages`, y = `Tax withholding`)) +
  geom_line(color = "red") +
  scale_x_continuous(breaks = round(seq(0, 30000, len = 16))) +
  scale_y_continuous(breaks = round(seq(0, 10000, len = 21))) +
  ggtitle("Federal Income Tax Withholding for Single") + theme_bw()
```

![](Tax_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

> FICA exemption can be qualified under two rules if a nonresident alien or in a student job class position and enrolled full-time. 

> Residential alien: To be considered to be an resident alien for tax purposes if been present in the US more than 183 days for 3 years as per [substantial presence test]( https://www.irs.gov/individuals/international-taxpayers/substantial-presence-test). But it says for those who temporarily present in the U.S. under an "F," "J," "M," or "Q" visa, who substantially complies with the requirements of the visa, they are considered to be exempt individuals leading to excluding days of presence in the US. In this regard, am I still entitled to be a non-resident alien for tax purposes?

> The 14% federal tax withholding is for stipend payments.

---
title: "Tax.R"
author: "Yohan_Min"
date: "Thu Feb 28 18:11:55 2019"
---
