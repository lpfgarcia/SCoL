# SCoL

The Simulated Complexity Library (SCoL) is a package that simulate a set of complexity measures using meta-models generate by a meta-learning approach to decrease the asymptotic computational complexity for classification problems. The measures capture  aspects that quantify the linearity of the data, the presence of informative feature, the sparsity and dimensionality of the datasets. 

## Measures

The measures available were originally proposed by Ho and Basu [1] and extend by many other works including the ECoL library [2]. The measures are based on: (1) feature overlapping measures, (2) neighborhood measures, (3) linearity measures, (4) dimensionality measures, (5) class balance measures and (6) network measures.
  
## Installation

The installation process using devtools is:

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("lpfgarcia/SCoL")
library("SCoL")
```

## Example of use

The simplest way to compute the complexity measures are using the `complexity` method. The method can be called by a symbolic description of the model or by a data frame. The parameters are the dataset and the measures to be extracted. The default paramenter is extract all the measures. A simple example is given next:

```r
## Extract all complexity measures 
complexity(Species ~ ., iris)

## Extract all complexity measures using data frame
complexity(iris[,1:4], iris[,5])

## Extract the F1 measure using overlapping function
complexity(Species ~ ., iris, features="F2")
```

## Developer notes

To submit bugs and feature requests, report at [project issues](https://github.com/lpfgarcia/SCoL/issues).
