# PokeNet
![Visualized Result](/Visualizing/neural_net.png?raw=True)
## Data
Taken from the [Pokémon Challenge on Kaggle](le.com/terminus7/pokemon-challenge/data)
## Tools
* **R** — Easy to perform complex operations statistical operations concisely
  * **RStudio** — IDE, for ease of coding
  * **library(neuralnet)** — for building the neural nets
  * **library(dplyr)** — for data manipulation i.e. pulling apart and concisely scaling data
  * **library(caTools)** — for partitioning data
  * **library(parallel)** — for parallelizing the process of generating neural nets with different parameterizations
  * **library(ggplot2)** — for beautiful and easy visualization of data
## Data Preparation
After generally cleaning up the data to be properly formatted for R (e.g. turning "True" strings into TRUE booleans),
I normalized all continuous data to Gaussian distributions. Categoricals as a whole presented a more
difficult challenge. Dealing with binary variables was easy enough — I encoded independent binary variables
as 1 and -1, and the dependent (the victor) as 0 and 1. The I used a different encoding approach between 
independent and dependent because [a resource](https://visualstudiomagazine.com/articles/2013/07/01/neural-network-data-normalization-and-encoding.aspx)
indicated this was preferable. I attempted to use 1-of-(C-1) effects encoding for non-binary categorical variables,
but after encoding them I was unable to use them as input, and thus chose to disregard them.  
## Neural Net Generation
I used the *neuralnet* package in R, which utilizes resilient backpropagation [(Riedmiller, 1994)](http://ieeexplore.ieee.org/document/298623/).  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;I arbitrarily chose to use 3 hidden layers. I put 10 nodes in each hidden layer as it was a round number that 
was slightly less than my number of input variables. As I experimented with training set size, I found that
decreasing returns warranted limiting the input training size to ~ 5000 battles. Because this is only 1/10 of the data
that I had at my disposal, this mean that I could extensively test my neural nets.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Because the ratio of my training to testing was 1:9 and yielded ~ 92% accuracy, I am confident that the degree of overfitting
is not significant.
## Results
```
Neural Net with training size of 1000 took: 1.204 seconds  
  Accuracy: 89.52%  
Neural Net with training size of 2000 took: 7.866 seconds  
  Accuracy: 91.26%  
Neural Net with training size of 3000 took: 45.632 seconds  
  Accuracy: 92.02%  
Neural Net with training size of 4000 took: 48.838 seconds  
  Accuracy: 91.86%  
Neural Net with training size of 5000 took: 201.124 seconds  
  Accuracy: 91.74%  
Neural Net with training size of 6000 took: 857.779 seconds  
  Accuracy: 92.7%  
Neural Net with training size of 7000 took: 432.797 seconds  
  Accuracy: 93.26%
```
