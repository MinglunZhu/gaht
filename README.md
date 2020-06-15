# gaht
Genetic Algorithm for Hyper-parameter Tuning.

## Description
This is a genetic algorthm built in R designed for hyper-parameter tuning for neural networks.

I've decided to open source it, as I don't think I can improve it further.

It takes in a population and randomly initializes the hyper-parameters, then it tests all agents in the population and takes the top ranked agents.

Then it randomly couples 2 agents together as father and mother, and inherit hyper-parameters from the parents as traits for the next generation with a random chance of mutation.

This process repeats and guides the hyper-parameter towards a direction that produces higher ranking agents.

This does not encode DNAs or genes or whatnots, as I'm not famililiar with those things. So, feel free to correct me if I'm doing this wrong.

## How to Use
### Dependencies
- dplyr

### Build Package
Open `rPkgEvlAlg.Rproj` in RStudio, and go to `Build` > `Clean and Rebuild`. This will build the package and make it available in your local computer. I'm not sure about other IDEs, but they should have something similar.

### Load Package
After building the package, you need a new session to be able to load the package. Assuming you are using a new session, use the following code to load the package:
```
library(rPkgEvlAlg)
```

### Using the Function
To start the algorithm, call:
```
evolve(ftr_settings, test, dupGens = 4, pop = NULL, popSize = NULL, maxGens)  
```
#### params
- `ftr_settings` (data.frame):
  - a list of features to evolve
  - each feature has a list of settings
  - each feature must have a min value and max value, the evolution will only happen within this limit
  - each individual in the population will have a set of evolved features
  - the features are then fed to the test function to test
   
- `test` (funciton):
  - a function that will take the features as argument and test them
  - must return a score, the higher the score the better
  - you must define how the test function works, and how it utilizes the features
   
- `dupGens` (int):
  - number of generations which the best score hasn't improved
  - when this is reached, the evolution stops and assumes that no improvements can be made
  - note that this number will increase automatically if the best score continues to improve for many generations
  - so you only need to set a number sensible for the initial few generations
   
- `pop` (list):
  - you can pre-supply a population
  - if pop not supplied, it will be randomly initialized
   
- `popSize` (int):
  - if pop is not pre-supplied, popSize is needed to randomly inistialize a population
  - min 4 is required
   
- `maxGens` (int):
  - max generations the algorithm will try to evolve,
  - after that all generations are considered duplicated generations
  - and will evolve for specified duplicate generations allowed
  - set to 0 to disable

#### returns
- (list):
  - the last generation of population.
  - The first in the list is the individual with the highest score.
  - The individual is a 1d vector with each item being a feature in the same order defined in `ftr_settings`.

#### Example
Example for the `ftr_settings` data frame:
```
settings <- data.frame(
  name =           c('filter1',   'filter2', 'numLayers_dense', 'numUnits1', 'dropRate1', 'numUnits2',       'dropRate2',       'numUnits3',       'dropRate3'),
  min =            c(300,         200,        3,                 200,        .8,           200,              .5,                200,               0),
  max =            c(300,         200,        3,                 200,        .8,           200,              .5,                200,               1),
  type =           c('integer',   'integer',  'integer',         'integer',  'numeric',   'integer',         'numeric',         'integer',         'numeric'),
  dependency =     c(NA,          NA,         NA,                NA,         NA,          'numLayers_dense', 'numLayers_dense', 'numLayers_dense', 'numLayers_dense'),
  dependency_min = c(NA,          NA,         NA,                NA,         NA,          2,                 2,                 3,                 3)
)
```
  - `name`: name of the hyper-parameter
  - `min`: minimum value for the hyper-parameter
  - `max`: maximum value for the hyper-parameter
  - `type`: data type, if is integer, the random mutation and initialization will round the number to integer
  - `dependency`: if the hyper-parameter depends on another hyper-parameter
      for example, if number of layers is 2, then the number of units and dropout rate in layer 3 will be set to 0
      regardless of what the min value for layer 3 units and layer3 dropout are
      so, layer 3 units and layer 3 dropout have a dependency on number of layers
  - `dependency_min`: the mininum value required for the depended hyper-parameter
  
  To temporarily lock a hyper-parameter, set the min value and max value to the same value. This means that the locked hyper-parameter will not evolve by the algorithm.
  
  To permanantly lock a hyper-parameter, don't add that hyper-parameter into the `ftr_settings` data frame, and just hard code it into the test function.
  
  Unfortunately, if you have 50 layers, you will need to create settings for all 50 layers in the data frame. You could use a loop in that case, rather than manually. The reason for this is that so 50 layers hyper-parameters can evolve independent of each other. This also allows you to have 50 different settings for all 50 layers. Although, it's very unlikely you will need 50 different settings. Most likely, you will have the same settings for most layers. Use a loop to create the settings data frame in that case.
  
## Caveats
Obviously, this is a very brute force way of testing hyper-parameters, because you train the neural network for the population size  for the number of generations, which can be very expensive. Especially considering that a large population is recommended.

I like, however, the fact that neural network mimics the human brain, and genetic algorithms mimic the evolution of brains.

## License
[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/)
