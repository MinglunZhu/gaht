# gaht
Genetic Algorithm for Hyper-parameter Tuning.

## Description
This is a genetic algorthm built in R designed for hyper-parameter tuning for neural networks.

I've decided to open source it, as I don't think I can improve it further.

It takes in a population and randomly initialized the hyper-parameters, then it tests all agents in the population and takes the top ranked agents.

Then it randomly couples 2 agents together as father and mother, and inherit hyper-parameters from the parents as traits for the next generation with a random chance of mutation.

This process repeats and guides the hyper-parameter towards a direction that produces higher ranking agents.

This does not encode DNAs or genes or whatnots, as I'm not famililiar with those things. So, feel free to correct me if I'm doing this wrong.

## License
[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/)
