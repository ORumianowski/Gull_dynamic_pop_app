
### What is a metapopulation ?        		   ![](images/metapopulation.png){ width=8% } 

A metapopulation consists of a group of spatially separated populations of the same species which interact at some level. The application of metapopulation models is relevant to the study of population dynamics in fragmented environments or when breeders aggregate at sites and form colonies. This application aims to provide an interface that applies and illustrates a discrete metapopulation model whose characteristics can be chosen by the user.
The size of each population is determined by three processes: births, deaths and migration :

>   N<sub>n+1</sub> = N<sub>n</sub>  + Birth - Death + Immigration - Emigration 

### Modelling a metacolony of Black-headed Gull   ![](images/silouette.png){ width=10% } 

The model used in this application is placed in the context of modeling a Black-headed Gull (*Chroicocephalus ridibundus*) metacolony. The adults of this gregarious species come together in spring to breed in a colony. 



The environment provided by the colony is crucial for reproduction and depends on a number of factors: proximity to hunting areas, potential predators, etc. This context directly determines the reproductive success of adult gulls.  

In addition to the quality of the breeding sites, each colony is also characterized by a maximum carrying capacity **K**. Beyond this value, the adults cannot find the space they need to build a nest and are forced to emigrate. 

The dynamics of each colony is modeled in two stages: (i) the population's own growth, i.e. births and deaths, and (ii) migration.

#### Birth and death
In this example, the self-growth of a colony **i** is modeled in a discrete way according to the Beverton-Holt's equation and depends on the quality of the colony which induces a reproduction rate **r<sub>i</sub>**. In addition, stochasticity is introduced into the model to incorporate variability linked to reproduction. Thus, after reproduction of the colony, the number of individuals is obtained by : 

> Birth<sub>i</sub> - Death<sub>i</sub> = Lognormal( R0 x  N<sub>n</sub> / ( 1 +  N<sub>n</sub> / M), σ )


#### Migration
After the breeding season, the adults have the choice of staying in the colony or leaving it (post-reproductive dispersal).  The decision to emigrate depends on how saturated the colony is. If, after reproduction, the number of individuals exceeds the maximum carrying capacity **K<sub>i</sub>**, then a number equal to the surplus will emigrate.  In our example, all colonies have the same probability of being chosen.

> Emigration = max((N<sub>n+1</sub> -  K<sub>i</sub> )  , 0) 

> Immigration = (sum(Nb_emigrant) - Nb_emigrant) / (length(Nb_emigrant) - 1)

#### References
Grumbach, C., Reurik, F.N., Segura, J. et al. The effect of dispersal on asymptotic total population size in discrete- and continuous-time two-patch models. J. Math. Biol. 87, 60 (2023)  [https://link.springer.com/article/10.1007/s00285-023-01984-8)

![](images/mouette_rieuse.png){ width=30% }



