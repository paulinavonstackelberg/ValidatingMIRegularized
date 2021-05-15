## Datasets

In this folder, you can find the simulated *N*=50000 datasets, as well as the missingness patterns generated for the simulation study. These are later called in the scripts. These are the datasets you will start out with; from those large datasets, a smaller sample (*n* = 200 or 1000) is randomly drawn in each iteration. The datasets differ based on two criteria: i) number of covariates (*p* = 10 or 20), and ii) pairwise correlation between the covariates (corr = 0 or 0.3). Of course, you can also change the specifics if you want to run your own simulation - however, to do so you are referred to the script `data_sim`, which you can find in the folder `Functions_simulation`.


| File name       | Description of file                                                        |
|-----------------|----------------------------------------------------------------------------|
| data_spec_1save | *N*=50000 dataset, 20 covariates, correlation between covariates *cor*=0   |
| data_spec_2save | *N*=50000 dataset, 10 covariates, correlation between covariates *cor*=0   |
| data_spec_3save | *N*=50000 dataset, 20 covariates, correlation between covariates *cor*=0.3 |
| data_spec_4save | *N*=50000 dataset, 10 covariates, correlation between covariates *cor*=0.3 |
| mis_patt10      | Missingness pattern for conditions involving 10 covariates                 |
| mis_patt20      | Missingness pattern for conditions involving 20 covariates                 |
