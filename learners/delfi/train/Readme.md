

# Traininng models
## Optimal
``` bash
./train_opt.sh
```
Set RUNTIMEDATA and SPLIT to change the training data. 
Set LABEL_TYPE to binary or discrete

## Satisficing
``` bash
./train_sat.sh
```
Set RUNTIMEDATA and SPLIT to change the training data.
Keep LABEL_TYPE=sat 


# Testing models
``` bash
./test_binary.py --model <model> --images data/images23 --runtimes <runtime_data> --problems <test_set> --verbose
```

``` bash
./test_discrete.py --model <model> --images data/images23 --runtimes <runtime_data> --problems <test_set> --verbose
```

``` bash
./test_sat.py --model <model> --images data/images23 --runtimes <runtime_data> --problems <test_set> --verbose
```
