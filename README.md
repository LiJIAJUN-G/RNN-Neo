# RNN-Neo 


RNN-Neo, a RNN (recurrent neural network) model framework, simulating the in vivo processing order from proteasomal cleavage, TAP transportation, pMHC binding, to TCR activation.



#### Required Dependencies

* [python](https://www.python.org) (3.10.13)
* [R](https://www.r-project.org/) (4.3.2)
* [numpy](https://numpy.org) (1.23.5)
* [pytorch](https://pytorch.org) (0.11.1)
* [pandas](https://pandas.pydata.org) (2.0.3)
* [scikit-learn](https://scikit-learn.org) (1.3.1)
* [tidyverse](https://www.tidyverse.org/) (2.0.0)
* [ggplot2](https://ggplot2.tidyverse.org/) (3.4.4)



### Install 

#### Get the RNN-Neo Source

```
git clone https://github.com/LiJIAJUN-G/RNN-Neo.git
```

### Usage

#### Train RNN-Neo 
If you want to retrain RNN-Neo, you can run the `01` code and output the `train.log` file, and the model is saved in the `model_test` folder
```
python  01RNN_train.py
```
#### Evaluate RNN-Neo 

Run the `02,03` code to implement the evaluation of RNN-Neo, and the result is generated in the `result` folder

```
python  02RNN_evaluate.py
python  03performance_calculation.py
```
#### Plot
Change  R code `setwd()` to the path of RNN-Neo.
Run the R program to display the results of the bar and box charts in the `fig` folder
```
barplot.R
boxplot.R
```
#### Authors
JiaJun Li and TianYi Qiu

### Contact   

23110700030@m.fudan.edu.cn or qiu.tianyi@zs-hospital.sh.cn
Fudan University, Shanghai, China

![RNN_Neo](./RNN_Neo.png)