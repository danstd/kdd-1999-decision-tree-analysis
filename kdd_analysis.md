KDD Decision Tree Analysis
================
Daniel Davis
September 27, 2020

### Description

In this project two training sets are selected from the 10% KDD Cup 1999 Dataset, located at <http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html>. Decision trees are used for classification of attack types.

### Data Retrieval and Preprocessing

The dataset was downloaded and extracted to a csv file, stored at Z:.csv The features for this dataset are stored in a .names file, stored at Z:.names

The KDD 10% Training Dataset, containing 494,020 records is used for this project The dataset and headers are in two separate files, which will be read in and preprocessed below.

``` r
kdd <- read.csv("Z:\\kddcup.csv", header = FALSE, stringsAsFactors = FALSE)
headers <- read.delim("Z:\\kddcup.names", header = FALSE, stringsAsFactors = FALSE, sep = ":")

#Get the data classification from the first entry
classificationTypes <- headers[1,1]
classificationTypes <- unlist(strsplit(headers[1,1],","))

#Remove the trailing period from the last classificationType entry.
x <- classificationTypes[length(classificationTypes)]
x <- substr(x, 1, nchar(x)-1)
classificationTypes[length(classificationTypes)] <- x

headers <- headers[[1]]
headers <- headers[-1]
headers[length(headers) + 1] <- "classification"

colnames(kdd) <- headers

#Write the dataset with headers to a new file
#write.csv(kdd, "Z:\\kddcupWithHeaders.csv")
```

### Data Exploration

``` r
length(unique(kdd$classification))
```

    ## [1] 23

``` r
unique(kdd$classification)
```

    ##  [1] "normal."          "buffer_overflow." "loadmodule."      "perl."            "neptune."         "smurf."           "guess_passwd."    "pod."             "teardrop."        "portsweep."      
    ## [11] "ipsweep."         "land."            "ftp_write."       "back."            "imap."            "satan."           "phf."             "nmap."            "multihop."        "warezmaster."    
    ## [21] "warezclient."     "spy."             "rootkit."

There are 23 unique attack types in the KDD 10% dataset, belonging to four different general classification types in addition to normal traffic (CUP-99 Task Description). These five values will be used for classification.

``` r
#Set a vector for each classification type

dos <- c("back.", "land.", "neptune.", "pod.", "smurf.","teardrop.")
probe <- c("ipsweep.", "nmap.", "portsweep.", "satan.")
u2r <- c("buffer_overflow.", "loadmodule.", "perl.", "rootkit.")
r2l <- c("ftp_write.", "guess_passwd.", "imap.", "multihop.", "phf.", "spy.", "warezmaster.", "warezclient.")
```

We see that there is a large disparity in the type of attacks represented in the dataset, which may be rectified with undersampling and oversampling. A new column is created with the classification type mapped to each attack.

    ##     Var1   Freq      percent
    ## 1    dos 391458 0.7923914166
    ## 2 normal  97278 0.1969106576
    ## 3  probe   4107 0.0083134118
    ## 4    r2l   1126 0.0022792553
    ## 5    u2r     52 0.0001052587

### Sample Creation

Below, a testing and a validation dataset are created using random sampling. A sample size of 4900 will be used for each, 1% of the total dataset. A separate testing sample will be used in order to evaluate both decision trees against a sample with the same distribution as the population dataset, without relying on the validation set.

``` r
set.seed(341)
sampleSize <- 9800
sampleVector <- sample(c(1:length(kdd$classification)), sampleSize)

kddCut <- kdd[sampleVector,]
#Remove the test and validation records from the kdd dataset
kdd <- kdd[-sampleVector,]

sampleSize <- sampleSize/2
sampleCut <- sample(c(1:length(sampleVector)), sampleSize)

kddTest <- kddCut[sampleCut,]

kddValidation <- kddCut[-sampleCut,] 

kddcut = NULL
```

The distribution of the test set is displayed:

    ##     Var1 Freq      percent
    ## 1    dos 3934 0.8028571429
    ## 2 normal  912 0.1861224490
    ## 3  probe   45 0.0091836735
    ## 4    r2l    8 0.0016326531
    ## 5    u2r    1 0.0002040816

The distribution of the validation set is displayed:

    ##     Var1 Freq      percent
    ## 1    dos 3852 0.7861224490
    ## 2 normal  993 0.2026530612
    ## 3  probe   43 0.0087755102
    ## 4    r2l    9 0.0018367347
    ## 5    u2r    3 0.0006122449

The test and validation sets closely matches the population distribution. At least one of each classification type is included in each set.

Our first training sample will also be selected using random sampling:

    ##     Var1 Freq      percent
    ## 1    dos 3872 0.7902040816
    ## 2 normal  986 0.2012244898
    ## 3  probe   34 0.0069387755
    ## 4    r2l    7 0.0014285714
    ## 5    u2r    1 0.0002040816

This sample also contains a similar distribution to the population; however it is important to note that only a single U2R record is included.

The second training sample will be oversampled on Probe, R2L and U2R records, and undersampled on DOS and Normal records. Stratified sampling procedure is sourced from Malato, 2019.

    ##     Var1 Freq     percent
    ## 1    dos 3605 0.735714286
    ## 2 normal  856 0.174693878
    ## 3  probe   73 0.014897959
    ## 4    r2l  346 0.070612245
    ## 5    u2r   20 0.004081633

Using a probability vector the likelihood of selection of each class has been highly modified, but with a relatively small impact on the classification distribution in the sample data set. The frequency of R2L and U2R records have both been increased by one order of magnitude with relatively little change to the other classes.

### Decision Trees

Decision trees for each sample will be created using the R Party package's implementation of the Conditional Inference Tree algorithm. To ensure that the class distribution discrepancy in this dataset is accounted for, the Kappa statistic will be used as the training criteria.

#### Random Distribution Sample Decision Tree

Using the FSelector library, the information gain for each classification will be calculated for the first training sample.

``` r
set.seed(134)
sampleImportance <- gain.ratio(classification~., kddTrain1)

sampleImportance$names <- rownames(sampleImportance)
sampleImportance <- sampleImportance[order(-sampleImportance$attr_importance),]
rownames(sampleImportance) <- NULL
head(sampleImportance,21)
```

    ##    attr_importance                       names
    ## 1       0.70021600                   dst_bytes
    ## 2       0.65148723                   logged_in
    ## 3       0.64969261                       count
    ## 4       0.53391318 dst_host_srv_diff_host_rate
    ## 5       0.49470212              dst_host_count
    ## 6       0.37928819          srv_diff_host_rate
    ## 7       0.35505401                    duration
    ## 8       0.33992181                     service
    ## 9       0.32181007                   src_bytes
    ## 10      0.29859575 dst_host_same_src_port_rate
    ## 11      0.27794271               protocol_type
    ## 12      0.22177432                   srv_count
    ## 13      0.15417179      dst_host_diff_srv_rate
    ## 14      0.14631888          dst_host_srv_count
    ## 15      0.13877817      dst_host_same_srv_rate
    ## 16      0.12174526               diff_srv_rate
    ## 17      0.11101976                 serror_rate
    ## 18      0.10942371    dst_host_srv_serror_rate
    ## 19      0.10924771               same_srv_rate
    ## 20      0.10684924        dst_host_serror_rate
    ## 21      0.09241454             srv_serror_rate

The first 20 attributes ranked by information gain are within the same order of magnitude, showing a range between 0.7 and 0.1, after which the attribute information gain drops off to no information gain. It is important to note that one or more of these attributes may be significant for attack types not well-represented in this sample.

``` r
#Create a cTree classifier
control <- trainControl(method = "cv")
set.seed(742)
classifier1 <- train(classification ~ ., data = kddTrain1,method='ctree', metric ="Kappa", trControl = control)

classifier1
```

    ## Conditional Inference Tree 
    ## 
    ## 4900 samples
    ##   41 predictor
    ##    5 classes: 'dos', 'normal', 'probe', 'r2l', 'u2r' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 4410, 4411, 4409, 4409, 4410, 4410, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mincriterion  Accuracy   Kappa    
    ##   0.01          0.9940837  0.9822597
    ##   0.50          0.9940837  0.9822597
    ##   0.99          0.9940837  0.9822597
    ## 
    ## Kappa was used to select the optimal model using the largest value.
    ## The final value used for the model was mincriterion = 0.99.

``` r
train1Classes <- predict(classifier1, newdata = kddTest[,-42])

confusionMatrix(train1Classes, kddTest$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3932     15     0    0    0
    ##     normal    2    895    11    8    1
    ##     probe     0      2    34    0    0
    ##     r2l       0      0     0    0    0
    ##     u2r       0      0     0    0    0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.992           
    ##                  95% CI : (0.9891, 0.9943)
    ##     No Information Rate : 0.8029          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.975           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9995        0.9814     0.755556   0.000000  0.0000000
    ## Specificity              0.9845        0.9945     0.999588   1.000000  1.0000000
    ## Pos Pred Value           0.9962        0.9760     0.944444        NaN        NaN
    ## Neg Pred Value           0.9979        0.9957     0.997738   0.998367  0.9997959
    ## Prevalence               0.8029        0.1861     0.009184   0.001633  0.0002041
    ## Detection Rate           0.8024        0.1827     0.006939   0.000000  0.0000000
    ## Detection Prevalence     0.8055        0.1871     0.007347   0.000000  0.0000000
    ## Balanced Accuracy        0.9920        0.9879     0.877572   0.500000  0.5000000

![Conditional Inference Tree from Random Distribution Training Sample](https://github.com/danstd/kdd-1999-decision-tree-analysis/blob/main/classifier1Tree.png)

#### Adjusted Distribution Sample Decision Tree

``` r
set.seed(884)
sampleImportance2 <- gain.ratio(classification~., kddTrain2)

sampleImportance2$names <- rownames(sampleImportance2)
sampleImportance2 <- sampleImportance2[order(-sampleImportance2$attr_importance),]
rownames(sampleImportance2) <- NULL
head(sampleImportance2, 34)
```

    ##    attr_importance                       names
    ## 1       0.71072389                  root_shell
    ## 2       0.67701133                   logged_in
    ## 3       0.55808157                   dst_bytes
    ## 4       0.53995381              is_guest_login
    ## 5       0.52738493             num_compromised
    ## 6       0.51780356 dst_host_srv_diff_host_rate
    ## 7       0.44036476              dst_host_count
    ## 8       0.43772106                       count
    ## 9       0.42533121                     service
    ## 10      0.41110295                         hot
    ## 11      0.40134761           num_failed_logins
    ## 12      0.37743603          num_file_creations
    ## 13      0.37485570                    duration
    ## 14      0.35526590                   src_bytes
    ## 15      0.35123377          srv_diff_host_rate
    ## 16      0.32132832                   srv_count
    ## 17      0.31556157               protocol_type
    ## 18      0.28908064                    num_root
    ## 19      0.25385653 dst_host_same_src_port_rate
    ## 20      0.22641213          dst_host_srv_count
    ## 21      0.20147568      dst_host_diff_srv_rate
    ## 22      0.17580820      dst_host_same_srv_rate
    ## 23      0.16508409               diff_srv_rate
    ## 24      0.15774071        dst_host_serror_rate
    ## 25      0.15549503               same_srv_rate
    ## 26      0.15468087    dst_host_srv_serror_rate
    ## 27      0.15223610                 serror_rate
    ## 28      0.14463390        dst_host_rerror_rate
    ## 29      0.12153562                        flag
    ## 30      0.12076547    dst_host_srv_rerror_rate
    ## 31      0.11681717                 rerror_rate
    ## 32      0.11526370             srv_serror_rate
    ## 33      0.05606098             srv_rerror_rate
    ## 34      0.00000000                        land

For the adjusted distribution sample we see a much flatter distribution in information gain.

``` r
#Create a cTree classifier
control <- trainControl(method = "cv")
set.seed(742)
classifier2 <- train(classification ~ ., data = kddTrain2,method='ctree', metric ="Kappa", trControl = control)

classifier2
```

    ## Conditional Inference Tree 
    ## 
    ## 4900 samples
    ##   41 predictor
    ##    5 classes: 'dos', 'normal', 'probe', 'r2l', 'u2r' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 4410, 4410, 4409, 4410, 4411, 4409, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mincriterion  Accuracy   Kappa    
    ##   0.01          0.9747022  0.9392567
    ##   0.50          0.9747022  0.9392842
    ##   0.99          0.9704135  0.9289442
    ## 
    ## Kappa was used to select the optimal model using the largest value.
    ## The final value used for the model was mincriterion = 0.5.

``` r
train2Classes <- predict(classifier2, newdata = kddTest[,-42])

confusionMatrix(train2Classes, kddTest$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3906     11     0    0    0
    ##     normal   24    883    11    0    0
    ##     probe     0      2    34    0    0
    ##     r2l       4     16     0    8    0
    ##     u2r       0      0     0    0    1
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9861          
    ##                  95% CI : (0.9824, 0.9892)
    ##     No Information Rate : 0.8029          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9571          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9929        0.9682     0.755556   1.000000  1.0000000
    ## Specificity              0.9886        0.9912     0.999588   0.995912  1.0000000
    ## Pos Pred Value           0.9972        0.9619     0.944444   0.285714  1.0000000
    ## Neg Pred Value           0.9715        0.9927     0.997738   1.000000  1.0000000
    ## Prevalence               0.8029        0.1861     0.009184   0.001633  0.0002041
    ## Detection Rate           0.7971        0.1802     0.006939   0.001633  0.0002041
    ## Detection Prevalence     0.7994        0.1873     0.007347   0.005714  0.0002041
    ## Balanced Accuracy        0.9907        0.9797     0.877572   0.997956  1.0000000

![Conditional Inference Tree from Adjusted Distribution Training Sample](https://github.com/danstd/kdd-1999-decision-tree-analysis/blob/main/classifier2Tree.png)

### Comparison

The decision tree created from the adjusted distribution sample is much more complex, We see that the decision tree created from the random distribution training sample did perform better than the adjusted distribution training sample. This is not necessarily surprising, as the validation set had the same random distribution. The random distribution decision tree did fail to accurately classify any R2L or U2R attacks, which the adjusted distribution decision tree performed better on.

It is important to note that only a few attributes are shared between the two decision trees. Better performance may be acheived by cutting down the number of variables. Below, the attributes most relevant for separating U2R attack types and R2L attack types from other classification types are explored.

#### What are the most important attributes for U2R and R2L attacks?

In order to answer this question, we can re-run the information gain algorithm on a reclassified sample dataset. Shown below is the procedure applied to copies of the random sampled distribution training dataset. This is likely to give more appropriate results for the population dataset considering there similar distributions. The procedure performed on the adjusted distribution dataset, not shown, did show similar results however.

    ##   attr_importance         names
    ## 1    1.107851e-03       service
    ## 2    2.410384e-04 protocol_type
    ## 3    7.723363e-05          flag
    ## 4    0.000000e+00      duration
    ## 5    0.000000e+00     src_bytes
    ## 6    0.000000e+00     dst_bytes

    ##   attr_importance                       names
    ## 1     0.047165644 dst_host_srv_diff_host_rate
    ## 2     0.031246287              dst_host_count
    ## 3     0.009683846                       count
    ## 4     0.008706661                   srv_count
    ## 5     0.006445221                   logged_in
    ## 6     0.005048820                     service

We see that service, protocol type, and flag are the only attributes found by the information gain algorithm that have an impact on distinguising U2L attacks from normal traffic or other attack types. The attributes shown to have the greatest impact for R2L attacks were already represented in the decision trees seen above. Below, the attributes used in the decision trees above, along with Service, Protocol Type, and Flag attributes are subsetted from the kdd dataset. All other attributes besides classification are removed from the training samples.

### Decision Trees using targeted attributes

In order to leverage this knowledge, two new decision trees will be created, using only the attributes selected by the first set of decision trees, along with the service, protocol type, and flag attributes shown to be of importance for R2L and U2R attack types.

``` r
attributeList <- c("logged_in", "dst_host_srv_count", "src_bytes", "root_shell", "servicesmtp", "servicehttp", "count", "dst_hos_same_srv_rate", "srv_count", "dst_host_diff_srv_rate", "num_compromised", "protocol_typeudp", "dst_host_count", "wrong_fragment", "service", "protocol_type", "flag", "classification")
kdd <- kdd[,which(colnames(kdd) %in% attributeList)]
```

#### Create a CTree classifier from the random sample distribution with targeted attributes

``` r
#Create a cTree classifier
control <- trainControl(method = "cv")
set.seed(742)

kddTrain1 <- kddTrain1[,which(colnames(kddTrain1) %in% attributeList)]
classifier1Targeted <- train(classification ~ ., data = kddTrain1,method='ctree', metric ="Kappa", trControl = control)

classifier1Targeted
```

    ## Conditional Inference Tree 
    ## 
    ## 4900 samples
    ##   13 predictor
    ##    5 classes: 'dos', 'normal', 'probe', 'r2l', 'u2r' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 4410, 4411, 4409, 4409, 4410, 4410, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mincriterion  Accuracy   Kappa    
    ##   0.01          0.9942844  0.9829102
    ##   0.50          0.9942844  0.9829102
    ##   0.99          0.9934689  0.9804608
    ## 
    ## Kappa was used to select the optimal model using the largest value.
    ## The final value used for the model was mincriterion = 0.5.

``` r
train1Classes <- predict(classifier1Targeted, newdata = kddTest[,-42])

confusionMatrix(train1Classes, kddTest$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3932     15     0    0    0
    ##     normal    2    895    11    3    1
    ##     probe     0      2    34    0    0
    ##     r2l       0      0     0    5    0
    ##     u2r       0      0     0    0    0
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9931          
    ##                  95% CI : (0.9903, 0.9952)
    ##     No Information Rate : 0.8029          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9782          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9995        0.9814     0.755556   0.625000  0.0000000
    ## Specificity              0.9845        0.9957     0.999588   1.000000  1.0000000
    ## Pos Pred Value           0.9962        0.9814     0.944444   1.000000        NaN
    ## Neg Pred Value           0.9979        0.9957     0.997738   0.999387  0.9997959
    ## Prevalence               0.8029        0.1861     0.009184   0.001633  0.0002041
    ## Detection Rate           0.8024        0.1827     0.006939   0.001020  0.0000000
    ## Detection Prevalence     0.8055        0.1861     0.007347   0.001020  0.0000000
    ## Balanced Accuracy        0.9920        0.9885     0.877572   0.812500  0.5000000

![Conditional Inference Tree from Random Distribution Training Sample with Selected Attributes](https://github.com/danstd/kdd-1999-decision-tree-analysis/blob/main/classifier1TreeTargeted.png)

#### Create a CTree classifier from the adjusted sample distribution with targeted attributes

``` r
#Create a cTree classifier
control <- trainControl(method = "cv")
set.seed(742)

kddTrain2 <- kddTrain2[,which(colnames(kddTrain2) %in% attributeList)]
classifier2Targeted <- train(classification ~ ., data = kddTrain2,method='ctree', metric ="Kappa", trControl = control)

classifier2Targeted
```

    ## Conditional Inference Tree 
    ## 
    ## 4900 samples
    ##   13 predictor
    ##    5 classes: 'dos', 'normal', 'probe', 'r2l', 'u2r' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 4410, 4410, 4409, 4410, 4411, 4409, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mincriterion  Accuracy   Kappa    
    ##   0.01          0.9802061  0.9531678
    ##   0.50          0.9806155  0.9541743
    ##   0.99          0.9767326  0.9449372
    ## 
    ## Kappa was used to select the optimal model using the largest value.
    ## The final value used for the model was mincriterion = 0.5.

``` r
train2Classes <- predict(classifier2Targeted, newdata = kddTest[,-42])

confusionMatrix(train2Classes, kddTest$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3921     12     0    0    0
    ##     normal    7    861     4    0    0
    ##     probe     0      2    34    0    0
    ##     r2l       6     37     7    8    0
    ##     u2r       0      0     0    0    1
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9847          
    ##                  95% CI : (0.9809, 0.9879)
    ##     No Information Rate : 0.8029          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9525          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9967        0.9441     0.755556   1.000000  1.0000000
    ## Specificity              0.9876        0.9972     0.999588   0.989779  1.0000000
    ## Pos Pred Value           0.9969        0.9874     0.944444   0.137931  1.0000000
    ## Neg Pred Value           0.9866        0.9873     0.997738   1.000000  1.0000000
    ## Prevalence               0.8029        0.1861     0.009184   0.001633  0.0002041
    ## Detection Rate           0.8002        0.1757     0.006939   0.001633  0.0002041
    ## Detection Prevalence     0.8027        0.1780     0.007347   0.011837  0.0002041
    ## Balanced Accuracy        0.9921        0.9707     0.877572   0.994890  1.0000000

![Conditional Inference Tree from Adjusted Distribution Training Sample with Selected Attributes](Z:\classes\IS678\Assignment%201\classifier2TreeTargeted.png)

With targeted attributes we see that the random distribution decision tree and the adjusted distribution tree had very similar performance. The random distribution tree actually performed better against R2L and U2L attacks than the adjusted distribution tree, which had eight times as many records misidentified as R2L attacks than there actually were. This is likely the result of the adjusted distribution itself.

In terms of attack vs. normal traffic classification in prediction of the test set: The random distribution tree misidentified 17 attacks as normal traffic, and 17 normal traffic records were misidentified as attacks. The adjusted distribution tree misidentified 11 attacks as normal traffic, and 51 normal traffic records were misidentified as attacks.

Below, the two trees are used to predict the classification of the validation set:

Random Distribution Against Validation Set:

``` r
validation1Classes <- predict(classifier1Targeted, newdata = kddValidation[,-42])

confusionMatrix(validation1Classes, kddValidation$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3850     13     0    0    0
    ##     normal    2    977     2    4    3
    ##     probe     0      2    41    0    0
    ##     r2l       0      1     0    5    0
    ##     u2r       0      0     0    0    0
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9945         
    ##                  95% CI : (0.992, 0.9964)
    ##     No Information Rate : 0.7861         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9838         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9995        0.9839     0.953488   0.555556  0.0000000
    ## Specificity              0.9876        0.9972     0.999588   0.999796  1.0000000
    ## Pos Pred Value           0.9966        0.9889     0.953488   0.833333        NaN
    ## Neg Pred Value           0.9981        0.9959     0.999588   0.999183  0.9993878
    ## Prevalence               0.7861        0.2027     0.008776   0.001837  0.0006122
    ## Detection Rate           0.7857        0.1994     0.008367   0.001020  0.0000000
    ## Detection Prevalence     0.7884        0.2016     0.008776   0.001224  0.0000000
    ## Balanced Accuracy        0.9935        0.9905     0.976538   0.777676  0.5000000

Adjusted Distribution Against Validation Set:

``` r
validation2Classes <- predict(classifier2Targeted, newdata = kddValidation[,-42])

confusionMatrix(validation2Classes, kddValidation$classification)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  dos normal probe  r2l  u2r
    ##     dos    3847      7     0    0    0
    ##     normal    4    939     2    0    0
    ##     probe     0      2    41    0    0
    ##     r2l       1     45     0    9    2
    ##     u2r       0      0     0    0    1
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9871          
    ##                  95% CI : (0.9836, 0.9901)
    ##     No Information Rate : 0.7861          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9625          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: dos Class: normal Class: probe Class: r2l Class: u2r
    ## Sensitivity              0.9987        0.9456     0.953488   1.000000  0.3333333
    ## Specificity              0.9933        0.9985     0.999588   0.990186  1.0000000
    ## Pos Pred Value           0.9982        0.9937     0.953488   0.157895  1.0000000
    ## Neg Pred Value           0.9952        0.9863     0.999588   1.000000  0.9995918
    ## Prevalence               0.7861        0.2027     0.008776   0.001837  0.0006122
    ## Detection Rate           0.7851        0.1916     0.008367   0.001837  0.0002041
    ## Detection Prevalence     0.7865        0.1929     0.008776   0.011633  0.0002041
    ## Balanced Accuracy        0.9960        0.9720     0.976538   0.995093  0.6666667

The two decision trees showed similar performance against the validation set as the test set.

### Discussion

In this analysis we saw that adjusting class distribution had little affect on conditional inference tree performance. This is likely only the case as all classes were represented to some degree in all sets. It is also true that the attack classes were imbalanced to such a degree that with the first random distribution tree, the algorithm may have discounted classifying R2L and U2R attacks. We did see improvement in both the random distribution and adjusted distribution trained decision trees when attributes were selected specifically rather than using all available. More domain knowledge would likely lead to better selection of attributes.

### References

CUP-99 Task Description. (n.d.). Retrieved September 17, 2020, from <http://kdd.ics.uci.edu/databases/kddcup99/task.html>

Malato, G. (2019, May 07). Stratified sampling and how to perform it in R. Retrieved September 17, 2020, from <https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef>
