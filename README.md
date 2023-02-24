# mental_accident
Mental health after a free time sport accident

## Summary

Herein, we investigated patterns of mental phenomena in a cross-sectional cohort of victims of leisure time sport accidents in Tyrol, Austria. 
The analysis techniques encompassed semi-supervised PAM clustering, canonical hypothesis testing and machine learning with oneR and conditional random forest algorithms.

<p align = "center"> 
<img src = "https://user-images.githubusercontent.com/80723424/221284887-7971c65c-de72-4f15-b596-fdd3e37b4836.png" width = "50%">
</p>

By clustering in respect to measures of anxiety, depression, somatization, panic, resilience, sense of coherence, quality of life, post-traumatic growth and post-traumatic syndrom disorder signs, three subsets of accident victims were identified:

- __neutral cluster__: individuals without symptoms of deteriorating mental health or post-traumatic symptom disorder but also without signs of post-traumatic growth

- __PTG cluster__: named after post-traumatic growth which ist the key hallmark of the cluster. 

- __PTSD cluster__: characterized by the highest intensity of post-traumatic syndrome disorder (PTSD) signs along with substantial anxiety, depression, somatic symptoms, loss of quality of life, reduced coherence and limited resilient coping

You may follow the analysis progress [here](https://github.com/PiotrTymoszuk/mental_accident/tree/main/paper).

## Usage

The analysis pipeline requires some development packages, the easiest way to install them is to use `devtools`:

```r

devtools::install_github('PiotrTymoszuk/ExDA')
devtools::install_github('PiotrTymoszuk/trafo')
devtools::install_github('PiotrTymoszuk/clustTools')
devtools::install_github('PiotrTymoszuk/soucer')
devtools::install_github('PiotrTymoszuk/figur')
devtools::install_github('PiotrTymoszuk/caretExtra')

```
To launch the entire pipeline, source the `exec.R` file:

```r

source('exec.R')

```

## Terms of use

The pipeline results will be included in a future publication. To reference and use analysis results, please cite our GitHub repository; the data sources listed below and, if available, the publication. In any questions, please contact [Dr. Katharina Hüfner](mailto:katharina.huefner@tirol-kliniken.at).

## Contact

The maintainer of the repository is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Any data requests should be addressed to the senior author [Dr. Katharina Hüfner](mailto:katharina.huefner@tirol-kliniken.at).

