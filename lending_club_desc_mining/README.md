# Mining Lending Club Loan Descriptions with Naive Bayes

In this project I used naive Bayes classifiers to extract information from the text descriptions of Lending Club loan applications. I also included a cute wordcloud visualization of the text, courtesy of the R `wordcloud` package.

This data is a favorite of mine, as it is a messy real-world set with a lot of interesting challenges.  In particular, I find it very difficult to perform predictive analytics that produces better results than the analytics Lending Club has already provided in the form of their loan grading.

Naive Bayes classifiers are attractive for their simplicity and interpretability, but pay for that with inflexibility. As such, I was not surprised to find that in this case their raw predictive power by themselves was very weak. The important product of this project, to me, was the set of posterior credit event probabilities, given the loan descriptions. I will definitely use this metric as an input to a more sophisticated model next time I revisit this data set.

### Files

##### desc_naive_bayes.md

Project report in GitHub markdown format. I recommend you read this if you intend to read in-browser.

##### desc_naive_bayes.pdf

Project report in pdf format.

##### desc_naive_bayes.Rmd

R markdown document used to generate the report files (md and pdf).

##### desc_naive_bayes.R

Script for the project, without any of the text explanation. Suitable for running yourself.

##### README.md

This file.
