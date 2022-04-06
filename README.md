# PovertyTargeting
Collect presentation and replications inspired by poverty targeting reading group.

## Where do I find the interesting parts of this repository?
### Empirical work
Data cleaning happens in "Replication Brown, Ravallion, van de Walle/Scripts/" and the files "A_Clean.R" and "B_PMT.R".
More interesting are the targeting predictions with traditional econometrics and some big data techniques. These happen in "Summary Verme/uganda_estimation.R"

### Presentation of results
To look at the results see the html-based presentation at the link: 
The (somewhat) interactive html-based presentation is created using CSS (via the xaringan package) in R Markdown. The code is in "Summary Verme/Slides.Rmd", but the repository needs to be copied to your local computer if you want to run the code. To see the resulting presentation better follow the link given above. 

## Replication Brown, Ravallion, van de Walle paper (A Poor Means Test, JDE 2018)
Use Uganda LSMS ISA data from 2011/12 to replicate their results and use cleaned data as basis for testing other ML algorithms, out-of-sample testing, etc...

## Summary Verme WP (Which Model for Poverty Predictions?)
For reading group, summarise working paper by Paolo Verme and - eventually - redo their empirical analysis with the Uganda data used in A Poor Means Test, JDE 2018.
