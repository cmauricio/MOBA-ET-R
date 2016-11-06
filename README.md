# Eye-tracking Analysis MOBA
R code used in the eye tracking data from the MOBA project
By Carlos Mauricio Castaño Díaz

# List of files (in alphabetic order)

 - AnovasDota.R
 - AnovasLoL.R
 - AnovasLoLDota.R
 - autoAnalyse.R
 - Descriptives.R
 - ETautoAnalysis_func.R
 - getFileData_func.R
 - IsoVarsCleanOutliers.R
 - Malsbrug-fixSac_func.R
 - ManovasDota.R
 - ManovasLoL.R
 - ManovasLoLDota.R
 - MapFilter_func.R
 - MultilevelICCDota.R
 - MultilevelICCLol.R
 - NAs.R
 - Outliers.R
 - runETanalysis.R
 - SaccadesTimeRatio.R
 - SubsettingDatabases.R

# Description of the files

#AnovasDota.R#

Different Anova anayses for the eye tracking data for DOTA2 data. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations given in the ANOVA.

#AnovasLoL.R#

Different Anova anayses for the eye tracking data for LOL data. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations given in the ANOVA.

#AnovasLoLDota.R#

Different Anova anayses for the eye tracking data for LOL and DOTA2 data. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations given in the ANOVA.

#autoAnalyse.R#

Code for automatically detecting saccades and fixations in all the files containing eye tracking data while separating it into data from map and minimap.
The output of this code is a database with the saccades, fixations, mean of fixations and standard deviation of fixations for each of the observations from the eye tracking data while differentiating the data by type (game or minimap AoIs).

#Descriptives.R#

Code for getting the Descriptive Statistics for LOL and DOTA2 using the package 'Psych' from CRAN.

#ETautoAnalysis_func.R#

This function is meant to automatically process the data raw data given by the pupil eye-tracking software taking into account the AoIs for the present study (game maps are separated from the minimaps).
The parameters Directory is the directory the data are.
The parameter Datatype is meant to chose how to process and save the data. Parameters accepted here are 'Map' and 'Minimap'.
The parameter Lambda is the lambda value from Malsbrug algorithm.

This function also nest the function detect.saccades entirely developed by Titus von der Malsburg. I copied it here because the function was not working in the CRAN package, but worked perfectly when run from the source code.

#getFileData_func.R#

Function for taking out all the data from different (same structure)Databases in a specific directory.
The function can also be used to extract the name of files in a specific directory.

#IsoVarsCleanOutliers.R#

This code is made to isolate the relevant variables from the pre-processed eye-tracking data and to clean the outliers that are higher or lower than three standard deviations from the mean.

This code uses the function 'clean.outliers' contained in the file 'Outliers.R'

#Malsbrug-fixSac_func.R#

This function is developed entirely by Titus von der Malsburg. It is the main function to detect saccades and fixations which I separated because the CRAN package was not working well. Following, a description of the code by the author

Functions for the detection of fixations in raw eye-tracking data.
Offers a function for detecting fixations in a stream of eye positions recorded by an eye-tracker.  The detection is done using an algorithm for saccade detection proposed by Ralf Engbert and Reinhold Kliegl (see reference below).  Anything that happens between two saccades is considered to be a fixation.  This software is therefore not suited for data sets with smooth-pursuit eye movements.
Ralf Engbert, Reinhold Kliegl: Microsaccades uncover the orientation of covert attention, Vision Research, 2003.

#ManovasDota.R#

MANOVA analyses for DOTA expertise groups, including post-hoc ANOVAs for detecting sources of difference. The standard analysis of variance uses Bonferroni correction. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations.

#ManovasLoL.R#

MANOVA analyses for LOL expertise groups, including post-hoc ANOVAs for detecting sources of difference. The standard analysis of variance uses Bonferroni correction. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations.

#ManovasLoLDota.R#

MANOVA analyses for LOL and DOTA expertise groups, including post-hoc ANOVAs for detecting sources of difference. The standard analysis of variance uses Bonferroni correction. This file also contains a function to find eta squared based on the F value, the degrees of freedom, and the number of observations.

#MapFilter_func.R#

This is a function created to separate data belonging to the different AoIs depending on the value of the variable 'on_srf' given by the pupil eye-tracker raw data. If the value is TRUE it means that the data data analysed corresponds to that particular AoI.

#MultilevelICCDota.R#
Multilevel ICC analyses for DOTA data.
These analysis were not included in the thesis or the article.

#MultilevelICCLol.R#

Multilevel ICC analyses for LOL data.
These analysis were not included in the thesis or the article.

#NAs.R#

The purpose of these functions are to detect and extract or eliminate missing values (NA). 
The missing values are extracted or eliminated row-wise. That is to say that when a missing value is found for any variable the  whole row of data (observation) is deleted.

#Outliers.R#

Functions to extract outliers and to Clean outliers (data with more than three SD from the mean).

#runETanalysis.R#

This code runs the eye tracking analyses for all the raw data and uses the function 'eyetracking.analysis' found in the file ETautoAnalysis_func.R.

Lambda of 2 was chosen as the best fitting lambda for 60Hz eye-tracking data, taking into account different test measurements carried, and the suggestions from von der Malsburg.

#SaccadesTimeRatio.R#

Code developed to find the amount of fixations and saccades per second and aggregate them to the clean databases (databases without NA or outliers) agregating four new variables to the study.

#SubsettingDatabases.R#
Code created to separate databases according to game, expertise, and condition.
