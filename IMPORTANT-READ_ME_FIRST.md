# My-Msc-Graphic
Interactive ROC curve for Msc project

To run on R:
1. Download MAofDTA.R and madaedit.tar.gz
2. Open MAofDTA.R within RStudio and install all the packages within library() commands - except for "mada"
3. If your R console already has "mada" installed this needs to be manually removed:
   i) Find where you set your R working directory to be (can use the command getwd() if unsure)
   ii) Go to the working directory then -> R -> win-library -> 3.3 (version number)
   iii) You should now see all the packages installed in folders. Delete the "mada" folder
4. Download the ammended mada package:
   i) In RStudio go to the 'Packages' tab in the bottom right panel
   ii) Select 'Install'
   iii) Select 'Package Archive File (.zip; .tar.gz)' from the 'Install from:' drop-down menu at the top
   iv) Browse within your computer and select the madaedit.tar.gz file that was downloaded at the beginning
   v) Click 'Install'
5. Your console should now have the ammended mada package. This is the exact same as the original mada package [1], and so functions as standard but has an additional option. When using plot(reitsma.object), to include the SROC curve, HSROC has to be set to true (e.g. plot(fit.reitsma, HSROC=T,...))
6. Now all the packages should be installed the application can be run. Simply clikc 'Run App' in the top right hand corner of the R Script panel and the apploication should open.

[1] Philipp Doebler (2014). mada: Meta-Analysis of Diagnostic Accuracy (mada). R package version 0.5.6.
  https://CRAN.R-project.org/package=mada
