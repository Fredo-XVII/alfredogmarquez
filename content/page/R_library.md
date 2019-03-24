---
title: "R Library"
categories:
- References
- Resources
- Books
- Links
tags:
- R books
- R references
- R resources
- R Programming
- R Links
keywords:
- R
- r
- R programming
comments:       false
showMeta:       false
showActions:    false
#thumbnailImage: //example.com/image.jpg
---

### R Workspace
The Workspace: http://www.statmethods.net/interface/workspace.html

### Hadley
  - R 4 Data Science: http://r4ds.had.co.nz/
  - http://stat545.com/bit001_dplyr-cheatsheet.html
  - Writing Packages: http://r-pkgs.had.co.nz/

### RStudio Server
https://www.r-bloggers.com/setting-up-a-datascience-server/

-----

### Visualization (Graphing and Plotting and Mapping and Dashboarding)

#### Graphing & Plotting: 

  - https://www.ggplot2-exts.org/gganimate.html
  - http://motioninsocial.com/tufte/?utm_content=bufferc1618&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  - https://briatte.github.io/ggcorr/
  - https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
  - https://plot.ly/feed/
  - https://plotly-book.cpsievert.me/index.html - book on Plotly and R 
  - https://www.r-graph-gallery.com/
  - Radial: http://rstudio-pubs-static.s3.amazonaws.com/72298_c1ba7f77276a4f27a0f375cadc9fac5d.html

#### Mapping & Cartography:

  - **Census**
    - ACS 
     - Package: http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf
     - https://www.r-bloggers.com/how-to-search-for-census-data-from-r/
     - https://rudeboybert.github.io/MATH216/jekyll/update/2016/11/02/HW-4.html

  - **Mapping**
    - Simple Features (sf) - https://cran.r-project.org/web/packages/sf/vignettes/sf1.html
    - https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html
    - http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
    
  - **Spacial Analysis**
    - https://www.zevross.com/ 
    - https://rpubs.com/ajlyons/rspatialdata
    
#### Dashboards:
  
  - http://lenkiefer.com/2017/01/16/cross-talk-dashboard/

-----

### Spark

  - https://spark.apache.org/docs/latest/sparkr.html#starting-up-from-rstudio
  - https://spark-packages.org/
  - https://blog.rstudio.org/2016/09/27/sparklyr-r-interface-for-apache-spark/
  - Installing on Windows:
    - https://jaceklaskowski.gitbooks.io/mastering-apache-spark/content/spark-tips-and-tricks-running-spark-windows.html
  - http://blog.danielemaasit.com/2015/07/26/installing-and-starting-sparkr-locally-on-windows-8-1-and-rstudio/
  
  - So far:
    - Environmental Variables: added 
      - R_HOME : 
      - JAVA_HOME :
      - SPARK_HOME : 
      - HADOOP_HOME :
      - WINUTIS_HOME :
        - https://github.com/steveloughran/winutils
        
  - ODBC/JDBC:
    - https://jaceklaskowski.gitbooks.io/mastering-apache-spark/content/spark-sql-thrift-server.html
    
-----

### Econometrics

  - Heckman Selection: https://cran.r-project.org/web/packages/sampleSelection/vignettes/selection.pdf

-----

### Machine Learning
  - Twitter Kmeans     
    - http://thinktostart.com/cluster-twitter-data-with-r-and-k-means/
    - https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

-----

### Tutorials
  UC-R:
  UCLA: https://stats.idre.ucla.edu/r/seminars/ggplot2_intro/
  
-----

### Databases

  - R and SQLite: Part 1: https://www.r-bloggers.com/r-and-sqlite-part-1/
  - R and SpaciaLite: http://www.gaia-gis.it/gaia-sins/spatialite-cookbook/index.html#toc
  - http://www.gaia-gis.it/gaia-sins/
  - R and Hive JDBC: https://cwiki.apache.org/confluence/display/Hive/HiveServer2+Clients#HiveServer2Clients-JDBC
  - R and Googlesheets: http://shiny.rstudio.com/articles/persistent-data-storage.html
  - R and Teradata: https://developer.teradata.com/blog/odbcteam/2016/02/r-with-teradata-odbc

-----

### RAM Memory

  - https://stat.ethz.ch/R-manual/R-devel/library/utils/html/memory.size.html

-----

### Library and Package management

  - https://www.stat.osu.edu/computer-support/mathstatistics-packages/installing-r-libraries-locally-your-home-directory

-----

### Rprofile and R Environmental variables ( related to Library Management above )

  - https://csgillespie.github.io/efficientR/3-3-r-startup.html

-----

### Docker
  - https://ropenscilabs.github.io/r-docker-tutorial/
  - Container Package: https://o2r.info/2017/05/30/containerit-package/
  - https://www.codeguru.com/csharp/csharp/cs_internet/using-r-with-docker-engine.html#Item4
  - https://www.rocker-project.org/
  - https://www.symbolix.com.au/blog-main/r-docker-hello
  - **EntryPoint vs. CMD:** http://goinbigdata.com/docker-run-vs-cmd-vs-entrypoint/
  
-----

### Writing Packages
  - No. 1 - Hadleys book bove: 
  - Looks like a nice resource: https://kbroman.org/pkg_primer/
    - Great layman explanation of software licenses: MIT vs. GPL
