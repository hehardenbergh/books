


=======================================================
README for "SOCIAL CHARACTERS: THE HIERARCHY OF GENDER"
=======================================================

Author: Eve Kraicer and Dr. Andrew Piper
Date: August 2018
=======================================================

These scripts generate the models, calculations and plots for "Social Characters: The Hierarchy of Gender in Contemporary English-Language Fiction" (Kraicer and Piper 2018). Using a collection of 1,333 novels published between 2001 and 2015 across seven groups (Prizewinners, NYT Reviewed, Bestsellers, Romance, Science Fiction, Young Adult Fiction and Mystery) this script analyses the patterns of gender visibility and gendered connectedness. 

The first script, 'hierarchy.R' has 3 sections: data, visibility and connectivity. In Section I, the data is processed and adjusted, and basic measurements are run on character distribution, author genders, and genre breakdowns. In Section II, the visibility of characters that are men and women are analyzed at three levels: all characters, main character and top pairs. In Section III, the patterns of gendered connectivity are analyzed through three models: assortativity, antagonism and balance.

The second script, 'hierarchy_adj.R' is a help script which uses hand validated gender predictions to adjust the number of men and women based on sensitivity and specificity calculations. It is sourced in hierarchy.R.  

Preprocessing 
=============

For every novel, the edges and nodes lists were derived from BookNLP by David Bamman which can be found here (https://github.com/dbamman/book-nlp). A gender prediction was added through a modification on BookNLP by Eva Portelance (2017).

To validate the gender predictions of outputs, a sample was collected from there necessary subsets of data, and then hand validated.

Every novel was also given metadata. This includes manually labelled for author gender as M (man), W (woman) or X (non-binary, either because of gender identity or multiple authorial signatures). The novels were then assigned a point of view (POV) of either 1p (first person) or 3p (third person) based on a machine learning algorithm with SVM classification based on pronoun distributions that was developed for this project using Python and R. The results of this are reported in the paper, and the code can be made available by contacting Eve Kraicer. 


Requirements 
============

This project requires the following: 

* R + Rstudio (optional)
* hierarchy_adj.R (helper code to run adjustment measures)
	* 'validation_1p.csv' (first person novels only)
	* 'validation_tp.csv' (top pairs only)
	* 'validation_all.csv' (all characters) 
* 'hierarchy_metadata.csv' (metadata w ID, genre, author gender, author name, pov)
* 'edges_ALL' (directory of characters each novel, w gender prediction and frequency)
* 'nodes_ALL' (directory of interactions in each novels, w sentiment scores)

For certain measures, a supplementary dataset is needed 'tokens_ALL' which was used to calculate the main character in 1p novels. 


Installation 
============

* https://cran.r-project.org/bin/macosx/
* https://www.rstudio.com/products/rstudio/download/


Usage 
======

Change wd to location of files.
Ensure running all helper functions throughout paper, and that hierarchy_adj.R is loaded using 'source'

Section I:
- - - - - -

1
---
1a: reads in data and creates necessary subsets of metadata
1b: creates TABLE ONE 
1c: creates FIGURE ONE and calculates ranks x mentions. 
	- must be run twice, first on all works, then only on works w main character was calculated. 
	- builds FIGURE 1 using ggplot2
1d: creates FIGURE TWO to calculate the number of men and women characters by rank position. 
	- can be calculated at four subsets, with accompanying conditionals: 
		- all works (raw)
		- all works with main character (raw_sub)
		- women's works (raw_w)
		- women's works with main character (raw_sub_w)
	- data is then cleaned and counted
	- builds FIGURE 2 using ggplot2


Section II:
- - - - - -

1: ALL CHARACTERS
------------------
1a: using hierarchy_adj.R, replace M and ? Characters with women to achieve adjusted counts, then bootstrap the adjusted values. 
1b: calculates reported values for bootstraps on various subsets
1c: calculates reported values for bootstraps, removing romance
1d: calculates significance across genre and gender
1e: builds TABLE 2
1f: builds FIGURE 3 and 4 after adjusting values
	- replaces characters in 1 and 2 position based on values calculated at 2b
	- builds FIGURE 3 and 4 using ggplot2

2: MAIN CHARACTER & TOP PAIR
-----------------------------

2a: prepares data building master table with genders of top pairs from each document
	- uses different methods for 1p and 3p pov novels
	- REQUIRES tokens_ALL data
2b: adjusts and cleans data based on different validation calculations for 1p/3p novels, then calculates main character and top pairs and bootstraps outputs. 
2c: builds TABLE 3 & 4
2d: calculates reported values for bootstraps on various subsets
2e: calculates significance across genre and gender

Section III:
- - - - - -

1: ASSORTATIVITY
-----------------

1a: determines the adjustments required for each novel based on prevalence calculations, and splits into those requiring a bootstrap (< 1 character gender must be replaced) and not (> 1 character gender must be replaced)
1b: calculates assortativity three ways 
	- no bootstrap
	- bootstrap
	- a control w permutations
1c: calculates reported values on various subsets
1d: calculates significance across genre and gender 
1e: builds FIGURE 5 and 6 using ggplot2

2: ANTAGONISM
--------------
2a: calculates antagonism with and without bootstrap, conditioning on gender type
2b: calculates reported values on various subsets
2c: calculates significance across genre and gender

3: BALANCE
-----------
3a: calculate balance, conditioning on gender of trio and on balance/imbalance of said trio
3b: calculates reported values on various subsets
3c: calculates significance across genre and gender
3d: builds FIGURE 8 using ggplot2