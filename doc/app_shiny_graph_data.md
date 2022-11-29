---
title: 'CATCH: Code description of the shiny app'
author: Jérôme Pasquier
date: Tuesday, November 29, 2022
papersize: a4
fontsize: 10pt
fontfamily: arev
numbersections: true
geometry:
- top=25mm
- left=20mm
- heightrounded
output:
  pdf_document:
    latex_engine: xelatex
---

<!--

Compilation ot the current document:
pandoc --shift-heading-level-by=-1 -o output.pdf input.pdf

Code location of the shiny app:
https://github.com/jpasquier/unisante-catch
-> R/app_shiny.R

-->

## REDCap data

  * Select only participants (1st instrument)

  * Select the following variables:

    * `cat_ec_participant_sid` as `id`

    * `cat_e_participant_type` as `indexCase`

    * `cat_s_familycode` as `fid`

    * `cat_s_arm` as `armExp`

    * `cat_l_result` as `positive`

    * `cat_dp_sexe_v2` as `sexM`

    * `cat_dp_prenom_v2` as `firstName`

  * Note that variables `armExp` and `sexM` are interpreted as `1=Yes/2=No`
    variables

  * Convert `1=Yes/2=No` variables to `0=No/1=Yes`.

  * For each selected REDCap lines, define the variable `participate=1`


## Graph data

Loop on adjacency matrix files


### Family ID

Get family id (`fid`) from filename


### Format adjacency matrix for use in R

  * Read CSV file as a R matrix

  * First row as column names and first column as row names

  * Replace NAs with empty strings

  * Trim character strings

  * Limit IDs to 10 characters

### Graph nodes (part 1)

  * Left join (by IDs) of the IDs contained in the matrix with the data
    extracted from redcap. Hence the REDCap variables are missing for the
    non-participant.

  * Replace the `NA` values of the `indexCase` and `partcipate` variables by
    `0` values.

  * Replace `NA` values in the `armExp` variable with the first non-missing
    value found for the family.

### Graph edges

  * Convert the adjacency matrix into a list of oriented edges. For each edge,
    the following variables are defined:

    * `from`: Start node

    * `to`: Target node

    * `label`: Family link (father, mother, brother, sister, son or daughter)

  * Only keep the edges that move away from the index case (desymmetrization).

    * Convert edge list in an igraph object

    * Compute distances between nodes using the `distances` function from the
      `igraph` package

    * Keep only the distances from the index case to the other nodes

    * Keep only the edges whose distance between the start node and the index
      case node is smaller than the distance between the target node and the
      index case node

### Graph nodes (part 2)

  * Filling in missing values for the `sexM` variable: The `sexM` variable for
    non-participants is not in the REDCap database. However, it can be inferred
    from the family links in the edge list.

  * Define generations within the family: A homemade code is used in the shiny
    app. However, it must be possible to determine the generations by
    calculating for each node the distances from the index case considering
    weights on the edges (-1 if child, 0 if sibling, 1 if parent).

  * Determine relatives who are not eligible:

    * If one parent of a positive case is positive, then the other parent is
      not eligible.

    * Relatives of a negative case, who is not an index case, are not
      eligible.

    Potential conflict: Not eligible, but participate.

  * Determine label, shape and color of the nodes:

    * Label: first name or ID (user choice)

    * Shape:

      ```
      if (sexM == 1):
        shape = 'square'
      else if (sexM == 0):
        shape = 'dot'
      else:
        shape = 'diamond'
      ```

    * Color:

      ```
      if (positive == 1):
        color = 'red'
      else if (positive == 0):
        color = 'green'
      else if (participate == 1):
        color = 'grey'
      else if (nonEligible == 1):
        color = 'brown'
      else:
        color = 'black'
      ```
