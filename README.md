# Fish Habitat Enhancement Project -- Fish Sampling

## Purpose
The intention this project is to determine the efficacy of utilizing oyster reefs to enhance the production of 
recreationally and commercially important juvenile finfish, such as:
<li>
<ul>Tautog</ul>
<ul>Black Sea Bass</ul>
<ul>Scup</ul>
<ul>Winter Flounder</ul>
<ul>Summer Flounder</ul>
</li>

The experiment takes a BACI design approach where we have 4 replicates of 3 treatments:
<li>
<ul><strong>Control </strong>(unimproved substrate)</ul>
<ul><strong>Unseeded reef </strong>(no oysters, only shell)</ul>
<ul><strong>Seeded reef </strong>(oysters and shell)</ul>
</li>

## Data cleaning & exploratory analysis

### data_import_and_cleaning.R
This script reads in:
<li>
<ul>Gillnet_Query.txt</ul>
<ul>EelPot_Query.txt</ul>
<ul>MinnowTrap_Query.txt</ul>
</li>
and saves them as R objects (also included).

<br>
These scripts contain the 2016 fish sampling by 3 sampling methods.

### compare_size_dist.R
This script reads in data from our long term seine survey to compare fish caught by length with our 3 sampling methods.
This comparison will allow us to determine whether our sampling methods are properly representing the fish
assemblage in the ponds.

### Work in Progress
We are currently working on comparing the 3 treatments across the season to determine if treatment has an
effect on species composition and abundance.
