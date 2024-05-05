# hhill

`hhill()` implements the Huntington-Hill method of legislative
 seat allocation which is currently used by the
 United States House of Representatives.
 The Huntington-Hill method is calculated as follows:
 each entity represented in the chamber
 is assigned a score, calculated as the population of that
 entity divided by the square root
 of  \emph{n}(\emph{n}+1), where \emph{n} is the current number of seats
 allocated to that entity. All entities are assigned
 the minimum number of seats, and then scores are calculated.
 The entity with the highest score receives a seat,
 its \emph{n} value increments by 1, and then scores are recalculated.
 The new entity with the highest score receives a seat,
 increments its \emph{n}, and the process repeats until all available
 seats have been allocated.
 
 ## Installation
 
 `devtools::install_github("aadams149/hhill")`
