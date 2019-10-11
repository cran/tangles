![Build Status](https://travis-ci.com/brendo1001/tangles.svg?token=55jxxyTm43o2mnqrHBvX&branch=master)

# Tangles
An R package for anonymization of spatial point patterns and raster objects.

This package achieves the relatively simple, yet pretty useful task of spatial anonymization. Anonymization is needed in situations where a data owner may not wish to share the actual spatial locations of their data, but allows sharing of the data that does not preclude the data from a spatial analysis procedure. 

Anonymization is achieved via 3 modes of spatial shifts:

* Vertical shifts
* Lateral shifts
* Rotational shifts

The `tangles` package can entangle both non-gridded spatial point patterns and raster objects. It can also entangle data using an a priory entanglement sequence, and can disentangle data back to their original spatial representations. Each entanglement process is given a unique hash key label to guarantee disentanglement is a success in terms of data being transformed back to their original representation. 

## Package installation

Installation of `tangles` can be achieved via [Github](https://github.com/) using the `devtools` package:

`devtools::install_github("brendo1001/tangles")`

## Contact
`brendan.malone@csiro.au`
