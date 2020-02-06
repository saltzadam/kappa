# kappa 

Diana Hubbard and I defined an invariant of transverse links in our paper [An Annular Refinement of the Transverse Invariant in Khovanov Homology](https://arxiv.org/abs/1507.06263).  This program computes the invariant.  I have written a [much better package] for computing link-homological invariants, but I haven't added kappa to it yet.  This package will remain here until I do.

Some documentation is available on [Hackage](http://hackage.haskell.org/package/braid).  A summary:

### main/Computekappa.hs

This is the executable.  At the command line,

    ./Computekappa [1,2,2,-4,-3,5] 6
 
will compute kappa for the braid with braid word sigma_1 sigma_2 sigma_2 -sigma_4^{-1} sigma_3^{-1} sigma_5 and width 6:

[IMAGE]

### src/Braids.hs

All the topology happens here.  Defines the data types `Braid`, `Diagram`, and `PD` (planar diagram) ala [Knotatlas](http://katlas.org/wiki/Planar_Diagrams).  The function `cubeOfResolutions` takes a braid as input and returns a list of resolved diagrams.  (This function should really have type `Braid -> Map Resolution Diagram` -- I take this approach in a [more recent package].)

### src/Complex.hs

A chain complex is a vector space with a self-map whose square is zero.  With coefficients in Z2, we can represent a chain complex as a finite function, i.e. a `Map`.  The self-map is represented as `Map Generator (Set Generator)` where `Generator` is a standard Khovanov generator.  

In `Cancellation.hs`, homology is computed using the cancellation algorithm, see Section ? of [this paper] by Baldwin.  At some point Diana and I decided we'd like to find complete chain representatives for homology classes.  To do so, we need to keep track of all the cancellations, in order.  Actually, we don't need all the cancellations -- some are redundant, while others can be combined.  The data type `Cancellations` is a wrapper on `Map Generator (Set Generator)` which keeps track of cancellations.  It has the structure of a monoid: the composition of two cancellation is a chain complex which represents both cancellations.

### src/Kh.hs

Computes the Khovanov chain complex of a `Diagram`.  It's pretty easy to generate the chain group using standard `Generator`s.  The tricky part is describing the morphisms.  (I do a better job [here].) 

This also contains the function `psi :: Braid -> Generator` which computes the chain for the transverse invariant.

### src/Cancellation.hs

The implementation of the cancellation algorithm, see Section ? of [this paper] by Baldwin.  Our algorithm also respects the annular filtration on the Khovanov complex of a closed braid.
