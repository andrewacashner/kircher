# kircher

| Andrew A. Cashner, University of Rochester
| acashner@ur.rochester.edu

## Description 

Project on Athanasius Kircher's composition machine from _Musurgia
universalis_ (Rome, 1650), algorithmic composition, discrete
mathematics and combinatorics in 17th-century music.

## Digital implementation of the _Arca musarithmica_

First attempt in C, then in Scheme (Guile with GOOPS object-oriented library)
with XML input.
Now in Haskell.

## Build and Run

This project is managed with [Stack](http://www.haskellstack.org). 
Follow the installation instructions on the Stack website for your system.

Then to build: `stack build`

To run: `stack run`

## Build Documentation

This project generates its own documentation, just say: `stack haddock`

To browse the docs: `stack haddock --open`
