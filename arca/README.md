# kircher

Andrew A. Cashner, University of Rochester

<mailto:acashner@ur.rochester.edu>

## Description 

Project on Athanasius Kircher's composition machine from _Musurgia
universalis_ (Rome, 1650), algorithmic composition, discrete
mathematics and combinatorics in 17th-century music.

## License

Everything in this repository is copyright © 2021 by Andrew A. Cashner.
All rights are reserved, except you may download the repository and compile
your own copy of the program.

## Digital implementation of the _Arca musarithmica_

Implemented in Haskell with MEI output (and possibility for Lilypond output).

The first attempt was in C with Lilypond output, then in Scheme with MEI
output.

## Build and Run

This project is managed with [Stack](http://www.haskellstack.org). 
Follow the installation instructions on the Stack website for your system.

1. Clone the repository: `git clone git@bitbucket.org:andrewacashner/kircher.git`
2. Go to the project directory: `cd kircher/arca`
3. Build the executable: `stack build`
4. Install: `stack install`
5. Run: `arca INFILE OUTFILE`
    - Use '-' for INFILE to read from standard input 
        - Example: `cat file.xml | arca - file.mei`
    - Use '-' for OUTFILE to write to standard output (default outfile name
      is `musica.mei`)
        - Example: `cat file.xml | arca - - > file.mei`

### Note: Static Build

This project is configured to build two executables: 

1. `arca` -- dynamically linked, "normal" type of binary executable built by `stack`
2. `arca-exe` -- statically linked, self-contained binary executable

I need the statically linked executable for use on the server for the web app
at <http://www.arca1650.info>.
This may make it more difficult to build on your system, though, because it
requires different system resources (static C libraries, the `lld` linker).

To change this, just comment out the entry for `arca-exe` from the
`executables` section of the `package.yaml` file.

## Build Documentation

This project generates its own documentation, just say: `stack haddock`

To only build the docs for this project, not the included system libraries:
`stack haddock --no-haddock-deps`

To browse the docs: `stack haddock --open`

The `doc` directory is a symbolic link to the Haddock html directory, so
you can also open `doc/index.html` in the browser (e.g., `firefox doc/index.html &`).

