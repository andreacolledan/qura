# Installation
## Prerequisites

Building QuRA requires the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) (Stack), which automatically fetches the required GHC version and the necessary build dependencies. On top of that, you'll need the following external dependencies:

- [git](https://git-scm.com) (to obtain the source code),
- [cvc5](https://cvc5.github.io) (to run QuRA).

Note that cvc5 must be present in your `PATH` for QuRA to work properly.

The following instructions have been tested on Ubuntu 24.10 and MacOS Sequoia 15.3.2.

## Build and install using Stack

Once your external dependencies are set up, you can install QuRA with the following commands:

```txt
git clone https://github.com/andreacolledan/qura
cd qura
stack install
```

To check that QuRA is correctly installed, run:

```txt
$ qura

Missing: FILE

Usage: qura FILE [-v|--verbose] [-d|--debug DEBUG] [--noprelude] 
            [-g|--global-metric-analysis METRIC] 
            [-l|--local-metric-analysis METRIC]

  Verify the resource consumption of the program in FILE according to the chosen METRIC.
```
