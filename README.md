# alineR
alineR is an R package that makes available Kondrak's _aline_ algorithm. _aline_ is a phonetic feature-based word similarity score. The underlying alignment algorithm was developed by Grzegorz Kondrak.

# History

alineR was originally published on CRAN (https://cran.r-project.org/package=alineR), and it is now being made available on GitHub for download and use. 

# Installation

Use this command to install alineR_2.0:

remotes::install_github("egkopf/alineR")

# Description

Functions are provided to calculate the 'ALINE' Distance between words as per (Kondrak 2000) and (Downey, Hallmark, Cox, Norquest, & Lansing, 2008, <doi:10.1080/09296170802326681>). The score is based on phonetic features represented using the Unicode-compliant International Phonetic Alphabet (IPA). Parameterized features weights are used to determine the optimal alignment and functions are provided to estimate optimum values using a genetic algorithm and supervised learning. See (Downey, Sun, and Norquest 2017, <https://journal.r-project.org/archive/2017/RJ-2017-005/index.html>.)
