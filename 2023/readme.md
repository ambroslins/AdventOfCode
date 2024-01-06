# Advent of Code 2023

## Total runtime
| Threads | Mean     | 
|--------:|---------:|
|       1 | 821.1 ms | 
|       2 | 545.2 ms | 
|       4 | 421.4 ms | 
|       8 | 397.8 ms | 
|      16 | 541.6 ms | 

Using GHC-9.6.3 (`-O2`) in WSL2 on a Ryzen 7 5700X with 32GB RAM.
Time measured with [hyperfine](https://github.com/sharkdp/hyperfine)

## Benchmarks
| Day    | Parser  | Part 1  | Part 2  | Total   | Allocated | Copied |
| ------ | -------:| -------:| -------:| -------:| ---------:| ------:|
| Day 01 | 62.6 μs | 23.1 μs | 445  μs | 561  μs |    3.6 MB |  17 KB |
| Day 02 | 128  μs | 142  ns | 152  ns | 132  μs |    932 KB | 977 B  |
| Day 03 | 176  μs | 259  μs | 226  μs | 829  μs |    2.5 MB |  61 KB |
| Day 04 | 384  μs | 656  ns | 7.03 μs | 390  μs |    2.4 MB | 2.1 KB |
| Day 05 | 36.6 μs | 15.6 μs | 63.7 μs | 130  μs |    519 KB | 1.7 KB |
| Day 06 | 333  ns | 68.1 ns | 2.39 μs | 2.98 μs |     17 KB |   9 B  |
| Day 07 | 93.8 μs | 655  μs | 638  μs | 1.44 ms |    5.0 MB |  80 KB |
| Day 08 | 88.7 μs | 110  μs | 822  μs | 1.53 ms |    1.1 MB |  12 KB |
| Day 09 | 341  μs | 77.6 μs | 76.8 μs | 449  μs |    3.4 MB |  37 KB |
| Day 10 | 34.4 μs | 1.31 ms | 2.07 ms | 2.11 ms |    9.3 MB | 500 KB |
| Day 11 | 89.6 ns | 641  μs | 653  μs | 1.31 ms |    612 KB | 2.0 KB |
| Day 12 | 272  μs | 548  μs | 6.97 ms | 8.11 ms |     51 MB | 402 KB |
| Day 13 | 122  μs | 278  μs | 281  μs | 542  μs |    1.6 MB |  18 KB |
| Day 14 | 19.3 μs | 29.2 μs | 38.3 ms | 38.4 ms |    104 MB | 1.9 MB |
| Day 15 | 457  μs | 24.7 μs | 196  μs | 797  μs |    4.1 MB | 257 KB |
| Day 16 | 22.7 μs | 617  μs | 30.4 ms | 31.3 ms |    711 MB | 1.2 MB |
| Day 17 | 48.7 μs | 27.0 ms | 54.3 ms | 85.6 ms |    211 MB |  42 MB |
| Day 18 | 140  μs | 25.7 μs | 25.5 μs | 236  μs |    1.3 MB |  12 KB |
| Day 19 | 368  μs | 70.5 μs | 138  μs | 681  μs |    3.0 MB |  52 KB |
| Day 20 | 8.72 μs | 4.53 ms | 10.7 ms | 15.4 ms |     50 MB | 2.3 MB |
| Day 21 | 31.2 μs | 1.98 ms | 31.1 ms | 32.6 ms |    165 MB | 810 KB |
| Day 22 | 336  μs | 29.0 ms | 60.9 ms | 41.8 ms |    164 MB | 1.2 MB |
| Day 23 | 35.9 μs | 1.19 ms | 69.7 ms | 68.5 ms |    1.2 GB | 173 KB |
| Day 24 | 466  μs | 461  μs | 2.08 ms | 3.17 ms |     10 MB |  43 KB |
| Day 25 | 257  μs | 2.77 ms |         | 3.11 ms |     12 MB | 303 KB |

Results are from [tasty-bench](https://hackage.haskell.org/package/tasty-bench) using
`--time-mode wall +RTS -t -N8 -A8m`.
The _Total_ is a separate benchmark and does not necessarily equal the sum of
_Parser_, _Part 1_ and _Part 2_.
Memory usage estimates are from the _Total_ benchmark.
The `peak memory` is omitted as it is skewed by other benchmarks.
