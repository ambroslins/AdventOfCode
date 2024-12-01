# Advent of Code 2023

## Total runtime
| Threads | Mean     | 
|--------:|---------:|
|       1 | 810.9 ms | 
|       2 | 533.0 ms | 
|       4 | 383.3 ms | 
|       8 | 333.4 ms | 
|      16 | 426.4 ms | 

Using GHC-9.6.3 (`-O2` and no parallel GC `-qg`) in WSL2
on a Ryzen 7 5700X with 32GB RAM.
Time measured with [hyperfine](https://github.com/sharkdp/hyperfine).
Solutions are executed in sequence but each solution may use multiple threads.

## Benchmarks
| Day    | Parser  | Part 1  | Part 2  | Total   | Allocated | Copied |
| ------ | -------:| -------:| -------:| -------:| ---------:| ------:|
| Day 01 | 45.5 μs | 18.8 μs | 338  μs | 436  μs |    3.6 MB |  34 KB |
| Day 02 | 93.3 μs | 148  ns | 151  ns | 96.4 μs |    935 KB | 1.8 KB |
| Day 03 | 136  μs | 246  μs | 209  μs | 745  μs |    2.5 MB | 124 KB |
| Day 04 | 280  μs | 622  ns | 6.51 μs | 287  μs |    2.4 MB | 4.1 KB |
| Day 05 | 27.2 μs | 13.4 μs | 57.1 μs | 103  μs |    519 KB | 3.4 KB |
| Day 06 | 234  ns | 64.1 ns | 1.88 μs | 2.38 μs |     17 KB |  18 B  |
| Day 07 | 72.2 μs | 563  μs | 580  μs | 1.28 ms |    5.0 MB | 159 KB |
| Day 08 | 58.5 μs | 114  μs | 827  μs | 1.46 ms |    1.1 MB |  31 KB |
| Day 09 | 233  μs | 63.3 μs | 60.8 μs | 315  μs |    3.4 MB |  70 KB |
| Day 10 | 27.5 μs | 1.02 ms | 2.25 ms | 2.33 ms |    9.3 MB | 1.6 MB |
| Day 11 | 60.9 ns | 668  μs | 680  μs | 1.37 ms |    688 KB | 4.4 KB |
| Day 12 | 212  μs | 430  μs | 3.78 ms | 7.75 ms |     51 MB | 774 KB |
| Day 13 | 89.7 μs | 257  μs | 266  μs | 485  μs |    1.6 MB |  33 KB |
| Day 14 | 14.7 μs | 26.1 μs | 35.4 ms | 35.4 ms |    105 MB | 2.5 MB |
| Day 15 | 405  μs | 22.5 μs | 178  μs | 725  μs |    4.1 MB | 509 KB |
| Day 16 | 18.0 μs | 981  μs | 17.3 ms | 17.5 ms |    720 MB | 2.6 MB |
| Day 17 | 38.5 μs | 24.2 ms | 57.1 ms | 81.6 ms |    212 MB |  65 MB |
| Day 18 | 103  μs | 20.8 μs | 21.0 μs | 184  μs |    1.3 MB |  23 KB |
| Day 19 | 296  μs | 64.0 μs | 111  μs | 582  μs |    3.0 MB | 103 KB |
| Day 20 | 6.20 μs | 3.85 ms | 9.47 ms | 13.4 ms |     50 MB | 2.1 MB |
| Day 21 | 24.7 μs | 1.63 ms | 24.1 ms | 26.3 ms |    165 MB | 1.6 MB |
| Day 22 | 247  μs | 23.2 ms | 56.0 ms | 36.5 ms |    166 MB | 1.4 MB |
| Day 23 | 28.1 μs | 926  μs | 59.5 ms | 60.3 ms |    1.2 GB | 278 KB |
| Day 24 | 382  μs | 460  μs | 1.75 ms | 2.71 ms |     10 MB |  81 KB |
| Day 25 | 203  μs | 2.45 ms |         | 2.74 ms |     12 MB | 612 KB |

Results are from [tasty-bench](https://hackage.haskell.org/package/tasty-bench) using
`--time-mode wall +RTS -t -N8`.
The _Total_ is a separate benchmark and does not necessarily equal the sum of
_Parser_, _Part 1_ and _Part 2_.
Memory usage estimates are from the _Total_ benchmark.
The `peak memory` is omitted as it is skewed by other benchmarks.
