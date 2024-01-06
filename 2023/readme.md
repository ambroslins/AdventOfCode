# Advent of Code 2023

### Total runtime
| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./AoC2023 +RTS -N1` | 1.054 ± 0.012 | 1.042 | 1.072 | 1.48 ± 0.02 |
| `./AoC2023 +RTS -N2` | 0.820 ± 0.008 | 0.812 | 0.832 | 1.15 ± 0.01 |
| `./AoC2023 +RTS -N4` | 0.714 ± 0.004 | 0.712 | 0.722 | 1.00 |
| `./AoC2023 +RTS -N8` | 0.773 ± 0.012 | 0.752 | 0.793 | 1.08 ± 0.02 |
| `./AoC2023 +RTS -N16` | 0.971 ± 0.022 | 0.943 | 1.013 | 1.36 ± 0.03 |

Using GHC-9.6.3 (`-O2`) in WSL2 on a Ryzen 7 5700X with 32GB RAM.

### Benchmarks
| Day    | Parser  | Part 1  | Part 2  | Total   | Allocated | Copied |
| ------ | -------:| -------:| -------:| -------:| ---------:| ------:|
| Day 01 | 44.6 μs | 22.0 μs | 338  μs | 428  μs | 3.6 MB    |  31 KB |
| Day 02 | 91.4 μs | 139  ns | 163  ns | 93.5 μs | 935 KB    | 1.4 KB |
| Day 03 | 131  μs | 244  μs | 207  μs | 737  μs | 2.5 MB    | 122 KB |
| Day 04 | 278  μs | 680  ns | 6.51 μs | 290  μs | 2.4 MB    | 2.9 KB |
| Day 05 | 27.2 μs | 13.5 μs | 55.0 μs | 106  μs | 519 KB    | 3.1 KB |
| Day 06 | 235  ns | 63.1 ns | 1.79 μs | 2.25 μs |  17 KB    |   9 B  |
| Day 07 | 71.4 μs | 532  μs | 547  μs | 1.23 ms | 5.0 MB    | 157 KB |
| Day 08 | 61.6 μs | 111  μs | 842  μs | 1.45 ms | 1.1 MB    |  28 KB |
| Day 09 | 227  μs | 59.8 μs | 59.3 μs | 310  μs | 3.4 MB    |  67 KB |
| Day 10 | 27.1 μs | 980  μs | 2.19 ms | 2.22 ms | 9.3 MB    | 1.6 MB |
| Day 11 | 57.1 ns | 665  μs | 664  μs | 1.36 ms | 687 KB    | 3.7 KB |
| Day 12 | 204  μs | 430  μs | 6.15 ms | 6.97 ms |  49 MB    | 658 KB |
| Day 13 | 85.7 μs | 254  μs | 258  μs | 480  μs | 1.6 MB    |  32 KB |
| Day 14 | 15.2 μs | 24.9 μs | 35.1 ms | 35.4 ms | 106 MB    | 2.3 MB |
| Day 15 | 386  μs | 23.7 μs | 170  μs | 690  μs | 4.1 MB    | 509 KB |
| Day 16 | 17.9 μs | 331  μs | 87.0 ms | 87.1 ms | 719 MB    | 2.2 MB |
| Day 17 | 37.9 μs | 24.1 ms | 57.3 ms | 82.0 ms | 212 MB    |  67 MB |
| Day 18 | 101  μs | 20.4 μs | 20.0 μs | 176  μs | 1.3 MB    |  22 KB |
| Day 19 | 284  μs | 60.5 μs | 108  μs | 559  μs | 3.0 MB    | 102 KB |
| Day 20 | 6.16 μs | 3.88 ms | 9.50 ms | 13.6 ms |  50 MB    | 2.4 MB |
| Day 21 | 24.2 μs | 1.56 ms | 24.2 ms | 25.9 ms | 165 MB    | 1.5 MB |
| Day 22 | 245  μs | 22.6 ms | 92.9 ms | 92.9 ms | 165 MB    | 1.3 MB |
| Day 23 | 27.7 μs | 896  μs | 352  ms | 353  ms | 1.2 GB    | 433 KB |
| Day 24 | 364  μs | 455  μs | 1.53 ms | 2.48 ms |  10 MB    |  76 KB |
| Day 25 | 195  μs | 2.45 ms | -       | 2.71 ms |  12 MB    | 605 KB |

Each benchmarks is single threaded.
