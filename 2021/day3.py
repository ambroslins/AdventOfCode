import numpy as np

numbers = np.array([[c == '1' for c in line.strip()] for line in open('inputs/3.txt')])

def bits_to_int(xs):
    return np.dot(np.flip(xs), 2**np.arange(xs.shape[0]))

def solve1(nums):
    gamma = np.sum(nums, 0) >= (nums.shape[0] / 2)
    epsilon = 1 - gamma
    return bits_to_int(gamma) * bits_to_int(epsilon)

print(solve1(numbers))

def solve2(nums):
    oxygen = nums.copy()
    co2 = nums.copy()
    for i in range(nums.shape[1]):
        b1 = np.sum(oxygen[:, i]) >= (oxygen.shape[0] / 2)
        oxygen = oxygen[oxygen[:, i] == b1]
        if len(co2) > 1:
            b2 = np.sum(co2[:, i]) < (co2.shape[0] / 2)
            co2 = co2[co2[:, i] == b2]
    return bits_to_int(oxygen[0]) * bits_to_int(co2[0])

print(solve2(numbers))