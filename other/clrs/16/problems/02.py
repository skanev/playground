import unittest
import random
import sys

def subarray(n):
    return (2**n - 1, 2**(n+1) - 1)


def binary_search(array, span, n):
    left, right = span
    while left < right:
        mid = (left + right) // 2
        if array[mid] == n:
            return mid
        elif n < array[mid]:
            right = mid
        else:
            left = mid + 1

    return None


def correct(array, span, index):
    start, stop = span

    while start < index and array[index - 1] > array[index]:
        array[index - 1], array[index] = array[index], array[index - 1]
        index -= 1

    while index < stop - 1 and array[index] > array[index + 1]:
        array[index + 1], array[index] = array[index], array[index + 1]
        index += 1


def advance(seq):
    try:
        return next(seq)
    except StopIteration:
        return None


def subseq(array, span):
    for i in range(*span):
        yield array[i]


def merge(left, right):
    a = advance(left)
    b = advance(right)

    while a != None and b != None:
        if a < b:
            yield a
            a = advance(left)
        else:
            yield b
            b = advance(right)

    while a is not None:
        yield a
        a = advance(left)

    while b is not None:
        yield b
        b = advance(right)


class DBS:
    def __init__(self):
        self.n = 0
        self.items = [None]


    def insert(self, item):
        if self.n == len(self.items):
            self.items.extend([None] * (len(self.items) + 1))

        to_merge = []
        c, n = 0, self.n

        while n % 2 == 1:
            c += 1
            n //= 2

        seq = subseq([item], (0, 1))

        for i in range(0, c):
            seq = merge(seq, subseq(self.items, subarray(i)))

        for (i, e) in zip(range(*subarray(c)), seq):
            self.items[i] = e

        self.n += 1


    def __contains__(self, n):
        return self.find(n) != None


    def find(self, n):
        for (i, span) in self.full_indices():
            index = binary_search(self.items, span, n)
            if index != None:
                return (i, index)

        return None


    def full_indices(self):
        n, i = self.n, 0

        while n > 0:
            if n % 2 == 1:
                yield i, subarray(i)
            i += 1
            n //= 2


    def delete(self, n):
        index = self.find(n)

        if not index:
            return

        removed = 0

        left = self.n
        while left % 2 == 0:
            removed += 1
            left //= 2

        if index[0] > removed:
            a, b = subarray(removed)[0], index[1]
            self.items[a], self.items[b] = self.items[b], self.items[a]

            correct(self.items, subarray(index[0]), b)

            index = (removed, a)

        i = 0
        for k in range(*subarray(removed)):
            if k == index[1]:
                continue

            self.items[i] = self.items[k]
            i += 1

        self.n -= 1


    def values(self):
        result = []

        for (i, span) in self.full_indices():
            result += self.items[span]

        result.sort()
        return result
