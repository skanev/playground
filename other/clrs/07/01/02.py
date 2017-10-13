def partition(numbers, start = 0, end = None):
    last = end - 1 if end else len(numbers) - 1

    pivot_value = numbers[last]
    pivot       = start
    repetitions = 0

    for i in range(start, last):
        value = numbers[i]

        if value < pivot_value:
            numbers[pivot+repetitions], numbers[i] = numbers[i], numbers[pivot+repetitions]
            numbers[pivot+repetitions], numbers[pivot] = numbers[pivot], numbers[pivot+repetitions]
            pivot += 1
        elif value == pivot_value:
            numbers[pivot+repetitions], numbers[i] = numbers[i], numbers[pivot+repetitions]
            numbers[pivot+repetitions], numbers[pivot] = numbers[pivot], numbers[pivot+repetitions]
            repetitions += 1
	
    numbers[pivot+repetitions], numbers[last] = numbers[last], numbers[pivot+repetitions]
    return pivot + repetitions // 2

def quicksort(numbers, start = 0, end = None):
    end = end if end else len(numbers)

    if start < end - 1:
        pivot = partition(numbers, start, end)
        quicksort(numbers, start, pivot)
        quicksort(numbers, pivot + 1, end)
