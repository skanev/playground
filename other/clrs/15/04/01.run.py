offline_caching = __import__("01").offline_caching

accesses = [3, 2, 2, 4, 1, 2, 3, 2, 1, 4, 3]
print(f"Blocks: {' '.join(map(str, accesses))}\n")
offline_caching(accesses, 2)
