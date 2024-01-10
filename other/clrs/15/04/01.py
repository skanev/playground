def offline_caching(accesses, cache_size):
    next = {}
    for (i, k) in enumerate(accesses):
        next.setdefault(k, []).append(i)

    for k in next:
        next[k].append(len(accesses) + 1)
        next[k].reverse()

    cache = set()
    for (i, k) in enumerate(accesses):
        assert next[k][-1] == i

        if k in cache:
            print(f"{i:2}: hit  {k} (cache: {cache})")
        elif len(cache) < cache_size:
            print(f"{i:2}: miss {k} (cache: {cache}), added")
            cache.add(k)
        else:
            evict = max(cache, key=lambda item: next[item][-1])
            print(f"{i:2}: miss {k} (cache: {cache}), evict {evict}")
            cache.remove(evict)
            cache.add(k)

        next[k].pop()
