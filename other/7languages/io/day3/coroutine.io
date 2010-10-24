vizzini := Object clone
vizzini talk := method(
    "Fezzik, are there rocks ahead?" println
    yield
    "No more rhymes now, I mean it" println
    yield
)

fezzik := Object clone
fezzik rhyme := method(
    yield
    "If there are, we'll all be dead" println
    yield
    "Anybody want a peanut?" println
)

vizzini @@talk; fezzik @@rhyme
Coroutine currentCoroutine pause
