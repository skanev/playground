guesses := 10
guess := nil
lastGuess := nil
number := (Random value * 100) floor + 1

loop(
    lastGuess = guess
    guess = File standardInput readLine("Your guess: ") asNumber
    guesses = guesses - 1

    (guess == number) ifTrue(writeln("You got it. Congratulations!"); break)
    (guesses == 0) ifTrue(writeln("Sorry, you failed. The number was: ", number); break)

    lastGuess ifNonNil(
        thisDistance := (number - guess) abs
        lastDistance := (number - lastGuess) abs
        if(thisDistance > lastDistance, "Colder", "Hotter") print
    ) ifNil (
        "Nope" print
    )
    writeln(". ", guesses, " guesses left")
)
