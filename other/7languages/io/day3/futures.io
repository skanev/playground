Lobby countAMillion := method(
    writeln("Counter started")
    for(i, 1, 1000000, "")
    "Counter done"
)
counter := Lobby @countAMillion 
writeln("Starting counter...")
writeln(counter)
writeln("Exiting")
