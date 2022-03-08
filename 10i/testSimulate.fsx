open simulate
let line times = String.replicate times "–"

printfn"\nTest of Drone class"
printfn "%s\n" (line 19)
printfn"Test of Fly"
printfn "%s" (line 11)
let drone1 = new Drone((0,0), (5,10), 1, "Drone 1")
let drone2 = new Drone((-5, -10), (0,0), 1, "Drone 2")
drone1.Fly()
drone2.Fly()
printfn"|%5b| %s startPos:%9s newPos: %8A" (drone1.Position = (0,1)) drone1.Name (string drone1.Start) drone1.Position
printfn"|%5b| %s startPos:%9s newPos: %8A" (drone2.Position = (-5,-9)) drone2.Name (string drone2.Start) drone2.Position
printfn "%s" (line 51)
printfn""


printfn"Test of atDestination"
printfn "%s" (line 21)
let drone3 = new Drone((0,0), (5,5), 3, "Drone 3")
let drone4 = new Drone((23,5), (-40,3), 18, "Drone 4")
let drone5 = new Drone((20,10), (100,200), 2, "Drone 5")
let air1 = new Airspace()
air1.AddManyDrones([drone3;drone4;drone5])
for i in 1..15 do
  air1.FlyDrones()
printfn"|%5b| %s Start:%8s Des:%10s drone arrived"  (drone3.AtDestination) drone3.Name (string drone3.Start) (string drone3.Destination)
printfn"|%5b| %s Start:%8s Des:%10s drone arrived"  (drone4.AtDestination) drone4.Name (string drone4.Start) (string drone4.Destination)
printfn"|%5b| %s Start:%8s Des:%10s drone not arrived"  (not (drone5.AtDestination)) drone5.Name (string drone5.Start) (string drone5.Destination)
printfn "%s" (line 63)
printfn""

let drone6 = new Drone((0,0), (4,9), 1, "Drone 6")
printfn "Test if drones stop after atDestination = true"
printfn "%s" (line 46)
for i in 1..10 do 
  drone6.Fly()
  printfn"Seconds|%2s| Currentpos|%7s| atDestination|%5b|" (string i) (string drone6.Position) (drone6.AtDestination)
printfn "%s" (line 52)
printfn"\n\n"

// Blackbox Airspace
printfn "Test of Airspace class"
printfn "%s\n" (line 22)

printfn"Test of AddDrone"
printfn "%s" (line 16)
let drone9 = new Drone((56,30), (0,0), 3, "Drone 9")
let drone10 = new Drone((0,0), (0,0), 1, "Drone 10")
let air4 = new Airspace()
air4.AddDrone drone9
air4.AddDrone drone10
printfn "|%5b| Airspace is not empty" (not (air4.Drones |> List.isEmpty))
printfn "|%5b| Added in correct order" (air4.Drones = [drone9; drone10])
printfn "%s" (line 30)
printfn""

printfn"Test of DroneDistance"
printfn "%s" (line 21)
let drone7 = new Drone((10,10), (0,0), 1, "Drone 7")
let drone8 = new Drone((7,6), (0,0), 1, "Drone 8")

let air3 = new Airspace()
air3.AddManyDrones([drone7;drone8])
printfn "|%5b| correct distance" (air3.DroneDist(drone7)(drone8) = 5.0)
printfn "%s" (line 24)
printfn""


printfn "Test of FlyDrones()"
printfn "%s" (line 19)
for i in 1..6 do
  air3.FlyDrones()
printfn "|%5b| Correct new position for all" (drone7.Position = (4, 4) && drone8.Position = (1, 1))
printfn "%s" (line 37)
printfn""

printfn"Test of willCollide"
printfn "%s" (line 19)
let air5 = new Airspace()
let drone11 = new Drone((0,0), (10,10), 1, "Drone 11")
let drone12 = new Drone((1,0), (0,10), 1, "Drone 12")
let drone13 = new Drone((0,0), (10,0), 2, "Drone 13") // tester at dronerne bliver fjerne efter kollition 
let drone20 = new Drone((506,0), (0,0), 3, "Drone 20")// ellers ville drone13 ramme drone20 efter den faktisk var faldet ned
air5.AddManyDrones [drone11; drone12; drone13; drone20]

air5.willCollide(1)
printfn "|%5b| Collitions are added" (not (air5.willCollide(1) |> List.isEmpty))
printfn "|%5b| Drone 20 is still flying" ((air5.Drones |> List.map (fun x -> x.Name)) = ["Drone 20"])
printfn "|%5b| All three collitions are added to list" (air5.CollideList = [("Drone 11", "Drone 12"); ("Drone 11", "Drone 13"); ("Drone 12", "Drone 13")])
printfn "-"
printfn"%A" (air5.willCollide(1))
printfn"%A" (air5.CollideList)
let air6 = new Airspace()
let drone14 = new Drone((0,0), (100,100), 1, "Drone 14")
let drone15 = new Drone((1000,230), (34,10), 1, "Drone 15")
let drone16 = new Drone((0,5000), (0,10), 1, "Drone 16")
air6.AddManyDrones [drone14; drone15; drone16]

air6.willCollide(2)
printfn "|%5b| No drones will collide" (air6.willCollide(1) |> List.isEmpty)
printfn "%s" (line 30)
