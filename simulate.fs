module simulate

/// <summary> Udregner afstanden mellem to coordinater ved pytagoras </summary>
/// <param name="from"> første koordinat </param>
/// <param name="t"> adnet koordinat </param>
/// <return> afstanden som float </return>
let EuclideanDistance (from:int*int) (t:int*int) : float =
    let x = float (fst t - fst from)
    let y = float (snd t - snd from)
    sqrt(x**2. + y**2.)

/// <summary> En klasse der repressenterer en drone </summary>
///<param name="p"> Startpositionen for dronen </param>
///<param name="d"> Dronens destination </param>
///<param name="s"> Dronens fart i meter pr. sekund </param>
///<param name="n"> Dronens navn </param>
/// <return> Et drone objekt </return>
type Drone (p:int*int, d:int*int, s:int, n:string) = 
    let mutable position = p
    let mutable destination = d
    let mutable speed = s
    let mutable name = n

    /// <summary> Dronens startposition </summary>
    member this.Start
        with get () = p

    /// <summary> Dronens nuværende position </summary>
    member this.Position 
        with get () = position 
        and set (pos) = position <- pos

    /// <summary> dronens fart </summary>
    member this.Speed
        with get () = speed

    /// <summary> dronens destination </summary>
    member this.Destination
        with get () =  destination

    /// <summary> Dronens navn </summary>
    member this.Name = name 
    
  
    /// <summary> Flyver dronen et sekund </summary>
    /// <returns> unit </returns>
    member this.Fly () =
        let distance = (EuclideanDistance this.Position this.Destination)
        match this.AtDestination with
        |true -> ()
        |false when (distance <= (float this.Speed)) -> this.Position <- this.Destination
        |_ -> let ratio = (float this.Speed) / distance
              let stepX = float (fst this.Destination - fst this.Position) * ratio
              let stepY = float (snd this.Destination - snd this.Position) * ratio
              let Newpos = ((fst this.Position) + int (round stepX), (snd this.Position) + int (round stepY))
              this.Position <- Newpos
    

    /// <summary> Tjekker om dronen er ankommet til sin destination </summary>
    /// <returns> Boolean </returns>
    member this.AtDestination : bool = (this.Position = this.Destination)




///<summary> En klasse der representerer luftrummet </summary>
///<returns> Et Airspace objekt </returns>
type Airspace () = 
    let mutable dronelist : Drone list = []
    let mutable collidedDrone : (Drone*Drone) list = []

    
    /// <summary> Liste af alle droner </summary> 
    member this.Drones = dronelist


    /// <summary> Udregner afstanden mellem to droner </summary>
    /// <param name="drone1"> A drone </param>
    /// <param name="drone2"> A drone </param>
    /// <returns> Dronernes afstand som float </returns>
    member this.DroneDist (drone1: Drone) (drone2: Drone) : float = 
        EuclideanDistance drone1.Position drone2.Position


    /// <summary> flyver alle droner i luftrummet et sekund </summary>
    /// <return> unit </return>
    member this.FlyDrones() = 
        for elm in this.Drones do elm.Fly()


    /// <summary> Tilføjer droneobjekt til luftrummet </summart>
    /// <param name="drone"> Drone objekt der skal tilføjes </param>
    /// <return> unit </return>
    member this.AddDrone (drone : Drone) = 
        dronelist <- dronelist @ [drone]


    /// <summary> Tilføjer flere droneobjekter til luftrummet </summart>
    /// <param name="x"> liste af droneobjekter der skal tilføjes </param>
    /// <return> unit </return>
    member this.AddManyDrones (x:Drone list) =
        dronelist <- dronelist @ x


    /// <summary> Tjekker om dronerne i luftrummet kolliderer i et givet interval </summary>
    /// <remark> Fjerner de kolliderede droner fra dronelisten </remark>
    /// <param name="minutes"> int for intervallet der skal tjekkes givet i minutter </param>
    /// <returns> En liste af de kolliderede droner som Drone tuples </returns>  
    member this.willCollide (minutes: int) : (Drone*Drone) list =
        for i in 1..(minutes*60) do // ændre det fra minutter til sekunder 
            this.FlyDrones()
            for j in dronelist do
                let jIndex:int = dronelist |> List.findIndex (fun x -> x=j)
                for k in dronelist.[jIndex+1..dronelist.Length-1] do // Hvis flere kolliderer falder alle til jorden
                    let Dronedone = j.AtDestination  || k.AtDestination 
                    if (EuclideanDistance j.Position k.Position) < 500.0 && not Dronedone then
                        collidedDrone <- collidedDrone @ [(j, k)]
                    else ()
            for i in collidedDrone do //Så de ikke flyver videre og bliver testet for kollition igen
                dronelist <- dronelist |> List.except [fst i] 
                dronelist <- dronelist |> List.except [snd i] 
        collidedDrone //|> List.distinct


    /// <summary> Laver en list af de kolliderede droners navn </summary>
    /// <return> En liste af de kolliderede droner i string tuples </return>
    member this.CollideList: (string * string) list =
        let mutable list : (string * string) list = []
        for elm in collidedDrone do
            list <- list @ [((fst elm).Name, (snd elm).Name)]
        list //|> List.distinct
                    
        

