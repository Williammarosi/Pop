module Robots

open System

type Direction = North | South | East | West

type Position = int * int

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore

/// <summary> En klasse der repressenterer det visuelle board </summary>
/// <param name = "rows"> Antallet af rækker for et board som int </param>
/// <param name = "cols"> Antallet af kolloner for et board som int </param>
/// <return> Et board objekt </return>
type BoardDisplay (rows:int, cols:int) =
    let board : string[,] = Array2D.init (rows*2+1) (cols*2+1) (fun i j -> match (i,j) with
                                                                            |(i,j) when (i = 0 || i = rows*2) && j % 2 = 0 && j <> 0 -> "+"
                                                                            |(i,j) when j % 2 = 0 && i % 2 = 0 ->  "+" 
                                                                            |(i,j) when j % 2 = 1 && (i=0 || i=rows*2) -> "––"
                                                                            |(i,j) when i % 2 = 1 && (j=0 || j=cols*2) -> "|"
                                                                            |_ -> " ")
    /// <summary> For at hente antallet af rækker </summary>
    member this.R = rows
    /// <summary> For at hente antallet af kolloner </summary>
    member this.C = cols
    /// <summary> Sætter indhold ind på en givet placering i arrayet </summary>
    /// <param name = "r"> Første koordinat for hvor indholdet skal være som int </param>
    /// <param name = "c"> Andet koordinat for hvor indholdet skal være som int </param>
    /// <param name = "cont"> Det string der skal være i koordinatet </param>
    /// <return> unit </return>
    member this.Set (r, c, cont) = board.[r*2-1,c*2-1] <- cont
    /// <summary> Indsætter den visuelle representation af en vandret væg </summary>
    /// <param name = "row"> Første koordinat for væggens placering som int </param>
    /// <param name = "col"> Andet koordinat for væggens placering som int </param>
    /// <remark> Sætter væggen syd for koordinatet
    /// <return> unit </return>
    member this.SetBottomWall (row, col) = board.[row*2,col*2-1] <- "––"
    /// <summary> Indsætter den visuelle representation af en lodret væg </summary>
    /// <param name = "row"> Første koordinat for væggens placering som int </param>
    /// <param name = "col"> Andet koordinat for væggens placering som int </param>
    /// <remark> Sætter væggen øst for koordinatet
    /// <return> unit </return>
    member this.SetRightWall (row, col) = board.[row*2-1,col*2] <- "|"
    /// <summary> Printer boardet til terminalen </summary>
    /// <return> unit </return>
    member this.Show () =   board |> Array2D.iteri (fun i j v -> match (i, j) with
                                                                 |(i, j) when j = 0 -> printf " %s" v
                                                                 |(i, j) when j % 2 = 0 && j <> cols*2 && j<>1 -> printf "%s" v
                                                                 |(i, j) when j = cols*2 -> printfn "%s" v
                                                                 |_ -> printf "%2s" v)



[<AbstractClass >]
type BoardElement () =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __.Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __.GameOver _ = false
    abstract member Name : string

/// <summary> En klasse der repressenter robotterne </summary>
/// <param name = "row"> Første koordinat for robottens placering som int </param>
/// <param name = "col"> Andet koordinat for robottens placering som int </param>
/// <param name = "name"> robottens navn som string </param>
/// <remark> Robottens navn bruges kun til at vælge den og vises ikke på boardet </remark>
/// <return> Et robot objekt </return>
and Robot(row:int, col:int, name:string) =
    inherit BoardElement()
    let mutable pos = (row, col)
    let mutable shownName = name
    /// <summary> Robottens nuværende position </summary>
    /// <return> et tuple af int </return>
    member this.Position
        with get () : int * int = pos
        and set (n) = pos <- n
    /// <summary> Tjekker om en anden robot er på vej ind i denne robot </summary>
    /// <param name = "other"> Den anden robot på boardet </param>
    /// <param name = "dir"> Den retning den anden robot vil rykke sig i af typen Direction </param>
    /// <return> Typen Action </return>
    override this.Interact other dir: Action = match dir with
                                               |North when other.Position = (fst pos+1,snd pos) -> Stop (other.Position)
                                               |South when other.Position = (fst pos-1,snd pos) -> Stop (other.Position)
                                               |East  when other.Position = (fst pos,snd pos-1) -> Stop (other.Position)
                                               |West  when other.Position = (fst pos,snd pos+1) -> Stop (other.Position)
                                               |_ -> Ignore
    /// <summary> Det navn for robotten der bliver vist i terminalen </summary>
    member this.NewName
        with get() = shownName
        and set(n) = shownName <- n
    /// <summary> Visualiserer robotten på boardet i terminalet </summary>
    /// <param name = "display"> Det BoardDisplay som robotten skal tilføjes til </param>
    /// <return> unit </return>
    override this.RenderOn display = display.Set(fst pos, snd pos, shownName)
    /// <summary> Robottens navn </summary>
    /// <remark> Navnet spilleren bruger til at vælge robotten </remark>
    override this.Name = name
    /// <summary> Får robotten til at rykke sig et felt </summary>
    /// <param name = "dir"> Den retining robotten skal flytte sig af typen Direction </param>
    /// <return> unit </return>
    member this.Step (dir: Direction) = match dir with
                                         |North -> pos <- ((fst pos)-1,snd pos)
                                         |South -> pos <- ((fst pos)+1,snd pos)
                                         |East  -> pos <- (fst pos, (snd pos)+1)
                                         |West  -> pos <- (fst pos, (snd pos)-1)
    /// <summary> Vælger robottens udseende ud fra tema og navn </summary>
    /// <param name = "gt"> Spillet tema som en string </param>
    /// <return> unit </return>
    member this.RobotTheme (gt: string) = match gt with
                                          |"dark"  -> match this.Name with                                            
                                                      |"aa" -> this.NewName <- "🔵"                                            
                                                      |"bb" -> this.NewName <- "🟦"                                           
                                                      |"cc" -> this.NewName <- "🔷"                                            
                                                      |_ -> ()
                                          |"light" -> match this.Name with                                            
                                                      |"aa" -> this.NewName <- "🍺"                                            
                                                      |"bb" -> this.NewName <- "🍷"                                           
                                                      |"cc" -> this.NewName <- "🍹"                                            
                                                      |_ -> ()
                                          |"pink"  -> match this.Name with                                            
                                                      |"aa" -> this.NewName <- "🍏"                                            
                                                      |"bb" -> this.NewName <- "🥝"                                           
                                                      |"cc" -> this.NewName <- "🥦"                                            
                                                      |_ -> ()
                                          |"green" -> match this.Name with                                            
                                                      |"aa" -> this.NewName <- "⚽️"                                            
                                                      |"bb" -> this.NewName <- "🏈"                                           
                                                      |"cc" -> this.NewName <- "🏉"                                            
                                                      |_ -> ()
                                          |_ -> ()


/// <summary> En klasse der representerer målfeltet </summary>
/// <param name = "r"> int for første koordinat af målfeltets placering </param>
/// <param name = "c"> int for andet koordinat af målfeltets placering </param>
/// <return> et Goal objekt </return>
type Goal (r, c) =
    inherit BoardElement()
    let mutable shownName = "WW"
    /// <summary> sætter målfeltet ind på BoardDisplayet </summary>
    /// <param name = "display"> Det BoardDisplay som feltet skal være på </param>
    /// <return> unit </return>
    override this.RenderOn display = display.Set (r, c, shownName)
    /// <summary> Tjekker for alle robotter om de er landet i feltet </summary>
    /// <param name = "l"> en liste af Robot for alle robotter på boardet </param>
    /// <return> bool </return>
    override this.GameOver (l : Robot list) = l |> List.exists (fun x -> x.Position = (r,c))
    /// <summary> Det der bliver vist som målfelt i terminalen </summary>
    /// <param name = "th"> Spillets tema som string </param>
    /// <return> unit </return>
    member this.RendName (th:string) = match th with
                                       |"dark"  -> shownName <- "🏁"
                                       |"light" -> shownName <- "🙅‍♂️"
                                       |"pink"  -> shownName <- "👨‍🍳"
                                       |"green" -> shownName <- "🥅"
                                       |_ -> ()
    /// <summary> målfeltets navn </summary>
    override this.Name = "Goal"

                                                     
    
/// <summary> en klasse der representerer et Boardets ramme </summary>
/// <param name = "r"> int for boardets antal af rækker </param> 
/// <param name = "c"> int for boardets antal af kolonner </param>
/// <return> et BoardFrame objekt </return>
type BoardFrame(r: int, c: int) =
    inherit BoardElement()
    /// <summary> Visualiserer rammen ind på BoardDisplayet </summary>
    /// <param name = "display"> Det BoardDisplay som rammen skal sættes ind på </param>
    /// <remark> Funktionen gør intet da rammen allerede allerede bliver printet når BoardDisplayet bliver oprettet </remark>
    /// <return> unit </return>
    override this.RenderOn display = () //sker allerede i BoardDisplay
    /// <summary> Tjekker om en robot er på vej ind i rammen </summary>
    /// <param name = "other"> Den Robot som er i gang med at rykke sig af typen Direction </param>
    /// <param name = "dir"> Den retning som robotten flytter sig i </param>
    /// <return> typen Action </return>
    override this.Interact other dir =
        match dir with
        | North when fst other.Position = 1 -> Stop(other.Position)
        | South when fst other.Position = (r) -> Stop(other.Position)
        | West  when snd other.Position = 1 -> Stop(other.Position)
        | East  when snd other.Position = (c) -> Stop(other.Position)
        | _ -> Ignore
    /// <summary> rammens navn </summary>
    override this.Name = "BoardFrame"

/// <summary> en klasse der representerer en lodret væg </summary>
/// <param name = "r"> int for væggens første koordinat </param>
/// <param name = "c"> int for væggens andet koordinat </param>
/// <param name = "n"> int for væggens længde </param>
/// <return> et VerticalWall objekt </return>
type VerticalWall(r: int, c: int, n: int) =
    inherit BoardElement()
    /// <summary> Visualiserer væggen på et BoardDisplay </summary>
    /// <param name = "display"> Det BoardDisplay som væggens skal sættes ind på </param>
    /// <return> unit </return>
    override this.RenderOn(display) =
        match n with
        |n when n > 0 -> for x in r .. r+n-1 do display.SetRightWall(x, c)
        |n when n < 0 -> for x in r+n+1 .. r do display.SetRightWall(x, c)
        |_ -> ()

    /// <summary> Tjekker om en ronot er på vej ind i væggen </summary>
    /// <param name = "other"> Den Robot som er i gang med at rykke sig </param>
    /// <param name = "dir"> Den retning som robotten flytter sig i af typen Direction </param>
    /// <return> typen Action </return>
    override this.Interact other dir =
        match dir with
        | West when n > 0 && List.contains (fst other.Position) [r .. r+n-1] && c = snd other.Position - 1 -> Stop(other.Position)
        | West when n < 0 && List.contains (fst other.Position) [r+n+1 .. r] && c = snd other.Position - 1 -> Stop(other.Position)
        | East when n > 0 && List.contains (fst other.Position) [r .. r+n-1] && c = snd other.Position     -> Stop(other.Position)
        | East when n < 0 && List.contains (fst other.Position) [r+n+1 .. r] && c = snd other.Position     -> Stop(other.Position)
        | _ -> Ignore

    /// <summary> væggens navn </summary>
    override this.Name = "Vertical Wall"

/// <summary> en klasse der representerer en vandret væg </summary>
/// <param name = "r"> int for væggens første koordinat </param>
/// <param name = "c"> int for væggens andet koordinat </param>
/// <param name = "n"> int for væggens længde </param>
/// <return> et HorizontalWall objekt </return>
type HorizontalWall(r: int, c: int, n: int) =
    inherit BoardElement()
    /// <summary> Visualiserer væggen på et BoardDisplay </summary>
    /// <param name = "display"> Det BoardDisplay som væggens skal sættes ind på </param>
    /// <return> unit </return>
    override this.RenderOn(display) =
        match n with
        |n when n > 0 -> for x in c .. c+n-1 do display.SetBottomWall(r, x)
        |n when n < 0 -> for x in c+n+1 .. c do display.SetBottomWall(r, x)
        |_ -> ()
    /// <summary> Tjekker om en ronot er på vej ind i væggen </summary>
    /// <param name = "other"> Den Robot som er i gang med at rykke sig </param>
    /// <param name = "dir"> Den retning som robotten flytter sig i af typen Direction </param>
    /// <return> typen Action </return>
    override this.Interact other dir =
        match dir with
        | North when n > 0 && List.contains (snd other.Position) [c .. c+n-1] && r = fst other.Position - 1 -> Stop(other.Position)
        | North when n < 0 && List.contains (snd other.Position) [c+n+1 .. c] && r = fst other.Position - 1 -> Stop(other.Position)
        | South when n > 0 && List.contains (snd other.Position) [c .. c+n-1] && r = fst other.Position     -> Stop(other.Position)
        | South when n < 0 && List.contains (snd other.Position) [c+n+1 .. c] && r = fst other.Position     -> Stop(other.Position)
        | _ -> Ignore
    /// <summary> væggens navn </summary>
    override this.Name = "Horizontal Wall"


/// <summary> En klasse der representerer et Board </summary>
/// <param name = "r"> int for boardets antal af rækker </param> 
/// <param name = "c"> int for boardets antal af kolonner </param>
/// <return> et Board objekt </return>
type Board(r: int, c: int) =
    let mutable bd = BoardDisplay(r, c)
    let mutable elements: BoardElement list = []
    let mutable robotList: Robot list = []
    /// <summary> Henter boardets BoardDisplay </summary>
    member this.Display = bd
    /// <summary> Henter Boardets elementer i en liste af BoardElement </summary>
    member this.Elements = elements
    /// <summary> Henter Boardets elementer i en liste af Robot </summary>
    member this.Robots = robotList
    /// <summary> Tilføjer en robot til spillet </summary>
    /// <param name = "robot"> typen Robot for robotten der skal tilføjes til spillet </param>
    /// <remark> tilføjer robotten til både listen af robotter og listen af elementer </remark>
    /// <return> unit </unit>
    member this.AddRobot(robot: Robot) =
        elements <- upcast robot :: elements
        robotList   <- robot :: robotList
    /// <summary> tilføjer elementer til spillet </summary>
    /// <param name = "element"> Det BoardElement der skal tilføjes </param>
    /// <return> unit </return>
    member this.AddElement (element: BoardElement) =
        elements <- element :: elements
    /// <summary> Rykker en Robot </summary>
    /// <param name = "robot"> Typen Robot for den robot som skal flyttes </param>
    /// <param name = "dir"> Den Direction robotten skal rykke sig i </param>
    /// <return> unit </return>
    member this.Move (robot: Robot) (dir: Direction):unit =
        let elmList = this.Elements |> List.except [robot :> BoardElement]
        let rec move (robot0 : Robot) (dir0 : Direction): unit =
            let actions = elmList |> List.map (fun x -> (x.Interact robot0 dir0))
            match actions |> List.forall (fun x -> (x = Ignore)) with
            | true ->
                robot0.Step dir0
                move robot0 dir0
            | _ ->
                robot0.Position <- robot0.Position
        move robot dir
    /// <summary> Tilføjer alle elementer og robotter til et BoardDisplay, så de kan vises i terminalen </summary>
    /// <return> unit </return>
    member this.RenderAll() =
        bd <- BoardDisplay(r,c)
        for x in this.Elements do
            x.RenderOn bd
    /// <summary> Vælger boardets farver ud fra et givet tema </summary>
    /// <param name = "th"> Spillet tema som string </param>
    /// <return> unit </return>
    member this.BoardTheme(th: string) =
                                    match th with
                                    |"dark"  -> Console.BackgroundColor <- System.ConsoleColor.Black
                                                Console.ForegroundColor <- System.ConsoleColor.Red
                                    |"light" -> Console.BackgroundColor <- System.ConsoleColor.White
                                                Console.ForegroundColor <- System.ConsoleColor.Black
                                    |"pink"  -> Console.BackgroundColor <- System.ConsoleColor.DarkMagenta
                                                Console.ForegroundColor <- System.ConsoleColor.White
                                    |"green" -> Console.BackgroundColor <- System.ConsoleColor.DarkGreen
                                                Console.ForegroundColor <- System.ConsoleColor.White
                                    |_ -> ()
    /// <summary> Spilleren bestemmer hvilket level der skal spilles </summary>
    /// <return> En string - bruges når Boardet skal laves </return>
    member this.gameDifficulty = let diffList = ["1"; "2"]
                                 let n = Console.ReadLine()
                                 let rec k d = match diffList |> List.exists (fun x -> x = d) with
                                               |true -> d
                                               |false -> printf " Invallid level! Try 1 or 2\n "; k (Console.ReadLine())
                                 k n
    /// <summary> Laver et board ud fra gameDifficulty </summary>
    /// <param name = "diff"> En string
    member this.Boardsize diff = match diff with
                                   |"1" -> Board(4, 7)
                                   |"2" -> Board(10, 16)
                                   |_ -> Board(0,0) //bliver ikke muligt
    /// <summary> Sætter bestemte elementer og robotter ind på Boardet ud fra sværhedsgraden </summary>
    /// <return> unit </return>
    member this.gameSetting diff = match diff with
                                   |"1" -> this.AddElement(Goal(3, 6))
                                           this.AddRobot(Robot(2,3,"aa"))
                                           this.AddRobot(Robot(1,1,"bb"))
                                           this.AddRobot(Robot(4,7,"cc"))
                                           this.AddElement(BoardFrame(this.Display.R, this.Display.C))
                                           this.AddElement(HorizontalWall(2, 3, 1))
                                           this.AddElement(HorizontalWall(1, 4, 2))
                                           this.AddElement(VerticalWall(2, 3, 1))
 
                                   |"2" -> this.AddElement(Goal(6, 14))
                                           this.AddRobot(Robot(3, 2,"aa"))
                                           this.AddRobot(Robot(10, 5,"bb"))
                                           this.AddRobot(Robot(9, 5,"cc"))
                                           this.AddElement(BoardFrame(this.Display.R, this.Display.C))
                                           this.AddElement(HorizontalWall(1, 10, 1))
                                           this.AddElement(HorizontalWall(2, 3, 1))
                                           this.AddElement(HorizontalWall(3, 12, 1))
                                           this.AddElement(HorizontalWall(4, 2, 1))
                                           this.AddElement(HorizontalWall(4, 7, 1))
                                           this.AddElement(HorizontalWall(4, 16, 1))
                                           this.AddElement(HorizontalWall(5, 1, 1))
                                           this.AddElement(HorizontalWall(6, 6, 1))
                                           this.AddElement(HorizontalWall(6, 14, 1))
                                           this.AddElement(HorizontalWall(7, 8, 2))
                                           this.AddElement(HorizontalWall(7, 11, 1))
                                           this.AddElement(HorizontalWall(8, 4, 1))
                                           this.AddElement(HorizontalWall(9, 2, 1))
                                           this.AddElement(HorizontalWall(9, 6, 1))
                                           this.AddElement(HorizontalWall(9, 8, 2))
   
                                           this.AddElement(VerticalWall(1, 5, 1))
                                           this.AddElement(VerticalWall(1, 11, 1))
                                           this.AddElement(VerticalWall(2, 3, 1))
                                           this.AddElement(VerticalWall(2, 10, 1))
                                           this.AddElement(VerticalWall(3, 12, 1))
                                           this.AddElement(VerticalWall(4, 1, 1))
                                           this.AddElement(VerticalWall(5, 6, 1))
                                           this.AddElement(VerticalWall(6, 13, 1))
                                           this.AddElement(VerticalWall(7, 6, 1))
                                           this.AddElement(VerticalWall(8, 4, 1))
                                           this.AddElement(VerticalWall(8, 7, 2))
                                           this.AddElement(VerticalWall(8, 9, 2))
                                           this.AddElement(VerticalWall(8, 10, 1))
                                           this.AddElement(VerticalWall(10, 2, 1))
                                           this.AddElement(VerticalWall(10, 5, 1))
                                           this.AddElement(VerticalWall(10, 13, 1))

                                   |_ -> this.AddElement(Goal(3, 6)) 
                                         this.AddElement(BoardFrame(this.Display.R, this.Display.C))
                                         this.AddRobot(Robot(2,3,"aa"))
                                         this.AddRobot(Robot(1,1,"bb"))
                                         this.AddRobot(Robot(4,7,"cc"))
                                         this.AddElement(VerticalWall(1, 5, 1))

/// <summary> en klasse der repressenterer et Game </summary>
/// <param name = "b"> af typen Board for det spil der skal spilles </param>
/// <return> et Game objekt </return>
type Game(brd : Board) =
    /// <summary> Starter og spiller spillet </summary>
    /// <return> int </return>
    member this.Play() : int =
        printf " Select a game difficulty\n 1 or 2\n "
        let difficulty = brd.gameDifficulty
        let b = brd.Boardsize difficulty
        b.gameSetting difficulty
        /// finder målfeltet i listen af BoardElement
        let goal: BoardElement = b.Elements |> List.find (fun (x : BoardElement) -> match x with elm when elm.Name = "Goal" -> true | _ -> false)
        Console.Clear()
        printf " Select game theme:\n dark, light, pink, green\n "
        /// lader spilleren bestemme spillets tema
        let gameTheme: string = 
            let themeList = ["dark"; "light"; "pink"; "green"] 
            let theme = Console.ReadLine()
            match theme with
             |theme when themeList |> List.exists (fun x -> x = theme) -> theme
             |_ -> " "

        (goal:?> Goal).RendName(gameTheme) //ændre målfeltes visuelle representation
        for elm in b.Robots do elm.RobotTheme(gameTheme) // ændre robotternes visuelle representation
        b.BoardTheme(gameTheme) // ændre spillets farver

        let mutable numMoves: int = 0
        while not(goal.GameOver b.Robots) do
            Console.Clear()
            printfn " Select from following robots:"
            for elm in b.Robots |> List.rev do printfn " for %s, type %s;" elm.NewName elm.Name  
            
            b.RenderAll()
            b.Display.Show ()
            printfn " Number of moves: %i\n" numMoves
            /// <summary> Vælger en af robotterne fra spillet </summary>
            /// <return> En Robot </return> 
            let rec select() : Robot = 
                printf " Choose the robot you wish to move: "
                let ro = Console.ReadLine()
                match ro with
                |ro when b.Robots |> List.exists (fun x -> x.Name = ro) -> b.Robots |> List.find (fun x -> x.Name = ro)
                |_ -> printfn "\n Not a robot" ;select()
            let robot = select()

            /// <summary> Vælger hvilken retning en robot skal bevæge sig </summary>
            /// <return> unit </return>
            let rec move() =
                printf " Press esc to choose a different robot\n Select move direction: ^, v, <, >\n "
                let key = Console.ReadKey().Key
                match key with
                |ConsoleKey.UpArrow -> b.Move robot North; numMoves <- numMoves + 1
                |ConsoleKey.DownArrow -> b.Move robot South; numMoves <- numMoves + 1
                |ConsoleKey.RightArrow -> b.Move robot East; numMoves <- numMoves + 1
                |ConsoleKey.LeftArrow -> b.Move robot West; numMoves <- numMoves + 1
                |ConsoleKey.Escape -> ()
                |_ -> printfn "\nINVALID DIRECTION"; move()
            move()


        /// Printer antallet af træk brugt samt rekorden alt efter hvilken sværhedsgrad man har valgt
        let record = match difficulty with
                                |"1" -> if numMoves = 6 then "You beat the game in the most optimal way with 6 moves"
                                        elif numMoves < 6 then $"You beat our highscore!\n You finished the level in {numMoves}!!\n Our highscore is 6"
                                        else $"You beat the game in {numMoves} moves!\n Try beating the highscore of 6 moves"
                                |"2" -> if numMoves = 11 then "You matched our highscore on this level with 11 moves"
                                        elif numMoves < 11 then $"You beat the game in {numMoves} moves!\n Try beating the highscore of 6 moves"
                                        else $"You beat the game in {numMoves} moves!\n Try beating the highscore of 11 moves"
                                |_ -> "We dont even know how that happened"
        Console.Clear()
        for i in 1..6 do
            Console.ForegroundColor <- System.ConsoleColor.Black
            Console.BackgroundColor <- System.ConsoleColor.Red
            Console.Clear()
            printfn "\n\n"
            printfn " ██╗   ██╗ ██████╗ ██╗   ██╗    ██╗    ██╗██╗███╗   ██╗"
            printfn " ╚██╗ ██╔╝██╔═══██╗██║   ██║    ██║    ██║██║████╗  ██║"
            printfn "  ╚████╔╝ ██║   ██║██║   ██║    ██║ █╗ ██║██║██╔██╗ ██║"
            printfn "   ╚██╔╝  ██║   ██║██║   ██║    ██║███╗██║██║██║╚██╗██║"
            printfn "    ██║   ╚██████╔╝╚██████╔╝    ╚███╔███╔╝██║██║ ╚████║"
            printfn "    ╚═╝    ╚═════╝  ╚═════╝      ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝"
            printfn "\n %s" record
            Threading.Thread.Sleep(500)

            Console.ForegroundColor <- System.ConsoleColor.White
            Console.BackgroundColor <- System.ConsoleColor.Blue
            Console.Clear()
            printfn "\n\n"
            printfn " ██╗   ██╗ ██████╗ ██╗   ██╗    ██╗    ██╗██╗███╗   ██╗"
            printfn " ╚██╗ ██╔╝██╔═══██╗██║   ██║    ██║    ██║██║████╗  ██║"
            printfn "  ╚████╔╝ ██║   ██║██║   ██║    ██║ █╗ ██║██║██╔██╗ ██║"
            printfn "   ╚██╔╝  ██║   ██║██║   ██║    ██║███╗██║██║██║╚██╗██║"
            printfn "    ██║   ╚██████╔╝╚██████╔╝    ╚███╔███╔╝██║██║ ╚████║"
            printfn "    ╚═╝    ╚═════╝  ╚═════╝      ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝"
            printfn "\n %s" record
            Threading.Thread.Sleep(500)
            
        numMoves
//██╗   ██╗ ██████╗ ██╗   ██╗    ██╗    ██╗██╗███╗   ██╗
//╚██╗ ██╔╝██╔═══██╗██║   ██║    ██║    ██║██║████╗  ██║
// ╚████╔╝ ██║   ██║██║   ██║    ██║ █╗ ██║██║██╔██╗ ██║
//  ╚██╔╝  ██║   ██║██║   ██║    ██║███╗██║██║██║╚██╗██║
//   ██║   ╚██████╔╝╚██████╔╝    ╚███╔███╔╝██║██║ ╚████║
//   ╚═╝    ╚═════╝  ╚═════╝      ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝
