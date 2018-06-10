
[<EntryPoint>]
let main argv =
    [0..9]
    |> Int2Lcd.renderLcd
    |> printfn "%s"

    0 // return an integer exit code
