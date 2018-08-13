module History
    exposing
        ( History
        , begin
        , now
        , record
        , undo
        , didChange
        )


type History a
    = History a (List a)


begin : a -> History a
begin current =
    History current []


now : History a -> a
now (History current _) =
    current


record : (a -> a) -> History a -> History a
record f (History current previous) =
    History (f current) (current :: previous)


undo : History a -> History a
undo (History current previous) =
    case previous of
        head :: tail ->
            History head tail

        [] ->
            History current previous


didChange : History a -> Bool
didChange (History current previous) =
    List.head previous
        |> Maybe.map ((/=) current)
        |> Maybe.withDefault True
