module History
    exposing
        ( History
        , begin
        , now
        , record
        , undo
        , redo
        , didChange
        )


type History a
    = History (List a) a (List a)


begin : a -> History a
begin present =
    History [] present []


now : History a -> a
now (History _ present _) =
    present


record : (a -> a) -> History a -> History a
record step (History _ present past) =
    History [] (step present) (present :: past)


undo : History a -> History a
undo (History future present past) =
    case past of
        head :: tail ->
            History (present :: future) head tail

        [] ->
            History future present past


redo : History a -> Maybe (History a)
redo (History future present past) =
    case future of
        head :: tail ->
            Just <| History tail head (present :: past)

        [] ->
            Nothing


didChange : History a -> Bool
didChange (History _ present past) =
    List.head past
        |> Maybe.map ((/=) present)
        |> Maybe.withDefault True
