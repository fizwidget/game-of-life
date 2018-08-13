module History
    exposing
        ( History
        , begin
        , current
        , advance
        , record
        , undo
        )


type History a
    = History a (List a)


begin : a -> History a
begin current =
    History current []


current : History a -> a
current (History current _) =
    current


advance : History a -> (a -> a) -> History a
advance history f =
    record ((current >> f) history) history


record : a -> History a -> History a
record value (History current previous) =
    History value (current :: previous)


undo : History a -> History a
undo (History current previous) =
    case previous of
        value :: rest ->
            History value (rest)

        [] ->
            History current previous
