module History
    exposing
        ( History
        , begin
        , now
        , record
        , undo
        )


type History a
    = History a (List a)


begin : a -> History a
begin current =
    History current []


now : History a -> a
now (History current _) =
    current


record : History a -> (a -> a) -> History a
record (History current previous) f =
    History (f current) (current :: previous)


undo : History a -> History a
undo (History current previous) =
    case previous of
        head :: tail ->
            History head tail

        [] ->
            History current previous
