module History exposing
    ( History
    , begin
    , isUnchanged
    , now
    , record
    , redo
    , undo
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
record step (History past present _) =
    History (present :: past) (step present) []


undo : History a -> Maybe (History a)
undo (History past present future) =
    case past of
        nextPresent :: nextPast ->
            Just (History nextPast nextPresent (present :: future))

        [] ->
            Nothing


redo : History a -> Maybe (History a)
redo (History past present future) =
    case future of
        nextPresent :: nextFuture ->
            Just (History (present :: past) nextPresent nextFuture)

        [] ->
            Nothing


isUnchanged : History a -> Bool
isUnchanged (History past present _) =
    List.head past
        |> Maybe.map ((==) present)
        |> Maybe.withDefault True
