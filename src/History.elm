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
    = History
        { past : List a
        , present : a
        , future : List a
        }


begin : a -> History a
begin present =
    History
        { past = []
        , present = present
        , future = []
        }


now : History a -> a
now (History { present }) =
    present


record : (a -> a) -> History a -> History a
record step (History { past, present }) =
    History
        { past = present :: past
        , present = step present
        , future = []
        }


undo : History a -> Maybe (History a)
undo (History { past, present, future }) =
    case past of
        [] ->
            Nothing

        newPresent :: newPast ->
            (Just << History)
                { past = newPast
                , present = newPresent
                , future = present :: future
                }


redo : History a -> Maybe (History a)
redo (History { past, present, future }) =
    case future of
        [] ->
            Nothing

        newPresent :: newFuture ->
            (Just << History)
                { past = present :: past
                , present = newPresent
                , future = newFuture
                }


isUnchanged : History a -> Bool
isUnchanged (History { past, present }) =
    List.head past
        |> Maybe.map ((==) present)
        |> Maybe.withDefault True
