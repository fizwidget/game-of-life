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


undo : History a -> History a
undo ((History { past, present, future }) as history) =
    case past of
        head :: tail ->
            History
                { past = tail
                , present = head
                , future = present :: future
                }

        [] ->
            history


redo : History a -> Maybe (History a)
redo (History { past, present, future }) =
    case future of
        head :: tail ->
            Just <|
                History
                    { past = present :: past
                    , present = head
                    , future = tail
                    }

        [] ->
            Nothing


didChange : History a -> Bool
didChange (History { past, present }) =
    List.head past
        |> Maybe.map ((/=) present)
        |> Maybe.withDefault True
