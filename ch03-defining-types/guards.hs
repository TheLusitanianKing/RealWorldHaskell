fromMaybe defval wrapped =
    case wrapped of
        Nothing -> defval
        Just x  -> x
-- "case" to use pattern matching within an expression