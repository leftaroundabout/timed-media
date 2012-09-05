module MMedia.MiscUtil.Data where


maybeCombine :: (a->a->a) -> Maybe a -> Maybe a -> Maybe a
maybeCombine _ Nothing Nothing = Nothing
maybeCombine _ (Just a) Nothing = Just a
maybeCombine _ Nothing (Just a) = Just a
maybeCombine f (Just a) (Just a') = Just $ f a a'
