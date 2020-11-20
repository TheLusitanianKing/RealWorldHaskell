foo = let a = 1
      in let b = 2
         in a + b

-- shadowing (be careful with this)
bar = let x = 1
      in ((let x = "foo" in x), x)
-- shadowing is even possible with function parameters
quux a = let a = "foo"
         in a ++ "eek!"