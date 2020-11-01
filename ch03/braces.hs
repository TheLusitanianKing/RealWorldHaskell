-- the offsite rule (of indentation) is not mandatory
-- the 2 following uses of let are equivalent :
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1; b = 2;
    c = 3 }
      in a + b + c