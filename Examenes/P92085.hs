data Nat = Z | S Nat 
    deriving Show

rec :: a -> (Nat -> a -> a) -> Nat -> a
rec base step Z = base
rec base step (S n) = step n (rec base step n)


isEven :: Nat -> Bool       -- indica si un natural és parell o no
isEven a = rec base step a
    where
        base = True
        step a = not


add :: Nat -> Nat -> Nat    -- retorna la suma de dos naturals
add a b = rec base step a
    where
        base = b
        step a = S

mul :: Nat -> Nat -> Nat   -- retorna el producte de dos naturals
mul a b = rec base step a
    where
        base = Z
        step a = add b

fact :: Nat -> Nat          -- retorna el factorial d’un natural
fact a = rec base step a
    where
        base = S Z
        step a = mul (S a)