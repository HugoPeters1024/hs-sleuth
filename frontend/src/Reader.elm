module Reader exposing (..)

type Reader env a = Reader (env -> a)

ask : Reader env env
ask = askFor identity

askFor : (env -> a) -> Reader env a
askFor = Reader

exec : (env -> Reader env a) -> Reader env a
exec rr = ask |> andThen rr

runReader : env -> Reader env a -> a
runReader env (Reader f) = f env

map : (a -> b) -> Reader env a -> Reader env b
map f (Reader a) = Reader <| \env -> f (a env)

map2 : (a -> b -> c) -> Reader env a -> Reader env b -> Reader env c
map2 f (Reader a) (Reader b) = Reader <| \env -> f (a env) (b env)

traverse : (a -> Reader env a) -> List a -> Reader env (List a)
traverse f xss = case xss of
    [] -> pure []
    x::xs -> map2 (\y ys -> y::ys) (f x) (traverse f xs)

pure : a -> Reader env a
pure x = Reader <| \_ -> x

andThen : (a -> Reader env b) -> Reader env a -> Reader env b
andThen f (Reader g) = Reader <| \env -> runReader env (f (g env))

foldM : (a -> b -> b) -> b -> List (Reader env a) -> Reader env b
foldM f def xs = Reader <| \env -> List.foldl f def (List.map (runReader env) xs)


