module State exposing (..)

type State s a = State (s -> (s, a))

fmap : (a -> b) -> State s a -> State s b
fmap f (State sf) = State (\s -> let (ns, r) = sf s in (ns, f r))

ab : State s (a -> b) -> State s a -> State s b
ab sf sarg = sf |> andThen (\f -> sarg |> andThen (\arg -> pure (f arg)))

pure : a -> State s a
pure r = State (\s -> (s, r))

get : State s s
get = State (\s -> (s,s))

put : s -> State s ()
put ns = State (\_ -> (ns, ()))

stateMap : (s -> s) -> State s ()
stateMap f = State (\s -> (f s, ()))

runState : State s a -> s -> (s, a)
runState (State sf) = sf

evalState : State s a -> s -> s
evalState s = Tuple.first << runState s

withState : (s -> State s a) -> State s a
withState f = get |> andThen f

andThen : (a -> State s b) -> State s a -> State s b
andThen f (State sf) = State (\s -> let (ns, r) = sf s in runState (f r) ns)

vndThen : State s b -> State s a -> State s b
vndThen f (State sf) = State (\s -> let (ns, r) = sf s in runState f ns)

sequence : List (State s a) -> State s (List a)
sequence list = case list of
    [] -> pure []
    (h::tl) -> h |> andThen (\hel -> sequence tl |> andThen (\tlel -> pure (hel::tlel)))

void : State s a -> State s ()
void s = s |> vndThen (pure ())

