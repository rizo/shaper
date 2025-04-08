
  $ shaper <<< '1'
  1

  $ shaper <<< 'a'
  a

  $ shaper <<< '-a'
  (-_ a)

  $ shaper <<< '- a'
  (-_ a)

  $ shaper <<< '1 + 1'
  (_+_ 1 1)

  $ shaper <<< '1+1'
  (_+_ 1 1)

  $ shaper <<< '1+ 1'
  (_+_ 1 1)

  $ shaper <<< '1 +1'
  (_+_ 1 1)

  $ shaper <<< 'a!'
  (_! a)

  $ shaper <<< 'a !'
  (_! a)

  $ shaper <<< '(a)'
  ((_) a)

  $ shaper <<< '(-a)'
  ((_) (-_ a))

  $ shaper <<< '(a!)'
  ((_) (_! a))

  $ shaper <<< '1 + 2 * 3'
  (_+_ 1 (_*_ 2 3))

  $ shaper <<< '1, 2'
  (, 1 2)

  $ shaper <<< '1, 2,'
  (, 1 2)

  $ shaper <<< '1, 2, 3'
  (, 1 2 3)

  $ shaper <<< '1;'
  (; 1)

  $ shaper <<< '{;}'
  ({_} (;))

  $ shaper <<< '; ; ;'
  (; (;) (;))

  $ shaper <<< '; 1; ; ;'
  (; (_ (;) 1) (;))

  $ shaper <<< '1; 2; 3'
  (; 1 2 3)

  $ shaper <<< '1; 2; 3, a'
  (; 1 2 (, 3 a))

  $ shaper <<< 'a b'
  (_ a b)

  $ shaper <<< 'a b c'
  (_ a b c)

  $ shaper <<< 'a b + 1'
  (_+_ (_ a b) 1)

  $ shaper <<< 'return: 1 + 2'
  (_:_ return (_+_ 1 2))

  $ shaper <<< 'a: 1'
  (_:_ a 1)

  $ shaper <<< 'a: f x'
  (_:_ a (_ f x))

  $ shaper <<< '1 b: 2'
  (_:_ (_ 1 b) 2)

  $ shaper <<< 'calc (2 + 2): 4'
  (_:_ (_ calc ((_) (_+_ 2 2))) 4)

  $ shaper <<< 'a:'
  (_: a)

  $ shaper <<< 'a + f 1: t'
  (_:_ (_+_ a (_ f 1)) t)

  $ shaper <<< 'a + f 1 : t1 : t2'
  (_:_ (_+_ a (_ f 1)) (_:_ t1 t2))
 
  $ shaper <<< 'a : 1 b : 2 c : 3 d : 4'
  (_:_ a (_:_ (_ 1 b) (_:_ (_ 2 c) (_:_ (_ 3 d) 4))))

  $ shaper <<< 'a: 1 + b: 2'
  (_:_ a (_:_ (_+_ 1 b) 2))

Blocks:
  $ shaper <<< '(1)'
  ((_) 1)

  $ shaper <<< '{}'
  ({_} ())

  $ shaper <<< '[1, 2, 3]'
  ([_] (, 1 2 3))

Operator in prefix-position without arguments:
  $ shaper <<< '@'
  @

  $ shaper <<< '(+)'
  ((_) +)

  $ shaper <<< '(+, #, @, !, -)'
  ((_) (, + # @ ! -))

  $ shaper <<< '.'
  .

  $ shaper <<< '..'
  ..

  $ shaper <<< '...'
  ...

Alternatives:
  $ shaper <<< '{ a | b | c }'
  ({_} (_|_ a (_|_ b c)))

  $ shaper <<< '{ | a | b | c }'
  ({_} (|_ (_|_ a (_|_ b c))))


Arrows:
  $ shaper <<< 'a -> b -> c -> d'
  (_->_ a (_->_ b (_->_ c d)))

Dots:
  $ shaper <<< 'f a.b.c 1 2 3'
  (_ f (_._ (_._ a b) c) 1 2 3)

