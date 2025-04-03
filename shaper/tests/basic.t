
  $ shaper <<< '1'
  1

  $ shaper <<< 'a'
  a

  $ shaper <<< '-a'
  (prefix - a)

  $ shaper <<< '- a'
  (prefix - a)

  $ shaper <<< '1 + 1'
  (infix + 1 1)

  $ shaper <<< '1+1'
  (infix + 1 1)

  $ shaper <<< '1+ 1'
  (infix + 1 1)

  $ shaper <<< '1 +1'
  (infix + 1 1)

  $ shaper <<< 'a!'
  (postfix ! a)

  $ shaper <<< 'a !'
  (postfix ! a)

  $ shaper <<< '(a)'
  (parens a)

  $ shaper <<< '(-a)'
  (parens (prefix - a))

  $ shaper <<< '(a!)'
  (parens (postfix ! a))

  $ shaper <<< '1 + 2 * 3'
  (infix + 1 (infix * 2 3))

  $ shaper <<< '1, 2'
  (, 1 2)

  $ shaper <<< '1, 2, 3'
  (, 1 2 3)

  $ shaper <<< '1;'
  (; 1)

  $ shaper <<< '1; 2; 3'
  (; 1 2 3)

  $ shaper <<< '1; 2; 3, a'
  (; 1 2 (, 3 a))

  $ shaper <<< 'a b'
  (_ a b)

  $ shaper <<< 'a b c'
  (_ a b c)

  $ shaper <<< 'a b + 1'
  (infix + (_ a b) 1)

  $ shaper <<< 'return: 1 + 2'
  (infix : return (infix + 1 2))

  $ shaper <<< 'a: 1'
  (infix : a 1)

  $ shaper <<< 'a: f x'
  (infix : a (_ f x))

#  $ shaper <<< 'a: 1 b: 2 c: 3 d: 4'
#  (_ (a: 1) (b: 2) (c: 3) (d: 4))

  $ shaper <<< '1 b: 2'
  (infix : (_ 1 b) 2)

#  $ shaper <<< 'if a then: 1 else: if c then: 2 else: 3'

  $ shaper <<< 'calc (2 + 2): 4'
  (infix : (_ calc (parens (infix + 2 2))) 4)

#  $ shaper <<< 'a:'
#  (postfix : a)

  $ shaper <<< 'a + f 1: t'
  (infix : (infix + a (_ f 1)) t)

  $ shaper <<< 'a + f 1: t : t2'
  (infix : (infix + a (_ f 1)) t)
 
  $ shaper <<< 'select: a from: c where: b and: x'

#  $ shaper <<< 'select: (a, b) from: c where: a > 1 and: (b not: null)'

#  $ shaper <<< 'a: 1 + b: 2'
#  (prefix (: a) (infix + 1 (prefix (: b) 2)))
