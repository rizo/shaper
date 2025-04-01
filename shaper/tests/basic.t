
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
  (return: (infix + 1 2))

#  $ shaper <<< '1 + a b'
#  (infix + 1 (_ a b))
#
#  $ shaper <<< 'a b, c d'
#  (, (_ a b) (_ c d))
#
#  $ shaper <<< 'a b, c d; e f, g h'
#  (; (, (_ a b) (_ c d)) (, (_ e f) (_ g h)))
#
  $ shaper <<< 'a: 1'
  (a: 1)

  $ shaper <<< '1 or: 2'
  (_ 1 (or: 2))

#  $ shaper <<< '1 a: 2 b: 3'
#  (_ 1 (: a 2) (b: 3))
#
#  $ shaper <<< 'a: 2 b: 3'
#  (_ (a: 2) (b: 3))
#
#  $ shaper <<< 'return: 1 + 2 * 3'
#  (return: (infix + 1 (infix * 2 3)))
#
#  $ shaper <<< 'return: 1 + 2 or: 3'
#  (_ (return: (infix + 1 2)) (: or 3))

  $ shaper <<< '(2 + 2): 4'
  (_ (postfix : (parens (infix + 2 2))) 4)

  $ shaper <<< 'a: f x'
  (a: (_ f x))

  $ shaper <<< 'a: 1 b: 2'
  (_ (a: 1) (b: 2))

  $ shaper <<< '1 b: 2'
  (_ 1 (b: 2))

#  $ shaper <<< 'if a then: 1 else: if c then: 2 else: 3'

#  $ shaper <<< 'calc (2 + 2): 4'
#  (_ calc (: (parens (infix + 2 2)) 4))

#  $ shaper <<< 'a:'
#  TODO

#  $ shaper <<< 'a | b -> c + 2, d'
#  (, (infix | a (infix -> b (infix + c 2))) d)
#
#  $ shaper <<< 'a: 1'
#  (prefix (: or) 1)
#
#  $ shaper <<< '1 or: b'
#  (infix (: or) 1 b)
#
#  $ shaper <<< 'a: 1 b: 2'
#  (prefix (: a) (infix (: b) 1 2))
#
#  $ shaper <<< 'a: 1 b: 2 c: 3'
#  (prefix (: a) (infix (: b) 1 (infix (: c) 2 3)))
#
#  $ shaper <<< 'a: f 1 2 b: g 3'
#  (prefix (: a) (infix (: b) 1 2))
#
#  $ shaper <<< 'select: (a, b) from: c where: a > 1 and: (b not: null)'
#  (: select (parens (, a b)) )
#
#  $ shaper <<< 'a: 1 + b: 2'
#  (prefix (: a) (infix + 1 (prefix (: b) 2)))
