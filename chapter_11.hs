-- The Ackermann function - a very interesting example of a total computable
-- function that is not primitive recursive. Its result grows rapidly as the
-- arguments increase, with x broadly specifying the operation type that is to
-- be applied to y. Passing three as the first argument rougly corresponds to
-- performing exponentiation using the second argument. Values of x above
-- three resolve to approximate representations of hyperoperations following
-- exponentiation. This is not the original version described by Wilhelm
-- Ackermann, which has three arguments.
ack :: Integer -> Integer -> Integer
ack 0 y = y + 1
ack x 0 = ack (x - 1) 1
ack x y = ack (x - 1) (ack x (y - 1))

