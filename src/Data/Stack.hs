module Data.Stack where

-- https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
data Stack a = Empty | NonEmpty a (Stack a)
             deriving (Show, Read, Eq)

empty :: Stack a
empty = Empty

-- Get element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
top :: Stack a -> a
top (Empty) = error "Empty stack"
top (NonEmpty a rest) = a

-- Get element from top of stack (if there is some, otherwise return Nothing)
topSafe :: Stack a -> Maybe a
topSafe (Empty) = Nothing
topSafe (NonEmpty a rest) = Just a

-- Pop element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
pop :: Stack a -> Stack a
pop (Empty) = error "Empty stack"
pop (NonEmpty a rest) = rest

-- Pop element from top of stack (if there is some, otherwise return Nothing)
popSafe :: Stack a -> Maybe (Stack a)
popSafe (Empty) = Nothing
popSafe (NonEmpty a rest) = Just rest

-- Push element to top of stack
push :: a -> Stack a -> Stack a
push v stack = NonEmpty v stack

-- Get number of elements in stack
size :: Num n => Stack a -> n
size Empty = 0
size (NonEmpty a rest) = 1 + size rest

-- Check if stack is empty
-- Note: is more effective than checking if size is zero
null :: Stack a -> Bool
null Empty = True
null (NonEmpty a rest) = False
