-- Solution 1
maxWeight :: Int -> Int -> (Int -> Int) -> Int
maxWeight 0 _ _ = 0
maxWeight i w weight
  | weight i > w = maxWeight (i-1) w weight
  | otherwise    = max (maxWeight (i-1) w weight) (maxWeight (i-1) (w - weight i) weight + weight i)

-- Solution 2
maxValue :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int
maxValue 0 _ _ _ = 0
maxValue i w weight value
  | weight i > w = maxValue (i-1) w weight value
  | otherwise    = max (maxValue (i-1) w weight value) ((maxValue (i-1) (w - weight i) weight value) + value i)

-- Solution 3
maxWeight' n w weight = maxValue n w weight weight

-- Solution 4
maxLayout :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Bool]
maxLayout n w weight value = maxLayoutIter n w weight value []

maxLayoutIter :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Bool] -> [Bool]
maxLayoutIter 0 _ _ _ layout = layout
maxLayoutIter i w weight value layout
  | weight i > w             = layoutWithout
  | valueWithout > valueWith = layoutWithout
  | otherwise                = layoutWith
                               where valueWithout  = maxValue (i-1) w weight value
                                     valueWith     = maxValue (i-1) (w - weight i) weight value + value i
                                     layoutWithout = maxLayoutIter (i-1) w weight value (False:layout)
                                     layoutWith    = maxLayoutIter (i-1) (w - weight i) weight value (True:layout)

-- Solution 5
allMaxLayouts :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [[Bool]]
allMaxLayouts n w weight value = allMaxLayoutsIter n w weight value []

allMaxLayoutsIter :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Bool] -> [[Bool]]
allMaxLayoutsIter 0 _ _ _ layout = [layout]
allMaxLayoutsIter i w weight value layout
  | weight i > w             = layoutsWithout
  | valueWithout > valueWith = layoutsWithout
  | valueWithout < valueWith = layoutsWith
  | otherwise                = layoutsWithout ++ layoutsWith
    where valueWithout   = maxValue (i-1) w weight value
          valueWith      = maxValue (i-1) (w - weight i) weight value + value i
          layoutsWithout = allMaxLayoutsIter (i-1) w weight value (False:layout)
          layoutsWith    = allMaxLayoutsIter (i-1) (w - weight i) weight value (True:layout)
          
-- Solution 6
maxValue' :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> Int
maxValue' 0 _ _ _ = 0
maxValue' i w weight value
 | weight i > w = maxValue' (i-1) w weight value
 | otherwise    = maxInt (maxValue' (i-1) w weight value) (maxValue' i (w - weight i) weight value + value i)

maxLayout' :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> [Int]
maxLayout' n w weight value = maxLayoutIter' n w weight value False []

maxLayoutIter' :: Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool -> [Int] -> [Int]
maxLayoutIter' 0 _ _ _ _ layout = layout
maxLayoutIter' i w weight value included layout
  | weight i > w               = layoutWithout
  | valueWithout > valueWith   = layoutWithout
  | otherwise                  = layoutWith
    where valueWithout   = maxValue' (i-1) w weight value
          valueWith      = maxValue' i (w - weight i) weight value + value i
          layoutWithout  = if included
                           then maxLayoutIter' (i-1) w weight value False layout
                           else maxLayoutIter' (i-1) w weight value False (0:layout)
          layoutWith     = if included
                           then maxLayoutIter' i (w - weight i) weight value True (1 + head layout:tail layout)
                           else maxLayoutIter' i (w - weight i) weight value True (1:layout)
