tobase2 :: Int -> String
tobase2 0 = "";
tobase2 x = (tobase2 (x `div` 2)) ++ (show (x `mod` 2))

b2wrap "" = "0"
b2wrap str = str

b2 = b2wrap . tobase2