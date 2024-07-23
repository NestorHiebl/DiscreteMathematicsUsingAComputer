import qualified Data.Set as Set

-- How many subsets with n elements?
numElemsWithNumMembers :: Set.Set (Set.Set a) -> Int -> Int

numElemsWithNumMembers s n =
    Set.size (Set.filter validator s)
    where validator y = Set.size y == n

