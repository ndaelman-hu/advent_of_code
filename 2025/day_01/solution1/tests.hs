prop_monoid_assoc :: (Eq , Show , Semigroup ) => Gen  -> Property
prop_monoid_assoc gen = property $ do
  a <- forAll gen; b <- forAll gen; c <- forAll gen
  (a <> b) <> c === a <> (b <> c)

prop_monoid_left_id :: (Eq , Show , Monoid ) => Gen  -> Property
prop_monoid_left_id gen = property $ do
  a <- forAll gen
  mempty <> a === a

prop_monoid_right_id :: (Eq , Show , Monoid ) => Gen  -> Property
prop_monoid_right_id gen = property $ do
  a <- forAll gen
  a <> mempty === a
