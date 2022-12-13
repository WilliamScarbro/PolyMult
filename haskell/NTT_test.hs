import FField
import NTT


test1 = (mNTT 4 5) `mv` (ffVec 4 5 id)
test2 = (mNTT 4 5) `mm` (mNTT 4 5) >>= (mm (mNTT 4 5))
test3 = ((mNTT 4 5) `mm` (mNTT_inv 4 5)) == Just (mId 4)
test4 = tensor (mId 2) (mNTT 3 7)

test_phi = phi 6 6 0 6 7

test5 = (mL 6 2) * (phi 6 3 0 6 7) * (mL 6 3)
test6 = test5 == test4

test7 = (mL 6 2) * (phi 6 3 6 12 13) * (mL 6 3)
test8 =  tensor (mId 2) (phi 3 3 6 12 13)
