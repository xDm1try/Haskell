p1 = (Pt 4 1 0)
p2 = (Pt 2 3 0)
p3 = (Pt 0 0 3)
v1 = (Vc 4 0 2)
v2 = (Vc 0 2 3)
v3 = (Vc 4 3 0)

ln1 = lineFromPointAndVec p3 v2
ln1' = lineFromPointAndVec (Pt 0 2 6) (Vc 0 1 1.5)
ln2 = lineFrom2Points (Pt 0 0 0) (toPoint v1)
ln3 = lineFrom2Points (Pt 0 0 0) (Pt 4 3 0)
lnOZ = lineFrom2Points (Pt 0 0 0) (Pt 0 0 1)
lnOX = lineFrom2Points (Pt 0 0 0) (Pt 1 0 0)
lnOY = lineFrom2Points (Pt 0 0 0) (Pt 0 1 0)
pntO = (Pt 0 0 0)
pln1' = (CPl 1 1 1 5)
pln1 = (CPl 2 2 2 10)

plnXOY = (CPl 0 0 1 0)
plnYOZ = (CPl 1 0 0 0)
plnXOZ = (CPl 0 1 0 0)


