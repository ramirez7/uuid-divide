# `uuid-divide`

Mutually-exclusive, collectively-exhaustive sets of UUID ranges.

Useful for evenly dividing operations over uniformly-distributed UUIDs (e.g. sharding).

### Uses

Note that `UUIDRange` is inclusive on both ends.

Divide into `2^n` equal ranges:
```
λ: uuidDivide 2
[ UUIDRange {uuidLower = 00000000-0000-0000-0000-000000000000, uuidUpper = 3fffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = 40000000-0000-0000-0000-000000000000, uuidUpper = 7fffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = 80000000-0000-0000-0000-000000000000, uuidUpper = bfffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = c0000000-0000-0000-0000-000000000000, uuidUpper = ffffffff-ffff-ffff-ffff-ffffffffffff} ]
```

Get the `nth` (starting at 0) range for a total division (must be a power of 2):
```
λ: nthRange 4 2
Right (UUIDRange {uuidLower = 80000000-0000-0000-0000-000000000000, uuidUpper = bfffffff-ffff-ffff-ffff-ffffffffffff})
```

Check for inclusion:
```
λ: :{
uuidRangeContains
  UUIDRange {
  uuidLower = fromJust $ fromText "80000000-0000-0000-0000-000000000000",
  uuidUpper = fromJust $ fromText "bfffffff-ffff-ffff-ffff-ffffffffffff" } $
  fromJust (fromText "a2cc10e1-57d6-4b6f-9899-38d972112d8c")
:}

True
```
