# `uuid-divide`

Mutually-exclusive, collectively-exhaustive sets of UUID ranges.

Useful for evenly dividing operations over uniformly-distributed UUIDs.

### Uses

Note that `UUIDRange` is inclusive on both ends.

Divide into `2^n` equal range:
```
λ: uuidDivide 2
[ UUIDRange {uuidLower = 00000000-0000-0000-0000-000000000000, uuidUpper = 3fffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = 40000000-0000-0000-0000-000000000000, uuidUpper = 7fffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = 80000000-0000-0000-0000-000000000000, uuidUpper = bfffffff-ffff-ffff-ffff-ffffffffffff}
, UUIDRange {uuidLower = c0000000-0000-0000-0000-000000000000, uuidUpper = ffffffff-ffff-ffff-ffff-ffffffffffff} ]
```

Get the `nth` (starting at 0) range for a `2^n` division:
```
λ: nthRange 2 2
Right (UUIDRange {uuidLower = 80000000-0000-0000-0000-000000000000, uuidUpper = bfffffff-ffff-ffff-ffff-ffffffffffff})
```

Check for inclusion:
```
λ: :{
*Data.UUID.Divide Data.Maybe Data.UUID| uuidRangeContains
*Data.UUID.Divide Data.Maybe Data.UUID|   UUIDRange {
*Data.UUID.Divide Data.Maybe Data.UUID|   uuidLower = fromJust $ fromText "80000000-0000-0000-0000-000000000000",
*Data.UUID.Divide Data.Maybe Data.UUID|   uuidUpper = fromJust $ fromText "bfffffff-ffff-ffff-ffff-ffffffffffff" } $
*Data.UUID.Divide Data.Maybe Data.UUID|   fromJust (fromText "a2cc10e1-57d6-4b6f-9899-38d972112d8c")
*Data.UUID.Divide Data.Maybe Data.UUID| :}

True
```
