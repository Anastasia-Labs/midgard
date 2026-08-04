# §3.2 Necessity artifact — validation dispute maximum complete item

WORKED TEMPLATE ARTIFACT. A real artifact lives under
`docs/exec-plans/evidence/necessity/`; its numbers must be measured. This
concrete structure example is excluded from evidence aggregation.

## Binding

- Family / item: `validation-dispute-v1` / `maximum-profile complete item`
- Applied validator hashes measured: `example-validator-hash-a`,
  `example-validator-hash-b` (any change
  invalidates this artifact; re-measure before CG5 — GOAL_SPEC.md §3.2)
- Parameter snapshot digests: capability floor `example-capability-digest`,
  target `example-target-digest`
- Fixture: `demo/midgard-validation/tests/fixtures/maximum-profile-v1.json`
  `example-fixture-digest` (deterministically regenerable)

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation                                                        | Tx bytes / maxTxSize                 | Mem / limit | CPU / limit | Fee    | Fits §3.3                  |
| --------------------------------------------------------------------- | ------------------------------------ | ----------- | ----------- | ------ | -------------------------- |
| 1. Complete item direct in proof tx                                   | 17020 / 16384                        | not reached | not reached | —      | NO                         |
| 2. Complete item as inline-datum publication + consume/reference      | pub: 15340/16384; proof: 14110/16384 | 11.2M / 14M | 7.1B / 10B  | 621000 | NO — memory reserve is 20% |
| 3. Minimum multi-output publication + complete logical reconstruction | 15980 / 16384                        | 10.9M / 14M | 7.0B / 10B  | 640000 | NO — memory reserve is 20% |
| 4. Incremental on-chain traversal                                     | 12100 / 16384                        | 9.8M / 14M  | 6.8B / 10B  | 590000 | YES                        |

## Exact limiting constraint

The direct representation is 636 bytes above the 16384-byte transaction
limit. The publication representations remain above the required 20 percent
memory reserve, so the incremental representation is the first fitting path.

## Why no simpler authenticated representation closes the gap

The complete direct item was measured first. Inline publication and minimum
multi-output reconstruction remove the byte overflow but do not retain the
required memory reserve. Incremental traversal retains both limits.

## Preserved complete-item path

Items at or below 12000 serialized bytes continue to use representation 1;
the fallback applies only above it. Both representations authenticate
commitment `validation-item-root-v1` and share the equivalence tests at
`demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`
(omission, duplication, reorder, substitution, trailing data all reject).
