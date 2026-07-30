# §3.2 Necessity artifact — <proof family> <item shape>

TEMPLATE — placeholder values. A real artifact lives under
`docs/exec-plans/evidence/necessity/` and every number is measured, not
estimated.

## Binding

- Family / item: `<family>` / `<canonical item and maximum shape>`
- Applied validator hashes measured: `<hash>`, `<hash>` (any change
  invalidates this artifact; re-measure before CG5 — GOAL_SPEC.md §3.2)
- Parameter snapshot digests: capability floor `<sha256>`, target `<sha256>`
- Fixture: `<path>` `<sha256>` (deterministically regenerable)

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit | CPU / limit | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete item direct in proof tx | n/a — exceeds maxTxSize by N bytes | — | — | — | NO |
| 2. Complete item as inline-datum publication + consume/reference | pub: N/16384; proof: N/16384 | N / limit·0.8 | N / limit·0.8 | N | NO — exceeds 20% memory reserve by N |
| 3. Minimum multi-output publication + complete logical reconstruction | ... | ... | ... | ... | YES/NO |
| 4. Incremental on-chain traversal | ... | ... | ... | ... | YES |

## Exact limiting constraint

`<the single byte/execution/Value/datum/reference-input/timing/economic limit
that blocks the simpler path, with the measured number and required margin>`

## Why no simpler authenticated representation closes the gap

`<one paragraph: what was tried, why each fails the limit above>`

## Preserved complete-item path

Items at or below `<threshold>` continue to use representation `<1 or 2>`;
the fallback applies only above it. Both representations authenticate
commitment `<root>` and share the equivalence tests at `<test paths>`
(omission, duplication, reorder, substitution, trailing data all reject).
