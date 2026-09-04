# Input-set uniqueness wrongful-rejection V1 size plan

## Scope and decisive rule

This upgrade preserves the existing two-validator wrongful-acceptance route.
Its new route authenticates a forced leaf whose exact reason is
`DuplicateInput`, opens both fixed-width input fields against that leaf's
transaction id, and proves the complete fields-0/1 union strictly increasing.
The prover supplies carriage and transaction layout only; direction, reason,
coordinates, scan verdict, checkpoint, and terminal action are derived and
checked on-chain.

## Physical validators

| Applied validator              | Imported semantic engine                                                     | Maximum dynamic evidence                                                               | Fit test                                                                    |
| ------------------------------ | ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- | --------------------------------------------------------------------------- |
| `input_set_uniqueness/step_01` | proof-thread forced-subject binding                                          | one forced-leaf membership proof plus compact transaction source                       | publish reference script; real forced Init→01→03 transaction                |
| `input_set_uniqueness/step_02` | existing authenticated item-equality rule                                    | two field openings / one compact source                                                | preserve existing accepted-invalid lifecycle and publication                |
| `input_set_uniqueness/step_03` | authenticated whole-field door and exact `DuplicateInput` coordinate binding | field-0 and field-1 carriage, at most three certified chunks each                      | publish reference script; real forced 01→03 initialization transaction      |
| `input_set_uniqueness/step_04` | bounded canonical union scan and substrate terminal contradiction            | up to sixteen adjacent items from one authenticated field opening per scan transaction | publish reference script; maximum-frontier resume and terminal transactions |

Step-04 consumes exactly sixteen items per continuation, or the remaining items
before the current field boundary when fewer than one hundred twenty-eight remain. A batch never
crosses from field 0 to field 1, so every item is read from the one authenticated
opening named by that transaction. Its datum carries the
authenticated verdict subject, both counts, next global cursor, previous
canonical item, and a domain-separated checkpoint over those values and the
next expected applied script. Step-03 seeds that checkpoint with the deployed
step-04 hash; step-04 re-derives it from its actual own script hash before each
resume. No unrelated subject adapter or decisive predicate enters any applied
validator.

## Required fit evidence

- Testnet blueprint built under the repository compiler lease.
- Complete signed reference-script publication for all four scripts, target
  `<= 15,872` bytes and hard limit `<= 16,384` bytes.
- Signed bytes, memory, and CPU for Init, forced bind, scan initialization,
  maximum-frontier scan/resume, final proof mint, every cancellation boundary,
  and leased state-queue removal.
- Limits: `16,384` signed bytes, `16,500,000` memory, `10,000,000,000` CPU;
  no oversized route, raised protocol parameter, or disabled local evaluation.
