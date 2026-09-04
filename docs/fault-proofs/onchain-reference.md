# On-Chain Fault-Proof Reference

Current Aiken map reviewed against the working tree on 2026-09-01.

## Compiled identity

| Item                 | Value                                                              |
| -------------------- | ------------------------------------------------------------------ |
| Blueprint            | `onchain/aiken/plutus.json`                                        |
| Validators           | 563                                                                |
| SHA-256              | `b885c3abb0eeaace296011a108fbe4a06d0e5303bfb9d73bbec48fc30f32f9de` |
| Catalogue categories | 32, IDs `00000000`–`0000001f`                                      |
| Catalogue root       | `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90` |

## Shared validators and libraries

| Surface                                | Source                                                                           |
| -------------------------------------- | -------------------------------------------------------------------------------- |
| Catalogue policy/validator             | `onchain/aiken/validators/fraud-proof-catalogue.ak`                              |
| Computation-thread policy              | `onchain/aiken/validators/computation-thread.ak`                                 |
| Permanent proof token                  | `onchain/aiken/validators/fraud-proof.ak`                                        |
| State-queue dispatch, init, and deinit | `onchain/aiken/validators/state-queue.ak`                                        |
| State-queue operational arms           | `onchain/aiken/validators/state-queue-yields.ak`                                 |
| Common family binding/cancel/finalize  | `onchain/aiken/lib/midgard/fraud-proofs/common.ak`                               |
| Native transaction commitments/codecs  | `onchain/aiken/lib/midgard/fraud-proofs/native-tx/`                              |
| Transition-trace proof logic           | `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/`                       |
| Validation machine and dispute types   | `onchain/aiken/lib/midgard/validation-machine-v1.ak`, `validation-dispute-v1.ak` |
| Large-field verifier support           | `onchain/aiken/validators/fraud-proofs/mpf-chunked-proof/`                       |

## Catalogue validator directories

```text
double-spend                  no-input
input-no-idx                  invalid-range
transition-trace              zero-input
validation-trace              da-hash-preimage
no-reference-input            reference-input-no-idx
invalid-signature             fabricated-deposit
fabricated-withdrawal         native-script-decoding
missing-signature             missing-native-script-tx
withdrawn-reference-input     canonical-decodability
committed-field-shape         min-fee
withdrawal-mistag             double-withdraw
cross-block-duplicate-event   l2-tx-mistag
withdrawn-input               value-not-preserved
input-set-uniqueness          mint-authorization
network-id                    missing-native-script-utxo
native-script-invalid         min-ada
```

These 32 directories correspond positionally to the SDK catalogue.
`mpf-chunked-proof` is the 33rd direct child and is not a category.

## Final three families

### `missing-native-script-utxo` (`0000001d`)

Seven steps bind the challenged transaction, select the spent input, prove the
predecessor UTxO and credential, authenticate native-script material, perform
bounded staged parsing/evaluation where required, and finalize only the fault
verdict. Tests live in `staged-v1.test.ak`.

### `native-script-invalid` (`0000001e`)

Five steps bind the transaction and native witness, scan the bounded address-
witness frontier, carry a resumable evaluator cursor/stack, and finalize only
when the selected native script evaluates false. Tests live in
`staged-v1.test.ak`.

### `min-ada` (`0000001f`)

Five steps support transaction-output and post-UTxO violation shapes. They use
the same canonical minimum-Ada function and production parameter snapshot as
the validation machine, authenticate membership/non-membership as required,
and reject exact-floor or inherited-underfunding false accusations. Tests live
in `family-v1.test.ak`.

## Invariants for every standalone family

- Step 01 binds the exact challenged HeaderV1 and authentic transaction or
  family source.
- Every transition consumes the expected thread NFT and exact predecessor
  datum.
- Every continuing output carries the same prover, category, and header.
- Each successor script hash is exact and deployment-authenticated.
- Cancellation requires the prover.
- Only an adjudicated terminal state can burn the thread and mint the permanent
  proof token.
- State-queue removal authenticates structural ancestry and the permanent proof
  token before removing the target.

## Environment

Build and deploy with:

```bash
cd onchain/aiken
aiken check
aiken build --env testnet
```

The default Aiken environment is not the demo/preprod/e2e deployment identity.
