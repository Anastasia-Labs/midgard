# On-Chain Fault-Proof Reference

Status: Active

Last reviewed: 2026-09-07 (source map).

## Compiled identity

See [catalogue status](catalogue-status.md) for source category IDs and how to
bind a generated blueprint to deployment acceptance. The generated
`onchain/aiken/plutus.json` is not a checked-in release artifact. Counts and
hashes from older builds do not identify the current deployment.

## Shared validators and libraries

| Surface                                | Source                                                                                                |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| Catalogue policy/validator             | `onchain/aiken/validators/fraud-proof-catalogue.ak`                                                   |
| Computation-thread policy              | `onchain/aiken/validators/computation-thread.ak`                                                      |
| Permanent proof token                  | `onchain/aiken/validators/fraud-proof.ak`                                                             |
| State-queue dispatch, init, and deinit | `onchain/aiken/validators/state-queue.ak`                                                             |
| State-queue operational arms           | `onchain/aiken/validators/state-queue-yields.ak`                                                      |
| Common family binding/cancel/finalize  | `onchain/aiken/lib/midgard/fraud-proofs/common.ak`                                                    |
| Native transaction commitments/codecs  | `onchain/aiken/lib/midgard/fraud-proofs/native-tx/`                                                   |
| Transition-trace proof logic           | `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/`                                            |
| Validation machine and dispute types   | `onchain/aiken/lib/midgard/validation-machine/`, `onchain/aiken/lib/midgard/validation-dispute-v1.ak` |
| Large-field verifier support           | `onchain/aiken/validators/fraud-proofs/mpf-chunked-proof/`                                            |

## Catalogue validator directories

Family validators live under `onchain/aiken/validators/fraud-proofs/`. The SDK
catalogue's explicit ID map determines category identity; filesystem order and
shared-helper directories do not. See [catalogue status](catalogue-status.md)
for the complete inventory.

## Native-script and minimum-Ada families

### `missing-native-script-utxo` (`0000001d`)

Seven steps bind the challenged transaction, select the spent input, prove the
predecessor UTxO and credential, bind native-script material to that credential,
and prove absence from the transaction's script witnesses through direct or
bounded grammar/scan continuations. Only the absence verdict can finalize. Tests live in `staged-v1.test.ak`.

### `native-script-invalid` (`0000001e`)

Five steps bind the transaction and native witness, scan the bounded address-
witness frontier, carry a resumable evaluator cursor/stack, and finalize only
when the selected native script contradicts the authenticated verdict: false
for accepted-invalid evidence, true for wrongful forced rejection. Tests live in
`staged-v1.test.ak`.

### `min-ada` (`0000001f`)

Five spending steps and two authenticated rewarding-script yields support
transaction-output and post-UTxO violation shapes. They use
the same canonical minimum-Ada function and production parameter snapshot as
the validation machine, authenticate membership/non-membership as required,
and reject exact-floor or inherited-underfunding false accusations on the
accepted-invalid route. Wrongful forced `OutputBelowMinAda` rejection instead
requires sufficiency, including the exact floor, at the authenticated output
index. Tests live
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
aiken check --env testnet
aiken build --env testnet
```

The default Aiken environment is not the demo/preprod/e2e deployment identity.
