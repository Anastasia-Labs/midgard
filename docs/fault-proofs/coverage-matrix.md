# Fault-Proof Coverage Matrix

Status: Active

Last reviewed: 2026-11-23 (rule rows against the catalogue).

The rule mapping below is a partial explanatory matrix, not an exhaustive
category roster or a fresh test receipt. [Catalogue status](catalogue-status.md)
owns the complete source inventory; [testing status](testing-status.md) scopes
acceptance evidence; and the [remaining-gap tracker](remaining-gaps.md) owns
known protocol statements that are impossible or only partially provable.
Later categories may refine a rule into narrower families.

This matrix distinguishes proof routes from release evidence. A compiled family is not release-complete
until its evidence remains constructible, its transactions fit, its valid-block
negative passes, and its full correction lifecycle is exercised.

## Transaction and ledger rules

| Rule or fault class                        | Canonical enforcement/proof route                               | Standalone category                            |
| ------------------------------------------ | --------------------------------------------------------------- | ---------------------------------------------- |
| Duplicate spend across transactions        | direct membership/equality proof                                | `doubleSpend`                                  |
| Missing spent input                        | double non-membership                                           | `nonExistentInput`                             |
| Spend output index out of range            | transaction/output-count opening                                | `nonExistentInputNoIndex`                      |
| Invalid validity interval                  | header/transaction interval comparison                          | `invalidRange`                                 |
| Zero spend inputs                          | authenticated empty field                                       | `zeroInput`                                    |
| Missing reference input                    | double non-membership                                           | `noReferenceInput`                             |
| Reference output index out of range        | transaction/output-count opening                                | `referenceInputNoIdx`                          |
| Invalid address signature                  | Ed25519 verification                                            | `invalidSignature`                             |
| Missing required signature                 | required-signer/witness frontier                                | `missingSignature`                             |
| Missing script source                      | inline-witness and resolved reference-script scan               | `missingScriptSource`                          |
| Invalid native script                      | bounded signer scan and resumable evaluator                     | `nativeScriptInvalid`                          |
| Withdrawn spend/reference input            | withdrawal/event and ledger proofs                              | `withdrawnInput`, `withdrawnReferenceInput`    |
| Duplicate/overlapping input sets           | ordered set scan                                                | `inputSetUniqueness`                           |
| Value not preserved                        | authenticated input/output/mint comparison                      | `valueNotPreserved`                            |
| Unauthorized native-policy mint            | policy/script evidence                                          | `mintAuthorization`                            |
| Wrong transaction/output network           | transaction/output address proof                                | `networkId`                                    |
| Minimum fee violation                      | exact canonical transaction size and fee formula                | `minFee`                                       |
| Minimum Ada violation                      | exact output size and canonical minimum-Ada formula             | `minAda`                                       |
| Non-canonical or malformed committed field | canonical decode/shape proofs                                   | `canonicalDecodability`, `committedFieldShape` |
| Plutus/MidgardV1 execution failure         | interactive validation-machine bisection and CEK one-step proof | `validationTraceDispute`                       |

## Transition and event rules

| Rule or fault class                                                              | Category               |
| -------------------------------------------------------------------------------- | ---------------------- |
| Trace boundary/link/source/event/count/duplicate/omission/window/one-step faults | `transitionTrace`      |
| DA transaction key/preimage mismatch                                             | `daHashPreimage`       |
| Fabricated deposit                                                               | `fabricatedDeposit`    |
| Fabricated withdrawal                                                            | `fabricatedWithdrawal` |
| Withdrawal validity mistag                                                       | `withdrawalMistag`     |
| Duplicate payable withdrawal                                                     | `doubleWithdraw`       |
| Normal L2 transaction mistagged invalid                                          | `l2TxMistag`           |

## Structural non-categories

These are intentionally not additional catalogue entries:

| Concern                                                       | Disposition                                                                                                         |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| Separate required-signer membership family                    | Structural N/A: covered by the authenticated Signatures path and `missingSignature`                                 |
| ADA minting                                                   | Canonical ADA minting is unrepresentable; malformed empty-policy field-5 bytes are proved by `mintItemNonCanonical` |
| Negative output value                                         | Structural N/A under canonical output/value decoding                                                                |
| Valid normal transaction made a no-op                         | Covered by accepted validation claim plus transition-trace binding                                                  |
| Valid forced transaction made a no-op or wrong forced verdict | Typed deterministic fault family or interactive execution dispute, plus transition-trace effect binding             |
| Shared large-field verification                               | `mpf-chunked-proof` support machinery, not a category                                                               |
| Unattested commitment timeout                                 | Separate no-slash suffix correction at any pending queue position                                                   |
| Post-attestation data withholding                             | Availability challenge: timeout removal with a DA bond-pool slash, not a transaction fault category                 |

## Open release gaps

Release acceptance (emulator catalogue closure under the shared Van Rossem
limits, valid-block negatives, correction, economics, and data lifetime) is
tracked in the [release acceptance plan](execution-plan.md); protocol statements
that are impossible or only partially provable are tracked in
[remaining gaps](remaining-gaps.md), including forced-order evidence lifetime
([NIFP-03](remaining-gaps.md#nifp-03--event-proof-lifetime)). The user escape
hatch is not implemented; [public-testnet readiness](../public_testnet_readiness.md)
marks it unsupported scope.
