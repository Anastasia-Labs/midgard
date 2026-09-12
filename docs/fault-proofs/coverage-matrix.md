# Fault-Proof Coverage Matrix

Status: Active

Documentation disposition reviewed: 2026-09-07.

The rule mapping below is a partial explanatory matrix, not an exhaustive
category roster or a fresh test receipt. [Catalogue status](catalogue-status.md)
owns the complete source inventory; [testing status](testing-status.md) scopes
acceptance evidence. Later categories may refine a rule into narrower families.

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
| Missing native script in transaction       | script credential/witness-set proof                             | `missingNativeScriptTx`                        |
| Missing native script at predecessor UTxO  | predecessor membership plus script-material proof               | `missingNativeScriptUtxo`                      |
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

| Rule or fault class                                                              | Category                   |
| -------------------------------------------------------------------------------- | -------------------------- |
| Trace boundary/link/source/event/count/duplicate/omission/window/one-step faults | `transitionTrace`          |
| DA transaction key/preimage mismatch                                             | `daHashPreimage`           |
| Fabricated deposit                                                               | `fabricatedDeposit`        |
| Fabricated withdrawal                                                            | `fabricatedWithdrawal`     |
| Withdrawal validity mistag                                                       | `withdrawalMistag`         |
| Duplicate payable withdrawal                                                     | `doubleWithdraw`           |
| Cross-block duplicate L1 event                                                   | `crossBlockDuplicateEvent` |
| Normal L2 transaction mistagged invalid                                          | `l2TxMistag`               |

## Structural non-categories

These are intentionally not additional catalogue entries:

| Concern                                                       | Disposition                                                                                             |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| Separate required-signer membership family                    | Structural N/A: covered by the authenticated Signatures path and `missingSignature`                     |
| ADA minting                                                   | Canonical ADA minting is unrepresentable; malformed empty-policy field-5 bytes are proved by `mintItemNonCanonical`                                      |
| Negative output value                                         | Structural N/A under canonical output/value decoding                                                    |
| Valid normal transaction made a no-op                         | Covered by accepted validation claim plus transition-trace binding                                      |
| Valid forced transaction made a no-op or wrong forced verdict | Typed deterministic fault family or interactive execution dispute, plus transition-trace effect binding |
| Shared large-field verification                               | `mpf-chunked-proof` support machinery, not a category                                                   |
| Unattested head timeout                                       | Separate no-slash state-queue correction path                                                           |
| Post-attestation data withholding                             | DA remedy/recovery problem, not a transaction fault category                                            |

## Cross-cutting gaps

1. **Emulator catalogue closure:** verify every enabled family and maximum
   supported shape on the release blueprint. Use [testing status](testing-status.md)
   for executable verification surfaces and evidence requirements.
2. **Autonomous application acceptance:** the watcher installs the source
   catalogue; release acceptance must verify admitted runners and complete
   correction on the intended deployment, not merely topology or classification.

3. **Data lifetime:** every proof input must remain authentic and
   retrievable through the complete challenge window, including after event
   NFT consumption or settlement.
4. **Transaction fit:** every maximum-shape lifecycle must pass with the shared
   Van Rossem 16,384-byte, 16.5M-memory, and 10B-CPU emulator limits. Tests may
   not raise these limits to establish completion.
5. **Soundness:** every family needs a valid-block negative at the same
   evidence frontier as its positive.
6. **Correction integration:** local structural pruning and re-inclusion exist;
   concurrent real-node and preprod correction remain unaccepted.
7. **Economics:** exact routing and non-zero compiled profiles exist;
   concurrent-claim acceptance and live balance conservation remain open.
8. **DA and liveness:** post-attestation withholding and the independent user
   escape hatch remain system-level launch blockers.

## Security judgement

The earlier “missing verifier family” gap for minimum Ada, missing predecessor
native-script material, and invalid native scripts is closed in source. The
highest remaining fault-proof risks are operational and evidentiary: a family
that is not installed, cannot obtain retained evidence, exceeds an L1 limit, or
has not been accepted against a real deployment can still fail to protect the
optimistic reserve before maturity.
