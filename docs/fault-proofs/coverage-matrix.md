# Fault-Proof Coverage Matrix

Current coverage reviewed against the working tree on 2026-09-01. This matrix distinguishes implemented
verifiers from release evidence. A compiled family is not release-complete
until its evidence remains constructible, its transactions fit, its valid-block
negative passes, and its full correction lifecycle is exercised.

## Transaction and ledger rules

| Rule or fault class                        | Canonical enforcement/proof route                               | Standalone category                            | Current state                                        | Remaining release work                                                  |
| ------------------------------------------ | --------------------------------------------------------------- | ---------------------------------------------- | ---------------------------------------------------- | ----------------------------------------------------------------------- |
| Duplicate spend across transactions        | direct membership/equality proof                                | `doubleSpend`                                  | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Missing spent input                        | double non-membership                                           | `nonExistentInput`                             | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Spend output index out of range            | transaction/output-count opening                                | `nonExistentInputNoIndex`                      | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Invalid validity interval                  | header/transaction interval comparison                          | `invalidRange`                                 | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Zero spend inputs                          | authenticated empty field                                       | `zeroInput`                                    | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Missing reference input                    | double non-membership                                           | `noReferenceInput`                             | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Reference output index out of range        | transaction/output-count opening                                | `referenceInputNoIdx`                          | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Invalid address signature                  | Ed25519 verification                                            | `invalidSignature`                             | Implemented; emulator removal covered                | watcher/preprod                                                         |
| Missing required signature                 | required-signer/witness frontier                                | `missingSignature`                             | Implemented; maximum-field emulator coverage         | watcher/preprod                                                         |
| Missing native script in transaction       | script credential/witness-set proof                             | `missingNativeScriptTx`                        | Implemented; direct/staged emulator coverage         | watcher/preprod                                                         |
| Missing native script at predecessor UTxO  | predecessor membership plus script-material proof               | `missingNativeScriptUtxo`                      | Aiken and off-chain implemented                      | standalone emulator, watcher install, preprod                           |
| Invalid native script                      | bounded signer scan and resumable evaluator                     | `nativeScriptInvalid`                          | Aiken/off-chain implemented; watcher installed       | standalone emulator and preprod                                         |
| Withdrawn spend/reference input            | withdrawal/event and ledger proofs                              | `withdrawnInput`, `withdrawnReferenceInput`    | Implemented; emulator coverage                       | watcher/preprod                                                         |
| Duplicate/overlapping input sets           | ordered set scan                                                | `inputSetUniqueness`                           | Implemented; tier-2 emulator removal covered         | watcher/preprod                                                         |
| Value not preserved                        | authenticated input/output/mint comparison                      | `valueNotPreserved`                            | Implemented; ADA/token emulator removal covered      | cancel drive, watcher install, preprod                                  |
| Unauthorized native-policy mint            | policy/script evidence                                          | `mintAuthorization`                            | Implemented; both-direction emulator removal covered | cancel drive, watcher install, preprod                                  |
| Wrong transaction/output network           | transaction/output address proof                                | `networkId`                                    | Implemented; emulator removal/cancel covered         | preprod                                                                 |
| Minimum fee violation                      | exact canonical transaction size and fee formula                | `minFee`                                       | Implemented; both-polarity emulator removal covered  | preprod                                                                 |
| Minimum Ada violation                      | exact output size and canonical minimum-Ada formula             | `minAda`                                       | Aiken and off-chain implemented                      | standalone emulator, watcher install, preprod                           |
| Non-canonical or malformed committed field | canonical decode/shape proofs                                   | `canonicalDecodability`, `committedFieldShape` | Implemented; emulator removal covered                | watcher/preprod and remaining tier-3 cases                              |
| Plutus/MidgardV1 execution failure         | interactive validation-machine bisection and CEK one-step proof | `validationTraceDispute`                       | Implemented locally across major semantic routes     | exhaustive differential/rejection-terminal, proof-fit, watcher, preprod |

## Transition and event rules

| Rule or fault class                                                              | Category                   | Current state                                                                                         | Remaining release work                               |
| -------------------------------------------------------------------------------- | -------------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| Trace boundary/link/source/event/count/duplicate/omission/window/one-step faults | `transitionTrace`          | Routed validator graph, retained-DA detection, submission, representative emulator finals and removal | watcher application and complete live/preprod corpus |
| DA transaction key/preimage mismatch                                             | `daHashPreimage`           | Implemented; local proof/removal lifecycle                                                            | watcher/preprod                                      |
| Fabricated deposit                                                               | `fabricatedDeposit`        | Implemented; emulator reaches permanent mint and rejects honest evidence                              | drive removal; watcher/preprod                       |
| Fabricated withdrawal                                                            | `fabricatedWithdrawal`     | Implemented; emulator reaches permanent mint and rejects honest evidence                              | drive removal; watcher/preprod                       |
| Withdrawal validity mistag                                                       | `withdrawalMistag`         | Implemented; both-polarity emulator removal                                                           | watcher/preprod                                      |
| Duplicate payable withdrawal                                                     | `doubleWithdraw`           | Implemented; on-chain honest non-payable refusal and emulator removal                                 | final spec wording, preprod                          |
| Cross-block duplicate L1 event                                                   | `crossBlockDuplicateEvent` | Implemented; deposit/withdrawal emulator removal                                                      | settlement-history retention, watcher/preprod        |
| Normal L2 transaction mistagged invalid                                          | `l2TxMistag`               | Implemented; adversarial emulator removal                                                             | preprod                                              |

## Structural non-categories

These are intentionally not additional catalogue entries:

| Concern                                                       | Disposition                                                                         |
| ------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| Required-signer-set duplicate family                          | Structural N/A: covered by the authenticated Signatures path and `missingSignature` |
| ADA minting                                                   | Structural N/A under canonical value encoding and validation rules                  |
| Negative output value                                         | Structural N/A under canonical output/value decoding                                |
| Valid normal transaction made a no-op                         | Covered by accepted validation claim plus transition-trace binding                  |
| Valid forced transaction made a no-op or wrong forced verdict | Covered by forced-source validation claim and `validationTraceDispute`              |
| Shared large-field verification                               | `mpf-chunked-proof` support machinery, not a category                               |
| Unattested head timeout                                       | Separate no-slash state-queue correction path                                       |
| Post-attestation data withholding                             | DA remedy/recovery problem, not a transaction fault category                        |

## Cross-cutting gaps

1. **Emulator catalogue closure:** standalone Lucid lifecycles exist for all
   three final families, but the shared setup path is red under Van Rossem's
   16,384-byte limit.
2. **Autonomous application coverage:** the watcher installs 25 of 32
   categories; topology and classification are broader than executable
   installation.
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
