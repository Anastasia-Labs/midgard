# Genuine workflow funding measurements

The F04 measurement producer captures a signed SDK proof transaction at the
existing `FraudProofPreSubmitBoundary`. It reads every exact spending,
reference and collateral input before submission, independently runs the
pinned Lucid Aiken phase-two evaluator, and retains signed budgets, actual
execution units, exact fees, physical reference-script bytes, output minimum
ADA and collateral return. The original SDK owns submission and confirmation.
The measurement callback never submits or creates a wallet transaction.

Create `createWorkflowFundingMeasurementContext` with the actual finalized
manifest, raw blueprint bytes, the declared blueprint build record, the
complete R3 parameter registry, the exact source snapshot digest and the
funding payment key hash. The context binds the blueprint bytes, Aiken source
digest and selected deployment profile to the build record, verifies raw
parameter reapplication against every deployed program and snapshots those
inputs. This records build identity; it does not replace the persistent
release authority, independent compiler rebuild or image correspondence.
`environment: "emulator"` identifies isolated emulator evidence explicitly.
Production collection uses `environment: "local-node"` and the deployment's
actual network and slot mapping. An explicitly branded emulator records its
own evaluator mapping separately from the applied deployment mapping; this
is isolated fixture evidence and cannot pass real-node mapping admission.

The initial supported capture is `measureComputationThreadInitFunding`.
Pass the transaction supplied by the shared SDK Init callback, its Lucid
client and its deployed category. The producer decodes the actual Init
redeemer, checks the catalogue category and first-step credential, and derives
prover capital from resolved wallet inputs, wallet change and the thread
output. A minted computation-thread identity NFT is recorded as identity,
not purchased capital. The existing funding-requirements consumer verifies
the funder signature and exact value/custody conservation.

The operational `captureAndWriteComputationThreadInitFunding` call creates
that context, captures the actual transaction and writes the artifact using
one package instance. Alternatively, write the returned in-process record with
`writeWorkflowFundingMeasurement` from `midgard-node-tools/funding-measurements`.
The caller supplies a new private evidence file. The writer uses exclusive
creation with mode `0600`, synchronizes the exact canonical artifact bytes and
returns their SHA-256. It refuses copied, mutated or reconstructed objects.
The file's hash is the measurement-artifact hash; the file contains no
self-referential hash field. Signed CBOR and resolved custody data stay in the
private evidence directory. No release key or wallet secret enters it.

Every current artifact records `transactionStatus: "locally_evaluated_prepared"`;
submission/confirmation evidence is retained by the SDK driver separately. It
has `status: "partial"` and explicit missing lifecycle
and maximum-shape coverage. `workflowFundingMeasurementCoverage` derives the
active category roster from the actual selected manifest; empty, foreign and
duplicate captures cannot close it. `assertCompleteWorkflowFundingMeasurements`
refuses this initial producer's incomplete corpus. These records cannot be
advertised as complete public funding profiles.

Remaining collection must capture real family steps and forced directions,
field carriers and chunk scans, proof completion, descendant pruning,
correction/removal, retries and every reviewed maximum reachable shape. DA
challenge/response/timeout funding needs its own genuine lifecycle capture.
All fraud-proof initiation is permissionless; it must not introduce a
challenger allow-list. Restored Plutus disputes require their actual
permissionless action and actor sequence, including thread-funded moves with
another mover's collateral. Actual wallet witnesses and custody bindings
remain necessary. The current single-funder consumer cannot silently attribute
another actor's contribution or invent an operator or prover cosignature.
The PD owner must supply the reviewed `threadFeeFunded` projection before those
moves can become measured funding actions.

The genuine producer test loads the built `dist/funding-measurements.js`
entry explicitly. With `MIDGARD_FUNDING_MEASUREMENT_OUTPUT_DIR` set to a private
untracked directory, it retains the actual measurement file and a separate
emulator confirmation receipt there. Default test runs remove their temporary
outputs. These test artifacts do not establish public deployment or funding.

Final production collection, owner-set funding budgets/addresses, the complete
public corpus and the signed release profiles remain separate release gates.
The persistent release author signs the completed profiles only after those
gates, using the established external trust-root boundary. The producer does
not select bond amounts, fund wallets, reset state or deploy contracts.
