# Field-preimage-length-mismatch V1 size and transition plan

Status: implementation prerequisite for Wave 1 category `fieldPreimageLengthMismatch`
(proposed catalogue ID `00000020`). This document was written before the
family source files.

## Narrow deployed seam

The family proves exactly `FieldPreimageLengthMismatch { field_index }` in both
directions. Its authenticated subject is one accepted transaction leaf or one
forced-transaction leaf, and its decisive evidence is one of that transaction's
nine committed raw field preimages plus the corresponding declared length from
the authenticated native proof source.

No item-width, CBOR-decoder, output-descriptor, observer, signer, script-source,
redeemer, execution, integrity-hash, asset-fold, or ledger-input subject adapter
is imported by any applied script. The field-opening import authenticates a
whole raw preimage only; it does not open an item or apply another field rule.

## Canonical state and transitions

1. `Init -> Dispatched`: the first-step validator authenticates an accepted
   source and carries its canonical source CBOR to the accepted authentication
   script, or records the forced direction and requires the forced
   authentication script. The two successors are fixed applied hashes.
2. `Dispatched -> Bound`: the direction-specific physical script authenticates
   the raw preimage against the selected positional field commitment, derives
   the declared and actual byte lengths, and for forced rejection binds the
   exact `FieldPreimageLengthMismatch { field_index }` reason and coordinate.
3. `Bound -> Final`: the common terminal re-checks canonical state, evaluates
   `declared_length != actual_length`, applies the shared terminal polarity
   (`true` for wrongful acceptance, `false` for wrongful rejection), burns the
   computation-thread token, and mints the permanent proof token.

All four applied validators retain the shared authenticated cancel path. No
resumable checkpoint exists because the maximum raw preimage is authenticated
in one bounded whole-field opening and the decisive rule is constant time.

## Physical applied validators

| Ordered role           | Applied validator                                                         | Parameters, in order                                                                                                                 | Imported semantic engine                                                                                              | Maximum dynamic evidence                                                                                                 | Fresh testnet raw body |
| ---------------------- | ------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ | ---------------------: |
| first-step hash role   | `fraud_proofs/field_preimage_length_mismatch/step_01.main.spend`          | `accepted_step_02_validator_script_hash`, `forced_step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle` | shared native accepted-leaf carriage and Wave-0 accepted-subject binder; no decisive rule                             | accepted native inclusion or constant-size forced dispatch                                                               |            9,347 bytes |
| accepted authenticator | `fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend` | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id`                        | exact native proof-source verifier and family-only authenticated-length reducer                                       | one raw authenticated field through inline, raw-UTxO, or certified carriage, at most 32,768 bytes                        |            9,967 bytes |
| forced authenticator   | `fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend`   | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id`                        | Wave-0 forced-subject/reason binder, exact native proof-source verifier, and family-only authenticated-length reducer | forced membership plus one raw authenticated field through inline, raw-UTxO, or certified carriage, at most 32,768 bytes |           12,175 bytes |
| common terminal        | `fraud_proofs/field_preimage_length_mismatch/step_03.main.spend`          | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id`                                     | shared terminal contradiction, generic finalizer, and family-only length predicate                                    | constant-size subject, field index, declared length, and actual length                                                   |            1,915 bytes |

The terminal script imports no native compact decoding or field carriage. The
first-step dispatcher imports no field semantic engine. Only the forced
authenticator imports a rejection reason, and it pattern matches exactly
`FieldPreimageLengthMismatch`; no unrelated subject adapter enters any applied
body.

## Fit tests and ledger

Build with the pinned compiler and testnet environment, then use the fresh
blueprint through `MIDGARD_REAL_BLUEPRINT_PATH`.

The family fit suite must publish all four fully applied reference scripts in
complete signed transactions and measure every lifecycle transaction:
accepted dispatch/authentication, forced dispatch/authentication in both verdict
polarities, raw/certified maximum field carriage, terminal mint, cancel from
each nonterminal physical step, and state-queue target/descendant removal. The
machine-readable ledger records signed
bytes, memory, CPU, and margins against 16,384 bytes, 16,500,000 memory, and
10,000,000,000 CPU. Publication acceptance additionally requires signed bytes
at or below the 15,872-byte reserve target.

Boundary fixtures are the maximum 32,768-byte field preimage and the adjacent
32,769-byte refusal. Positive scenarios use the shared Van Rossem parameters,
local UPLC evaluation, and no `oversized`, `maxTxSize`, or ExUnit override.

The raw-body measurements above come from the freshly built testnet blueprint.
They are diagnostic only: complete signed authenticated reference-script
publication transactions remain the acceptance surface.

### Current family-local evidence (2026-09-01)

The deterministic machine-readable ledger is
[`field-preimage-length-mismatch-v1-fit-ledger.json`](./field-preimage-length-mismatch-v1-fit-ledger.json).
It is bound to the pinned compiler and exact testnet blueprint SHA-256 and is
reproduced by the family-local fit-ledger test.

- Pinned compiler: `aiken v1.1.23+5adf783`.
- `aiken build --env testnet`: passed against the shared checkout.
- Fresh raw bodies: 9,347 / 9,967 / 12,175 / 1,915 bytes in the ordered table
  above; all are below the 15,872-byte signed-publication target before wrapper
  overhead, but raw size is not publication acceptance.
- Focused decisive-rule selector: 8 collected, 8 passed, including maximum and
  adjacent-over-bound.
- SDK ABI/semantic suite: six tests pass, including exact CBOR goldens for the
  dispatch, shared authentication state, accepted authentication, and terminal
  redeemers.
- Durable family workflow suite: five tests pass over the five transaction
  actions (`init`, `dispatch`, `authenticate`, `finalize`, `remove`) and restart
  transaction-identity reconciliation.
- Manifest-bound configuration and routing suite: fifteen tests pass. The
  loader binds the finalized manifest, fresh blueprint digest, catalogue
  identity, signer/network, all four distinct reference-script out-refs, and
  shared witness references. Routing selects accepted versus forced physical
  scripts from admitted direction only and exposes every cancel/removal slot.
- Complete signed reference-script publication under the emulator's Van
  Rossem parameters now measures, in physical order: 9,764 bytes (6,620-byte
  margin), 10,349 (6,035), 12,558 (3,826), and 2,308 (14,076). Publication
  transactions execute no Plutus redeemers, so their CPU and memory totals are
  correctly zero.
- The direct retained-DA accepted preparer rebuilds the raw transactions PHAS,
  checks its counted root and cardinality against the L1 header, requires the
  retained canonical transaction to reproduce the committed transaction id,
  compact bytes, and witness-set compact bytes, and treats only the committed
  length vector as disputed. A regression proves whole-block reconstruction
  rejects that same malformed source while this direct preparer authenticates
  and convicts it.
- Real Lucid coverage starts at registered-category generic `Init` and completes
  both terminal paths. Wrongful acceptance performs PHAS-authenticated dispatch,
  field authentication, proof mint, mutation-leased descendant removal, and
  target removal. Canonical wrongful
  rejection performs forced direction-1 dispatch, forced-leaf membership and
  exact reason/coordinate authentication, proof mint, and target removal.
  Direction 0 is refused locally against the contradictory rejected leaf and
  then cancelled. Independent real cancels execute at physical steps 0, 1, 2,
  and 3.
- The maximum 32,768-byte field uses real chunk publication plus the shared
  certificate policy and authenticates with signed size 821 bytes (15,563-byte
  margin), memory 1,106,472 (15,393,528 margin), and CPU 394,102,582
  (9,605,897,418 margin). The adjacent 32,769-byte actual preimage is refused
  before construction.
- Representative lifecycle ledger rows (Van Rossem parameters, local UPLC):
  accepted dispatch 2,532 bytes / 1,285,889 memory / 436,470,065 CPU; accepted
  inline authentication 713 / 885,473 / 292,909,632; forced authentication
  1,724 / 1,107,407 / 450,760,052; terminal mint 916 / at most 300,177 /
  107,672,726; cancel 611 / at most 115,208 / 40,916,566. Every row has a
  positive signed-byte, memory, and CPU margin; the test emits the complete
  machine-readable ledger on each run.
- The production surface now exports all ten concrete Lucid builder bindings,
  re-resolves authenticated stage out-refs/evidence before every action, and
  composes those builders with the manifest-bound direction router and durable
  journal runner. Restart coverage proves a captured transaction identity is
  observed and reconciled before any resubmission.
- Non-inline production evidence now retains the authenticated compact,
  witness-set compact, and exact item CBOR locally, then deterministically
  selects and actuates `RawUtxo` or `Certified` carriage from that material.
  Publication and certificate transaction intents are journaled before submit,
  confirmations reconcile idempotently after restart, and certificate
  transaction substitution is refused. Accepted and forced preparations cover
  both non-inline tiers at the 14,337-byte and 32,768-byte boundaries; 32,769
  bytes remains a pre-construction refusal.

The primary integrator has installed proposed ID `00000020`, the applied
contracts, deployment/reference-script identities, generic runtime category
map, and real emulator-chain option in the shared checkout. No raised limit,
unsafe cast, caller verdict, or reconstructed-whole-block success substitutes
for the authenticated family evidence.

The family-local semantic suites cover both honest polarities, wrong reason,
coordinate, transaction, maximum+1, malformed-length, and ABI/state mutations;
the Lucid suites cover both terminal directions, forced membership
authentication, inline and certified carriage, cancellation at every physical
state, mint, and removal. Central watcher/registry registration remains outside
this family-local change boundary.

### Audit against the program contract (2026-09-04)

The family was re-audited item by item against the program's §5.1–§5.4 and
the Wave 1 gate on blueprint SHA-256
`172c72d392706de52b5665dc0c1b208354a490298fba5ac0f411597f8454d10b`
(`aiken v1.1.23+5adf783`, `aiken build --env testnet`). No applied script
changed; every hash in the ledger above is the registered one.

- **Registered chain.** The lifecycle suite
  (`tests/field-preimage-length-mismatch-lifecycle.test.ts`) drives
  `harness.contracts.fraudProofContracts.fieldPreimageLengthMismatch`, the
  chain the harness folds into the catalogue root, and pins it with
  `expectRegisteredChainParity` against both the family SDK builder
  (`buildFieldPreimageLengthMismatchFaultProofContracts`) and the central SDK
  chain builder (`buildFaultProofContracts`), plus the catalogue category's
  first-step hash and ID `00000020`. Removal resolves through the canonical
  catalogue record by category name.
- **On-chain refusals, not preparer refusals.** Every §5.3 negative is now a
  refusal by the applied script under local UPLC evaluation, recorded by the
  suite as it runs and closed by `assertCompleteLifecycleCoverage`: honest
  accepted block (terminal), honest forced rejection over a truly mismatched
  leaf (terminal), reason/coordinate mutation on both doors, subject
  transaction-id mutation, substituted transaction leaf (PHAS membership),
  substituted forced leaf (forced-transactions root), flipped preimage byte,
  reordered certified chunks, and the forced authenticator offered as the
  accepted successor. The wrongful-acceptance direction additionally completes
  through the forced door over a `ForcedTxValid` leaf whose committed vector
  overstates field 0, so both physical authenticators reach the proof mint.
  Leaves that whole-block reconstruction refuses by design are opened straight
  from the retained root entries
  (`tests/support/field-preimage-length-mismatch-forced-fixture.ts`), the same
  provenance the accepted preparer uses.
- **Aiken selectors.** `authenticated.test.ak` adds 18 selectors over the real
  verified proof source and committed structures: both successful directions,
  a witness-set field, both honest refusals, mutated reason coordinate and
  other reason constructor, coordinate nine / negative / body-claim-on-witness
  field, non-canonical length vector, compact of another transaction, padded
  preimage, substituted witness set, the 32,768-byte certified maximum, the
  32,769-byte adjacent refusal at the door, a certificate naming another
  field, and reordered chunks. With `rule.test.ak` the family selector
  `midgard/fraud_proofs/field_preimage_length_mismatch.{..}` collects 26.
  There is no resumable checkpoint, so the malformed-checkpoint arm does not
  apply; the wrong-successor arm is the lifecycle's on-chain refusal above.
- **Adjacent over-bound.** A 32,769-byte field has no admissible carriage:
  the workflow preparer and the carriage planner both refuse before any
  transaction exists, and the applied reducer's own bound is exercised by the
  certified-carriage selector.
- **Ledger.** Regenerated against the fresh blueprint in the shared
  `midgard-van-rossem-fit-ledger-v1` schema, 35 rows. Minimum margins: 512
  signed bytes (the two full 15,872-byte certified chunk publications, exactly
  at the reserve target), 13,407,804 memory and 8,948,409,811 CPU (the
  forced-accepted removal). The 32,768-byte certified authentication measures
  821 bytes / 1,103,604 memory / 393,210,335 CPU.

## Central wiring intentionally deferred

This family document and implementation do not edit catalogue order/root, SDK
`FraudProofs` unions, generated blueprint, deployment manifests, watcher or
runner registries, or central status documents. The implementation handoff
supplies those exact identities to the primary integrator.
