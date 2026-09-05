# `spendInputSignerMissing` V1 maximum-shape and transition plan

- Frozen category ID: `00000027`.
- Typed rejection reason: `SpendInputSignerMissing { input_index }`.
- Subject: one field-0 spend input of an accepted or forced native transaction,
  the pub-key payment credential of the output it references in the header's
  committed `prev_utxos_root`, and the same transaction's authenticated field-7
  address-witness collection.
- Logical topology: five family-owned computation-thread steps after generic
  `Init`.

## Applied validators, state, and parameter order

1. `fraud-proofs/spend-input-signer-missing/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)`: authenticates the
   accepted or forced transaction, binds the exact field-0 `input_index`, the
   authenticated header's prior-UTxO root, and, for wrongful rejection, exactly
   `SpendInputSignerMissing { input_index }`. It forwards `BoundSpendInputV1`.
2. `fraud-proofs/spend-input-signer-missing/step-02`
   `(step_03_hash, step_05_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   opens exactly field 0 and reads its authenticated item count (field 0 is
   fixed-stride, so every carriage tier answers). A bound coordinate past
   that count closes directly at step 05 with `signer_required = False`,
   under the forced direction only. Otherwise it selects and canonically
   decodes the bound input item, proves membership of that exact out-ref and
   descriptor in the bound prior root, checks the descriptor index, and
   canonically decodes its address. A pub-key payment credential forwards
   the authenticated 28-byte credential together with the verdict subject
   and the transaction's witness-set anchor to step 03; a script payment
   credential closes directly at step 05 with `signer_required = False`,
   again under the forced direction only. The accepted direction has exactly
   one exit, unchanged.
3. `fraud-proofs/spend-input-signer-missing/step-03`
   `(step_04_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   opens the transaction's authenticated field-7 address-witness collection,
   checks its complete carriage/certificate commitment, asserts that the
   opened view's item count is authenticated (field 7 is fixed-stride, so no
   tier hands the scan a provisional §5.1 count), and initializes the
   canonical field-walk checkpoint. No caller-supplied signer frontier enters
   thread state.
4. `fraud-proofs/spend-input-signer-missing/step-04`
   `(step_05_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   resumes the digest-bound field walk in fixed batches of 16. Every item is
   canonically decoded and its Ed25519 signature is verified over the bound
   native transaction ID before its verification-key hash may enter the
   frontier. An invalid signature is never evidence that its key signed. A
   valid matching key terminates with `signer_missing = False`; exhausting all
   318 positions without one terminates with `signer_missing = True`.
   Otherwise the validator self-loops with only the next checkpoint digest.
5. `fraud-proofs/spend-input-signer-missing/step-05`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`:
   recomputes the accepted/forced polarity from the authenticated terminal
   verdict `VerdictV1 { subject, signer_required, signer_missing }` — the
   decisive fault is `signer_required && signer_missing` — burns the
   computation-thread token, and mints the permanent proof.

Every applied validator retains the common cancellation branch. Step 04 is the
only self-loop; step 02 is the only step with two exits. The state wire carries
no output bytes, witness preimage, public key, signature, membership proof, or
caller-selected reason.

## Direct terminal route for the forced direction

The witness-scan path proves a forced `SpendInputSignerMissing { input_index }`
wrong only by exhibiting a valid witness for a pub-key credential. Canonical
validation (`validation_machine/resolve_inputs`, then
`validation_machine/shared.input_signer_authorization`) emits that rejection
only when the machine's cursor reaches field-0 position `input_index`, the
input resolves in the prior ledger (otherwise the canonical reason is
`InputNotFound`), and `payment_credential_signer_authorization` sees a
`PubKeyCredential` with no valid signer; a `ScriptCredential` is
`InputSignerAuthorized` without any signer. Two shapes of a wrongful rejection
are therefore false without any witness at all, and the machine's ordering
places both inside this reason rather than another:

- a script-locked spend input: the machine evaluated the coordinate and
  needed no signer, so no `InputNotFound`/decoding reason precedes the claim;
- a coordinate at or past the field-0 item count: the machine's cursor never
  evaluates it, so no reason of any family is canonical there. It is
  contradicted here, by the authenticated `spend_input_count` of the same
  field-0 view the scan route opens, not routed to the coordinate rules.

An unresolved in-range input (canonical reason `InputNotFound`) stays outside
this family: contradicting it needs a prior-root non-membership proof, and the
forced path still requires membership. That boundary is recorded below.

| Step                                         | Wrongful acceptance (`direction = 0`)                        | Wrongful forced rejection (`direction = 1`)                                                                                                                                         |
| -------------------------------------------- | ------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 02, `input_index >= spend_input_count(view)` | refused (accepted subjects never take the direct exit)       | writes `VerdictV1 { subject, signer_required: False, signer_missing: False }` to `step_05_hash`                                                                                     |
| 02, in range, `ScriptCredential`             | refused, unchanged                                           | same direct verdict to `step_05_hash`                                                                                                                                               |
| 02, in range, `PubKeyCredential`             | `AuthenticatedCredentialV1` to `step_03_hash`, unchanged     | same, unchanged                                                                                                                                                                     |
| 03–04                                        | unchanged; step 04's terminal writes `signer_required: True` | same                                                                                                                                                                                |
| 05                                           | closes on `signer_required && signer_missing`                | closes on `!(signer_required && signer_missing)` for the exact carried reason, so both the direct verdict and a scan that found the signer close; a scan that found none is refused |

The redeemer of step 02 is unchanged in shape; on the out-of-range arm the
membership carriage and descriptor are not read. The direct exit is decided on
chain from the authenticated count and decoded credential, never from a
redeemer hint. State shapes stay parallel with `protectedOutputSignerMissing`
(`VerdictV1 { subject, signer_required, signer_present }` there).

## Maximum evidence and mutation frontier

The fixed field-7 address-witness stride is 103 bytes. The exact maximum field
therefore contains
`floor((32,768 - 3) / 103) = 318` witnesses; 319 is refused by the aggregate
field bound. That bound is the family's adjacent consensus bound, and it is
enforced below any lifecycle transaction: the L2 codec refuses to lay out a
32,860-byte field for carriage, and the door refuses the certified view over
it (`step_04_refuses_an_adjacent_over_bound_witness_field`). The lifecycle
suite therefore records no adjacent refusal of its own; the off-chain refusal
is pinned by the family unit test. The maximum run initializes once and
performs twenty step-04 transactions (nineteen 16-item batches and one 14-item
suffix). Each step re-opens field 7 through direct, published, or certified
carriage and resumes only from the checkpoint bytes whose digest the preceding
transaction stored.

The prior-ledger credential proof exercises the maximum supported MPF depth and
both raw-redeemer and published-proof-chunk carriage. Field 0 and field 7 each
exercise the complete carriage ladder, including the exact 32,768-byte
certified frontier and 32,769-byte refusal.

Required negatives are: wrong input coordinate; normal/reference source
substitution; transaction ID, prior root, out-ref, descriptor, output index, or
payment credential substitution; script-locked output; wrong reason
constructor or reason coordinate; forged witness-set hash; reordered/mutated
field-7 chunks; malformed witness; correct key with an invalid signature;
valid signature from the wrong key; checkpoint substitution; skipped batch;
premature terminal; missing signer under forced wrongful-rejection polarity;
present valid signer under accepted polarity; an accepted subject on the
direct exit; a direct verdict claiming `signer_required = True`; a direct
verdict for an in-range pub-key coordinate; and a direct exit continuing at
step 03. The two honest terminals are accepted-plus-missing and
forced-exact-reason-plus-present.

Where each negative is pinned:

- Aiken, `fraud_proofs/spend_input_signer_missing/step_02.{..}`: the direct
  exit for a script-locked forced coordinate and for a forced coordinate past
  the authenticated field-0 count; the pub-key hand-off to step 03 in both
  directions; an accepted subject refused on both direct arms; a direct
  verdict claiming a required signer; a direct verdict for a pub-key
  coordinate; a substituted direct-verdict coordinate; a substituted
  descriptor credential; and the direct exit sent to the step-03 successor.
- Aiken, `fraud_proofs/spend_input_signer_missing/step_03.{..}`: inline and
  certified frontier initialization; a field-6 certificate presented for
  field 7; a certificate minted over a substituted preimage; a substituted
  witness-set anchor; a wrong successor.
- Aiken, `fraud_proofs/spend_input_signer_missing/step_04.{..}`: the
  decisive fold (valid signature enters; invalid signature never enters;
  valid signature from the wrong credential never matches); exhaustion of an
  invalid witness and of an empty field to `signer_missing = True`; the
  16-item loop and its resumed suffix; substituted and malformed checkpoint
  bytes; a premature terminal; a claimed present signer; wrong successors on
  loop and on terminal; the adjacent 319-witness certified field.
- Aiken, `midgard/fraud_proofs/spend_input_signer_missing/rule.{..}`: both
  terminal polarities, the exact forced coordinate, the direct verdict
  closing only under the forced direction, and `signer_required = False`
  never closing an accepted subject.
- Lucid lifecycle, on the registered chain: both successful directions;
  cancel from every physical step including the resumed scan; the honest
  accepted block (a certified 160-witness field whose valid signature is in
  the tenth batch) refused by the family builder and by the generic
  finalizer; the honest forced rejection (a valid signature from the wrong
  key beside the right key with an invalid signature) refused the same way;
  a substituted transactions root, a substituted prior-output descriptor, a
  certificate honestly minted over another transaction's witness field, a
  substituted forced leaf, an out-of-range spend coordinate, and a shifted
  forced reason coordinate; the forced direct route over a script-locked
  spend input closing at step 05 straight from step 02 (the credential seam
  mutated: the same forced claim over a pub-key coordinate is refused on the
  direct exit, and the honest forced rejection over an unsigned pub-key
  coordinate is still refused at the terminal); cancel from the step-05
  thread the direct route produced.

## Reachability boundary

Step 01 reaches only verdict-subject/native-transaction authentication. Step 02
reaches only the field-0 door and its authenticated count, canonical out-ref
codec, prior-root MPF membership, descriptor/address decoder, and payment
credential classification. Steps 03 and 04 reach only the authenticated
field-7 door, canonical address-witness decoder, checkpoint walk, key hashing,
and Ed25519 verification. Step 05 reaches only exact terminal polarity and
generic finalization. No applied validator imports observer, redeemer,
native-script, CEK, mint/value, or output-reconstruction engines.

The forced direction reaches a wrongful `SpendInputSignerMissing` whose
coordinate is a signed pub-key input (witness scan), a script-locked input, or
a position past the field-0 count (direct route). It does not reach a forced
rejection whose in-range input is absent from the prior ledger: the canonical
reason there is `InputNotFound`, and this family's step 02 requires
membership, so that claim needs a non-membership proof this family does not
carry.

## Reproducible fit gate

Build with pinned `aiken v1.1.23+5adf783` for `testnet` without changing the
repository blueprint. Publish every applied script in an ordinary complete
signed reference-script transaction and execute a Lucid Evolution lifecycle
against that fresh blueprint: accepted and forced init-to-terminal journeys;
maximum-depth membership; direct and certified field carriage; all twenty
maximum-frontier scan transactions; cancellation from every nonterminal step;
terminal mint; state-queue target; and descendant-aware removal. Local UPLC
evaluation remains enabled.

The deterministic JSON fit ledger records compiler and blueprint digests plus
signed bytes, memory, CPU, and remaining margins for every measured row.
Acceptance requires signed bytes `<= 16,384`, memory `<= 16,500,000`, and CPU
`<= 10,000,000,000`; reference publication additionally targets
`<= 15,872` bytes. Every margin must be positive and the ledger test must
reproduce the artifact from the lifecycle measurements.

The ledger is written by the lifecycle suite itself
(`MIDGARD_UPDATE_FIT=1 vitest run tests/spend-input-signer-missing-lifecycle.test.ts`)
from the transactions it submitted, never transcribed by hand, and it is bound
to the SHA-256 of the `plutus.json` the suite ran against. The fit-ledger test
re-derives every margin from the stored raw measurements, checks the stored
digest against the blueprint on disk, and pins the row names of the maximum
shape, the cancel sweep, and every applied-script publication.

The current ledger is bound to the testnet blueprint `sha256 7fa79805…`
(`aiken v1.1.23+5adf783`; ledger digest `d52f7739…`). Complete signed
reference-script publication measured 14,939, 12,469, 7,528, 9,097 and 2,232
bytes for steps 01–05; step 02 grew by 353 bytes (12,116 → 12,469) for the
direct exit, its second successor parameter and the authenticated field-0
count, and keeps a 3,403-byte reserved publication margin; steps 04 and 05
grew by 22 and 25 bytes for the `signer_required` scalar. The direct terminal route was
measured on its own forced block: a forced `SpendInputSignerMissing { 0 }`
rejection over a script-locked spend input closed at step 05 straight from
step 02 in a 1,727-byte transaction (1,673,197 memory, 550,240,030 CPU) —
the same cost as the scan route's step 02, since the membership proof and
descriptor decode are shared and only the exit differs — followed by the
ordinary 916-byte proof mint and a 2,361-byte removal. The same block first
refused the scan door for that coordinate, and the honest forced block (a
pub-key input whose only witness does not verify) refused the direct exit
before walking the field to the terminal refusal.
