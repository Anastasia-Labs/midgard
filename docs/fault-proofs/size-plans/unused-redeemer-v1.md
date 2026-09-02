# `unusedRedeemer` V1 maximum-shape and transition plan

- Frozen category ID: `00000030`.
- Typed rejection reason: `UnusedRedeemer { redeemer_index }`.
- Subject: one exact item in the authenticated field-8 redeemer collection,
  checked against the complete canonical purpose and execution-selection
  frontiers committed by `ScriptSources`.
- Logical topology: six family-owned computation-thread steps after generic
  `Init`, implemented by nine physical validators because trace/control and
  bounded item authentication must remain separately publishable. No
  missing-redeemer, script-source, native-evaluation, or integrity predicate
  is reachable from these applied scripts.

## Applied validators and reverse parameter order

1. `fraud-proofs/unused-redeemer/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)` binds the accepted
   or forced transaction, exact field-8 coordinate, direction, and exact
   `UnusedRedeemer` reason in the forced direction.
2. `fraud-proofs/unused-redeemer/step-02`
   `(step_02a_hash, computation_thread_policy)` authenticates the exact event
   and validation-trace descriptor.
3. `fraud-proofs/unused-redeemer/step-02a`
   `(step_02b_hash, computation_thread_policy)` authenticates machine state,
   trace proof, transaction/context commitments, and the exact
   producer-committed `ScriptSources` work root and requires the authenticated
   trace-proof state index to equal the global validation-machine program
   counter.
4. `fraud-proofs/unused-redeemer/step-02b`
   `(step_02c_hash, computation_thread_policy)` authenticates the bounded item
   header and fixes the canonical purpose tag, pointer, and data span.
5. `fraud-proofs/unused-redeemer/step-02c`
   `(step_03_hash, computation_thread_policy)` authenticates the bounded item
   tail, completes the exact descriptor, and applies the direction-specific
   used-bitmap polarity at the complete stage-12 audit frontier.
6. `fraud-proofs/unused-redeemer/step-03`
   `(step_04_hash, computation_thread_policy)` freezes the complete canonical
   purpose frontier and initializes a domain-separated reverse-scan
   checkpoint.
7. `fraud-proofs/unused-redeemer/step-04`
   `(step_05_hash, computation_thread_policy)` authenticates the corresponding
   execution-selection leaf for every purpose before it may affect the result.
8. `fraud-proofs/unused-redeemer/step-05`
   `(step_06_hash, computation_thread_policy)` resumably reverse-matches the
   selected redeemer pointer against authenticated purpose/execution pairs.
   It self-loops in fixed batches until a match is found or the exact frontier
   is exhausted.
9. `fraud-proofs/unused-redeemer/step-06`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`
   checks accepted/forced polarity, burns the computation-thread token, and
   permanently mints the proof token.

Every nonterminal validator retains the common cancellation arm. The state
has one canonical wire form and successor. It carries transaction identity,
frontier commitments/counts, selected pointer/leaf, cursor, monotone `used`
bit, and checkpoint; it never carries a caller verdict.

## Imported semantic engines and maximum evidence

Step 01 imports only the proof-thread subject/reason binder. Step 02 imports
the frozen validation-trace verifier, validation Merkle verifier,
`redeemer_item_proof_v1`, and canonical field-8 descriptor rules. Steps 03-05
import only `script_proof_v1` purpose/execution leaf encoders, the validation
Merkle verifier, and the family-local bounded reverse fold. Step 06 imports
only the terminal contradiction and generic permanent-proof finalizer.

The purpose-kind/redeemer-tag map is consensus fixed: spend `0 -> 0`, mint
`1 -> 1`, observe `2 -> 3`, receive `3 -> 6`. Each purpose opening is paired
with an execution leaf containing the exact purpose leaf, authenticated source
leaf, language, and selected redeemer leaf. Native selections have an empty
redeemer leaf and cannot mark a redeemer used. Plutus selections may mark only
their exact canonical pointer. All four purpose kinds, reverse-position
matches, duplicate-looking payloads at different coordinates, and both
accepted/forced directions are tested.

Maximum dynamic evidence is the complete 32,768-byte certified field-8
frontier plus the largest purpose/execution frontier admitted by the native
transaction aggregate bounds. Per-transaction scan work is capped; absence
can finalize only at the authenticated total count. The checkpoint binds the
transaction, redeemer coordinate/leaf, cursor, total, both frontier roots,
accumulator, used bit, and next expected script. Reordered, skipped,
substituted, regressed, or premature evidence fails.

Production reconstruction requires retained public `ScriptSources` witnesses
for the exact stage-10 selection and stage-12 audit seams, including
`RedeemerScanBeginWitness` and family-form `RedeemerItemStepWitness`; no node
database-only authority is admitted. A fabricated producer frontier changes
the committed work root and is handled as trace invalidity, not trusted as a
direct family verdict.

For the concrete one-Spend/two-redeemer lifecycle trace, the authenticated
global coordinates are:

| Purpose/direction | Selection begin/header/tail | Audit begin/header/tail | Bound control |
| --- | --- | --- | --- |
| Spend `0`, wrongful forced rejection | `78 / 79 / 80` (stage 10) | `84 / 85 / 86` | stage 12 at PC `85` |
| Mint `0`, wrongful acceptance | no matching selection | `87 / 88 / 89` | stage 12 at PC `88` |

The stage-10 header precedes insertion of its execution leaf and therefore has
`execution_count = 0` while `purpose_count = 1`; it is not a complete reverse
scan frontier. The validator instead binds the exact stage-12 header state,
where the complete execution frontier is present, and proves respectively
that the target bitmap bit is set or clear. The on-chain trace seam also
requires `machine_state.program_counter == trace_proof.state_index` and the
counter to be strictly inside the authenticated descriptor step count, so a
different global PC cannot be substituted behind the same control bytes.

## Publication and lifecycle fit gate

Build the fresh testnet blueprint with the pinned compiler under the shared
Aiken lock. Apply all nine physical validators backwards and publish every complete
reference script in an ordinary signed transaction using the shared Van
Rossem parameters and local UPLC evaluation. The machine-readable ledger must
measure reference publication, `Init`, each step (including maximum resume),
cancel from every nonterminal state, permanent proof mint, state-queue target,
and descendant removal.

Signed bytes must be `<= 16,384`, memory `<= 16,500,000`, CPU
`<= 10,000,000,000`; every reference publication targets `<= 15,872` bytes.
Every margin must be positive. No positive path may use `oversized: true`, a
raised protocol limit, or disabled local evaluation.

Fresh pinned `aiken v1.1.23+5adf783` testnet blueprint SHA-256 at the family
fit gate: `5a131c16641da7b254a01f0fa739114172c3583e60ff0f1231677bad5dd35de3`.
Fully applied signed reference-script publication sizes in physical order are
`14,965`, `7,644`, `11,750`, `14,935`, `5,473`, `1,945`, `1,768`, `4,086`,
and `2,211` bytes. Their respective 15,872-byte reserve margins are `907`,
`8,228`, `4,122`, `937`, `10,399`, `13,927`, `14,104`, `11,786`, and
`13,661` bytes.
