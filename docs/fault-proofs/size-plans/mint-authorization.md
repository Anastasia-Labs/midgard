# Mint authorization lifecycle and installed workflow

Status: implementation and measurement in progress; this document is not fit evidence.

The existing accepted-transaction predicate remains unchanged. Direction A
proves a minted or burned policy absent from all inline script witnesses and
all resolved reference-input scripts. Direction B proves a hash-pinned native
policy false against the committed signer frontier and validity interval.
Malformed native scripts remain a decoding-family claim. Plutus execution is
outside this single-party family. Wrongful forced rejection is covered by the
existing missing/invalid native-script families, not a new mint polarity here.

## Transitions

Source binding → mint-policy selection → direction dispatch remains unchanged.
The installed absence path freezes the authenticated field-6 preimage in a
seventh physical validator. It checks at most 16 canonical versioned witness
items per transaction, then enters the existing ordered reference-input scan.
The installed unsatisfied-policy path freezes the exact policy payload and
signer field in a sixth physical validator. It first collects at most 16
canonical signer witnesses per transaction, then evaluates at most 16 native
structure tokens or authenticated parent frames per transaction. Native
evaluation cannot begin until the exact authenticated signer count is reached. The terminal
transition requires exact byte exhaustion, an empty stack and a false result.
Both new stages self-loop and retain the existing cancellation mechanism.
The original direct dispatch arms remain available with identical predicates.

Step02 retains its original inline encoding and adds a structured reference
arm for the static header, event membership, transition membership and claim
coordinate. The existing counted-root and exact accepted-event checks consume
the reconstructed typed Data. The installed runner automatically publishes
this evidence when it exceeds 6,000 bytes. No asserted root or journal record
replaces either membership check.

The absence preimage uses at most three ordered reference chunks and remains
bounded by 32,768 bytes. Native evaluation carries the policy bytes followed by
the exact signer-field preimage: each component remains bounded by 32,768 bytes,
with at most five ordered chunks and 65,536 aggregate bytes. Dispatch freezes
the exact component boundary, signer count, total length and chunk commitments.
Automatic field grammar certification supports the maximum mint, witness,
signer and reference fields. Whole-field mint selection preserves the compact
transaction/anchor authentication and certified field grammar.

## Retained evidence and installed runner

Reconstruct the source inclusion, exact event-to-step coordinate, and prior
ledger from authenticated current/predecessor retained DA. Include earlier
accepted transaction effects when resolving references. Derive descriptor MPF
proofs and native reference-script bytes from retained output preimages.
Store canonical envelopes and the selected source/policy coordinate in the
journal; rederive every field, proof, finding and direction on admission.
Install complete replay, typed detection routing, manifest-bound transaction
ports, automatic field publication/certification, watcher configuration and
runtime exports. Use the existing authenticated prerequisite and removal ports.

## Required measured shapes

- Absent policy with positive mint and negative burn quantities, empty sources,
  matching inline source refusal, and matching last reference source refusal.
- Unsatisfied native policy with mint/burn, inline/reference retained payloads,
  signature and timelock predicates, satisfied-boundary refusals, changed payload
  and changed signer frontier refusals.
- Maximum supported aggregate field budget (32,768 bytes), named field 5 policy
  tail, field 6 witness tail, field 7 signer frontier, and field 1 reference tail.
- Maximum authenticated transaction/source and event/trace proof shapes; maximum
  native evaluator node/depth/byte limits and adjacent invalid shapes.
- Publication of every script and carriage/certificate, both completed claim
  directions, permanent proof mint and fraudulent-block removal, cancellation
  and resumed journal execution.

Record every signed transaction through the shared Van Rossem ledger writer.
Regeneration uses `MIDGARD_WRITE_FIT_LEDGER=1`; the verifier binds the current
`MIDGARD_REAL_BLUEPRINT_PATH` digest and all recorded margins. No protocol-limit
or local-evaluation bypass is allowed for successful lifecycle evidence.

## Maximum proof and field evidence

The proof-depth fixture uses four independently verified 64-branch MPF
frontiers (source, event, transition and prior ledger), together with the
32,768-byte mint-field tail. These sibling frontiers are synthetic physical
fit vectors; retained-DA workflow cases separately reconstruct their complete
actual ledgers and sources. The maximum native-byte field is 32,768 bytes,
containing a 32,759-byte policy after its canonical witness envelope. Both a
10,919-node wide tree and a depth-10,918 tree exercise the reachable byte
frontier. A further combined case fills that native field with signature
predicates while also opening the 318-signer frontier.

## Verification execution

Bounded installed direction, burn, signer and reference cases reopen the fsynced
directory journal at every action boundary. The native, witness and reference
maxima use the same cursor protocol with the validated memory journal to avoid
quadratic filesystem history reads during thousands of signed transactions.
Each action still creates a fresh adapter and re-admits the retained artifact;
publication observations come from actual emulator UTxOs. This separation does
not establish a filesystem throughput claim for the maximum workflow.

Reference membership preparation constructs one trie for all requested exact
openings and checks every key, value and resulting root. It does not infer
membership from a descriptor supplied by the workflow journal.

The generator uses `MIDGARD_WRITE_FIT_LEDGER=1`. A filtered rerun replaces its
entire named scenario; other measured scenarios are retained only when the
blueprint digest matches exactly. The independent ledger verifier requires
every named completed path and publication group, so a partial regeneration
does not satisfy the gate. Maximum memory-journal loops yield once per action
to let Vitest process its worker IPC; no chain validation or journal event is
skipped. All authenticated reference scripts, including removal scripts, are
published during deployment before the native authorization policy expires.
Long proof execution does not extend that four-hour minting window.

Run the lifecycle generator from `demo/midgard-fault-proofs` using the declared
Node 22 / pnpm 9 toolchain and a fresh pinned testnet blueprint:

```sh
NODE_OPTIONS=--max-old-space-size=8192 MIDGARD_WRITE_FIT_LEDGER=1 \
  pnpm exec vitest run tests/mint-authorization-installed-lifecycle.test.ts --reporter verbose
pnpm exec vitest run tests/mint-authorization-workflow-fit-ledger.test.ts --reporter verbose
```

The long native/reference cases deliberately execute every bounded continuation;
the per-case timeout is 30 minutes. No result from an interrupted run counts as
a completed scenario. The writer may preserve other complete scenario rows only
when their blueprint digest is identical, and the verifier still requires the
whole named roster.

Regeneration also writes a diagnostic-only raw-measurement checkpoint before
the fit guards execute. `MIDGARD_FIT_MEASUREMENT_CHECKPOINT` overrides its
temporary-file path. These captured values are not an accepted ledger; changing
the blueprint requires fresh execution measurements. Automatic `publish_*`
actions are classified as publications and must satisfy the publication reserve.

The bounded signer repair passed four targeted installed lifecycles (absence,
unsatisfied policy, 318 signers, and combined 318 signers/native maximum),
recording 305 transactions under the shared writer: 201 lifecycle and 104
publication rows. Maximum Step03 signer binding used 895,228 memory units;
the combined evaluator maximum used 11,798,200 memory units and 3,935,085,279
CPU units. Its tightest publication was 15,872 bytes, preserving the full
512-byte reserve. These targeted rows do not replace the complete roster:
a fresh full shared-blueprint regeneration remains required.
