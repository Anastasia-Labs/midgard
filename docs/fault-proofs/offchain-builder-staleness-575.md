# Off-Chain Builder Staleness After #575

> Written 2026-08-08 against branch `colll78/canonical-v1-watcher-l1-source-checkpoint`;
> **remediation completed by #604 on 2026-08-14.** It was the single explanation
> behind every `⚠️ **STALE AS OF #575**` banner in
> `demo/midgard-sdk/src/fraud-proof/` and `demo/midgard-fault-proofs/src/`. The
> banners are gone; this remains the single account of *what the rebind moved and
> why*, which is what those modules' headers now point here for.

## Remediation status: **complete** (#604)

**Every module listed in §5 is re-derived and every `⚠️ STALE AS OF #575` banner
is gone.** `grep -rl "STALE AS OF #575" demo/` returns nothing. This document is
kept rather than deleted because §1, §2 and §6 are the only written account of
what the rebind moved and why, and the modules' own headers now point here for
that account rather than for a warning.

The SDK half — `field-opening-v1.ts` and the seven family modules — is described
below in the same terms it was when it was the only half done. What #604 landed
after it:

- **A builder-side door.** `demo/midgard-fault-proofs/src/field-opening-v1.ts` is
  the transaction half of the §8.8 door and the single seam all seven families
  use: it derives the §5.1 preimage from canonical item bytes, plans the §8
  carriage, publishes it when the tier calls for one, resolves positional
  reference-input indices by content (§8.7), and refuses — *before* a transaction
  is built — every condition the door aborts on. Those refusals are the door's,
  one for one: the compact bytes must re-derive to the anchored id; the preimage
  must hash to the commitment the compact structure carries **at the named field
  index**; a witness-set field must carry the transaction's own compact witness
  set, whose hash is both what the compact structure names and what the thread
  anchored. Pinned by `tests/field-opening-v1.test.ts`.
- **A new input on every step-02-class builder**, threaded through the
  programmatic API, the CLI (`--native-tx-compact <native-tx-compact.json>`) and
  the emulator harness: the disputed transaction's compact structure. It was not
  previously an input because the retired datum carried the field hash instead.
- **Two family-specific publication mechanisms deleted**, not re-pointed, because
  §8's ladder is their replacement and is one mechanism for all nine families:
  `input-no-idx`'s typed `PublishedSpendInputsV1` datum, and `double-spend`'s
  `ensureSpendInputsReferenceWitness`. Both bound a publication to one
  computation thread and one prover, which is exactly the coupling §8.7's
  content addressing forbids; §8.5 raw carriage is healable by anyone.
- **`submit-input-no-idx-fold` retired.** `input_no_idx/step_02.Args` is a flat
  record on-chain: the `FoldStart`/`FoldNext` arms that command drove no longer
  exist, so there is no redeemer it could emit. The command now refuses with that
  explanation rather than being silently removed. Twenty spend inputs are 800
  bytes of §5.1 preimage — inside §8.4's 14,336-byte tier-1 bound — so what took
  twenty transactions takes one.

### Two stale sites the typechecker could not see

Worth recording, because the typechecker was otherwise a complete worklist (59
errors across 20 modules, all of them genuine construction sites). Two call sites
were stale and compiled cleanly:
`submit-input-no-idx-step-01.ts` and `submit-reference-input-no-idx-step-01.ts`
each passed a **field commitment** to an SDK state constructor whose parameter had
been renamed to take the **§2.5 anchor**. Both parameters are bare `string`, so
the rename was invisible to `tsc` and both threads were written with 32 bytes the
door would refuse. They were caught by the emulator legs, not by the compiler.
A `MidgardTxId` nominal type would have caught them; that is not this ticket's to
add, and it is the reason each family's emulator flow was run rather than treated
as covered by a green typecheck.

## Correction to §5's exclusion list (found by #604)

`reference-input-no-idx` is listed in §5 as deliberately unmarked, on the grounds
that it "is not among the nine families #575 rebound; no validator under
`onchain/aiken/validators/fraud-proofs/reference-input-no-idx/` changed". That
was true when this document was written on 2026-08-08 and false one day later:
**#576 rebound the family onto the field door in `b824ad6ea` (2026-08-09)**, so
`lib/midgard/fraud-proofs/reference-input-no-idx/step-02.ak` and `step-04.ak`
carry `FieldOpeningV1` and `verified_tx_id` exactly as the nine do. The family's
six off-chain modules were therefore *unbannered* while the nine's were marked,
which is worse than stale and marked, so #604 audited all six on the §5 test —
does this module construct a step datum, a step spend redeemer, or a retired
field-preimage carriage datum? Five did and are re-derived alongside the nine:
`demo/midgard-sdk/src/fraud-proof/reference-input-no-idx.ts` and the four
`demo/midgard-fault-proofs/src/submit-reference-input-no-idx-step-0N.ts`.

The sixth, `demo/midgard-fault-proofs/src/prepare-reference-input-no-idx.ts`,
was **not** stale and correctly received no diff. It contains no `Data.to` and no
`Data.from`: it decodes canonical §5.3 item bytes, re-derives the compact
structure, and writes a JSON artifact for the submitters to consume. It belongs
with `double-spend-inputs.ts` and `ne-proofs.ts` in the exclusion list below —
modules that serve a rebound family without mirroring any on-chain shape — and
the rebind moved nothing it depends on.

The other §5 exclusions were re-checked against
`grep -rln FieldOpeningV1 onchain/aiken/lib/midgard/fraud-proofs` and hold:
`invalid-range` and `da-hash-preimage` never acquired the door, and the
`missing-native-script-tx` / `missing-signature` / `withdrawn-reference-input`
families still have no off-chain builder to mark.

## How the remediation landed on #604 (history)

> Recorded here as history; §4 gives the standing reason the work was split off
> from the #575 lane at all.

This document originally assigned the remediation to #579. It was re-pointed at
#604 on 2026-08-13 by owner ruling on #579. #579 regenerated the blueprint and
carried the third divergence in §2 — the new
`field_preimage_certificate_policy_id` parameter — at all twelve of its applied
sites, because that one is derivable from the blueprint alone. The first two
divergences were not: they are hand-written datum and redeemer shapes across the
thirty-three modules listed in §5, and they split out of the identity batch into
#604 rather than riding a cascade whose other work was bookkeeping.

The split was measured, not assumed. After #579's parameter fix,
`da-hash-preimage` — not one of the nine rebound families — passed its emulator
lifecycle 2/2, while `input-no-idx` — one of the nine — stayed 4/4 red on
`failed script execution Spend[0] the validator crashed / exited prematurely`.
The separation landed exactly on the rebound/not-rebound line, which is what
identified the residue as these two divergences and nothing else. #604 has since
re-derived them; the remediation status at the top of this document is the
current state.

## 1. What changed on-chain

Issue #575 is the Phase-5 first-wave Q1x family rebind, landed against spec #565.
It moved nine fault-proof families off the retired counted bounded-collection
idiom and onto the §8.8 field-access door. The nine rebound families are
`double-spend`, `input-no-idx`, `invalid-signature`, `missing-native-script-tx`,
`missing-signature`, `no-input`, `no-reference-input`,
`withdrawn-reference-input`, and `zero-input`; every step validator under
`onchain/aiken/validators/fraud-proofs/` for those families was rewritten.

Under the retired scheme each of the nine committed native-transaction fields was
committed with a per-item Merkle construction, and a family step proved a claim
about a field by reproducing that field's items in the redeemer and recomputing
the collection commitment. Under the scheme #575 installs, a field commitment is
a plain `blake2b_256` over the field's enveloped preimage bytes, a step names one
of §8's three carriage tiers rather than reproducing anything, and the door —
`authenticated_field_view` in
`onchain/aiken/lib/midgard/native-tx-field-access-v1.ak` — is the only code that
opens a field.

The off-chain builders in `demo/` mirror those datum and redeemer shapes as
hand-written Lucid `Data` schemas rather than deriving them from a blueprint.
That is why nothing broke visibly: TypeScript stays green, the packages build,
and the CBOR these modules produce no longer matches the validators that would
have to accept it. A builder that looks healthy and submits an unacceptable
transaction is precisely the failure mode the banners exist to make loud.

## 2. The three concrete divergences

**Thread state carries the transaction id, not a field hash.** A step's `State`
used to carry the collection commitment for the field the next step would open —
`..._spend_inputs_hash`, `..._addr_tx_wits_hash`, and their siblings. It now
carries the §2.5 anchor instead: the transaction id, and, for a family whose
later step reads one of the witness-set fields (6 through 8), the
`witness_set_hash` alongside it. The second component is not redundant. §3's
transaction-id preimage is the body alone, so the id does not commit the witness
set, and `NativeTxAnchorV1`'s two arms in
`onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak` — `BodyAnchor` for
fields 0 through 5 and `WitnessAnchor` for fields 6 through 8 — exist to make
that distinction structural rather than a convention a step could forget.

**The redeemer carries a `FieldOpeningV1`, not a reproduced preimage.** The
prover names one of §8's three carriage tiers — inline bytes, a raw reference
UTxO, or a certified chunk vector — and the door authenticates whichever it was
handed. The old `..._preimage: List<...>` redeemer argument is gone from every
rebound step, so an off-chain builder that still fills one is producing a
redeemer with a constructor arity the validator will not decode.

One restriction on that choice is worth carrying off-chain, because a builder
that ignores it will produce a redeemer the validator refuses at run time rather
than one TypeScript can catch. A field in the **witness set** — §2.5 fields 6, 7
and 8 — may not be carried under tier 3. A §8.6 certificate is minted against a
`witness_set_hash` taken from the minter's own redeemer, and §3's transaction id
does not commit that value, so for those three fields the certificate binds the
preimage to nothing the disputing thread anchored. `carriage_reaches_the_anchor`
in `field-opening-v1.ak` refuses it, and `docs/spec/midgard-tx.md` §8.3 erratum
E2 records the limit.

**The repair is #606's** (owner ruling, 2026-08-14, in-session, recorded on
#606). It was assigned to #579 when this was written and re-pointed at #604 on
2026-08-13; #604 then measured what it costs. Folding the field commitment into
the §8.6 asset name changes `field_preimage_certificate_asset_name`, which both
the minting policy and the door compile against, so it moves the certificate
policy id — a validator *parameter* at the twelve applied sites #579 swept — and
with it every field-opening step's applied script hash. That is a blueprint
regeneration, and #579's single regeneration was spent. One hypothesis is
recorded falsified so it is not re-tried: carrying `witness_set_hash` in the
thread anchor does **not** repair limit 3. The anchor already carries it; that
closes the tiers-1/2 forgery, because there the door hashes the preimage itself.
Under tier 3 the door never hashes it, the certificate is the binding, and the
certificate's token name is `(tx_id, field_index)` alone — there is nothing in
the token for a step to check the anchored hash against.

What #604 *did* land here is off-chain hardening: `fieldOpeningV1ForField`
refuses to emit tier-3 carriage for fields 6–8, duplicating the door's refusal at
the point a transaction is built. It constrains honest builders and does nothing
to an adversary, who does not use them, so it is explicitly not a substitute for
the repair.

**A fourth divergence, found by #604 and not in the three above.** #575
(`2fec6b0fb`) also *deleted* `invalid_signature/step_01.Args`'s
`bad_tx_witness_set_compact` field, collapsing a two-field record to a bare
`NativeTxInclusionArgs`. The witness-set preimage no longer travels through
step-01, because step-02 opens field 7 through the door and re-derives it from
whatever carriage the prover chose; what step-01 still owes the thread is the
witness-set *hash*, and that goes into `step_02.State`.

It is recorded here as its own item because following the three divergences
above does not find it — the SDK's `InvalidSignatureStep01ArgsSchema` compiled,
typechecked and round-tripped against itself while emitting the retired wrapper.
Its failure mode is the worst kind: the validator decodes a two-field record
positionally into the wrong fields rather than refusing outright, so the
observed signature was `Spend[0] unexpected empty list` — which reads like a
malformed fixture, not like a stale ABI. The lesson is the same one the two
string-typed anchor sites teach: the typechecker is a worklist, not a proof, and
each family's emulator flow is what actually closes the loop.

**The validators take a new parameter.** Every step that opens a field is now
parameterised by `field_preimage_certificate_policy_id`, because tier-3 carriage
has to check chunk digests against a certificate minted under a known policy.
A new parameter changes the applied script, so the script hash — and therefore
the spending address the submitters compute and compare against — differs from
any currently deployed one.

## 3. Why this is not a pre-existing defect

These builders were correct at the commit immediately before #575. Each of them
mirrored the on-chain shape that was live at the time it was written, and several
carry their own local re-derivation checks so that a prepared JSON artifact
cannot smuggle in commitments the chain did not authenticate. Nothing about them
was wrong then, and nothing recorded here is a latent bug being disclosed late.
They are stale in the ordinary sense: the counterpart they mirror moved, and they
have not yet been moved after it.

## 4. Why they were not re-derived in the #575 lane

Re-deriving them correctly requires the regenerated blueprint.
`onchain/aiken/plutus.json` is an untracked build output, not a committed
artifact, so a hand-written re-derivation landed in this branch could not be
checked against anything — it would be a second hand-written guess sitting beside
the first, with no mechanism to tell which one matched the compiled validators.
Blueprint regeneration is #579's scope, and the re-derivation that follows from
it is #604's. Splitting the work that way keeps this branch's on-chain rebind
reviewable on its own terms and keeps the off-chain correction honest, because
#604 can diff its output against a regenerated blueprint rather than against
prose — that blueprint now exists, having been produced by #579.

The authority for the current shapes, until that blueprint exists, is the Aiken
source under `onchain/aiken/lib/midgard/fraud-proofs/` — in particular
`field-opening-v1.ak`, which defines `FieldOpeningV1`, `NativeTxAnchorV1`, and
the `opened_field_view` entry point every rebound step now calls, and
`onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`, which defines the door
those types are consumed by.

## 5. Affected off-chain modules

Every module below constructs a step datum, a step spend redeemer, or a retired
field-preimage carriage datum for one of the nine rebound families. All of them
carried the short pointer banner; **all of them are now re-derived and none
carries it.** The tables are kept as the record of what the remediation had to
cover.

In `demo/midgard-sdk/src/fraud-proof/`:

| Module                    | Rebound family it mirrors |
| ------------------------- | ------------------------- |
| `double-spend.ts`         | `double-spend`            |
| `input-no-idx.ts`         | `input-no-idx`            |
| `invalid-signature.ts`    | `invalid-signature`       |
| `no-reference-input.ts`   | `no-reference-input`      |
| `non-existent-input.ts`   | `no-input`                |
| `zero-input.ts`           | `zero-input`              |

In `demo/midgard-fault-proofs/src/`:

| Module                                   | Rebound family it serves |
| ---------------------------------------- | ------------------------ |
| `prepare-double-spend.ts`                | `double-spend`           |
| `prepare-input-no-idx.ts`                | `input-no-idx`           |
| `prepare-invalid-signature.ts`           | `invalid-signature`      |
| `prepare-no-reference-input.ts`          | `no-reference-input`     |
| `prepare-non-existent-input.ts`          | `no-input`               |
| `prepare-zero-input.ts`                  | `zero-input`             |
| `spend-input-witness.ts`                 | `double-spend`           |
| `submit-step-01.ts` … `submit-step-04.ts`| `double-spend`           |
| `ne-submit-step-01.ts` … `ne-submit-step-04.ts` | `no-input`        |
| `submit-input-no-idx-step-01.ts` … `-04.ts` | `input-no-idx`        |
| `submit-invalid-signature-step-01.ts`, `-02.ts` | `invalid-signature` |
| `submit-no-reference-input-step-01.ts` … `-04.ts` | `no-reference-input` |
| `submit-zero-input-step-01.ts`, `-02.ts` | `zero-input`             |

The banner deliberately did not track the RF-043 retirement boundary in
`legacy-submission-boundary-v1.ts`. Some of these routes — the `submit-step-0N`,
`ne-submit-step-0N`, and `submit-zero-input-step-0N` commands — already fail
closed before they touch a blueprint or a provider, so their staleness cannot
reach a chain through the shipped CLI. The rest are live. Both kinds carry the
same banner, because the builders are importable from the package barrel and a
reader arriving at one of these modules needs to know its shapes are stale
whether or not the CLI route in front of it happened to be retired at the time.

Three families rebound on-chain by #575 — `missing-native-script-tx`,
`missing-signature`, and `withdrawn-reference-input` — have no off-chain builder
at all, so there is nothing in `demo/` to mark for them.

Modules deliberately left unmarked, and why:

- `demo/midgard-sdk/src/fraud-proof/reference-input-no-idx.ts` and
  `demo/midgard-fault-proofs/src/prepare-reference-input-no-idx.ts` with its four
  `submit-reference-input-no-idx-step-0N.ts` submitters. `reference-input-no-idx`
  is not among the nine families #575 rebound; no validator under
  `onchain/aiken/validators/fraud-proofs/reference-input-no-idx/` changed.
  **Superseded — see "Correction to §5's exclusion list" above.** #576 rebound
  the family a day after this was written, so five of these six were stale after
  all and #604 re-derived them; only `prepare-reference-input-no-idx.ts` stayed
  correctly excluded, and for a different reason than the one given here.
- `demo/midgard-sdk/src/fraud-proof/invalid-range.ts`, `da-hash-preimage.ts`, and
  their submitters and preparers, for the same reason.
- `demo/midgard-fault-proofs/src/submit-init.ts`. It builds the thread's first
  step datum with `data: null` for every category, which #575 did not change, and
  it resolves each category's script hash and address from the deployed manifest
  at run time rather than embedding one, so the third divergence reaches it as
  data rather than as stale code.
- `demo/midgard-sdk/src/fraud-proof/computation-threads.ts`. The generic
  `FraudProofComputationThreadStepDatum` envelope — `fraud_prover` plus an opaque
  `data` payload — is unchanged; what moved is the per-family `data`.
- `demo/midgard-sdk/src/fraud-proof/field-preimage-carriage-v1.ts`. This is the
  new §8 carriage publisher, written for the scheme #575 installs rather than the
  one it retired.
- `demo/midgard-fault-proofs/src/double-spend-inputs.ts` and `ne-proofs.ts`.
  The first parses and bounds-checks a CLI index; the second builds
  merkle-patricia-forestry tries and membership proofs for the ledger state.
  Neither constructs a datum or a redeemer.

## 6. Related correction: `verify_native_tx_witness_set`

#575 also deleted the on-chain helper `verify_native_tx_witness_set`, which four
off-chain modules narrated as an existing function. The check it performed did
not disappear; it moved inside the field-access door. `authenticated_field_view`
refuses to read fields 6 through 8 unless
`blake2b_256(encode_native_tx_witness_set_compact(witness_set))` equals the
`witness_set_hash` the supplied compact transaction carries, and — because §3's
id preimage is the body alone and so does not cover that trailing hash — a
downstream family step must additionally show that the hash is the anchored one,
which `opened_field_view` does by matching it against `WitnessAnchor` in thread
state. The comments in `prepare-invalid-signature.ts`,
`submit-invalid-signature-step-01.ts`, `invalid-signature.ts`, and
`tests/fault-proof.test.ts` were corrected to describe that arrangement.
