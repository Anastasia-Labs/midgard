# `fieldItemWidthIllegal` V1 size and transition plan

- Proposed category ID: `00000021` (central catalogue wiring is intentionally
  deferred to the primary integrator).
- Logical topology: three family-scoped computation-thread steps after generic
  `Init`.
- Authenticated subject: one accepted or forced native transaction and exactly
  one `(field_index, item_index)` coordinate.

## Physical validators and state

1. `fraud-proofs/field-item-width-illegal/step-01`: imports only the Wave-0
   verdict-subject substrate and native transaction inclusion/forced-leaf
   binders. It authenticates accepted/forced source identity and, for wrongful
   rejection, requires the exact
   `FieldItemWidthIllegal { field_index, item_index }` constructor and
   coordinates. It forwards `BoundCoordinateV1` to the exact step-02 hash.
2. `fraud-proofs/field-item-width-illegal/step-02`: imports the field-access
   door and the family semantic engine. It authenticates the selected item
   against the transaction's positional field commitment using tier 1, raw
   UTxO, or certified/chunked carriage. It derives the payload width on-chain
   and forwards `AuthenticatedWidthV1` to the exact step-03 hash.
3. `fraud-proofs/field-item-width-illegal/step-03`: imports only the Wave-0
   terminal-polarity helper and the family semantic engine. It recomputes the
   decisive predicate from the authenticated `(field_index, item_index,
item_width)` state and finalizes only an illegal accepted item or a legal
   item carrying that exact forced-rejection reason.

All three spending validators use the common computation-thread cancel path.
No scan self-loop is needed: item selection is one authenticated random access,
so there is no cursor checkpoint or resumable dynamic frontier.

## Production evidence and actuation

The installed runner accepts no stage resolver or prepared evidence callback.
It reconstructs the accepted transaction PHAS trie and membership proof, or
the exact forced leaf membership, from the retained DA block; compact and
witness-set bytes are re-derived and compared with that authenticated source.
Current step numbers and thread/state-queue out-refs come only from the
release-final raw-L1 family observation port. Lucid resolves the UTxO named by
that authenticated out-ref and the family helper rechecks its address and
computation-thread token before building.

Raw-UTxO and certified maximum carriage are package-owned prerequisite
transactions. Each publication and certificate records a locally evaluated
pre-submit intent and exact transaction hash in the same central workflow
journal, then must be visible through raw L1 before the proof step is allowed
to consume it. These auxiliary confirmations do not advance the three-step
family cursor. Descendant removal uses the package submitter with the
installation's state-queue mutation-lease coordinator; the installation may
configure the coordinator but cannot replace the removal transaction builder.

## Semantic engine and maximum evidence

The pure engine is
`midgard/fraud_proofs/field_item_width_illegal/rule.ak`. It is the narrow twin
of `validation-machine-v1.transaction_field_item_encoded_length`: V1 width
illegality is exactly an empty field-5 mint-policy item or a field-2 output item
above `max_serialized_output_preimage_bytes`. All other authenticated item
widths are legal for this typed reason; fixed-stride envelope failures are
owned by the existing committed-field-shape/canonical-decodability boundary.

Maximum dynamic evidence is one 32,768-byte committed field, at most three
15,148-byte certified chunks plus the certificate, one native transaction
membership carriage, and one selected item. Step state is constant-size and
contains no item bytes. The adjacent boundary is a field-2 item at exactly the
serialized-output limit versus one byte above it; field 5 covers width zero
versus one.

## Reachability and unrelated-adapter proof

The applied imports are intentionally directional:

- step 01 cannot reach field semantic adapters;
- step 02 reaches only the canonical native transaction field door and this
  family's width predicate;
- step 03 reaches only this family's predicate and the Wave-0 terminal helper.

None imports output decoding, mint decoding, native-script scanning, signature
verification, ledger membership, observer, redeemer, CEK, or value
accumulation adapters. Body/witness field selection is handled by the one
canonical field-door claim shape, not by importing the unrelated family rules
that later interpret those fields.

## Fit gate and lifecycle evidence

Build with pinned Aiken in the `testnet` environment, publish every applied
step as a complete signed reference-script transaction, and run the full Lucid
Evolution lifecycle with `MIDGARD_REAL_BLUEPRINT_PATH` pointing to that fresh
blueprint under shared Van Rossem parameters. The machine-readable family
ledger (`field-item-width-illegal-v1-fit-ledger.json`, written by
`field-item-width-illegal-lifecycle.test.ts` under `MIDGARD_WRITE_FIT_LEDGER=1`
and pinned by `field-item-width-illegal-fit-ledger.test.ts`) records signed
bytes, memory, CPU, and margin for all three script publications and for every
lifecycle transaction of four shapes:

- accepted field 2 at the maximum: a 32,768-byte outputs field holding one
  32,764-byte item, opened through three certified chunks (two at exactly the
  15,872-byte publication target) plus the certificate;
- forced field 2 at the maximum: a 32,767-byte outputs field whose selected
  second item is exactly 16,384 bytes, so the door walks the whole field under
  certified carriage to reach the legal item;
- accepted and forced field 5 through inline carriage, selecting the empty
  item and the 29-byte item of the same two-item mint field.

The lifecycle suite drives the registered SDK chain
(`harness.contracts.fraudProofContracts.fieldItemWidthIllegal`), pins the
family-side application against it step for step, and ends with
`assertCompleteLifecycleCoverage`. On chain it refuses: the honest accepted
block at exactly 16,384 bytes and the honest non-empty mint item; the honest
forced rejections of a 16,385-byte output and of an empty mint item; the
adjacent-over-bound item coordinate (index equal to the item count); the
forced-leaf reason coordinate, header, membership, and direction; a
substituted native transaction source, inline preimage bytes, certificate
reference, and chunk order at the field door; a wrong successor script; and
cancels from every physical step. Acceptance requires signed bytes
`<= 15,872` for publication reliability and every transaction below the hard
16,384-byte, 16,500,000-memory, and 10,000,000,000-CPU limits with local UPLC
evaluation; the ledger carries a positive margin for every entry.
