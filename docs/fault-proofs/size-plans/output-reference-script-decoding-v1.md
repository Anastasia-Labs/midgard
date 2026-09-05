# `outputReferenceScriptDecoding` V1 size and transition plan

- Category: `outputReferenceScriptDecoding`
- Frozen category ID: `0000002a`
- Typed reasons: `OutputReferenceScriptMalformed`,
  `OutputReferenceScriptNodeLimit`, and `OutputReferenceScriptDepthLimit`
- Authenticated subject: one field-2 transaction output and exactly the
  reference-script item committed by its canonical output descriptor.

## Physical chain

| Step | Applied validator                                                  | Imported semantic engine                                                            | Carried state                                                                                     |
| ---- | ------------------------------------------------------------------ | ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| 01   | `fraud_proofs/output_reference_script_decoding/step_01.main.spend` | common accepted/forced native-transaction binding                                   | exact verdict subject, transaction id, witness-set hash, output coordinate, accused result class  |
| 02   | `.../step_02.main.spend`                                           | authenticated field-2 opening and canonical ledger-output descriptor reconstruction | exact output commitment plus reference-script language, hash, length, and bounded-item commitment |
| 03   | `.../step_03.main.spend`                                           | resumable canonical ledger-output descriptor reconstruction                         | exact output commitment and authenticated reference-script item offset                            |
| 04   | `.../step_04.main.spend`                                           | bounded-item authentication for the descriptor's exact reference-script item        | canonical native-script structural control and domain-separated checkpoint                        |
| 05   | `.../step_05.main.spend`                                           | frozen `native-script-decoding/engine` structural scan                              | resumable control/checkpoint and terminal malformed/node/depth/no-fault class                     |
| 06   | `.../step_06.main.spend`                                           | exact reason/result polarity and common proof finalization                          | terminal verdict bound to the original source and output coordinate                               |

The five logical transitions use six physical validators because the first
complete signed measurement of a combined output-scan/reference-bind step was
20,159 bytes, exceeding the 16,384-byte ordinary transaction limit. Every
physical step has the common cancel arm and one exact successor; steps 03 and
05 may self-loop. Step 06 burns the computation-thread token and mints the permanent
fraud-proof token.

## Maximum dynamic evidence

- One canonical accepted transaction or exact forced leaf retained through
  authenticated public DA.
- The maximum legal field-2 preimage and selected output item, carried through
  Direct, Raw UTxO, or Certified publication as required.
- One canonical output descriptor whose reference-script language, hash,
  total length, and bounded-item commitment are reproduced from the exact
  selected output bytes.
- One selected versioned reference-script item spanning the bounded-item chunk
  frontier, with current and adjacent authenticated chunks supplied to each
  structural scan transaction.
- A canonical structural control at the protocol node/depth bound of 16,384.
  The exact bound is admitted and 16,385 refused at the rule level
  (`exact_node_boundary_is_admitted`, `adjacent_node_over_bound_is_refused`,
  `exact_and_adjacent_depth_boundary`). Neither limit is reachable through a
  committed output: `ledger_output_v1.max_output_canonical_cbor_bytes` is
  16,384 and a canonical node costs at least three bytes, so 16,385 nodes or
  nesting levels need at least 49,155 bytes. The wrongful-acceptance direction
  of the NodeLimit and DepthLimit arms therefore has no realisable subject; the
  wrongful-rejection direction of both arms is exercised end to end.
- The consensus output bound itself: a 16,384-byte output is the maximum
  accepted shape; a 16,385-byte committed output is refused off chain by the
  evidence preparer and on chain at step 02 (`initial_output_scan_v1`).
- The widest decodable native script that fits the bound: `all` of 510
  signature nodes (16,372-byte output, four bounded-item chunks), scanned in
  64 sixteen-step transactions with the adjacent chunk window supplied at
  every chunk crossing.

## Reachability and isolation

Step 01 cannot interpret output bytes. Step 02 reaches only field 2 and
initializes the canonical ledger-output descriptor engine. Step 03 advances
only that output descriptor. Step 04 reaches only the descriptor-bound
reference-script item and versioned-script header. Step 05 reaches only
bounded chunks and the frozen structural scan engine shared by the three Wave
4 families. Step 06 reaches only exact result/reason polarity and generic
finalization. No witness-script ordinal, resolved-input adapter, execution
source selector, signature frontier, prior-ledger trie, observer, redeemer,
mint/value fold, or CEK engine enters an applied validator.

## Fit and lifecycle gate

Build the testnet blueprint with the pinned Aiken compiler, publish all six
fully applied validators in complete signed reference-script transactions,
and execute real Lucid Evolution journeys with the shared Van Rossem limits:
16,384 transaction bytes, 16,500,000 memory units, and 10,000,000,000 CPU
units. The measured suite must cover malformed decoding, exact node/depth
boundaries, adjacent-over-bound refusal, decodable wrongful rejection,
authenticated resume, subject/descriptor/item substitution, cancellation,
terminal burn/mint, and leased fraudulent-block removal. The machine-readable
fit ledger must retain positive byte, memory, and CPU margins without an
oversized route, raised parameter, or disabled local evaluation.

The reproducible signed-publication ledger is
`output-reference-script-decoding-v1-fit-ledger.json`, written by the
lifecycle suite (`MIDGARD_WRITE_FIT_LEDGER=1`) from complete signed emulator
measurements and pinned by `output-reference-script-decoding-fit-ledger.test.ts`
against the fresh blueprint. The measured publication sizes for steps 01–06
are respectively 14,800, 7,417, 11,523, 12,924, 11,686, and 2,930 bytes,
leaving 1,584 bytes of headroom at the tightest step. The same ledger records
the 16,384-byte accepted lifecycle (nine descriptor windows, the malformed
verdict at token 0), the empty-payload bind close, the widest all-of forced
lifecycle (every chunk-crossing scan window and the exact-end close), the
nested-container and signature-script forced lifecycles, all six
cancellations, permanent mint, and leased removal. The tightest lifecycle
rows are the 16-step scan transactions at 9,707 signed bytes and 6,894,649
memory units (margins 6,677 bytes / 9,605,351 units / 7,297,492,423 CPU) and
the certified field carriage chunk at exactly the 15,872-byte publication
target.

Two on-chain defects were closed while completing this gate:

- `rule.bind_reference_script_v1` parsed the versioned header itself and
  aborted on a tag-0 empty payload (`initial_structure_control_v1` requires a
  non-empty region), although canonical validation classes that output
  `InvalidReferenceScript`; the coordinate was unprovable in both directions.
  The bind now routes through the frozen engine's `bind_machine_v1`.
- Step 05 authenticated the chunk window only when the resumed control was at
  the token stage, so any planned scan segment that opens on a frame step and
  continues into token steps was unsubmittable (every container with eight or
  more children stalled after its first segment). The window is now
  authenticated whenever it is supplied, as in the reference family's
  `step_03_advance_or_close`.
