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
  The exact bound is supported; 16,385 is refused before submission.

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
`output-reference-script-decoding-v1-fit-ledger.json`. The post-split measured
transaction sizes for steps 01–06 are respectively 14,800, 7,417, 11,523,
12,836, 11,710, and 2,930 bytes, leaving 1,584 bytes of headroom at the tightest
step. The same ledger records every maximum accepted lifecycle transaction,
all five nonterminal cancellations, permanent mint, and leased removal. Its
canonical digest is
`d1479fb3a8907376755007aa7cfc473a9ab53fffb5dc040cb2ce2efd04efbb7d`.
