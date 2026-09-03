/**
 * Counting the machine's field-trace and chunk steps for transaction field carriage.
 */

import {
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedCollectionV1,
  buildMidgardBoundedItemChunkProofV1,
  deriveMidgardV1TxFieldPreimages,
  type MidgardBoundedCollectionItemProofV1,
  type MidgardBoundedCollectionV1,
  type MidgardBoundedItemChunkProofV1,
} from "@al-ft/midgard-core";
import {
  decodeMidgardFieldPreimageV1,
  encodeMidgardDefiniteBytesV1,
  midgardFieldHeaderLengthForCountV1,
} from "@al-ft/midgard-core/codec";

/**
 * The validation machine's own per-item trace material for one of the nine
 * fields: the §5.1 item split, folded into a counted bounded collection so the
 * machine can emit `transactionFieldChunk` witnesses with a per-item opening.
 *
 * **This is machine-internal trace structure, not a field commitment.** Under
 * `docs/spec/midgard-tx.md` §4 a field commits to a flat `blake2b_256` of its
 * preimage bytes, and nothing here is ever compared against one — the collection
 * root this builds is not `spend_inputs_hash` or any of its eight siblings, and
 * no caller treats it as such. What it feeds is the machine's proof-step trace,
 * whose on-chain twin is `lib/midgard/validation-machine-v1.ak`.
 *
 * **It survived the rebind, and the correction is worth stating (#592 → #597).**
 * The docstrings here used to promise that this trace "retires with the
 * openings". That is true only of the per-item *collection* opening, which is
 * gone: `bounded_collection_v1.verify_item` no longer runs in the machine and
 * `verifyMachineFieldItemV1` retired with it. What did not go is the walk — the
 * machine still steps item-major then chunk-major, four phases still hand a
 * partially-read item forward under a genuine `bounded_item_v1` root, and five
 * suites use these functions for step counting and size measurement. What changed
 * is the *provenance* of that root: it is derived from bytes the §8 door
 * authenticated against the flat §4 field commitment, rather than asserted by a
 * prover's `ItemProofV1`. So the structure stays and the claim it used to make
 * about the committed field is gone.
 *
 * What *did* change with the reversion is the input: the items come from §5.1's
 * one uniform enveloped byte-list decode, replacing the retired counted-era
 * three-way split (byte lists / raw item concatenation / the field-5 raw map).
 * Field-5 policy items are byte-identical either way — §5.6's
 * `82 ‖ 58 1C policy_id ‖ map(k) ‖ assets` is the same `[bytes, map]` pair the
 * counted map-entry split produced — so only the field-level envelope moved.
 *
 * Exported so the machine's own test helpers build this trace the same way the
 * machine does. It replaced `deriveMidgardNativeFieldCollectionV1`, which used to
 * live in `@al-ft/midgard-core` — the package every consumer depends on.
 *
 * **What moving it did and did not buy.** `src/index.ts` re-exports this module
 * wholesale, so the name is public API of `@al-ft/midgard-validation` and is
 * already imported across the package boundary:
 * `demo/midgard-fault-proofs/tests/cardano-capability-retained-da-v1.test.ts`
 * takes `countedMachineTransactionChunkStepsV1` that way. So this is not
 * containment; what it buys is that the counted spelling now lives in the package
 * whose on-chain twin still asks for it, one import away from this note, instead
 * of in the dependency every producer already pulls in. The discipline it asks
 * for is by name: `counted…` marks a machine-trace structure, and nothing called
 * `counted…` may be compared against a §4 field commitment. Reach for
 * `midgardFieldCommitmentV1` / `verifyMidgardV1TxFieldPreimage` for that.
 */
export const countedMachineFieldTraceV1 = (
  fieldIndex: number,
  preimageCbor: Uint8Array,
): MidgardBoundedCollectionV1 =>
  buildMidgardBoundedCollectionV1({
    fieldIndex,
    items: decodeMidgardFieldPreimageV1(preimageCbor),
  });

/**
 * One `transactionFieldChunk` step of the machine's walk over a field: the
 * per-item collection opening, the chunk opening inside that item, and the §5.1
 * byte count the field has completed through this step.
 *
 * `fieldEncodedSize` is where the retired counted grammar showed most plainly.
 * It used to need a per-field rule — a CBOR header plus the item for the byte-list
 * fields, the item minus one byte for field 5's map pair, the raw item for the
 * concatenated fields 6 and 8. §5.1 gives all nine fields one envelope, so it is
 * now the header width plus the wrapper-and-payload width of each completed item,
 * with no field in it at all.
 */
export type MachineFieldChunkStepV1 = {
  readonly fieldIndex: number;
  readonly collectionProof: MidgardBoundedCollectionItemProofV1;
  readonly chunkProof: MidgardBoundedItemChunkProofV1;
  readonly fieldEncodedSize: number;
};

/**
 * §5.1's `definite_bytes_header(L) ‖ payload` width, measured with the encoder
 * that defines it rather than re-spelled here.
 *
 * The re-spelling this replaces stopped at three header bytes, so it silently
 * under-counted by two for any item wide enough to need `5a` — and an
 * under-counted `fieldEncodedSize` is exactly what the terminating check below
 * exists to catch, which means the duplicate could only ever have turned a real
 * encoding into a spurious failure or, worse, agreed by accident.
 */
const midgardWrappedItemBytesV1 = (item: Uint8Array): number =>
  encodeMidgardDefiniteBytesV1(item).length;

/**
 * The machine's chunk steps for one field, in the order it emits them:
 * item-major, chunk-major.
 *
 * This is the replacement for `midgard-core`'s retired
 * `deriveMidgardV1TxFieldChunks`, and the difference is what AC2 of #585 is about.
 * That function *published* per-item openings against a field's committed hash,
 * which §4 leaves nothing to check against. This one produces the machine's own
 * trace steps and makes no claim about the field commitment — a caller that wants
 * the field authenticated calls `verifyMidgardV1TxFieldPreimage`, once, over the
 * whole preimage.
 */
export const countedMachineFieldChunkStepsV1 = (
  fieldIndex: number,
  preimageCbor: Uint8Array,
): readonly MachineFieldChunkStepV1[] => {
  const collection = countedMachineFieldTraceV1(fieldIndex, preimageCbor);
  const steps: MachineFieldChunkStepV1[] = [];
  let fieldEncodedSize = midgardFieldHeaderLengthForCountV1(
    collection.items.length,
  );
  for (const [itemIndex, item] of collection.items.entries()) {
    const collectionProof = buildMidgardBoundedCollectionItemProofV1(
      collection,
      itemIndex,
    );
    for (const [chunkIndex] of item.chunkHashes.entries()) {
      if (chunkIndex + 1 === item.chunkHashes.length) {
        fieldEncodedSize += midgardWrappedItemBytesV1(item.bytes);
      }
      steps.push({
        fieldIndex,
        collectionProof,
        chunkProof: buildMidgardBoundedItemChunkProofV1(item, chunkIndex),
        fieldEncodedSize,
      });
    }
  }
  if (fieldEncodedSize !== preimageCbor.length) {
    throw new Error(
      `V1 field ${fieldIndex.toString()} trace does not terminate at the committed field length: ${fieldEncodedSize.toString()} != ${preimageCbor.length.toString()}`,
    );
  }
  return steps;
};

/*
 * `verifyMachineFieldItemV1` used to live here and is retired (#597, the
 * TypeScript twin of #592's wire change). It checked one `transactionFieldItem`
 * opening against the machine's own trace for that field — and the machine
 * verifies no openings any more. Under §8 the door authenticates the whole §5.1
 * preimage once against the flat §4 commitment and an item is a slice of it, so
 * there is no per-item opening for a caller to hand over and nothing left for
 * this function to check. Its seven mutation rows retire with it: count,
 * ordering, swap, substitution, trailing-byte and field-substitution mutations
 * are all mutations of a preimage the door refuses by aborting (§7.3), which is
 * fail-closed but not a `False` an off-chain predicate can observe.
 *
 * A caller that wants a field authenticated calls
 * `verifyMidgardV1TxFieldPreimage`, once, over the whole preimage.
 */

/**
 * What a field-reading step says about the field it read, before anything knows
 * how the bytes will travel (#600).
 *
 * A step names one committed field and needs its §5.1 preimage to reach the
 * consuming transaction; §8's three tiers are three answers to *how*, and the
 * answer is not a property of the field. Tiers 2–3 name their bytes by
 * **positional reference-input index**, and §8.7 makes those indices
 * content-resolved against a concrete transaction's canonically-sorted
 * reference-input set — so the tier cannot be decided here, where no transaction
 * exists.
 *
 * The tier is therefore decided at the one place that has a transaction:
 * `buildValidationOneStepArgumentV1`. The committed `evidence_hash` is
 * transition-only (#619) — no carriage is ever hashed into it — so resolving
 * there is not a late substitution: the observe-stage field door verifies
 * whatever carriage arrives by content, and the tier is honestly named at the
 * first moment one can be.
 *
 * **This is why the producer never refuses.** It is not only the dispute path
 * that builds traces: the operator's block-build routine runs this exact
 * producer once per transaction in a block
 * (`demo/midgard-node/src/mpf/validation-trace.ts`, wired from `process.ts`),
 * where there is no dispute transaction, no published carriage and no
 * reference-input set — and never will be. A producer that refused a preimage
 * above §8.3's tier-1 cap would fail the whole block build for a legal ~14.3 KB
 * output, which is strictly worse than the dispute-side gap refusing was meant to
 * name. Carrying the plan input keeps the producer a pure function of the L2
 * transaction, exactly as its callers require, while no carriage §8.4 does not
 * admit ever exists at any instant.
 */
export type ValidationMachineFieldCarriagePlanInputV1 = {
  readonly fieldIndex: number;
  readonly fieldPreimage: Buffer;
};

/** Every field's chunk steps, field-major — the whole-transaction walk order. */
export const countedMachineTransactionChunkStepsV1 = (
  canonicalTransactionCbor: Uint8Array,
): readonly MachineFieldChunkStepV1[] =>
  deriveMidgardV1TxFieldPreimages(canonicalTransactionCbor).flatMap((field) =>
    countedMachineFieldChunkStepsV1(field.fieldIndex, field.preimageCbor),
  );
