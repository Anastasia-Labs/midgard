/**
 * Counting the machine's field-trace and chunk steps for transaction field carriage.
 */

import {
  buildMidgardBoundedCollection,
  buildMidgardBoundedCollectionItemProof,
  buildMidgardBoundedItemChunkProof,
  deriveMidgardTxFieldPreimages,
  type MidgardBoundedCollection,
  type MidgardBoundedCollectionItemProof,
  type MidgardBoundedItemChunkProof,
} from "@al-ft/midgard-core";
import {
  decodeMidgardFieldPreimage,
  encodeMidgardDefiniteBytes,
  midgardFieldHeaderLengthForCount,
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
 * whose on-chain twin is `lib/midgard/validation-machine/`.
 *
 * The machine steps item-major then chunk-major, four phases hand a
 * partially-read item forward under a genuine `bounded_item_v1` root, and five
 * suites use these functions for step counting and size measurement. That root
 * is derived from bytes the §8 door authenticated against the flat §4 field
 * commitment; it makes no claim about the committed field. The items come from
 * §5.1's one uniform enveloped byte-list decode.
 *
 * Exported so the machine's own test helpers build this trace the same way the
 * machine does. `src/index.ts` re-exports this module wholesale, so the name is
 * public API of `@al-ft/midgard-validation`, imported across the package
 * boundary by
 * `demo/midgard-fault-proofs/tests/cardano-capability-retained-da.reconstruct-authenticated-canonical-transaction-from-field-chunks.ts`,
 * which takes `countedMachineTransactionChunkSteps`. The counted spelling
 * lives in the package whose on-chain twin asks for it, not in
 * `@al-ft/midgard-core`. The discipline it asks
 * for is by name: `counted…` marks a machine-trace structure, and nothing called
 * `counted…` may be compared against a §4 field commitment. Reach for
 * `midgardFieldCommitmentV1` / `verifyMidgardV1TxFieldPreimage` for that.
 */
export const countedMachineFieldTrace = (
  fieldIndex: number,
  preimageCbor: Uint8Array,
): MidgardBoundedCollection =>
  buildMidgardBoundedCollection({
    fieldIndex,
    items: decodeMidgardFieldPreimage(preimageCbor),
  });

/**
 * One `transactionFieldChunk` step of the machine's walk over a field: the
 * per-item collection opening, the chunk opening inside that item, and the §5.1
 * byte count the field has completed through this step.
 *
 * `fieldEncodedSize` is the header width plus the wrapper-and-payload width of
 * each completed item: §5.1 gives all nine fields one envelope, so no
 * per-field rule enters it.
 */
export type MachineFieldChunkStep = {
  readonly fieldIndex: number;
  readonly collectionProof: MidgardBoundedCollectionItemProof;
  readonly chunkProof: MidgardBoundedItemChunkProof;
  readonly fieldEncodedSize: number;
};

/**
 * §5.1's `definite_bytes_header(L) ‖ payload` width, measured with the encoder
 * that defines it rather than re-spelled here.
 */
const midgardWrappedItemBytes = (item: Uint8Array): number =>
  encodeMidgardDefiniteBytes(item).length;

/**
 * The machine's chunk steps for one field, in the order it emits them:
 * item-major, chunk-major.
 *
 * These are the machine's own trace steps, not per-item openings against a
 * field's committed hash: §4 leaves nothing to check such an opening against.
 * They make no claim about the field commitment — a caller that wants
 * the field authenticated calls `verifyMidgardV1TxFieldPreimage`, once, over the
 * whole preimage.
 */
export const countedMachineFieldChunkSteps = (
  fieldIndex: number,
  preimageCbor: Uint8Array,
): readonly MachineFieldChunkStep[] => {
  const collection = countedMachineFieldTrace(fieldIndex, preimageCbor);
  const steps: MachineFieldChunkStep[] = [];
  let fieldEncodedSize = midgardFieldHeaderLengthForCount(
    collection.items.length,
  );
  for (const [itemIndex, item] of collection.items.entries()) {
    const collectionProof = buildMidgardBoundedCollectionItemProof(
      collection,
      itemIndex,
    );
    for (const [chunkIndex] of item.chunkHashes.entries()) {
      if (chunkIndex + 1 === item.chunkHashes.length) {
        fieldEncodedSize += midgardWrappedItemBytes(item.bytes);
      }
      steps.push({
        fieldIndex,
        collectionProof,
        chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
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
 * Under §8 the door authenticates the whole §5.1 preimage once against the
 * flat §4 commitment and an item is a slice of it, so there is no per-item
 * opening to check. A caller that wants a field authenticated calls
 * `verifyMidgardV1TxFieldPreimage`, once, over the whole preimage.
 */

/**
 * What a field-reading step says about the field it read, before anything knows
 * how the bytes will travel.
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
 * transition-only — no carriage is ever hashed into it — so resolving
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
 * output. Carrying the plan input keeps the producer a pure function of the L2
 * transaction, exactly as its callers require, while no carriage §8.4 does not
 * admit ever exists at any instant.
 */
export type ValidationMachineFieldCarriagePlanInput = {
  readonly fieldIndex: number;
  readonly fieldPreimage: Buffer;
};

/** Every field's chunk steps, field-major — the whole-transaction walk order. */
export const countedMachineTransactionChunkSteps = (
  canonicalTransactionCbor: Uint8Array,
  sourceKind: "normal" | "forced" = "normal",
): readonly MachineFieldChunkStep[] =>
  deriveMidgardTxFieldPreimages(canonicalTransactionCbor, sourceKind).flatMap(
    (field) =>
      countedMachineFieldChunkSteps(field.fieldIndex, field.preimageCbor),
  );
