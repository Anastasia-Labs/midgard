import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  aikenSerialisedPlutusDataCbor,
  appendMidgardValidationMerkleLeafV1,
  buildMidgardBlake2b224TraceV1,
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedCollectionV1,
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputAssetFrontierV1,
  buildMidgardLedgerOutputProofTraceV1,
  buildMidgardMpfProofFoldTraceV1,
  buildMidgardRedeemerItemProofTraceV1,
  buildMidgardValidationLedgerDeltaFrontierV1,
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  buildMidgardValidationTraceTree,
  commitMidgardValidationMerkleFrontierV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardLedgerOutputCommitmentV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldPreimages,
  encodeCbor,
  encodeMidgardBlake2b224TraceControlV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardLedgerOutputProofControlV1,
  encodeMidgardMpfProofDescriptorV1,
  finalizeMidgardRedeemerItemProofV1,
  hashMidgardCekMachineStateV1,
  hashMidgardCekProgramEnvelopeV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardMintAssetLeafV1,
  hashMidgardOutputDescriptorLeafV1,
  hashMidgardOutputItemLeafV1,
  hashMidgardOutputLeafV1,
  hashMidgardRedeemerItemLeafV1,
  hashMidgardRedeemerItemProofControlV1,
  hashMidgardRedeemerLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardResolvedContextItemLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardScriptSourceLeafV1,
  hashMidgardSignerLeafV1,
  hashMidgardValidationContextV1,
  hashMidgardValidationLedgerDeltaOperationV1,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardBlake2b224TraceControlV1,
  MidgardBlake2b224TraceStagesV1,
  type MidgardBoundedCollectionItemProofV1,
  type MidgardBoundedCollectionV1,
  midgardBoundedItemChunkCountV1,
  type MidgardBoundedItemChunkProofV1,
  type MidgardConsensusProfileV1,
  type MidgardLedgerOutputAssetV1,
  type MidgardLedgerOutputProofControlV1,
  type MidgardLedgerOutputProofWitnessV1,
  type MidgardMpfProofFoldTraceV1,
  type MidgardMpfProofFrameV1,
  type MidgardRedeemerItemProofControlV1,
  MidgardRedeemerItemProofModesV1,
  MidgardRedeemerItemProofStagesV1,
  type MidgardRedeemerItemProofWitnessV1,
  type MidgardValidationMachineStateV1,
  type MidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleMembershipV1,
  type MidgardValidationPhaseName,
  type MidgardValidationTraceTree,
  parseMidgardMpfProofJsonV1,
} from "@al-ft/midgard-core";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompactV1,
  decodeMidgardSpendInputItemV1,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  decodeSingleCbor,
  deriveMidgardNativeTxProofSourceV1,
  encodeMidgardDefiniteBytesV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardVersionedScript,
  midgardFieldHeaderLengthForCountV1,
  type MidgardValue,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import {
  composeMidgardCekContextSummaryV1,
  decodeMidgardCekContextV1,
  encodeMidgardCekValidationWitnessV1,
  finalizeMidgardCekObserverItemsV1,
  hashMidgardCekContextPartsControlV1,
  hashMidgardCekFinalContextControlV1,
  hashMidgardCekRedeemerContextControlV1,
  hashMidgardCekTxInfoAssemblyControlV1,
  initialMidgardCekContextControlV1,
  initialMidgardCekRedeemerContextControlV1,
  type MidgardCekContextControlV1,
  type MidgardCekContextPartsControlV1,
  type MidgardCekFinalContextControlV1,
  type MidgardCekRedeemerContextControlV1,
  type MidgardCekTxInfoAssemblyControlV1,
  prependMidgardCekObserverItemV1,
  summarizeMidgardCekContextPartsV1,
  summarizeMidgardCekLucidDataV1,
  validateMidgardCekObserverCollectionV1,
} from "./cek-context.js";
import {
  buildMidgardCekExecutionGraphV1,
  executeMidgardCekStructuralProgramV1,
  type MidgardCekExecutionGraphV1,
  type MidgardCekExecutionStepV1,
  type MidgardCekStructuralExecutionV1,
} from "./cek-executor.js";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
  buildCanonicalMidgardLedgerOutputMaterialV1,
} from "./ledger-output-descriptor.js";
import {
  type MidgardRawEnvelopePhaseAProjectionV1,
  projectMidgardRawEnvelopeForPhaseAV1,
} from "./ledger-tx.js";
import type { LocalScriptEvalResult } from "./local-script-eval.js";
import {
  cardanoScriptPurposeData,
  type DecodedMidgardRedeemer,
  decodeMidgardRedeemers,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "./midgard-redeemers.js";
import { validatePhaseASingle } from "./phase-a.js";
import { runPhaseBValidationWithPatch } from "./phase-b.js";
import {
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekMapDataV1,
  summarizeMidgardCekSmallConstrDataV1,
} from "./script-context-proof.js";
import { txOutRefData } from "./tx-out-ref.js";
import type { QueuedTx, RejectCode, RejectedTx } from "./types.js";

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
 * (`demo/midgard-node/src/workers/utils/mpf.ts:1194-1234`, wired at `:4480-4483`),
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

export type MidgardPurposeKindV1 = 0 | 1 | 2 | 3;
export type MidgardRedeemerPurposeTagV1 = 0 | 1 | 3 | 6;

export function redeemerTagForPurposeKindV1(
  purposeKind: MidgardPurposeKindV1,
): MidgardRedeemerPurposeTagV1;
export function redeemerTagForPurposeKindV1(
  purposeKind: number,
): MidgardRedeemerPurposeTagV1 | null;
export function redeemerTagForPurposeKindV1(
  purposeKind: number,
): MidgardRedeemerPurposeTagV1 | null {
  switch (purposeKind) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 2:
      return 3;
    case 3:
      return 6;
    default:
      return null;
  }
}

export const purposeKindForRedeemerTagV1 = (
  redeemerTag: number,
): MidgardPurposeKindV1 | null => {
  switch (redeemerTag) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 3:
      return 2;
    case 6:
      return 3;
    default:
      return null;
  }
};

export const redeemerPointerMatchesPurposeV1 = (input: {
  readonly purposeKind: number;
  readonly purposeIndex: bigint;
  readonly redeemerTag: number;
  readonly redeemerIndex: bigint;
}): boolean => {
  const expectedTag = redeemerTagForPurposeKindV1(input.purposeKind);
  return (
    expectedTag !== null &&
    input.redeemerTag === expectedTag &&
    input.redeemerIndex === input.purposeIndex
  );
};
import { RejectCodes } from "./types.js";
import { outputCborMeetsMinAdaV1 } from "./value-accounting.js";

type ValidationControlDataV1 =
  | bigint
  | Buffer
  | readonly ValidationControlDataV1[];

const encodeValidationControlDataV1 = (
  value: ValidationControlDataV1,
): Buffer => {
  if (typeof value === "bigint") {
    return encodeCborInteger(value);
  }
  if (Buffer.isBuffer(value)) {
    if (value.length <= 64) {
      return encodeCborBytes(value);
    }
    const chunks: Buffer[] = [];
    for (let offset = 0; offset < value.length; offset += 64) {
      chunks.push(encodeCborBytes(value.subarray(offset, offset + 64)));
    }
    return Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]);
  }
  return encodeCborArrayRaw(value.map(encodeValidationControlDataV1));
};

const encodeValidationControlListV1 = (
  values: readonly ValidationControlDataV1[],
): Buffer =>
  Buffer.concat([
    Buffer.from([0x9f]),
    ...values.map(encodeValidationControlDataV1),
    Buffer.from([0xff]),
  ]);

const encodeValidationFrontierPeaksV1 = (
  frontier: MidgardValidationMerkleFrontierV1,
): readonly (readonly [bigint, Buffer])[] =>
  frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]);

export type ScriptDiscoveryTraceControlV1 = {
  readonly purposeCursor: number;
  readonly sourceCursor: number;
  readonly redeemerCursor: number;
  readonly currentPurposeKind: -1 | 0 | 1 | 2 | 3;
  readonly currentPurposeIndex: bigint;
  readonly currentScriptHash: Buffer;
  readonly currentSubject: Buffer;
  readonly matchedSourceIndex: number;
  readonly matchedLanguageTag: -1 | 0 | 3 | 128;
  readonly matchedSourceLeaf: Buffer;
  readonly usedInlineBitmap: bigint;
  readonly usedRedeemerBitmap: bigint;
  readonly redeemerItemControlHash: Buffer;
  readonly executionFrontier: MidgardValidationMerkleFrontierV1;
};

export const encodeScriptDiscoveryControlCborV1 = (
  discovery: ScriptDiscoveryTraceControlV1,
): Buffer =>
  encodeCbor([
    BigInt(discovery.purposeCursor),
    BigInt(discovery.sourceCursor),
    BigInt(discovery.redeemerCursor),
    BigInt(discovery.currentPurposeKind),
    discovery.currentPurposeIndex,
    discovery.currentScriptHash,
    discovery.currentSubject,
    BigInt(discovery.matchedSourceIndex),
    BigInt(discovery.matchedLanguageTag),
    discovery.matchedSourceLeaf,
    discovery.usedInlineBitmap,
    discovery.usedRedeemerBitmap,
    discovery.redeemerItemControlHash,
    BigInt(discovery.executionFrontier.count),
    encodeValidationFrontierPeaksV1(discovery.executionFrontier),
  ]);

export type ValidationMachineLedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
};

export type ValidationMachineLedgerOp =
  | { readonly type: "delete"; readonly key: Buffer }
  /** Insert values are exact canonical Midgard ledger output descriptors. */
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer };

export type ValidationMachineLedgerMutationStep = {
  readonly operation: ValidationMachineLedgerOp;
  readonly preRoot: Buffer;
  readonly postRoot: Buffer;
  /** Canonical bounded-frame form consumed by the deployed resolver chain. */
  readonly proofFoldTrace: MidgardMpfProofFoldTraceV1;
};

export type ValidationMachineValueMutationStep = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
  readonly oldDelta: bigint | null;
  readonly preAssetRoot: Buffer;
  readonly postAssetRoot: Buffer;
  /** Membership/non-membership witness for unit against preAssetRoot. */
  readonly proofCbor: Buffer;
  readonly postSeenAssetCount: number;
  readonly postNonzeroAssetCount: number;
};

export type ValidationMachineReplayInput = {
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly eventKeyCbor: Buffer;
  readonly transactionId: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor?: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorUtxosRoot: string;
  readonly postUtxosRoot: string;
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly expectedLedgerOps: readonly ValidationMachineLedgerOp[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly expectedVerdict: "accepted" | "rejected";
  readonly expectedRejectionCode: RejectCode | null;
  /**
   * Verdict carried by the COMMITTED forced leaf — the operator's
   * adjudication, which is what `source_binding_is_exact` reveals on-chain
   * and therefore what the machine's `transaction_commitment` must bind.
   * Defaults to the replay's own verdict, which is exact on the classifier
   * path (the leaf is produced from this replay, and the machine aborts on
   * any expected/replayed divergence). A dispute trace replayed AGAINST an
   * operator leaf whose verdict it contests must pass the leaf's verdict
   * here, or its states bind a commitment the committed leaf does not carry.
   */
  readonly committedForcedVerdict?: "accepted" | "rejected";
  readonly blockEndTimeMs: number;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
};

const exactTrieRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

export const buildValidationMachineLedgerInsertOpV1 = ({
  key,
  outputCbor,
}: {
  readonly key: Uint8Array;
  readonly outputCbor: Uint8Array;
}): Extract<ValidationMachineLedgerOp, { readonly type: "insert" }> => ({
  type: "insert",
  key: Buffer.from(key),
  value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: key,
    outputCbor,
  }).descriptorCbor,
});

export const buildValidationMachineLedgerMutationSteps = async (input: {
  readonly initialEntries: readonly ValidationMachineLedgerEntry[];
  readonly operations: readonly ValidationMachineLedgerOp[];
}): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [...input.initialEntries].sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  )) {
    await trie.insert(
      entry.outRef,
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: entry.outRef,
        outputCbor: entry.output,
      }).descriptorCbor,
    );
  }
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of input.operations) {
    steps.push(
      await applyValidationMachineLedgerMutationStepV1(trie, operation),
    );
  }
  return steps;
};

export const applyValidationMachineLedgerMutationStepV1 = async (
  trie: Trie,
  operation: ValidationMachineLedgerOp,
): Promise<ValidationMachineLedgerMutationStep> => {
  const preRoot = exactTrieRoot(trie);
  const mutationValue =
    operation.type === "insert"
      ? Buffer.from(operation.value)
      : await trie.get(operation.key);
  if (mutationValue === undefined) {
    throw new Error(
      "cannot construct a ledger deletion proof for an absent key",
    );
  }
  const proof = await trie.prove(operation.key, operation.type === "insert");
  const proofFoldTrace = buildMidgardMpfProofFoldTraceV1({
    key: operation.key,
    value: mutationValue,
    steps: parseMidgardMpfProofJsonV1(proof.toJSON()),
  });
  if (operation.type === "delete") {
    await trie.delete(operation.key);
  } else {
    await trie.insert(operation.key, operation.value);
  }
  const postRoot = exactTrieRoot(trie);
  const foldPreRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.includingRoot
      : proofFoldTrace.terminal.excludingRoot;
  const foldPostRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.excludingRoot
      : proofFoldTrace.terminal.includingRoot;
  if (!foldPreRoot.equals(preRoot) || !foldPostRoot.equals(postRoot)) {
    throw new Error(
      "bounded MPF proof fold disagrees with the applied ledger mutation",
    );
  }
  return {
    operation,
    preRoot,
    postRoot,
    proofFoldTrace,
  };
};

export type ValidationMachineWorkWitness = {
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly cbor: Buffer;
  readonly auxiliary:
    | {
        /**
         * One item of one committed field, reached through §8's door. Nine of
         * the machine's fifteen per-item sites match this arm, across all eight
         * phases that read a field.
         *
         * It used to carry a counted `(collectionProof, chunkProof)` pair
         * checked against the §4 flat field commitment — a predicate no honest
         * prover could satisfy (#592). What replaces the pair is not a smaller
         * proof but *no* proof: the carriage names where the field's preimage
         * is, the door authenticates the whole preimage once against the flat
         * commitment, and the item is then a slice.
         *
         * `fieldIndex` is on the wire because §4 removed field-index domain
         * separation and two phases read more than one slot — `canonicalDecode`,
         * which walks all nine from its own control, and `inputSets`, which
         * alternates fields 0 and 1. `itemIndex` is on the wire because two
         * sites let the prover choose the item order and the claimed successor
         * pins it.
         *
         * `fieldPreimage` is the plan input, not wire: it is replaced by the §8
         * carriage §8.4 admits for its length when the auxiliary is encoded
         * (#600). See {@link ValidationMachineFieldCarriagePlanInputV1}.
         */
        readonly kind: "transactionFieldChunk";
        readonly fieldIndex: number;
        readonly itemIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        /**
         * `canonicalDecode`'s complete-item step: one item read whole rather
         * than chunk by chunk. Field index and item index come from the phase's
         * control, so the carriage is the entire wire surface — `fieldIndex`
         * here is the plan input's, never encoded (#600).
         */
        readonly kind: "transactionFieldItem";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        readonly kind: "ledgerOutputProofBegin";
        readonly outputIndex: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "ledgerOutputProofStep";
        readonly witness: MidgardLedgerOutputProofWitnessV1;
      }
    | {
        readonly kind: "ledgerOutputProofFinalize";
        readonly descriptorCbor: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        /**
         * A field-4 required-signer item plus the signer-set membership
         * evidence the step decides on. No field or item index **on the wire**:
         * the field is 4 by construction and the item index is
         * `control.required_seen`. `fieldIndex` is carried only as the plan
         * input's, and is never encoded (#600).
         */
        readonly kind: "requiredSignerItem";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptToken";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptFrame";
        readonly frame: ValidationMachineNativeScriptFrameV1;
      }
    | {
        readonly kind: "scriptSourceHashBlock";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
      }
    | {
        readonly kind: "mintFoldAsset";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
      }
    | {
        readonly kind: "scheduledLedgerLookup";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer | null;
        readonly proofCbor: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "resolvedInputReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
      }
    | {
        readonly kind: "scriptPurposeScan";
        readonly purposeKind: 0 | 1 | 2 | 3;
        readonly purposeIndex: bigint;
        readonly scriptHash: Buffer;
        readonly subject: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "scriptSourceScan";
        readonly sourceIndex: number;
        readonly originKind: "inline" | "reference";
        readonly sourceKey: Buffer;
        readonly scriptLanguageTag: 0 | 3 | 128;
        readonly scriptHash: Buffer;
        readonly scriptTotalLength: number;
        readonly scriptItemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "redeemerScanBegin";
        readonly itemIndex: number;
        readonly itemCount: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        /**
         * `scriptSources` stage 1 (field 8, one redeemer item) and stage 4
         * (field 2, one output item). Both stages need the item's length and its
         * `bounded_item_v1` commitment and never look at its bytes, so the
         * door's derived commitment is all the carriage has to yield; field
         * index and item index are fixed by the stage and its cursor, so
         * `fieldIndex` here is the plan input's and is never encoded (#600).
         *
         * This is the C21-STAGE4 site. Its evidence is O(1) in output size
         * exactly when the resolved carriage is tier 2 or 3
         * (`onchain/aiken/lib/midgard/validation-machine-v1.ak:9189-9192`), which
         * is what resolving at evidence-commitment time restores.
         */
        readonly kind: "transactionRedeemerItemBegin";
        readonly fieldIndex: number;
        readonly fieldPreimage: Buffer;
      }
    | {
        readonly kind: "nativeExecutionScan";
        readonly executionIndex: number;
        readonly languageTag: 0 | 3 | 128;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly source: {
          readonly sourceIndex: number;
          readonly originKind: "inline" | "reference";
          readonly sourceKey: Buffer;
          readonly scriptTotalLength: number;
          readonly scriptItemCommitment: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly redeemerLeaf: Buffer;
        readonly executionSiblings: readonly Buffer[];
        readonly firstChunkProof: MidgardBoundedItemChunkProofV1;
      }
    | {
        readonly kind: "nativeExecutionDescriptor";
        readonly executionIndex: number;
        readonly languageTag: 0 | 3 | 128;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly source: {
          readonly sourceIndex: number;
          readonly originKind: "inline" | "reference";
          readonly sourceKey: Buffer;
          readonly scriptTotalLength: number;
          readonly scriptItemCommitment: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly redeemerLeaf: Buffer;
        readonly executionSiblings: readonly Buffer[];
        readonly firstChunkProof: MidgardBoundedItemChunkProofV1 | null;
        readonly signerFrontier: MidgardValidationMerkleFrontierV1;
      }
    | {
        readonly kind: "cekCoreStep";
        readonly step: MidgardCekExecutionStepV1;
      }
    | {
        readonly kind: "cekResolvedContextItem";
        readonly sourceKind: "spend" | "reference";
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekOutputContextItem";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekSignerContextItem";
        readonly frontier: MidgardValidationMerkleFrontierV1;
        readonly signerIndex: number;
        readonly signerHash: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekMintContextItem";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekRedeemerContextSelect";
        readonly control: MidgardCekRedeemerContextControlV1;
        readonly itemIndex: number;
        readonly itemCount: number;
        readonly totalLength: number;
        readonly itemCommitment: Buffer;
        readonly redeemerSiblings: readonly Buffer[];
        readonly purposeFrontierIndex: number;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
      }
    | {
        readonly kind: "redeemerItemStep";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1 | null;
        readonly control: MidgardRedeemerItemProofControlV1;
        readonly witness: MidgardRedeemerItemProofWitnessV1;
      }
    | {
        readonly kind: "cekContextFinalize";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
      }
    | {
        readonly kind: "cekContextFinalizeSpend";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekContextAssemble";
        readonly control: MidgardCekContextPartsControlV1;
      }
    | {
        readonly kind: "cekTxInfoFinalize";
        readonly control: MidgardCekTxInfoAssemblyControlV1;
      }
    | {
        readonly kind: "cekContextSeed";
        readonly control: MidgardCekFinalContextControlV1;
      }
    | {
        readonly kind: "valueInputAsset";
        readonly sourceKind: "spend";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly descriptorCbor: Buffer;
        readonly assetIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly assetFrontier: MidgardValidationMerkleFrontierV1;
        readonly assetSiblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueOutputDescriptor";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "valueOutputAsset";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly assetIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly assetFrontier: MidgardValidationMerkleFrontierV1;
        readonly assetSiblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueMintAsset";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "ledgerDeltaOperation";
        readonly operationKind: "delete" | "insert";
        readonly key: Buffer;
        readonly value: Buffer;
        readonly mutationStep: ValidationMachineLedgerMutationStep;
        readonly operationMembership: MidgardValidationMerkleMembershipV1;
      }
    | {
        readonly kind: "ledgerDeltaReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
      }
    | {
        readonly kind: "ledgerDeltaOutput";
        readonly outputIndex: number;
        readonly descriptorCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "ledgerDeltaProofFrame";
        readonly frame: MidgardMpfProofFrameV1;
        readonly siblings: readonly Buffer[];
      }
    | null;
};

export type ValidationMachineSignerSetProof =
  | { readonly kind: "none" }
  | {
      readonly kind: "membership";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly signerIndex: number;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "empty";
      readonly frontier: MidgardValidationMerkleFrontierV1;
    }
  | {
      readonly kind: "belowFirst";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly firstSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "aboveLast";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lastSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "between";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lowerIndex: number;
      readonly lowerSignerHash: Buffer;
      readonly lowerSiblings: readonly Buffer[];
      readonly upperSignerHash: Buffer;
      readonly upperSiblings: readonly Buffer[];
    };

export type DeterministicValidationMachineTrace = {
  readonly validationContextCbor: Buffer;
  /** Canonical, immutable input material for the CEK selection transition. */
  readonly programMaterialSidecarCbor: Buffer;
  readonly states: readonly MidgardValidationMachineStateV1[];
  readonly witnesses: readonly ValidationMachineWorkWitness[];
  readonly tree: MidgardValidationTraceTree;
  readonly verdict: "accepted" | "rejected";
  readonly rejectionCode: RejectCode | null;
  readonly ledgerOps: readonly ValidationMachineLedgerOp[];
};

const ZERO_32 = Buffer.alloc(32);

const RESOLVED_INPUTS_ACCUMULATOR_DOMAIN = Buffer.from(
  "MidgardResolvedInputsAccumulatorV1",
  "ascii",
);
const INPUT_RESOLUTION_SCHEDULE_DOMAIN = Buffer.from(
  "MidgardInputResolutionScheduleV1",
  "ascii",
);
const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(Buffer.from(bytes), { dkLen: 32 }));

const NATIVE_SCRIPT_SCAN_FRAME_DOMAIN_V1 = Buffer.from(
  "MidgardNativeScriptScanFrameV1",
  "ascii",
);
const MAX_NATIVE_SCRIPT_SCAN_NODES_V1 = 16_384;
const MAX_NATIVE_SCRIPT_SCAN_DEPTH_V1 = 16_384;

type ValidationMachineNativeScriptTokenV1 = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly nextOffset: number;
  readonly childCount: number;
  readonly required: bigint;
  readonly keyHash: Buffer;
  readonly slot: bigint;
};

type ValidationMachineNativeScriptTokenHeadV1 = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly payloadOffset: number;
};

export type ValidationMachineNativeScriptFrameV1 = {
  readonly tail: Buffer;
  readonly kind: 1 | 2 | 3;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

type ValidationMachineVersionedScriptHeaderV1 = {
  readonly languageTag: 0 | 3 | 128;
  readonly payloadOffset: number;
  readonly payloadLength: number;
};

const readValidationMachineVersionedScriptHeaderV1 = (
  item: Buffer,
): ValidationMachineVersionedScriptHeaderV1 => {
  const outer = readCborArrayHeader(item, 0, "versioned_script");
  if (outer.length !== 2) {
    throw new Error("versioned script must contain exactly two fields");
  }
  const language = readCborUnsigned(
    item,
    outer.nextOffset,
    "versioned_script.language",
  );
  const payload = readCborBytes(
    item,
    language.nextOffset,
    "versioned_script.payload",
  );
  if (
    (language.value !== 0n &&
      language.value !== 3n &&
      language.value !== 128n) ||
    payload.nextOffset !== item.length
  ) {
    throw new Error("versioned script has an invalid language or length");
  }
  return {
    languageTag: Number(language.value) as 0 | 3 | 128,
    payloadOffset: payload.nextOffset - payload.value.length,
    payloadLength: payload.value.length,
  };
};

const readValidationMachineNativeScriptTokenHeadV1 = (
  item: Buffer,
  offset: number,
): ValidationMachineNativeScriptTokenHeadV1 => {
  const outer = readCborArrayHeader(item, offset, "native_script");
  const tag = readCborUnsigned(item, outer.nextOffset, "native_script.tag");
  if (tag.value < 0n || tag.value > 5n) {
    throw new Error("native script has an unsupported tag");
  }
  const kind = Number(tag.value) as 0 | 1 | 2 | 3 | 4 | 5;
  if (
    (kind === 3 && outer.length !== 3) ||
    (kind !== 3 && outer.length !== 2)
  ) {
    throw new Error("native script has an invalid outer shape");
  }
  return { kind, payloadOffset: tag.nextOffset };
};

const readValidationMachineNativeScriptPayloadV1 = (
  item: Buffer,
  offset: number,
  kind: 0 | 1 | 2 | 3 | 4 | 5,
): ValidationMachineNativeScriptTokenV1 => {
  if (kind === 0) {
    const keyHash = readCborBytes(item, offset, "native_script.key_hash");
    if (keyHash.value.length !== 28) {
      throw new Error("native signature script has an invalid shape");
    }
    return {
      kind: 0,
      nextOffset: keyHash.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: keyHash.value,
      slot: 0n,
    };
  }
  if (kind === 1 || kind === 2) {
    const children = readCborArrayHeader(
      item,
      offset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
      throw new Error("native all/any script has an invalid shape");
    }
    return {
      kind,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 3) {
    const required = readCborUnsigned(item, offset, "native_script.required");
    const children = readCborArrayHeader(
      item,
      required.nextOffset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
      throw new Error("native at-least script has an invalid shape");
    }
    return {
      kind: 3,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: required.value,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 4 || kind === 5) {
    const slot = readCborUnsigned(item, offset, "native_script.slot");
    return {
      kind,
      nextOffset: slot.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: slot.value,
    };
  }
  throw new Error("native script payload has an unsupported tag");
};

const validationMachineNativeScriptFrameIsWellFormedV1 = (
  frame: ValidationMachineNativeScriptFrameV1,
): boolean => {
  const processed = frame.childCount - frame.remaining;
  return (
    (frame.tail.length === 0 || frame.tail.length === 32) &&
    frame.childCount > 0 &&
    frame.childCount <= MAX_NATIVE_SCRIPT_SCAN_NODES_V1 &&
    frame.remaining > 0 &&
    frame.remaining <= frame.childCount &&
    frame.validCount >= 0 &&
    frame.validCount <= processed &&
    (frame.kind === 3 ? frame.required >= 0n : frame.required === 0n)
  );
};

const hashValidationMachineNativeScriptFrameV1 = (
  frame: ValidationMachineNativeScriptFrameV1,
): Buffer => {
  if (!validationMachineNativeScriptFrameIsWellFormedV1(frame)) {
    throw new Error("cannot hash a malformed native-script frame");
  }
  return hash32(
    Buffer.concat([
      NATIVE_SCRIPT_SCAN_FRAME_DOMAIN_V1,
      encodeCbor([
        frame.tail,
        BigInt(frame.kind),
        BigInt(frame.childCount),
        BigInt(frame.remaining),
        BigInt(frame.validCount),
        frame.required,
      ]),
    ]),
  );
};

const canonicalCborArgumentHeaderSize = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("canonical CBOR argument must be a non-negative integer");
  }
  if (value < 24) return 1;
  if (value < 0x100) return 2;
  if (value < 0x1_0000) return 3;
  if (value < 0x1_0000_0000) return 5;
  return 9;
};

const MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX = 6;
const MIDGARD_V1_ADDRESS_WITNESSES_FIELD_INDEX = 7;

const canonicalFieldItemEncodedLength = (
  fieldIndex: number,
  itemLength: number,
): number => {
  if (
    [0, 1, 2, 3, 4, MIDGARD_V1_ADDRESS_WITNESSES_FIELD_INDEX].includes(
      fieldIndex,
    )
  ) {
    return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
  }
  if (
    fieldIndex === MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX ||
    fieldIndex === 8
  ) {
    return itemLength;
  }
  if (fieldIndex !== 5 || itemLength === 0) {
    throw new Error(
      `invalid canonical field item length at field ${fieldIndex.toString()}`,
    );
  }
  return itemLength - 1;
};

export const initialMidgardResolvedInputsAccumulatorV1 = (): Buffer =>
  hash32(RESOLVED_INPUTS_ACCUMULATOR_DOMAIN);

export const emptyMidgardInputResolutionScheduleV1 = (): Buffer =>
  hash32(INPUT_RESOLUTION_SCHEDULE_DOMAIN);

export const prependMidgardInputResolutionScheduleV1 = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly nextHash: Uint8Array;
}): Buffer => {
  if (input.nextHash.length !== 32) {
    throw new Error("input-resolution schedule hash must contain 32 bytes");
  }
  return hash32(
    Buffer.concat([
      INPUT_RESOLUTION_SCHEDULE_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      Buffer.from(input.nextHash),
    ]),
  );
};

export const advanceMidgardResolvedInputsAccumulatorV1 = (input: {
  readonly accumulator: Uint8Array;
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly value: Uint8Array;
}): Buffer => {
  if (input.accumulator.length !== 32) {
    throw new Error("resolved-input accumulator must contain exactly 32 bytes");
  }
  return hash32(
    Buffer.concat([
      RESOLVED_INPUTS_ACCUMULATOR_DOMAIN,
      Buffer.from(input.accumulator),
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      encodeCbor(Buffer.from(input.value)),
    ]),
  );
};

const exactHash32 = (hex: string, field: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(hex)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return Buffer.from(hex, "hex");
};

const canonicalLedgerOps = (
  operations: readonly ValidationMachineLedgerOp[],
): Buffer =>
  encodeCbor(
    operations.map((operation) =>
      operation.type === "delete"
        ? [0n, operation.key]
        : [1n, operation.key, operation.value],
    ),
  );

const sameLedgerOps = (
  left: readonly ValidationMachineLedgerOp[],
  right: readonly ValidationMachineLedgerOp[],
): boolean => canonicalLedgerOps(left).equals(canonicalLedgerOps(right));

type ValidationValueAccumulator = {
  lovelaceDelta: bigint;
  assetRoot: Buffer;
  seenAssetCount: number;
  nonzeroAssetCount: number;
};

const emptyValidationValueAccumulator = (): ValidationValueAccumulator => ({
  lovelaceDelta: 0n,
  assetRoot: Buffer.alloc(32),
  seenAssetCount: 0,
  nonzeroAssetCount: 0,
});

const encodeValidationValueAccumulator = (
  accumulator: ValidationValueAccumulator,
): Buffer =>
  encodeCbor([
    accumulator.lovelaceDelta,
    accumulator.assetRoot,
    BigInt(accumulator.seenAssetCount),
    BigInt(accumulator.nonzeroAssetCount),
  ]);

type ValidationValueContribution = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
};

const midgardValueAssets = (
  value: MidgardValue,
): readonly MidgardLedgerOutputAssetV1[] =>
  [...value.assets.entries()].flatMap(([policyId, policyAssets]) =>
    [...policyAssets.entries()].map(([assetName, quantity]) => ({
      policyId: Buffer.from(policyId, "hex"),
      assetName: Buffer.from(assetName, "hex"),
      quantity,
    })),
  );

const midgardValueContributions = (
  value: MidgardValue,
  multiplier: 1n | -1n,
): readonly ValidationValueContribution[] =>
  midgardValueAssets(value).map(({ policyId, assetName, quantity }) => ({
    unit: Buffer.concat([policyId, assetName]),
    quantityDelta: quantity * multiplier,
  }));

const buildValidationValueMutationSteps = async (
  contributions: readonly ValidationValueContribution[],
): Promise<readonly ValidationMachineValueMutationStep[]> => {
  const assetStore = new Store(undefined);
  await assetStore.ready();
  const assetTrie = new Trie(assetStore);
  const deltas = new Map<string, bigint>();
  const steps: ValidationMachineValueMutationStep[] = [];

  for (const contribution of contributions) {
    if (contribution.quantityDelta === 0n) {
      throw new Error("value mutation quantity delta must be non-zero");
    }
    const unit = Buffer.from(contribution.unit);
    const unitHex = unit.toString("hex");
    const oldDelta = deltas.get(unitHex) ?? null;
    const preAssetRoot = exactTrieRoot(assetTrie);
    const proofCbor = Buffer.from(
      (await assetTrie.prove(unit, oldDelta === null)).toCBOR(),
    );
    const nextDelta = (oldDelta ?? 0n) + contribution.quantityDelta;

    if (oldDelta !== null) {
      await assetTrie.delete(unit);
    }
    await assetTrie.insert(unit, encodeCbor(nextDelta));
    deltas.set(unitHex, nextDelta);

    steps.push({
      unit,
      quantityDelta: contribution.quantityDelta,
      oldDelta,
      preAssetRoot,
      postAssetRoot: exactTrieRoot(assetTrie),
      proofCbor,
      postSeenAssetCount: deltas.size,
      postNonzeroAssetCount: [...deltas.values()].filter(
        (quantity) => quantity !== 0n,
      ).length,
    });
  }
  return steps;
};

const applyValidationValueMutationStep = (
  accumulator: ValidationValueAccumulator,
  step: ValidationMachineValueMutationStep,
): void => {
  if (!accumulator.assetRoot.equals(step.preAssetRoot)) {
    throw new Error(
      "value mutation step does not continue the authenticated root",
    );
  }
  accumulator.assetRoot = Buffer.from(step.postAssetRoot);
  accumulator.seenAssetCount = step.postSeenAssetCount;
  accumulator.nonzeroAssetCount = step.postNonzeroAssetCount;
};

const rejectionPhase = (rejection: RejectedTx): MidgardValidationPhaseName => {
  if (rejection.consensusPhase === undefined) {
    throw new Error(
      `V1 rejection ${rejection.code} is missing its exact consensus phase`,
    );
  }
  return rejection.consensusPhase;
};

const orderedPhases: readonly MidgardValidationPhaseName[] = [
  "canonicalDecode",
  "compactBinding",
  "staticLedgerRules",
  "inputSets",
  "signatures",
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
  "resolveInputs",
  "scriptSources",
  "nativeScripts",
  "scriptIntegrity",
  "cek",
  "valueAndMint",
  "ledgerDelta",
];

const safeBlockEndTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockEndTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};

/**
 * Replays the exact Phase A/B implementation and commits every macro-machine
 * witness it consumed. This is deliberately strict: a supplied operator
 * verdict, rejection code, or ledger delta that differs from replay aborts
 * block construction.
 */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    const contextCbor = Buffer.from(
      aikenSerialisedPlutusDataCbor(
        encodeCbor([
          1n,
          Buffer.from(input.consensusProfile.profileId, "ascii"),
          safeBlockEndTime(input.blockEndTimeMs),
          input.expectedNetworkId,
          input.minFeeA,
          input.minFeeB,
          input.blockSlot,
        ]).toString("hex"),
      ),
      "hex",
    );
    const validationContextHash = hashMidgardValidationContextV1(contextCbor);
    const priorLedgerRoot = exactHash32(input.priorUtxosRoot, "priorUtxosRoot");
    const postLedgerRoot = exactHash32(input.postUtxosRoot, "postUtxosRoot");
    if (input.transactionId.length !== 32) {
      return yield* Effect.fail(
        new Error("transactionId must contain exactly 32 bytes"),
      );
    }

    const queued: QueuedTx = {
      txId: Buffer.from(input.transactionId),
      txCbor: Buffer.from(input.canonicalTransactionCbor),
      programMaterialSidecarCbor:
        input.programMaterialSidecarCbor === undefined
          ? encodeMidgardCekProgramMaterialSidecarV1([])
          : Buffer.from(input.programMaterialSidecarCbor),
      arrivalSeq: 0n,
      createdAt: new Date(input.blockEndTimeMs),
    };
    const phaseA = validatePhaseASingle(queued, {
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
      consensusProfile: input.consensusProfile,
    });

    const ledgerState = new Map<string, Buffer>();
    const ledgerDescriptorState = new Map<string, Buffer>();
    for (const entry of input.ledgerWitnessEntries) {
      const outRefHex = entry.outRef.toString("hex");
      if (ledgerState.has(outRefHex)) {
        return yield* Effect.fail(
          new Error(`duplicate ledger witness entry for out-ref ${outRefHex}`),
        );
      }
      ledgerState.set(outRefHex, Buffer.from(entry.output));
      const outputMaterial = yield* Effect.try({
        try: () =>
          buildCanonicalMidgardLedgerEntryOutputMaterialV1({
            outRef: entry.outRef,
            outputCbor: entry.output,
          }),
        catch: () =>
          new Error(
            `persisted ledger output ${outRefHex} cannot produce an exact V1 descriptor`,
          ),
      });
      ledgerDescriptorState.set(outRefHex, outputMaterial.descriptorCbor);
    }
    let rawExecutionProjection: MidgardRawEnvelopePhaseAProjectionV1 | null =
      null;
    if (
      !("ledgerTx" in phaseA) &&
      phaseA.code === RejectCodes.InvalidFieldType &&
      phaseA.consensusPhase === "canonicalDecode"
    ) {
      try {
        const projected = projectMidgardRawEnvelopeForPhaseAV1(queued.txCbor);
        if (
          projected.canonicalSubmittedTx === null &&
          projected.scriptWitnesses.some(
            ({ languageTag, versionedItemBytes }) => {
              if (languageTag !== 0) return false;
              try {
                decodeMidgardVersionedScript(versionedItemBytes);
                return false;
              } catch {
                return true;
              }
            },
          )
        )
          rawExecutionProjection = projected;
      } catch {
        // Non-field-6 malformed material remains the original fail-closed
        // canonicalDecode rejection.
      }
    }
    const phaseALedgerTx =
      "ledgerTx" in phaseA
        ? phaseA.ledgerTx
        : rawExecutionProjection === null
          ? null
          : ({
              ...rawExecutionProjection.ledgerTx,
              scriptWitnesses: rawExecutionProjection.scriptWitnesses.map(
                (witness) => ({
                  index: witness.index,
                  hash: witness.hash,
                  script:
                    witness.languageTag === 0
                      ? {
                          language: "NativeCardano" as const,
                          scriptBytes: witness.scriptBytes,
                          // Structural semantics consume the retained bytes;
                          // this placeholder never reaches ledger evaluation.
                          nativeScript: { type: "all" as const, scripts: [] },
                        }
                      : witness.languageTag === 3
                        ? {
                            language: "PlutusV3" as const,
                            scriptBytes: witness.scriptBytes,
                          }
                        : {
                            language: "MidgardV1" as const,
                            scriptBytes: witness.scriptBytes,
                          },
                }),
              ),
            } as const);
    const scriptEvaluations: {
      readonly scriptBytes: Buffer;
      readonly contextCbor: Buffer;
      readonly result: LocalScriptEvalResult;
      readonly graph: MidgardCekExecutionGraphV1 | null;
      readonly execution: MidgardCekStructuralExecutionV1 | null;
    }[] = [];
    const programMaterial = decodeMidgardCekProgramMaterialSidecarV1(
      queued.programMaterialSidecarCbor ??
        encodeMidgardCekProgramMaterialSidecarV1([]),
    );
    const canonicalProgramMaterialSidecarCbor = Buffer.from(
      encodeMidgardCekProgramMaterialSidecarV1(programMaterial),
    );
    if (
      !canonicalProgramMaterialSidecarCbor.equals(
        queued.programMaterialSidecarCbor ?? Buffer.alloc(0),
      )
    ) {
      return yield* Effect.fail(
        new Error("program material sidecar must use canonical V1 CBOR"),
      );
    }

    let rejection: RejectedTx | null = null;
    let ledgerOps: readonly ValidationMachineLedgerOp[] = [];
    if (!("ledgerTx" in phaseA)) {
      rejection = phaseA;
      if (rawExecutionProjection !== null)
        rejection = { ...phaseA, consensusPhase: "nativeScripts" };
    } else {
      const phaseB = yield* runPhaseBValidationWithPatch(
        [phaseA],
        ledgerState,
        {
          nowCardanoSlotNo: input.blockSlot,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          evaluateProofScript: (
            scriptBytes,
            scriptContextCbor,
            executionBudget?: {
              readonly cpu: bigint;
              readonly memory: bigint;
            },
          ) =>
            Effect.sync(() => {
              let graph: MidgardCekExecutionGraphV1 | null = null;
              let execution: MidgardCekStructuralExecutionV1 | null = null;
              let result: LocalScriptEvalResult;
              try {
                const envelope = decodeMidgardCekProgramEnvelopeV1(scriptBytes);
                graph = buildMidgardCekExecutionGraphV1(
                  envelope,
                  programMaterial,
                  scriptContextCbor,
                );
                execution = executeMidgardCekStructuralProgramV1({
                  root: graph.root,
                  material: graph.material.values(),
                  constantWitnesses: graph.constantWitnesses,
                  maxSteps:
                    input.consensusProfile.limits.maxValidationMachineStepCount,
                  executionBudget,
                });
                result =
                  execution.stopReason === "budgetExceeded" ||
                  execution.terminalState.mode === "haltSuccess"
                    ? {
                        kind: "accepted",
                        budget: {
                          cpu: execution.terminalState.cpu,
                          memory: execution.terminalState.memory,
                        },
                      }
                    : {
                        kind: "script_invalid",
                        detail: `V1 CEK halted with error ${execution.terminalState.auxiliary.toString(10)}`,
                      };
              } catch (cause) {
                result = {
                  kind: "script_invalid",
                  detail: `V1 CEK execution failed closed: ${String(cause)}`,
                };
              }
              scriptEvaluations.push({
                scriptBytes: Buffer.from(scriptBytes),
                contextCbor: Buffer.from(scriptContextCbor),
                result,
                graph,
                execution,
              });
              return result;
            }),
        },
      );
      rejection = phaseB.rejected[0] ?? null;
      if (rejection === null) {
        ledgerOps = [
          ...phaseB.statePatch.deletedOutRefs.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...phaseB.statePatch.upsertedOutRefs.map(([outRef, output]) =>
            buildValidationMachineLedgerInsertOpV1({
              key: Buffer.from(outRef, "hex"),
              outputCbor: output,
            }),
          ),
        ];
      }
    }

    const verdict = rejection === null ? "accepted" : "rejected";
    const rejectionCode = rejection?.code ?? null;
    if (
      verdict !== input.expectedVerdict ||
      rejectionCode !== input.expectedRejectionCode
    ) {
      return yield* Effect.fail(
        new Error(
          `validation replay disagrees with operator classification: expected=${input.expectedVerdict}/${input.expectedRejectionCode ?? "none"},actual=${verdict}/${rejectionCode ?? "none"},detail=${rejection?.detail ?? "none"}`,
        ),
      );
    }
    if (!sameLedgerOps(ledgerOps, input.expectedLedgerOps)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger delta differs from block transition",
        ),
      );
    }
    if (
      input.ledgerMutationSteps.length !== ledgerOps.length ||
      input.ledgerMutationSteps.some(
        (step, index) => !sameLedgerOps([step.operation], [ledgerOps[index]!]),
      )
    ) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation steps differ from the exact ledger delta",
        ),
      );
    }
    let mutationRoot = priorLedgerRoot;
    for (const step of input.ledgerMutationSteps) {
      if (!step.preRoot.equals(mutationRoot)) {
        return yield* Effect.fail(
          new Error(
            "validation replay ledger-mutation roots are not contiguous",
          ),
        );
      }
      mutationRoot = step.postRoot;
    }
    if (!mutationRoot.equals(postLedgerRoot)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation terminal root differs from the block transition",
        ),
      );
    }
    if (
      verdict === "rejected" &&
      (!priorLedgerRoot.equals(postLedgerRoot) || ledgerOps.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("a rejected transaction must commit an exact ledger no-op"),
      );
    }

    const authenticatedLedgerOps = input.ledgerMutationSteps.map(
      ({ operation, proofFoldTrace }) => ({
        ...operation,
        proofDescriptor: proofFoldTrace.descriptor,
      }),
    );
    const ledgerDeltaFrontier = buildMidgardValidationLedgerDeltaFrontierV1(
      authenticatedLedgerOps,
    );
    const ledgerDeltaRoot = hashMidgardValidationLedgerDeltaV1(
      authenticatedLedgerOps,
    );
    const ledgerDeltaOperationLeafHashes = authenticatedLedgerOps.map(
      hashMidgardValidationLedgerDeltaOperationV1,
    );
    const ledgerDeltaOperationMembership = (
      operationIndex: number,
    ): MidgardValidationMerkleMembershipV1 =>
      buildMidgardValidationMerkleMembershipV1(
        ledgerDeltaOperationLeafHashes,
        operationIndex,
      );
    // The machine's `transaction_commitment` — and every carriage that reveals
    // compact bytes — binds the COMMITTED source triple, i.e. the leaf under
    // the block root. For a forced transaction that leaf carries the
    // OPERATOR'S adjudicated validity scalar (§2.4.3(e)), not the submitted
    // admission claim — and not this replay's verdict: a challenger replaying
    // an operator's accepted claim to a rejection still binds the accepted
    // leaf it disputes. So the proof source is adjudicated by the committed
    // leaf's verdict (defaulting to the replayed verdict, exact on the
    // classifier path where this replay produces the leaf). No machine step
    // reads the scalar (on-chain or here) and the body bytes are untouched,
    // so the trace's decisions are unchanged; only the bound bytes move.
    // Normal sources are committed as submitted.
    const committedForcedVerdict = input.committedForcedVerdict ?? verdict;
    const proofSource =
      input.sourceKind === "forced"
        ? deriveMidgardNativeTxProofSourceV1(
            adjudicateMidgardNativeTxFullV1Validity(
              decodeMidgardNativeTxFullV1FromCanonicalCbor(
                input.canonicalTransactionCbor,
              ),
              committedForcedVerdict === "accepted"
                ? "TxIsValid"
                : "TxIsInvalid",
            ),
          )
        : deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
            input.canonicalTransactionCbor,
          );
    const compactProofTransaction = decodeMidgardNativeTxCompactV1(
      proofSource.compactCbor,
    );
    const compactProofWitnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
      proofSource.witnessSetCompactCbor,
    );
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(proofSource);
    const fieldPreimages = deriveMidgardV1TxFieldPreimages(
      input.canonicalTransactionCbor,
    );
    const machineFieldTrace = (
      fieldIndex: number,
    ): MidgardBoundedCollectionV1 =>
      countedMachineFieldTraceV1(
        fieldIndex,
        fieldPreimages[fieldIndex]!.preimageCbor,
      );
    /**
     * The §5.1 preimage every field-reading step names — the carriage plan
     * input, not a carriage (#600). One helper rather than thirteen call-site
     * expressions, because "which bytes this step read" has to be answered the
     * same way at every site; the tier those bytes travel under is decided once,
     * later, where a transaction exists.
     */
    const fieldPreimage = (fieldIndex: number): Buffer =>
      Buffer.from(fieldPreimages[fieldIndex]!.preimageCbor);
    const spendInputsCollection = machineFieldTrace(0);
    const referenceInputsCollection = machineFieldTrace(1);
    const outputsCollection = machineFieldTrace(2);
    const requiredObserversCollection = machineFieldTrace(3);
    const requiredSignersCollection = machineFieldTrace(4);
    const mintCollection = machineFieldTrace(5);
    const scriptWitnessesCollection = machineFieldTrace(
      MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX,
    );
    const addressWitnessesCollection = machineFieldTrace(
      MIDGARD_V1_ADDRESS_WITNESSES_FIELD_INDEX,
    );
    const redeemerWitnessesCollection = machineFieldTrace(8);
    const inputSetScanItems = [
      ...spendInputsCollection.items.map((item) => ({
        sourceKind: "spend" as const,
        collection: spendInputsCollection,
        item,
      })),
      ...referenceInputsCollection.items.map((item) => ({
        sourceKind: "reference" as const,
        collection: referenceInputsCollection,
        item,
      })),
    ].sort((left, right) => Buffer.compare(left.item.bytes, right.item.bytes));
    const resolutionItems = inputSetScanItems.map(({ sourceKind, item }) => ({
      sourceKind,
      key: item.bytes,
    }));
    const resolutionScheduleNodes: {
      sourceKind: "spend" | "reference";
      key: Buffer;
      nextScheduleHash: Buffer;
      scheduleHash: Buffer;
      proofCbor: Buffer;
    }[] = new Array(resolutionItems.length);
    let resolutionScheduleHash = emptyMidgardInputResolutionScheduleV1();
    for (let index = resolutionItems.length - 1; index >= 0; index -= 1) {
      const item = resolutionItems[index]!;
      const nextScheduleHash = resolutionScheduleHash;
      resolutionScheduleHash = prependMidgardInputResolutionScheduleV1({
        sourceKind: item.sourceKind,
        key: item.key,
        nextHash: nextScheduleHash,
      });
      resolutionScheduleNodes[index] = {
        ...item,
        nextScheduleHash,
        scheduleHash: resolutionScheduleHash,
        proofCbor: Buffer.alloc(0),
      };
    }
    const resolutionProofs = yield* Effect.tryPromise({
      try: async () => {
        const store = new Store(undefined);
        await store.ready();
        const trie = new Trie(store);
        for (const entry of [...input.ledgerWitnessEntries].sort(
          (left, right) => Buffer.compare(left.outRef, right.outRef),
        )) {
          const descriptorCbor = ledgerDescriptorState.get(
            entry.outRef.toString("hex"),
          );
          if (descriptorCbor === undefined) {
            throw new Error(
              "input-resolution descriptor state lost a persisted ledger entry",
            );
          }
          await trie.insert(entry.outRef, descriptorCbor);
        }
        return await Promise.all(
          resolutionScheduleNodes.map(async (node) =>
            Buffer.from(
              (
                await trie.prove(
                  node.key,
                  !ledgerDescriptorState.has(node.key.toString("hex")),
                )
              ).toCBOR(),
            ),
          ),
        );
      },
      catch: (cause) =>
        cause instanceof Error
          ? cause
          : new Error("failed to build input-resolution MPF witnesses"),
    });
    for (let index = 0; index < resolutionScheduleNodes.length; index += 1) {
      resolutionScheduleNodes[index]!.proofCbor = resolutionProofs[index]!;
    }
    const transactionContextWitnessCbor = encodeCbor([
      input.canonicalTransactionCbor,
      contextCbor,
    ]);
    const sourceContextWitnessCbor = encodeCbor([
      proofSource.compactCbor,
      proofSource.witnessSetCompactCbor,
      proofSource.fieldPreimageLengthsCbor,
      contextCbor,
    ]);
    const inputSetsWitnessCbor = (control: {
      readonly spendCount: number;
      readonly referenceCount: number;
      readonly spendSeen: number;
      readonly referenceSeen: number;
      readonly previousKey: Buffer;
      readonly resolutionScheduleHash: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(control.spendCount),
        BigInt(control.referenceCount),
        BigInt(control.spendSeen),
        BigInt(control.referenceSeen),
        control.previousKey,
        control.resolutionScheduleHash,
      ]);
    const decodeAddressWitnessItem = (
      witnessCbor: Buffer,
    ): {
      readonly verificationKey: Buffer;
      readonly signature: Buffer;
      readonly signerHash: Buffer;
    } => {
      const header = readCborArrayHeader(witnessCbor, 0, "address_witness");
      if (header.length !== 2) {
        throw new Error("address witness must contain [vkey, signature]");
      }
      const verificationKey = readCborBytes(
        witnessCbor,
        header.nextOffset,
        "address_witness.vkey",
      );
      const signature = readCborBytes(
        witnessCbor,
        verificationKey.nextOffset,
        "address_witness.signature",
      );
      if (
        verificationKey.value.length !== 32 ||
        signature.value.length !== 64 ||
        signature.nextOffset !== witnessCbor.length
      ) {
        throw new Error("address witness has a non-canonical shape");
      }
      return {
        verificationKey: verificationKey.value,
        signature: signature.value,
        signerHash: Buffer.from(blake2b(verificationKey.value, { dkLen: 28 })),
      };
    };
    const addressWitnessScanItems = addressWitnessesCollection.items
      .map((item) => {
        const decoded = decodeAddressWitnessItem(item.bytes);
        return {
          item,
          decoded,
          orderKey: Buffer.concat([
            decoded.signerHash,
            item.bytes,
            encodeCbor(BigInt(item.itemIndex)),
          ]),
        };
      })
      .sort((left, right) => Buffer.compare(left.orderKey, right.orderKey));
    const canonicalSignerHashes = addressWitnessScanItems
      .map(({ decoded }) => decoded.signerHash)
      .sort(Buffer.compare)
      .filter(
        (hash, index, hashes) =>
          index === 0 || !hash.equals(hashes[index - 1]!),
      );
    const signerLeafHashes = canonicalSignerHashes.map((signerHash) =>
      hashMidgardSignerLeafV1(signerHash),
    );
    const signerFrontier =
      buildMidgardValidationMerkleFrontierV1(signerLeafHashes);
    const signerFrontierCommitment =
      commitMidgardValidationMerkleFrontierV1(signerFrontier);
    type ScriptSourceProofEntry = {
      readonly originKind: "inline" | "reference";
      readonly sourceKey: Buffer;
      readonly script: MidgardVersionedScript;
      readonly authenticatedVersionedItemBytes: Buffer;
      readonly scriptLanguageTag: 0 | 3 | 128;
      readonly scriptHash: Buffer;
      readonly scriptTotalLength: number;
      readonly scriptItemCommitment: Buffer;
      readonly leaf: Buffer;
    };
    type ScriptPurposeProofEntry = {
      readonly purposeKind: 0 | 1 | 2 | 3;
      readonly purposeIndex: bigint;
      readonly scriptHash: Buffer;
      readonly subject: Buffer;
      readonly leaf: Buffer;
    };
    type ScriptExecutionProofEntry = {
      readonly purpose: ScriptPurposeProofEntry;
      readonly source: ScriptSourceProofEntry;
      readonly sourceIndex: number;
      readonly languageTag: 0 | 3 | 128;
      readonly redeemerLeaf: Buffer;
      readonly leaf: Buffer;
    };
    const scriptSourceEntries: ScriptSourceProofEntry[] = (
      phaseALedgerTx?.scriptWitnesses ?? []
    ).map((witness) => {
      const sourceKey = encodeCbor(BigInt(witness.index));
      const item = scriptWitnessesCollection.items[witness.index]!;
      const scriptLanguageTag =
        witness.script.language === "NativeCardano"
          ? 0
          : witness.script.language === "PlutusV3"
            ? 3
            : 128;
      const scriptHash = Buffer.from(witness.hash);
      return {
        originKind: "inline",
        sourceKey,
        script: witness.script,
        authenticatedVersionedItemBytes: Buffer.from(item.bytes),
        scriptLanguageTag,
        scriptHash,
        scriptTotalLength: item.bytes.length,
        scriptItemCommitment: item.commitment,
        leaf: hashMidgardInlineScriptSourceLeafV1({
          sourceIndex: BigInt(witness.index),
          scriptLanguageTag,
          scriptHash,
          scriptTotalLength: item.bytes.length,
          itemCommitment: item.commitment,
        }),
      };
    });
    const boundedItemForScriptSource = (source: ScriptSourceProofEntry) => {
      // The two origin kinds carry two different keys, and each has exactly one
      // decoder. An inline key is a bare canonical CBOR index; a reference key
      // *is* the ledger out-ref, i.e. §5.3's fixed-index 38-byte item, whose
      // `19 0000` head a minimal-CBOR reader rejects — so it goes through the
      // §5.3 twin, never `decodeSingleCbor`. See `docs/spec/midgard-tx.md` §5.3.
      const itemIndexValue =
        source.originKind === "inline"
          ? decodeSingleCbor(source.sourceKey)
          : decodeMidgardSpendInputItemV1(source.sourceKey).outputIndex;
      const itemIndex =
        typeof itemIndexValue === "number"
          ? itemIndexValue
          : typeof itemIndexValue === "bigint" &&
              itemIndexValue <= BigInt(Number.MAX_SAFE_INTEGER)
            ? Number(itemIndexValue)
            : -1;
      if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
        throw new Error("V1 script source has a noncanonical item index");
      }
      const item = buildMidgardBoundedItemV1({
        fieldIndex:
          source.originKind === "inline"
            ? MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX
            : 2,
        itemIndex,
        bytes: source.authenticatedVersionedItemBytes,
      });
      if (
        item.bytes.length !== source.scriptTotalLength ||
        !item.commitment.equals(source.scriptItemCommitment)
      ) {
        throw new Error(
          "V1 script source bytes disagree with authenticated descriptor facts",
        );
      }
      return item;
    };
    const inlineScriptSourceLeafHashes = scriptSourceEntries.map(
      (entry) => entry.leaf,
    );
    const scriptPurposeEntries: ScriptPurposeProofEntry[] = [];
    const scriptExecutionEntries: ScriptExecutionProofEntry[] = [];
    const inlineScriptSourceFrontier = buildMidgardValidationMerkleFrontierV1(
      inlineScriptSourceLeafHashes,
    );
    const outputCbors = decodeMidgardNativeByteListPreimage(
      fieldPreimages[2]!.preimageCbor,
      "v1.outputs",
    );
    const outputLeafHashes = outputCbors.map((outputCbor, outputIndex) =>
      hashMidgardOutputLeafV1({ outputIndex, outputCbor }),
    );
    const outputFrontier =
      buildMidgardValidationMerkleFrontierV1(outputLeafHashes);
    const outputMembership = (outputIndex: number) =>
      buildMidgardValidationMerkleMembershipV1(outputLeafHashes, outputIndex);
    const admittedOutputDescriptorCbors: Buffer[] = [];
    const admittedOutputDescriptorLeafHashes: Buffer[] = [];
    const decodedProofRedeemers = decodeMidgardRedeemers(
      fieldPreimages[8]!.preimageCbor,
    );
    const canonicalRedeemerWitnessCbors = decodedProofRedeemers.map(
      (redeemer) =>
        encodeCbor([
          BigInt(redeemer.tag),
          redeemer.index,
          Buffer.from(redeemer.dataCborHex, "hex"),
          [redeemer.exUnits.memory, redeemer.exUnits.steps],
        ]),
    );
    const redeemerLeafHashes = canonicalRedeemerWitnessCbors.map(
      (canonicalRedeemerWitnessCbor, redeemerIndex) =>
        hashMidgardRedeemerLeafV1({
          redeemerIndex,
          canonicalRedeemerWitnessCbor,
        }),
    );
    const redeemerFrontier =
      buildMidgardValidationMerkleFrontierV1(redeemerLeafHashes);
    const encodeFrontierPeaks = encodeValidationFrontierPeaksV1;
    const emptyValidationFrontier = buildMidgardValidationMerkleFrontierV1([]);
    type SignatureScanControl = {
      readonly stage: 0 | 1 | 2;
      readonly addressCount: number;
      readonly requiredCount: number;
      readonly addressSeen: number;
      readonly requiredSeen: number;
      readonly previousOrderKey: Buffer;
      readonly previousSignerHash: Buffer;
      readonly signerFrontier: MidgardValidationMerkleFrontierV1;
      readonly invalidSignatureSeen: 0 | 1;
    };
    const signaturesScanWitnessCbor = (control: SignatureScanControl): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.addressCount),
        BigInt(control.requiredCount),
        BigInt(control.addressSeen),
        BigInt(control.requiredSeen),
        control.previousOrderKey,
        control.previousSignerHash,
        BigInt(control.signerFrontier.count),
        encodeFrontierPeaks(control.signerFrontier),
        BigInt(control.invalidSignatureSeen),
      ]);
    const initialSignatureScanControl: SignatureScanControl = {
      stage: 0,
      addressCount: addressWitnessesCollection.items.length === 0 ? 0 : -1,
      requiredCount: requiredSignersCollection.items.length === 0 ? 0 : -1,
      addressSeen: 0,
      requiredSeen: 0,
      previousOrderKey: Buffer.alloc(0),
      previousSignerHash: Buffer.alloc(0),
      signerFrontier: emptyValidationFrontier,
      invalidSignatureSeen: 0,
    };
    type PhaseANativeScriptsScanControl = {
      readonly stage: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8;
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
      readonly itemLength: number;
      readonly itemCommitment: Buffer;
      readonly cursor: number;
      readonly stackRoot: Buffer;
      readonly stackDepth: number;
      readonly nodeCount: number;
      readonly result: -1 | 0 | 1;
    };
    const phaseANativeScriptsScanWitnessCbor = (
      control: PhaseANativeScriptsScanControl,
      continuationCbor: Buffer = Buffer.alloc(0),
    ): Buffer =>
      encodeValidationControlListV1([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.scriptCount),
        BigInt(control.scriptSeen),
        BigInt(control.containsNonNativeScript),
        BigInt(control.itemLength),
        control.itemCommitment,
        BigInt(control.cursor),
        control.stackRoot,
        BigInt(control.stackDepth),
        BigInt(control.nodeCount),
        BigInt(control.result),
        BigInt(signerFrontier.count),
        [encodeFrontierPeaks(signerFrontier), continuationCbor],
      ]);
    const resetPhaseANativeScriptsScanControl = (input: {
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
    }): PhaseANativeScriptsScanControl => ({
      stage: 0,
      scriptCount: input.scriptCount,
      scriptSeen: input.scriptSeen,
      containsNonNativeScript: input.containsNonNativeScript,
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      cursor: 0,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: 0,
      result: -1,
    });
    const initialPhaseANativeScriptsScanControl =
      resetPhaseANativeScriptsScanControl({
        scriptCount: scriptWitnessesCollection.items.length === 0 ? 0 : -1,
        scriptSeen: 0,
        containsNonNativeScript: 0,
      });
    let resolvedItemFrontier = emptyValidationFrontier;
    type MintFoldTraceControl = {
      readonly policyCount: number;
      readonly policyCursor: number;
      readonly previousPolicy: Buffer;
      readonly activePolicy: Buffer;
      readonly itemLength: number;
      readonly itemCommitment: Buffer;
      readonly itemCursor: number;
      readonly assetsRemaining: number;
      readonly policyAssetCursor: number;
      readonly previousAsset: Buffer;
      readonly assetFrontier: MidgardValidationMerkleFrontierV1;
    };
    const emptyMintFoldControl: MintFoldTraceControl = {
      policyCount: -1,
      policyCursor: 0,
      previousPolicy: Buffer.alloc(0),
      activePolicy: Buffer.alloc(0),
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      itemCursor: 0,
      assetsRemaining: 0,
      policyAssetCursor: 0,
      previousAsset: Buffer.alloc(0),
      assetFrontier: emptyValidationFrontier,
    };
    let mintFoldControl = emptyMintFoldControl;
    const encodeMintFoldControl = (
      control: MintFoldTraceControl,
    ): readonly unknown[] => [
      BigInt(control.policyCount),
      BigInt(control.policyCursor),
      control.previousPolicy,
      control.activePolicy,
      BigInt(control.itemLength),
      control.itemCommitment,
      BigInt(control.itemCursor),
      BigInt(control.assetsRemaining),
      BigInt(control.policyAssetCursor),
      control.previousAsset,
      BigInt(control.assetFrontier.count),
      encodeFrontierPeaks(control.assetFrontier),
    ];
    const emptyScriptDiscoveryControl: ScriptDiscoveryTraceControlV1 = {
      purposeCursor: 0,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: -1,
      currentPurposeIndex: -1n,
      currentScriptHash: Buffer.alloc(0),
      currentSubject: Buffer.alloc(0),
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
      usedInlineBitmap: 0n,
      usedRedeemerBitmap: 0n,
      executionFrontier: emptyValidationFrontier,
      redeemerItemControlHash: Buffer.alloc(0),
    };
    const scriptDiscoveryControlCbor = encodeScriptDiscoveryControlCborV1;
    const scriptSourcesWitnessCbor = (input: {
      readonly resolvedInputCount: number;
      readonly resolvedInputsAccumulator: Buffer;
      readonly stage: number;
      readonly sourceFrontier: MidgardValidationMerkleFrontierV1;
      readonly redeemerFrontier: MidgardValidationMerkleFrontierV1;
      readonly replayCursor?: number;
      readonly replayAccumulator?: Buffer;
      readonly replayRemainingScheduleHash?: Buffer;
      readonly spendIndex?: number;
      readonly purposeFrontier?: MidgardValidationMerkleFrontierV1;
      readonly outputCursor?: number;
      readonly outputFrontier?: MidgardValidationMerkleFrontierV1;
      readonly outputTotalCount?: number;
      readonly receiveScan?: {
        readonly sourceFrontier: MidgardValidationMerkleFrontierV1;
        readonly receiveCount: number;
        readonly previousHash: Buffer;
        readonly candidateHash: Buffer;
        readonly descriptorFrontier: MidgardValidationMerkleFrontierV1;
      };
      readonly sourceTotalCount?: number;
      readonly redeemerTotalCount?: number;
      readonly observerScan?: {
        readonly totalCount: number;
        readonly seen: number;
        readonly previousHash: Buffer;
      };
      readonly outputProof?: MidgardLedgerOutputProofControlV1 | null;
      readonly discovery?: ScriptDiscoveryTraceControlV1;
      readonly pendingSource?: {
        readonly sourceIndex: number;
        readonly sourceTotalCount: number;
        readonly languageTag: 0 | 3 | 128;
        readonly payloadOffset: number;
        readonly payloadLength: number;
        readonly itemLength: number;
        readonly itemCommitment: Buffer;
        readonly hashControl: MidgardBlake2b224TraceControlV1;
      } | null;
      readonly redeemerItemControlHash?: Buffer;
    }): Buffer => {
      const observerScan = input.observerScan ?? {
        totalCount: 0,
        seen: 0,
        previousHash: Buffer.alloc(0),
      };
      const receiveScan = input.receiveScan ?? {
        sourceFrontier: emptyValidationFrontier,
        receiveCount: 0,
        previousHash: Buffer.alloc(0),
        candidateHash: Buffer.alloc(0),
        descriptorFrontier: emptyValidationFrontier,
      };
      const fields: unknown[] = [
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(input.resolvedInputCount),
        input.resolvedInputsAccumulator,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        encodeFrontierPeaks(resolvedItemFrontier),
        BigInt(input.stage),
        BigInt(input.sourceFrontier.count),
        encodeFrontierPeaks(input.sourceFrontier),
        BigInt(input.redeemerFrontier.count),
        encodeFrontierPeaks(input.redeemerFrontier),
        BigInt(input.replayCursor ?? 0),
        input.replayAccumulator ?? initialMidgardResolvedInputsAccumulatorV1(),
        input.replayRemainingScheduleHash ??
          emptyMidgardInputResolutionScheduleV1(),
        BigInt(input.spendIndex ?? 0),
        BigInt(input.purposeFrontier?.count ?? 0),
        encodeFrontierPeaks(input.purposeFrontier ?? emptyValidationFrontier),
        BigInt(input.outputCursor ?? 0),
        BigInt(input.outputFrontier?.count ?? 0),
        encodeFrontierPeaks(input.outputFrontier ?? emptyValidationFrontier),
        BigInt(input.outputTotalCount ?? input.outputFrontier?.count ?? 0),
        [
          BigInt(receiveScan.sourceFrontier.count),
          encodeFrontierPeaks(receiveScan.sourceFrontier),
          BigInt(receiveScan.receiveCount),
          receiveScan.previousHash,
          receiveScan.candidateHash,
          encodeFrontierPeaks(receiveScan.descriptorFrontier),
        ],
        BigInt(input.sourceTotalCount ?? input.sourceFrontier.count),
        BigInt(input.redeemerTotalCount ?? input.redeemerFrontier.count),
        [
          BigInt(observerScan.totalCount),
          observerScan.previousHash,
          BigInt(observerScan.seen),
        ],
        encodeMintFoldControl(mintFoldControl),
        resolutionScheduleHash,
      ];
      if (
        input.stage === 0 &&
        input.pendingSource !== undefined &&
        input.pendingSource !== null
      ) {
        fields.push(
          encodeCbor([
            1n,
            BigInt(input.pendingSource.sourceIndex),
            BigInt(input.pendingSource.sourceTotalCount),
            BigInt(input.pendingSource.languageTag),
            BigInt(input.pendingSource.payloadOffset),
            BigInt(input.pendingSource.payloadLength),
            BigInt(input.pendingSource.itemLength),
            input.pendingSource.itemCommitment,
            encodeMidgardBlake2b224TraceControlV1(
              input.pendingSource.hashControl,
            ),
          ]),
        );
      } else if (
        input.stage === 1 &&
        input.redeemerItemControlHash !== undefined &&
        input.redeemerItemControlHash.length > 0
      ) {
        fields.push(input.redeemerItemControlHash);
      } else if (
        input.stage === 5 &&
        input.outputProof !== undefined &&
        input.outputProof !== null
      ) {
        fields.push(encodeMidgardLedgerOutputProofControlV1(input.outputProof));
      } else if (input.stage >= 8) {
        fields.push(
          scriptDiscoveryControlCbor(
            input.discovery ?? emptyScriptDiscoveryControl,
          ),
        );
      }
      return encodeCbor(fields);
    };
    const signerMembership = (signerIndex: number) =>
      buildMidgardValidationMerkleMembershipV1(signerLeafHashes, signerIndex);
    const signerProofForHash = (
      signerHash: Buffer,
    ): ValidationMachineSignerSetProof => {
      const insertionIndex = canonicalSignerHashes.findIndex(
        (candidate) => Buffer.compare(candidate, signerHash) >= 0,
      );
      if (
        insertionIndex >= 0 &&
        canonicalSignerHashes[insertionIndex]!.equals(signerHash)
      ) {
        return {
          kind: "membership",
          frontier: signerFrontier,
          signerIndex: insertionIndex,
          siblings: signerMembership(insertionIndex).siblings,
        };
      }
      if (canonicalSignerHashes.length === 0) {
        return { kind: "empty", frontier: signerFrontier };
      }
      if (insertionIndex === 0) {
        return {
          kind: "belowFirst",
          frontier: signerFrontier,
          firstSignerHash: canonicalSignerHashes[0]!,
          siblings: signerMembership(0).siblings,
        };
      }
      if (insertionIndex === -1) {
        const lastIndex = canonicalSignerHashes.length - 1;
        return {
          kind: "aboveLast",
          frontier: signerFrontier,
          lastSignerHash: canonicalSignerHashes[lastIndex]!,
          siblings: signerMembership(lastIndex).siblings,
        };
      }
      return {
        kind: "between",
        frontier: signerFrontier,
        lowerIndex: insertionIndex - 1,
        lowerSignerHash: canonicalSignerHashes[insertionIndex - 1]!,
        lowerSiblings: signerMembership(insertionIndex - 1).siblings,
        upperSignerHash: canonicalSignerHashes[insertionIndex]!,
        upperSiblings: signerMembership(insertionIndex).siblings,
      };
    };
    const signerSetProof = (
      sourceKind: "spend" | "reference",
      value: Buffer | null,
    ): ValidationMachineSignerSetProof => {
      if (sourceKind === "reference" || value === null) {
        return { kind: "none" };
      }
      let signerHash: Buffer;
      try {
        const output = decodeMidgardTxOutput(value);
        const credential = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;
        if (credential.kind === "Script") return { kind: "none" };
        signerHash = Buffer.from(credential.hash);
      } catch {
        return { kind: "none" };
      }
      return signerProofForHash(signerHash);
    };
    const protectedOutputSignerProof = (
      outputCbor: Buffer,
    ): ValidationMachineSignerSetProof => {
      const output = decodeMidgardTxOutput(outputCbor);
      const address = decodeMidgardAddressBytes(output.address);
      if (!address.protected || address.paymentCredential.kind === "Script") {
        return { kind: "none" };
      }
      return signerProofForHash(Buffer.from(address.paymentCredential.hash));
    };
    const phaseAScriptPreconditionsWitnessCbor = (control: {
      readonly containsNonNativeScript: 0 | 1;
      readonly observerCount: number;
      readonly observerSeen: number;
      readonly previousObserver: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        BigInt(control.containsNonNativeScript),
        control.previousObserver,
        BigInt(control.observerCount),
        BigInt(control.observerSeen),
      ]);
    const macroWitnessByPhase = new Map<MidgardValidationPhaseName, Buffer>([
      [
        "compactBinding",
        encodeCbor([
          input.transactionId,
          transactionCommitment,
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
        ]),
      ],
      ["staticLedgerRules", sourceContextWitnessCbor],
      ["valueAndMint", transactionContextWitnessCbor],
      ["nativeScripts", transactionContextWitnessCbor],
      ["scriptIntegrity", transactionContextWitnessCbor],
      ["cek", transactionContextWitnessCbor],
      ["ledgerDelta", transactionContextWitnessCbor],
    ]);
    const macroAuxiliaryByPhase = new Map<
      MidgardValidationPhaseName,
      ValidationMachineWorkWitness["auxiliary"]
    >([]);

    const terminalPhase =
      rejection === null ? "ledgerDelta" : rejectionPhase(rejection);
    const stopIndex = orderedPhases.indexOf(terminalPhase);
    if (stopIndex < 0) {
      return yield* Effect.fail(
        new Error(`unknown validation terminal phase ${terminalPhase}`),
      );
    }
    const witnesses: ValidationMachineWorkWitness[] = [];
    const witnessExecutionBudgets: {
      readonly cpu: bigint;
      readonly memory: bigint;
    }[] = [];
    let traceExecutionCpu = 0n;
    let traceExecutionMemory = 0n;
    const pushWitness = (
      phase: MidgardValidationPhaseName,
      cbor: Buffer,
      auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
    ): void => {
      witnesses.push({
        phase,
        programCounter: witnesses.length,
        cbor,
        auxiliary,
      });
      witnessExecutionBudgets.push({
        cpu: traceExecutionCpu,
        memory: traceExecutionMemory,
      });
    };
    const macroWitness = (phase: MidgardValidationPhaseName): Buffer => {
      const witness = macroWitnessByPhase.get(phase);
      if (witness === undefined) {
        throw new Error(`missing macro witness for ${phase}`);
      }
      return witness;
    };
    const macroAuxiliary = (
      phase: MidgardValidationPhaseName,
    ): ValidationMachineWorkWitness["auxiliary"] =>
      macroAuxiliaryByPhase.get(phase) ?? null;

    let stoppedAtRejection = false;
    let authenticatedNativeScriptsWitnessCbor: Buffer | null = null;
    let authenticatedNativeScriptsBaseFields: unknown[] | null = null;
    for (const field of fieldPreimages) {
      const collection = countedMachineFieldTraceV1(
        field.fieldIndex,
        field.preimageCbor,
      );
      if (collection.items.length === 0) {
        pushWitness(
          "canonicalDecode",
          encodeCbor([
            proofSource.compactCbor,
            proofSource.witnessSetCompactCbor,
            proofSource.fieldPreimageLengthsCbor,
            contextCbor,
            BigInt(field.fieldIndex),
            0n,
            0n,
            -1n,
            0n,
          ]),
        );
        continue;
      }
      let itemCount = -1;
      let encodedLength = 0;
      for (const item of collection.items) {
        if (
          item.bytes.length <=
          MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes
        ) {
          pushWitness(
            "canonicalDecode",
            encodeCbor([
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(field.fieldIndex),
              BigInt(item.itemIndex),
              0n,
              BigInt(itemCount),
              BigInt(encodedLength),
            ]),
            {
              kind: "transactionFieldItem",
              fieldIndex: field.fieldIndex,
              fieldPreimage: fieldPreimage(field.fieldIndex),
            },
          );
          if (itemCount === -1) {
            itemCount = collection.items.length;
            encodedLength = canonicalCborArgumentHeaderSize(itemCount);
          }
          encodedLength += canonicalFieldItemEncodedLength(
            field.fieldIndex,
            item.bytes.length,
          );
          continue;
        }
        const chunkCount = midgardBoundedItemChunkCountV1(item.bytes.length);
        for (let chunkIndex = 0; chunkIndex < chunkCount; chunkIndex += 1) {
          pushWitness(
            "canonicalDecode",
            encodeCbor([
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(field.fieldIndex),
              BigInt(item.itemIndex),
              BigInt(chunkIndex),
              BigInt(itemCount),
              BigInt(encodedLength),
            ]),
            {
              kind: "transactionFieldChunk",
              fieldIndex: field.fieldIndex,
              itemIndex: item.itemIndex,
              fieldPreimage: fieldPreimage(field.fieldIndex),
            },
          );
          if (itemCount === -1) {
            itemCount = collection.items.length;
            encodedLength = canonicalCborArgumentHeaderSize(itemCount);
          }
          if (chunkIndex + 1 === chunkCount) {
            encodedLength += canonicalFieldItemEncodedLength(
              field.fieldIndex,
              item.bytes.length,
            );
          }
        }
      }
    }
    if (rejection !== null && terminalPhase === "canonicalDecode") {
      return yield* Effect.fail(
        new Error(
          `V1 canonical rejection ${rejection.code} is not representable by the bounded canonical source`,
        ),
      );
    }
    for (const phase of ["compactBinding", "staticLedgerRules"] as const) {
      if (stoppedAtRejection) break;
      pushWitness(phase, macroWitness(phase), macroAuxiliary(phase));
      if (rejection !== null && phase === terminalPhase) {
        stoppedAtRejection = true;
        break;
      }
    }

    if (!stoppedAtRejection) {
      let spendCount = spendInputsCollection.items.length === 0 ? 0 : -1;
      let referenceCount =
        referenceInputsCollection.items.length === 0 ? 0 : -1;
      let spendSeen = 0;
      let referenceSeen = 0;
      let previousKey = Buffer.alloc(0);
      let inputScheduleHash = emptyMidgardInputResolutionScheduleV1();
      const currentInputSetsWitness = (): Buffer =>
        inputSetsWitnessCbor({
          spendCount,
          referenceCount,
          spendSeen,
          referenceSeen,
          previousKey,
          resolutionScheduleHash: inputScheduleHash,
        });

      if (spendCount === 0) {
        pushWitness("inputSets", currentInputSetsWitness());
        if (
          terminalPhase !== "inputSets" ||
          rejectionCode !== RejectCodes.EmptyInputs
        ) {
          return yield* Effect.fail(
            new Error(
              `bounded input scan found no spend inputs but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
            ),
          );
        }
        stoppedAtRejection = true;
      } else {
        for (let index = inputSetScanItems.length - 1; index >= 0; index -= 1) {
          const scan = inputSetScanItems[index]!;
          const key = scan.item.bytes;
          pushWitness("inputSets", currentInputSetsWitness(), {
            kind: "transactionFieldChunk",
            // `inputSets` is one of the two phases that read more than one slot
            // — fields 0 and 1, alternating — so the index comes off the scan's
            // own collection rather than a literal.
            fieldIndex: scan.collection.fieldIndex,
            itemIndex: scan.item.itemIndex,
            fieldPreimage: fieldPreimage(scan.collection.fieldIndex),
          });
          if (previousKey.length > 0 && key.equals(previousKey)) {
            if (
              terminalPhase !== "inputSets" ||
              rejectionCode !== RejectCodes.DuplicateInputInTx
            ) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan found a duplicate but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (previousKey.length > 0 && Buffer.compare(key, previousKey) >= 0) {
            return yield* Effect.fail(
              new Error("bounded input scan is not strictly descending"),
            );
          }
          if (scan.sourceKind === "spend") {
            if (spendCount === -1) {
              spendCount = scan.collection.items.length;
            }
            spendSeen += 1;
          } else {
            if (referenceCount === -1) {
              referenceCount = scan.collection.items.length;
            }
            referenceSeen += 1;
          }
          previousKey = key;
          inputScheduleHash = prependMidgardInputResolutionScheduleV1({
            sourceKind: scan.sourceKind,
            key,
            nextHash: inputScheduleHash,
          });
        }
        if (!stoppedAtRejection) {
          if (spendCount <= 0 || referenceCount < 0) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal both input counts"),
            );
          }
          if (spendSeen !== spendCount || referenceSeen !== referenceCount) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal every input"),
            );
          }
          if (!inputScheduleHash.equals(resolutionScheduleHash)) {
            return yield* Effect.fail(
              new Error(
                `bounded input scan schedule ${inputScheduleHash.toString("hex")} differs from committed ${resolutionScheduleHash.toString("hex")}`,
              ),
            );
          }
          if (terminalPhase === "inputSets") {
            if (rejectionCode !== RejectCodes.InvalidValidityIntervalFormat) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan cannot prove rejection ${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
          }
        }
      }
    }

    if (!stoppedAtRejection) {
      let signatureControl = initialSignatureScanControl;
      const pushSignatureWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "signatures",
          signaturesScanWitnessCbor(signatureControl),
          auxiliary,
        );
      };
      if (signatureControl.addressCount === 0) {
        pushSignatureWitness();
        signatureControl = { ...signatureControl, stage: 1 };
      } else {
        for (
          let index = 0;
          index < addressWitnessScanItems.length;
          index += 1
        ) {
          const scan = addressWitnessScanItems[index]!;
          pushSignatureWitness({
            kind: "transactionFieldChunk",
            fieldIndex: MIDGARD_V1_ADDRESS_WITNESSES_FIELD_INDEX,
            itemIndex: scan.item.itemIndex,
            fieldPreimage: fieldPreimage(
              MIDGARD_V1_ADDRESS_WITNESSES_FIELD_INDEX,
            ),
          });
          if (
            signatureControl.previousOrderKey.length > 0 &&
            Buffer.compare(signatureControl.previousOrderKey, scan.orderKey) >=
              0
          ) {
            return yield* Effect.fail(
              new Error("address-witness scan is not strictly ordered"),
            );
          }
          const newSigner = !scan.decoded.signerHash.equals(
            signatureControl.previousSignerHash,
          );
          const signerFrontier = newSigner
            ? appendMidgardValidationMerkleLeafV1(
                signatureControl.signerFrontier,
                hashMidgardSignerLeafV1(scan.decoded.signerHash),
              )
            : signatureControl.signerFrontier;
          let signatureIsValid = false;
          try {
            const publicKey = CML.PublicKey.from_bytes(
              scan.decoded.verificationKey,
            );
            const signature = CML.Ed25519Signature.from_raw_bytes(
              scan.decoded.signature,
            );
            try {
              signatureIsValid = publicKey.verify(
                input.transactionId,
                signature,
              );
            } finally {
              publicKey.free();
              signature.free();
            }
          } catch {
            signatureIsValid = false;
          }
          const addressSeen = signatureControl.addressSeen + 1;
          const addressCount =
            signatureControl.addressCount === -1
              ? addressWitnessesCollection.items.length
              : signatureControl.addressCount;
          signatureControl =
            addressSeen === addressCount
              ? {
                  ...signatureControl,
                  stage: 1,
                  addressCount,
                  addressSeen,
                  previousOrderKey: Buffer.alloc(0),
                  previousSignerHash: Buffer.alloc(0),
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                }
              : {
                  ...signatureControl,
                  addressCount,
                  addressSeen,
                  previousOrderKey: scan.orderKey,
                  previousSignerHash: scan.decoded.signerHash,
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                };
        }
      }
      if (signatureControl.stage !== 1) {
        return yield* Effect.fail(
          new Error("address-witness scan did not reach required signers"),
        );
      }
      if (signatureControl.requiredCount === 0) {
        pushSignatureWitness();
        if (signatureControl.invalidSignatureSeen === 1) {
          if (
            terminalPhase !== "signatures" ||
            rejectionCode !== RejectCodes.InvalidSignature
          ) {
            return yield* Effect.fail(
              new Error(
                `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
              ),
            );
          }
          stoppedAtRejection = true;
        } else {
          signatureControl = { ...signatureControl, stage: 2 };
        }
      } else {
        for (
          let index = 0;
          index < requiredSignersCollection.items.length;
          index += 1
        ) {
          const item = requiredSignersCollection.items[index]!;
          const signerProof = signerProofForHash(item.bytes);
          pushSignatureWitness({
            kind: "requiredSignerItem",
            // No field or item index on the wire: the field is 4 by
            // construction and the item index is `control.required_seen`.
            fieldIndex: 4,
            fieldPreimage: fieldPreimage(4),
            signerProof,
          });
          if (signerProof.kind !== "membership") {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.MissingRequiredWitness
            ) {
              return yield* Effect.fail(
                new Error(
                  `required signer is absent but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          const requiredSeen = signatureControl.requiredSeen + 1;
          const requiredCount =
            signatureControl.requiredCount === -1
              ? requiredSignersCollection.items.length
              : signatureControl.requiredCount;
          signatureControl = {
            ...signatureControl,
            requiredCount,
            requiredSeen,
          };
          if (
            requiredSeen === requiredCount &&
            signatureControl.invalidSignatureSeen === 1
          ) {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.InvalidSignature
            ) {
              return yield* Effect.fail(
                new Error(
                  `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (requiredSeen === requiredCount) {
            signatureControl = { ...signatureControl, stage: 2 };
          }
        }
      }
      if (!stoppedAtRejection) {
        if (signatureControl.stage !== 2) {
          return yield* Effect.fail(
            new Error("required-signer scan did not reach its handoff"),
          );
        }
        pushSignatureWitness();
        if (terminalPhase === "signatures") {
          return yield* Effect.fail(
            new Error(
              `signature scan cannot prove rejection ${rejectionCode ?? "none"}`,
            ),
          );
        }
      }
    }

    let phaseANativeControl = initialPhaseANativeScriptsScanControl;
    if (!stoppedAtRejection && rawExecutionProjection === null) {
      const nativeScriptFrames: ValidationMachineNativeScriptFrameV1[] = [];
      const expectedPhaseANativeRejection = (code: RejectCode): boolean =>
        rejection !== null &&
        terminalPhase === "phaseANativeScripts" &&
        rejectionCode === code;
      const failUnexpectedPhaseANativeRejection = (
        actual: RejectCode,
      ): Effect.Effect<never, Error> =>
        Effect.fail(
          new Error(
            `bounded native-script scan found ${actual} at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor} but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
          ),
        );
      const pushPhaseANativeWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "phaseANativeScripts",
          phaseANativeScriptsScanWitnessCbor(phaseANativeControl),
          auxiliary,
        );
      };

      if (phaseANativeControl.scriptCount === 0) {
        pushPhaseANativeWitness();
      } else {
        for (const item of scriptWitnessesCollection.items) {
          const activeScriptCount =
            phaseANativeControl.scriptCount === -1
              ? scriptWitnessesCollection.items.length
              : phaseANativeControl.scriptCount;
          pushPhaseANativeWitness({
            kind: "transactionFieldChunk",
            fieldIndex: MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX,
            itemIndex: item.itemIndex,
            fieldPreimage: fieldPreimage(
              MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX,
            ),
          });

          let header: ValidationMachineVersionedScriptHeaderV1;
          try {
            header = readValidationMachineVersionedScriptHeaderV1(item.bytes);
          } catch {
            if (!expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)) {
              return yield* failUnexpectedPhaseANativeRejection(
                RejectCodes.InvalidFieldType,
              );
            }
            stoppedAtRejection = true;
            break;
          }

          if (header.languageTag !== 0) {
            phaseANativeControl = resetPhaseANativeScriptsScanControl({
              scriptCount: activeScriptCount,
              scriptSeen: phaseANativeControl.scriptSeen + 1,
              containsNonNativeScript: 1,
            });
            continue;
          }

          phaseANativeControl = {
            ...phaseANativeControl,
            stage: 1,
            scriptCount: activeScriptCount,
            itemLength: item.bytes.length,
            itemCommitment: item.commitment,
            cursor: header.payloadOffset,
          };
          while (!stoppedAtRejection) {
            if (phaseANativeControl.stage === 1) {
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor /
                  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let head: ValidationMachineNativeScriptTokenHeadV1 | null = null;
              try {
                head = readValidationMachineNativeScriptTokenHeadV1(
                  item.bytes,
                  phaseANativeControl.cursor,
                );
              } catch {
                // The authenticated token witness still proves the exact
                // malformed bytes to the one-step resolver.
              }
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(item, chunkIndex + 1)
                    : null,
                signerProof: { kind: "none" },
              });
              if (head === null) {
                if (
                  !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              const nextNodeCount = phaseANativeControl.nodeCount + 1;
              if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
                if (
                  !expectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  )
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              phaseANativeControl = {
                ...phaseANativeControl,
                stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                cursor: head.payloadOffset,
                nodeCount: nextNodeCount,
              };
              continue;
            }

            if (phaseANativeControl.stage >= 3) {
              const kind = (phaseANativeControl.stage - 3) as
                | 0
                | 1
                | 2
                | 3
                | 4
                | 5;
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor /
                  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let token: ValidationMachineNativeScriptTokenV1 | null = null;
              let payloadParseFailure = "none";
              try {
                token = readValidationMachineNativeScriptPayloadV1(
                  item.bytes,
                  phaseANativeControl.cursor,
                  kind,
                );
              } catch (cause) {
                payloadParseFailure = String(cause);
                // The authenticated payload witness proves the exact
                // malformed bytes to the selected one-step resolver.
              }
              const signerProof =
                token?.kind === 0
                  ? signerProofForHash(token.keyHash)
                  : ({ kind: "none" } as const);
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(item, chunkIndex + 1)
                    : null,
                signerProof,
              });
              if (token === null) {
                if (
                  !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
                ) {
                  return yield* Effect.fail(
                    new Error(
                      `bounded native-script payload failed at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor},bytes=${item.bytes.subarray(phaseANativeControl.cursor).toString("hex")}: ${payloadParseFailure}`,
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              if (token.kind >= 1 && token.kind <= 3 && token.childCount > 0) {
                const nextDepth = phaseANativeControl.stackDepth + 1;
                if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH_V1) {
                  if (
                    !expectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    )
                  ) {
                    return yield* failUnexpectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    );
                  }
                  stoppedAtRejection = true;
                  break;
                }
                const frame: ValidationMachineNativeScriptFrameV1 = {
                  tail: phaseANativeControl.stackRoot,
                  kind: token.kind as 1 | 2 | 3,
                  childCount: token.childCount,
                  remaining: token.childCount,
                  validCount: 0,
                  required: token.required,
                };
                nativeScriptFrames.push(frame);
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  cursor: token.nextOffset,
                  stackRoot: hashValidationMachineNativeScriptFrameV1(frame),
                  stackDepth: nextDepth,
                };
                continue;
              }

              let valid: boolean;
              if (token.kind === 0) {
                valid = signerProof.kind === "membership";
              } else if (token.kind === 4) {
                valid =
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= 0n &&
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= token.slot;
              } else if (token.kind === 5) {
                valid =
                  compactProofTransaction.transactionBody.validityIntervalEnd >=
                    0n &&
                  compactProofTransaction.transactionBody.validityIntervalEnd <=
                    token.slot;
              } else if (token.kind === 1) {
                valid = true;
              } else if (token.kind === 2) {
                valid = false;
              } else {
                valid = token.required === 0n;
              }
              phaseANativeControl = {
                ...phaseANativeControl,
                stage: 2,
                cursor: token.nextOffset,
                result: valid ? 1 : 0,
              };
              continue;
            }

            const frame = nativeScriptFrames[nativeScriptFrames.length - 1];
            if (frame !== undefined) {
              pushPhaseANativeWitness({
                kind: "nativeScriptFrame",
                frame,
              });
              const validCount =
                frame.validCount + (phaseANativeControl.result === 1 ? 1 : 0);
              if (frame.remaining === 1) {
                nativeScriptFrames.pop();
                const valid =
                  frame.kind === 1
                    ? validCount === frame.childCount
                    : frame.kind === 2
                      ? validCount > 0
                      : BigInt(validCount) >= frame.required;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stackRoot: frame.tail,
                  stackDepth: phaseANativeControl.stackDepth - 1,
                  result: valid ? 1 : 0,
                };
              } else {
                const nextFrame: ValidationMachineNativeScriptFrameV1 = {
                  ...frame,
                  remaining: frame.remaining - 1,
                  validCount,
                };
                nativeScriptFrames[nativeScriptFrames.length - 1] = nextFrame;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  stackRoot:
                    hashValidationMachineNativeScriptFrameV1(nextFrame),
                  result: -1,
                };
              }
              continue;
            }

            pushPhaseANativeWitness();
            if (phaseANativeControl.cursor !== phaseANativeControl.itemLength) {
              if (
                !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.InvalidFieldType,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (phaseANativeControl.result === 0) {
              if (
                !expectedPhaseANativeRejection(RejectCodes.NativeScriptInvalid)
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.NativeScriptInvalid,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            phaseANativeControl = resetPhaseANativeScriptsScanControl({
              scriptCount: activeScriptCount,
              scriptSeen: phaseANativeControl.scriptSeen + 1,
              containsNonNativeScript:
                phaseANativeControl.containsNonNativeScript,
            });
            break;
          }
          if (stoppedAtRejection) break;
        }
      }

      if (!stoppedAtRejection && terminalPhase === "phaseANativeScripts") {
        return yield* Effect.fail(
          new Error(
            `bounded native-script scan cannot prove rejection ${rejectionCode ?? "none"}`,
          ),
        );
      }
    }

    if (!stoppedAtRejection && rawExecutionProjection !== null) {
      phaseANativeControl = resetPhaseANativeScriptsScanControl({
        scriptCount: rawExecutionProjection.scriptWitnesses.length,
        scriptSeen: rawExecutionProjection.scriptWitnesses.length,
        containsNonNativeScript: rawExecutionProjection.scriptWitnesses.some(
          ({ languageTag }) => languageTag !== 0,
        )
          ? 1
          : 0,
      });
      pushWitness(
        "phaseANativeScripts",
        phaseANativeScriptsScanWitnessCbor(phaseANativeControl),
      );
    }

    if (!stoppedAtRejection) {
      let observerCount = 0;
      let observerSeen = 0;
      let previousObserver = Buffer.alloc(0);
      const currentPreconditionsWitness = (): Buffer =>
        phaseAScriptPreconditionsWitnessCbor({
          containsNonNativeScript: phaseANativeControl.containsNonNativeScript,
          observerCount,
          observerSeen,
          previousObserver,
        });
      for (const observer of requiredObserversCollection.items) {
        pushWitness(
          "phaseAScriptPreconditions",
          currentPreconditionsWitness(),
          {
            kind: "transactionFieldChunk",
            fieldIndex: 3,
            itemIndex: observer.itemIndex,
            fieldPreimage: fieldPreimage(3),
          },
        );
        if (
          observerSeen > 0 &&
          Buffer.compare(previousObserver, observer.bytes) >= 0
        ) {
          if (
            rejection === null ||
            terminalPhase !== "phaseAScriptPreconditions" ||
            rejection.code !== RejectCodes.InvalidFieldType
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded observer scan found a duplicate or noncanonical ordering without the exact InvalidFieldType rejection",
              ),
            );
          }
          stoppedAtRejection = true;
          break;
        }
        if (observerCount === 0) {
          observerCount = requiredObserversCollection.items.length;
        }
        observerSeen += 1;
        previousObserver = observer.bytes;
      }
      if (!stoppedAtRejection) {
        pushWitness("phaseAScriptPreconditions", currentPreconditionsWitness());
        if (
          rejection !== null &&
          terminalPhase === "phaseAScriptPreconditions"
        ) {
          stoppedAtRejection = true;
        }
      }
    }

    if (!stoppedAtRejection) {
      if (phaseALedgerTx === null) {
        return yield* Effect.fail(
          new Error(
            "V1 trace reached input resolution without a Phase A ledger transaction",
          ),
        );
      }
      let resolutionAccumulator = initialMidgardResolvedInputsAccumulatorV1();
      let remainingScheduleHash = resolutionScheduleHash;
      let resolutionCursor = 0;
      const resolutionWitnessCbor = (
        pending:
          | {
              readonly node: (typeof resolutionScheduleNodes)[number];
              readonly descriptorCbor: Buffer;
              readonly outputProof: MidgardLedgerOutputProofControlV1;
            }
          | undefined,
      ): Buffer =>
        encodeCbor([
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
          BigInt(resolutionCursor),
          resolutionAccumulator,
          remainingScheduleHash,
          BigInt(signerFrontier.count),
          signerFrontierCommitment,
          pending === undefined
            ? Buffer.from([0])
            : encodeCbor([
                pending.node.sourceKind === "spend" ? 0n : 1n,
                pending.node.key,
                pending.node.nextScheduleHash,
                pending.descriptorCbor,
                encodeMidgardLedgerOutputProofControlV1(pending.outputProof),
              ]),
          resolutionScheduleHash,
        ]);

      pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
      if (
        terminalPhase === "resolveInputs" &&
        rejectionCode === RejectCodes.ValidityIntervalMismatch
      ) {
        stoppedAtRejection = true;
      } else {
        resolutionCursor = 1;

        for (
          let index = 0;
          index < resolutionScheduleNodes.length;
          index += 1
        ) {
          const item = resolutionScheduleNodes[index]!;
          if (!remainingScheduleHash.equals(item.scheduleHash)) {
            return yield* Effect.fail(
              new Error(
                "input-resolution schedule diverged from its committed hash chain",
              ),
            );
          }
          const outRefHex = item.key.toString("hex");
          const outputCbor = ledgerState.get(outRefHex);
          const descriptorCbor = ledgerDescriptorState.get(outRefHex);
          if (outputCbor === undefined || descriptorCbor === undefined) {
            pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
              kind: "scheduledLedgerLookup",
              sourceKind: item.sourceKind,
              key: item.key,
              nextScheduleHash: item.nextScheduleHash,
              value: null,
              proofCbor: item.proofCbor,
              signerProof: { kind: "none" },
            });
            if (
              terminalPhase !== "resolveInputs" ||
              rejectionCode !== RejectCodes.InputNotFound
            ) {
              return yield* Effect.fail(
                new Error(
                  `input resolution found no ledger member but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }

          const outputProof = buildMidgardLedgerOutputProofTraceV1({
            outputIndex: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
              outRef: item.key,
              outputCbor,
            }).descriptor.outputIndex,
            outputCbor,
          });
          pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
            kind: "scheduledLedgerLookup",
            sourceKind: item.sourceKind,
            key: item.key,
            nextScheduleHash: item.nextScheduleHash,
            value: descriptorCbor,
            proofCbor: item.proofCbor,
            signerProof: { kind: "none" },
          });
          for (const proofStep of outputProof.steps) {
            pushWitness(
              "resolveInputs",
              resolutionWitnessCbor({
                node: item,
                descriptorCbor,
                outputProof: proofStep.control,
              }),
              {
                kind: "ledgerOutputProofStep",
                witness: proofStep.witness,
              },
            );
          }
          const signerProof = signerSetProof(item.sourceKind, outputCbor);
          pushWitness(
            "resolveInputs",
            resolutionWitnessCbor({
              node: item,
              descriptorCbor,
              outputProof: outputProof.terminal,
            }),
            {
              kind: "ledgerOutputProofFinalize",
              descriptorCbor,
              signerProof,
            },
          );
          if (
            terminalPhase === "resolveInputs" &&
            rejectionCode === RejectCodes.MissingRequiredWitness &&
            item.sourceKind === "spend" &&
            signerProof.kind !== "membership"
          ) {
            stoppedAtRejection = true;
            break;
          }

          resolutionAccumulator = advanceMidgardResolvedInputsAccumulatorV1({
            accumulator: resolutionAccumulator,
            sourceKind: item.sourceKind,
            key: item.key,
            value: descriptorCbor,
          });
          remainingScheduleHash = item.nextScheduleHash;
          resolutionCursor += 1;
        }

        if (!stoppedAtRejection) {
          if (terminalPhase === "resolveInputs") {
            return yield* Effect.fail(
              new Error(
                `input-resolution rejection ${rejectionCode ?? "none"} has no exact V1 instruction`,
              ),
            );
          }
          pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
          const scriptSourceControl = {
            resolvedInputCount: resolutionItems.length,
            resolvedInputsAccumulator: resolutionAccumulator,
          };
          let authenticatedInlineSourceFrontier = emptyValidationFrontier;
          let inlineSourceTotalCount = 0;
          const currentInlineSourceWitness = (
            pendingSource?: {
              readonly sourceIndex: number;
              readonly sourceTotalCount: number;
              readonly languageTag: 0 | 3 | 128;
              readonly payloadOffset: number;
              readonly payloadLength: number;
              readonly itemLength: number;
              readonly itemCommitment: Buffer;
              readonly hashControl: MidgardBlake2b224TraceControlV1;
            } | null,
          ): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 0,
              sourceFrontier: authenticatedInlineSourceFrontier,
              redeemerFrontier: emptyValidationFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount: 0,
              pendingSource,
            });
          for (const item of scriptWitnessesCollection.items) {
            pushWitness("scriptSources", currentInlineSourceWitness(), {
              kind: "transactionFieldChunk",
              fieldIndex: MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX,
              itemIndex: item.itemIndex,
              fieldPreimage: fieldPreimage(
                MIDGARD_V1_SCRIPT_WITNESSES_FIELD_INDEX,
              ),
            });
            if (inlineSourceTotalCount === 0) {
              inlineSourceTotalCount = scriptWitnessesCollection.items.length;
            }
            const source = scriptSourceEntries[item.itemIndex];
            if (source === undefined || source.originKind !== "inline") {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script item lost its canonical source entry",
                ),
              );
            }
            const scriptArray = readCborArrayHeader(
              item.bytes,
              0,
              "v1.script_source",
            );
            const scriptLanguage = readCborInteger(
              item.bytes,
              scriptArray.nextOffset,
              "v1.script_source.language",
            );
            const scriptPayload = readCborBytes(
              item.bytes,
              scriptLanguage.nextOffset,
              "v1.script_source.payload",
            );
            const payloadOffset =
              scriptPayload.nextOffset - scriptPayload.value.length;
            if (
              scriptArray.length !== 2 ||
              scriptPayload.nextOffset !== item.bytes.length ||
              scriptPayload.value.length !== source.script.scriptBytes.length ||
              !scriptPayload.value.equals(source.script.scriptBytes)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script item is not its exact canonical versioned-script encoding",
                ),
              );
            }
            const exactLanguageTag: 0 | 3 | 128 =
              source.script.language === "NativeCardano"
                ? 0
                : source.script.language === "PlutusV3"
                  ? 3
                  : 128;
            if (scriptLanguage.value !== BigInt(exactLanguageTag)) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script language diverged from its canonical source",
                ),
              );
            }
            const hashMessage = Buffer.concat([
              Buffer.from([exactLanguageTag]),
              source.script.scriptBytes,
            ]);
            const hashTrace = buildMidgardBlake2b224TraceV1(hashMessage);
            let pendingSource = {
              sourceIndex: item.itemIndex,
              sourceTotalCount: inlineSourceTotalCount,
              languageTag: exactLanguageTag,
              payloadOffset,
              payloadLength: scriptPayload.value.length,
              itemLength: item.bytes.length,
              itemCommitment: item.commitment,
              hashControl: hashTrace[0]!.control,
            };
            for (const hashStep of hashTrace) {
              let auxiliary:
                | {
                    readonly kind: "scriptSourceHashBlock";
                    readonly chunkProof: MidgardBoundedItemChunkProofV1;
                    readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
                  }
                | undefined;
              if (hashStep.block !== null) {
                const contentLength =
                  hashStep.block.length -
                  (hashStep.control.cursor === 0 ? 1 : 0);
                const itemCursor =
                  hashStep.control.cursor === 0
                    ? payloadOffset
                    : payloadOffset + hashStep.control.cursor - 1;
                const chunkIndex =
                  contentLength === 0
                    ? 0
                    : Math.floor(
                        itemCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
                      );
                const chunkProof = buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                );
                const offset =
                  contentLength === 0
                    ? payloadOffset
                    : itemCursor -
                      chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
                const crossesChunk =
                  contentLength > chunkProof.chunk.length - offset;
                auxiliary = {
                  kind: "scriptSourceHashBlock",
                  chunkProof,
                  nextChunkProof: crossesChunk
                    ? buildMidgardBoundedItemChunkProofV1(item, chunkIndex + 1)
                    : null,
                };
              }
              pushWitness(
                "scriptSources",
                currentInlineSourceWitness(pendingSource),
                auxiliary,
              );
              pendingSource = {
                ...pendingSource,
                hashControl: hashStep.next,
              };
            }
            if (
              pendingSource.hashControl.stage !==
                MidgardBlake2b224TraceStagesV1.Terminal ||
              !pendingSource.hashControl.chainingValue
                .subarray(0, 28)
                .equals(source.scriptHash)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script hash trace diverged from its canonical identity",
                ),
              );
            }
            pushWitness(
              "scriptSources",
              currentInlineSourceWitness(pendingSource),
            );
            authenticatedInlineSourceFrontier =
              appendMidgardValidationMerkleLeafV1(
                authenticatedInlineSourceFrontier,
                inlineScriptSourceLeafHashes[item.itemIndex]!,
              );
          }
          pushWitness("scriptSources", currentInlineSourceWitness());
          if (
            !commitMidgardValidationMerkleFrontierV1(
              authenticatedInlineSourceFrontier,
            ).equals(
              commitMidgardValidationMerkleFrontierV1(
                inlineScriptSourceFrontier,
              ),
            )
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated inline source fold diverged from the canonical source frontier",
              ),
            );
          }
          let authenticatedRedeemerFrontier = emptyValidationFrontier;
          let redeemerTotalCount = 0;
          const currentRedeemerWitness = (
            redeemerItemControlHash: Buffer = Buffer.alloc(0),
          ): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 1,
              sourceFrontier: inlineScriptSourceFrontier,
              redeemerFrontier: authenticatedRedeemerFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount,
              redeemerItemControlHash,
            });
          for (const item of redeemerWitnessesCollection.items) {
            const redeemer = decodedProofRedeemers[item.itemIndex];
            const canonicalRedeemerWitnessCbor =
              canonicalRedeemerWitnessCbors[item.itemIndex];
            if (
              redeemer === undefined ||
              canonicalRedeemerWitnessCbor === undefined ||
              !item.bytes.equals(canonicalRedeemerWitnessCbor)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded redeemer item diverged from its canonical decoded witness",
                ),
              );
            }
            pushWitness("scriptSources", currentRedeemerWitness(), {
              kind: "transactionRedeemerItemBegin",
              // Stage 1: field 8, item index `control.redeemer_count`. Both are
              // fixed by the stage and its cursor, so the carriage is the whole
              // wire surface.
              fieldIndex: 8,
              fieldPreimage: fieldPreimage(8),
            });
            if (redeemerTotalCount === 0) {
              redeemerTotalCount = redeemerWitnessesCollection.items.length;
            }
            const itemTrace = buildMidgardRedeemerItemProofTraceV1({
              itemIndex: item.itemIndex,
              itemCount: redeemerTotalCount,
              itemBytes: item.bytes,
              mode: MidgardRedeemerItemProofModesV1.Data,
            });
            let activeItemControlHash = hashMidgardRedeemerItemProofControlV1(
              itemTrace.initial,
            );
            for (const itemStep of itemTrace.steps) {
              pushWitness(
                "scriptSources",
                currentRedeemerWitness(activeItemControlHash),
                {
                  kind: "redeemerItemStep",
                  redeemerControl: null,
                  control: itemStep.control,
                  witness: itemStep.witness,
                },
              );
              if (
                itemStep.next.stage ===
                MidgardRedeemerItemProofStagesV1.Terminal
              ) {
                authenticatedRedeemerFrontier =
                  appendMidgardValidationMerkleLeafV1(
                    authenticatedRedeemerFrontier,
                    hashMidgardRedeemerItemLeafV1({
                      redeemerIndex: item.itemIndex,
                      itemCommitment: item.commitment,
                    }),
                  );
                activeItemControlHash = Buffer.alloc(0);
              } else {
                activeItemControlHash = hashMidgardRedeemerItemProofControlV1(
                  itemStep.next,
                );
              }
            }
            if (activeItemControlHash.length !== 0) {
              return yield* Effect.fail(
                new Error(
                  "redeemer item proof did not reach its terminal control",
                ),
              );
            }
          }
          pushWitness("scriptSources", currentRedeemerWitness());
          if (
            !commitMidgardValidationMerkleFrontierV1(
              authenticatedRedeemerFrontier,
            ).equals(commitMidgardValidationMerkleFrontierV1(redeemerFrontier))
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated redeemer fold diverged from the canonical redeemer frontier",
              ),
            );
          }
          {
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 2,
                sourceFrontier: inlineScriptSourceFrontier,
                redeemerFrontier,
              }),
            );
            let replayCursor = 0;
            let replayAccumulator = initialMidgardResolvedInputsAccumulatorV1();
            let replayRemainingScheduleHash = resolutionScheduleHash;
            let replaySpendIndex = 0;
            let replaySourceFrontier = inlineScriptSourceFrontier;
            let replayPurposeFrontier = emptyValidationFrontier;
            for (const node of resolutionScheduleNodes) {
              const outRefHex = node.key.toString("hex");
              const outputCbor = ledgerState.get(outRefHex);
              const descriptorCbor = ledgerDescriptorState.get(outRefHex);
              if (outputCbor === undefined || descriptorCbor === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay lost previously authenticated output material",
                  ),
                );
              }
              const outputMaterial =
                buildCanonicalMidgardLedgerEntryOutputMaterialV1({
                  outRef: node.key,
                  outputCbor,
                });
              if (!outputMaterial.descriptorCbor.equals(descriptorCbor)) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay descriptor differs from retained output material",
                  ),
                );
              }
              const descriptor = outputMaterial.descriptor;
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 3,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                }),
                {
                  kind: "resolvedInputReplay",
                  sourceKind: node.sourceKind,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value: descriptorCbor,
                },
              );
              if (!replayRemainingScheduleHash.equals(node.scheduleHash)) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay schedule diverged from its committed hash chain",
                  ),
                );
              }
              if (
                node.sourceKind === "reference" &&
                descriptor.referenceScriptLanguage !== -1
              ) {
                const output = decodeMidgardTxOutput(outputCbor);
                if (output.script_ref === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "reference-input descriptor commits a missing retained reference script",
                    ),
                  );
                }
                const leaf = hashMidgardReferenceScriptSourceLeafV1({
                  sourceKey: node.key,
                  scriptLanguageTag: descriptor.referenceScriptLanguage,
                  scriptHash: descriptor.referenceScriptHash,
                  scriptTotalLength: descriptor.referenceScriptTotalLength,
                  itemCommitment: descriptor.referenceScriptItemCommitment,
                });
                if (
                  !leaf.equals(
                    hashMidgardScriptSourceLeafV1({
                      originKind: "reference",
                      sourceKey: node.key,
                      script: output.script_ref,
                    }),
                  )
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "retained reference script differs from its authenticated descriptor facts",
                    ),
                  );
                }
                const sourceEntry: ScriptSourceProofEntry = {
                  originKind: "reference",
                  sourceKey: node.key,
                  script: output.script_ref,
                  authenticatedVersionedItemBytes: encodeMidgardVersionedScript(
                    output.script_ref,
                  ),
                  scriptLanguageTag: descriptor.referenceScriptLanguage,
                  scriptHash: descriptor.referenceScriptHash,
                  scriptTotalLength: descriptor.referenceScriptTotalLength,
                  scriptItemCommitment:
                    descriptor.referenceScriptItemCommitment,
                  leaf,
                };
                scriptSourceEntries.push(sourceEntry);
                replaySourceFrontier = appendMidgardValidationMerkleLeafV1(
                  replaySourceFrontier,
                  sourceEntry.leaf,
                );
              }
              if (node.sourceKind === "spend") {
                const credential = decodeMidgardAddressBytes(
                  descriptor.address,
                ).paymentCredential;
                if (credential.kind === "Script") {
                  const purposeEntry: ScriptPurposeProofEntry = {
                    purposeKind: 0,
                    purposeIndex: BigInt(replaySpendIndex),
                    scriptHash: Buffer.from(credential.hash),
                    subject: node.key,
                    leaf: hashMidgardScriptPurposeLeafV1({
                      purposeKind: 0,
                      purposeIndex: BigInt(replaySpendIndex),
                      scriptHash: credential.hash,
                      subject: node.key,
                    }),
                  };
                  scriptPurposeEntries.push(purposeEntry);
                  replayPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                    replayPurposeFrontier,
                    purposeEntry.leaf,
                  );
                }
                replaySpendIndex += 1;
              }
              resolvedItemFrontier = appendMidgardValidationMerkleLeafV1(
                resolvedItemFrontier,
                hashMidgardResolvedContextItemLeafV1({
                  sourceKind: node.sourceKind,
                  itemIndex: replayCursor,
                  key: node.key,
                  outputCbor: descriptorCbor,
                }),
              );
              replayAccumulator = advanceMidgardResolvedInputsAccumulatorV1({
                accumulator: replayAccumulator,
                sourceKind: node.sourceKind,
                key: node.key,
                value: descriptorCbor,
              });
              replayRemainingScheduleHash = node.nextScheduleHash;
              replayCursor += 1;
            }
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 3,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
              }),
            );
            let authenticatedOutputFrontier = emptyValidationFrontier;
            let outputTotalCount = 0;
            const currentOutputCommitmentWitness = (): Buffer =>
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 4,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
                outputFrontier: authenticatedOutputFrontier,
                outputTotalCount,
              });
            for (const item of outputsCollection.items) {
              const outputCbor = outputCbors[item.itemIndex];
              if (outputCbor === undefined || !item.bytes.equals(outputCbor)) {
                return yield* Effect.fail(
                  new Error(
                    "bounded output item diverged from its canonical decoded output",
                  ),
                );
              }
              // Stage 4 folds only the authenticated
              // (field_index, item_index, item_length, item_commitment) tuple,
              // all four of which the door *derives* from the authenticated
              // preimage. The item bytes are still not revealed here, and the
              // reason is unchanged: revealing them re-proves only that an
              // authenticated commitment has a preimage — which canonicalDecode
              // and the stage-5 output traversal already establish — while
              // making the one-step evidence grow with output size and exceed
              // the L1 envelope for legal 16,384-byte outputs (C21-STAGE4-GAP,
              // Option A).
              //
              // What *has* changed is where the size now comes from. The
              // carriage keeps this redeemer O(1) in output size only under
              // tiers 2-3, where the preimage rides reference inputs
              // (`onchain/aiken/lib/midgard/validation-machine-v1.ak:9189`).
              // The step therefore carries the *plan input* — which field, which
              // bytes — and the tier is resolved at evidence commitment, where a
              // transaction exists to index reference inputs into (#600). Above
              // §8.3's 14,336-byte tier-1 cap the resolution is genuinely tier 2
              // or 3 and this evidence is O(1); below it, tier-1 `Inline`. The
              // producer itself never refuses and never names a tier.
              pushWitness("scriptSources", currentOutputCommitmentWitness(), {
                kind: "transactionRedeemerItemBegin",
                fieldIndex: 2,
                fieldPreimage: fieldPreimage(2),
              });
              if (outputTotalCount === 0) {
                outputTotalCount = outputsCollection.items.length;
              }
              authenticatedOutputFrontier = appendMidgardValidationMerkleLeafV1(
                authenticatedOutputFrontier,
                hashMidgardOutputItemLeafV1({
                  outputIndex: item.itemIndex,
                  itemCommitment: item.commitment,
                }),
              );
            }
            pushWitness("scriptSources", currentOutputCommitmentWitness());
            if (
              !commitMidgardValidationMerkleFrontierV1(
                authenticatedOutputFrontier,
              ).equals(commitMidgardValidationMerkleFrontierV1(outputFrontier))
            ) {
              return yield* Effect.fail(
                new Error(
                  "authenticated output fold diverged from the canonical output frontier",
                ),
              );
            }
            let outputCursor = 0;
            let receiveSourceFrontier = emptyValidationFrontier;
            let outputDescriptorFrontier = emptyValidationFrontier;
            const receiveSourceEntries: ScriptPurposeProofEntry[] = [];
            const receiveSourceScan = () => ({
              sourceFrontier: receiveSourceFrontier,
              receiveCount: 0,
              previousHash: Buffer.alloc(0),
              candidateHash: Buffer.alloc(0),
              descriptorFrontier: outputDescriptorFrontier,
            });
            const retainedOutputDescriptorScan = () => ({
              sourceFrontier: emptyValidationFrontier,
              receiveCount: 0,
              previousHash: Buffer.alloc(0),
              candidateHash: Buffer.alloc(0),
              descriptorFrontier: outputDescriptorFrontier,
            });
            const protectedSignerRejection =
              rejection !== null &&
              terminalPhase === "scriptSources" &&
              rejection.code === RejectCodes.MissingRequiredWitness &&
              rejection.detail?.startsWith(
                "missing witness for protected output signer ",
              ) === true;
            const outputNetworkRejection =
              rejection !== null &&
              terminalPhase === "scriptSources" &&
              rejection.code === RejectCodes.NetworkIdMismatch;
            for (const outputCbor of outputCbors) {
              const outputItem = outputsCollection.items[outputCursor];
              if (
                outputItem === undefined ||
                !outputItem.bytes.equals(outputCbor)
              ) {
                return yield* Effect.fail(
                  new Error(
                    "output admission lost its authenticated bounded item",
                  ),
                );
              }
              const outputProof = buildMidgardLedgerOutputProofTraceV1({
                outputIndex: outputCursor,
                outputCbor,
              });
              const outputMaterial =
                buildCanonicalMidgardLedgerOutputMaterialV1({
                  outputIndex: outputCursor,
                  outputCbor,
                });
              const signerProof = protectedOutputSignerProof(outputCbor);
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
                {
                  kind: "ledgerOutputProofBegin",
                  outputIndex: outputCursor,
                  totalLength: outputItem.bytes.length,
                  itemCommitment: outputItem.commitment,
                  siblings: outputMembership(outputCursor).siblings,
                },
              );
              for (const proofStep of outputProof.steps) {
                pushWitness(
                  "scriptSources",
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage: 5,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: replayPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveScan: receiveSourceScan(),
                    outputProof: proofStep.control,
                  }),
                  {
                    kind: "ledgerOutputProofStep",
                    witness: proofStep.witness,
                  },
                );
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                  outputProof: outputProof.terminal,
                }),
                {
                  kind: "ledgerOutputProofFinalize",
                  descriptorCbor: outputMaterial.descriptorCbor,
                  signerProof,
                },
              );
              const output = decodeMidgardTxOutput(outputCbor);
              const address = decodeMidgardAddressBytes(output.address);
              if (
                outputNetworkRejection &&
                BigInt(address.networkId) !== input.expectedNetworkId
              ) {
                stoppedAtRejection = true;
                break;
              }
              if (
                protectedSignerRejection &&
                address.protected &&
                address.paymentCredential.kind === "PubKey" &&
                signerProof.kind !== "membership"
              ) {
                stoppedAtRejection = true;
                break;
              }
              outputDescriptorFrontier = appendMidgardValidationMerkleLeafV1(
                outputDescriptorFrontier,
                hashMidgardOutputDescriptorLeafV1({
                  outputIndex: outputCursor,
                  descriptorCbor: outputMaterial.descriptorCbor,
                }),
              );
              admittedOutputDescriptorCbors.push(outputMaterial.descriptorCbor);
              admittedOutputDescriptorLeafHashes.push(
                hashMidgardOutputDescriptorLeafV1({
                  outputIndex: outputCursor,
                  descriptorCbor: outputMaterial.descriptorCbor,
                }),
              );
              if (
                address.protected &&
                address.paymentCredential.kind === "Script"
              ) {
                const scriptHash = Buffer.from(address.paymentCredential.hash);
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveSourceFrontier.count),
                  scriptHash,
                  subject: scriptHash,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 3,
                    purposeIndex: BigInt(receiveSourceFrontier.count),
                    scriptHash,
                    subject: scriptHash,
                  }),
                };
                receiveSourceEntries.push(purposeEntry);
                receiveSourceFrontier = appendMidgardValidationMerkleLeafV1(
                  receiveSourceFrontier,
                  purposeEntry.leaf,
                );
              }
              outputCursor += 1;
            }
            if (!stoppedAtRejection) {
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
              );
              let mintPurposeFrontier = replayPurposeFrontier;
              for (const policyItem of mintCollection.items) {
                pushWitness(
                  "scriptSources",
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage: 6,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: mintPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveScan: receiveSourceScan(),
                  }),
                  {
                    kind: "transactionFieldChunk",
                    fieldIndex: 5,
                    itemIndex: policyItem.itemIndex,
                    fieldPreimage: fieldPreimage(5),
                  },
                );
                const itemHeader = readCborArrayHeader(
                  policyItem.bytes,
                  0,
                  `v1.mint.policy[${policyItem.itemIndex}]`,
                );
                if (itemHeader.length !== 2) {
                  throw new Error(
                    "V1 mint policy item must contain two fields",
                  );
                }
                const policy = readCborBytes(
                  policyItem.bytes,
                  itemHeader.nextOffset,
                  `v1.mint.policy[${policyItem.itemIndex}].id`,
                );
                const assets = readCborMapHeader(
                  policyItem.bytes,
                  policy.nextOffset,
                  `v1.mint.policy[${policyItem.itemIndex}].assets`,
                );
                const policyId = Buffer.from(policy.value);
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 1,
                  purposeIndex: BigInt(policyItem.itemIndex),
                  scriptHash: policyId,
                  subject: policyId,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 1,
                    purposeIndex: BigInt(policyItem.itemIndex),
                    scriptHash: policyId,
                    subject: policyId,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                mintPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                  mintPurposeFrontier,
                  purposeEntry.leaf,
                );
                mintFoldControl = {
                  ...mintFoldControl,
                  policyCount: mintCollection.items.length,
                  activePolicy: policyId,
                  itemLength: policyItem.bytes.length,
                  itemCommitment: Buffer.from(policyItem.commitment),
                  itemCursor: assets.nextOffset,
                  assetsRemaining: assets.length,
                  policyAssetCursor: 0,
                  previousAsset: Buffer.alloc(0),
                };
                let assetCursor = assets.nextOffset;
                for (
                  let assetIndex = 0;
                  assetIndex < assets.length;
                  assetIndex += 1
                ) {
                  const expectedChunkIndex = Math.floor(
                    assetCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
                  );
                  const nextChunkIndex =
                    expectedChunkIndex + 1 <
                    midgardBoundedItemChunkCountV1(policyItem.bytes.length)
                      ? expectedChunkIndex + 1
                      : null;
                  pushWitness(
                    "scriptSources",
                    scriptSourcesWitnessCbor({
                      ...scriptSourceControl,
                      stage: 6,
                      sourceFrontier: replaySourceFrontier,
                      redeemerFrontier,
                      replayCursor,
                      replayAccumulator,
                      replayRemainingScheduleHash,
                      spendIndex: replaySpendIndex,
                      purposeFrontier: mintPurposeFrontier,
                      outputCursor,
                      outputFrontier,
                      receiveScan: receiveSourceScan(),
                    }),
                    {
                      kind: "mintFoldAsset",
                      chunkProof: buildMidgardBoundedItemChunkProofV1(
                        policyItem,
                        expectedChunkIndex,
                      ),
                      nextChunkProof:
                        nextChunkIndex === null
                          ? null
                          : buildMidgardBoundedItemChunkProofV1(
                              policyItem,
                              nextChunkIndex,
                            ),
                    },
                  );
                  const asset = readCborBytes(
                    policyItem.bytes,
                    assetCursor,
                    `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].name`,
                  );
                  const quantity = readCborInteger(
                    policyItem.bytes,
                    asset.nextOffset,
                    `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].quantity`,
                  );
                  assetCursor = quantity.nextOffset;
                  const nextAssetFrontier = appendMidgardValidationMerkleLeafV1(
                    mintFoldControl.assetFrontier,
                    hashMidgardMintAssetLeafV1({
                      policyId,
                      assetName: asset.value,
                      quantity: quantity.value,
                    }),
                  );
                  const finishedPolicy = assetIndex + 1 === assets.length;
                  mintFoldControl = finishedPolicy
                    ? {
                        ...mintFoldControl,
                        policyCursor: mintFoldControl.policyCursor + 1,
                        previousPolicy: policyId,
                        activePolicy: Buffer.alloc(0),
                        itemLength: 0,
                        itemCommitment: Buffer.alloc(0),
                        itemCursor: 0,
                        assetsRemaining: 0,
                        policyAssetCursor: 0,
                        previousAsset: Buffer.alloc(0),
                        assetFrontier: nextAssetFrontier,
                      }
                    : {
                        ...mintFoldControl,
                        itemCursor: assetCursor,
                        assetsRemaining: mintFoldControl.assetsRemaining - 1,
                        policyAssetCursor:
                          mintFoldControl.policyAssetCursor + 1,
                        previousAsset: Buffer.from(asset.value),
                        assetFrontier: nextAssetFrontier,
                      };
                }
                if (assetCursor !== policyItem.bytes.length) {
                  throw new Error("V1 mint policy item has trailing bytes");
                }
              }
              if (mintCollection.items.length === 0) {
                mintFoldControl = {
                  ...mintFoldControl,
                  policyCount: 0,
                };
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 6,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: mintPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
              );
              let observerPurposeFrontier = mintPurposeFrontier;
              let observerTotalCount = 0;
              let observerSeen = 0;
              let previousObserverHash = Buffer.alloc(0);
              const currentObserverPurposeWitness = (): Buffer =>
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 7,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: observerPurposeFrontier,
                  outputCursor: 0,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                  observerScan: {
                    totalCount: observerTotalCount,
                    seen: observerSeen,
                    previousHash: previousObserverHash,
                  },
                });
              for (const observer of requiredObserversCollection.items) {
                pushWitness("scriptSources", currentObserverPurposeWitness(), {
                  kind: "transactionFieldChunk",
                  fieldIndex: 3,
                  itemIndex: observer.itemIndex,
                  fieldPreimage: fieldPreimage(3),
                });
                if (observerTotalCount === 0) {
                  observerTotalCount = requiredObserversCollection.items.length;
                }
                const observerHash = observer.bytes;
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 2,
                  purposeIndex: BigInt(observerSeen),
                  scriptHash: observerHash,
                  subject: observerHash,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 2,
                    purposeIndex: BigInt(observerSeen),
                    scriptHash: observerHash,
                    subject: observerHash,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                observerPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                  observerPurposeFrontier,
                  purposeEntry.leaf,
                );
                observerSeen += 1;
                previousObserverHash = observerHash;
              }
              let allPurposeFrontier = observerPurposeFrontier;
              const receiveSourceLeaves = receiveSourceEntries.map(
                (entry) => entry.leaf,
              );
              const receiveSourceMembership = (sourceIndex: number) =>
                buildMidgardValidationMerkleMembershipV1(
                  receiveSourceLeaves,
                  sourceIndex,
                );
              let receiveSourceCursor = 0;
              let receiveCount = 0;
              let receivePreviousHash = Buffer.alloc(0);
              let receiveCandidateHash = Buffer.alloc(0);
              const currentReceivePurposeWitness = (): Buffer =>
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 7,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: allPurposeFrontier,
                  outputCursor: receiveSourceCursor,
                  outputFrontier,
                  receiveScan: {
                    sourceFrontier: receiveSourceFrontier,
                    receiveCount,
                    previousHash: receivePreviousHash,
                    candidateHash: receiveCandidateHash,
                    descriptorFrontier: outputDescriptorFrontier,
                  },
                  observerScan: {
                    totalCount: observerTotalCount,
                    seen: observerSeen,
                    previousHash: previousObserverHash,
                  },
                });
              while (true) {
                if (receiveSourceCursor === receiveSourceEntries.length) {
                  pushWitness("scriptSources", currentReceivePurposeWitness());
                  if (receiveCandidateHash.length === 0) {
                    break;
                  }
                  const scriptHash = receiveCandidateHash;
                  const purposeEntry: ScriptPurposeProofEntry = {
                    purposeKind: 3,
                    purposeIndex: BigInt(receiveCount),
                    scriptHash,
                    subject: scriptHash,
                    leaf: hashMidgardScriptPurposeLeafV1({
                      purposeKind: 3,
                      purposeIndex: BigInt(receiveCount),
                      scriptHash,
                      subject: scriptHash,
                    }),
                  };
                  scriptPurposeEntries.push(purposeEntry);
                  allPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                    allPurposeFrontier,
                    purposeEntry.leaf,
                  );
                  receiveCount += 1;
                  receivePreviousHash = scriptHash;
                  receiveCandidateHash = Buffer.alloc(0);
                  receiveSourceCursor = 0;
                  continue;
                }
                const receiveSource =
                  receiveSourceEntries[receiveSourceCursor]!;
                pushWitness("scriptSources", currentReceivePurposeWitness(), {
                  kind: "scriptPurposeScan",
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveSourceCursor),
                  scriptHash: receiveSource.scriptHash,
                  subject: receiveSource.subject,
                  siblings:
                    receiveSourceMembership(receiveSourceCursor).siblings,
                });
                const scriptHash = receiveSource.scriptHash;
                if (
                  (receivePreviousHash.length === 0 ||
                    Buffer.compare(receivePreviousHash, scriptHash) < 0) &&
                  (receiveCandidateHash.length === 0 ||
                    Buffer.compare(scriptHash, receiveCandidateHash) < 0)
                ) {
                  receiveCandidateHash = scriptHash;
                }
                receiveSourceCursor += 1;
              }
              {
                const sourceLeaves = scriptSourceEntries.map(
                  (entry) => entry.leaf,
                );
                const purposeLeaves = scriptPurposeEntries.map(
                  (entry) => entry.leaf,
                );
                const redeemerLeaves = redeemerLeafHashes;
                const discoveryWitnessCbor = (
                  stage: number,
                  discovery: ScriptDiscoveryTraceControlV1,
                ): Buffer =>
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: allPurposeFrontier,
                    outputCursor: outputFrontier.count,
                    outputFrontier,
                    receiveScan: retainedOutputDescriptorScan(),
                    discovery,
                  });
                const sourceMembership = (sourceIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    sourceLeaves,
                    sourceIndex,
                  );
                const purposeMembership = (purposeIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    purposeLeaves,
                    purposeIndex,
                  );
                const redeemerMembership = (redeemerIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    redeemerLeaves,
                    redeemerIndex,
                  );
                const setDiscoveryBit = (
                  bitmap: bigint,
                  index: number,
                ): bigint => bitmap | (1n << BigInt(index));
                const resetCurrent = (
                  discovery: ScriptDiscoveryTraceControlV1,
                ): ScriptDiscoveryTraceControlV1 => ({
                  ...discovery,
                  sourceCursor: 0,
                  redeemerCursor: 0,
                  currentPurposeKind: -1,
                  currentPurposeIndex: -1n,
                  currentScriptHash: Buffer.alloc(0),
                  currentSubject: Buffer.alloc(0),
                  matchedSourceIndex: -1,
                  matchedLanguageTag: -1,
                  matchedSourceLeaf: Buffer.alloc(0),
                  redeemerItemControlHash: Buffer.alloc(0),
                });

                let discovery = emptyScriptDiscoveryControl;
                for (
                  let purposeCursor = 0;
                  purposeCursor < scriptPurposeEntries.length;
                  purposeCursor += 1
                ) {
                  const purpose = scriptPurposeEntries[purposeCursor]!;
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                    {
                      kind: "scriptPurposeScan",
                      purposeKind: purpose.purposeKind,
                      purposeIndex: purpose.purposeIndex,
                      scriptHash: purpose.scriptHash,
                      subject: purpose.subject,
                      siblings: purposeMembership(purposeCursor).siblings,
                    },
                  );
                  discovery = {
                    ...discovery,
                    sourceCursor: 0,
                    redeemerCursor: 0,
                    currentPurposeKind: purpose.purposeKind,
                    currentPurposeIndex: purpose.purposeIndex,
                    currentScriptHash: purpose.scriptHash,
                    currentSubject: purpose.subject,
                    matchedSourceIndex: -1,
                    matchedLanguageTag: -1,
                    matchedSourceLeaf: Buffer.alloc(0),
                  };

                  let matchedSource:
                    | {
                        readonly entry: ScriptSourceProofEntry;
                        readonly sourceIndex: number;
                        readonly languageTag: 0 | 3 | 128;
                      }
                    | undefined;
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        scriptLanguageTag: source.scriptLanguageTag,
                        scriptHash: source.scriptHash,
                        scriptTotalLength: source.scriptTotalLength,
                        scriptItemCommitment: source.scriptItemCommitment,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    const sourceHash = source.scriptHash;
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                    if (sourceHash.equals(purpose.scriptHash)) {
                      const exactLanguageTag = source.scriptLanguageTag;
                      discovery = {
                        ...discovery,
                        matchedSourceIndex: sourceIndex,
                        matchedLanguageTag: exactLanguageTag,
                        matchedSourceLeaf: source.leaf,
                        usedInlineBitmap:
                          source.originKind === "inline"
                            ? setDiscoveryBit(
                                discovery.usedInlineBitmap,
                                sourceIndex,
                              )
                            : discovery.usedInlineBitmap,
                      };
                      matchedSource = {
                        entry: source,
                        sourceIndex,
                        languageTag: exactLanguageTag,
                      };
                      break;
                    }
                  }
                  if (matchedSource === undefined) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 source scan reached an exact missing-source rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }

                  if (matchedSource.languageTag === 0) {
                    const executionLeaf = hashMidgardScriptExecutionLeafV1({
                      languageTag: 0,
                      purposeLeaf: purpose.leaf,
                      sourceLeaf: matchedSource.entry.leaf,
                    });
                    scriptExecutionEntries.push({
                      purpose,
                      source: matchedSource.entry,
                      sourceIndex: matchedSource.sourceIndex,
                      languageTag: 0,
                      redeemerLeaf: Buffer.alloc(0),
                      leaf: executionLeaf,
                    });
                    discovery = resetCurrent({
                      ...discovery,
                      purposeCursor: purposeCursor + 1,
                      executionFrontier: appendMidgardValidationMerkleLeafV1(
                        discovery.executionFrontier,
                        executionLeaf,
                      ),
                    });
                    continue;
                  }

                  let matchedRedeemerIndex = -1;
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    const redeemer = decodedProofRedeemers[redeemerIndex]!;
                    const item =
                      redeemerWitnessesCollection.items[redeemerIndex]!;
                    const itemTrace = buildMidgardRedeemerItemProofTraceV1({
                      itemIndex: redeemerIndex,
                      itemCount: decodedProofRedeemers.length,
                      itemBytes: item.bytes,
                      mode: MidgardRedeemerItemProofModesV1.Descriptor,
                    });
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                      {
                        kind: "redeemerScanBegin",
                        itemIndex: redeemerIndex,
                        itemCount: decodedProofRedeemers.length,
                        totalLength: item.bytes.length,
                        itemCommitment: item.commitment,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    discovery = {
                      ...discovery,
                      redeemerItemControlHash:
                        hashMidgardRedeemerItemProofControlV1(
                          itemTrace.initial,
                        ),
                    };
                    for (const itemStep of itemTrace.steps) {
                      pushWitness(
                        "scriptSources",
                        discoveryWitnessCbor(10, discovery),
                        {
                          kind: "redeemerItemStep",
                          redeemerControl: null,
                          control: itemStep.control,
                          witness: itemStep.witness,
                        },
                      );
                      if (
                        itemStep.next.stage !==
                        MidgardRedeemerItemProofStagesV1.Terminal
                      ) {
                        discovery = {
                          ...discovery,
                          redeemerItemControlHash:
                            hashMidgardRedeemerItemProofControlV1(
                              itemStep.next,
                            ),
                        };
                        continue;
                      }
                      if (
                        redeemerPointerMatchesPurposeV1({
                          purposeKind: purpose.purposeKind,
                          purposeIndex: purpose.purposeIndex,
                          redeemerTag: redeemer.tag,
                          redeemerIndex: redeemer.index,
                        })
                      ) {
                        matchedRedeemerIndex = redeemerIndex;
                        const executionLeaf = hashMidgardScriptExecutionLeafV1({
                          languageTag: matchedSource.languageTag,
                          purposeLeaf: purpose.leaf,
                          sourceLeaf: matchedSource.entry.leaf,
                          redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                        });
                        scriptExecutionEntries.push({
                          purpose,
                          source: matchedSource.entry,
                          sourceIndex: matchedSource.sourceIndex,
                          languageTag: matchedSource.languageTag,
                          redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                          leaf: executionLeaf,
                        });
                        discovery = resetCurrent({
                          ...discovery,
                          purposeCursor: purposeCursor + 1,
                          usedRedeemerBitmap: setDiscoveryBit(
                            discovery.usedRedeemerBitmap,
                            redeemerIndex,
                          ),
                          executionFrontier:
                            appendMidgardValidationMerkleLeafV1(
                              discovery.executionFrontier,
                              executionLeaf,
                            ),
                        });
                      } else {
                        discovery = {
                          ...discovery,
                          redeemerCursor: redeemerIndex + 1,
                          redeemerItemControlHash: Buffer.alloc(0),
                        };
                      }
                    }
                    if (matchedRedeemerIndex >= 0) {
                      break;
                    }
                  }
                  if (matchedRedeemerIndex < 0) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 redeemer scan reached an exact missing-redeemer rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                  );
                  discovery = resetCurrent({
                    ...discovery,
                    sourceCursor: 0,
                  });
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(11, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        scriptLanguageTag: source.scriptLanguageTag,
                        scriptHash: source.scriptHash,
                        scriptTotalLength: source.scriptTotalLength,
                        scriptItemCommitment: source.scriptItemCommitment,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    if (
                      source.originKind === "inline" &&
                      (discovery.usedInlineBitmap &
                        (1n << BigInt(sourceIndex))) ===
                        0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 source audit found an extraneous inline script that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(11, discovery),
                  );
                  discovery = {
                    ...discovery,
                    redeemerCursor: 0,
                  };
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    const item =
                      redeemerWitnessesCollection.items[redeemerIndex]!;
                    const itemTrace = buildMidgardRedeemerItemProofTraceV1({
                      itemIndex: redeemerIndex,
                      itemCount: decodedProofRedeemers.length,
                      itemBytes: item.bytes,
                      mode: MidgardRedeemerItemProofModesV1.Descriptor,
                    });
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(12, discovery),
                      {
                        kind: "redeemerScanBegin",
                        itemIndex: redeemerIndex,
                        itemCount: decodedProofRedeemers.length,
                        totalLength: item.bytes.length,
                        itemCommitment: item.commitment,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    discovery = {
                      ...discovery,
                      redeemerItemControlHash:
                        hashMidgardRedeemerItemProofControlV1(
                          itemTrace.initial,
                        ),
                    };
                    for (const itemStep of itemTrace.steps) {
                      pushWitness(
                        "scriptSources",
                        discoveryWitnessCbor(12, discovery),
                        {
                          kind: "redeemerItemStep",
                          redeemerControl: null,
                          control: itemStep.control,
                          witness: itemStep.witness,
                        },
                      );
                      if (
                        itemStep.next.stage !==
                        MidgardRedeemerItemProofStagesV1.Terminal
                      ) {
                        discovery = {
                          ...discovery,
                          redeemerItemControlHash:
                            hashMidgardRedeemerItemProofControlV1(
                              itemStep.next,
                            ),
                        };
                      }
                    }
                    if (
                      (discovery.usedRedeemerBitmap &
                        (1n << BigInt(redeemerIndex))) ===
                      0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 redeemer audit found an extraneous redeemer that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      redeemerCursor: redeemerIndex + 1,
                      redeemerItemControlHash: Buffer.alloc(0),
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  const nativeScriptBaseFields: unknown[] = [
                    proofSource.compactCbor,
                    proofSource.witnessSetCompactCbor,
                    proofSource.fieldPreimageLengthsCbor,
                    contextCbor,
                    BigInt(scriptSourceControl.resolvedInputCount),
                    scriptSourceControl.resolvedInputsAccumulator,
                    BigInt(replaySpendIndex),
                    encodeFrontierPeaks(resolvedItemFrontier),
                    BigInt(signerFrontier.count),
                    signerFrontierCommitment,
                    BigInt(replaySourceFrontier.count),
                    encodeFrontierPeaks(replaySourceFrontier),
                    BigInt(redeemerFrontier.count),
                    encodeFrontierPeaks(redeemerFrontier),
                    BigInt(allPurposeFrontier.count),
                    encodeFrontierPeaks(allPurposeFrontier),
                    BigInt(outputFrontier.count),
                    encodeFrontierPeaks(outputFrontier),
                    encodeFrontierPeaks(outputDescriptorFrontier),
                    BigInt(mintFoldControl.assetFrontier.count),
                    encodeFrontierPeaks(mintFoldControl.assetFrontier),
                    BigInt(discovery.executionFrontier.count),
                    encodeFrontierPeaks(discovery.executionFrontier),
                  ];
                  const nativeScriptFields: unknown[] = [
                    ...nativeScriptBaseFields,
                    0n,
                    0n,
                    resolutionScheduleHash,
                  ];
                  authenticatedNativeScriptsBaseFields = nativeScriptBaseFields;
                  authenticatedNativeScriptsWitnessCbor =
                    encodeCbor(nativeScriptFields);
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(12, discovery),
                  );
                  if (rejection !== null && terminalPhase === "scriptSources") {
                    return yield* Effect.fail(
                      new Error(
                        "V1 validation reports a ScriptSources rejection but all exact discovery and audit instructions accepted",
                      ),
                    );
                  }
                }
              }
            }
          }
        }
      }
    }

    if (!stoppedAtRejection) {
      const nativeBaseFields = authenticatedNativeScriptsBaseFields;
      if (
        authenticatedNativeScriptsWitnessCbor === null ||
        nativeBaseFields === null
      ) {
        return yield* Effect.fail(
          new Error(
            "V1 did not authenticate the NativeScripts handoff witness",
          ),
        );
      }
      const nativeControlCbor = (
        executionCursor: number,
        languageBitmap: number,
      ): Buffer =>
        encodeCbor([
          ...nativeBaseFields,
          BigInt(executionCursor),
          BigInt(languageBitmap),
          resolutionScheduleHash,
        ]);
      const executionLeaves = scriptExecutionEntries.map((entry) => entry.leaf);
      const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
      const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
      let languageBitmap = 0;
      for (
        let executionIndex = 0;
        executionIndex < scriptExecutionEntries.length;
        executionIndex += 1
      ) {
        const execution = scriptExecutionEntries[executionIndex]!;
        const item = boundedItemForScriptSource(execution.source);
        const continuationCbor = nativeControlCbor(
          executionIndex,
          languageBitmap,
        );
        pushWitness("nativeScripts", continuationCbor, {
          kind: "nativeExecutionDescriptor",
          executionIndex,
          languageTag: execution.languageTag,
          purpose: {
            purposeKind: execution.purpose.purposeKind,
            purposeIndex: execution.purpose.purposeIndex,
            scriptHash: execution.purpose.scriptHash,
            subject: execution.purpose.subject,
            siblings: buildMidgardValidationMerkleMembershipV1(
              purposeLeaves,
              executionIndex,
            ).siblings,
          },
          source: {
            sourceIndex: execution.sourceIndex,
            originKind: execution.source.originKind,
            sourceKey: execution.source.sourceKey,
            scriptTotalLength: execution.source.scriptTotalLength,
            scriptItemCommitment: execution.source.scriptItemCommitment,
            siblings: buildMidgardValidationMerkleMembershipV1(
              sourceLeaves,
              execution.sourceIndex,
            ).siblings,
          },
          redeemerLeaf: execution.redeemerLeaf,
          executionSiblings: buildMidgardValidationMerkleMembershipV1(
            executionLeaves,
            executionIndex,
          ).siblings,
          firstChunkProof:
            execution.languageTag === 0
              ? buildMidgardBoundedItemChunkProofV1(item, 0)
              : null,
          signerFrontier:
            execution.languageTag === 0
              ? signerFrontier
              : emptyValidationFrontier,
        });
        if (execution.languageTag === 0) {
          if (execution.source.script.language !== "NativeCardano") {
            return yield* Effect.fail(
              new Error(
                "V1 native execution language disagrees with its script source",
              ),
            );
          }

          let header: ValidationMachineVersionedScriptHeaderV1;
          try {
            header = readValidationMachineVersionedScriptHeaderV1(item.bytes);
          } catch {
            return yield* Effect.fail(
              new Error(
                "authenticated native script has an invalid versioned-script header",
              ),
            );
          }
          if (header.languageTag !== 0) {
            return yield* Effect.fail(
              new Error(
                "authenticated native script descriptor has a non-native header",
              ),
            );
          }

          let lateControl: PhaseANativeScriptsScanControl = {
            stage: 1,
            scriptCount: 1,
            scriptSeen: 0,
            containsNonNativeScript: 0,
            itemLength: item.bytes.length,
            itemCommitment: item.commitment,
            cursor: header.payloadOffset,
            stackRoot: Buffer.alloc(0),
            stackDepth: 0,
            nodeCount: 0,
            result: -1,
          };
          const nativeScriptFrames: ValidationMachineNativeScriptFrameV1[] = [];
          const expectedLateRejection = (code: RejectCode): boolean =>
            rejection !== null &&
            terminalPhase === "nativeScripts" &&
            rejection.code === code;
          const failUnexpectedLateRejection = (
            actual: RejectCode,
          ): Effect.Effect<never, Error> =>
            Effect.fail(
              new Error(
                `bounded execution native-script scan found ${actual} at stage=${lateControl.stage},cursor=${lateControl.cursor} but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
              ),
            );
          const pushLateWitness = (
            auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
          ): void => {
            pushWitness(
              "phaseANativeScripts",
              phaseANativeScriptsScanWitnessCbor(lateControl, continuationCbor),
              auxiliary,
            );
          };

          while (!stoppedAtRejection) {
            if (lateControl.stage === 1) {
              const chunkIndex = Math.floor(
                lateControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let head: ValidationMachineNativeScriptTokenHeadV1 | null = null;
              try {
                head = readValidationMachineNativeScriptTokenHeadV1(
                  item.bytes,
                  lateControl.cursor,
                );
              } catch {
                // The authenticated token witness proves the malformed bytes.
              }
              pushLateWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(item, chunkIndex + 1)
                    : null,
                signerProof: { kind: "none" },
              });
              if (head === null) {
                if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              const nextNodeCount = lateControl.nodeCount + 1;
              if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
                if (!expectedLateRejection(RejectCodes.NativeScriptNodeCount)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.NativeScriptNodeCount,
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              lateControl = {
                ...lateControl,
                stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                cursor: head.payloadOffset,
                nodeCount: nextNodeCount,
              };
              continue;
            }

            if (lateControl.stage >= 3) {
              const kind = (lateControl.stage - 3) as 0 | 1 | 2 | 3 | 4 | 5;
              const chunkIndex = Math.floor(
                lateControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let token: ValidationMachineNativeScriptTokenV1 | null = null;
              try {
                token = readValidationMachineNativeScriptPayloadV1(
                  item.bytes,
                  lateControl.cursor,
                  kind,
                );
              } catch {
                // The authenticated payload witness proves the malformed bytes.
              }
              const signerProof =
                token?.kind === 0
                  ? signerProofForHash(token.keyHash)
                  : ({ kind: "none" } as const);
              pushLateWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(item, chunkIndex + 1)
                    : null,
                signerProof,
              });
              if (token === null) {
                if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              if (token.kind >= 1 && token.kind <= 3 && token.childCount > 0) {
                const nextDepth = lateControl.stackDepth + 1;
                if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH_V1) {
                  if (!expectedLateRejection(RejectCodes.NativeScriptDepth)) {
                    return yield* failUnexpectedLateRejection(
                      RejectCodes.NativeScriptDepth,
                    );
                  }
                  stoppedAtRejection = true;
                  break;
                }
                const frame: ValidationMachineNativeScriptFrameV1 = {
                  tail: lateControl.stackRoot,
                  kind: token.kind as 1 | 2 | 3,
                  childCount: token.childCount,
                  remaining: token.childCount,
                  validCount: 0,
                  required: token.required,
                };
                nativeScriptFrames.push(frame);
                lateControl = {
                  ...lateControl,
                  stage: 1,
                  cursor: token.nextOffset,
                  stackRoot: hashValidationMachineNativeScriptFrameV1(frame),
                  stackDepth: nextDepth,
                };
                continue;
              }

              let valid: boolean;
              if (token.kind === 0) {
                valid = signerProof.kind === "membership";
              } else if (token.kind === 4) {
                valid =
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= 0n &&
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= token.slot;
              } else if (token.kind === 5) {
                valid =
                  compactProofTransaction.transactionBody.validityIntervalEnd >=
                    0n &&
                  compactProofTransaction.transactionBody.validityIntervalEnd <=
                    token.slot;
              } else if (token.kind === 1) {
                valid = true;
              } else if (token.kind === 2) {
                valid = false;
              } else {
                valid = token.required === 0n;
              }
              lateControl = {
                ...lateControl,
                stage: 2,
                cursor: token.nextOffset,
                result: valid ? 1 : 0,
              };
              continue;
            }

            const frame = nativeScriptFrames[nativeScriptFrames.length - 1];
            if (frame !== undefined) {
              pushLateWitness({ kind: "nativeScriptFrame", frame });
              const validCount =
                frame.validCount + (lateControl.result === 1 ? 1 : 0);
              if (frame.remaining === 1) {
                nativeScriptFrames.pop();
                const valid =
                  frame.kind === 1
                    ? validCount === frame.childCount
                    : frame.kind === 2
                      ? validCount > 0
                      : BigInt(validCount) >= frame.required;
                lateControl = {
                  ...lateControl,
                  stackRoot: frame.tail,
                  stackDepth: lateControl.stackDepth - 1,
                  result: valid ? 1 : 0,
                };
              } else {
                const nextFrame: ValidationMachineNativeScriptFrameV1 = {
                  ...frame,
                  remaining: frame.remaining - 1,
                  validCount,
                };
                nativeScriptFrames[nativeScriptFrames.length - 1] = nextFrame;
                lateControl = {
                  ...lateControl,
                  stage: 1,
                  stackRoot:
                    hashValidationMachineNativeScriptFrameV1(nextFrame),
                  result: -1,
                };
              }
              continue;
            }

            pushLateWitness();
            if (lateControl.cursor !== lateControl.itemLength) {
              if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                return yield* failUnexpectedLateRejection(
                  RejectCodes.InvalidFieldType,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (lateControl.result === 0) {
              if (!expectedLateRejection(RejectCodes.NativeScriptInvalid)) {
                return yield* failUnexpectedLateRejection(
                  RejectCodes.NativeScriptInvalid,
                );
              }
              stoppedAtRejection = true;
            }
            break;
          }
          if (stoppedAtRejection) break;
        } else if (execution.languageTag === 3) {
          languageBitmap |= 1;
        } else {
          languageBitmap |= 2;
        }
      }
      if (!stoppedAtRejection) {
        pushWitness(
          "nativeScripts",
          nativeControlCbor(scriptExecutionEntries.length, languageBitmap),
        );
        if (rejection !== null && terminalPhase === "nativeScripts") {
          return yield* Effect.fail(
            new Error(
              "V1 validation reports a NativeScripts rejection but every authenticated native execution accepted",
            ),
          );
        }
        const authenticatedNativeControlCbor = nativeControlCbor(
          scriptExecutionEntries.length,
          languageBitmap,
        );
        const scriptIntegrityWitnessCbor = encodeCbor([
          authenticatedNativeControlCbor,
          0n,
        ]);
        pushWitness("scriptIntegrity", scriptIntegrityWitnessCbor);
        pushWitness(
          "scriptIntegrity",
          encodeCbor([authenticatedNativeControlCbor, 1n]),
        );
        pushWitness(
          "scriptIntegrity",
          encodeCbor([
            authenticatedNativeControlCbor,
            2n,
            compactProofTransaction.transactionBody.scriptIntegrityHash,
            compactProofTransaction.transactionWitnessSetHash,
          ]),
        );
        pushWitness(
          "scriptIntegrity",
          encodeCbor([
            authenticatedNativeControlCbor,
            3n,
            compactProofTransaction.transactionBody.scriptIntegrityHash,
            compactProofWitnessSet.redeemerTxWitsHash,
          ]),
        );
        if (rejection !== null && terminalPhase === "scriptIntegrity") {
          stoppedAtRejection = true;
        } else {
          const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
          const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
          const redeemerLeaves = redeemerLeafHashes;
          const resolvedLeaves = resolutionScheduleNodes.map(
            (node, itemIndex) => {
              const descriptorCbor = ledgerDescriptorState.get(
                node.key.toString("hex"),
              );
              if (descriptorCbor === undefined) {
                throw new Error(
                  "CEK context construction lost an authenticated resolved-input descriptor",
                );
              }
              return hashMidgardResolvedContextItemLeafV1({
                sourceKind: node.sourceKind,
                itemIndex,
                key: node.key,
                outputCbor: descriptorCbor,
              });
            },
          );
          const sameSummary = (
            left: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.cborLength === right.cborLength &&
            left.memory === right.memory;
          const sameSequence = (
            left: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.length === right.length &&
            left.payloadCborLength === right.payloadCborLength &&
            left.memory === right.memory;
          const exactDescriptorSummary = (summary: {
            readonly root: Uint8Array;
            readonly cborLength: bigint;
            readonly memory: bigint;
          }) => ({
            root: Buffer.from(summary.root),
            cborLength: summary.cborLength,
            memory: summary.memory,
          });
          const outRefSummary = (key: Buffer) =>
            summarizeMidgardCekLucidDataV1(
              txOutRefData(key.toString("hex")) as never,
            );
          const resolvedTxInInfoSummary = (
            key: Buffer,
            output: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
          ) =>
            summarizeMidgardCekSmallConstrDataV1(
              0n,
              prependMidgardCekDataListSummaryV1(
                outRefSummary(key),
                prependMidgardCekDataListSummaryV1(
                  exactDescriptorSummary(output),
                  emptyMidgardCekDataListSummaryV1(),
                ),
              ),
            );
          const cardanoSpendScriptInfoSummary = (
            key: Buffer,
            spendDatum: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
          ) =>
            summarizeMidgardCekSmallConstrDataV1(
              1n,
              prependMidgardCekDataListSummaryV1(
                outRefSummary(key),
                prependMidgardCekDataListSummaryV1(
                  exactDescriptorSummary(spendDatum),
                  emptyMidgardCekDataListSummaryV1(),
                ),
              ),
            );
          const cekWitness = (input: {
            readonly contextControl: MidgardCekContextControlV1 | null;
            readonly executionCursor: number;
            readonly completedCpu: bigint;
            readonly completedMemory: bigint;
            readonly activeStateHash: Uint8Array | null;
            readonly executionCpuLimit: bigint;
            readonly executionMemoryLimit: bigint;
            readonly programEnvelopeHash: Uint8Array | null;
          }): Buffer =>
            encodeMidgardCekValidationWitnessV1({
              nativeControlCbor: authenticatedNativeControlCbor,
              ...input,
            });
          const cekContextWitness = (input: {
            readonly contextControl: MidgardCekContextControlV1;
            readonly executionCursor: number;
            readonly completedCpu: bigint;
            readonly completedMemory: bigint;
          }): Buffer =>
            cekWitness({
              ...input,
              activeStateHash: null,
              executionCpuLimit: 0n,
              executionMemoryLimit: 0n,
              programEnvelopeHash: input.contextControl.programEnvelopeHash,
            });
          const executionAuxiliary = (
            execution: ScriptExecutionProofEntry,
            executionIndex: number,
          ): NonNullable<ValidationMachineWorkWitness["auxiliary"]> => {
            const sourceItem = boundedItemForScriptSource(execution.source);
            return {
              kind: "nativeExecutionScan",
              executionIndex,
              languageTag: execution.languageTag,
              purpose: {
                purposeKind: execution.purpose.purposeKind,
                purposeIndex: execution.purpose.purposeIndex,
                scriptHash: execution.purpose.scriptHash,
                subject: execution.purpose.subject,
                siblings: buildMidgardValidationMerkleMembershipV1(
                  purposeLeaves,
                  executionIndex,
                ).siblings,
              },
              source: {
                sourceIndex: execution.sourceIndex,
                originKind: execution.source.originKind,
                sourceKey: execution.source.sourceKey,
                scriptTotalLength: execution.source.scriptTotalLength,
                scriptItemCommitment: execution.source.scriptItemCommitment,
                siblings: buildMidgardValidationMerkleMembershipV1(
                  sourceLeaves,
                  execution.sourceIndex,
                ).siblings,
              },
              redeemerLeaf: execution.redeemerLeaf,
              executionSiblings: buildMidgardValidationMerkleMembershipV1(
                executionLeaves,
                executionIndex,
              ).siblings,
              firstChunkProof: buildMidgardBoundedItemChunkProofV1(
                sourceItem,
                0,
              ),
            };
          };
          const purposeForProof = (
            purpose: ScriptPurposeProofEntry,
          ): MidgardScriptPurpose => {
            const scriptHash = purpose.scriptHash.toString("hex");
            if (purpose.purposeKind === 0) {
              return {
                kind: "spend",
                scriptHash,
                outRefHex: purpose.subject.toString("hex"),
              };
            }
            if (purpose.purposeKind === 1) {
              return {
                kind: "mint",
                scriptHash,
                policyId: scriptHash,
              };
            }
            if (purpose.purposeKind === 2) {
              return { kind: "observe", scriptHash };
            }
            return { kind: "receive", scriptHash };
          };
          const purposeSummary = (
            purpose: ScriptPurposeProofEntry,
            languageTag: 3 | 128,
          ) =>
            summarizeMidgardCekLucidDataV1(
              (languageTag === 128
                ? midgardScriptPurposeData(purposeForProof(purpose))
                : cardanoScriptPurposeData(purposeForProof(purpose))) as never,
            );
          const selectedRedeemer = (
            execution: ScriptExecutionProofEntry,
          ): {
            readonly index: number;
            readonly value: DecodedMidgardRedeemer;
          } => {
            const index = redeemerLeaves.findIndex((leaf) =>
              leaf.equals(execution.redeemerLeaf),
            );
            if (index < 0) {
              throw new Error(
                "CEK execution does not select an authenticated redeemer",
              );
            }
            return { index, value: decodedProofRedeemers[index]! };
          };

          let evaluationIndex = 0;
          for (
            let executionIndex = 0;
            executionIndex < scriptExecutionEntries.length;
            executionIndex += 1
          ) {
            const executionEntry = scriptExecutionEntries[executionIndex]!;
            const completedCpu = traceExecutionCpu;
            const completedMemory = traceExecutionMemory;
            pushWitness(
              "cek",
              cekWitness({
                contextControl: null,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
                activeStateHash: null,
                executionCpuLimit: 0n,
                executionMemoryLimit: 0n,
                programEnvelopeHash: null,
              }),
              executionAuxiliary(executionEntry, executionIndex),
            );

            if (
              executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 3
            ) {
              if (
                rejection === null ||
                terminalPhase !== "cek" ||
                rejection.code !== RejectCodes.PlutusScriptInvalid
              ) {
                throw new Error(
                  "PlutusV3 receive-purpose rejection disagrees with validation",
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (executionEntry.languageTag === 0) {
              continue;
            }

            const evaluation = scriptEvaluations[evaluationIndex++];
            if (
              evaluation === undefined ||
              !evaluation.scriptBytes.equals(
                executionEntry.source.script.scriptBytes,
              ) ||
              evaluation.graph === null
            ) {
              throw new Error(
                "CEK execution is missing its authenticated program graph",
              );
            }
            const selected = selectedRedeemer(executionEntry);
            const exactExecution = executeMidgardCekStructuralProgramV1({
              root: evaluation.graph.root,
              material: evaluation.graph.material.values(),
              constantWitnesses: evaluation.graph.constantWitnesses,
              executionIndex: BigInt(executionIndex),
              maxSteps:
                input.consensusProfile.limits.maxValidationMachineStepCount,
              executionBudget: {
                cpu: selected.value.exUnits.steps,
                memory: selected.value.exUnits.memory,
              },
            });
            const programEnvelope = decodeMidgardCekProgramEnvelopeV1(
              executionEntry.source.script.scriptBytes,
            );
            let contextControl = initialMidgardCekContextControlV1({
              languageTag: executionEntry.languageTag,
              programTermRoot: programEnvelope.termRoot,
              programEnvelopeHash:
                hashMidgardCekProgramEnvelopeV1(programEnvelope),
              purposeKind: executionEntry.purpose.purposeKind,
              purposeIndex: executionEntry.purpose.purposeIndex,
              scriptHash: executionEntry.purpose.scriptHash,
              subject: executionEntry.purpose.subject,
              redeemerLeaf: executionEntry.redeemerLeaf,
            });
            const decodedContext = decodeMidgardCekContextV1(
              evaluation.contextCbor,
            );
            const contextParts = summarizeMidgardCekContextPartsV1(
              decodedContext,
              executionEntry.languageTag,
            );

            let redeemerControl = initialMidgardCekRedeemerContextControlV1();
            const selectedItem =
              redeemerWitnessesCollection.items[selected.index]!;
            const selectionTrace = buildMidgardRedeemerItemProofTraceV1({
              itemIndex: selected.index,
              itemCount: decodedProofRedeemers.length,
              itemBytes: selectedItem.bytes,
              mode: MidgardRedeemerItemProofModesV1.Descriptor,
              expectedPurposeTag: redeemerTagForPurposeKindV1(
                executionEntry.purpose.purposeKind,
              ),
              expectedPointerIndex: Number(executionEntry.purpose.purposeIndex),
            });
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "redeemerScanBegin",
                itemIndex: selected.index,
                itemCount: decodedProofRedeemers.length,
                totalLength: selectedItem.bytes.length,
                itemCommitment: selectedItem.commitment,
                siblings: buildMidgardValidationMerkleMembershipV1(
                  redeemerLeaves,
                  selected.index,
                ).siblings,
              },
            );
            contextControl = {
              ...contextControl,
              redeemerContextControlHash: hashMidgardRedeemerItemProofControlV1(
                selectionTrace.initial,
              ),
            };
            for (const itemStep of selectionTrace.steps) {
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "redeemerItemStep",
                  redeemerControl: null,
                  control: itemStep.control,
                  witness: itemStep.witness,
                },
              );
              contextControl =
                itemStep.next.stage ===
                MidgardRedeemerItemProofStagesV1.Terminal
                  ? {
                      ...contextControl,
                      stage: 1,
                      executionMemoryLimit: itemStep.next.executionMemory,
                      executionCpuLimit: itemStep.next.executionSteps,
                      redeemerContextControlHash:
                        hashMidgardCekRedeemerContextControlV1(redeemerControl),
                    }
                  : {
                      ...contextControl,
                      redeemerContextControlHash:
                        hashMidgardRedeemerItemProofControlV1(itemStep.next),
                    };
            }

            const spendCount = resolutionScheduleNodes.filter(
              (node) => node.sourceKind === "spend",
            ).length;
            const addressEncoding =
              executionEntry.languageTag === 128 ? "midgard" : "cardano";
            for (
              let itemIndex = resolutionScheduleNodes.length - 1;
              itemIndex >= spendCount;
              itemIndex -= 1
            ) {
              const node = resolutionScheduleNodes[itemIndex]!;
              const descriptorCbor = ledgerDescriptorState.get(
                node.key.toString("hex"),
              );
              if (descriptorCbor === undefined) {
                throw new Error(
                  "CEK reference-input context lost its authenticated ledger descriptor",
                );
              }
              const descriptor =
                decodeMidgardLedgerOutputCommitmentV1(descriptorCbor);
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekResolvedContextItem",
                  sourceKind: "reference",
                  itemIndex,
                  key: node.key,
                  descriptorCbor,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    resolvedLeaves,
                    itemIndex,
                  ).siblings,
                },
              );
              const item = resolvedTxInInfoSummary(
                node.key,
                addressEncoding === "midgard"
                  ? descriptor.midgardTxOut
                  : descriptor.cardanoTxOut,
              );
              contextControl = {
                ...contextControl,
                referenceItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.referenceItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 2 };
            if (
              !sameSequence(
                contextControl.referenceItems,
                contextParts.referenceItems,
              )
            ) {
              throw new Error(
                "CEK reference-input context differs from the evaluated context",
              );
            }

            for (
              let itemIndex = spendCount - 1;
              itemIndex >= 0;
              itemIndex -= 1
            ) {
              const node = resolutionScheduleNodes[itemIndex]!;
              const descriptorCbor = ledgerDescriptorState.get(
                node.key.toString("hex"),
              );
              if (descriptorCbor === undefined) {
                throw new Error(
                  "CEK spend-input context lost its authenticated ledger descriptor",
                );
              }
              const descriptor =
                decodeMidgardLedgerOutputCommitmentV1(descriptorCbor);
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekResolvedContextItem",
                  sourceKind: "spend",
                  itemIndex,
                  key: node.key,
                  descriptorCbor,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    resolvedLeaves,
                    itemIndex,
                  ).siblings,
                },
              );
              const item = resolvedTxInInfoSummary(
                node.key,
                addressEncoding === "midgard"
                  ? descriptor.midgardTxOut
                  : descriptor.cardanoTxOut,
              );
              contextControl = {
                ...contextControl,
                spendItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.spendItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 3 };
            if (
              !sameSequence(contextControl.spendItems, contextParts.spendItems)
            ) {
              throw new Error(
                "CEK spend-input context differs from the evaluated context",
              );
            }

            for (
              let outputIndex = admittedOutputDescriptorCbors.length - 1;
              outputIndex >= 0;
              outputIndex -= 1
            ) {
              const descriptorCbor = admittedOutputDescriptorCbors[outputIndex];
              if (descriptorCbor === undefined) {
                throw new Error(
                  "CEK output context lost its authenticated output descriptor",
                );
              }
              const descriptor =
                decodeMidgardLedgerOutputCommitmentV1(descriptorCbor);
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekOutputContextItem",
                  outputIndex,
                  descriptorCbor,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    admittedOutputDescriptorLeafHashes,
                    outputIndex,
                  ).siblings,
                },
              );
              const item = exactDescriptorSummary(
                addressEncoding === "midgard"
                  ? descriptor.midgardTxOut
                  : descriptor.cardanoTxOut,
              );
              contextControl = {
                ...contextControl,
                outputItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.outputItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 4 };
            if (
              !sameSequence(
                contextControl.outputItems,
                contextParts.outputItems,
              )
            ) {
              throw new Error(
                "CEK output context differs from the evaluated context",
              );
            }

            for (
              let signerIndex = canonicalSignerHashes.length - 1;
              signerIndex >= 0;
              signerIndex -= 1
            ) {
              const signerHash = canonicalSignerHashes[signerIndex]!;
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekSignerContextItem",
                  frontier: signerFrontier,
                  signerIndex,
                  signerHash,
                  siblings: signerMembership(signerIndex).siblings,
                },
              );
              contextControl = {
                ...contextControl,
                signerItems: prependMidgardCekDataListSummaryV1(
                  summarizeMidgardCekLucidDataV1(signerHash.toString("hex")),
                  contextControl.signerItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 5 };
            if (
              !sameSequence(
                contextControl.signerItems,
                contextParts.signerItems,
              )
            ) {
              throw new Error(
                "CEK signer context differs from the evaluated context",
              );
            }

            const observerCount = requiredObserversCollection.items.length;
            validateMidgardCekObserverCollectionV1(
              requiredObserversCollection.items.map(
                (observer) => observer.bytes,
              ),
            );
            const midgardObserverEncoding = executionEntry.languageTag === 128;
            for (
              let observerIndex = observerCount - 1;
              observerIndex >= 0;
              observerIndex -= 1
            ) {
              const observer =
                requiredObserversCollection.items[observerIndex]!;
              if (
                contextControl.previousObserver.length > 0 &&
                Buffer.compare(
                  observer.bytes,
                  contextControl.previousObserver,
                ) >= 0
              ) {
                throw new Error(
                  "CEK observer context is not strictly ordered and unique",
                );
              }
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "transactionFieldChunk",
                  fieldIndex: 3,
                  itemIndex: observer.itemIndex,
                  fieldPreimage: fieldPreimage(3),
                },
              );
              contextControl = {
                ...contextControl,
                observerCount,
                observerItems: prependMidgardCekObserverItemV1({
                  observerHash: observer.bytes,
                  midgardEncoding: midgardObserverEncoding,
                  tail: contextControl.observerItems,
                }),
                previousObserver: observer.bytes,
              };
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            const observerSummary = finalizeMidgardCekObserverItemsV1({
              items: contextControl.observerItems,
              midgardEncoding: midgardObserverEncoding,
            });
            contextControl = {
              ...contextControl,
              stage: 6,
              observerSummary,
            };
            if (!sameSummary(observerSummary, contextParts.observer)) {
              throw new Error(
                "CEK observer context differs from the evaluated context",
              );
            }

            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            const authenticatedMintAssets = [
              ...phaseALedgerTx!.mint.assets,
            ].map((asset) => ({
              policyId: Buffer.from(asset.policyId),
              assetName: Buffer.from(asset.assetName),
              quantity: asset.quantity,
            }));
            const authenticatedMintLeaves = authenticatedMintAssets.map(
              (asset) => hashMidgardMintAssetLeafV1(asset),
            );
            const authenticatedMintFrontier =
              buildMidgardValidationMerkleFrontierV1(authenticatedMintLeaves);
            if (
              !commitMidgardValidationMerkleFrontierV1(
                authenticatedMintFrontier,
              ).equals(
                commitMidgardValidationMerkleFrontierV1(
                  mintFoldControl.assetFrontier,
                ),
              )
            ) {
              throw new Error(
                "CEK mint context does not match the authenticated NativeScripts mint frontier",
              );
            }
            if (authenticatedMintAssets.length === 0) {
              contextControl = {
                ...contextControl,
                stage: 9,
                mintSummary: contextParts.mint,
              };
            } else {
              contextControl = {
                ...contextControl,
                stage: 8,
              };

              for (
                let mintIndex = authenticatedMintAssets.length - 1;
                mintIndex >= 0;
                mintIndex -= 1
              ) {
                const asset = authenticatedMintAssets[mintIndex]!;
                pushWitness(
                  "cek",
                  cekContextWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "cekMintContextItem",
                    mintIndex,
                    policyId: asset.policyId,
                    assetName: asset.assetName,
                    quantity: asset.quantity,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      authenticatedMintLeaves,
                      mintIndex,
                    ).siblings,
                  },
                );
                const nextAssetSummary = prependMidgardCekDataPairSummaryV1(
                  summarizeMidgardCekLucidDataV1(
                    asset.assetName.toString("hex"),
                  ),
                  summarizeMidgardCekLucidDataV1(asset.quantity),
                  contextControl.currentMintAssets,
                );
                if (
                  contextControl.currentMintPolicy.length === 0 ||
                  contextControl.currentMintPolicy.equals(asset.policyId)
                ) {
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: nextAssetSummary,
                  };
                } else {
                  const priorPolicy = prependMidgardCekDataPairSummaryV1(
                    summarizeMidgardCekLucidDataV1(
                      contextControl.currentMintPolicy.toString("hex"),
                    ),
                    summarizeMidgardCekMapDataV1(
                      contextControl.currentMintAssets,
                    ),
                    contextControl.mintPolicies,
                  );
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: prependMidgardCekDataPairSummaryV1(
                      summarizeMidgardCekLucidDataV1(
                        asset.assetName.toString("hex"),
                      ),
                      summarizeMidgardCekLucidDataV1(asset.quantity),
                      emptyMidgardCekDataPairSummaryV1(),
                    ),
                    mintPolicies: priorPolicy,
                  };
                }
              }
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
              );
              const finalPolicies = prependMidgardCekDataPairSummaryV1(
                summarizeMidgardCekLucidDataV1(
                  contextControl.currentMintPolicy.toString("hex"),
                ),
                summarizeMidgardCekMapDataV1(contextControl.currentMintAssets),
                contextControl.mintPolicies,
              );
              contextControl = {
                ...contextControl,
                stage: 9,
                currentMintPolicy: Buffer.alloc(0),
                currentMintAssets: emptyMidgardCekDataPairSummaryV1(),
                mintPolicies: finalPolicies,
                mintSummary: summarizeMidgardCekMapDataV1(finalPolicies),
              };
            }
            if (!sameSummary(contextControl.mintSummary, contextParts.mint)) {
              throw new Error(
                "CEK mint context differs from the evaluated context",
              );
            }

            for (
              let redeemerIndex = decodedProofRedeemers.length - 1;
              redeemerIndex >= 0;
              redeemerIndex -= 1
            ) {
              const redeemer = decodedProofRedeemers[redeemerIndex]!;
              const purposeKind = purposeKindForRedeemerTagV1(redeemer.tag);
              const purposeFrontierIndex = scriptPurposeEntries.findIndex(
                (purpose) =>
                  purpose.purposeKind === purposeKind &&
                  purpose.purposeIndex === redeemer.index,
              );
              if (purposeFrontierIndex < 0 || purposeKind === null) {
                throw new Error(
                  "CEK redeemer does not select an authenticated purpose",
                );
              }
              const purpose = scriptPurposeEntries[purposeFrontierIndex]!;
              const item = redeemerWitnessesCollection.items[redeemerIndex]!;
              const descriptorOnly =
                executionEntry.languageTag === 3 && purpose.purposeKind === 3;
              const itemTrace = buildMidgardRedeemerItemProofTraceV1({
                itemIndex: redeemerIndex,
                itemCount: decodedProofRedeemers.length,
                itemBytes: item.bytes,
                mode: descriptorOnly
                  ? MidgardRedeemerItemProofModesV1.Descriptor
                  : MidgardRedeemerItemProofModesV1.Data,
                expectedPurposeTag: redeemerTagForPurposeKindV1(
                  purpose.purposeKind,
                ),
                expectedPointerIndex: Number(purpose.purposeIndex),
              });
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekRedeemerContextSelect",
                  control: redeemerControl,
                  itemIndex: redeemerIndex,
                  itemCount: decodedProofRedeemers.length,
                  totalLength: item.bytes.length,
                  itemCommitment: item.commitment,
                  redeemerSiblings: buildMidgardValidationMerkleMembershipV1(
                    redeemerLeaves,
                    redeemerIndex,
                  ).siblings,
                  purposeFrontierIndex,
                  purpose: {
                    purposeKind: purpose.purposeKind,
                    purposeIndex: purpose.purposeIndex,
                    scriptHash: purpose.scriptHash,
                    subject: purpose.subject,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      purposeLeaves,
                      purposeFrontierIndex,
                    ).siblings,
                  },
                },
              );
              const semanticPurpose = descriptorOnly
                ? initialMidgardCekRedeemerContextControlV1().activePurpose
                : purposeSummary(purpose, executionEntry.languageTag);
              redeemerControl = {
                ...redeemerControl,
                activeScanHash: hashMidgardRedeemerItemProofControlV1(
                  itemTrace.initial,
                ),
                activeRedeemerLeaf: redeemerLeaves[redeemerIndex]!,
                activePurpose: semanticPurpose,
              };
              contextControl = {
                ...contextControl,
                redeemerContextControlHash:
                  hashMidgardCekRedeemerContextControlV1(redeemerControl),
              };
              for (const itemStep of itemTrace.steps) {
                pushWitness(
                  "cek",
                  cekContextWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "redeemerItemStep",
                    redeemerControl,
                    control: itemStep.control,
                    witness: itemStep.witness,
                  },
                );
                if (
                  itemStep.next.stage ===
                  MidgardRedeemerItemProofStagesV1.Terminal
                ) {
                  if (descriptorOnly) {
                    redeemerControl = {
                      ...redeemerControl,
                      cursor: redeemerControl.cursor + 1,
                      activeScanHash: Buffer.alloc(0),
                      activeRedeemerLeaf: Buffer.alloc(0),
                      activePurpose:
                        initialMidgardCekRedeemerContextControlV1()
                          .activePurpose,
                    };
                  } else {
                    const nextSummary = finalizeMidgardRedeemerItemProofV1(
                      itemStep.next,
                    );
                    if (nextSummary === null) {
                      throw new Error(
                        "terminal redeemer item proof lacks a Data summary",
                      );
                    }
                    const nextCurrent = redeemerLeaves[redeemerIndex]!.equals(
                      executionEntry.redeemerLeaf,
                    )
                      ? nextSummary
                      : redeemerControl.currentRedeemer;
                    redeemerControl = {
                      ...redeemerControl,
                      cursor: redeemerControl.cursor + 1,
                      mapItems: prependMidgardCekDataPairSummaryV1(
                        redeemerControl.activePurpose,
                        nextSummary,
                        redeemerControl.mapItems,
                      ),
                      activeScanHash: Buffer.alloc(0),
                      activeRedeemerLeaf: Buffer.alloc(0),
                      activePurpose:
                        initialMidgardCekRedeemerContextControlV1()
                          .activePurpose,
                      currentRedeemer: nextCurrent,
                    };
                  }
                } else {
                  redeemerControl = {
                    ...redeemerControl,
                    activeScanHash: hashMidgardRedeemerItemProofControlV1(
                      itemStep.next,
                    ),
                  };
                }
                contextControl = {
                  ...contextControl,
                  stage:
                    redeemerControl.cursor === decodedProofRedeemers.length
                      ? 10
                      : 9,
                  redeemerContextControlHash:
                    hashMidgardCekRedeemerContextControlV1(redeemerControl),
                };
              }
            }
            if (
              contextControl.stage !== 10 ||
              !sameSummary(
                redeemerControl.currentRedeemer,
                contextParts.redeemer,
              ) ||
              !sameSequence(
                redeemerControl.mapItems,
                contextParts.redeemerItems,
              )
            ) {
              throw new Error(
                "CEK redeemer context differs from the evaluated context",
              );
            }

            const selectedSpendItem =
              executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 0
                ? resolutionScheduleNodes[
                    Number(executionEntry.purpose.purposeIndex)
                  ]
                : undefined;
            const selectedSpendDescriptorCbor =
              selectedSpendItem === undefined
                ? undefined
                : ledgerDescriptorState.get(
                    selectedSpendItem.key.toString("hex"),
                  );
            if (
              selectedSpendItem !== undefined &&
              selectedSpendDescriptorCbor === undefined
            ) {
              throw new Error(
                "CEK spend finalization lost its authenticated ledger descriptor",
              );
            }
            const authenticatedScriptInfo =
              selectedSpendItem === undefined
                ? contextParts.scriptInfo
                : cardanoSpendScriptInfoSummary(
                    selectedSpendItem.key,
                    decodeMidgardLedgerOutputCommitmentV1(
                      selectedSpendDescriptorCbor!,
                    ).cardanoSpendDatum,
                  );
            if (
              !sameSummary(authenticatedScriptInfo, contextParts.scriptInfo)
            ) {
              throw new Error(
                "CEK descriptor-derived script info differs from the evaluated context",
              );
            }
            const partsControl: MidgardCekContextPartsControlV1 = {
              redeemerItems: redeemerControl.mapItems,
              redeemer: redeemerControl.currentRedeemer,
              scriptInfo: authenticatedScriptInfo,
            };
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              selectedSpendItem === undefined
                ? {
                    kind: "cekContextFinalize",
                    redeemerControl,
                  }
                : {
                    kind: "cekContextFinalizeSpend",
                    redeemerControl,
                    itemIndex: Number(executionEntry.purpose.purposeIndex),
                    key: selectedSpendItem.key,
                    descriptorCbor: selectedSpendDescriptorCbor!,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      resolvedLeaves,
                      Number(executionEntry.purpose.purposeIndex),
                    ).siblings,
                  },
            );
            contextControl = {
              ...contextControl,
              stage: 11,
              redeemerContextControlHash:
                hashMidgardCekContextPartsControlV1(partsControl),
            };
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekContextAssemble",
                control: partsControl,
              },
            );
            const assemblyControl: MidgardCekTxInfoAssemblyControlV1 = {
              tailFields: contextParts.tailFields,
              redeemer: contextParts.redeemer,
              scriptInfo: authenticatedScriptInfo,
            };
            contextControl = {
              ...contextControl,
              stage: 12,
              redeemerContextControlHash:
                hashMidgardCekTxInfoAssemblyControlV1(assemblyControl),
            };
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekTxInfoFinalize",
                control: assemblyControl,
              },
            );
            const finalControl: MidgardCekFinalContextControlV1 = {
              txInfo: contextParts.txInfo,
              redeemer: contextParts.redeemer,
              scriptInfo: contextParts.scriptInfo,
            };
            contextControl = {
              ...contextControl,
              stage: 13,
              redeemerContextControlHash:
                hashMidgardCekFinalContextControlV1(finalControl),
            };
            if (
              !sameSummary(
                composeMidgardCekContextSummaryV1(finalControl),
                contextParts.context,
              )
            ) {
              throw new Error(
                "CEK final context composition differs from evaluation",
              );
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              { kind: "cekContextSeed", control: finalControl },
            );
            const contextWitness = evaluation.graph.constantWitnesses.get(
              Buffer.from(evaluation.graph.contextValueRoot).toString("hex"),
            );
            if (
              !Buffer.from(exactExecution.initialState.focusRoot).equals(
                Buffer.from(evaluation.graph.root),
              ) ||
              exactExecution.initialState.executionIndex !==
                BigInt(executionIndex) ||
              contextWitness?.kind !== "semanticConstant" ||
              !sameSummary(
                contextWitness.witness.payload,
                contextParts.context,
              ) ||
              contextWitness.witness.memory !== contextParts.context.memory
            ) {
              throw new Error(
                "CEK execution does not begin at its authenticated context state",
              );
            }

            for (const step of exactExecution.steps) {
              pushWitness(
                "cek",
                cekWitness({
                  contextControl: null,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                  activeStateHash: hashMidgardCekMachineStateV1(step.pre),
                  executionCpuLimit: selected.value.exUnits.steps,
                  executionMemoryLimit: selected.value.exUnits.memory,
                  programEnvelopeHash: contextControl.programEnvelopeHash,
                }),
                { kind: "cekCoreStep", step },
              );
              traceExecutionCpu = completedCpu + step.post.cpu;
              traceExecutionMemory = completedMemory + step.post.memory;
              const budgetExceeded =
                step.post.cpu > selected.value.exUnits.steps ||
                step.post.memory > selected.value.exUnits.memory;
              if (budgetExceeded || step.post.mode === "haltError") {
                if (
                  rejection === null ||
                  terminalPhase !== "cek" ||
                  rejection.code !== RejectCodes.PlutusScriptInvalid
                ) {
                  throw new Error(
                    "CEK failure transition disagrees with validation",
                  );
                }
                stoppedAtRejection = true;
                break;
              }
            }
            if (stoppedAtRejection) break;
            if (
              exactExecution.terminalState.mode !== "haltSuccess" ||
              evaluation.result.kind !== "accepted"
            ) {
              throw new Error(
                "CEK successful trace disagrees with local validation",
              );
            }
          }
          if (
            !stoppedAtRejection &&
            evaluationIndex !== scriptEvaluations.length
          ) {
            throw new Error(
              "CEK trace did not consume every local script evaluation",
            );
          }
          if (scriptExecutionEntries.length === 0) {
            pushWitness(
              "cek",
              cekWitness({
                contextControl: null,
                executionCursor: 0,
                completedCpu: 0n,
                completedMemory: 0n,
                activeStateHash: null,
                executionCpuLimit: 0n,
                executionMemoryLimit: 0n,
                programEnvelopeHash: null,
              }),
            );
          }
        }

        if (!stoppedAtRejection) {
          const mintAssets = [...phaseALedgerTx!.mint.assets];
          const mintLeaves = mintAssets.map((asset) =>
            hashMidgardMintAssetLeafV1({
              policyId: asset.policyId,
              assetName: asset.assetName,
              quantity: asset.quantity,
            }),
          );
          const valueContributions: ValidationValueContribution[] = [];
          for (const node of resolutionScheduleNodes) {
            if (node.sourceKind !== "spend") continue;
            const value = ledgerState.get(node.key.toString("hex"));
            if (value === undefined) {
              return yield* Effect.fail(
                new Error(
                  "value mutation planning lost a previously authenticated ledger value",
                ),
              );
            }
            valueContributions.push(
              ...midgardValueContributions(
                decodeMidgardTxOutput(value).value,
                1n,
              ),
            );
          }
          for (const outputCbor of outputCbors) {
            valueContributions.push(
              ...midgardValueContributions(
                decodeMidgardTxOutput(outputCbor).value,
                -1n,
              ),
            );
          }
          for (const asset of mintAssets) {
            valueContributions.push({
              unit: Buffer.concat([
                Buffer.from(asset.policyId),
                Buffer.from(asset.assetName),
              ]),
              quantityDelta: asset.quantity,
            });
          }
          const valueMutationSteps = yield* Effect.tryPromise({
            try: () => buildValidationValueMutationSteps(valueContributions),
            catch: (cause) =>
              cause instanceof Error
                ? cause
                : new Error("failed to build authenticated value mutations"),
          });
          const valueAccumulator = emptyValidationValueAccumulator();
          let valueReplayCursor = 0;
          let valueReplayAssetCursor = 0;
          let valueReplayValueHash = Buffer.alloc(32);
          let valueReplayAccumulator =
            initialMidgardResolvedInputsAccumulatorV1();
          let valueReplayRemainingScheduleHash =
            emptyMidgardInputResolutionScheduleV1();
          let valueOutputCursor = 0;
          let valueOutputAssetCursor = 0;
          let valueMintCursor = 0;
          let valueMutationCursor = 0;
          const valueAndMintControlCbor = (input: {
            readonly stage: number;
            readonly replayScheduleHash: Buffer;
            readonly replayCursor?: number;
            readonly replayAccumulator?: Buffer;
            readonly replayRemainingScheduleHash?: Buffer;
            readonly outputCursor?: number;
            readonly mintCursor?: number;
          }): Buffer =>
            encodeCbor([
              authenticatedNativeControlCbor,
              BigInt(input.stage),
              input.replayScheduleHash,
              BigInt(input.replayCursor ?? valueReplayCursor),
              BigInt(valueReplayAssetCursor),
              valueReplayValueHash,
              input.replayAccumulator ?? valueReplayAccumulator,
              input.replayRemainingScheduleHash ??
                valueReplayRemainingScheduleHash,
              BigInt(input.outputCursor ?? valueOutputCursor),
              BigInt(valueOutputAssetCursor),
              BigInt(input.mintCursor ?? valueMintCursor),
              encodeValidationValueAccumulator(valueAccumulator),
            ]);

          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 0,
              replayScheduleHash: emptyMidgardInputResolutionScheduleV1(),
            }),
          );
          valueReplayRemainingScheduleHash = resolutionScheduleHash;
          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 1,
              replayScheduleHash: resolutionScheduleHash,
            }),
          );

          if (!stoppedAtRejection) {
            for (const node of resolutionScheduleNodes) {
              const outRefHex = node.key.toString("hex");
              const outputCbor = ledgerState.get(outRefHex);
              const descriptorCbor = ledgerDescriptorState.get(outRefHex);
              if (outputCbor === undefined || descriptorCbor === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "value replay lost a previously authenticated ledger descriptor",
                  ),
                );
              }
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                }),
                {
                  kind: "resolvedInputReplay",
                  sourceKind: node.sourceKind,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value: descriptorCbor,
                },
              );
              const decodedValue = decodeMidgardTxOutput(outputCbor).value;
              const assets =
                node.sourceKind === "spend"
                  ? midgardValueAssets(decodedValue)
                  : [];
              const assetMaterial =
                buildMidgardLedgerOutputAssetFrontierV1(assets);
              if (node.sourceKind === "spend") {
                valueAccumulator.lovelaceDelta += decodedValue.lovelace;
              }
              if (assets.length > 0) {
                valueReplayAssetCursor = 1;
                valueReplayValueHash = hash32(descriptorCbor);
                for (
                  let assetIndex = 0;
                  assetIndex < assets.length;
                  assetIndex += 1
                ) {
                  const asset = assets[assetIndex]!;
                  const mutationStep = valueMutationSteps[valueMutationCursor];
                  if (mutationStep === undefined) {
                    return yield* Effect.fail(
                      new Error(
                        "value replay exhausted authenticated mutation steps",
                      ),
                    );
                  }
                  pushWitness(
                    "valueAndMint",
                    valueAndMintControlCbor({
                      stage: 2,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    {
                      kind: "valueInputAsset",
                      sourceKind: "spend",
                      key: node.key,
                      nextScheduleHash: node.nextScheduleHash,
                      descriptorCbor,
                      assetIndex,
                      policyId: asset.policyId,
                      assetName: asset.assetName,
                      quantity: asset.quantity,
                      assetFrontier: assetMaterial.frontier,
                      assetSiblings: buildMidgardValidationMerkleMembershipV1(
                        assetMaterial.leaves,
                        assetIndex,
                      ).siblings,
                      mutationStep,
                    },
                  );
                  if (
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 spend-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueReplayAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueReplayAssetCursor = 0;
              valueReplayValueHash = Buffer.alloc(32);
              valueReplayAccumulator =
                advanceMidgardResolvedInputsAccumulatorV1({
                  accumulator: valueReplayAccumulator,
                  sourceKind: node.sourceKind,
                  key: node.key,
                  value: descriptorCbor,
                });
              valueReplayRemainingScheduleHash = node.nextScheduleHash;
              valueReplayCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 2,
                replayScheduleHash: resolutionScheduleHash,
              }),
            );
            for (
              let outputIndex = 0;
              outputIndex < outputCbors.length;
              outputIndex += 1
            ) {
              const outputCbor = outputCbors[outputIndex]!;
              const descriptorCbor = admittedOutputDescriptorCbors[outputIndex];
              if (descriptorCbor === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "value replay lost an authenticated transaction-output descriptor",
                  ),
                );
              }
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 3,
                  replayScheduleHash: resolutionScheduleHash,
                }),
                {
                  kind: "valueOutputDescriptor",
                  outputIndex,
                  descriptorCbor,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    admittedOutputDescriptorLeafHashes,
                    outputIndex,
                  ).siblings,
                },
              );
              const decodedValue = decodeMidgardTxOutput(outputCbor).value;
              // E_MIN_ADA / MIN-ADA-TX (#618 ruling 1; R8 of decision 0005).
              // The mirror of the ValueAndMint stage-3 output-descriptor
              // conjunct in
              // onchain/aiken/lib/midgard/validation-machine-v1.ak, evaluated
              // in the same place: after the descriptor step's witness is
              // committed, before this output's Ada is folded into the
              // accumulator and before the asset cursor opens. `outputCbor` is
              // the canonical output preimage the descriptor's `total_length`
              // binds, so both halves price the same bytes.
              if (!outputCborMeetsMinAdaV1(outputCbor, decodedValue.lovelace)) {
                if (
                  rejection === null ||
                  terminalPhase !== "valueAndMint" ||
                  rejection.code !== RejectCodes.MinAda
                ) {
                  return yield* Effect.fail(
                    new Error(
                      `V1 output[${outputIndex.toString()}] is below the minimum-Ada floor but validation did not reject it with ${RejectCodes.MinAda} in ValueAndMint (rejected at ${terminalPhase}/${rejectionCode ?? "none"})`,
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              valueAccumulator.lovelaceDelta -= decodedValue.lovelace;
              const assets = midgardValueAssets(decodedValue);
              const assetMaterial =
                buildMidgardLedgerOutputAssetFrontierV1(assets);
              if (assets.length > 0) {
                valueOutputAssetCursor = 1;
                valueReplayValueHash = hash32(descriptorCbor);
                for (
                  let assetIndex = 0;
                  assetIndex < assets.length;
                  assetIndex += 1
                ) {
                  const asset = assets[assetIndex]!;
                  const mutationStep = valueMutationSteps[valueMutationCursor];
                  if (mutationStep === undefined) {
                    return yield* Effect.fail(
                      new Error(
                        "output replay exhausted authenticated value mutations",
                      ),
                    );
                  }
                  pushWitness(
                    "valueAndMint",
                    valueAndMintControlCbor({
                      stage: 3,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    {
                      kind: "valueOutputAsset",
                      outputIndex,
                      descriptorCbor,
                      assetIndex,
                      policyId: asset.policyId,
                      assetName: asset.assetName,
                      quantity: asset.quantity,
                      assetFrontier: assetMaterial.frontier,
                      assetSiblings: buildMidgardValidationMerkleMembershipV1(
                        assetMaterial.leaves,
                        assetIndex,
                      ).siblings,
                      mutationStep,
                    },
                  );
                  if (
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 output-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueOutputAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueOutputAssetCursor = 0;
              valueReplayValueHash = Buffer.alloc(32);
              valueOutputCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 3,
                replayScheduleHash: resolutionScheduleHash,
              }),
            );
            for (
              let mintIndex = 0;
              mintIndex < mintAssets.length;
              mintIndex += 1
            ) {
              const asset = mintAssets[mintIndex]!;
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 4,
                  replayScheduleHash: resolutionScheduleHash,
                }),
                {
                  kind: "valueMintAsset",
                  mintIndex,
                  policyId: Buffer.from(asset.policyId),
                  assetName: Buffer.from(asset.assetName),
                  quantity: asset.quantity,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    mintLeaves,
                    mintIndex,
                  ).siblings,
                  mutationStep: valueMutationSteps[valueMutationCursor]!,
                },
              );
              const mutationStep = valueMutationSteps[valueMutationCursor];
              if (mutationStep === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "mint replay exhausted authenticated value mutations",
                  ),
                );
              }
              if (
                mutationStep.postSeenAssetCount >
                input.consensusProfile.limits.maxDistinctAssetCount
              ) {
                if (
                  rejection === null ||
                  terminalPhase !== "valueAndMint" ||
                  rejection.code !== RejectCodes.AssetCount
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "V1 mint replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              applyValidationValueMutationStep(valueAccumulator, mutationStep);
              valueMutationCursor += 1;
              valueMintCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 4,
                replayScheduleHash: resolutionScheduleHash,
              }),
            );
            const valueIsPreserved =
              valueAccumulator.lovelaceDelta - phaseALedgerTx!.fee === 0n &&
              valueAccumulator.nonzeroAssetCount === 0;
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 5,
                replayScheduleHash: resolutionScheduleHash,
              }),
            );
            if (!valueIsPreserved) {
              if (
                rejection === null ||
                terminalPhase !== "valueAndMint" ||
                rejection.code !== RejectCodes.ValueNotPreserved
              ) {
                return yield* Effect.fail(
                  new Error("V1 value equation disagrees with validation"),
                );
              }
              stoppedAtRejection = true;
            } else {
              if (rejection !== null && terminalPhase === "valueAndMint") {
                return yield* Effect.fail(
                  new Error(
                    "V1 validation reports a ValueAndMint rejection but the authenticated value equation accepted",
                  ),
                );
              }
              let ledgerReplayCursor = 0;
              let ledgerReplayAccumulator =
                initialMidgardResolvedInputsAccumulatorV1();
              let ledgerReplayRemainingScheduleHash =
                emptyMidgardInputResolutionScheduleV1();
              let currentLedgerRoot = Buffer.from(priorLedgerRoot);
              let ledgerOutputCursor = 0;
              let operationFrontier = emptyValidationFrontier;
              let mutationIndex = 0;
              let pendingMutation:
                | {
                    readonly status: "authorized";
                    readonly kind: "delete" | "insert";
                    readonly key: Buffer;
                    readonly value: Buffer;
                    readonly proofFoldTrace: MidgardMpfProofFoldTraceV1;
                    readonly foldControl: null;
                  }
                | {
                    readonly status: "folding";
                    readonly kind: "delete" | "insert";
                    readonly key: Buffer;
                    readonly value: Buffer;
                    readonly proofFoldTrace: MidgardMpfProofFoldTraceV1;
                    readonly foldControl: MidgardMpfProofFoldTraceV1["initial"];
                  }
                | null = null;
              let ledgerResolvedInputsAccumulator =
                initialMidgardResolvedInputsAccumulatorV1();
              for (const node of resolutionScheduleNodes) {
                const value = ledgerDescriptorState.get(
                  node.key.toString("hex"),
                );
                if (value === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta context lost a previously authenticated ledger descriptor",
                    ),
                  );
                }
                ledgerResolvedInputsAccumulator =
                  advanceMidgardResolvedInputsAccumulatorV1({
                    accumulator: ledgerResolvedInputsAccumulator,
                    sourceKind: node.sourceKind,
                    key: node.key,
                    value,
                  });
              }
              const ledgerOutputDescriptorFrontier =
                buildMidgardValidationMerkleFrontierV1(
                  admittedOutputDescriptorLeafHashes,
                );
              const pendingMutationCbor = (): Buffer =>
                pendingMutation === null
                  ? Buffer.alloc(0)
                  : encodeCbor([
                      1n,
                      pendingMutation.status === "authorized" ? 0n : 1n,
                      pendingMutation.kind === "delete" ? 0n : 1n,
                      pendingMutation.key,
                      pendingMutation.value,
                      encodeMidgardMpfProofDescriptorV1(
                        pendingMutation.proofFoldTrace.descriptor,
                      ),
                      BigInt(pendingMutation.foldControl?.nextFrameIndex ?? -1),
                      pendingMutation.foldControl?.includingRoot ??
                        Buffer.alloc(0),
                      pendingMutation.foldControl?.excludingRoot ??
                        Buffer.alloc(0),
                      BigInt(
                        pendingMutation.foldControl?.expectedNextCursor ?? 0,
                      ),
                    ]);
              const ledgerDeltaControlCbor = (input: {
                readonly stage: number;
                readonly replayScheduleHash: Buffer;
              }): Buffer =>
                encodeCbor([
                  BigInt(resolutionItems.length),
                  ledgerResolvedInputsAccumulator,
                  BigInt(outputCbors.length),
                  encodeFrontierPeaks(ledgerOutputDescriptorFrontier),
                  BigInt(input.stage),
                  input.replayScheduleHash,
                  BigInt(ledgerReplayCursor),
                  ledgerReplayAccumulator,
                  ledgerReplayRemainingScheduleHash,
                  currentLedgerRoot,
                  BigInt(ledgerOutputCursor),
                  BigInt(operationFrontier.count),
                  pendingMutationCbor(),
                  encodeFrontierPeaks(operationFrontier),
                ]);
              ledgerReplayRemainingScheduleHash = resolutionScheduleHash;
              for (const node of resolutionScheduleNodes) {
                const value = ledgerDescriptorState.get(
                  node.key.toString("hex"),
                );
                if (value === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta replay lost a previously authenticated ledger descriptor",
                    ),
                  );
                }
                const mutationStep =
                  node.sourceKind === "spend"
                    ? (input.ledgerMutationSteps[mutationIndex] ?? null)
                    : null;
                if (node.sourceKind === "spend") {
                  if (
                    mutationStep === null ||
                    mutationStep.operation.type !== "delete" ||
                    !mutationStep.operation.key.equals(node.key) ||
                    !mutationStep.preRoot.equals(currentLedgerRoot)
                  ) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta deletion mutation does not match the authenticated spend schedule",
                      ),
                    );
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 0,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    {
                      kind: "ledgerDeltaOperation",
                      operationKind: "delete",
                      key: node.key,
                      value: Buffer.alloc(0),
                      mutationStep,
                      operationMembership:
                        ledgerDeltaOperationMembership(mutationIndex),
                    },
                  );
                  pendingMutation = {
                    status: "authorized",
                    kind: "delete",
                    key: Buffer.from(node.key),
                    value: Buffer.alloc(0),
                    proofFoldTrace: mutationStep.proofFoldTrace,
                    foldControl: null,
                  };
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 0,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaReplay",
                    sourceKind: node.sourceKind,
                    key: node.key,
                    nextScheduleHash: node.nextScheduleHash,
                    value,
                  },
                );
                ledgerReplayAccumulator =
                  advanceMidgardResolvedInputsAccumulatorV1({
                    accumulator: ledgerReplayAccumulator,
                    sourceKind: node.sourceKind,
                    key: node.key,
                    value,
                  });
                ledgerReplayRemainingScheduleHash = node.nextScheduleHash;
                ledgerReplayCursor += 1;
                if (node.sourceKind === "spend") {
                  if (mutationStep === null || pendingMutation === null) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta deletion lost its authenticated operation",
                      ),
                    );
                  }
                  pendingMutation = {
                    ...pendingMutation,
                    status: "folding",
                    kind: "delete",
                    key: Buffer.from(node.key),
                    value: Buffer.from(value),
                    foldControl: mutationStep.proofFoldTrace.initial,
                  };
                  for (const foldStep of mutationStep.proofFoldTrace.steps) {
                    if (
                      pendingMutation.foldControl !== foldStep.pre &&
                      (pendingMutation.foldControl.nextFrameIndex !==
                        foldStep.pre.nextFrameIndex ||
                        pendingMutation.foldControl.expectedNextCursor !==
                          foldStep.pre.expectedNextCursor ||
                        !pendingMutation.foldControl.includingRoot.equals(
                          foldStep.pre.includingRoot,
                        ) ||
                        !pendingMutation.foldControl.excludingRoot.equals(
                          foldStep.pre.excludingRoot,
                        ))
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "ledger-delta deletion proof fold is not contiguous",
                        ),
                      );
                    }
                    pushWitness(
                      "ledgerDelta",
                      ledgerDeltaControlCbor({
                        stage: 0,
                        replayScheduleHash: resolutionScheduleHash,
                      }),
                      {
                        kind: "ledgerDeltaProofFrame",
                        frame: foldStep.frame,
                        siblings: foldStep.membership.siblings,
                      },
                    );
                    pendingMutation = {
                      ...pendingMutation,
                      foldControl: foldStep.post,
                    };
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 0,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                  );
                  currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                  operationFrontier = appendMidgardValidationMerkleLeafV1(
                    operationFrontier,
                    hashMidgardValidationLedgerDeltaOperationV1(
                      authenticatedLedgerOps[mutationIndex]!,
                    ),
                  );
                  mutationIndex += 1;
                  pendingMutation = null;
                }
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 0,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              for (
                let outputIndex = 0;
                outputIndex < outputCbors.length;
                outputIndex += 1
              ) {
                const descriptorCbor =
                  admittedOutputDescriptorCbors[outputIndex];
                if (descriptorCbor === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta insertion lost an admitted output descriptor",
                    ),
                  );
                }
                const mutationStep = input.ledgerMutationSteps[mutationIndex];
                // The ledger trie key is §5.3's fixed-index input item
                // (`82 ‖ 58 20 tx_id ‖ 19 index_be16`, 38 bytes) — the same
                // bytes on-chain `ledger_outref_key` derives. `encodeCbor([txId,
                // index])` would spell indices 0–23 minimally and miss every key
                // the trie actually holds.
                const outputKey = encodeMidgardSpendInputItemV1({
                  txId: input.transactionId,
                  outputIndex,
                });
                if (
                  mutationStep === undefined ||
                  mutationStep.operation.type !== "insert" ||
                  !mutationStep.operation.key.equals(outputKey) ||
                  !mutationStep.operation.value.equals(descriptorCbor) ||
                  !mutationStep.preRoot.equals(currentLedgerRoot)
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta insertion mutation does not match the authenticated output frontier",
                    ),
                  );
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaOperation",
                    operationKind: "insert",
                    key: outputKey,
                    value: descriptorCbor,
                    mutationStep,
                    operationMembership:
                      ledgerDeltaOperationMembership(mutationIndex),
                  },
                );
                pendingMutation = {
                  status: "authorized",
                  kind: "insert",
                  key: Buffer.from(outputKey),
                  value: Buffer.from(descriptorCbor),
                  proofFoldTrace: mutationStep.proofFoldTrace,
                  foldControl: null,
                };
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaOutput",
                    outputIndex,
                    descriptorCbor,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      admittedOutputDescriptorLeafHashes,
                      outputIndex,
                    ).siblings,
                  },
                );
                ledgerOutputCursor += 1;
                pendingMutation = {
                  ...pendingMutation,
                  status: "folding",
                  foldControl: mutationStep.proofFoldTrace.initial,
                };
                for (const foldStep of mutationStep.proofFoldTrace.steps) {
                  if (
                    pendingMutation.foldControl !== foldStep.pre &&
                    (pendingMutation.foldControl.nextFrameIndex !==
                      foldStep.pre.nextFrameIndex ||
                      pendingMutation.foldControl.expectedNextCursor !==
                        foldStep.pre.expectedNextCursor ||
                      !pendingMutation.foldControl.includingRoot.equals(
                        foldStep.pre.includingRoot,
                      ) ||
                      !pendingMutation.foldControl.excludingRoot.equals(
                        foldStep.pre.excludingRoot,
                      ))
                  ) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta insertion proof fold is not contiguous",
                      ),
                    );
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 1,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    {
                      kind: "ledgerDeltaProofFrame",
                      frame: foldStep.frame,
                      siblings: foldStep.membership.siblings,
                    },
                  );
                  pendingMutation = {
                    ...pendingMutation,
                    foldControl: foldStep.post,
                  };
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                );
                currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                operationFrontier = appendMidgardValidationMerkleLeafV1(
                  operationFrontier,
                  hashMidgardValidationLedgerDeltaOperationV1(
                    authenticatedLedgerOps[mutationIndex]!,
                  ),
                );
                mutationIndex += 1;
                pendingMutation = null;
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 1,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              if (
                mutationIndex !== input.ledgerMutationSteps.length ||
                !currentLedgerRoot.equals(postLedgerRoot) ||
                commitMidgardValidationMerkleFrontierV1(
                  operationFrontier,
                ).equals(ledgerDeltaRoot) === false
              ) {
                return yield* Effect.fail(
                  new Error(
                    "ledger-delta replay did not reach its committed roots",
                  ),
                );
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
            }
          }
        }
      }
    }
    if (rejection !== null && !stoppedAtRejection) {
      return yield* Effect.fail(
        new Error(`V1 trace did not reach rejection phase ${terminalPhase}`),
      );
    }

    const terminalWitness: ValidationMachineWorkWitness = {
      phase: "terminal",
      programCounter: witnesses.length,
      cbor: encodeCbor([
        verdict === "accepted" ? 1n : 2n,
        rejectionCode === null
          ? Buffer.alloc(0)
          : Buffer.from(rejectionCode, "ascii"),
        postLedgerRoot,
        verdict === "accepted"
          ? encodeCbor([
              BigInt(ledgerDeltaFrontier.count),
              encodeFrontierPeaks(ledgerDeltaFrontier),
            ])
          : Buffer.from("80", "hex"),
      ]),
      auxiliary: null,
    };
    witnesses.push(terminalWitness);
    witnessExecutionBudgets.push({
      cpu: traceExecutionCpu,
      memory: traceExecutionMemory,
    });

    const eventKeyHash = hash32(input.eventKeyCbor);
    const rejectionCodeHash =
      rejectionCode === null
        ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
        : hashMidgardValidationRejectionCodeV1(rejectionCode);
    const states = witnesses.map((witness, index) => {
      const terminal = index === witnesses.length - 1;
      const budget = witnessExecutionBudgets[index]!;
      return {
        machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
        eventKeyHash,
        transactionId: Buffer.from(input.transactionId),
        transactionCommitment,
        validationContextHash,
        sourceKind: input.sourceKind,
        priorLedgerRoot,
        phase: witness.phase,
        programCounter: witness.programCounter,
        workRoot: hashMidgardValidationWorkWitnessV1({
          phase: witness.phase,
          programCounter: witness.programCounter,
          witnessCbor: witness.cbor,
        }),
        executionCpu: budget.cpu,
        executionMemory: budget.memory,
        verdict: terminal ? verdict : ("pending" as const),
        rejectionCodeHash: terminal
          ? rejectionCodeHash
          : MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        ledgerDeltaRoot,
      } satisfies MidgardValidationMachineStateV1;
    });
    if (states.length === 0) {
      return yield* Effect.fail(new Error("validation trace has no states"));
    }
    const tree = buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      verdict,
      rejectionCodeHash,
    );
    if (
      tree.descriptor.initialStateHash.equals(ZERO_32) ||
      tree.descriptor.terminalStateHash.equals(ZERO_32)
    ) {
      return yield* Effect.fail(
        new Error("validation trace endpoint hash must not be zero"),
      );
    }
    return {
      validationContextCbor: contextCbor,
      programMaterialSidecarCbor: Buffer.from(
        canonicalProgramMaterialSidecarCbor,
      ),
      states,
      witnesses,
      tree,
      verdict,
      rejectionCode,
      ledgerOps,
    };
  });
