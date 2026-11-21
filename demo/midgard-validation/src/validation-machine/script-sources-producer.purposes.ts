/**
 * ScriptSources stages 6 and 7: the mint policies folded asset by asset
 * into mint purposes, then the required observers and the protected
 * receive scripts, in ascending script-hash order, appended as purposes.
 */
import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardBoundedItemChunkProof,
  buildMidgardValidationMerkleMembership,
  hashMidgardMintAssetLeaf,
  hashMidgardScriptPurposeLeaf,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
} from "@al-ft/midgard-core/codec/cbor";
import { Effect } from "effect";

import {
  emptyMintFoldControl,
  type ResolvedInputReplay,
  type ScriptSourcesPrepared,
  type ScriptSourcesWitness,
  scriptSourcesWitnessCbor,
} from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import type { OutputAdmission } from "./script-sources-producer.outputs.js";
import type {
  MintFoldTraceControl,
  ScriptPurposeProofEntry,
} from "./trace-builder-types.js";
import type { TraceWalk } from "./trace-walk.js";

type PurposesPrepared = ScriptSourcesPrepared &
  Pick<
    PreparedValidationTransaction,
    | "fieldPreimages"
    | "mintCollection"
    | "requiredObserversCollection"
    | "outputFrontier"
  >;

/** What stage 7 leaves for discovery: every purpose, and the mint fold. */
export type ScriptPurposes = {
  readonly mintFoldControl: MintFoldTraceControl;
  readonly purposeFrontier: MidgardValidationMerkleFrontier;
  /** The mint, observer and receive purposes, in fold order. */
  readonly purposeEntries: readonly ScriptPurposeProofEntry[];
};

const purpose = (
  purposeKind: 1 | 2 | 3,
  purposeIndex: bigint,
  scriptHash: Buffer,
): ScriptPurposeProofEntry => ({
  purposeKind,
  purposeIndex,
  scriptHash,
  subject: scriptHash,
  leaf: hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex,
    scriptHash,
    subject: scriptHash,
  }),
});

/**
 * Stages 6 and 7, over stage 3's replay and stage 5's admission. A
 * malformed mint policy item is a defect: canonical decode admitted it.
 */
export const produceScriptPurposes = (
  prepared: PurposesPrepared,
  push: TraceWalk["push"],
  replay: ResolvedInputReplay,
  admission: Pick<
    OutputAdmission,
    "outputCursor" | "receiveScan" | "receiveSourceEntries"
  >,
): Effect.Effect<ScriptPurposes> =>
  Effect.sync(() => {
    const purposeEntries: ScriptPurposeProofEntry[] = [];
    const { items: policies } = prepared.mintCollection;
    let purposeFrontier = replay.purposeFrontier;
    let mintFoldControl = emptyMintFoldControl(
      prepared.emptyValidationFrontier,
    );
    const witness = (
      fields: Pick<ScriptSourcesWitness, "stage" | "outputCursor"> &
        Partial<ScriptSourcesWitness>,
    ): Buffer =>
      scriptSourcesWitnessCbor(prepared, {
        ...replay,
        purposeFrontier,
        outputFrontier: prepared.outputFrontier,
        receiveScan: admission.receiveScan,
        mintFoldControl,
        ...fields,
      });
    const mintWitness = (): Buffer =>
      witness({ stage: 6, outputCursor: admission.outputCursor });
    for (const policyItem of policies) {
      push("scriptSources", mintWitness(), {
        kind: "transactionFieldChunk",
        fieldIndex: 5,
        itemIndex: policyItem.itemIndex,
        fieldPreimage: preparedFieldPreimage(prepared, 5),
      });
      const itemHeader = readCborArrayHeader(
        policyItem.bytes,
        0,
        `v1.mint.policy[${policyItem.itemIndex}]`,
      );
      if (itemHeader.length !== 2) {
        throw new Error("V1 mint policy item must contain two fields");
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
      const purposeEntry = purpose(1, BigInt(policyItem.itemIndex), policyId);
      purposeEntries.push(purposeEntry);
      purposeFrontier = appendMidgardValidationMerkleLeaf(
        purposeFrontier,
        purposeEntry.leaf,
      );
      mintFoldControl = {
        ...mintFoldControl,
        policyCount: policies.length,
        activePolicy: policyId,
        itemLength: policyItem.bytes.length,
        itemCommitment: Buffer.from(policyItem.commitment),
        itemCursor: assets.nextOffset,
        assetsRemaining: assets.length,
        policyAssetCursor: 0,
        previousAsset: Buffer.alloc(0),
      };
      let assetCursor = assets.nextOffset;
      for (let assetIndex = 0; assetIndex < assets.length; assetIndex += 1) {
        const expectedChunkIndex = Math.floor(
          assetCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        );
        const nextChunkIndex =
          expectedChunkIndex + 1 <
          midgardBoundedItemChunkCount(policyItem.bytes.length)
            ? expectedChunkIndex + 1
            : null;
        push("scriptSources", mintWitness(), {
          kind: "mintFoldAsset",
          chunkProof: buildMidgardBoundedItemChunkProof(
            policyItem,
            expectedChunkIndex,
          ),
          nextChunkProof:
            nextChunkIndex === null
              ? null
              : buildMidgardBoundedItemChunkProof(policyItem, nextChunkIndex),
        });
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
        const nextAssetFrontier = appendMidgardValidationMerkleLeaf(
          mintFoldControl.assetFrontier,
          hashMidgardMintAssetLeaf({
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
              policyAssetCursor: mintFoldControl.policyAssetCursor + 1,
              previousAsset: Buffer.from(asset.value),
              assetFrontier: nextAssetFrontier,
            };
      }
      if (assetCursor !== policyItem.bytes.length) {
        throw new Error("V1 mint policy item has trailing bytes");
      }
    }
    push("scriptSources", mintWitness());
    // The empty-mint normalization belongs to the stage-six
    // transition. Its predecessor must retain the stage-five fold.
    if (policies.length === 0) {
      mintFoldControl = { ...mintFoldControl, policyCount: 0 };
    }

    const { items: observers } = prepared.requiredObserversCollection;
    let observerTotalCount = 0;
    let observerSeen = 0;
    let previousObserverHash = Buffer.alloc(0);
    const observerScan = () => ({
      totalCount: observerTotalCount,
      seen: observerSeen,
      previousHash: previousObserverHash,
    });
    for (const observer of observers) {
      push(
        "scriptSources",
        witness({ stage: 7, outputCursor: 0, observerScan: observerScan() }),
        {
          kind: "transactionFieldChunk",
          fieldIndex: 3,
          itemIndex: observer.itemIndex,
          fieldPreimage: preparedFieldPreimage(prepared, 3),
        },
      );
      if (observerTotalCount === 0) observerTotalCount = observers.length;
      const observerHash = observer.bytes;
      const purposeEntry = purpose(2, BigInt(observerSeen), observerHash);
      purposeEntries.push(purposeEntry);
      purposeFrontier = appendMidgardValidationMerkleLeaf(
        purposeFrontier,
        purposeEntry.leaf,
      );
      observerSeen += 1;
      previousObserverHash = observerHash;
    }

    const sources = admission.receiveSourceEntries;
    const sourceLeaves = sources.map((entry) => entry.leaf);
    let sourceCursor = 0;
    let receiveCount = 0;
    let previousHash = Buffer.alloc(0);
    let candidateHash = Buffer.alloc(0);
    const receiveWitness = (): Buffer =>
      witness({
        stage: 7,
        outputCursor: sourceCursor,
        receiveScan: {
          sourceFrontier: admission.receiveScan.sourceFrontier,
          receiveCount,
          previousHash,
          candidateHash,
          descriptorFrontier: admission.receiveScan.descriptorFrontier,
        },
        observerScan: observerScan(),
      });
    while (true) {
      if (sourceCursor === sources.length) {
        push("scriptSources", receiveWitness());
        if (candidateHash.length === 0) break;
        const scriptHash = candidateHash;
        const purposeEntry = purpose(3, BigInt(receiveCount), scriptHash);
        purposeEntries.push(purposeEntry);
        purposeFrontier = appendMidgardValidationMerkleLeaf(
          purposeFrontier,
          purposeEntry.leaf,
        );
        receiveCount += 1;
        previousHash = scriptHash;
        candidateHash = Buffer.alloc(0);
        sourceCursor = 0;
        continue;
      }
      const source = sources[sourceCursor]!;
      push("scriptSources", receiveWitness(), {
        kind: "scriptPurposeScan",
        purposeKind: 3,
        purposeIndex: BigInt(sourceCursor),
        scriptHash: source.scriptHash,
        subject: source.subject,
        siblings: buildMidgardValidationMerkleMembership(
          sourceLeaves,
          sourceCursor,
        ).siblings,
      });
      const scriptHash = source.scriptHash;
      if (
        (previousHash.length === 0 ||
          Buffer.compare(previousHash, scriptHash) < 0) &&
        (candidateHash.length === 0 ||
          Buffer.compare(scriptHash, candidateHash) < 0)
      ) {
        candidateHash = scriptHash;
      }
      sourceCursor += 1;
    }
    return { mintFoldControl, purposeFrontier, purposeEntries };
  });
