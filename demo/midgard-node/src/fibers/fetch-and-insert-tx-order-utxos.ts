import { createHash } from "node:crypto";

import { midgardBoundedItemChunkCountV1 } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
  midgardCekProgramMaterialKindFromTagV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfileV1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveMidgardTxFieldReceiptAssetNameV1,
  reconstructMidgardTransactionV1,
  verifyMidgardV1TxFieldChunk,
} from "@al-ft/midgard-core/consensus-validation-v1";
import { collectMidgardV1AttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  CekProgramMaterialDB,
  ForcedTransactionsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "@/fibers/user-event-ingestion.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";

const rawDatum = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const datum = txOrderUTxO.utxo.datum;
      if (datum === undefined || datum === null) {
        throw new Error(
          `Missing inline datum for tx-order UTxO ${txOrderUTxO.utxo.txHash}#${txOrderUTxO.utxo.outputIndex.toString()}`,
        );
      }
      return Buffer.from(datum, "hex");
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to read tx-order inline datum",
        cause,
      }),
  });

/**
 * Fetches the currently visible tx-order UTxO set.
 *
 * This mirrors deposit and withdrawal ingestion: reconciling the full visible
 * set is safer than cursor-only scans when provider visibility lags.
 */
const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.TxOrderUTxOV1[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { txOrder } = yield* MidgardContracts;
    const fetchConfig: SDK.UserEventFetchConfig = {
      eventAddress: txOrder.spendingScriptAddress,
      eventPolicyId: txOrder.policyId,
      ...config,
    };
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    return yield* SDK.fetchTxOrderUTxOsV1Program(lucid, fetchConfig);
  });

type TxOrderPayloadV1 = SDK.TxOrderUTxOV1["datum"]["event"]["tx"];

const outRefKeyV1 = (reference: SDK.OutputReference): string =>
  `${reference.transactionId}#${reference.outputIndex.toString()}`;

const lucidOutRefV1 = (
  reference: SDK.OutputReference,
  label: string,
): Pick<UTxO, "txHash" | "outputIndex"> => {
  const outputIndex = Number(reference.outputIndex);
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(
      `${label} output index does not fit a non-negative safe integer`,
    );
  }
  return { txHash: reference.transactionId, outputIndex };
};

const canonicalHeaderV1 = (major: number, length: number): Buffer => {
  if (!Number.isSafeInteger(length) || length < 0) {
    throw new Error("canonical CBOR header length is invalid");
  }
  if (length < 24) return Buffer.from([(major << 5) | length]);
  if (length <= 0xff) return Buffer.from([(major << 5) | 24, length]);
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = (major << 5) | 25;
    header.writeUInt16BE(length, 1);
    return header;
  }
  if (length <= 0xffff_ffff) {
    const header = Buffer.alloc(5);
    header[0] = (major << 5) | 26;
    header.writeUInt32BE(length, 1);
    return header;
  }
  throw new Error("canonical CBOR header length exceeds uint32");
};

const fieldItemEncodedSizeV1 = (
  fieldIndex: number,
  itemLength: number,
): number => {
  if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
    return canonicalHeaderV1(2, itemLength).length + itemLength;
  }
  if (fieldIndex === 5) {
    if (itemLength <= 1) {
      throw new Error("mint policy item is missing its array-pair header");
    }
    return itemLength - 1;
  }
  if (fieldIndex === 6 || fieldIndex === 8) return itemLength;
  throw new Error(`unknown V1 field index ${fieldIndex.toString()}`);
};

const frontierMatchesV1 = (
  left: SDK.TxFieldReceiptV1["collection_proof"]["frontier"],
  right: SDK.TxFieldReceiptV1["collection_proof"]["frontier"],
): boolean =>
  left.length === right.length &&
  left.every(
    (peak, index) =>
      peak.height === right[index]!.height && peak.hash === right[index]!.hash,
  );

const receiptIsImmediatePredecessorV1 = (
  predecessor: SDK.TxFieldReceiptV1,
  successor: SDK.TxFieldReceiptV1,
): boolean => {
  const previous = predecessor.collection_proof;
  const current = successor.collection_proof;
  const previousChunkCount = midgardBoundedItemChunkCountV1(
    Number(previous.item_length),
  );
  const currentChunkCount = midgardBoundedItemChunkCountV1(
    Number(current.item_length),
  );
  const currentFinalChunk =
    Number(successor.chunk_index) + 1 === currentChunkCount;
  const successorSizeBeforeItem =
    Number(successor.field_encoded_size) -
    (currentFinalChunk
      ? fieldItemEncodedSizeV1(
          Number(current.field_index),
          Number(current.item_length),
        )
      : 0);

  if (Number(successor.chunk_index) > 0) {
    return (
      Data.to(previous, SDK.BoundedCollectionItemProofV1) ===
        Data.to(current, SDK.BoundedCollectionItemProofV1) &&
      Number(predecessor.chunk_index) + 1 === Number(successor.chunk_index) &&
      Number(predecessor.field_encoded_size) === successorSizeBeforeItem
    );
  }
  if (Number(current.item_index) > 0) {
    return (
      Number(previous.field_index) === Number(current.field_index) &&
      Number(previous.item_count) === Number(current.item_count) &&
      Number(previous.item_index) + 1 === Number(current.item_index) &&
      frontierMatchesV1(previous.frontier, current.frontier) &&
      Number(predecessor.chunk_index) + 1 === previousChunkCount &&
      Number(predecessor.field_encoded_size) === successorSizeBeforeItem
    );
  }
  return (
    Number(previous.field_index) < Number(current.field_index) &&
    Number(previous.item_index) + 1 === Number(previous.item_count) &&
    Number(predecessor.chunk_index) + 1 === previousChunkCount
  );
};

export const reconstructTxOrderMaterialV1 = ({
  lucid,
  txOrderId,
  payload,
  txOrderPolicyId,
  fieldPreimageAddress,
  fieldReceiptPolicyId,
  fieldReceiptAddress,
}: {
  readonly lucid: LucidEvolution;
  readonly txOrderId: SDK.OutputReference;
  readonly payload: TxOrderPayloadV1;
  readonly txOrderPolicyId: string;
  readonly fieldPreimageAddress: string;
  readonly fieldReceiptPolicyId: string;
  readonly fieldReceiptAddress: string;
}): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const transactionId = Buffer.from(payload.tx_id, "hex");
      const transactionCommitment = Buffer.from(
        payload.transaction_commitment,
        "hex",
      );
      const source = {
        compactCbor: Buffer.from(payload.source.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          payload.source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          payload.source.field_preimage_lengths_cbor,
          "hex",
        ),
      };
      const fieldLengths = decodeMidgardNativeTxProofFieldLengthsV1(
        source.fieldPreimageLengthsCbor,
      );
      const maximumReceiptSteps = fieldLengths.reduce(
        (total, length) => total + length,
        0,
      );
      const fieldPreimages = fieldLengths.map((length) => Buffer.alloc(length));
      const seenFields = new Set<number>();
      const txOrderIdCbor = Data.to(txOrderId, SDK.OutputReference);

      const resolveOne = async (
        reference: SDK.OutputReference,
        label: string,
      ): Promise<UTxO> => {
        const utxos = await lucid.utxosByOutRef([
          lucidOutRefV1(reference, label),
        ]);
        if (utxos.length !== 1) {
          throw new Error(
            `${label} ${outRefKeyV1(reference)} resolved to ${utxos.length.toString()} UTxOs`,
          );
        }
        return utxos[0]!;
      };

      if (payload.terminal_receipt_reference === null) {
        for (const [fieldIndex, length] of fieldLengths.entries()) {
          if (length !== 1) {
            throw new Error(
              `empty material field ${fieldIndex.toString()} has committed length ${length.toString()}`,
            );
          }
          fieldPreimages[fieldIndex] = Buffer.from([0x80]);
        }
      } else {
        let reference: SDK.OutputReference | null =
          payload.terminal_receipt_reference;
        let successor: SDK.TxFieldReceiptV1 | undefined;
        let finalReceipt: SDK.TxFieldReceiptV1 | undefined;
        let firstReceipt: SDK.TxFieldReceiptV1 | undefined;
        let steps = 0;

        while (reference !== null) {
          steps += 1;
          if (steps > maximumReceiptSteps) {
            throw new Error(
              `receipt chain exceeds its committed-byte bound ${maximumReceiptSteps.toString()}`,
            );
          }
          const receiptUtxo = await resolveOne(reference, "V1 field receipt");
          if (
            receiptUtxo.address !== fieldReceiptAddress ||
            receiptUtxo.datum == null
          ) {
            throw new Error(
              `field receipt ${outRefKeyV1(reference)} is not an inline datum at the compiled receipt validator`,
            );
          }
          const receipt = SDK.decodeTxFieldReceiptV1Cbor(
            Buffer.from(receiptUtxo.datum, "hex"),
          );
          const collection = receipt.collection_proof;
          const fieldIndex = Number(collection.field_index);
          const itemIndex = Number(collection.item_index);
          const itemCount = Number(collection.item_count);
          const itemLength = Number(collection.item_length);
          const chunkIndex = Number(receipt.chunk_index);
          const fieldEncodedSize = Number(receipt.field_encoded_size);
          const chunkCount = midgardBoundedItemChunkCountV1(itemLength);
          if (
            collection.version !== 1n ||
            !Number.isSafeInteger(fieldIndex) ||
            fieldIndex < 0 ||
            fieldIndex >= 9 ||
            !Number.isSafeInteger(itemIndex) ||
            itemIndex < 0 ||
            !Number.isSafeInteger(itemCount) ||
            itemCount <= 0 ||
            itemIndex >= itemCount ||
            !Number.isSafeInteger(itemLength) ||
            itemLength < 0 ||
            !Number.isSafeInteger(chunkIndex) ||
            chunkIndex < 0 ||
            chunkIndex >= chunkCount ||
            !Number.isSafeInteger(fieldEncodedSize) ||
            fieldEncodedSize < 0 ||
            fieldEncodedSize > fieldLengths[fieldIndex]!
          ) {
            throw new Error(
              `field receipt ${outRefKeyV1(reference)} has invalid cursor metadata`,
            );
          }
          const receiptAssetName = deriveMidgardTxFieldReceiptAssetNameV1({
            txOrderPolicyId: Buffer.from(txOrderPolicyId, "hex"),
            txOrderTransactionId: Buffer.from(txOrderId.transactionId, "hex"),
            txOrderOutputIndex: txOrderId.outputIndex,
            transactionCommitment,
            fieldIndex,
            itemIndex,
            chunkIndex,
          }).toString("hex");
          const receiptUnit = toUnit(fieldReceiptPolicyId, receiptAssetName);
          const receiptPolicyTokens = Object.entries(receiptUtxo.assets).filter(
            ([unit, quantity]) =>
              unit.startsWith(fieldReceiptPolicyId) && quantity !== 0n,
          );
          if (
            receipt.field_receipt_policy_id !== fieldReceiptPolicyId ||
            receipt.tx_order_policy_id !== txOrderPolicyId ||
            Data.to(receipt.tx_order_id, SDK.OutputReference) !==
              txOrderIdCbor ||
            receipt.transaction_commitment !== payload.transaction_commitment ||
            receiptPolicyTokens.length !== 1 ||
            (receiptUtxo.assets[receiptUnit] ?? 0n) !== 1n
          ) {
            throw new Error(
              `field receipt ${outRefKeyV1(reference)} has invalid identity or receipt token`,
            );
          }
          const finalChunk = chunkIndex + 1 === chunkCount;
          const itemEncodedSize = fieldItemEncodedSizeV1(
            fieldIndex,
            itemLength,
          );
          const sizeBeforeItem =
            fieldEncodedSize - (finalChunk ? itemEncodedSize : 0);
          const fieldHeader = canonicalHeaderV1(
            fieldIndex === 5 ? 5 : 4,
            itemCount,
          );
          if (
            sizeBeforeItem < fieldHeader.length ||
            (itemIndex === 0 && sizeBeforeItem !== fieldHeader.length) ||
            (finalChunk &&
              itemIndex + 1 === itemCount &&
              fieldEncodedSize !== fieldLengths[fieldIndex])
          ) {
            throw new Error(
              `field receipt ${outRefKeyV1(reference)} has invalid encoded-size state`,
            );
          }
          if (successor === undefined) {
            if (!finalChunk || itemIndex + 1 !== itemCount) {
              throw new Error("terminal receipt is not at an item boundary");
            }
            finalReceipt = receipt;
          } else if (!receiptIsImmediatePredecessorV1(receipt, successor)) {
            throw new Error(
              `receipt ${outRefKeyV1(reference)} is not the immediate predecessor of its successor`,
            );
          }

          const fieldUtxo = await resolveOne(
            receipt.field_reference,
            "V1 field fragment",
          );
          if (
            fieldUtxo.address !== fieldPreimageAddress ||
            fieldUtxo.datum == null
          ) {
            throw new Error(
              `field fragment ${outRefKeyV1(receipt.field_reference)} is not an inline datum at the compiled field validator`,
            );
          }
          const field = SDK.decodeTxFieldPreimageV1Cbor(
            Buffer.from(fieldUtxo.datum, "hex"),
          );
          if (
            field.field_receipt_policy_id !== fieldReceiptPolicyId ||
            field.tx_order_policy_id !== txOrderPolicyId ||
            Data.to(field.tx_order_id, SDK.OutputReference) !== txOrderIdCbor ||
            field.transaction_commitment !== payload.transaction_commitment ||
            Data.to(
              field.collection_proof,
              SDK.BoundedCollectionItemProofV1,
            ) !==
              Data.to(
                receipt.collection_proof,
                SDK.BoundedCollectionItemProofV1,
              ) ||
            field.proof.field_index !== collection.field_index ||
            field.proof.item_index !== collection.item_index ||
            field.proof.total_length !== collection.item_length ||
            field.proof.chunk_index !== receipt.chunk_index
          ) {
            throw new Error(
              `field fragment ${outRefKeyV1(receipt.field_reference)} does not match its receipt`,
            );
          }
          const collectionProof = {
            version: 1 as const,
            fieldIndex,
            itemCount,
            itemIndex,
            itemLength,
            itemCommitment: Buffer.from(collection.item_commitment, "hex"),
            frontier: {
              count: itemCount,
              peaks: collection.frontier.map((peak) => ({
                height: Number(peak.height),
                hash: Buffer.from(peak.hash, "hex"),
              })),
            },
            siblings: collection.siblings.map((sibling) =>
              Buffer.from(sibling, "hex"),
            ),
          };
          const proof = {
            version: 1 as const,
            fieldIndex,
            itemIndex,
            totalLength: itemLength,
            chunkIndex,
            chunk: Buffer.from(field.proof.chunk, "hex"),
            frontier: {
              count: chunkCount,
              peaks: field.proof.frontier.map((peak) => ({
                height: Number(peak.height),
                hash: Buffer.from(peak.hash, "hex"),
              })),
            },
            siblings: field.proof.siblings.map((sibling) =>
              Buffer.from(sibling, "hex"),
            ),
          };
          verifyMidgardV1TxFieldChunk({
            transactionId,
            transactionCommitment,
            source,
            collectionProof,
            proof,
          });

          const target = fieldPreimages[fieldIndex]!;
          fieldHeader.copy(target, 0);
          const itemStart = sizeBeforeItem;
          const chunkOffset = chunkIndex * 4_095;
          if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
            const bytesHeader = canonicalHeaderV1(2, itemLength);
            bytesHeader.copy(target, itemStart);
            proof.chunk.copy(
              target,
              itemStart + bytesHeader.length + chunkOffset,
            );
          } else if (fieldIndex === 5) {
            const sourceOffset = Math.max(0, 1 - chunkOffset);
            proof.chunk.copy(
              target,
              itemStart + Math.max(0, chunkOffset - 1),
              sourceOffset,
            );
          } else {
            proof.chunk.copy(target, itemStart + chunkOffset);
          }
          seenFields.add(fieldIndex);
          successor = receipt;
          firstReceipt = receipt;
          reference = receipt.predecessor_receipt_reference;
        }
        if (
          finalReceipt === undefined ||
          firstReceipt === undefined ||
          firstReceipt.collection_proof.item_index !== 0n ||
          firstReceipt.chunk_index !== 0n
        ) {
          throw new Error(
            "receipt chain does not terminate at its first cursor",
          );
        }
        for (const [fieldIndex, length] of fieldLengths.entries()) {
          if (!seenFields.has(fieldIndex)) {
            if (length !== 1) {
              throw new Error(
                `unpublished field ${fieldIndex.toString()} is not canonically empty`,
              );
            }
            fieldPreimages[fieldIndex] = Buffer.from([0x80]);
          }
        }
      }

      return reconstructMidgardTransactionV1({
        transactionId,
        transactionCommitment,
        source,
        fieldPreimages,
      });
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to walk and reconstruct the authenticated V1 tx-order material chain",
        cause,
      }),
  });

const txOrderUTxOToEntry = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  lucid: LucidEvolution,
  txOrderPolicyId: string,
  fieldPreimageAddress: string,
  fieldReceiptPolicyId: string,
  fieldReceiptAddress: string,
  publishedProgramMaterial: readonly MidgardCekProgramMaterialEntryV1[],
): Effect.Effect<
  ForcedTransactionsDB.Entry,
  SDK.LucidError | DatabaseError,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const inclusionTime = txOrderUTxO.inclusionTime;
    const datum = yield* rawDatum(txOrderUTxO);
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const txOrderUTxOV1 = txOrderUTxO;
    const payload = txOrderUTxOV1.datum.event.tx;
    const nativeTxCbor = yield* reconstructTxOrderMaterialV1({
      lucid,
      txOrderId: txOrderUTxOV1.datum.event.id,
      payload,
      txOrderPolicyId,
      fieldPreimageAddress,
      fieldReceiptPolicyId,
      fieldReceiptAddress,
    });
    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor);
    const attachedProgramEnvelopes = (() => {
      try {
        return collectMidgardV1AttachedProgramEnvelopes(decoded);
      } catch {
        return Object.freeze([]);
      }
    })();
    if (attachedProgramEnvelopes.length > 0) {
      yield* CekProgramMaterialDB.persistVerifiedBundles(
        attachedProgramEnvelopes,
        publishedProgramMaterial,
      ).pipe(
        Effect.catchAll((cause) =>
          Effect.logWarning(
            `V1 tx-order ${payload.tx_id} is visible before its complete L1 CEK material bundle: ${String(cause)}`,
          ),
        ),
      );
    }
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      operatorValidity: decoded.validity,
      consensusProfile: consensusProfile satisfies MidgardConsensusProfileV1,
    });
    if (payload.tx_id !== encoded.txId.toString("hex")) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "V1 tx-order transaction id does not match its canonical transaction",
          cause: `datum=${payload.tx_id},derived=${encoded.txId.toString("hex")}`,
        }),
      );
    }
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecarV1(
      [],
    );
    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
        txOrderUTxOV1.idCbor,
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.from(
        txOrderUTxOV1.utxo.txHash,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]:
        txOrderUTxOV1.utxo.outputIndex,
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from(
        txOrderUTxOV1.assetName,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: datum,
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: decoded.validity,
      [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
        consensusProfile.profileId,
      [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
      [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
        encoded.transactionCommitment,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
        programMaterialSidecarCbor,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
        createHash("sha256").update(programMaterialSidecarCbor).digest(),
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

export const publishedProgramMaterialEntries = (
  utxos: readonly UTxO[],
): {
  readonly entries: readonly MidgardCekProgramMaterialEntryV1[];
  readonly malformedCount: number;
} => {
  const entries: MidgardCekProgramMaterialEntryV1[] = [];
  let malformedCount = 0;
  for (const utxo of utxos) {
    try {
      if (utxo.datum == null) {
        throw new Error("material UTxO has no inline datum");
      }
      const datum = SDK.decodeCekProgramMaterialDatumV1Cbor(
        Buffer.from(utxo.datum, "hex"),
      );
      entries.push(
        decodeMidgardCekProgramMaterialEntryV1(
          encodeMidgardCekProgramMaterialEntryV1({
            kind: midgardCekProgramMaterialKindFromTagV1(datum.kind),
            root: Buffer.from(
              datum.root,
              "hex",
            ) as MidgardCekProgramMaterialEntryV1["root"],
            preimage: Buffer.from(datum.preimage, "hex"),
          }),
        ),
      );
    } catch {
      malformedCount += 1;
    }
  }
  return { entries: Object.freeze(entries), malformedCount };
};

export const reconcileVisibleTxOrderUTxOs = (
  config?: UserEventFetchBounds,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { consensusProfile } = yield* ContractDeploymentIdentity;
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const txOrderUTxOs: SDK.TxOrderUTxOV1[] = [
      ...(yield* fetchTxOrderUTxOs(lucid, consensusProfile, config)),
    ];
    const {
      txOrder,
      txOrderFieldPreimage,
      txOrderFieldReceipt,
      cekProgramMaterial,
    } = yield* MidgardContracts;
    const material = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(cekProgramMaterial.spendingScriptAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to resolve V1 L1 CEK program material",
          cause,
        }),
    }).pipe(Effect.map(publishedProgramMaterialEntries));
    if (material.malformedCount > 0) {
      yield* Effect.logWarning(
        `Ignored ${material.malformedCount.toString()} malformed UTxO(s) at the permissionless V1 CEK material address.`,
      );
    }
    if (material.entries.length > 0) {
      yield* CekProgramMaterialDB.persistVerifiedBundles([], material.entries);
    }
    return yield* persistVisibleUserEventUTxOs({
      visibleUtxos: txOrderUTxOs,
      toEntry: (utxo) =>
        txOrderUTxOToEntry(
          utxo,
          consensusProfile,
          lucid,
          txOrder.policyId,
          txOrderFieldPreimage.spendingScriptAddress,
          txOrderFieldReceipt.policyId,
          txOrderFieldReceipt.spendingScriptAddress,
          material.entries,
        ),
      insertEntries: ForcedTransactionsDB.insertEntries,
      emptyLogMessage: "No tx-order UTxOs found.",
      foundLogMessage: (count) => `${count} tx-order UTxO(s) found.`,
    });
  });

export const fetchAndInsertTxOrderUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logDebug("fetching TxOrderUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleTxOrderUTxOs();
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `Reconciled ${count} visible tx-order UTxO(s) into forced_transaction_utxos.`,
  });
});

export const fetchAndInsertTxOrderUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> =>
  runCommitTimeUserEventIngestionBarrier({
    inclusionTimeUpperBound,
    inclusionTimeUpperBoundOffsetMs: 1,
    startLogMessage: (upperBound) =>
      `Running commit-time tx-order ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `Commit-time tx-order barrier reconciled ${reconciledCount} tx-order UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleTxOrderUTxOs,
  });

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  | MidgardContracts
  | ContractDeploymentIdentity
  | Lucid
  | Database
  | NodeConfig
  | Globals
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "Fetch and insert TxOrderUTxOs.",
    spanName: "fetch-and-insert-tx-order-utxos-fiber",
    action: fetchAndInsertTxOrderUTxOs,
  });
