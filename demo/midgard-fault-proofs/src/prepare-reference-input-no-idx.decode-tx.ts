import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
  formatUnknownError,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  type MidgardTxInput,
  type NativeTxCompact as NativeTxCompactData,
} from "@al-ft/midgard-sdk";

import { parseHex } from "./json-file.js";
import {
  deriveL2TransactionSourceCbor,
  type FetchLike,
  type NodeTransactionPayload,
} from "./prepare-double-spend.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import { nativeTxFromCoreCompact } from "./step-support.js";

/**
 * One reference input of the bad transaction, as committed by its native
 * `reference_inputs_hash`. Consumed by
 * `submit-reference-input-no-idx-step-02.ts`, which re-commits the whole list
 * against the hash the on-chain step-02 datum carries.
 */
export type ReferenceInputNoIdxPreimageEntry = {
  readonly txId: string;
  readonly index: number | bigint;
};

/** step-01/step-03 material: a committed tx and its membership proof. */
export type PreparedReferenceInputNoIdxTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  // Raw transactions MPF root the membership proof opens; authenticated on-chain
  // against the header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export type PrepareReferenceInputNoIdxCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareReferenceInputNoIdxFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
};

export type PreparedReferenceInputNoIdxOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root used by the transaction membership proofs. */
  readonly transactionsRoot: string;
  /** Counted root committed in the state-queue block header. */
  readonly committedTransactionsRoot: string;
  readonly badTxId: string;
  /** Index of the offending input inside the bad transaction's reference list. */
  readonly badReferenceInputIndex: number;
  /** The offending reference input: its `tx_id` is the producing tx, `output_index` is out of range. */
  readonly badReferenceInput: MidgardTxInput;
  readonly producingTxId: string;
  readonly producingTxOutputCount: number;
  readonly expectedTransactionsRoot?: {
    readonly value: string;
    readonly matches: boolean;
  };
  /** step-01 material: the bad tx and its membership proof. */
  readonly badTxInclusion: PreparedReferenceInputNoIdxTxInclusionJson;
  /** step-02 material: the bad tx's reference-inputs preimage. */
  readonly referenceInputsPreimage: readonly ReferenceInputNoIdxPreimageEntry[];
  /** step-03 material: the producing tx and its membership proof. */
  readonly producingTxInclusion: PreparedReferenceInputNoIdxTxInclusionJson;
  /**
   * step-04 material: the producing tx's canonical outputs preimage, one raw
   * `encode_midgard_tx_output` item per output. The step-04 redeemer carries
   * these as structured `MidgardTxOutput` PlutusData, which JSON cannot hold
   * losslessly (native-asset maps), so the artifact stores the canonical items
   * the on-chain step re-encodes.
   */
  readonly outputsPreimageCbor: readonly string[];
  readonly files?: {
    readonly badTxInclusionPath: string;
    readonly referenceInputsPreimagePath: string;
    readonly producingTxInclusionPath: string;
    readonly outputsPreimagePath: string;
    readonly planPath: string;
  };
};

export type DecodedTx = {
  readonly nodeTxId: string;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly nativeCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly referenceInputs: readonly MidgardTxInput[];
  /** Producing transaction's outputs preimage, one raw CBOR hex string per output. */
  readonly outputsPreimageCbor: readonly string[];
};

export type ReferenceInputNoIdxDetectedViolation = Readonly<{
  badTxIndex: number;
  badTxId: string;
  badReferenceInputIndex: number;
  producingTxId: string;
  badReferenceInputOutputIndex: bigint;
  producingTxOutputCount: number;
}>;

export const decodeTx = (payload: NodeTransactionPayload): DecodedTx => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFull;
  try {
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(txCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${nodeTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const computed = computeMidgardNativeTxId(nativeTx).toString("hex");
  if (computed !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computed}.`,
    );
  }
  const referenceInputCbors = decodeMidgardNativeByteListPreimage(
    nativeTx.body.referenceInputsPreimageCbor,
    `tx ${nodeTxId} reference_inputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  const outputsPreimageCbor = decodeMidgardNativeByteListPreimage(
    nativeTx.body.outputsPreimageCbor,
    `tx ${nodeTxId} outputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  return {
    nodeTxId,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    nativeCompactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    ),
    l2TransactionSourceCbor: deriveL2TransactionSourceCbor(
      Buffer.from(txCbor, "hex"),
    ),
    referenceInputs: spendInputsWitnessFromCbors(
      referenceInputCbors,
      "reference_inputs",
    ).inputs,
    outputsPreimageCbor,
  };
};

export const parseBadReferenceInputIndex = (
  value: string | number | undefined,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(
      `--bad-reference-input-index must be a non-negative integer, got "${String(value)}".`,
    );
  }
  return parsed;
};

/**
 * Complete same-block scan used by the sealed production replay bundle. A
 * reference input whose producer is absent belongs to `noReferenceInput`; only
 * an out-of-range index into a producer committed by this same block is
 * emitted here.
 */
export const detectReferenceInputNoIdxViolationsFromTransactions = (
  transactions: readonly NodeTransactionPayload[],
): readonly ReferenceInputNoIdxDetectedViolation[] => {
  const decoded = transactions.map(decodeTx);
  const byId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));
  const detections: ReferenceInputNoIdxDetectedViolation[] = [];
  for (const [badTxIndex, badTx] of decoded.entries()) {
    for (const [
      badReferenceInputIndex,
      input,
    ] of badTx.referenceInputs.entries()) {
      const producingTx = byId.get(input.tx_id);
      if (producingTx === undefined) continue;
      if (input.output_index < producingTx.outputsPreimageCbor.length) continue;
      detections.push(
        Object.freeze({
          badTxIndex,
          badTxId: badTx.nodeTxId,
          badReferenceInputIndex,
          producingTxId: producingTx.nodeTxId,
          badReferenceInputOutputIndex: input.output_index,
          producingTxOutputCount: producingTx.outputsPreimageCbor.length,
        }),
      );
    }
  }
  return Object.freeze(detections);
};
