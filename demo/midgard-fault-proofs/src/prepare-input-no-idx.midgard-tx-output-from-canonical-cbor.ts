import {
  decodeMidgardNativeByteListPreimage,
  formatUnknownError,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type DecodedTransactionMaterial,
  decodeTransactionMaterial,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
} from "./prepare-double-spend.js";
import {
  byteAt,
  type Cursor,
  INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION,
  readAddress,
  readBytes,
  readValue,
  readVersionedScript,
  reject,
} from "./prepare-input-no-idx.read-value.js";

/**
 * Projects one canonical native output CBOR item to the exact
 * `MidgardTxOutput` PlutusData the step-04 redeemer carries.
 */
export const midgardTxOutputFromCanonicalCbor = (
  bytes: Buffer,
): SDK.MidgardTxOutput => {
  const entryTag = byteAt(bytes, 0, "output");
  if (entryTag < 0xa2 || entryTag > 0xa4) {
    return reject("malformed_native_output", "output map entry count");
  }
  const entryCount = entryTag - 0xa0;
  if (byteAt(bytes, 1, "output.address_key") !== 0) {
    return reject("malformed_native_output", "output.address key must be 0");
  }
  const [address, afterAddress] = readAddress(bytes, 2);
  if (byteAt(bytes, afterAddress, "output.value_key") !== 1) {
    return reject("malformed_native_output", "output.value key must be 1");
  }
  const [value, afterValue] = readValue(bytes, afterAddress + 1);
  const end: Cursor = { value: afterValue };
  let datumCbor: string | null = null;
  let scriptRef: SDK.MidgardVersionedScript | null = null;
  let cursor = end.value;
  if (entryCount > 2) {
    const extraKey = byteAt(bytes, cursor, "output.extra_key");
    cursor += 1;
    if (extraKey === 2) {
      const [datum, afterDatum] = readBytes(bytes, cursor, "output.datum");
      datumCbor = datum.toString("hex");
      cursor = afterDatum;
      if (entryCount === 4) {
        if (byteAt(bytes, cursor, "output.script_key") !== 3) {
          return reject(
            "malformed_native_output",
            "output.script_ref key must be 3",
          );
        }
        const [script, afterScript] = readVersionedScript(bytes, cursor + 1);
        scriptRef = script;
        cursor = afterScript;
      }
    } else if (extraKey === 3 && entryCount === 3) {
      const [script, afterScript] = readVersionedScript(bytes, cursor);
      scriptRef = script;
      cursor = afterScript;
    } else {
      return reject("malformed_native_output", "output optional field layout");
    }
  }
  if (cursor !== bytes.length) {
    return reject("malformed_native_output", "output has trailing bytes");
  }
  return {
    address,
    value,
    datum_cbor: datumCbor,
    script_ref: scriptRef,
  };
};

// ## Builder

/** step-02 material: the complete spend-inputs preimage of the bad tx. */
export type PreparedInputNoIdxInputsPreimageJson = {
  readonly badTxId: string;
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimage: readonly SDK.MidgardTxInput[];
  readonly badInputsIndex: number;
};

/**
 * step-04 material: the complete outputs preimage of the producing tx, carried
 * as the canonical `encode_midgard_tx_output` bytes. The structured
 * `MidgardTxOutput` PlutusData the redeemer needs contains a native-asset map,
 * which JSON cannot represent losslessly, so the artifact stores the canonical
 * items the on-chain step re-encodes and the submitter re-projects them.
 */
export type PreparedInputNoIdxOutputsPreimageJson = {
  readonly producingTxId: string;
  readonly producingTxOutputsHash: string;
  readonly outputsPreimageCbor: readonly string[];
  readonly badInputOutputIndex: string;
};

/**
 * Complete-item proof-fit measurement (§3.2/§3.3). Byte counts are of the
 * serialized PlutusData each step redeemer carries directly; no chunked or
 * multi-output representation is produced.
 */
export type PreparedInputNoIdxProofFit = {
  readonly step02InputsPreimageItemCount: number;
  readonly step02InputsPreimageDatumBytes: number;
  readonly step04OutputsPreimageItemCount: number;
  readonly step04OutputsPreimageDatumBytes: number;
  readonly badTxCompactCborBytes: number;
  readonly producingTxCompactCborBytes: number;
  /**
   * §5.1's envelope over field 0's items — the bytes §8.4 partitions on.
   *
   * #604: this replaced the retired `completeItemCarriage`/`step02Execution`
   * pair, which reported a direct/fold split that no longer exists. Step-02 has
   * one route; what varies is which §8 tier the preimage travels under, and
   * §8.4 decides that from this length alone.
   */
  readonly step02SpendInputsPreimageBytes: number;
  /** The tier §8.4 selects for that preimage: `Inline`, `RawUtxo` or `Certified`. */
  readonly step02CarriageTier: string;
};

export type PreparedInputNoIdxOutput = {
  readonly schemaVersion: typeof INPUT_NO_IDX_EVIDENCE_SCHEMA_VERSION;
  readonly violationId: typeof SDK.INPUT_NO_IDX_VIOLATION_ID;
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root opened by both membership proofs. */
  readonly transactionsPhasRoot: string;
  /** Counted, domain-separated root committed by the block header. */
  readonly committedTransactionsRoot: string;
  readonly expectedTransactionsRoot: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly evidence: SDK.InputNoIdxEvidence;
  /** step-01 argument: the bad transaction's inclusion in the block. */
  readonly badTxInclusion: PreparedTxInclusionJson;
  /** step-03 argument: the producing transaction's inclusion in the block. */
  readonly producingTxInclusion: PreparedTxInclusionJson;
  readonly step02: PreparedInputNoIdxInputsPreimageJson;
  readonly step02State: SDK.InputNoIdxStep02State;
  readonly step03State: SDK.InputNoIdxStep03State;
  readonly step04: PreparedInputNoIdxOutputsPreimageJson;
  /** In-memory projection of `step04.outputsPreimageCbor`. */
  readonly outputsPreimage: readonly SDK.MidgardTxOutput[];
  readonly step04State: SDK.InputNoIdxStep04State;
  readonly proofFit: PreparedInputNoIdxProofFit;
  readonly files?: {
    readonly badTxInclusionPath: string;
    readonly producingTxInclusionPath: string;
    readonly inputsPreimagePath: string;
    readonly outputsPreimagePath: string;
    readonly planPath: string;
  };
};

export const spendInputsOf = (
  tx: DecodedTransactionMaterial,
): readonly SDK.MidgardTxInput[] =>
  tx.inputs.map((input) => ({
    tx_id: input.transactionId.toLowerCase(),
    output_index: input.outputIndex,
  }));

export const nativeOutputItems = (
  tx: DecodedTransactionMaterial,
): readonly Buffer[] => {
  try {
    return decodeMidgardNativeByteListPreimage(
      tx.nativeTx.body.outputsPreimageCbor,
      `tx ${tx.nodeTxId} outputs`,
    ).map((bytes) => Buffer.from(bytes));
  } catch (cause) {
    return reject(
      "malformed_native_output",
      `tx ${tx.nodeTxId} outputs preimage is not a canonical native byte list: ${formatUnknownError(cause)}`,
    );
  }
};

export type Candidate = {
  readonly badTx: DecodedTransactionMaterial;
  readonly badInputsIndex: number;
  readonly badInput: SDK.MidgardTxInput;
  readonly producingTx: DecodedTransactionMaterial;
  readonly producingOutputs: readonly Buffer[];
};

export type InputNoIdxDetectedViolation = Readonly<{
  badTxIndex: number;
  badTxId: string;
  badInputsIndex: number;
  producingTxId: string;
  badInputOutputIndex: bigint;
  producingTxOutputCount: number;
}>;

/**
 * Complete same-block scan used by the sealed production replay bundle. An
 * input whose producer is absent belongs to `nonExistentInput`; only an
 * out-of-range index into a producer committed by this same block is emitted.
 */
export const detectInputNoIdxViolationsFromTransactions = async (
  transactions: readonly NodeTransactionPayload[],
): Promise<readonly InputNoIdxDetectedViolation[]> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const byTxId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));
  const detections: InputNoIdxDetectedViolation[] = [];
  for (const [badTxIndex, badTx] of decoded.entries()) {
    for (const [badInputsIndex, badInput] of spendInputsOf(badTx).entries()) {
      const producingTx = byTxId.get(badInput.tx_id);
      if (producingTx === undefined) continue;
      const producingTxOutputCount = nativeOutputItems(producingTx).length;
      if (
        SDK.isInputNoIdxViolation({
          badInputOutputIndex: badInput.output_index,
          producingTxOutputCount,
        })
      ) {
        detections.push(
          Object.freeze({
            badTxIndex,
            badTxId: badTx.nodeTxId,
            badInputsIndex,
            producingTxId: producingTx.nodeTxId,
            badInputOutputIndex: badInput.output_index,
            producingTxOutputCount,
          }),
        );
      }
    }
  }
  return Object.freeze(detections);
};
