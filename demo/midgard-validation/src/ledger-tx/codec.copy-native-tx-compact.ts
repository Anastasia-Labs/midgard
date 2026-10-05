import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_MAX_OUTPUT_INDEX,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec/forced";

import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
  MidgardOutRef,
  MidgardScriptHash,
  MidgardSubmittedTx,
  MidgardTxId,
} from "./types.js";

const HASH32_LENGTH = 32;

export const HASH28_LENGTH = 28;

export const MAX_ASSET_NAME_LENGTH = 32;

export const failDecode = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

export const failEncode = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.CborEncode,
    message,
    detail,
  );
};

export type MidgardLedgerTxDecodeStage = "canonical-cbor" | "ledger";

export class MidgardLedgerTxDecodeError extends Error {
  readonly stage: MidgardLedgerTxDecodeStage;
  readonly causeValue: unknown;
  readonly invalidOutput: boolean;
  /** Position of the output that failed to decode, when one item did. */
  readonly invalidOutputIndex: number | undefined;

  constructor(
    stage: MidgardLedgerTxDecodeStage,
    causeValue: unknown,
    invalidOutput = false,
    invalidOutputIndex?: number,
  ) {
    super(`failed to decode Midgard ledger tx at ${stage} stage`);
    this.name = "MidgardLedgerTxDecodeError";
    this.stage = stage;
    this.causeValue = causeValue;
    this.invalidOutput = invalidOutput;
    this.invalidOutputIndex = invalidOutputIndex;
  }
}

export type MidgardProjectedRawScriptWitness = Readonly<{
  index: number;
  languageTag: 0 | 3 | 128;
  scriptBytes: Buffer;
  versionedItemBytes: Buffer;
  hash: MidgardScriptHash;
}>;

export type MidgardRawEnvelopePhaseAProjection = Readonly<{
  canonical:
    | ReturnType<typeof deriveMidgardNativeTxFaultEvidenceMaterial>["canonical"]
    | ReturnType<
        typeof deriveMidgardForcedTxFaultEvidenceMaterial
      >["canonical"];
  transactionId: Buffer;
  scriptWitnesses: readonly MidgardProjectedRawScriptWitness[];
  ledgerTx: Omit<
    MidgardLedgerTx,
    "scriptWitnesses" | "nativeScriptHashes" | "plutusScriptHashes"
  > &
    Readonly<{
      scriptWitnesses: readonly MidgardProjectedRawScriptWitness[];
      nativeScriptHashes: readonly MidgardScriptHash[];
      plutusScriptHashes: readonly MidgardScriptHash[];
    }>;
  canonicalSubmittedTx: MidgardSubmittedTx | null;
}>;

export class MidgardLedgerOutputDecodeError extends Error {
  readonly causeValue: unknown;
  /** Position of the output item that failed, unless the list itself did. */
  readonly outputIndex: number | undefined;

  constructor(causeValue: unknown, outputIndex?: number) {
    super("failed to decode Midgard ledger output");
    this.name = "MidgardLedgerOutputDecodeError";
    this.causeValue = causeValue;
    this.outputIndex = outputIndex;
  }
}

export const copyBuffer = (value: Uint8Array): Buffer => Buffer.from(value);

export const optionalPosixTime = (value: bigint): bigint | undefined =>
  value === MIDGARD_POSIX_TIME_NONE ? undefined : value;

export const encodeOptionalPosixTime = (value: bigint | undefined): bigint =>
  value ?? MIDGARD_POSIX_TIME_NONE;

export const optionalNetworkId = (value: bigint): bigint | undefined =>
  value === MIDGARD_NATIVE_NETWORK_ID_NONE ? undefined : value;

export const encodeOptionalNetworkId = (value: bigint | undefined): bigint =>
  value ?? MIDGARD_NATIVE_NETWORK_ID_NONE;

const assertByteLength = (
  value: Uint8Array,
  length: number,
  fieldName: string,
): Buffer => {
  const bytes = copyBuffer(value);
  if (bytes.length !== length) {
    failEncode(
      `${fieldName} must be ${length} bytes`,
      `length=${bytes.length}`,
    );
  }
  return bytes;
};

export const assertHash32 = (value: Uint8Array, fieldName: string): Buffer =>
  assertByteLength(value, HASH32_LENGTH, fieldName);

export const assertHash28 = (value: Uint8Array, fieldName: string): Buffer =>
  assertByteLength(value, HASH28_LENGTH, fieldName);

export const assertBufferEquals = (
  fieldName: string,
  actual: Uint8Array,
  expected: Uint8Array,
): void => {
  const actualBuffer = copyBuffer(actual);
  const expectedBuffer = copyBuffer(expected);
  if (!actualBuffer.equals(expectedBuffer)) {
    failEncode(
      `${fieldName} mismatch`,
      `expected=${expectedBuffer.toString("hex")} actual=${actualBuffer.toString("hex")}`,
    );
  }
};

export const assertBufferArrayEquals = (
  fieldName: string,
  actual: readonly Uint8Array[],
  expected: readonly Uint8Array[],
): void => {
  if (actual.length !== expected.length) {
    failEncode(
      `${fieldName} length mismatch`,
      `expected=${expected.length} actual=${actual.length}`,
    );
  }
  for (let i = 0; i < actual.length; i += 1) {
    assertBufferEquals(`${fieldName}[${i}]`, actual[i], expected[i]);
  }
};

export const copyNativeTxCompact = (
  compact: MidgardNativeTxFull["compact"] | MidgardForcedTxFull["compact"],
): MidgardSubmittedTx["commitments"]["transactionCompact"] => ({
  version: compact.version,
  transactionBody: {
    spendInputsHash: copyBuffer(compact.transactionBody.spendInputsHash),
    referenceInputsHash: copyBuffer(
      compact.transactionBody.referenceInputsHash,
    ),
    outputsHash: copyBuffer(compact.transactionBody.outputsHash),
    fee: compact.transactionBody.fee,
    validityIntervalStart: compact.transactionBody.validityIntervalStart,
    validityIntervalEnd: compact.transactionBody.validityIntervalEnd,
    requiredObserversHash: copyBuffer(
      compact.transactionBody.requiredObserversHash,
    ),
    requiredSignersHash: copyBuffer(
      compact.transactionBody.requiredSignersHash,
    ),
    mintHash: copyBuffer(compact.transactionBody.mintHash),
    scriptIntegrityHash: copyBuffer(
      compact.transactionBody.scriptIntegrityHash,
    ),
    auxiliaryDataHash: copyBuffer(compact.transactionBody.auxiliaryDataHash),
    networkId: compact.transactionBody.networkId,
  },
  transactionWitnessSetHash: copyBuffer(compact.transactionWitnessSetHash),
  ...("validity" in compact ? { validity: compact.validity } : {}),
});

export const copyNativeWitnessSetCompact = (
  witnessSet: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact>,
): ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact> => ({
  addrTxWitsHash: copyBuffer(witnessSet.addrTxWitsHash),
  scriptTxWitsHash: copyBuffer(witnessSet.scriptTxWitsHash),
  redeemerTxWitsHash: copyBuffer(witnessSet.redeemerTxWitsHash),
});

/**
 * The §5.1 envelope over `enc_i` bytes the caller already has. Routed through
 * `midgard-core`'s one §5.1 encoder so the producer shares its width rules with
 * `decodeMidgardNativeByteListPreimage`, which is what reads these back.
 */
export const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimage(items);

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes.
 *
 * A Midgard out-ref has exactly one byte spelling, and this is it — ledger MPF
 * trie keys, field-0/1 preimage items and transition-effect source keys are all
 * these same 38 bytes, so this is the inverse of `encodeOutRef` below and of
 * on-chain `decode_midgard_tx_input_cbor`. Every other shape throws: a
 * development ledger written under any other spelling must be reset, not
 * migrated (`docs/spec/midgard-tx.md` §5.3). Tolerating a second shape here
 * would be worse than useless — a stale 36-byte key would decode and re-encode
 * to different bytes, silently re-keying the row instead of failing.
 */
export const decodeMidgardOutRefBytes = (
  inputBytes: Uint8Array,
): MidgardOutRef => {
  const decoded = decodeMidgardSpendInputItem(inputBytes);
  return {
    txId: Buffer.from(decoded.txId) as MidgardTxId,
    index: BigInt(decoded.outputIndex),
  };
};

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes.
 *
 * This must be the same encoder that produces the ledger MPF trie key
 * (`midgardOutRefToCbor`), because `toNativeTx` re-encodes a decoded ledger
 * transaction and asserts the recomputed tx id: a minimal output index here and
 * a fixed one there would make every round trip fail. On-chain the two are
 * literally one function — `ledger_outref_key` calls `encode_midgard_tx_input`,
 * the field-0/1 item encoder. See `docs/spec/midgard-tx.md` §5.3.
 */
const encodeOutRef = (outRef: MidgardOutRef, fieldName: string): Buffer => {
  if (outRef.index < 0n || outRef.index > BigInt(MIDGARD_MAX_OUTPUT_INDEX)) {
    failEncode(
      `${fieldName}.index must be 0..65,535 (§5.3 fixed uint16 output index)`,
      `index=${outRef.index.toString()}`,
    );
  }
  return encodeMidgardSpendInputItem({
    txId: copyBuffer(assertHash32(outRef.txId, `${fieldName}.txId`)),
    outputIndex: Number(outRef.index),
  });
};

export const decodeOutRefList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): MidgardOutRef[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    decodeMidgardOutRefBytes,
  );

export const encodeOutRefList = (
  outRefs: readonly MidgardOutRef[],
  fieldName: string,
): Buffer =>
  encodeByteList(
    outRefs.map((outRef, index) =>
      encodeOutRef(outRef, `${fieldName}[${index}]`),
    ),
  );

const toLedgerOutput = (output: MidgardTxOutput): MidgardLedgerOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.script_ref === undefined ? {} : { scriptRef: output.script_ref }),
});

const toCodecOutput = (output: MidgardLedgerOutput): MidgardTxOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.scriptRef === undefined ? {} : { script_ref: output.scriptRef }),
});

export const decodeOutputs = (
  preimageCbor: Uint8Array,
): MidgardLedgerOutput[] => {
  let items: readonly Uint8Array[];
  try {
    items = decodeMidgardNativeByteListPreimage(preimageCbor, "native.outputs");
  } catch (e) {
    throw new MidgardLedgerOutputDecodeError(e);
  }
  return items.map((outputBytes, index) => {
    try {
      return toLedgerOutput(decodeMidgardTxOutput(outputBytes));
    } catch (e) {
      throw new MidgardLedgerOutputDecodeError(e, index);
    }
  });
};

export const encodeOutputs = (
  outputs: readonly MidgardLedgerOutput[],
): Buffer =>
  encodeByteList(
    outputs.map((output) => encodeMidgardTxOutput(toCodecOutput(output))),
  );

export const decodeHashList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): Buffer[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    (bytes, index) => {
      if (bytes.length !== HASH28_LENGTH) {
        failDecode(
          `${fieldName}[${index}] must be ${HASH28_LENGTH} bytes`,
          `length=${bytes.length}`,
        );
      }
      return copyBuffer(bytes);
    },
  );
