import { CML } from "@lucid-evolution/lucid";
import {
  BinaryReader,
  BinaryWriter,
  ensureNoTrailingBytes,
  readBigU64,
  readHash32,
  writeBigU64,
  writeHash32,
} from "./binary.js";
import {
  decodeMidgardBytesListPreimage,
  decodeMidgardHash28ListPreimage,
  decodeMidgardMintPreimage,
  decodeMidgardOutputReferenceListPreimage,
  decodeMidgardVKeyWitnessListPreimage,
  type MidgardMint,
} from "./native-preimage.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import { decodeMidgardTxOutput } from "./output.js";
import { decodeMidgardAddressBytes } from "./address.js";
import {
  decodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";
import { midgardValueToCmlValue, type MidgardValue } from "./value.js";
import { cardanoTxBytesToMidgardNativeTxCanonical } from "./native-cardano-conversion.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
import {
  decodeNativeTxBodyCanonical,
  decodeNativeTxBodyCompact,
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCanonical,
  encodeNativeTxBodyCompact,
  readNativeTxBodyCanonicalDynamic,
  readNativeTxBodyCanonicalStatic,
  readNativeTxBodyCompact,
  writeNativeTxBodyCanonicalDynamic,
  writeNativeTxBodyCanonicalStatic,
  writeNativeTxBodyCompact,
} from "./native-body.js";
import { verifyNativeTxFullConsistency } from "./native-consistency.js";
import {
  decodeNativeTxWitnessSetCanonical,
  decodeNativeTxWitnessSetCompact,
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessSetCanonical,
  encodeNativeTxWitnessSetCompact,
  readNativeTxWitnessSetCanonicalDynamic,
  readNativeTxWitnessSetCanonicalStatic,
  readNativeTxWitnessSetCompact,
  writeNativeTxWitnessSetCanonicalDynamic,
  writeNativeTxWitnessSetCanonicalStatic,
  writeNativeTxWitnessSetCompact,
} from "./native-witness.js";
import {
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
export {
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  EMPTY_PREIMAGE_LIST,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
export {
  MidgardTxValidityCodes,
  type MidgardTxValidity,
} from "./native-validation.js";

export type MidgardNativeTxCompact = {
  readonly version: bigint;
  readonly transactionBody: MidgardNativeTxBodyCompact;
  readonly transactionWitnessSetHash: Hash32;
  readonly validity: MidgardTxValidity;
};

export type MidgardNativeTxBodyCompact = {
  readonly spendInputsHash: Hash32;
  readonly referenceInputsHash: Hash32;
  readonly outputsHash: Hash32;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversHash: Hash32;
  readonly requiredSignersHash: Hash32;
  readonly mintHash: Hash32;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCompact = {
  readonly addrTxWitsHash: Hash32;
  readonly scriptTxWitsHash: Hash32;
  readonly redeemerTxWitsHash: Hash32;
};

export type MidgardNativeTxBodyCanonical = {
  readonly spendInputsPreimage: Buffer;
  readonly referenceInputsPreimage: Buffer;
  readonly outputsPreimage: Buffer;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversPreimage: Buffer;
  readonly requiredSignersPreimage: Buffer;
  readonly mintPreimage: Buffer;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCanonical = {
  readonly addrTxWitsPreimage: Buffer;
  readonly scriptTxWitsPreimage: Buffer;
  readonly redeemerTxWitsPreimage: Buffer;
};

export type MidgardNativeTxCanonical = {
  readonly version: bigint;
  readonly validity: MidgardTxValidity;
  readonly body: MidgardNativeTxBodyCanonical;
  readonly witnessSet: MidgardNativeTxWitnessSetCanonical;
};

export type MidgardNativeTxFull = MidgardNativeTxCanonical & {
  readonly compact: MidgardNativeTxCompact;
};

export type MidgardNativeCodecOptions = {
  readonly enforceConsistency?: boolean;
};

// Compact tx envelope: version (u64) + compact body (fixed-size) +
// witness-set hash (32) + validity code (u64). Fully static.
const writeNativeTxCompact = (
  w: BinaryWriter,
  tx: MidgardNativeTxCompact,
): void => {
  writeBigU64(w, decodeVersion(tx.version, "transaction_compact.version"));
  writeNativeTxBodyCompact(w, tx.transactionBody);
  writeHash32(
    w,
    ensureHash32(
      tx.transactionWitnessSetHash,
      "transaction_compact.transaction_witness_set",
    ),
  );
  writeBigU64(w, encodeValidityCode(tx.validity));
};

const readNativeTxCompact = (r: BinaryReader): MidgardNativeTxCompact => {
  const version = decodeVersion(readBigU64(r), "transaction_compact[0]");
  const transactionBody = readNativeTxBodyCompact(r);
  const transactionWitnessSetHash = ensureHash32(
    readHash32(r),
    "transaction_compact[2]",
  );
  const validity = decodeValidityCode(readBigU64(r), "transaction_compact[3]");
  return { version, transactionBody, transactionWitnessSetHash, validity };
};

// Canonical tx envelope: version (u64) + validity code (u64), then the body
// and witness-set static sections, then their dynamic sections.
const writeNativeTxCanonical = (
  w: BinaryWriter,
  tx: MidgardNativeTxCanonical,
): void => {
  writeBigU64(w, decodeVersion(tx.version, "transaction.version"));
  writeBigU64(w, encodeValidityCode(tx.validity));
  writeNativeTxBodyCanonicalStatic(w, tx.body);
  writeNativeTxWitnessSetCanonicalStatic(w, tx.witnessSet);
  writeNativeTxBodyCanonicalDynamic(w, tx.body);
  writeNativeTxWitnessSetCanonicalDynamic(w, tx.witnessSet);
};

const readNativeTxCanonical = (r: BinaryReader): MidgardNativeTxCanonical => {
  const version = decodeVersion(readBigU64(r), "transaction[0]");
  const validity = decodeValidityCode(readBigU64(r), "transaction[1]");
  const bodyPartial = readNativeTxBodyCanonicalStatic(r);
  const wsPartial = readNativeTxWitnessSetCanonicalStatic(r);
  const body = readNativeTxBodyCanonicalDynamic(r, bodyPartial);
  const witnessSet = readNativeTxWitnessSetCanonicalDynamic(r, wsPartial);
  return { version, validity, body, witnessSet };
};

export const deriveMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => deriveNativeTxBodyCompact(body);

export const deriveMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): MidgardNativeTxWitnessSetCompact =>
  deriveNativeTxWitnessSetCompact(witnessSet);

export const toMidgardNativeTxCanonical = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxCanonical => ({
  version: tx.version,
  validity: tx.validity,
  body: {
    ...tx.body,
    spendInputsPreimage: Buffer.from(tx.body.spendInputsPreimage),
    referenceInputsPreimage: Buffer.from(
      tx.body.referenceInputsPreimage,
    ),
    outputsPreimage: Buffer.from(tx.body.outputsPreimage),
    requiredObserversPreimage: Buffer.from(
      tx.body.requiredObserversPreimage,
    ),
    requiredSignersPreimage: Buffer.from(
      tx.body.requiredSignersPreimage,
    ),
    mintPreimage: Buffer.from(tx.body.mintPreimage),
  },
  witnessSet: {
    addrTxWitsPreimage: Buffer.from(tx.witnessSet.addrTxWitsPreimage),
    scriptTxWitsPreimage: Buffer.from(
      tx.witnessSet.scriptTxWitsPreimage,
    ),
    redeemerTxWitsPreimage: Buffer.from(
      tx.witnessSet.redeemerTxWitsPreimage,
    ),
  },
});

export const deriveMidgardNativeTxCompact = (
  body: MidgardNativeTxBodyCanonical,
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  validity: MidgardTxValidity,
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCompact => {
  const bodyCompact = deriveMidgardNativeTxBodyCompact(body);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(witnessSet);
  return {
    version,
    transactionBody: bodyCompact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(witnessCompact),
    ),
    validity,
  };
};

export const materializeMidgardNativeTxFromCanonical = (
  canonical: MidgardNativeTxCanonical,
): MidgardNativeTxFull => {
  const version = decodeVersion(
    canonical.version,
    "transaction_canonical.version",
  );
  const compact = deriveMidgardNativeTxCompact(
    canonical.body,
    canonical.witnessSet,
    canonical.validity,
    version,
  );
  return {
    version,
    validity: canonical.validity,
    compact,
    body: canonical.body,
    witnessSet: canonical.witnessSet,
  };
};

export const verifyMidgardNativeTxFullConsistency = (
  tx: MidgardNativeTxFull,
): void => verifyNativeTxFullConsistency(tx);

export const encodeMidgardNativeTxCompact = (
  tx: MidgardNativeTxCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxCompact(w, tx);
  return w.toBytes();
};

export const decodeMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact => {
  const r = new BinaryReader(bytes);
  const tx = readNativeTxCompact(r);
  ensureNoTrailingBytes(r, "transaction_compact");
  return tx;
};

export const encodeMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => encodeNativeTxBodyCompact(body);

export const decodeMidgardNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact => decodeNativeTxBodyCompact(bytes);

export const encodeMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => encodeNativeTxWitnessSetCompact(witnessSet);

export const decodeMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact => decodeNativeTxWitnessSetCompact(bytes);

export const encodeMidgardNativeTxBodyCanonical = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => encodeNativeTxBodyCanonical(body);

export const decodeMidgardNativeTxBodyCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical => decodeNativeTxBodyCanonical(bytes);

export const encodeMidgardNativeTxWitnessPreimages = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  _version = MIDGARD_NATIVE_TX_VERSION,
): Buffer => encodeNativeTxWitnessSetCanonical(witnessSet);

export const decodeMidgardNativeTxWitnessPreimages = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical =>
  decodeNativeTxWitnessSetCanonical(bytes);

export const encodeMidgardNativeTxFull = (
  tx: MidgardNativeTxFull,
  options: MidgardNativeCodecOptions = {},
): Buffer => {
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  const w = new BinaryWriter();
  writeNativeTxCanonical(w, tx);
  return w.toBytes();
};

export const decodeMidgardNativeTxFull = (
  bytes: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): MidgardNativeTxFull => {
  const r = new BinaryReader(bytes);
  const canonical = readNativeTxCanonical(r);
  ensureNoTrailingBytes(r, "transaction");
  const tx = materializeMidgardNativeTxFromCanonical(canonical);
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  return tx;
};

export const computeMidgardNativeTxId = (
  tx: MidgardNativeTxFull | MidgardNativeTxCompact,
): Buffer => {
  const compact = "compact" in tx ? tx.compact : tx;
  return computeHash32(
    encodeMidgardNativeTxBodyCompact(compact.transactionBody),
  );
};

/**
 * Decode a binary-encoded list of variable-length byte entries.
 * Use this for the outputs preimage and the script-witness preimage where each
 * entry is itself a length-prefixed opaque blob.
 *
 * For inputs / vkey witnesses / signers / observers, use the field-specific
 * helpers below — those preimages are not variable-length lists.
 */
export const decodeMidgardNativeByteListPreimage = (
  preimage: Uint8Array,
  fieldName = "preimage",
): Buffer[] => decodeMidgardBytesListPreimage(preimage, fieldName);

/**
 * Decode an inputs / reference-inputs preimage and return one Cardano-canonical
 * CBOR `TransactionInput` blob per entry — preserving the shape callers used
 * back when the preimage itself was a CBOR list of those blobs.
 */
export const decodeMidgardNativeInputsPreimageAsCbor = (
  preimage: Uint8Array,
  fieldName = "inputs",
): Buffer[] => {
  const refs = decodeMidgardOutputReferenceListPreimage(preimage, fieldName);
  return refs.map((ref) => encodeCanonicalOutRefCbor(ref.txId, ref.index));
};

/**
 * Decode an addr-tx-wits preimage and return one Cardano-canonical CBOR
 * `Vkeywitness` blob per entry.
 */
export const decodeMidgardNativeAddrWitsPreimageAsCbor = (
  preimage: Uint8Array,
  fieldName = "addr_tx_wits",
): Buffer[] => {
  const witnesses = decodeMidgardVKeyWitnessListPreimage(preimage, fieldName);
  return witnesses.map((w) => encodeCanonicalVkeyWitnessCbor(w.vkey, w.signature));
};

// Hand-rolled canonical CBOR for `[txHash(32), index(uint)]` — avoids the CML
// allocation + to_cbor_bytes path that dominated phase-A throughput.
export const encodeCanonicalOutRefCbor = (
  txId: Buffer,
  index: number,
): Buffer => {
  if (index < 24) {
    const buf = Buffer.allocUnsafe(36);
    buf[0] = 0x82; // array(2)
    buf[1] = 0x58; // bytes(...)
    buf[2] = 0x20; // length 32
    txId.copy(buf, 3);
    buf[35] = index;
    return buf;
  }
  if (index < 0x100) {
    const buf = Buffer.allocUnsafe(37);
    buf[0] = 0x82;
    buf[1] = 0x58;
    buf[2] = 0x20;
    txId.copy(buf, 3);
    buf[35] = 0x18;
    buf[36] = index;
    return buf;
  }
  if (index < 0x10000) {
    const buf = Buffer.allocUnsafe(38);
    buf[0] = 0x82;
    buf[1] = 0x58;
    buf[2] = 0x20;
    txId.copy(buf, 3);
    buf[35] = 0x19;
    buf.writeUInt16BE(index, 36);
    return buf;
  }
  const buf = Buffer.allocUnsafe(40);
  buf[0] = 0x82;
  buf[1] = 0x58;
  buf[2] = 0x20;
  txId.copy(buf, 3);
  buf[35] = 0x1a;
  buf.writeUInt32BE(index, 36);
  return buf;
};

// Hand-rolled canonical CBOR for `vkeywitness = [vkey(32), signature(64)]`.
const encodeCanonicalVkeyWitnessCbor = (
  vkey: Buffer,
  signature: Buffer,
): Buffer => {
  const buf = Buffer.allocUnsafe(101);
  buf[0] = 0x82; // array(2)
  buf[1] = 0x58; // bytes(...)
  buf[2] = 0x20; // length 32
  vkey.copy(buf, 3);
  buf[35] = 0x58; // bytes(...)
  buf[36] = 0x40; // length 64
  signature.copy(buf, 37);
  return buf;
};

/**
 * Decode a required-signers / required-observers preimage as raw 28-byte
 * hashes (one Buffer per entry).
 */
export const decodeMidgardNativeHash28ListPreimage = (
  preimage: Uint8Array,
  fieldName = "hash28_list",
): Buffer[] => decodeMidgardHash28ListPreimage(preimage, fieldName);

export const cardanoTxBytesToMidgardNativeTxFull = (
  cardanoTxBytes: Uint8Array,
): MidgardNativeTxFull => {
  const canonical = cardanoTxBytesToMidgardNativeTxCanonical(cardanoTxBytes, {
    nativeTxVersion: MIDGARD_NATIVE_TX_VERSION,
    posixTimeNone: MIDGARD_POSIX_TIME_NONE,
    networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
  });
  return materializeMidgardNativeTxFromCanonical(canonical);
};

export const cardanoTxBytesToMidgardNativeTxFullBytes = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxFull(
    cardanoTxBytesToMidgardNativeTxFull(cardanoTxBytes),
  );

const toCardanoNetworkId = (
  networkId: bigint,
  fieldName: string,
): InstanceType<typeof CML.NetworkId> | undefined => {
  if (networkId === MIDGARD_NATIVE_NETWORK_ID_NONE) {
    return undefined;
  }
  if (networkId === 0n) {
    return CML.NetworkId.testnet();
  }
  if (networkId === 1n) {
    return CML.NetworkId.mainnet();
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    "Unsupported Cardano network id for reverse conversion",
    `${fieldName}: ${networkId.toString(10)}`,
  );
};

const decodeNativeRequiredSignersToCardano = (
  preimage: Uint8Array,
): InstanceType<typeof CML.Ed25519KeyHashList> => {
  const signerBytes = decodeMidgardHash28ListPreimage(
    preimage,
    "native.required_signers",
  );
  const signers = CML.Ed25519KeyHashList.new();
  for (let i = 0; i < signerBytes.length; i++) {
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(signerBytes[i]));
  }
  return signers;
};

const decodeNativeObserversToWithdrawals = (
  preimage: Uint8Array,
  networkId: InstanceType<typeof CML.NetworkId> | undefined,
): InstanceType<typeof CML.MapRewardAccountToCoin> | undefined => {
  const observerBytes = decodeMidgardHash28ListPreimage(
    preimage,
    "native.required_observers",
  );
  if (observerBytes.length === 0) {
    return undefined;
  }
  if (networkId === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Observer-to-withdrawal conversion requires an explicit Cardano network id",
      "native.network_id",
    );
  }
  const withdrawals = CML.MapRewardAccountToCoin.new();
  for (let i = 0; i < observerBytes.length; i++) {
    const credential = CML.Credential.new_script(
      CML.ScriptHash.from_raw_bytes(observerBytes[i]),
    );
    withdrawals.insert(
      CML.RewardAddress.new(Number(networkId.network()), credential),
      0n,
    );
  }
  return withdrawals;
};

const decodeNativeInputsToCardano = (
  preimage: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.TransactionInputList> => {
  const refs = decodeMidgardOutputReferenceListPreimage(preimage, fieldName);
  const inputs = CML.TransactionInputList.new();
  for (const ref of refs) {
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(ref.txId),
        BigInt(ref.index),
      ),
    );
  }
  return inputs;
};

const midgardVersionedScriptToCardano = (
  script: MidgardVersionedScript,
  fieldName: string,
): InstanceType<typeof CML.Script> => {
  switch (script.language) {
    case "NativeCardano":
      return CML.Script.new_native(
        CML.NativeScript.from_cbor_bytes(script.scriptBytes),
      );
    case "PlutusV3":
      return CML.Script.new_plutus_v3(
        CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
      );
    case "MidgardV1":
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
        "MidgardV1 scripts cannot be represented as Cardano script references",
        fieldName,
      );
  }
};

const midgardOutputBytesToCardano = (
  outputBytes: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.TransactionOutput> => {
  const decoded = decodeMidgardTxOutput(outputBytes);
  const address = decodeMidgardAddressBytes(decoded.address);
  if (address.protected) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Protected Midgard addresses cannot be represented as Cardano TxOut addresses",
      fieldName,
    );
  }
  const output = CML.ConwayFormatTxOut.new(
    CML.Address.from_raw_bytes(decoded.address),
    midgardValueToCmlValue(decoded.value),
  );
  if (decoded.datum !== undefined) {
    output.set_datum_option(
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_bytes(decoded.datum.cbor),
      ),
    );
  }
  if (decoded.script_ref !== undefined) {
    output.set_script_reference(
      midgardVersionedScriptToCardano(
        decoded.script_ref,
        `${fieldName}.script_ref`,
      ),
    );
  }
  return CML.TransactionOutput.new_conway_format_tx_out(output);
};

const decodeNativeOutputsToCardano = (
  preimage: Uint8Array,
): InstanceType<typeof CML.TransactionOutputList> => {
  const outputBytes = decodeMidgardBytesListPreimage(
    preimage,
    "native.outputs",
  );
  const outputs = CML.TransactionOutputList.new();
  for (let i = 0; i < outputBytes.length; i++) {
    outputs.add(
      midgardOutputBytesToCardano(outputBytes[i], `native.outputs[${i}]`),
    );
  }
  return outputs;
};

const decodeNativeAddrWitnessesToCardano = (
  preimage: Uint8Array,
): InstanceType<typeof CML.VkeywitnessList> | undefined => {
  const witnesses = decodeMidgardVKeyWitnessListPreimage(
    preimage,
    "native.addr_tx_wits",
  );
  if (witnesses.length === 0) {
    return undefined;
  }
  const out = CML.VkeywitnessList.new();
  for (const w of witnesses) {
    out.add(
      CML.Vkeywitness.new(
        CML.PublicKey.from_bytes(w.vkey),
        CML.Ed25519Signature.from_raw_bytes(w.signature),
      ),
    );
  }
  return out;
};

type DecodedCardanoScripts = {
  readonly nativeScripts?: InstanceType<typeof CML.NativeScriptList>;
  readonly plutusV3Scripts?: InstanceType<typeof CML.PlutusV3ScriptList>;
};

export type DecodedMidgardNativeMint = {
  readonly mint: InstanceType<typeof CML.Mint>;
  readonly policyIds: readonly string[];
  readonly mintedValue: InstanceType<typeof CML.Value>;
  readonly burnedValue: InstanceType<typeof CML.Value>;
};

const decodeNativeScriptsToCardano = (
  preimage: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimage,
    "native.script_tx_wits",
  );
  if (scripts.length === 0) {
    return {};
  }
  const nativeScripts = CML.NativeScriptList.new();
  const plutusV3Scripts = CML.PlutusV3ScriptList.new();
  for (let i = 0; i < scripts.length; i++) {
    const script = scripts[i];
    switch (script.language) {
      case "NativeCardano":
        nativeScripts.add(CML.NativeScript.from_cbor_bytes(script.scriptBytes));
        break;
      case "PlutusV3":
        plutusV3Scripts.add(
          CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
        );
        break;
      case "MidgardV1":
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
          "MidgardV1 inline scripts cannot be represented in Cardano witness sets",
          `native.script_tx_wits[${i}]`,
        );
    }
  }
  return {
    nativeScripts: nativeScripts.len() > 0 ? nativeScripts : undefined,
    plutusV3Scripts: plutusV3Scripts.len() > 0 ? plutusV3Scripts : undefined,
  };
};

const valueFromMultiasset = (
  multiasset: InstanceType<typeof CML.MultiAsset>,
): InstanceType<typeof CML.Value> =>
  multiasset.policy_count() === 0
    ? CML.Value.zero()
    : CML.Value.new(0n, multiasset);

export const decodeMidgardNativeMint = (
  preimage: Uint8Array,
): DecodedMidgardNativeMint | undefined => {
  const decoded: MidgardMint = decodeMidgardMintPreimage(preimage, "native.mint");
  if (decoded.length === 0) {
    return undefined;
  }
  const mint = CML.Mint.new();
  for (let i = 0; i < decoded.length; i++) {
    const policy = decoded[i];
    if (policy.assets.length === 0) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Mint policy asset list cannot be empty",
        `native.mint[${i}]`,
      );
    }
    const assets = CML.MapAssetNameToNonZeroInt64.new();
    for (let j = 0; j < policy.assets.length; j++) {
      const a = policy.assets[j];
      assets.insert(CML.AssetName.from_raw_bytes(a.name), a.amount);
    }
    mint.insert_assets(CML.ScriptHash.from_raw_bytes(policy.policyId), assets);
  }

  const policyIds = Array.from({ length: mint.keys().len() }, (_, index) =>
    mint.keys().get(index).to_hex(),
  ).sort((a, b) => a.localeCompare(b));

  return {
    mint,
    policyIds,
    mintedValue: valueFromMultiasset(mint.as_positive_multiasset()),
    burnedValue: valueFromMultiasset(mint.as_negative_multiasset()),
  };
};

const isEmptyPreimage = (bytes: Uint8Array): boolean => {
  if (bytes.length !== 8) return bytes.length === 0;
  for (let i = 0; i < 8; i++) {
    if (bytes[i] !== 0) return false;
  }
  return true;
};

const decodeNativeRedeemersToCardano = (
  preimage: Uint8Array,
): InstanceType<typeof CML.Redeemers> | undefined => {
  if (isEmptyPreimage(preimage)) {
    return undefined;
  }
  return CML.Redeemers.from_cbor_bytes(preimage);
};

export type MidgardToCardanoTxEncodingOptions = {
  readonly omitVkeyWitnesses?: boolean;
};

export const midgardNativeTxFullToCardanoTxEncoding = (
  tx: MidgardNativeTxFull,
  options?: MidgardToCardanoTxEncodingOptions,
): Buffer => {
  verifyMidgardNativeTxFullConsistency(tx);

  const inputs = decodeNativeInputsToCardano(
    tx.body.spendInputsPreimage,
    "native.spend_inputs",
  );
  const outputs = decodeNativeOutputsToCardano(tx.body.outputsPreimage);
  const body = CML.TransactionBody.new(inputs, outputs, tx.body.fee);
  const networkId = toCardanoNetworkId(tx.body.networkId, "native.network_id");
  if (networkId !== undefined) {
    body.set_network_id(networkId);
  }

  const referenceInputs = decodeNativeInputsToCardano(
    tx.body.referenceInputsPreimage,
    "native.reference_inputs",
  );
  if (referenceInputs.len() > 0) {
    body.set_reference_inputs(referenceInputs);
  }

  if (tx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE) {
    body.set_validity_interval_start(tx.body.validityIntervalStart);
  }
  if (tx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE) {
    body.set_ttl(tx.body.validityIntervalEnd);
  }

  const withdrawals = decodeNativeObserversToWithdrawals(
    tx.body.requiredObserversPreimage,
    networkId,
  );
  if (withdrawals !== undefined) {
    body.set_withdrawals(withdrawals);
  }

  const requiredSigners = decodeNativeRequiredSignersToCardano(
    tx.body.requiredSignersPreimage,
  );
  if (requiredSigners.len() > 0) {
    body.set_required_signers(requiredSigners);
  }

  const decodedMint = decodeMidgardNativeMint(tx.body.mintPreimage);
  if (decodedMint !== undefined) {
    body.set_mint(decodedMint.mint);
  }

  if (!tx.body.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)) {
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(tx.body.scriptIntegrityHash),
    );
  }
  if (!tx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    body.set_auxiliary_data_hash(
      CML.AuxiliaryDataHash.from_raw_bytes(tx.body.auxiliaryDataHash),
    );
  }

  const witnessSet = CML.TransactionWitnessSet.new();
  if (options?.omitVkeyWitnesses !== true) {
    const vkeyWitnesses = decodeNativeAddrWitnessesToCardano(
      tx.witnessSet.addrTxWitsPreimage,
    );
    if (vkeyWitnesses !== undefined) {
      witnessSet.set_vkeywitnesses(vkeyWitnesses);
    }
  }

  const scripts = decodeNativeScriptsToCardano(
    tx.witnessSet.scriptTxWitsPreimage,
  );
  if (scripts.nativeScripts !== undefined) {
    witnessSet.set_native_scripts(scripts.nativeScripts);
  }
  if (scripts.plutusV3Scripts !== undefined) {
    witnessSet.set_plutus_v3_scripts(scripts.plutusV3Scripts);
  }

  const redeemers = decodeNativeRedeemersToCardano(
    tx.witnessSet.redeemerTxWitsPreimage,
  );
  if (redeemers !== undefined) {
    witnessSet.set_redeemers(redeemers);
  }

  return Buffer.from(
    CML.Transaction.new(
      body,
      witnessSet,
      tx.validity === "TxIsValid",
      undefined,
    ).to_cbor_bytes(),
  );
};
