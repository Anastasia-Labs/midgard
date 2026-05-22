import { CML } from "@lucid-evolution/lucid";
import { asArray, asBytes, asMap, decodeSingleCbor } from "./cbor.js";
import {
  BinaryReader,
  BinaryWriter,
  ensureNoTrailingBytes,
  readBigU64,
  readHash32,
  writeBigU64,
  writeHash32,
} from "./binary.js";
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
  asSigned,
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
export {
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
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
  readonly spendInputsPreimageCbor: Buffer;
  readonly referenceInputsPreimageCbor: Buffer;
  readonly outputsPreimageCbor: Buffer;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly requiredObserversPreimageCbor: Buffer;
  readonly requiredSignersPreimageCbor: Buffer;
  readonly mintPreimageCbor: Buffer;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCanonical = {
  readonly addrTxWitsPreimageCbor: Buffer;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsPreimageCbor: Buffer;
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
    spendInputsPreimageCbor: Buffer.from(tx.body.spendInputsPreimageCbor),
    referenceInputsPreimageCbor: Buffer.from(
      tx.body.referenceInputsPreimageCbor,
    ),
    outputsPreimageCbor: Buffer.from(tx.body.outputsPreimageCbor),
    requiredObserversPreimageCbor: Buffer.from(
      tx.body.requiredObserversPreimageCbor,
    ),
    requiredSignersPreimageCbor: Buffer.from(
      tx.body.requiredSignersPreimageCbor,
    ),
    mintPreimageCbor: Buffer.from(tx.body.mintPreimageCbor),
  },
  witnessSet: {
    addrTxWitsPreimageCbor: Buffer.from(tx.witnessSet.addrTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor: Buffer.from(
      tx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    redeemerTxWitsPreimageCbor: Buffer.from(
      tx.witnessSet.redeemerTxWitsPreimageCbor,
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

export const decodeMidgardNativeByteListPreimage = (
  preimageCbor: Uint8Array,
  fieldName = "preimage_cbor",
): Buffer[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  const arr = asArray(decoded, fieldName);
  return arr.map((item, index) =>
    Buffer.from(asBytes(item, `${fieldName}[${index}]`)),
  );
};

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

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.Credential> => {
  if (observerBytes.length === 28) {
    return CML.Credential.new_script(
      CML.ScriptHash.from_raw_bytes(observerBytes),
    );
  }
  try {
    const credential = CML.Credential.from_cbor_bytes(observerBytes);
    if (credential.kind() !== CML.CredentialKind.Script) {
      throw new Error("observer credential must be a script credential");
    }
    return credential;
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard observer must be a script hash or a CBOR-encoded script credential",
      `${fieldName}: ${String(e)}`,
    );
  }
};

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
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.Ed25519KeyHashList> => {
  const signerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_signers",
  );
  const signers = CML.Ed25519KeyHashList.new();
  for (let i = 0; i < signerBytes.length; i++) {
    const signer = signerBytes[i];
    if (signer.length !== 28) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Required signer must be 28 bytes",
        `native.required_signers[${i}]`,
      );
    }
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(signer));
  }
  return signers;
};

const decodeNativeObserversToWithdrawals = (
  preimageCbor: Uint8Array,
  networkId: InstanceType<typeof CML.NetworkId> | undefined,
): InstanceType<typeof CML.MapRewardAccountToCoin> | undefined => {
  const observerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
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
    const credential = decodeNativeCredentialObserver(
      observerBytes[i],
      `native.required_observers[${i}]`,
    );
    withdrawals.insert(
      CML.RewardAddress.new(Number(networkId.network()), credential),
      0n,
    );
  }
  return withdrawals;
};

const decodeNativeInputsToCardano = (
  preimageCbor: Uint8Array,
  fieldName: string,
): InstanceType<typeof CML.TransactionInputList> => {
  const inputBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    fieldName,
  );
  const inputs = CML.TransactionInputList.new();
  for (let i = 0; i < inputBytes.length; i++) {
    inputs.add(CML.TransactionInput.from_cbor_bytes(inputBytes[i]));
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
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.TransactionOutputList> => {
  const outputBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
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
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.VkeywitnessList> | undefined => {
  const witnessBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.addr_tx_wits",
  );
  if (witnessBytes.length === 0) {
    return undefined;
  }
  const witnesses = CML.VkeywitnessList.new();
  for (let i = 0; i < witnessBytes.length; i++) {
    witnesses.add(CML.Vkeywitness.from_cbor_bytes(witnessBytes[i]));
  }
  return witnesses;
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
  preimageCbor: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
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
  preimageCbor: Uint8Array,
): DecodedMidgardNativeMint | undefined => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    if (decoded.length === 0) {
      return undefined;
    }
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard mint preimage must be an empty array or a CBOR map",
      "native.mint",
    );
  }

  const policies = asMap(decoded, "native.mint");
  if (policies.size === 0) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard mint map cannot be empty",
      "native.mint",
    );
  }

  const mint = CML.Mint.new();
  for (const [policyBytesValue, assetsValue] of policies.entries()) {
    const policyBytes = asBytes(policyBytesValue, "native.mint.policy");
    if (policyBytes.length !== 28) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Mint policy id must be 28 bytes",
        "native.mint.policy",
      );
    }

    const assetsMap = asMap(assetsValue, "native.mint.assets");
    if (assetsMap.size === 0) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Mint policy asset map cannot be empty",
        "native.mint.assets",
      );
    }
    const assets = CML.MapAssetNameToNonZeroInt64.new();
    for (const [assetNameValue, quantityValue] of assetsMap.entries()) {
      const assetName = asBytes(assetNameValue, "native.mint.asset_name");
      const quantity = asSigned(quantityValue, "native.mint.quantity");
      if (quantity === 0n) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.InvalidFieldType,
          "Mint quantity cannot be zero",
          "native.mint.quantity",
        );
      }
      assets.insert(CML.AssetName.from_raw_bytes(assetName), quantity);
    }

    mint.insert_assets(CML.ScriptHash.from_raw_bytes(policyBytes), assets);
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

const decodeNativeRedeemersToCardano = (
  preimageCbor: Uint8Array,
): InstanceType<typeof CML.Redeemers> | undefined => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded) && decoded.length === 0) {
    return undefined;
  }
  return CML.Redeemers.from_cbor_bytes(preimageCbor);
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
    tx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  const outputs = decodeNativeOutputsToCardano(tx.body.outputsPreimageCbor);
  const body = CML.TransactionBody.new(inputs, outputs, tx.body.fee);
  const networkId = toCardanoNetworkId(tx.body.networkId, "native.network_id");
  if (networkId !== undefined) {
    body.set_network_id(networkId);
  }

  const referenceInputs = decodeNativeInputsToCardano(
    tx.body.referenceInputsPreimageCbor,
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
    tx.body.requiredObserversPreimageCbor,
    networkId,
  );
  if (withdrawals !== undefined) {
    body.set_withdrawals(withdrawals);
  }

  const requiredSigners = decodeNativeRequiredSignersToCardano(
    tx.body.requiredSignersPreimageCbor,
  );
  if (requiredSigners.len() > 0) {
    body.set_required_signers(requiredSigners);
  }

  const decodedMint = decodeMidgardNativeMint(tx.body.mintPreimageCbor);
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
      tx.witnessSet.addrTxWitsPreimageCbor,
    );
    if (vkeyWitnesses !== undefined) {
      witnessSet.set_vkeywitnesses(vkeyWitnesses);
    }
  }

  const scripts = decodeNativeScriptsToCardano(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );
  if (scripts.nativeScripts !== undefined) {
    witnessSet.set_native_scripts(scripts.nativeScripts);
  }
  if (scripts.plutusV3Scripts !== undefined) {
    witnessSet.set_plutus_v3_scripts(scripts.plutusV3Scripts);
  }

  const redeemers = decodeNativeRedeemersToCardano(
    tx.witnessSet.redeemerTxWitsPreimageCbor,
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
