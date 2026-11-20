import { CML } from "@lucid-evolution/lucid";

import { decodeMidgardAddressBytes } from "./address.js";
import {
  asArray,
  asBytes,
  asMap,
  decodeSingleCbor,
  encodeCbor,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import {
  decodeNativeTxBodyCanonicalCbor,
  decodeNativeTxBodyCanonicalValue,
  decodeNativeTxBodyCompactCbor,
  decodeNativeTxBodyCompactValue,
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCanonicalCbor,
  encodeNativeTxBodyCanonicalValue,
  encodeNativeTxBodyCompactCbor,
  encodeNativeTxBodyCompactValue,
} from "./native-body.js";
import { cardanoTxBytesToMidgardNativeTxCanonical } from "./native-cardano-conversion.js";
import { verifyNativeTxFullConsistency } from "./native-consistency.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
import {
  asFixedArray,
  asSigned,
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
import {
  decodeNativeTxWitnessPreimagesCbor,
  decodeNativeTxWitnessSetCanonicalValue,
  decodeNativeTxWitnessSetCompactCbor,
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessPreimagesCbor,
  encodeNativeTxWitnessSetCanonicalValue,
  encodeNativeTxWitnessSetCompactCbor,
} from "./native-witness.js";
import { decodeMidgardTxOutput } from "./output.js";
import { midgardValueToCmlValue } from "./value.js";
import {
  decodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";
export {
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
export {
  type MidgardTxValidity,
  MidgardTxValidityCodes,
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

const encodeNativeTxCompactValue = (
  tx: MidgardNativeTxCompact,
): readonly [
  bigint,
  ReturnType<typeof encodeNativeTxBodyCompactValue>,
  Hash32,
  bigint,
] => [
  decodeVersion(tx.version, "transaction_compact.version"),
  encodeNativeTxBodyCompactValue(tx.transactionBody),
  ensureHash32(
    tx.transactionWitnessSetHash,
    "transaction_compact.transaction_witness_set",
  ),
  encodeValidityCode(tx.validity),
];

const decodeNativeTxCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxCompact => {
  const v = asFixedArray(value, 4, fieldName);
  return {
    version: decodeVersion(v[0], `${fieldName}[0]`),
    transactionBody: decodeNativeTxBodyCompactValue(v[1], `${fieldName}[1]`),
    transactionWitnessSetHash: ensureHash32(
      asBytes(v[2], `${fieldName}[2]`),
      `${fieldName}[2]`,
    ),
    validity: decodeValidityCode(v[3], `${fieldName}[3]`),
  };
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
): Buffer => encodeCbor(encodeNativeTxCompactValue(tx));

export const decodeMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact =>
  decodeNativeTxCompactValue(decodeSingleCbor(bytes), "transaction_compact");

export const encodeMidgardNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => encodeNativeTxBodyCompactCbor(body);

export const decodeMidgardNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact => decodeNativeTxBodyCompactCbor(bytes);

export const encodeMidgardNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => encodeNativeTxWitnessSetCompactCbor(witnessSet);

export const decodeMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  decodeNativeTxWitnessSetCompactCbor(bytes);

export const encodeMidgardNativeTxBodyCanonical = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => encodeNativeTxBodyCanonicalCbor(body);

export const decodeMidgardNativeTxBodyCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical => decodeNativeTxBodyCanonicalCbor(bytes);

export const encodeMidgardNativeTxWitnessPreimages = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
  version = MIDGARD_NATIVE_TX_VERSION,
): Buffer => encodeNativeTxWitnessPreimagesCbor(witnessSet, version);

export const decodeMidgardNativeTxWitnessPreimages = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical =>
  decodeNativeTxWitnessPreimagesCbor(bytes);

const hasDerivedCompact = (
  tx: MidgardNativeTxCanonical | MidgardNativeTxFull,
): tx is MidgardNativeTxFull => "compact" in tx;

export const encodeMidgardNativeTxCanonical = (
  tx: MidgardNativeTxCanonical | MidgardNativeTxFull,
  options: MidgardNativeCodecOptions = {},
): Buffer => {
  if (options.enforceConsistency !== false && hasDerivedCompact(tx)) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  const version = decodeVersion(tx.version, "transaction.version");
  return encodeCbor([
    version,
    encodeNativeTxBodyCanonicalValue(tx.body),
    encodeNativeTxWitnessSetCanonicalValue(version, tx.witnessSet),
    encodeValidityCode(tx.validity),
  ]);
};

export const decodeMidgardNativeTxCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxCanonical => {
  const decoded = decodeSingleCbor(bytes);
  const v = asFixedArray(decoded, 4, "transaction");
  const version = decodeVersion(v[0], "transaction[0]");
  return {
    version,
    body: decodeNativeTxBodyCanonicalValue(v[1], "transaction[1]"),
    witnessSet: decodeNativeTxWitnessSetCanonicalValue(
      v[2],
      "transaction[2]",
      version,
    ),
    validity: decodeValidityCode(v[3], "transaction[3]"),
  };
};

export const decodeMidgardNativeTxFullFromCanonicalCbor = (
  bytes: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): MidgardNativeTxFull => {
  const tx = materializeMidgardNativeTxFromCanonical(
    decodeMidgardNativeTxCanonical(bytes),
  );
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
  return arr.map((item, index) => asBytes(item, `${fieldName}[${index}]`));
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

export const cardanoTxBytesToMidgardNativeTxCanonicalCbor = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxCanonical(
    cardanoTxBytesToMidgardNativeTxCanonical(cardanoTxBytes, {
      nativeTxVersion: MIDGARD_NATIVE_TX_VERSION,
      posixTimeNone: MIDGARD_POSIX_TIME_NONE,
      networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
    }),
  );

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): CML.Credential => {
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
): CML.NetworkId | undefined => {
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
): CML.Ed25519KeyHashList => {
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
  networkId: CML.NetworkId | undefined,
): CML.MapRewardAccountToCoin | undefined => {
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
): CML.TransactionInputList => {
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
): CML.Script => {
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
): CML.TransactionOutput => {
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
): CML.TransactionOutputList => {
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
): CML.VkeywitnessList | undefined => {
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
  readonly nativeScripts?: CML.NativeScriptList;
  readonly plutusV3Scripts?: CML.PlutusV3ScriptList;
};

export type DecodedMidgardNativeMint = {
  readonly mint: CML.Mint;
  readonly policyIds: readonly string[];
  readonly mintedValue: CML.Value;
  readonly burnedValue: CML.Value;
};

const decodeNativeScriptsToCardano = (
  preimageCbor: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
    "native.script_tx_wits",
  );
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

const valueFromMultiasset = (multiasset: CML.MultiAsset): CML.Value =>
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
): CML.Redeemers | undefined => {
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
