import { CML } from "@lucid-evolution/lucid";

import { decodeMidgardAddressBytes } from "./address.js";
import { BinaryReader, BinaryWriter, readBigU64, writeBigU64 } from "./binary.js";
import {
  HASH28_LENGTH,
  ensureHash28,
  type Hash28,
  type OutputReference,
  type VKeyWitness,
} from "./binary-types.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import {
  decodeNativeTxBodyCanonical,
  decodeNativeTxBodyCompact,
  deriveNativeTxBodyCompact,
  encodeNativeTxBodyCanonical,
  encodeNativeTxBodyCompact,
  readNativeTxBodyCanonicalDynamic,
  readNativeTxBodyCanonicalStatic,
  readNativeTxBodyCompactStatic,
  writeNativeTxBodyCanonicalDynamic,
  writeNativeTxBodyCanonicalStatic,
  writeNativeTxBodyCompactStatic,
} from "./native-body.js";
export {
  encodeSpendInputsBinary,
  encodeReferenceInputsBinary,
  encodeOutputsBinary,
  encodeRequiredObserversBinary,
  encodeRequiredSignersBinary,
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
  decodeValidityCode,
  decodeVersion,
  encodeValidityCode,
  type MidgardTxValidity,
} from "./native-validation.js";
import {
  decodeNativeTxWitnessSetCanonical,
  decodeNativeTxWitnessSetCompact,
  deriveNativeTxWitnessSetCompact,
  encodeNativeTxWitnessSetCanonical,
  encodeNativeTxWitnessSetCompact,
  readNativeTxWitnessSetCanonicalDynamic,
  readNativeTxWitnessSetCanonicalStatic,
  writeNativeTxWitnessSetCanonicalDynamic,
  writeNativeTxWitnessSetCanonicalStatic,
} from "./native-witness.js";
export {
  encodeAddrTxWitsBinary,
  encodeScriptTxWitsBinary,
  encodeRedeemerTxWitsBinary,
} from "./native-witness.js";
import { type MidgardTxOutput } from "./output.js";
import {
  midgardValueToCmlValue,
  type MidgardMint,
} from "./value.js";
import {
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
export type { OutputReference, VKeyWitness } from "./binary-types.js";
export type { MidgardMint } from "./value.js";

// ===========================================================================
// Type model (typed fields — no more CBOR preimages).
// ===========================================================================

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
  readonly spendInputs: readonly OutputReference[];
  readonly referenceInputs: readonly OutputReference[];
  readonly outputs: readonly MidgardTxOutput[];
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  /**
   * Observer entries are either a 28-byte script hash or an opaque
   * CBOR-encoded credential envelope. We preserve the raw bytes so the
   * binary encoding is bit-exact across observers of either flavour.
   */
  readonly requiredObservers: readonly Buffer[];
  readonly requiredSigners: readonly Hash28[];
  readonly mint: MidgardMint;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export type MidgardNativeTxWitnessSetCanonical = {
  readonly addrTxWits: readonly VKeyWitness[];
  readonly scriptTxWits: readonly MidgardVersionedScript[];
  /** Opaque CBOR redeemer-set blob (Plutus payload, passed through). */
  readonly redeemerTxWits: Buffer;
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

// ===========================================================================
// Derivation helpers
// ===========================================================================

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
  body: tx.body,
  witnessSet: tx.witnessSet,
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

// ===========================================================================
// Binary envelopes
// ===========================================================================
//
// MidgardNativeTxCompact (binary, fully static):
//   version (u64) + body_compact + validity (u64) + witness_set_hash (32)
//
// MidgardNativeTxCanonical (binary):
//   Static:  version (u64) + body.static + witness_set.static + validity (u64)
//   Dynamic: body.dynamic + witness_set.dynamic

export const encodeMidgardNativeTxCompact = (
  tx: MidgardNativeTxCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeBigU64(w, decodeVersion(tx.version, "transaction_compact.version"));
  writeNativeTxBodyCompactStatic(w, tx.transactionBody);
  w.write(
    ensureHash32(
      tx.transactionWitnessSetHash,
      "transaction_compact.transaction_witness_set_hash",
    ),
  );
  writeBigU64(w, encodeValidityCode(tx.validity));
  return w.toBytes();
};

export const decodeMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact => {
  const r = new BinaryReader(bytes);
  const version = decodeVersion(readBigU64(r), "transaction_compact.version");
  const transactionBody = readNativeTxBodyCompactStatic(r);
  const transactionWitnessSetHash = ensureHash32(
    r.read(32),
    "transaction_compact.transaction_witness_set_hash",
  );
  const validity = decodeValidityCode(
    readBigU64(r),
    "transaction_compact.validity",
  );
  r.expectEnd("transaction_compact");
  return { version, transactionBody, transactionWitnessSetHash, validity };
};

export const encodeMidgardNativeTxBodyCompact = encodeNativeTxBodyCompact;
export const decodeMidgardNativeTxBodyCompact = decodeNativeTxBodyCompact;

export const encodeMidgardNativeTxWitnessSetCompact =
  encodeNativeTxWitnessSetCompact;
export const decodeMidgardNativeTxWitnessSetCompact =
  decodeNativeTxWitnessSetCompact;

export const encodeMidgardNativeTxBodyCanonical = encodeNativeTxBodyCanonical;
export const decodeMidgardNativeTxBodyCanonical = decodeNativeTxBodyCanonical;

export const encodeMidgardNativeTxWitnessSetCanonical =
  encodeNativeTxWitnessSetCanonical;
export const decodeMidgardNativeTxWitnessSetCanonical =
  decodeNativeTxWitnessSetCanonical;

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
  const sw = new BinaryWriter();
  writeBigU64(sw, version);
  writeNativeTxBodyCanonicalStatic(sw, tx.body);
  writeNativeTxWitnessSetCanonicalStatic(sw, tx.witnessSet);
  writeBigU64(sw, encodeValidityCode(tx.validity));
  const dw = new BinaryWriter();
  writeNativeTxBodyCanonicalDynamic(dw, tx.body);
  writeNativeTxWitnessSetCanonicalDynamic(dw, tx.witnessSet);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardNativeTxCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxCanonical => {
  const r = new BinaryReader(bytes);
  const version = decodeVersion(readBigU64(r), "transaction.version");
  const bodyPartial = readNativeTxBodyCanonicalStatic(r);
  const wsPartial = readNativeTxWitnessSetCanonicalStatic(r);
  const validity = decodeValidityCode(readBigU64(r), "transaction.validity");
  const body = readNativeTxBodyCanonicalDynamic(r, bodyPartial);
  const witnessSet = readNativeTxWitnessSetCanonicalDynamic(r, wsPartial);
  r.expectEnd("transaction");
  return { version, validity, body, witnessSet };
};

export const decodeMidgardNativeTxFullFromCanonicalBinary = (
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

export const cardanoTxBytesToMidgardNativeTxCanonicalBinary = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxCanonical(
    cardanoTxBytesToMidgardNativeTxCanonical(cardanoTxBytes, {
      nativeTxVersion: MIDGARD_NATIVE_TX_VERSION,
      posixTimeNone: MIDGARD_POSIX_TIME_NONE,
      networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
    }),
  );

// ===========================================================================
// Helpers: typed fields → Cardano CML conversion (used by Cardano-tx export).
// ===========================================================================

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): CML.Credential => {
  if (observerBytes.length === HASH28_LENGTH) {
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
  if (networkId === MIDGARD_NATIVE_NETWORK_ID_NONE) return undefined;
  if (networkId === 0n) return CML.NetworkId.testnet();
  if (networkId === 1n) return CML.NetworkId.mainnet();
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    "Unsupported Cardano network id for reverse conversion",
    `${fieldName}: ${networkId.toString(10)}`,
  );
};

const outputReferenceToCml = (
  outref: OutputReference,
): CML.TransactionInput =>
  CML.TransactionInput.new(
    CML.TransactionHash.from_raw_bytes(outref.txId),
    BigInt(outref.index),
  );

const requiredSignersToCml = (
  signers: readonly Hash28[],
): CML.Ed25519KeyHashList => {
  const out = CML.Ed25519KeyHashList.new();
  for (let i = 0; i < signers.length; i += 1) {
    const signer = ensureHash28(signers[i], `native.required_signers[${i}]`);
    out.add(CML.Ed25519KeyHash.from_raw_bytes(signer));
  }
  return out;
};

const observersToWithdrawalsCml = (
  observers: readonly Buffer[],
  networkId: CML.NetworkId | undefined,
): CML.MapRewardAccountToCoin | undefined => {
  if (observers.length === 0) return undefined;
  if (networkId === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Observer-to-withdrawal conversion requires an explicit Cardano network id",
      "native.network_id",
    );
  }
  const withdrawals = CML.MapRewardAccountToCoin.new();
  for (let i = 0; i < observers.length; i += 1) {
    const credential = decodeNativeCredentialObserver(
      observers[i],
      `native.required_observers[${i}]`,
    );
    withdrawals.insert(
      CML.RewardAddress.new(Number(networkId.network()), credential),
      0n,
    );
  }
  return withdrawals;
};

const mintToCml = (
  mint: MidgardMint,
): CML.Mint | undefined => {
  if (mint.size === 0) return undefined;
  const out = CML.Mint.new();
  for (const [policyHex, assets] of mint.entries()) {
    const cmlAssets = CML.MapAssetNameToNonZeroInt64.new();
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity === 0n) continue;
      cmlAssets.insert(
        CML.AssetName.from_raw_bytes(Buffer.from(assetNameHex, "hex")),
        quantity,
      );
    }
    out.insert_assets(CML.ScriptHash.from_hex(policyHex), cmlAssets);
  }
  return out;
};

export type DecodedMidgardNativeMint = {
  readonly mint: CML.Mint;
  readonly policyIds: readonly string[];
  readonly mintedValue: CML.Value;
  readonly burnedValue: CML.Value;
};

const valueFromMultiasset = (
  multiasset: CML.MultiAsset,
): CML.Value =>
  multiasset.policy_count() === 0
    ? CML.Value.zero()
    : CML.Value.new(0n, multiasset);

export const decodeMidgardNativeMint = (
  mint: MidgardMint,
): DecodedMidgardNativeMint | undefined => {
  if (mint.size === 0) return undefined;
  const cml = mintToCml(mint);
  if (cml === undefined) return undefined;
  const policyIds = Array.from({ length: cml.keys().len() }, (_, i) =>
    cml.keys().get(i).to_hex(),
  ).sort((a, b) => a.localeCompare(b));
  return {
    mint: cml,
    policyIds,
    mintedValue: valueFromMultiasset(cml.as_positive_multiasset()),
    burnedValue: valueFromMultiasset(cml.as_negative_multiasset()),
  };
};

const versionedScriptToCml = (
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

const outputToCml = (
  output: MidgardTxOutput,
  fieldName: string,
): CML.TransactionOutput => {
  const decoded = decodeMidgardAddressBytes(output.address);
  if (decoded.protected) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Protected Midgard addresses cannot be represented as Cardano TxOut addresses",
      fieldName,
    );
  }
  const conway = CML.ConwayFormatTxOut.new(
    CML.Address.from_raw_bytes(output.address),
    midgardValueToCmlValue(output.value),
  );
  if (output.datum !== undefined) {
    conway.set_datum_option(
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_bytes(output.datum.cbor),
      ),
    );
  }
  if (output.script_ref !== undefined) {
    conway.set_script_reference(
      versionedScriptToCml(output.script_ref, `${fieldName}.script_ref`),
    );
  }
  return CML.TransactionOutput.new_conway_format_tx_out(conway);
};

const addrWitnessesToCml = (
  witnesses: readonly VKeyWitness[],
): CML.VkeywitnessList | undefined => {
  if (witnesses.length === 0) return undefined;
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
  readonly nativeScripts?: CML.NativeScriptList;
  readonly plutusV3Scripts?: CML.PlutusV3ScriptList;
};

const scriptWitnessesToCml = (
  scripts: readonly MidgardVersionedScript[],
): DecodedCardanoScripts => {
  const nativeScripts = CML.NativeScriptList.new();
  const plutusV3Scripts = CML.PlutusV3ScriptList.new();
  for (let i = 0; i < scripts.length; i += 1) {
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

const redeemersBlobToCml = (
  redeemerBlob: Uint8Array,
): CML.Redeemers | undefined => {
  if (redeemerBlob.length === 0) return undefined;
  return CML.Redeemers.from_cbor_bytes(redeemerBlob);
};

export type MidgardToCardanoTxEncodingOptions = {
  readonly omitVkeyWitnesses?: boolean;
};

export const midgardNativeTxFullToCardanoTxEncoding = (
  tx: MidgardNativeTxFull,
  options?: MidgardToCardanoTxEncodingOptions,
): Buffer => {
  verifyMidgardNativeTxFullConsistency(tx);

  const inputs = CML.TransactionInputList.new();
  for (const ref of tx.body.spendInputs) inputs.add(outputReferenceToCml(ref));

  const outputs = CML.TransactionOutputList.new();
  for (let i = 0; i < tx.body.outputs.length; i += 1) {
    outputs.add(outputToCml(tx.body.outputs[i], `native.outputs[${i}]`));
  }

  const body = CML.TransactionBody.new(inputs, outputs, tx.body.fee);
  const networkId = toCardanoNetworkId(tx.body.networkId, "native.network_id");
  if (networkId !== undefined) body.set_network_id(networkId);

  if (tx.body.referenceInputs.length > 0) {
    const refInputs = CML.TransactionInputList.new();
    for (const ref of tx.body.referenceInputs) refInputs.add(outputReferenceToCml(ref));
    body.set_reference_inputs(refInputs);
  }

  if (tx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE) {
    body.set_validity_interval_start(tx.body.validityIntervalStart);
  }
  if (tx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE) {
    body.set_ttl(tx.body.validityIntervalEnd);
  }

  const withdrawals = observersToWithdrawalsCml(tx.body.requiredObservers, networkId);
  if (withdrawals !== undefined) body.set_withdrawals(withdrawals);

  if (tx.body.requiredSigners.length > 0) {
    body.set_required_signers(requiredSignersToCml(tx.body.requiredSigners));
  }

  const cmlMint = mintToCml(tx.body.mint);
  if (cmlMint !== undefined) body.set_mint(cmlMint);

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
    const vkeyWitnesses = addrWitnessesToCml(tx.witnessSet.addrTxWits);
    if (vkeyWitnesses !== undefined) witnessSet.set_vkeywitnesses(vkeyWitnesses);
  }

  const scripts = scriptWitnessesToCml(tx.witnessSet.scriptTxWits);
  if (scripts.nativeScripts !== undefined) {
    witnessSet.set_native_scripts(scripts.nativeScripts);
  }
  if (scripts.plutusV3Scripts !== undefined) {
    witnessSet.set_plutus_v3_scripts(scripts.plutusV3Scripts);
  }

  const redeemers = redeemersBlobToCml(tx.witnessSet.redeemerTxWits);
  if (redeemers !== undefined) witnessSet.set_redeemers(redeemers);

  return Buffer.from(
    CML.Transaction.new(
      body,
      witnessSet,
      tx.validity === "TxIsValid",
      undefined,
    ).to_cbor_bytes(),
  );
};
