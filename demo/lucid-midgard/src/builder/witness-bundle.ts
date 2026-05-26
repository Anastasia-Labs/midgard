import { CML } from "@lucid-evolution/lucid";
import {
  asArray,
  asBytes,
  computeMidgardNativeTxId,
  decodeMidgardNativeAddrWitsPreimageAsCbor,
  decodeSingleCbor,
  deriveMidgardNativeTxCompact,
  encodeCbor,
  encodeMidgardNativeTxFull,
  encodeMidgardVKeyWitnessListPreimage,
  verifyMidgardNativeTxFullConsistency,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError, SigningError } from "../core/errors.js";
import {
  assertVKeyWitness,
  makeVKeyWitness,
  type MidgardWallet,
  type PrivateKey,
  type VKeyWitness,
} from "../wallet.js";

export type VKeyWitnessInput = VKeyWitness | Uint8Array | string;

export type MidgardPartialWitnessBundle = {
  readonly kind: "MidgardPartialWitnessBundle";
  readonly version: 1;
  readonly midgardNativeTxVersion: number;
  readonly txId: string;
  readonly bodyHash: string;
  readonly witnesses: readonly string[];
  readonly signerKeyHashes: readonly string[];
};

export type PartialWitnessBundleInput =
  | MidgardPartialWitnessBundle
  | Uint8Array
  | string
  | { readonly cbor: Uint8Array | string }
  | { readonly cborHex: string };

const PARTIAL_WITNESS_BUNDLE_KIND = "MidgardPartialWitnessBundle";
const PARTIAL_WITNESS_BUNDLE_VERSION = 1;

const compareCanonicalStrings = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

export const nonEmptyBytesFromHex = (
  hex: string,
  fieldName: string,
): Buffer => {
  const normalized = hex.trim().toLowerCase();
  if (
    normalized.length === 0 ||
    normalized.length % 2 !== 0 ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
  return Buffer.from(normalized, "hex");
};

const strictBytesFromHex = (
  hex: string,
  fieldName: string,
  expectedBytes?: 28 | 32,
): Buffer => {
  const bytes = nonEmptyBytesFromHex(hex, fieldName);
  if (expectedBytes !== undefined && bytes.length !== expectedBytes) {
    throw new BuilderInvariantError(
      `${fieldName} must be a ${expectedBytes.toString()}-byte hex string`,
      hex,
    );
  }
  return bytes;
};

const decodeCanonicalVKeyWitness = (
  witnessBytes: Uint8Array,
  fieldName: string,
): VKeyWitness => {
  try {
    const bytes = Buffer.from(witnessBytes);
    const witness = CML.Vkeywitness.from_cbor_bytes(bytes);
    const canonical = Buffer.from(witness.to_cbor_bytes());
    if (!canonical.equals(bytes)) {
      throw new SigningError(
        `${fieldName} must be canonical vkey witness CBOR`,
      );
    }
    return witness;
  } catch (cause) {
    if (cause instanceof SigningError) {
      throw cause;
    }
    throw new SigningError(
      `Invalid ${fieldName}`,
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const decodeAddrWitnesses = (
  preimageCbor: Uint8Array,
): readonly VKeyWitness[] =>
  decodeMidgardNativeAddrWitsPreimageAsCbor(preimageCbor, "native.addr_tx_wits").map(
    (witnessBytes, index) =>
      decodeCanonicalVKeyWitness(
        witnessBytes,
        `native.addr_tx_wits[${index.toString()}]`,
      ),
  );

export const addrWitnessMetadata = (
  witnesses: readonly VKeyWitness[],
): {
  readonly addrWitnessCount: number;
  readonly signedBy: readonly string[];
} => ({
  addrWitnessCount: witnesses.length,
  signedBy: witnesses.map(witnessKeyHash),
});

export const addrWitnessKeyHashes = (
  witnesses: readonly VKeyWitness[],
): readonly string[] => witnesses.map(witnessKeyHash);

const witnessKeyHash = (witness: VKeyWitness): string =>
  witness.vkey().hash().to_hex();

const witnessCborBytes = (witness: VKeyWitness): Buffer =>
  Buffer.from(witness.to_cbor_bytes());

const vkeyWitnessInputBytes = (
  witness: VKeyWitnessInput,
  fieldName: string,
): Buffer => {
  if (typeof witness === "string") {
    return nonEmptyBytesFromHex(witness, fieldName);
  }
  if (witness instanceof Uint8Array) {
    return Buffer.from(witness);
  }
  return Buffer.from(witness.to_cbor_bytes());
};

export const normalizeVKeyWitnessInput = (
  witness: VKeyWitnessInput,
  bodyHash: Uint8Array,
  fieldName: string,
): VKeyWitness => {
  const decoded = decodeCanonicalVKeyWitness(
    vkeyWitnessInputBytes(witness, fieldName),
    fieldName,
  );
  return assertVKeyWitness(bodyHash, decoded);
};

const canonicalizeAddrWitnesses = (
  bodyHash: Uint8Array,
  witnesses: readonly VKeyWitness[],
): readonly VKeyWitness[] =>
  uniqueAddrWitnesses(
    witnesses.map((witness) => assertVKeyWitness(bodyHash, witness)),
  );

const uniqueAddrWitnesses = (
  witnesses: readonly VKeyWitness[],
): readonly VKeyWitness[] => {
  const byKeyHash = new Map<string, VKeyWitness>();
  for (const witness of witnesses) {
    const keyHash = witnessKeyHash(witness);
    const existing = byKeyHash.get(keyHash);
    if (existing !== undefined) {
      if (!witnessCborBytes(existing).equals(witnessCborBytes(witness))) {
        throw new SigningError(
          "Conflicting vkey witnesses for the same key hash",
          keyHash,
        );
      }
      continue;
    }
    byKeyHash.set(keyHash, witness);
  }
  return [...byKeyHash.entries()]
    .sort(([left], [right]) => compareCanonicalStrings(left, right))
    .map(([, witness]) => witness);
};

const encodeAddrWitnesses = (witnesses: readonly VKeyWitness[]): Buffer =>
  encodeMidgardVKeyWitnessListPreimage(
    uniqueAddrWitnesses(witnesses).map((w) => ({
      vkey: Buffer.from(w.vkey().to_raw_bytes()),
      signature: Buffer.from(w.ed25519_signature().to_raw_bytes()),
    })),
  );

export const applyAddrWitnessesToTx = (
  tx: MidgardNativeTxFull,
  witnesses: readonly VKeyWitness[],
): {
  readonly tx: MidgardNativeTxFull;
  readonly witnesses: readonly VKeyWitness[];
} => {
  const bodyHash = computeMidgardNativeTxId(tx);
  const merged = canonicalizeAddrWitnesses(bodyHash, [
    ...decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimage),
    ...witnesses,
  ]);
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimage: encodeAddrWitnesses(merged),
  };
  const signedTx: MidgardNativeTxFull = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(
      tx.body,
      witnessSet,
      tx.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistency(signedTx);
  return { tx: signedTx, witnesses: merged };
};

export const decodeImportAddrWitnesses = (
  tx: MidgardNativeTxFull,
): readonly VKeyWitness[] => {
  let witnesses: readonly VKeyWitness[];
  try {
    witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimage);
  } catch (cause) {
    throw new SigningError(
      "Invalid address witness preimage",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const byKeyHash = new Map<string, Buffer>();
  for (const witness of witnesses) {
    assertVKeyWitness(computeMidgardNativeTxId(tx), witness);
    const keyHash = witness.vkey().hash().to_hex();
    const bytes = Buffer.from(witness.to_cbor_bytes());
    const existing = byKeyHash.get(keyHash);
    if (existing !== undefined) {
      throw new SigningError(
        existing.equals(bytes)
          ? "Duplicate address witness"
          : "Conflicting address witness",
        keyHash,
      );
    }
    byKeyHash.set(keyHash, bytes);
  }
  return witnesses;
};

export const signMidgardNativeTx = async (
  tx: MidgardNativeTxFull,
  wallet: MidgardWallet,
): Promise<MidgardNativeTxFull> => {
  const bodyHash = computeMidgardNativeTxId(tx);
  const witness = assertVKeyWitness(
    bodyHash,
    await wallet.signBodyHash(bodyHash),
  );
  return applyAddrWitnessesToTx(tx, [witness]).tx;
};

const normalizePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundle,
): MidgardPartialWitnessBundle => {
  if (typeof bundle !== "object" || bundle === null) {
    throw new SigningError("Partial witness bundle must be an object");
  }
  if (bundle.kind !== PARTIAL_WITNESS_BUNDLE_KIND) {
    throw new SigningError("Unsupported partial witness bundle kind");
  }
  if (bundle.version !== PARTIAL_WITNESS_BUNDLE_VERSION) {
    throw new SigningError("Unsupported partial witness bundle version");
  }
  if (
    !Number.isSafeInteger(bundle.midgardNativeTxVersion) ||
    bundle.midgardNativeTxVersion <= 0
  ) {
    throw new SigningError("Invalid partial witness bundle tx version");
  }
  const txId = partialBundleHexString(bundle.txId, "partial bundle txId", 32);
  const bodyHash = partialBundleHexString(
    bundle.bodyHash,
    "partial bundle bodyHash",
    32,
  );
  if (bodyHash !== txId) {
    throw new SigningError("Partial witness bundle tx id/body hash mismatch");
  }
  if (!Array.isArray(bundle.witnesses)) {
    throw new SigningError("Partial witness bundle witnesses must be an array");
  }
  if (!Array.isArray(bundle.signerKeyHashes)) {
    throw new SigningError(
      "Partial witness bundle signerKeyHashes must be an array",
    );
  }
  if (bundle.witnesses.length === 0) {
    throw new SigningError("Partial witness bundle must contain witnesses");
  }
  const witnesses = canonicalizeAddrWitnesses(
    strictBytesFromHex(bodyHash, "partial bundle bodyHash", 32),
    bundle.witnesses.map((witnessHex, index) =>
      decodeCanonicalVKeyWitness(
        partialBundleHexBytes(
          witnessHex,
          `partial bundle witnesses[${index.toString()}]`,
        ),
        `partial bundle witnesses[${index.toString()}]`,
      ),
    ),
  );
  const signerKeyHashes = witnesses.map((witness) =>
    witness.vkey().hash().to_hex(),
  );
  const declaredSignerKeyHashes = bundle.signerKeyHashes.map((keyHash, index) =>
    partialBundleHexString(
      keyHash,
      `partial bundle signerKeyHashes[${index.toString()}]`,
      28,
    ),
  );
  if (
    declaredSignerKeyHashes.length !== signerKeyHashes.length ||
    declaredSignerKeyHashes.some(
      (keyHash, index) => keyHash !== signerKeyHashes[index],
    )
  ) {
    throw new SigningError(
      "Partial witness bundle signer metadata does not match witnesses",
    );
  }
  return {
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion: bundle.midgardNativeTxVersion,
    txId,
    bodyHash,
    witnesses: witnesses.map((witness) =>
      Buffer.from(witness.to_cbor_bytes()).toString("hex"),
    ),
    signerKeyHashes,
  };
};

export const partialWitnessBundleFromWitnesses = (
  tx: MidgardNativeTxFull,
  witnesses: readonly VKeyWitness[],
): MidgardPartialWitnessBundle => {
  const bodyHash = computeMidgardNativeTxId(tx);
  const canonical = canonicalizeAddrWitnesses(bodyHash, witnesses);
  if (canonical.length === 0) {
    throw new SigningError("Partial witness bundle must contain witnesses");
  }
  const txId = bodyHash.toString("hex");
  return normalizePartialWitnessBundle({
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion: Number(tx.version),
    txId,
    bodyHash: txId,
    witnesses: canonical.map((witness) =>
      Buffer.from(witness.to_cbor_bytes()).toString("hex"),
    ),
    signerKeyHashes: canonical.map((witness) => witness.vkey().hash().to_hex()),
  });
};

export const encodePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundle,
): Buffer => {
  const normalized = normalizePartialWitnessBundle(bundle);
  return encodeCbor([
    normalized.kind,
    normalized.version,
    normalized.midgardNativeTxVersion,
    strictBytesFromHex(normalized.txId, "partial bundle txId", 32),
    strictBytesFromHex(normalized.bodyHash, "partial bundle bodyHash", 32),
    normalized.witnesses.map((witness) =>
      nonEmptyBytesFromHex(witness, "partial bundle witness"),
    ),
    normalized.signerKeyHashes.map((keyHash) =>
      strictBytesFromHex(keyHash, "partial bundle signerKeyHash", 28),
    ),
  ]);
};

const assertPartialBundleNumber = (
  value: unknown,
  fieldName: string,
): number => {
  if (!Number.isSafeInteger(value) || Number(value) <= 0) {
    throw new SigningError(`${fieldName} must be a positive safe integer`);
  }
  return Number(value);
};

const partialBundleHexBytes = (
  value: unknown,
  fieldName: string,
  expectedBytes?: 28 | 32,
): Buffer => {
  if (typeof value !== "string") {
    throw new SigningError(`${fieldName} must be hex`);
  }
  const normalized = value.trim().toLowerCase();
  if (
    normalized.length === 0 ||
    normalized.length % 2 !== 0 ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new SigningError(`${fieldName} must be hex`);
  }
  const bytes = Buffer.from(normalized, "hex");
  if (expectedBytes !== undefined && bytes.length !== expectedBytes) {
    throw new SigningError(
      `${fieldName} must be a ${expectedBytes.toString()}-byte hex string`,
    );
  }
  return bytes;
};

const partialBundleHexString = (
  value: unknown,
  fieldName: string,
  expectedBytes?: 28 | 32,
): string =>
  partialBundleHexBytes(value, fieldName, expectedBytes).toString("hex");

export const decodePartialWitnessBundle = (
  input: Uint8Array | string,
): MidgardPartialWitnessBundle => {
  const bytes =
    typeof input === "string"
      ? nonEmptyBytesFromHex(input, "partial witness bundle CBOR")
      : Buffer.from(input);
  const decoded = asArray(decodeSingleCbor(bytes), "partial_witness_bundle");
  if (decoded.length !== 7) {
    throw new SigningError("Partial witness bundle must be a 7-item tuple");
  }
  if (decoded[0] !== PARTIAL_WITNESS_BUNDLE_KIND) {
    throw new SigningError("Unsupported partial witness bundle kind");
  }
  const version = assertPartialBundleNumber(
    decoded[1],
    "partial witness bundle version",
  );
  const midgardNativeTxVersion = assertPartialBundleNumber(
    decoded[2],
    "partial witness bundle tx version",
  );
  const witnessBytes = asArray(
    decoded[5],
    "partial witness bundle witnesses",
  ).map((item, index) =>
    asBytes(
      item,
      `partial witness bundle witnesses[${index.toString()}]`,
    ).toString("hex"),
  );
  const signerKeyHashes = asArray(
    decoded[6],
    "partial witness bundle signer key hashes",
  ).map((item, index) =>
    asBytes(
      item,
      `partial witness bundle signerKeyHashes[${index.toString()}]`,
    ).toString("hex"),
  );
  const normalized = normalizePartialWitnessBundle({
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: version as 1,
    midgardNativeTxVersion,
    txId: asBytes(decoded[3], "partial witness bundle tx id").toString("hex"),
    bodyHash: asBytes(decoded[4], "partial witness bundle body hash").toString(
      "hex",
    ),
    witnesses: witnessBytes,
    signerKeyHashes,
  });
  if (!encodePartialWitnessBundle(normalized).equals(bytes)) {
    throw new SigningError("Partial witness bundle CBOR is not canonical");
  }
  return normalized;
};

export const parsePartialWitnessBundle = (
  input: PartialWitnessBundleInput,
): MidgardPartialWitnessBundle => {
  if (input instanceof Uint8Array || typeof input === "string") {
    return decodePartialWitnessBundle(input);
  }
  if (typeof input !== "object" || input === null) {
    throw new SigningError("Partial witness bundle input must be an object");
  }
  if ("cbor" in input) {
    return decodePartialWitnessBundle(input.cbor);
  }
  if ("cborHex" in input) {
    return decodePartialWitnessBundle(input.cborHex);
  }
  return normalizePartialWitnessBundle(input);
};

export const assertPartialBundleMatchesTx = (
  tx: MidgardNativeTxFull,
  bundle: MidgardPartialWitnessBundle,
): void => {
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  if (bundle.txId !== txId || bundle.bodyHash !== txId) {
    throw new SigningError(
      "Partial witness bundle belongs to a different transaction",
      `expected=${txId} actual=${bundle.txId}`,
    );
  }
  if (bundle.midgardNativeTxVersion !== Number(tx.version)) {
    throw new SigningError(
      "Partial witness bundle native transaction version mismatch",
      `expected=${tx.version.toString()} actual=${bundle.midgardNativeTxVersion.toString()}`,
    );
  }
};

const dummyWitnessPrivateKey = (index: number): PrivateKey => {
  const seed = Buffer.alloc(32);
  let value = index + 1;
  for (let offset = seed.length - 1; offset >= 0 && value > 0; offset -= 1) {
    seed[offset] = value & 0xff;
    value = Math.floor(value / 0x100);
  }
  return CML.PrivateKey.from_normal_bytes(seed);
};

export const withEstimatedAddrWitnesses = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): MidgardNativeTxFull => {
  if (expectedWitnessCount === 0) {
    return tx;
  }
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimage);
  if (witnesses.length >= expectedWitnessCount) {
    return tx;
  }
  const estimatedWitnesses = [...witnesses];
  for (let index = witnesses.length; index < expectedWitnessCount; index += 1) {
    estimatedWitnesses.push(
      makeVKeyWitness(
        computeMidgardNativeTxId(tx),
        dummyWitnessPrivateKey(index),
      ),
    );
  }
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimage: encodeAddrWitnesses(estimatedWitnesses),
  };
  const estimatedTx: MidgardNativeTxFull = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(
      tx.body,
      witnessSet,
      tx.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistency(estimatedTx);
  return estimatedTx;
};

export const estimatedSignedTxByteLength = (
  tx: MidgardNativeTxFull,
  expectedWitnessCount: number,
): number =>
  encodeMidgardNativeTxFull(
    withEstimatedAddrWitnesses(tx, expectedWitnessCount),
  ).length;
