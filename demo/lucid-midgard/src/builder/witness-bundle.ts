import {
  asArray,
  asBytes,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeSingleCbor,
  deriveMidgardNativeTxCompact,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  type MidgardNativeTxFull,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { CML } from "@lucid-evolution/lucid";

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
  readonly kind: "MidgardPartialWitnessBundleV1";
  readonly version: 1;
  readonly midgardNativeTxVersion: 1;
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

const PARTIAL_WITNESS_BUNDLE_KIND = "MidgardPartialWitnessBundleV1";
const PARTIAL_WITNESS_BUNDLE_VERSION = 1;
const PARTIAL_WITNESS_BUNDLE_FIELDS = [
  "kind",
  "version",
  "midgardNativeTxVersion",
  "txId",
  "bodyHash",
  "witnesses",
  "signerKeyHashes",
] as const;

const compareCanonicalStrings = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const assertExactObjectKeys = (
  value: object,
  expectedKeys: readonly string[],
  fieldName: string,
): void => {
  if (Array.isArray(value)) {
    throw new SigningError(`${fieldName} must be an object`);
  }
  const actual = Object.keys(value).sort(compareCanonicalStrings);
  const expected = [...expectedKeys].sort(compareCanonicalStrings);
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new SigningError(
      `${fieldName} must contain exactly: ${expectedKeys.join(", ")}`,
    );
  }
};

export const nonEmptyBytesFromHex = (
  hex: string,
  fieldName: string,
): Buffer => {
  try {
    return hexToBytes(hex, { fieldName });
  } catch {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
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
  decodeMidgardNativeByteListPreimage(preimageCbor, "native.addr_tx_wits").map(
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
      throw new SigningError("Duplicate vkey witness", keyHash);
    }
    byKeyHash.set(keyHash, witness);
  }
  return [...byKeyHash.entries()]
    .sort(([left], [right]) => compareCanonicalStrings(left, right))
    .map(([, witness]) => witness);
};

const encodeAddrWitnesses = (witnesses: readonly VKeyWitness[]): Buffer =>
  encodeCbor(uniqueAddrWitnesses(witnesses).map(witnessCborBytes));

export const applyAddrWitnessesToTx = (
  tx: MidgardNativeTxFull,
  witnesses: readonly VKeyWitness[],
): {
  readonly tx: MidgardNativeTxFull;
  readonly witnesses: readonly VKeyWitness[];
} => {
  const bodyHash = computeMidgardNativeTxId(tx);
  const merged = canonicalizeAddrWitnesses(bodyHash, [
    ...decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor),
    ...witnesses,
  ]);
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimageCbor: encodeAddrWitnesses(merged),
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
    witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  } catch (cause) {
    throw new SigningError(
      "Invalid address witness preimage",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const bodyHash = computeMidgardNativeTxId(tx);
  const byKeyHash = new Map<string, Buffer>();
  for (const witness of witnesses) {
    assertVKeyWitness(bodyHash, witness);
    const keyHash = witnessKeyHash(witness);
    const bytes = witnessCborBytes(witness);
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
  assertExactObjectKeys(
    bundle,
    PARTIAL_WITNESS_BUNDLE_FIELDS,
    "Partial witness bundle",
  );
  if (bundle.kind !== PARTIAL_WITNESS_BUNDLE_KIND) {
    throw new SigningError("Unsupported partial witness bundle kind");
  }
  if (bundle.version !== PARTIAL_WITNESS_BUNDLE_VERSION) {
    throw new SigningError("Unsupported partial witness bundle version");
  }
  if (bundle.midgardNativeTxVersion !== Number(MIDGARD_NATIVE_TX_VERSION)) {
    throw new SigningError(
      "Unsupported partial witness bundle native transaction version",
    );
  }
  const txId = canonicalPartialBundleHexString(
    bundle.txId,
    "partial bundle txId",
    32,
  );
  const bodyHash = canonicalPartialBundleHexString(
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
  const declaredWitnesses = bundle.witnesses.map((witnessHex, index) =>
    canonicalPartialBundleHexString(
      witnessHex,
      `partial bundle witnesses[${index.toString()}]`,
    ),
  );
  const witnesses = canonicalizeAddrWitnesses(
    Buffer.from(bodyHash, "hex"),
    declaredWitnesses.map((witnessHex, index) =>
      decodeCanonicalVKeyWitness(
        Buffer.from(witnessHex, "hex"),
        `partial bundle witnesses[${index.toString()}]`,
      ),
    ),
  );
  const canonicalWitnesses = witnesses.map((witness) =>
    witnessCborBytes(witness).toString("hex"),
  );
  if (
    declaredWitnesses.length !== canonicalWitnesses.length ||
    declaredWitnesses.some(
      (witness, index) => witness !== canonicalWitnesses[index],
    )
  ) {
    throw new SigningError(
      "Partial witness bundle witnesses are not in canonical order",
    );
  }
  const signerKeyHashes = witnesses.map(witnessKeyHash);
  const declaredSignerKeyHashes = bundle.signerKeyHashes.map((keyHash, index) =>
    canonicalPartialBundleHexString(
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
    witnesses: canonicalWitnesses,
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
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new SigningError(
      "Unsupported partial witness bundle native transaction version",
    );
  }
  const txId = bodyHash.toString("hex");
  return {
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion: PARTIAL_WITNESS_BUNDLE_VERSION,
    txId,
    bodyHash: txId,
    witnesses: canonical.map((witness) =>
      witnessCborBytes(witness).toString("hex"),
    ),
    signerKeyHashes: canonical.map(witnessKeyHash),
  };
};

const encodeNormalizedPartialWitnessBundle = (
  normalized: MidgardPartialWitnessBundle,
): Buffer =>
  encodeCbor([
    normalized.kind,
    normalized.version,
    normalized.midgardNativeTxVersion,
    Buffer.from(normalized.txId, "hex"),
    Buffer.from(normalized.bodyHash, "hex"),
    normalized.witnesses.map((witness) => Buffer.from(witness, "hex")),
    normalized.signerKeyHashes.map((keyHash) => Buffer.from(keyHash, "hex")),
  ]);

export const encodePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundle,
): Buffer =>
  encodeNormalizedPartialWitnessBundle(normalizePartialWitnessBundle(bundle));

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
  let bytes: Buffer;
  try {
    bytes = hexToBytes(value, { fieldName });
  } catch {
    throw new SigningError(`${fieldName} must be hex`);
  }
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

const canonicalPartialBundleHexString = (
  value: unknown,
  fieldName: string,
  expectedBytes?: 28 | 32,
): string => {
  const normalized = partialBundleHexString(value, fieldName, expectedBytes);
  if (value !== normalized) {
    throw new SigningError(`${fieldName} must use canonical lowercase hex`);
  }
  return normalized;
};

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
    midgardNativeTxVersion: midgardNativeTxVersion as 1,
    txId: asBytes(decoded[3], "partial witness bundle tx id").toString("hex"),
    bodyHash: asBytes(decoded[4], "partial witness bundle body hash").toString(
      "hex",
    ),
    witnesses: witnessBytes,
    signerKeyHashes,
  });
  if (!encodeNormalizedPartialWitnessBundle(normalized).equals(bytes)) {
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
  const record = input as unknown as Record<string, unknown>;
  if (
    Object.prototype.hasOwnProperty.call(record, "cbor") ||
    Object.prototype.hasOwnProperty.call(record, "cborHex")
  ) {
    if (Object.prototype.hasOwnProperty.call(record, "cbor")) {
      assertExactObjectKeys(record, ["cbor"], "Partial witness bundle wrapper");
      if (
        !(record.cbor instanceof Uint8Array) &&
        typeof record.cbor !== "string"
      ) {
        throw new SigningError(
          "Partial witness bundle wrapper cbor must be bytes or hex",
        );
      }
      return decodePartialWitnessBundle(record.cbor);
    }
    assertExactObjectKeys(
      record,
      ["cborHex"],
      "Partial witness bundle wrapper",
    );
    if (typeof record.cborHex !== "string") {
      throw new SigningError(
        "Partial witness bundle wrapper cborHex must be hex",
      );
    }
    return decodePartialWitnessBundle(record.cborHex);
  }
  return normalizePartialWitnessBundle(input as MidgardPartialWitnessBundle);
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
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
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
    addrTxWitsPreimageCbor: encodeAddrWitnesses(estimatedWitnesses),
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
  encodeMidgardNativeTxCanonical(
    withEstimatedAddrWitnesses(tx, expectedWitnessCount),
  ).length;
