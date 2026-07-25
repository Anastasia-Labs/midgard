import {
  asArray,
  asBytes,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeSingleCbor,
  deriveMidgardNativeTxCompactV1,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  type MidgardNativeTxFullV1,
  verifyMidgardNativeTxFullConsistencyV1,
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
      continue;
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
  tx: MidgardNativeTxFullV1,
  witnesses: readonly VKeyWitness[],
): {
  readonly tx: MidgardNativeTxFullV1;
  readonly witnesses: readonly VKeyWitness[];
} => {
  const bodyHash = computeMidgardNativeTxIdV1(tx);
  const merged = canonicalizeAddrWitnesses(bodyHash, [
    ...decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor),
    ...witnesses,
  ]);
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimageCbor: encodeAddrWitnesses(merged),
  };
  const signedTx: MidgardNativeTxFullV1 = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompactV1(
      tx.body,
      witnessSet,
      tx.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistencyV1(signedTx);
  return { tx: signedTx, witnesses: merged };
};

export const decodeImportAddrWitnesses = (
  tx: MidgardNativeTxFullV1,
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
  const bodyHash = computeMidgardNativeTxIdV1(tx);
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
  tx: MidgardNativeTxFullV1,
  wallet: MidgardWallet,
): Promise<MidgardNativeTxFullV1> => {
  const bodyHash = computeMidgardNativeTxIdV1(tx);
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
    Buffer.from(bodyHash, "hex"),
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
  const signerKeyHashes = witnesses.map(witnessKeyHash);
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
      witnessCborBytes(witness).toString("hex"),
    ),
    signerKeyHashes,
  };
};

export const partialWitnessBundleFromWitnesses = (
  tx: MidgardNativeTxFullV1,
  witnesses: readonly VKeyWitness[],
): MidgardPartialWitnessBundle => {
  const bodyHash = computeMidgardNativeTxIdV1(tx);
  const canonical = canonicalizeAddrWitnesses(bodyHash, witnesses);
  if (canonical.length === 0) {
    throw new SigningError("Partial witness bundle must contain witnesses");
  }
  const midgardNativeTxVersion = Number(tx.version);
  if (
    !Number.isSafeInteger(midgardNativeTxVersion) ||
    midgardNativeTxVersion <= 0
  ) {
    throw new SigningError("Invalid partial witness bundle tx version");
  }
  const txId = bodyHash.toString("hex");
  return {
    kind: PARTIAL_WITNESS_BUNDLE_KIND,
    version: PARTIAL_WITNESS_BUNDLE_VERSION,
    midgardNativeTxVersion,
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
  if ("cbor" in input) {
    return decodePartialWitnessBundle(input.cbor);
  }
  if ("cborHex" in input) {
    return decodePartialWitnessBundle(input.cborHex);
  }
  return normalizePartialWitnessBundle(input);
};

export const assertPartialBundleMatchesTx = (
  tx: MidgardNativeTxFullV1,
  bundle: MidgardPartialWitnessBundle,
): void => {
  const txId = computeMidgardNativeTxIdV1(tx).toString("hex");
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
  tx: MidgardNativeTxFullV1,
  expectedWitnessCount: number,
): MidgardNativeTxFullV1 => {
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
        computeMidgardNativeTxIdV1(tx),
        dummyWitnessPrivateKey(index),
      ),
    );
  }
  const witnessSet = {
    ...tx.witnessSet,
    addrTxWitsPreimageCbor: encodeAddrWitnesses(estimatedWitnesses),
  };
  const estimatedTx: MidgardNativeTxFullV1 = {
    ...tx,
    witnessSet,
    compact: deriveMidgardNativeTxCompactV1(
      tx.body,
      witnessSet,
      tx.validity,
      tx.version,
    ),
  };
  verifyMidgardNativeTxFullConsistencyV1(estimatedTx);
  return estimatedTx;
};

export const estimatedSignedTxByteLength = (
  tx: MidgardNativeTxFullV1,
  expectedWitnessCount: number,
): number =>
  encodeMidgardNativeTxCanonicalV1(
    withEstimatedAddrWitnesses(tx, expectedWitnessCount),
  ).length;
