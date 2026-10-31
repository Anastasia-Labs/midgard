import {
  asArray,
  asBytes,
  computeMidgardNativeTxId,
  decodeSingleCbor,
  encodeCbor,
  MIDGARD_NATIVE_TX_VERSION,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import { SigningError } from "../core/errors.js";
import { type PrivateKey, type VKeyWitness } from "../wallet.js";
import {
  assertExactObjectKeys,
  canonicalizeAddrWitnesses,
  canonicalPartialBundleHexString,
  decodeCanonicalVKeyWitness,
  type MidgardPartialWitnessBundleV1,
  nonEmptyBytesFromHex,
  PARTIAL_WITNESS_BUNDLE_FIELDS,
  PARTIAL_WITNESS_BUNDLE_KIND,
  PARTIAL_WITNESS_BUNDLE_VERSION,
  type PartialWitnessBundleInput,
  witnessCborBytes,
  witnessKeyHash,
} from "./witness-bundle.decode-import-addr-witnesses.js";

const normalizePartialWitnessBundle = (
  bundle: MidgardPartialWitnessBundleV1,
): MidgardPartialWitnessBundleV1 => {
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
): MidgardPartialWitnessBundleV1 => {
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
  normalized: MidgardPartialWitnessBundleV1,
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
  bundle: MidgardPartialWitnessBundleV1,
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

export const decodePartialWitnessBundle = (
  input: Uint8Array | string,
): MidgardPartialWitnessBundleV1 => {
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
): MidgardPartialWitnessBundleV1 => {
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
  return normalizePartialWitnessBundle(input as MidgardPartialWitnessBundleV1);
};

export const assertPartialBundleMatchesTx = (
  tx: MidgardNativeTxFull,
  bundle: MidgardPartialWitnessBundleV1,
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

export const dummyWitnessPrivateKey = (index: number): PrivateKey => {
  const seed = Buffer.alloc(32);
  let value = index + 1;
  for (let offset = seed.length - 1; offset >= 0 && value > 0; offset -= 1) {
    seed[offset] = value & 0xff;
    value = Math.floor(value / 0x100);
  }
  return CML.PrivateKey.from_normal_bytes(seed);
};
