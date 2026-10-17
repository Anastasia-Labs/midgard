import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxCompact,
  encodeCbor,
  type MidgardNativeTxFull,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { CML } from "@lucid-evolution/lucid";

import { BuilderInvariantError, SigningError } from "../core/errors.js";
import {
  assertVKeyWitness,
  type MidgardWallet,
  type VKeyWitness,
} from "../wallet.js";

export type VKeyWitnessInput = VKeyWitness | Uint8Array | string;

export type MidgardPartialWitnessBundleV1 = {
  readonly kind: "MidgardPartialWitnessBundleV1";
  readonly version: 1;
  readonly midgardNativeTxVersion: 1;
  readonly txId: string;
  readonly bodyHash: string;
  readonly witnesses: readonly string[];
  readonly signerKeyHashes: readonly string[];
};

export type PartialWitnessBundleInput =
  | MidgardPartialWitnessBundleV1
  | Uint8Array
  | string
  | { readonly cbor: Uint8Array | string }
  | { readonly cborHex: string };

export const PARTIAL_WITNESS_BUNDLE_KIND = "MidgardPartialWitnessBundleV1";

export const PARTIAL_WITNESS_BUNDLE_VERSION = 1;

export const PARTIAL_WITNESS_BUNDLE_FIELDS = [
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

export const assertExactObjectKeys = (
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

export const decodeCanonicalVKeyWitness = (
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

export const witnessKeyHash = (witness: VKeyWitness): string =>
  witness.vkey().hash().to_hex();

export const witnessCborBytes = (witness: VKeyWitness): Buffer =>
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

export const canonicalizeAddrWitnesses = (
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

export const encodeAddrWitnesses = (
  witnesses: readonly VKeyWitness[],
): Buffer => encodeCbor(uniqueAddrWitnesses(witnesses).map(witnessCborBytes));

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

export const canonicalPartialBundleHexString = (
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
