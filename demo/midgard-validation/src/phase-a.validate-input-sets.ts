import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import { MidgardTxCodecError } from "@al-ft/midgard-core/codec";
import { type MidgardConsensusViolationCode } from "@al-ft/midgard-core/consensus-validation";
import { CML } from "@lucid-evolution/lucid";

import { MidgardLedgerTxDecodeError } from "./ledger-tx/codec.js";
import type {
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
} from "./ledger-tx/types.js";
import { runInputSetMerge } from "./phase-a.input-set-merge.js";
import type { RejectSubject } from "./reject-subject.js";
import {
  PhaseALocalContext,
  RejectCode,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import { midgardOutRefToCbor } from "./validation-candidate.js";

export const reject = (
  txId: Buffer,
  code: RejectCode,
  detail: string | null = null,
  consensusPhase: MidgardValidationPhaseName = "canonicalDecode",
  subject?: RejectSubject,
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
  ...(subject === undefined ? {} : { subject }),
});

export const codecErrorDetail = (error: unknown): string => {
  if (error instanceof MidgardLedgerTxDecodeError) {
    return codecErrorDetail(error.causeValue);
  }
  if (error instanceof MidgardTxCodecError) {
    return error.detail === null
      ? `${error.code}: ${error.message}`
      : `${error.code}: ${error.message} (${error.detail})`;
  }
  return String(error);
};

export const consensusProfileRejectCode = (
  code: MidgardConsensusViolationCode,
): RejectCode => {
  switch (code) {
    case "E_TX_VERSION":
      return RejectCodes.TxVersion;
    case "E_TX_SIZE":
      return RejectCodes.TxSize;
    case "E_IS_VALID_FALSE_FORBIDDEN":
      return RejectCodes.IsValidFalseForbidden;
    case "E_AUX_DATA_FORBIDDEN":
      return RejectCodes.AuxDataForbidden;
    case "E_INPUT_COUNT":
      return RejectCodes.InputCount;
    case "E_REFERENCE_INPUT_COUNT":
      return RejectCodes.ReferenceInputCount;
    case "E_OUTPUT_COUNT":
      return RejectCodes.OutputCount;
    case "E_ADDRESS_WITNESS_COUNT":
      return RejectCodes.AddressWitnessCount;
    case "E_REQUIRED_SIGNER_COUNT":
      return RejectCodes.RequiredSignerCount;
    case "E_SCRIPT_EXECUTION_COUNT":
      return RejectCodes.ScriptExecutionCount;
    case "E_OBSERVER_COUNT":
      return RejectCodes.ObserverCount;
    case "E_FIELD_PREIMAGE_SIZE":
      return RejectCodes.FieldPreimageSize;
    case "E_LEDGER_OUTPUT_SIZE":
      return RejectCodes.LedgerOutputSize;
    case "E_VALUE_SIZE":
      return RejectCodes.ValueSize;
    case "E_SCRIPT_PROGRAM_SIZE":
      return RejectCodes.ScriptProgramSize;
    case "E_SCRIPT_PROGRAM_ENCODING":
      return RejectCodes.ScriptProgramEncoding;
  }
};

const EMPTY_HASH_HEXES: readonly string[] = [];

const DEFAULT_PUBLIC_KEY_CACHE_MAX_ENTRIES = 4_096;

const publicKeyCache = new Map<string, CML.PublicKey>();

let publicKeyCacheMaxEntries = DEFAULT_PUBLIC_KEY_CACHE_MAX_ENTRIES;

let publicKeyCacheHits = 0;

let publicKeyCacheMisses = 0;

let publicKeyCacheEvictions = 0;

export const phaseAPublicKeyCacheStats = (): {
  readonly size: number;
  readonly maxEntries: number;
  readonly hits: number;
  readonly misses: number;
  readonly evictions: number;
} => ({
  size: publicKeyCache.size,
  maxEntries: publicKeyCacheMaxEntries,
  hits: publicKeyCacheHits,
  misses: publicKeyCacheMisses,
  evictions: publicKeyCacheEvictions,
});

/** Test/process-shutdown hook; production validation uses the 4096-key cap. */
export const resetPhaseAPublicKeyCache = (
  maxEntries = DEFAULT_PUBLIC_KEY_CACHE_MAX_ENTRIES,
): void => {
  for (const publicKey of publicKeyCache.values()) {
    publicKey.free();
  }
  publicKeyCache.clear();
  publicKeyCacheMaxEntries = Math.max(1, Math.floor(maxEntries));
  publicKeyCacheHits = 0;
  publicKeyCacheMisses = 0;
  publicKeyCacheEvictions = 0;
};

const cachedPublicKey = (vkey: Buffer): CML.PublicKey => {
  const cacheKey = vkey.toString("hex");
  const cached = publicKeyCache.get(cacheKey);
  if (cached !== undefined) {
    publicKeyCache.delete(cacheKey);
    publicKeyCache.set(cacheKey, cached);
    publicKeyCacheHits += 1;
    return cached;
  }

  const publicKey = CML.PublicKey.from_bytes(vkey);
  publicKeyCacheMisses += 1;
  if (publicKeyCache.size >= publicKeyCacheMaxEntries) {
    const oldestKey = publicKeyCache.keys().next().value as string | undefined;
    if (oldestKey !== undefined) {
      const evicted = publicKeyCache.get(oldestKey);
      publicKeyCache.delete(oldestKey);
      evicted?.free();
      publicKeyCacheEvictions += 1;
    }
  }
  publicKeyCache.set(cacheKey, publicKey);
  return publicKey;
};

export const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.length === 0
    ? EMPTY_HASH_HEXES
    : hashes.map((hash) => hash.toString("hex"));

/**
 * EmptyInputs, else the DuplicateInput the descending merge reaches first
 * (`runInputSetMerge`), with the item scanned earlier as `first`.
 */
export const validateInputSets = (
  tx: Pick<MidgardLedgerTx, "txId" | "spendInputs" | "referenceInputs">,
): RejectedTx | null => {
  if (tx.spendInputs.length === 0) {
    return reject(tx.txId, RejectCodes.EmptyInputs, null, "inputSets");
  }
  const duplicate = runInputSetMerge([
    tx.spendInputs.map(midgardOutRefToCbor),
    tx.referenceInputs.map(midgardOutRefToCbor),
  ]);
  if (duplicate === null) {
    return null;
  }
  const { first, second } = duplicate;
  const keyHex = Buffer.from(duplicate.key).toString("hex");
  return reject(
    tx.txId,
    RejectCodes.DuplicateInputInTx,
    first.fieldIndex !== second.fieldIndex
      ? `outref appears in both spend and reference inputs ${keyHex}`
      : first.fieldIndex === 1
        ? `duplicate reference input ${keyHex}`
        : keyHex,
    "inputSets",
    {
      arm: "DuplicateInput",
      first: {
        fieldIndex: BigInt(first.fieldIndex),
        itemIndex: BigInt(first.itemIndex),
      },
      second: {
        fieldIndex: BigInt(second.fieldIndex),
        itemIndex: BigInt(second.itemIndex),
      },
    },
  );
};

export const validateValidityInterval = (
  tx: Pick<
    MidgardLedgerTx,
    "txId" | "validityIntervalStart" | "validityIntervalEnd"
  >,
): RejectedTx | null => {
  // A present bound is already a nonnegative slot: the native body codec
  // refuses any bound below the `-1` sentinel (`asValidityBound`,
  // `docs/spec/midgard-tx.md` §2.1) and the ledger view maps `-1` to
  // `undefined`. Mirrors `validity_interval_is_malformed` on-chain.
  if (
    tx.validityIntervalStart !== undefined &&
    tx.validityIntervalEnd !== undefined &&
    tx.validityIntervalStart >= tx.validityIntervalEnd
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `${tx.validityIntervalStart} >= ${tx.validityIntervalEnd}`,
      "inputSets",
    );
  }

  return null;
};

const verifyVKeyWitnessWithCml = (
  txBodyHash: Buffer,
  witness: MidgardLedgerVKeyWitness,
): boolean => {
  const publicKey = cachedPublicKey(witness.vkey);
  const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
  try {
    return publicKey.verify(txBodyHash, signature);
  } finally {
    signature.free();
  }
};

/**
 * Reports the first invalid witness in field-7 order, which the Signatures
 * cursor scans: the node refused field 7 at decode unless it is strictly
 * ascending by (signer hash, item bytes).
 */
export const verifyVKeyWitnessSignatures = (
  tx: Pick<MidgardLedgerTx, "txId" | "vkeyWitnesses">,
  verifySignature: NonNullable<
    PhaseALocalContext["verifyVKeyWitnessSignature"]
  > = verifyVKeyWitnessWithCml,
): RejectedTx | null => {
  for (const witness of tx.vkeyWitnesses) {
    if (!verifySignature(tx.txId, witness)) {
      return reject(
        tx.txId,
        RejectCodes.InvalidSignature,
        `invalid native vkey witness #${witness.index}`,
        "signatures",
        { arm: "AddressWitnessSignatureInvalid", index: BigInt(witness.index) },
      );
    }
  }
  return null;
};

export const validateRequiredSigners = (
  tx: Pick<
    MidgardLedgerTx,
    "txId" | "requiredSignerHashes" | "witnessKeyHashes"
  >,
): RejectedTx | null => {
  if (tx.requiredSignerHashes.length === 0) {
    return null;
  }
  const witnessSignerSet = new Set(hashHexes(tx.witnessKeyHashes));
  const requiredSigners = hashHexes(tx.requiredSignerHashes);
  for (let index = 0; index < requiredSigners.length; index += 1) {
    const requiredSigner = requiredSigners[index]!;
    if (!witnessSignerSet.has(requiredSigner)) {
      return reject(
        tx.txId,
        RejectCodes.MissingRequiredWitness,
        `missing witness for signer ${requiredSigner}`,
        "signatures",
        { arm: "RequiredSignerUnsigned", index: BigInt(index) },
      );
    }
  }
  return null;
};
