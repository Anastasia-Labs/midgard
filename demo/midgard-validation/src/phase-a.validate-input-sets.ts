import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import { MidgardTxCodecError } from "@al-ft/midgard-core/codec";
import { type MidgardConsensusViolationCode } from "@al-ft/midgard-core/consensus-validation";
import { CML } from "@lucid-evolution/lucid";

import { MidgardLedgerTxDecodeError } from "./ledger-tx/codec.js";
import type {
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
} from "./ledger-tx/types.js";
import type { RejectSubject } from "./reject-subject.js";
import {
  PhaseALocalContext,
  RejectCode,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import { midgardOutRefToCborHex } from "./validation-candidate.js";

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
    case "E_NATIVE_SCRIPT_DEPTH":
      return RejectCodes.NativeScriptDepth;
    case "E_NATIVE_SCRIPT_NODE_COUNT":
      return RejectCodes.NativeScriptNodeCount;
    case "E_ASSET_COUNT":
      return RejectCodes.AssetCount;
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

/** Ordinals of the earliest-completed duplicate pair, first before second. */
const firstDuplicate = (
  values: readonly string[],
): { readonly first: number; readonly second: number } | undefined => {
  const seen = new Map<string, number>();
  for (let index = 0; index < values.length; index += 1) {
    const first = seen.get(values[index]!);
    if (first !== undefined) {
      return { first, second: index };
    }
    seen.set(values[index]!, index);
  }
  return undefined;
};

const duplicateInputSubject = (
  firstField: bigint,
  first: number,
  secondField: bigint,
  second: number,
): RejectSubject => ({
  arm: "DuplicateInput",
  first: { fieldIndex: firstField, itemIndex: BigInt(first) },
  second: { fieldIndex: secondField, itemIndex: BigInt(second) },
});

const outRefIdentity = (
  outRef: MidgardLedgerTx["spendInputs"][number],
): string => `${outRef.txId.toString("hex")}#${outRef.index.toString()}`;

export const validateInputSets = (tx: MidgardLedgerTx): RejectedTx | null => {
  if (tx.spendInputs.length === 0) {
    return reject(tx.txId, RejectCodes.EmptyInputs, null, "inputSets");
  }

  if (tx.spendInputs.length === 1 && tx.referenceInputs.length === 0) {
    return null;
  }

  const spendOutRefIdentities = tx.spendInputs.map(outRefIdentity);
  if (spendOutRefIdentities.length > 1) {
    const duplicateSpend = firstDuplicate(spendOutRefIdentities);
    if (duplicateSpend !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        midgardOutRefToCborHex(tx.spendInputs[duplicateSpend.first]!),
        "inputSets",
        duplicateInputSubject(
          0n,
          duplicateSpend.first,
          0n,
          duplicateSpend.second,
        ),
      );
    }
  }

  if (tx.referenceInputs.length === 0) {
    return null;
  }

  const spent = new Map(
    spendOutRefIdentities.map((identity, index) => [identity, index]),
  );
  const referenceOutRefs = tx.referenceInputs.map(outRefIdentity);
  if (referenceOutRefs.length > 1) {
    const duplicateReference = firstDuplicate(referenceOutRefs);
    if (duplicateReference !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `duplicate reference input ${midgardOutRefToCborHex(
          tx.referenceInputs[duplicateReference.first]!,
        )}`,
        "inputSets",
        duplicateInputSubject(
          1n,
          duplicateReference.first,
          1n,
          duplicateReference.second,
        ),
      );
    }
  }

  for (let index = 0; index < referenceOutRefs.length; index += 1) {
    const spendIndex = spent.get(referenceOutRefs[index]!);
    if (spendIndex !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `outref appears in both spend and reference inputs ${midgardOutRefToCborHex(
          tx.referenceInputs[index]!,
        )}`,
        "inputSets",
        duplicateInputSubject(0n, spendIndex, 1n, index),
      );
    }
  }

  return null;
};

export const validateValidityInterval = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  if (
    (tx.validityIntervalStart !== undefined && tx.validityIntervalStart < 0n) ||
    (tx.validityIntervalEnd !== undefined && tx.validityIntervalEnd < 0n)
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity bounds must be non-negative unless unbounded sentinel",
      "inputSets",
    );
  }

  if (
    tx.validityIntervalStart !== undefined &&
    tx.validityIntervalEnd !== undefined &&
    tx.validityIntervalStart > tx.validityIntervalEnd
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `${tx.validityIntervalStart} > ${tx.validityIntervalEnd}`,
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

export const verifyVKeyWitnessSignatures = (
  tx: MidgardLedgerTx,
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
  tx: MidgardLedgerTx,
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
