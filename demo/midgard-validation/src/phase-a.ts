import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialSidecarV1,
  verifyMidgardCekProgramMaterialBundleV1,
  verifyMidgardCekProgramMaterialV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  EMPTY_NULL_ROOT,
  MidgardTxCodecError,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  type MidgardConsensusV1ViolationCode,
  validateMidgardConsensusV1TxCbor,
} from "@al-ft/midgard-core/consensus-validation-v1";
import { collectMidgardV1AttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  MidgardLedgerTxDecodeError,
} from "./ledger-tx/codec.js";
import type {
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
  MidgardSubmittedTx,
} from "./ledger-tx/types.js";
import {
  PhaseAConfig,
  PhaseALocalContext,
  PhaseAResult,
  PhaseAValidatedTx,
  QueuedTx,
  RejectCode,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import {
  buildPhaseAValidatedTx,
  midgardOutRefToCborHex,
} from "./validation-candidate.js";

const reject = (
  txId: Buffer,
  code: RejectCode,
  detail: string | null = null,
  consensusPhase: MidgardValidationPhaseName = "canonicalDecode",
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
});

const codecErrorDetail = (error: unknown): string => {
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

const consensusProfileRejectCode = (
  code: MidgardConsensusV1ViolationCode,
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

const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.length === 0
    ? EMPTY_HASH_HEXES
    : hashes.map((hash) => hash.toString("hex"));

const firstDuplicate = (values: readonly string[]): string | undefined => {
  const seen = new Set<string>();
  for (const value of values) {
    if (seen.has(value)) {
      return value;
    }
    seen.add(value);
  }
  return undefined;
};

const outRefIdentity = (
  outRef: MidgardLedgerTx["spendInputs"][number],
): string => `${outRef.txId.toString("hex")}#${outRef.index.toString()}`;

const validateInputSets = (tx: MidgardLedgerTx): RejectedTx | null => {
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
      const duplicate = tx.spendInputs.find(
        (outRef) => outRefIdentity(outRef) === duplicateSpend,
      )!;
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        midgardOutRefToCborHex(duplicate),
        "inputSets",
      );
    }
  }

  if (tx.referenceInputs.length === 0) {
    return null;
  }

  const spent = new Set(spendOutRefIdentities);
  const referenceOutRefs = tx.referenceInputs.map(outRefIdentity);
  if (referenceOutRefs.length > 1) {
    const duplicateReference = firstDuplicate(referenceOutRefs);
    if (duplicateReference !== undefined) {
      const duplicate = tx.referenceInputs.find(
        (outRef) => outRefIdentity(outRef) === duplicateReference,
      )!;
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `duplicate reference input ${midgardOutRefToCborHex(duplicate)}`,
        "inputSets",
      );
    }
  }

  for (let index = 0; index < referenceOutRefs.length; index += 1) {
    if (spent.has(referenceOutRefs[index]!)) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `outref appears in both spend and reference inputs ${midgardOutRefToCborHex(
          tx.referenceInputs[index]!,
        )}`,
        "inputSets",
      );
    }
  }

  return null;
};

const validateValidityInterval = (tx: MidgardLedgerTx): RejectedTx | null => {
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

const verifyVKeyWitnessSignatures = (
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
      );
    }
  }
  return null;
};

const validateRequiredSigners = (tx: MidgardLedgerTx): RejectedTx | null => {
  if (tx.requiredSignerHashes.length === 0) {
    return null;
  }
  const witnessSignerSet = new Set(hashHexes(tx.witnessKeyHashes));
  for (const requiredSigner of hashHexes(tx.requiredSignerHashes)) {
    if (!witnessSignerSet.has(requiredSigner)) {
      return reject(
        tx.txId,
        RejectCodes.MissingRequiredWitness,
        `missing witness for signer ${requiredSigner}`,
        "signatures",
      );
    }
  }
  return null;
};

const validateNativeScriptWitnesses = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  let witnessSigners: ReadonlySet<string> | undefined;
  for (const witness of tx.scriptWitnesses) {
    if (witness.script.language !== "NativeCardano") {
      continue;
    }
    witnessSigners ??= new Set(hashHexes(tx.witnessKeyHashes));
    if (
      !verifyMidgardNativeScript(witness.script.nativeScript, {
        validityIntervalStart: tx.validityIntervalStart,
        validityIntervalEnd: tx.validityIntervalEnd,
        witnessSigners,
      })
    ) {
      return reject(
        tx.txId,
        RejectCodes.NativeScriptInvalid,
        `native script verification failed for script index ${witness.index}`,
        "phaseANativeScripts",
      );
    }
  }
  return null;
};

const validateRequiredObservers = (tx: MidgardLedgerTx): RejectedTx | null => {
  if (tx.requiredObserverHashes.length < 2) {
    return null;
  }
  const duplicateObserver = firstDuplicate(
    hashHexes(tx.requiredObserverHashes),
  );
  if (duplicateObserver !== undefined) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      `duplicate required observer ${duplicateObserver}`,
      "phaseAScriptPreconditions",
    );
  }
  return null;
};

const validateScriptEvaluationPreconditions = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  if (!tx.requiresPlutusEvaluation) {
    return null;
  }
  if (Buffer.from(tx.scriptIntegrityHash).equals(EMPTY_NULL_ROOT)) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "missing script_integrity_hash for plutus witness bundle",
      "phaseAScriptPreconditions",
    );
  }

  if (tx.requiredObserverHashes.length > 0 && tx.networkId === undefined) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "network_id is required when plutus witness bundles use required observers",
      "phaseAScriptPreconditions",
    );
  }

  return null;
};

export const validatePhaseASingle = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
  localContext: PhaseALocalContext = {},
): PhaseAValidatedTx | RejectedTx => {
  let submittedTx: MidgardSubmittedTx;
  try {
    submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(queuedTx.txCbor);
  } catch (e) {
    const code =
      e instanceof MidgardLedgerTxDecodeError && e.stage === "ledger"
        ? e.invalidOutput
          ? RejectCodes.InvalidOutput
          : RejectCodes.InvalidFieldType
        : RejectCodes.CborDeserialization;
    return reject(queuedTx.txId, code, codecErrorDetail(e));
  }

  const { ledgerTx } = submittedTx;

  if (!ledgerTx.txId.equals(queuedTx.txId)) {
    return reject(
      queuedTx.txId,
      RejectCodes.TxHashMismatch,
      `queued tx_id ${queuedTx.txId.toString("hex")} != native ${ledgerTx.txId.toString("hex")}`,
      "compactBinding",
    );
  }

  // Consensus admission is intentionally independent of the configurable
  // validation strictness profile. Only the exact compiled V1 tuple may reach
  // Phase B, even if an operator relaxes local configuration.
  const consensusProfile =
    config.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE_V1;
  if (!isMidgardConsensusProfileV1(consensusProfile)) {
    return reject(
      ledgerTx.txId,
      RejectCodes.TxVersion,
      "unsupported consensus profile",
    );
  }
  if (queuedTx.programMaterialSidecarCbor == null) {
    return reject(
      ledgerTx.txId,
      RejectCodes.CekProgramMaterial,
      "V1 admission is missing its canonical program-material sidecar",
    );
  }
  const consensusViolation = validateMidgardConsensusV1TxCbor(queuedTx.txCbor);
  if (consensusViolation !== null) {
    return reject(
      ledgerTx.txId,
      consensusProfileRejectCode(consensusViolation.code),
      `${consensusViolation.featureId}: ${consensusViolation.detail}`,
    );
  }
  try {
    const material = decodeMidgardCekProgramMaterialSidecarV1(
      queuedTx.programMaterialSidecarCbor,
    );
    const canonicalTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      queuedTx.txCbor,
    );
    const envelopes = collectMidgardV1AttachedProgramEnvelopes(canonicalTx);
    if (ledgerTx.referenceInputs.length > 0) {
      // Phase A has not resolved reference-input outputs yet. Require complete
      // attached programs now; Phase B checks the exact combined bundle once
      // the referenced program envelopes are authoritative.
      for (const envelope of envelopes) {
        verifyMidgardCekProgramMaterialV1(envelope, material, {
          allowUnreachable: true,
        });
      }
    } else {
      verifyMidgardCekProgramMaterialBundleV1(envelopes, material);
    }
  } catch (cause) {
    return reject(
      ledgerTx.txId,
      RejectCodes.CekProgramMaterial,
      `invalid V1 program material: ${String(cause)}`,
    );
  }

  if (
    ledgerTx.networkId !== undefined &&
    ledgerTx.networkId !== config.expectedNetworkId
  ) {
    return reject(
      ledgerTx.txId,
      RejectCodes.NetworkIdMismatch,
      `${ledgerTx.networkId} != ${config.expectedNetworkId}`,
      "staticLedgerRules",
    );
  }

  const minFee =
    config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
  if (ledgerTx.fee < minFee) {
    return reject(
      ledgerTx.txId,
      RejectCodes.MinFee,
      `${ledgerTx.fee} < ${minFee}`,
      "staticLedgerRules",
    );
  }

  let rejection = validateInputSets(ledgerTx);
  if (rejection === null) rejection = validateValidityInterval(ledgerTx);
  if (rejection === null) rejection = validateRequiredSigners(ledgerTx);
  if (rejection === null) {
    rejection = verifyVKeyWitnessSignatures(
      ledgerTx,
      localContext.verifyVKeyWitnessSignature,
    );
  }
  if (rejection === null) rejection = validateNativeScriptWitnesses(ledgerTx);
  if (rejection === null) rejection = validateRequiredObservers(ledgerTx);
  if (rejection === null) {
    rejection = validateScriptEvaluationPreconditions(ledgerTx);
  }
  if (rejection !== null) {
    return rejection;
  }

  try {
    return buildPhaseAValidatedTx({
      ledgerTx,
      txCbor: submittedTx.txCbor,
      programMaterialSidecarCbor: queuedTx.programMaterialSidecarCbor ?? null,
      arrivalSeq: queuedTx.arrivalSeq,
      createdAt: queuedTx.createdAt,
      redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
    });
  } catch (e) {
    return reject(
      ledgerTx.txId,
      RejectCodes.InvalidOutput,
      `failed to materialize Phase B candidate: ${String(e)}`,
      "phaseAScriptPreconditions",
    );
  }
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
  localContext: PhaseALocalContext = {},
): Effect.Effect<PhaseAResult> =>
  Effect.gen(function* () {
    const orderedResults = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) =>
        Effect.sync(() => validatePhaseASingle(queuedTx, config, localContext)),
      {
        concurrency: config.concurrency <= 0 ? "unbounded" : config.concurrency,
      },
    );

    const accepted: PhaseAValidatedTx[] = [];
    const rejected: RejectedTx[] = [];
    for (const item of orderedResults) {
      if ("ledgerTx" in item) {
        accepted.push(item);
      } else {
        rejected.push(item);
      }
    }

    return { accepted, rejected };
  });
