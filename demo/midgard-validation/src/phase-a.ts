import {
  EMPTY_NULL_ROOT,
  MidgardTxCodecError,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
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
): RejectedTx => ({
  txId,
  code,
  detail,
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

const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.map((hash) => hash.toString("hex"));

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

const validateInputSets = (tx: MidgardLedgerTx): RejectedTx | null => {
  if (tx.spendInputs.length === 0) {
    return reject(tx.txId, RejectCodes.EmptyInputs);
  }

  const spendOutRefs = tx.spendInputs.map(midgardOutRefToCborHex);
  const duplicateSpend = firstDuplicate(spendOutRefs);
  if (duplicateSpend !== undefined) {
    return reject(tx.txId, RejectCodes.DuplicateInputInTx, duplicateSpend);
  }

  const spent = new Set(spendOutRefs);
  const referenceOutRefs = tx.referenceInputs.map(midgardOutRefToCborHex);
  const duplicateReference = firstDuplicate(referenceOutRefs);
  if (duplicateReference !== undefined) {
    return reject(
      tx.txId,
      RejectCodes.DuplicateInputInTx,
      `duplicate reference input ${duplicateReference}`,
    );
  }

  for (const referenceOutRef of referenceOutRefs) {
    if (spent.has(referenceOutRef)) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `outref appears in both spend and reference inputs ${referenceOutRef}`,
      );
    }
  }

  return null;
};

const validateValidityInterval = (tx: MidgardLedgerTx): RejectedTx | null => {
  if (
    (tx.validityIntervalStart !== undefined &&
      tx.validityIntervalStart < 0n) ||
    (tx.validityIntervalEnd !== undefined && tx.validityIntervalEnd < 0n)
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity bounds must be non-negative unless unbounded sentinel",
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
    );
  }

  return null;
};

const verifyVKeyWitnessSignatures = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  for (const witness of tx.vkeyWitnesses) {
    if (!verifyVKeyWitness(tx.txId, witness)) {
      return reject(
        tx.txId,
        RejectCodes.InvalidSignature,
        `invalid native vkey witness #${witness.index}`,
      );
    }
  }
  return null;
};

const verifyVKeyWitness = (
  txBodyHash: Buffer,
  witness: MidgardLedgerVKeyWitness,
): boolean => {
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
  return publicKey.verify(txBodyHash, signature);
};

const validateRequiredSigners = (tx: MidgardLedgerTx): RejectedTx | null => {
  const witnessSignerSet = new Set(hashHexes(tx.witnessKeyHashes));
  for (const requiredSigner of hashHexes(tx.requiredSignerHashes)) {
    if (!witnessSignerSet.has(requiredSigner)) {
      return reject(
        tx.txId,
        RejectCodes.MissingRequiredWitness,
        `missing witness for signer ${requiredSigner}`,
      );
    }
  }
  return null;
};

const validateNativeScriptWitnesses = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  const witnessSigners = new Set(hashHexes(tx.witnessKeyHashes));
  for (const witness of tx.scriptWitnesses) {
    if (witness.script.language !== "NativeCardano") {
      continue;
    }
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
      );
    }
  }
  return null;
};

const validateRequiredObservers = (tx: MidgardLedgerTx): RejectedTx | null => {
  const duplicateObserver = firstDuplicate(hashHexes(tx.requiredObserverHashes));
  if (duplicateObserver !== undefined) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      `duplicate required observer ${duplicateObserver}`,
    );
  }
  return null;
};

const validateScriptEvaluationPreconditions = (
  tx: MidgardLedgerTx,
): RejectedTx | null => {
  if (
    tx.requiresPlutusEvaluation &&
    Buffer.from(tx.scriptIntegrityHash).equals(EMPTY_NULL_ROOT)
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "missing script_integrity_hash for plutus witness bundle",
    );
  }

  if (
    tx.requiresPlutusEvaluation &&
    tx.requiredObserverHashes.length > 0 &&
    tx.networkId === undefined
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "network_id is required when plutus witness bundles use required observers",
    );
  }

  return null;
};

const validateNativeOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
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
    return reject(
      queuedTx.txId,
      code,
      codecErrorDetail(e),
    );
  }

  const { ledgerTx } = submittedTx;

  if (!ledgerTx.txId.equals(queuedTx.txId)) {
    return reject(
      queuedTx.txId,
      RejectCodes.TxHashMismatch,
      `queued tx_id ${queuedTx.txId.toString("hex")} != native ${ledgerTx.txId.toString("hex")}`,
    );
  }

  if (ledgerTx.validity !== "TxIsValid") {
    return reject(queuedTx.txId, RejectCodes.IsValidFalseForbidden);
  }

  if (!ledgerTx.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    return reject(
      ledgerTx.txId,
      RejectCodes.AuxDataForbidden,
      "auxiliary_data_hash must match canonical empty hash",
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
    );
  }

  const minFee =
    config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
  if (ledgerTx.fee < minFee) {
    return reject(ledgerTx.txId, RejectCodes.MinFee, `${ledgerTx.fee} < ${minFee}`);
  }

  for (const validation of [
    validateInputSets,
    validateValidityInterval,
    validateRequiredSigners,
    verifyVKeyWitnessSignatures,
    validateNativeScriptWitnesses,
    validateRequiredObservers,
    validateScriptEvaluationPreconditions,
  ]) {
    const rejection = validation(ledgerTx);
    if (rejection !== null) {
      return rejection;
    }
  }

  try {
    return buildPhaseAValidatedTx({
      ledgerTx,
      txCbor: submittedTx.txCbor,
      arrivalSeq: queuedTx.arrivalSeq,
      createdAt: queuedTx.createdAt,
      redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
    });
  } catch (e) {
    return reject(
      ledgerTx.txId,
      RejectCodes.InvalidOutput,
      `failed to materialize Phase B candidate: ${String(e)}`,
    );
  }
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): Effect.Effect<PhaseAResult> =>
  Effect.gen(function* () {
    const orderedResults = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) => Effect.sync(() => validateNativeOne(queuedTx, config)),
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
