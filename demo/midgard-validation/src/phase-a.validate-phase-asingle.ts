import { type LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import {
  decodeMidgardVersionedScript,
  EMPTY_NULL_ROOT,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { Effect } from "effect";

import { carriedPlutusWitnessScriptFault } from "./carried-plutus-script-decoding.js";
import { awaitLedgerEd25519SignatureReady } from "./ledger-ed25519-ready.js";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  locateMidgardForcedTxOutputFault,
  locateMidgardForcedTxWitnessScriptFault,
  MidgardLedgerTxDecodeError,
  type MidgardRawEnvelopePhaseAProjection,
  projectMidgardMalformedNativeWitnessEnvelopeV1,
} from "./ledger-tx/codec.js";
import type { MidgardLedgerTx, MidgardSubmittedTx } from "./ledger-tx/types.js";
import {
  codecErrorDetail,
  consensusProfileRejectCode,
  hashHexes,
  reject,
  validateInputSets,
  validateRequiredSigners,
  validateValidityInterval,
  verifyVKeyWitnessSignatures,
} from "./phase-a.validate-input-sets.js";
import {
  PhaseAConfig,
  PhaseAResult,
  PhaseAValidatedTx,
  QueuedTx,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import { buildPhaseAValidatedTx } from "./validation-candidate.js";
import { oversizedCanonicalOutputIndex } from "./validation-machine/canonical-output-bound.js";

/**
 * The witness-script phase, in witness item order: a native witness must parse
 * and hold, and a Plutus witness's stored program must decode.
 */
const validateWitnessScripts = (
  tx: MidgardLedgerTx | MidgardRawEnvelopePhaseAProjection["ledgerTx"],
): RejectedTx | null => {
  let witnessSigners: ReadonlySet<string> | undefined;
  for (const witness of tx.scriptWitnesses) {
    let script;
    try {
      script =
        "script" in witness
          ? witness.script
          : decodeMidgardVersionedScript(witness.versionedItemBytes);
    } catch (cause) {
      return reject(
        tx.txId,
        RejectCodes.InvalidFieldType,
        codecErrorDetail(cause),
        "phaseANativeScripts",
        { arm: "WitnessNativeScriptMalformed", index: BigInt(witness.index) },
      );
    }
    if (script.language !== "NativeCardano") {
      const plutusFault = carriedPlutusWitnessScriptFault(
        tx.txId,
        script,
        witness.index,
      );
      if (plutusFault !== null) return plutusFault;
      continue;
    }
    witnessSigners ??= new Set(hashHexes(tx.witnessKeyHashes));
    if (
      !verifyMidgardNativeScript(script.nativeScript, {
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
        { arm: "WitnessNativeScriptFalse", index: BigInt(witness.index) },
      );
    }
  }
  return null;
};

const validateRequiredObservers = (
  tx: Pick<MidgardLedgerTx, "txId" | "requiredObserverHashes">,
): RejectedTx | null => {
  if (tx.requiredObserverHashes.length < 2) {
    return null;
  }
  for (let index = 1; index < tx.requiredObserverHashes.length; index += 1) {
    if (
      Buffer.compare(
        tx.requiredObserverHashes[index - 1]!,
        tx.requiredObserverHashes[index]!,
      ) >= 0
    ) {
      return reject(
        tx.txId,
        RejectCodes.InvalidFieldType,
        `required observers must be strictly ordered and unique at index ${index}`,
        "phaseAScriptPreconditions",
        { arm: "ObserverOrderInvalid", index: BigInt(index) },
      );
    }
  }
  return null;
};

const validateScriptEvaluationPreconditions = (
  tx: Pick<
    MidgardLedgerTx,
    | "txId"
    | "requiresPlutusEvaluation"
    | "scriptIntegrityHash"
    | "requiredObserverHashes"
    | "networkId"
  >,
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
      { arm: "ScriptIntegrityHashMissing" },
    );
  }

  if (tx.requiredObserverHashes.length > 0 && tx.networkId === undefined) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "network_id is required when plutus witness bundles use required observers",
      "phaseAScriptPreconditions",
      { arm: "ObserversForbiddenOnUntaggedNetwork" },
    );
  }

  return null;
};

/**
 * A normal transaction's declared execution units, summed over its redeemers,
 * must fit the per-transaction execution capability of the consensus profile
 * (Cardano's maxTxExUnits) in both components. Forced transactions carry no
 * redeemers in this release.
 */
const validateDeclaredExUnits = (
  tx: Pick<MidgardLedgerTx, "txId" | "redeemers">,
  limits: MidgardConsensusProfile["limits"],
): RejectedTx | null => {
  let memory = 0n;
  let steps = 0n;
  for (const redeemer of tx.redeemers) {
    memory += redeemer.exUnits.memory;
    steps += redeemer.exUnits.steps;
  }
  const maxMemory = BigInt(limits.minSupportedTransactionExecutionMemoryUnits);
  const maxSteps = BigInt(limits.minSupportedTransactionExecutionCpuUnits);
  return memory > maxMemory || steps > maxSteps
    ? reject(
        tx.txId,
        RejectCodes.TxExUnits,
        `declared ex-units mem=${memory} steps=${steps} exceed the per-tx cap mem=${maxMemory} steps=${maxSteps}`,
        "phaseAScriptPreconditions",
      )
    : null;
};

export const validatePhaseASingle = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAValidatedTx | RejectedTx => {
  const forced = queuedTx.sourceKind === "forced";
  // The tx-order mint admits a forced transaction whose outputs or native
  // script bodies may not decode. Each such item is a machine verdict: the
  // rejection names the lowest faulty output first.
  if (forced) {
    const outputFault = locateMidgardForcedTxOutputFault(queuedTx.txCbor);
    if (outputFault !== null)
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `output[${outputFault.itemIndex}] is not a canonical ledger output`,
        "canonicalDecode",
        { arm: "OutputNonCanonical", index: BigInt(outputFault.itemIndex) },
      );
  }
  let submittedTx: MidgardSubmittedTx | null = null;
  let rawProjection: MidgardRawEnvelopePhaseAProjection | null = null;
  try {
    submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(
      queuedTx.txCbor,
      queuedTx.sourceKind,
    );
  } catch (e) {
    const code =
      e instanceof MidgardLedgerTxDecodeError && e.stage === "ledger"
        ? e.invalidOutput
          ? RejectCodes.InvalidOutput
          : RejectCodes.InvalidFieldType
        : RejectCodes.CborDeserialization;
    const outputIndex =
      e instanceof MidgardLedgerTxDecodeError
        ? e.invalidOutputIndex
        : undefined;
    // The envelope authenticates every earlier Phase A field while retaining
    // the malformed native payload for the machine's later native scan.
    const malformedNative =
      code === RejectCodes.InvalidFieldType
        ? projectMidgardMalformedNativeWitnessEnvelopeV1(
            queuedTx.txCbor,
            queuedTx.sourceKind,
          )
        : null;
    const scriptFault =
      forced && malformedNative?.malformedScriptIndex == null
        ? locateMidgardForcedTxWitnessScriptFault(queuedTx.txCbor)
        : null;
    if (malformedNative?.malformedScriptIndex != null) {
      rawProjection = malformedNative.projection;
    } else if (scriptFault !== null) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidFieldType,
        codecErrorDetail(e),
        "canonicalDecode",
        {
          arm:
            scriptFault.itemClass === "headerMalformed"
              ? "WitnessScriptHeaderMalformed"
              : "WitnessNativeScriptMalformed",
          index: BigInt(scriptFault.itemIndex),
        },
      );
    } else {
      return reject(
        queuedTx.txId,
        code,
        codecErrorDetail(e),
        "canonicalDecode",
        outputIndex !== undefined
          ? { arm: "OutputNonCanonical", index: BigInt(outputIndex) }
          : undefined,
      );
    }
  }

  const ledgerTx =
    submittedTx !== null ? submittedTx.ledgerTx : rawProjection!.ledgerTx;

  const oversizedOutput = oversizedCanonicalOutputIndex(
    queuedTx.txCbor,
    queuedTx.sourceKind ?? "normal",
  );
  if (oversizedOutput !== undefined)
    return reject(
      ledgerTx.txId,
      RejectCodes.InvalidFieldType,
      `output[${oversizedOutput}] exceeds the declared canonical output preimage bound`,
      "canonicalDecode",
      {
        arm: "FieldItemWidthIllegal",
        fieldIndex: 2n,
        itemIndex: BigInt(oversizedOutput),
      },
    );

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
  const consensusProfile = config.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE;
  if (!isMidgardConsensusProfile(consensusProfile)) {
    return reject(
      ledgerTx.txId,
      RejectCodes.TxVersion,
      "unsupported consensus profile",
    );
  }
  // Normal admission retains the proof-fit bounds; Flat decoding is separate.
  const consensusViolation =
    !forced && rawProjection === null
      ? validateMidgardConsensusTxCbor(queuedTx.txCbor)
      : null;
  if (consensusViolation !== null) {
    return reject(
      ledgerTx.txId,
      consensusProfileRejectCode(consensusViolation.code),
      `${consensusViolation.featureId}: ${consensusViolation.detail}`,
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
    config.minFeeA *
      BigInt(
        queuedTx.txCbor.length + (queuedTx.sourceKind === "forced" ? 1 : 0),
      ) +
    config.minFeeB;
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
  if (rejection === null) rejection = verifyVKeyWitnessSignatures(ledgerTx);
  if (rejection === null) rejection = validateWitnessScripts(ledgerTx);
  if (rejection === null) rejection = validateRequiredObservers(ledgerTx);
  if (rejection === null) {
    rejection = validateScriptEvaluationPreconditions(ledgerTx);
  }
  if (rejection === null && queuedTx.sourceKind !== "forced") {
    rejection = validateDeclaredExUnits(ledgerTx, consensusProfile.limits);
  }
  if (rejection !== null) {
    return rejection;
  }

  if (submittedTx === null) {
    throw new Error(
      "malformed native projection passed its native-script scan",
    );
  }

  // Every output decoded above, so building the candidate cannot fail on
  // transaction content; a throw here is a defect, not a verdict.
  return buildPhaseAValidatedTx({
    sourceKind: queuedTx.sourceKind ?? "normal",
    ledgerTx: submittedTx.ledgerTx,
    expectedNetworkId: config.expectedNetworkId,
    txCbor: submittedTx.txCbor,
    arrivalSeq: queuedTx.arrivalSeq,
    createdAt: queuedTx.createdAt,
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
  });
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): Effect.Effect<PhaseAResult, LedgerSignatureVerifierUnavailableError> =>
  Effect.gen(function* () {
    yield* awaitLedgerEd25519SignatureReady;
    const orderedResults = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) => Effect.sync(() => validatePhaseASingle(queuedTx, config)),
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
