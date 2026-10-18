import { type LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterial,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
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
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import { Effect } from "effect";

import { awaitLedgerEd25519SignatureReady } from "./ledger-ed25519-ready.js";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  locateMidgardForcedTxOutputFault,
  locateMidgardForcedTxWitnessScriptFault,
  type MidgardForcedTxWitnessScriptFault,
  MidgardLedgerTxDecodeError,
  type MidgardRawEnvelopePhaseAProjection,
  projectMidgardMalformedNativeWitnessEnvelopeV1,
  projectMidgardRawEnvelopeForPhaseAV1,
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

const validateNativeScriptWitnesses = (
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

/** The first output past the canonical width bound, if the fields derive. */
const oversizedOutputIndex = (
  txCbor: Buffer,
  sourceKind: "normal" | "forced",
): number | undefined => {
  try {
    return oversizedCanonicalOutputIndex(txCbor, sourceKind);
  } catch {
    // The canonical decode below refuses bytes whose fields do not derive.
    return undefined;
  }
};

/**
 * A forced transaction's projection up to its first faulty script item, or
 * null when a field outside field 6 does not decode either.
 */
const projectScriptPrefix = (
  txCbor: Buffer,
  outputCount: number | undefined,
  scriptCount: number,
): MidgardRawEnvelopePhaseAProjection | null => {
  try {
    return projectMidgardRawEnvelopeForPhaseAV1(txCbor, "forced", {
      outputCount,
      scriptCount,
    });
  } catch {
    return null;
  }
};

/** A field-6 item's fault at its turn in the Phase A native-script scan. */
const scriptItemRejection = (
  txId: Buffer,
  fault: MidgardForcedTxWitnessScriptFault,
): RejectedTx =>
  reject(
    txId,
    RejectCodes.InvalidFieldType,
    `script witness ${fault.itemIndex} is not a canonical versioned script`,
    "phaseANativeScripts",
    {
      arm:
        fault.itemClass === "headerMalformed"
          ? "WitnessScriptHeaderMalformed"
          : "WitnessNativeScriptMalformed",
      index: BigInt(fault.itemIndex),
    },
  );

export const validatePhaseASingle = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAValidatedTx | RejectedTx => {
  const sourceKind = queuedTx.sourceKind ?? "normal";
  const forced = sourceKind === "forced";
  // The canonical scan bounds every item's width before any later phase.
  const oversizedOutput = oversizedOutputIndex(queuedTx.txCbor, sourceKind);
  if (oversizedOutput !== undefined)
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      `output[${oversizedOutput}] exceeds the declared canonical output preimage bound`,
      "canonicalDecode",
      {
        arm: "FieldItemWidthIllegal",
        fieldIndex: 2n,
        itemIndex: BigInt(oversizedOutput),
      },
    );
  // The tx-order mint admits a forced transaction whose outputs or script
  // items may not decode. The machine reads an output at the ScriptSources
  // output scan and a script item at its turn in the Phase A native-script
  // scan, so each such fault waits for its phase: the decode stops before the
  // first faulty output, which Phase B refuses after input resolution, and
  // the projection before the first faulty script item, refused below after
  // the signatures and the native scripts before it.
  const heldOutputFault = forced
    ? (locateMidgardForcedTxOutputFault(queuedTx.txCbor) ?? undefined)
    : undefined;
  const heldOutputFaultIndex = heldOutputFault?.itemIndex;
  let submittedTx: MidgardSubmittedTx | null = null;
  let rawProjection: MidgardRawEnvelopePhaseAProjection | null = null;
  let heldScriptFault: MidgardForcedTxWitnessScriptFault | null = null;
  try {
    submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(
      queuedTx.txCbor,
      sourceKind,
      heldOutputFaultIndex,
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
            sourceKind,
            heldOutputFaultIndex,
          )
        : null;
    const scriptFault =
      forced && malformedNative?.malformedScriptIndex == null
        ? locateMidgardForcedTxWitnessScriptFault(queuedTx.txCbor)
        : null;
    if (malformedNative?.malformedScriptIndex != null) {
      rawProjection = malformedNative.projection;
    } else if (scriptFault !== null) {
      rawProjection = projectScriptPrefix(
        queuedTx.txCbor,
        heldOutputFaultIndex,
        scriptFault.itemIndex,
      );
      heldScriptFault = scriptFault;
    }
    if (rawProjection === null) {
      return reject(
        queuedTx.txId,
        scriptFault !== null ? RejectCodes.InvalidFieldType : code,
        codecErrorDetail(e),
        "canonicalDecode",
        scriptFault !== null
          ? scriptItemRejection(queuedTx.txId, scriptFault).subject
          : outputIndex !== undefined
            ? { arm: "OutputNonCanonical", index: BigInt(outputIndex) }
            : undefined,
      );
    }
  }

  const ledgerTx =
    submittedTx !== null ? submittedTx.ledgerTx : rawProjection!.ledgerTx;

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
  // A forced transaction passed the tx-order mint's admission rules and runs
  // no program, so it has no normal-admission screen and no program material.
  if (!forced && queuedTx.programMaterialSidecarCbor == null) {
    return reject(
      ledgerTx.txId,
      RejectCodes.CekProgramMaterial,
      "V1 admission is missing its canonical program-material sidecar",
    );
  }
  // Normal-admission proof-fit bounds. The raw projection is reject-only: its
  // native payload cannot decode and must reach the ordered native scan after
  // the earlier Phase A checks. Normal structured admission/material decoders
  // would throw on that payload before the machine can name the first faulty
  // witness. Every admissible candidate also passes the material screen below.
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
  if (!forced && rawProjection === null) {
    try {
      const material = decodeMidgardCekProgramMaterialSidecar(
        queuedTx.programMaterialSidecarCbor!,
      );
      const envelopes = collectMidgardAttachedProgramEnvelopes(
        decodeMidgardNativeTxFullFromCanonicalCbor(queuedTx.txCbor),
      );
      if (ledgerTx.referenceInputs.length > 0) {
        // Phase A has not resolved reference-input outputs yet. Require complete
        // attached programs now; Phase B checks the exact combined bundle once
        // the referenced program envelopes are authoritative.
        for (const envelope of envelopes) {
          verifyMidgardCekProgramMaterial(envelope, material, {
            allowUnreachable: true,
          });
        }
      } else {
        verifyMidgardCekProgramMaterialBundle(envelopes, material);
      }
    } catch (cause) {
      return reject(
        ledgerTx.txId,
        RejectCodes.CekProgramMaterial,
        `invalid V1 program material: ${String(cause)}`,
      );
    }
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
  if (rejection === null) rejection = validateNativeScriptWitnesses(ledgerTx);
  if (rejection === null && heldScriptFault !== null)
    rejection = scriptItemRejection(ledgerTx.txId, heldScriptFault);
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

  // Every output the candidate keeps decoded above, so building it cannot
  // fail on transaction content; a throw here is a defect, not a verdict.
  return buildPhaseAValidatedTx({
    sourceKind: queuedTx.sourceKind ?? "normal",
    ledgerTx: submittedTx.ledgerTx,
    expectedNetworkId: config.expectedNetworkId,
    txCbor: submittedTx.txCbor,
    programMaterialSidecarCbor: queuedTx.programMaterialSidecarCbor ?? null,
    arrivalSeq: queuedTx.arrivalSeq,
    createdAt: queuedTx.createdAt,
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
    heldOutputFault,
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
