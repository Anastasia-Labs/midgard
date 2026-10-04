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
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusForcedTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import { Effect } from "effect";

import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
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
  PhaseALocalContext,
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

export const validatePhaseASingle = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
  localContext: PhaseALocalContext = {},
): PhaseAValidatedTx | RejectedTx => {
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
    if (malformedNative?.malformedScriptIndex != null) {
      rawProjection = malformedNative.projection;
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
  if (queuedTx.programMaterialSidecarCbor == null) {
    return reject(
      ledgerTx.txId,
      RejectCodes.CekProgramMaterial,
      "V1 admission is missing its canonical program-material sidecar",
    );
  }
  // Forced orders retain only screens without a deployed total machine arm.
  // Normal-admission proof-fit bounds must not preempt a forced machine verdict.
  // The raw projection is reject-only: its native payload cannot decode and
  // must reach the ordered native scan after the earlier Phase A checks.
  // Normal structured admission/material decoders would throw on that payload
  // before the machine can name the first faulty witness. Forced admission
  // parses native envelopes only, so its operator-stop screens still run.
  // Every admissible candidate also passes the material screen below.
  const consensusViolation =
    queuedTx.sourceKind === "forced"
      ? validateMidgardConsensusForcedTxCbor(queuedTx.txCbor)
      : rawProjection === null
        ? validateMidgardConsensusTxCbor(queuedTx.txCbor)
        : null;
  if (consensusViolation !== null) {
    return reject(
      ledgerTx.txId,
      consensusProfileRejectCode(consensusViolation.code),
      `${consensusViolation.featureId}: ${consensusViolation.detail}`,
    );
  }
  if (rawProjection === null) {
    try {
      const material = decodeMidgardCekProgramMaterialSidecar(
        queuedTx.programMaterialSidecarCbor,
      );
      const canonicalTx = (
        queuedTx.sourceKind === "forced"
          ? decodeMidgardForcedTxFullFromCanonicalCbor
          : decodeMidgardNativeTxFullFromCanonicalCbor
      )(queuedTx.txCbor);
      const envelopes = collectMidgardAttachedProgramEnvelopes(
        canonicalTx,
        queuedTx.sourceKind,
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

  if (submittedTx === null) {
    throw new Error(
      "malformed native projection passed its native-script scan",
    );
  }

  try {
    return buildPhaseAValidatedTx({
      sourceKind: queuedTx.sourceKind ?? "normal",
      ledgerTx: submittedTx.ledgerTx,
      expectedNetworkId: config.expectedNetworkId,
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
