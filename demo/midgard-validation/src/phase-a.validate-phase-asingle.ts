import { type LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
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
import type { MidgardSubmittedTx } from "./ledger-tx/types.js";
import {
  codecErrorDetail,
  consensusProfileRejectCode,
  reject,
  validateInputSets,
  validateRequiredSigners,
  validateValidityInterval,
  verifyVKeyWitnessSignatures,
} from "./phase-a.validate-input-sets.js";
import {
  validateDeclaredExUnits,
  validateRequiredObservers,
  validateScriptEvaluationPreconditions,
  validateWitnessScripts,
} from "./phase-a.validate-script-rules.js";
import {
  PhaseAConfig,
  PhaseAResult,
  PhaseAValidatedTx,
  QueuedTx,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import { buildPhaseAValidatedTx } from "./validation-candidate.js";
import { canonicalFieldOrderFault } from "./validation-machine/canonical-field-order.js";
import { oversizedCanonicalOutputIndex } from "./validation-machine/canonical-output-bound.js";

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
  // CanonicalDecode, the machine's first item walk, refuses fields 0/1/7 out
  // of order before any later phase reads them. The walk stops at an item
  // refused for its width, whose verdict follows.
  const orderFault = canonicalFieldOrderFault(queuedTx.txCbor, sourceKind);
  if (orderFault !== undefined)
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      `field[${orderFault.fieldIndex}].item[${orderFault.itemIndex}] is out of canonical order`,
      "canonicalDecode",
      {
        arm: "FieldItemWidthIllegal",
        fieldIndex: BigInt(orderFault.fieldIndex),
        itemIndex: BigInt(orderFault.itemIndex),
      },
    );
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
