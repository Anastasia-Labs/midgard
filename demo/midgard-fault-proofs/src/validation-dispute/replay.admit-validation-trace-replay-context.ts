import { createHash } from "node:crypto";

import {
  collectMidgardAttachedProgramEnvelopes,
  computeMidgardForcedTxProofCommitment,
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardSpendInputItem,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_PROTOCOL_VERSION,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  classifyWithdrawalFromLedger,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  ForcedTxProofSource,
  GENESIS_HEADER_HASH,
  TxOrderDatum,
  ValidationTraceDescriptor,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  deriveCanonicalOriginalDepositTransitionEffect,
  DirectValidationTraceUnavailable,
  RejectCodes,
  replayValidationMachineEvent,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type TransitionTraceL1Events } from "../transition-trace/l1-events.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../transition-trace/replay-authority.js";
import {
  type CompleteCanonicalReplayPredecessor,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import {
  type ReplayPrerequisiteFailure,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import {
  authorities,
  committedDepositMatchesOrigin,
  committedWithdrawalMatchesOrigin,
  isInteractiveDisagreement,
  matchingOrigin,
  readmitEvidence,
  readOriginEvents,
  replayDetectionId,
  type ReplayMaterial,
  sameEvidenceIdentity,
  VALIDATION_TRACE_REPLAY_CONTEXT,
  type ValidationTraceReplayContext,
} from "./replay.read-origin-events.js";

/**
 * Freshly authenticates retained inputs and derives each verdict,
 * ledger mutation and trace through the canonical validation owner. No caller
 * supplies a verdict, descriptor, replay input, evaluator or detector callback.
 * Non-L2 events require their exact originating L1 authority. Under decision
 * 0007 a committed deposit or withdrawal whose origin is absent, or whose
 * authentic origin differs in content, records the prerequisite owed to
 * `fabricatedDeposit`/`fabricatedWithdrawal` rather than aborting; an absent
 * forced origin still aborts, because no family proves it.
 */
export const admitValidationTraceReplayContext = async ({
  evidence,
  predecessor,
  transitionTraceEvents,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly predecessor?: CompleteCanonicalReplayPredecessor;
  readonly transitionTraceEvents?: TransitionTraceL1Events;
}): Promise<ValidationTraceReplayContext> => {
  const predecessorEvidence = completeCanonicalReplayPredecessorEvidence({
    evidence,
    context: predecessor === undefined ? undefined : { predecessor },
  });
  // Snapshot and re-admit both envelopes before using any reconstructed value.
  const [current, prior] = await Promise.all([
    readmitEvidence(evidence),
    predecessorEvidence === undefined
      ? Promise.resolve(undefined)
      : readmitEvidence(predecessorEvidence),
  ]);
  if (!sameEvidenceIdentity(current, evidence))
    throw new Error("validation replay current evidence identity changed");
  if (
    current.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION) ||
    !Number.isSafeInteger(Number(current.header.endTime))
  )
    throw new Error("validation replay requires the compiled header context");
  if (prior === undefined) {
    if (
      current.header.prevHeaderHash !== GENESIS_HEADER_HASH ||
      current.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT
    )
      throw new Error("validation replay requires an admitted predecessor");
  } else if (
    predecessor === undefined ||
    !sameEvidenceIdentity(prior, predecessor) ||
    prior.headerHash !== current.header.prevHeaderHash ||
    prior.header.utxosRoot !== current.header.prevUtxosRoot
  ) {
    throw new Error("validation replay predecessor identity changed");
  }

  const reconstruction = current.reconstruction;
  const origins =
    transitionTraceEvents === undefined
      ? undefined
      : readOriginEvents(current, transitionTraceEvents);
  if (
    origins === undefined &&
    reconstruction.sourceEvents.some(({ phase }) => phase !== "L2Transaction")
  )
    throw new Error(
      "validation replay requires admitted originating L1 events",
    );
  const steps = [...reconstruction.transitionTrace].sort((left, right) =>
    left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
  );
  if (
    steps.length !== reconstruction.sourceEvents.length ||
    reconstruction.eventToStep.length !== steps.length ||
    reconstruction.rootData.validationTraces.entries.length !==
      reconstruction.transactions.length +
        reconstruction.forcedTransactions.length
  )
    throw new Error(
      "validation replay requires complete event and trace coverage",
    );
  const descriptors = new Map(
    reconstruction.rootData.validationTraces.entries.map(({ key, value }) => [
      key.toString("hex"),
      value.toString("hex"),
    ]),
  );
  const blockMaterial =
    reconstruction.payload.block_body.cek_program_material.map(
      ([root, value]) =>
        decodeMidgardCekProgramMaterialDaEntry(
          Buffer.from(root, "hex"),
          Buffer.from(value, "hex"),
        ),
    );
  const state = new Map(
    (prior?.reconstruction.utxos ?? []).map(({ key, value }) => [
      key.toString("hex"),
      Buffer.from(value),
    ]),
  );
  let priorRoot = current.header.prevUtxosRoot;
  const ledgerEntries = () =>
    [...state].map(([outRef, output]) => ({
      outRef: Buffer.from(outRef, "hex"),
      output: Buffer.from(output),
    }));
  if (
    (await validationMachineLedgerRoot(ledgerEntries())).toString("hex") !==
    priorRoot
  )
    throw new Error("validation replay prior ledger differs from its header");
  const seen = new Set<string>();
  const material: ReplayMaterial[] = [];
  const prerequisites: ReplayPrerequisiteFailure[] = [];
  for (const [stepIndex, step] of steps.entries()) {
    const fingerprint = eventKeyFingerprint(step.value.event_key);
    const source = reconstruction.sourceEventsByFingerprint.get(fingerprint);
    const eventToStep =
      reconstruction.eventToStepByFingerprint.get(fingerprint);
    if (
      step.key !== BigInt(stepIndex) ||
      step.value.step_index !== step.key ||
      source === undefined ||
      step.value.phase !== source.phase ||
      eventToStep?.value.step_index !== step.key ||
      eventToStep.value.phase !== source.phase ||
      seen.has(fingerprint)
    )
      throw new Error("validation replay event ordering is not exact");
    seen.add(fingerprint);
    const eventKeyCbor = Data.to(source.eventKey, EventKey);
    const origin =
      source.phase === "L2Transaction"
        ? undefined
        : matchingOrigin(source, origins!);
    if (source.phase !== "L2Transaction" && origin === undefined) {
      // Decision 0007: report the fabricated-family finding owed to this
      // event instead of throwing out of classification. The ledger is left
      // untouched, exactly as the other prerequisite routes leave it.
      prerequisites.push(
        ...replayPrerequisiteFailure(
          evidence.headerHash,
          source.eventKey,
          "present_source_origin",
        ).failures,
      );
      continue;
    }
    if (source.phase === "Deposit") {
      if (origin?.kind !== "deposit")
        throw new Error("Validation replay deposit origin kind differs");
      const original = origin.original;
      if (
        !committedDepositMatchesOrigin({
          originInfoCbor: origin.infoCbor,
          committedValueBytes: source.entry.valueBytes.toString("hex"),
        })
      ) {
        // Decision 0007: the authentic deposit's content differs from the
        // committed one, which is exactly `fabricatedDeposit`.
        prerequisites.push(
          ...replayPrerequisiteFailure(
            evidence.headerHash,
            source.eventKey,
            "matching_source_origin",
          ).failures,
        );
        continue;
      }
      const effect = deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: origins!.network,
        eventId: original.id,
        l2NetworkId: original.info.l2_network_id,
        l2Address: original.info.l2_address,
        l2DatumCbor:
          original.info.l2_datum === null
            ? null
            : Buffer.from(
                plutusConstrFieldCbor(origin.infoCbor, [2, 0]),
                "hex",
              ),
        originalAssets: origin.originalAssets,
      });
      if (
        effect.operations.some(
          (operation) =>
            operation.type === "insert" &&
            state.has(operation.outRefCbor.toString("hex")),
        )
      ) {
        // The committed deposit re-creates an output the ledger already
        // holds: a repeated source event. Its effect is owed to the finding
        // that names the repeat, so keep the ledger and scan the rest.
        prerequisites.push(
          ...replayPrerequisiteFailure(
            evidence.headerHash,
            source.eventKey,
            "prior_transition_effect",
          ).failures,
        );
        continue;
      }
      for (const operation of effect.operations) {
        const key = operation.outRefCbor.toString("hex");
        if (operation.type !== "insert")
          throw new Error(
            "validation replay deposit does not insert an absent ledger output",
          );
        state.set(key, Buffer.from(operation.outputCbor));
      }
      priorRoot = (await validationMachineLedgerRoot(ledgerEntries())).toString(
        "hex",
      );
      continue;
    }
    if (source.phase === "Withdrawal") {
      if (origin?.kind !== "withdrawal")
        throw new Error("Validation replay withdrawal origin kind differs");
      const original = origin.original;
      if (
        !committedWithdrawalMatchesOrigin({
          originInfoCbor: origin.infoCbor,
          committedValueBytes: source.entry.valueBytes.toString("hex"),
        })
      ) {
        // Decision 0007: the authentic order's body or signature differs from
        // the committed one, which is exactly `fabricatedWithdrawal`.
        prerequisites.push(
          ...replayPrerequisiteFailure(
            evidence.headerHash,
            source.eventKey,
            "matching_source_origin",
          ).failures,
        );
        continue;
      }
      const outRef = encodeMidgardSpendInputItem({
        txId: Buffer.from(original.info.body.l2_outref.transactionId, "hex"),
        outputIndex: Number(original.info.body.l2_outref.outputIndex),
      });
      const classification = await Effect.runPromise(
        classifyWithdrawalFromLedger({
          l2Owner: original.info.body.l2_owner,
          l2ValueCbor: plutusConstrFieldCbor(origin.infoCbor, [0, 2]),
          eventInfoCbor: origin.infoCbor,
          ledgerOutRef: outRef,
          ledgerOutput: state.get(outRef.toString("hex")) ?? null,
        }),
      );
      if (classification.shouldDeleteLedgerUtxo)
        state.delete(outRef.toString("hex"));
      priorRoot = (await validationMachineLedgerRoot(ledgerEntries())).toString(
        "hex",
      );
      continue;
    }
    if (source.phase === "ForcedTransaction") {
      const original = Data.from(origin!.event.datum!, TxOrderDatum).event;
      const submitted = deriveMidgardForcedTxProofSource(
        decodeMidgardForcedTxFullFromCanonicalCbor(
          source.entry.fullTransactionCbor,
        ),
      );
      const exactSource = {
        compact_cbor: submitted.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          submitted.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          submitted.fieldPreimageLengthsCbor.toString("hex"),
      };
      if (
        original.tx.tx_id !== source.entry.value.tx_id ||
        original.tx.transaction_commitment !==
          computeMidgardForcedTxProofCommitment(submitted).toString("hex") ||
        Data.to(original.tx.submitted_source, ForcedTxProofSource) !==
          Data.to(exactSource, ForcedTxProofSource)
      )
        throw new Error(
          "validation replay forced bytes differ from their originating commitment",
        );
    }
    const committedDescriptorCbor = descriptors.get(eventKeyCbor);
    if (committedDescriptorCbor === undefined)
      throw new Error("validation replay lacks an authenticated descriptor");
    const transactionIndex =
      source.phase === "L2Transaction"
        ? reconstruction.transactions.indexOf(source.entry)
        : reconstruction.forcedTransactions.indexOf(source.entry);
    if (transactionIndex < 0)
      throw new Error(
        "validation replay source is not a canonical transaction entry",
      );
    const transaction = source.entry.fullTransactionCbor;
    const envelopes = collectMidgardAttachedProgramEnvelopes(
      (source.phase === "ForcedTransaction"
        ? decodeMidgardForcedTxFullFromCanonicalCbor
        : decodeMidgardNativeTxFullFromCanonicalCbor)(transaction),
    );
    const reachable = new Set(
      verifyMidgardCekProgramMaterialBundle(envelopes, blockMaterial, {
        allowUnreachable: true,
      }).flatMap(({ reachableRoots }) => [...reachableRoots]),
    );
    const sidecar = encodeMidgardCekProgramMaterialSidecar(
      blockMaterial.filter(({ root }) =>
        reachable.has(Buffer.from(root).toString("hex")),
      ),
    );
    const replayResult = await Effect.runPromise(
      Effect.either(
        replayValidationMachineEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(eventKeyCbor, "hex"),
          canonicalTransactionCbor: transaction,
          programMaterialSidecarCbor: sidecar,
          ...(source.phase === "L2Transaction"
            ? { sourceKind: "normal" as const }
            : {
                sourceKind: "forced" as const,
              }),
          ledgerWitnessEntries: ledgerEntries(),
          priorUtxosRoot: priorRoot,
          blockEndTimeMs: Number(current.header.endTime),
          expectedNetworkId: current.header.expectedNetworkId,
          minFeeA: current.header.minFeeA,
          minFeeB: current.header.minFeeB,
          blockSlot: current.header.blockSlot,
        }),
      ),
    );
    if (replayResult._tag === "Left") {
      if (replayResult.left instanceof DirectValidationTraceUnavailable) {
        prerequisites.push(
          ...replayPrerequisiteFailure(
            evidence.headerHash,
            source.eventKey,
            replayResult.left.rejectionCode === RejectCodes.InvalidFieldType
              ? "representable_field_shape"
              : "representable_validity_flag",
          ).failures,
        );
        // The canonical validator rejected before any ledger mutation. Keep
        // its unchanged ledger and continue scanning later events for faults.
        continue;
      }
      throw replayResult.left;
    }
    const replay = replayResult.right;
    material.push({
      transactionIndex,
      stepIndex: step.key,
      eventKeyCbor,
      committedDescriptorCbor,
      committedPriorRoot: step.value.pre_utxos_root,
      challengerDescriptorCbor: Data.to(
        validationTraceDescriptorDataFromCore(replay.trace.tree.descriptor),
        ValidationTraceDescriptor,
      ),
      replay,
      sourceKind: source.phase === "L2Transaction" ? "normal" : "forced",
      committedRejectionReason:
        source.phase === "ForcedTransaction" &&
        source.entry.value.verdict !== "ForcedTxValid"
          ? source.entry.value.verdict.ForcedTxInvalid.reason
          : undefined,
      exactL1ReferenceOutRefs:
        origin === undefined
          ? []
          : [
              `${origin.event.txHash}#${origin.event.outputIndex.toString()}`,
              `${origins!.hub.txHash}#${origins!.hub.outputIndex.toString()}`,
            ].sort(),
    });
    applyUTxOStatePatch(state, replay.statePatch);
    priorRoot = replay.replayInput.postUtxosRoot;
  }
  const detections = Object.freeze(
    material.filter(isInteractiveDisagreement).map((entry) =>
      Object.freeze({
        detectionId: replayDetectionId(entry),
        headerHash: current.headerHash,
        violationId: "validation-trace",
        position: BigInt(entry.transactionIndex),
        diagnostic: `Canonical Plutus execution disagrees with the retained validation descriptor at step ${entry.stepIndex.toString()}`,
      }),
    ),
  );
  const eventEvidenceDigest =
    transitionTraceEvents === undefined
      ? undefined
      : computeTransitionTraceL1EventEvidenceDigest({
          evidence: current,
          l1Events: transitionTraceEvents,
        });
  const context: ValidationTraceReplayContext = Object.freeze({
    schemaVersion: VALIDATION_TRACE_REPLAY_CONTEXT,
    headerHash: current.headerHash,
    payloadEnvelopeSha256: current.payloadEnvelopeSha256,
    payloadSha256: current.payloadSha256,
    ...(transitionTraceEvents === undefined
      ? {}
      : {
          eventSnapshotDigest: transitionTraceEvents.snapshotDigest,
          eventEvidenceDigest,
        }),
    replayDigest: createHash("sha256")
      .update(
        JSON.stringify([
          VALIDATION_TRACE_REPLAY_CONTEXT,
          current.headerHash,
          current.payloadEnvelopeSha256,
          current.payloadSha256,
          predecessor ?? null,
          eventEvidenceDigest ?? null,
          prerequisites,
          material.map((entry) => [
            entry.transactionIndex,
            entry.sourceKind,
            entry.stepIndex.toString(),
            entry.eventKeyCbor,
            entry.committedDescriptorCbor,
            entry.committedPriorRoot,
            entry.challengerDescriptorCbor,
            entry.replay.replayInput.priorUtxosRoot,
            entry.replay.replayInput.postUtxosRoot,
            entry.exactL1ReferenceOutRefs,
          ]),
        ]),
      )
      .digest("hex"),
  });
  authorities.set(context, {
    predecessor,
    transitionTraceEvents,
    evidence: current,
    material,
    detections,
    prerequisites,
  });
  return context;
};
