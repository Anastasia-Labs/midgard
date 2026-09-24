import { createHash } from "node:crypto";

import {
  collectMidgardAttachedProgramEnvelopes,
  computeMidgardForcedTxProofCommitment,
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardSpendInputItem,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_PROTOCOL_VERSION,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  classifyWithdrawalFromLedger,
  type DepositEvent,
  DepositInfo,
  EMPTY_MERKLE_TREE_ROOT,
  EventHistoryCommitment,
  EventHistoryOpening,
  EventKey,
  ForcedTxProofSource,
  GENESIS_HEADER_HASH,
  opensEventHistoryCommitmentCbor,
  OutputReference,
  type RejectionReason,
  TxOrderDatum,
  ValidationTraceDescriptor,
  validationTraceDescriptorDataFromCore,
  valueToAssets,
  withdrawalContentBytesCbor,
  type WithdrawalEvent,
} from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  deriveCanonicalOriginalDepositTransitionEffect,
  DirectValidationTraceUnavailable,
  RejectCodes,
  replayValidationMachineEvent,
  type ValidationMachineEventReplay,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import { type Assets, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  readFreshTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "../transition-trace/l1-events.js";
import {
  eventKeyFingerprint,
  type SourceEventRecord,
} from "../transition-trace/reconstruct.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../transition-trace/replay-authority.js";
import { buildRetainedValidationClaimWitness } from "../transition-trace/witnesses.js";
import {
  admitValidationTraceChallenge,
  type ReplayChallengeCoordinate,
  type ValidationTraceChallenge,
} from "../workflow/challenge-authority.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayPredecessor,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import { TYPED_REASON_DISPOSITIONS } from "../workflow/reason-disposition.js";
import {
  completeReplayFindings,
  type ReplayPrerequisiteFailure,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";

export const VALIDATION_TRACE_REPLAY_CONTEXT =
  "midgard-validation-trace-replay-context-v1" as const;

/** Identity only. The canonical replay and its challenge material stay private. */
export type ValidationTraceReplayContext = Readonly<{
  schemaVersion: typeof VALIDATION_TRACE_REPLAY_CONTEXT;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  replayDigest: string;
  eventSnapshotDigest?: string;
  eventEvidenceDigest?: string;
}>;

type ReplayMaterial = Readonly<{
  transactionIndex: number;
  stepIndex: bigint;
  eventKeyCbor: string;
  committedDescriptorCbor: string;
  committedPriorRoot: string;
  challengerDescriptorCbor: string;
  replay: ValidationMachineEventReplay;
  sourceKind: "normal" | "forced";
  committedRejectionReason: RejectionReason | undefined;
  exactL1ReferenceOutRefs: readonly string[];
}>;

type ReplayAuthority = Readonly<{
  predecessor: CompleteCanonicalReplayPredecessor | undefined;
  transitionTraceEvents: TransitionTraceL1Events | undefined;
  evidence: CanonicalBlockEvidence;
  material: readonly ReplayMaterial[];
  detections: readonly CanonicalViolationDetection[];
  prerequisites: readonly ReplayPrerequisiteFailure[];
}>;

const authorities = new WeakMap<
  ValidationTraceReplayContext,
  ReplayAuthority
>();

const readmitEvidence = (evidence: CanonicalBlockEvidence) =>
  canonicalBlockEvidenceFromVerifiedPayload({
    observation: evidence.observation,
    payloadEnvelopeCbor: Buffer.from(
      evidence.reconstruction.payloadEnvelopeCbor,
    ),
    daProvenance: evidence.provenance.da,
  });

const sameEvidenceIdentity = (
  left: CanonicalBlockEvidence,
  right: Pick<
    CanonicalBlockEvidence,
    "headerHash" | "payloadEnvelopeSha256" | "payloadSha256"
  >,
) =>
  left.headerHash === right.headerHash &&
  left.payloadEnvelopeSha256 === right.payloadEnvelopeSha256 &&
  left.payloadSha256 === right.payloadSha256;

type OriginEvent = Readonly<{ event: UTxO; assetName: string }> &
  (
    | Readonly<{ kind: "forcedTransaction" }>
    | Readonly<{
        kind: "deposit";
        original: DepositEvent;
        infoCbor: string;
        originalAssets: Assets;
      }>
    | Readonly<{
        kind: "withdrawal";
        original: WithdrawalEvent;
        infoCbor: string;
      }>
  );

/** Reopen the frozen result of raw snapshot admission through its opaque handle.
 * Its deposit/withdrawal entries are authenticated Orders with captured openings;
 * roots, fillers and unrelated donations were excluded during list admission. */
const readOriginEvents = (
  evidence: CanonicalBlockEvidence,
  handle: TransitionTraceL1Events,
) => {
  const admitted = readFreshTransitionTraceL1Events(handle);
  if (
    handle.headerHash !== evidence.headerHash ||
    admitted.snapshot.headerHash !== evidence.headerHash ||
    createHash("sha256")
      .update(JSON.stringify(admitted.snapshot))
      .digest("hex") !== handle.snapshotDigest
  )
    throw new Error("validation replay originating event snapshot changed");
  const events: OriginEvent[] = admitted.events.map((entry): OriginEvent => {
    const base = { event: entry.utxo, assetName: entry.assetName };
    if (entry.kind === "forcedTransaction")
      return { ...base, kind: entry.kind };
    const commitment = Data.from(
      entry.history.commitmentCbor,
      EventHistoryCommitment,
    );
    const opening = Data.from(entry.history.openingCbor, EventHistoryOpening);
    if (
      !opensEventHistoryCommitmentCbor(
        commitment,
        plutusConstrFieldCbor(entry.history.openingCbor, [0]),
        plutusConstrFieldCbor(entry.history.openingCbor, [1]),
      )
    )
      throw new Error(
        "Validation replay history opening differs from its admitted commitment",
      );
    if (entry.kind === "deposit" && "DepositPayload" in opening.payload)
      return {
        ...base,
        kind: "deposit",
        original: opening.payload.DepositPayload.event,
        infoCbor: plutusConstrFieldCbor(entry.history.openingCbor, [0, 0, 1]),
        originalAssets: valueToAssets(opening.original_assets),
      };
    if (entry.kind === "withdrawal" && "WithdrawalPayload" in opening.payload)
      return {
        ...base,
        kind: "withdrawal",
        original: opening.payload.WithdrawalPayload.event,
        infoCbor: plutusConstrFieldCbor(entry.history.openingCbor, [0, 0, 1]),
      };
    throw new Error(
      "Validation replay history payload has the wrong event kind",
    );
  });
  return { events, hub: admitted.hub, network: admitted.network };
};

const matchingOrigin = (
  source: Exclude<SourceEventRecord, { phase: "L2Transaction" }>,
  origins: ReturnType<typeof readOriginEvents>,
): OriginEvent | undefined => {
  const kind =
    source.phase === "Deposit"
      ? "deposit"
      : source.phase === "Withdrawal"
        ? "withdrawal"
        : "forcedTransaction";
  const matches = origins.events.filter((origin) => {
    if (origin.kind !== kind) return false;
    const id =
      origin.kind === "forcedTransaction"
        ? Data.from(origin.event.datum!, TxOrderDatum).event.id
        : origin.original.id;
    return (
      Data.to(id, OutputReference) ===
      Data.to(source.entry.key, OutputReference)
    );
  });
  if (matches.length > 1)
    throw new Error(
      "validation replay captured ambiguous originating events for one identity",
    );
  if (matches.length === 0) {
    // Decision 0007: a committed deposit or withdrawal with no L1 origin is
    // the fabricated-family fraud, not a replay abort. The caller turns the
    // absence into a prerequisite that only that family's finding discharges.
    // A current absence alone is not proof of fraud: history capture and the
    // accused interval/finalized frontier determine whether that finding is
    // admissible. Forced transactions have no fabricated family, so their
    // absent origin stays an abort.
    if (source.phase === "ForcedTransaction")
      throw new Error(
        "validation replay requires one captured originating forced event; absent or consumed origins require retained history",
      );
    return undefined;
  }
  return matches[0]!;
};

/** Decision 0007: the fabricated-deposit comparison is the authentic deposit's
 * whole committed body; a deposit carries no operator-owned verdict. */
export const committedDepositMatchesOrigin = ({
  originInfoCbor,
  committedValueBytes,
}: {
  readonly originInfoCbor: string;
  readonly committedValueBytes: string;
}): boolean => {
  Data.from(originInfoCbor, DepositInfo);
  Data.from(committedValueBytes, DepositInfo);
  return (
    aikenSerialisedPlutusDataCborPreservingMapOrder(originInfoCbor) ===
    committedValueBytes
  );
};

/** The operator owns validity; fabricated content compares only the raw body
 * and signature. The original user-signed body must survive typed decoding. */
export const committedWithdrawalMatchesOrigin = ({
  originInfoCbor,
  committedValueBytes,
}: {
  readonly originInfoCbor: string;
  readonly committedValueBytes: string;
}): boolean =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(committedValueBytes) ===
    committedValueBytes &&
  withdrawalContentBytesCbor(originInfoCbor) ===
    withdrawalContentBytesCbor(committedValueBytes);

/**
 * The direct reason catalogue owns every non-Plutus route. Descriptor drift
 * alone cannot select an interactive dispute, even on a transaction that also
 * contains a Plutus script.
 */
const isInteractiveDisagreement = (material: ReplayMaterial): boolean => {
  if (material.committedDescriptorCbor === material.challengerDescriptorCbor)
    return false;
  // A prior discrepancy must not prevent complete canonical replay of later
  // events, but a later challenge can only begin at the prior root committed
  // by its own transition step.
  if (
    material.committedPriorRoot !== material.replay.replayInput.priorUtxosRoot
  )
    return false;
  const committed = Data.from(
    material.committedDescriptorCbor,
    ValidationTraceDescriptor,
  );
  const trace = material.replay.trace;
  if (TYPED_REASON_DISPOSITIONS.PlutusExecutionFailed.proving !== "interactive")
    throw new Error("Plutus execution no longer owns the interactive route");
  const reason = material.committedRejectionReason;
  if (reason !== undefined) {
    if (
      typeof reason === "string" ||
      !("PlutusExecutionFailed" in reason) ||
      committed.verdict !== "Rejected" ||
      committed.rejection_code_hash !==
        hashMidgardValidationRejectionCode(
          RejectCodes.PlutusScriptInvalid,
        ).toString("hex") ||
      trace.verdict !== "accepted"
    )
      return false;
    const executionIndex = reason.PlutusExecutionFailed.execution_index;
    return trace.witnesses.some(
      ({ auxiliary }) =>
        auxiliary?.kind === "cekCoreStep" &&
        auxiliary.step.pre.executionIndex === executionIndex,
    );
  }
  // The descriptor's E_PLUTUS_SCRIPT_INVALID hash also represents the direct
  // ReceivePurposePlutusV3Forbidden arm. Ordinary L2 source leaves carry no
  // typed rejection reason, so that hash never authorizes a wrongful-rejection
  // route. The canonical rejection boundary does distinguish actual CEK
  // failure: the trace builder stops at its failing core step. The direct
  // receive-language rule stops at a nativeExecutionDescriptor instead.
  return (
    committed.verdict === "Accepted" &&
    trace.verdict === "rejected" &&
    trace.rejectionCode === RejectCodes.PlutusScriptInvalid &&
    trace.witnesses.at(-2)?.auxiliary?.kind === "cekCoreStep"
  );
};

const replayDetectionId = (entry: ReplayMaterial): string =>
  `validation-trace:${entry.sourceKind}:${entry.transactionIndex.toString()}:${entry.replay.replayInput.transactionId.toString("hex")}`;

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

export const requireValidationTraceReplayContext = ({
  evidence,
  context,
  predecessor,
  transitionTraceEvents,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: ValidationTraceReplayContext;
  readonly predecessor?: CompleteCanonicalReplayPredecessor;
  readonly transitionTraceEvents?: TransitionTraceL1Events;
}): ValidationTraceReplayContext => {
  const authority = authorities.get(context);
  if (
    authority === undefined ||
    context.schemaVersion !== VALIDATION_TRACE_REPLAY_CONTEXT ||
    !sameEvidenceIdentity(evidence, context) ||
    authority.predecessor !== predecessor ||
    authority.transitionTraceEvents !== transitionTraceEvents ||
    context.eventSnapshotDigest !== transitionTraceEvents?.snapshotDigest ||
    context.eventEvidenceDigest !==
      (transitionTraceEvents === undefined
        ? undefined
        : computeTransitionTraceL1EventEvidenceDigest({
            evidence,
            l1Events: transitionTraceEvents,
          }))
  )
    throw new Error(
      "validation replay context was not admitted for this block and predecessor",
    );
  return context;
};

export const detectValidationTraceReplay = (
  input: Parameters<typeof requireValidationTraceReplayContext>[0],
): readonly CanonicalViolationDetection[] => {
  requireValidationTraceReplayContext(input);
  const authority = authorities.get(input.context)!;
  return completeReplayFindings(authority.detections, authority.prerequisites);
};

type ReplaySelectionInput = Parameters<
  typeof requireValidationTraceReplayContext
>[0] &
  Readonly<{ detectionId: string }>;

const selectedMaterial = (input: ReplaySelectionInput): ReplayMaterial => {
  requireValidationTraceReplayContext(input);
  const authority = authorities.get(input.context)!;
  if (
    !authority.detections.some(
      ({ detectionId }) => detectionId === input.detectionId,
    )
  )
    throw new Error(
      "validation replay selection is not an admitted interactive disagreement",
    );
  const material = authority.material.find(
    (entry) => replayDetectionId(entry) === input.detectionId,
  );
  if (material === undefined)
    throw new Error("validation replay selected material disappeared");
  return material;
};

/** Only identity metadata leaves the owner; verdict, replay input and trace do not. */
export const readValidationTraceReplaySelection = (
  input: ReplaySelectionInput,
) => {
  const material = selectedMaterial(input);
  return Object.freeze({
    detectionId: input.detectionId,
    headerHash: input.context.headerHash,
    payloadEnvelopeSha256: input.context.payloadEnvelopeSha256,
    payloadSha256: input.context.payloadSha256,
    eventKeyCbor: material.eventKeyCbor,
    coordinate: Object.freeze({
      domain: "transition_step" as const,
      index: material.stepIndex.toString(),
    }),
  });
};

/** Called by the adapter after its fresh transcript admission. This operation
 * binds the selected step itself and consumes only the owner's private material. */
export const admitValidationTraceChallengeFromReplayContext = async (
  input: ReplaySelectionInput &
    Readonly<{ coordinate: ReplayChallengeCoordinate }>,
): Promise<ValidationTraceChallenge> => {
  const coordinate: ReplayChallengeCoordinate = Object.freeze({
    ...input.coordinate,
    coordinate: Object.freeze({ ...input.coordinate.coordinate }),
  });
  const snapshot: ReplaySelectionInput = {
    evidence: input.evidence,
    context: input.context,
    predecessor: input.predecessor,
    transitionTraceEvents: input.transitionTraceEvents,
    detectionId: input.detectionId,
  };
  const selection = readValidationTraceReplaySelection(snapshot);
  if (
    coordinate.headerHash !== selection.headerHash ||
    coordinate.payloadEnvelopeSha256 !== selection.payloadEnvelopeSha256 ||
    coordinate.payloadSha256 !== selection.payloadSha256 ||
    coordinate.coordinate.domain !== selection.coordinate.domain ||
    coordinate.coordinate.index !== selection.coordinate.index
  )
    throw new Error(
      "validation replay challenge coordinate changed selected event",
    );
  const authority = authorities.get(snapshot.context)!;
  const material = selectedMaterial(snapshot);
  const { claim } = await buildRetainedValidationClaimWitness({
    reconstruction: authority.evidence.reconstruction,
    eventKey: Data.from(material.eventKeyCbor, EventKey),
  });
  return await admitValidationTraceChallenge({
    coordinate,
    evidence: authority.evidence,
    claim,
    challengerReplayInput: material.replay.replayInput,
    exactL1ReferenceOutRefs: material.exactL1ReferenceOutRefs,
  });
};
