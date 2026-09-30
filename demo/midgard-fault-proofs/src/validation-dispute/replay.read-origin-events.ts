import { createHash } from "node:crypto";

import { hashMidgardValidationRejectionCode } from "@al-ft/midgard-core";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type DepositEvent,
  DepositInfo,
  EventHistoryCommitment,
  EventHistoryOpening,
  opensEventHistoryCommitmentCbor,
  OutputReference,
  type RejectionReason,
  TxOrderDatum,
  ValidationTraceDescriptor,
  valueToAssets,
  withdrawalContentBytesCbor,
  type WithdrawalEvent,
} from "@al-ft/midgard-sdk";
import {
  RejectCodes,
  type ValidationMachineEventReplay,
} from "@al-ft/midgard-validation";
import { type Assets, Data, type UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  readFreshTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "../transition-trace/l1-events.js";
import { type SourceEventRecord } from "../transition-trace/reconstruct.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { type CompleteCanonicalReplayPredecessor } from "../workflow/complete-replay.js";
import { TYPED_REASON_DISPOSITIONS } from "../workflow/reason-disposition.js";
import { type ReplayPrerequisiteFailure } from "../workflow/replay-prerequisite.js";

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

export type ReplayMaterial = Readonly<{
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

export const authorities = new WeakMap<
  ValidationTraceReplayContext,
  ReplayAuthority
>();

export const readmitEvidence = (evidence: CanonicalBlockEvidence) =>
  canonicalBlockEvidenceFromVerifiedPayload({
    observation: evidence.observation,
    payloadEnvelopeCbor: Buffer.from(
      evidence.reconstruction.payloadEnvelopeCbor,
    ),
    daProvenance: evidence.provenance.da,
  });

export const sameEvidenceIdentity = (
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
export const readOriginEvents = (
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

export const matchingOrigin = (
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
export const isInteractiveDisagreement = (
  material: ReplayMaterial,
): boolean => {
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

export const replayDetectionId = (entry: ReplayMaterial): string =>
  `validation-trace:${entry.sourceKind}:${entry.transactionIndex.toString()}:${entry.replay.replayInput.transactionId.toString("hex")}`;
