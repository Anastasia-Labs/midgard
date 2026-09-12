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
  classifyWithdrawalFromLedger,
  DepositDatum,
  DepositInfo,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  ForcedTxProofSource,
  GENESIS_HEADER_HASH,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  OutputReference,
  type RejectionReason,
  TxOrderDatum,
  ValidationTraceDescriptor,
  validationTraceDescriptorDataFromCore,
  Value,
  WithdrawalInfo,
  WithdrawalOrderDatum,
} from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  deriveCanonicalDepositTransitionEffect,
  DirectValidationTraceUnavailable,
  RejectCodes,
  replayValidationMachineEvent,
  type ValidationMachineEventReplay,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  requireTransitionTraceL1Events,
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

type OriginEvent = Readonly<{
  kind: "deposit" | "withdrawal" | "forcedTransaction";
  event: UTxO;
  assetName: string;
}>;

/** Reopens raw snapshot bytes rather than exposing or trusting mutable projections. */
const readOriginEvents = (
  evidence: CanonicalBlockEvidence,
  handle: TransitionTraceL1Events,
) => {
  const admitted = requireTransitionTraceL1Events(handle);
  const serialized = JSON.stringify(admitted.snapshot);
  if (
    handle.headerHash !== evidence.headerHash ||
    admitted.snapshot.headerHash !== evidence.headerHash ||
    createHash("sha256").update(serialized).digest("hex") !==
      handle.snapshotDigest
  )
    throw new Error("validation replay originating event snapshot changed");
  const snapshot: typeof admitted.snapshot = JSON.parse(serialized);
  const hubScope = snapshot.scopes.find(({ role }) => role === "hub_oracle");
  if (hubScope === undefined)
    throw new Error("validation replay origin snapshot omits its hub");
  const details = getAddressDetails(hubScope.address);
  if (details.paymentCredential?.type !== "Script")
    throw new Error("validation replay origin hub is not script-bound");
  const hubUnit = details.paymentCredential.hash + HUB_ORACLE_ASSET_NAME;
  const decodeOutput = (raw: (typeof hubScope.utxos)[number]): UTxO => {
    const [txHash, index] = raw.outRef.split("#");
    return {
      ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
      txHash: txHash!,
      outputIndex: Number(index),
    };
  };
  const hubs = hubScope.utxos
    .map(decodeOutput)
    .filter(({ assets }) => assets[hubUnit] === 1n);
  if (hubs.length !== 1 || hubs[0]!.datum == null)
    throw new Error(
      "validation replay origin snapshot lacks the exact hub NFT",
    );
  const hub = hubs[0]!;
  const parameters = Data.from(hub.datum!, HubOracleDatum);
  const definitions = [
    { role: "deposit_event", kind: "deposit", policy: parameters.deposit },
    {
      role: "withdrawal_event",
      kind: "withdrawal",
      policy: parameters.withdrawal,
    },
    {
      role: "forced_transaction_event",
      kind: "forcedTransaction",
      policy: parameters.tx_order,
    },
  ] as const;
  const events: OriginEvent[] = [];
  for (const definition of definitions) {
    const scope = snapshot.scopes.find(({ role }) => role === definition.role);
    if (scope === undefined)
      throw new Error(
        "validation replay originating event coverage is incomplete",
      );
    for (const raw of scope.utxos) {
      const event = decodeOutput(raw);
      const tokens = Object.entries(event.assets).filter(([unit]) =>
        unit.startsWith(definition.policy),
      );
      if (tokens.length === 0) continue;
      if (tokens.length !== 1 || tokens[0]![1] !== 1n || event.datum == null)
        throw new Error("validation replay originating event NFT is ambiguous");
      events.push({
        kind: definition.kind,
        event,
        assetName: tokens[0]![0].slice(56),
      });
    }
  }
  // Preview and preprod use the same Cardano network id in the projected bytes.
  const network: Network = details.networkId === 1 ? "Mainnet" : "Preprod";
  return { events, hub, network, depositPolicyId: parameters.deposit };
};

const matchingOrigin = (
  source: Exclude<SourceEventRecord, { phase: "L2Transaction" }>,
  origins: ReturnType<typeof readOriginEvents>,
): OriginEvent => {
  const kind =
    source.phase === "Deposit"
      ? "deposit"
      : source.phase === "Withdrawal"
        ? "withdrawal"
        : "forcedTransaction";
  const matches = origins.events.filter((origin) => {
    if (origin.kind !== kind) return false;
    const id =
      origin.kind === "deposit"
        ? Data.from(origin.event.datum!, DepositDatum).event.id
        : origin.kind === "withdrawal"
          ? Data.from(origin.event.datum!, WithdrawalOrderDatum).event.id
          : Data.from(origin.event.datum!, TxOrderDatum).event.id;
    return (
      Data.to(id, OutputReference) ===
      Data.to(source.entry.key, OutputReference)
    );
  });
  if (matches.length !== 1)
    throw new Error(
      "validation replay requires one captured originating event; absent or consumed origins require retained history",
    );
  return matches[0]!;
};

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
 * Non-L2 events require their exact originating L1 authority. Its present-event
 * capture cannot stand in for an absent or already-consumed origin's history.
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
    if (source.phase === "Deposit") {
      const original = Data.from(origin!.event.datum!, DepositDatum).event;
      if (
        Data.to(original.info, DepositInfo) !==
        source.entry.valueBytes.toString("hex")
      )
        throw new Error(
          "validation replay deposit differs from its originating event",
        );
      const effect = deriveCanonicalDepositTransitionEffect({
        configuredNetwork: origins!.network,
        eventId: original.id,
        l2NetworkId: original.info.l2_network_id,
        l2Address: original.info.l2_address,
        l2DatumCbor:
          original.info.l2_datum === null
            ? null
            : Buffer.from(Data.to(original.info.l2_datum), "hex"),
        l1Assets: origin!.event.assets,
        depositPolicyId: origins!.depositPolicyId,
        depositAssetNameHex: origin!.assetName,
      });
      for (const operation of effect.operations) {
        const key = operation.outRefCbor.toString("hex");
        if (operation.type !== "insert" || state.has(key))
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
      const original = Data.from(
        origin!.event.datum!,
        WithdrawalOrderDatum,
      ).event;
      if (
        Data.to(
          { ...original.info, validity: source.entry.value.validity },
          WithdrawalInfo,
        ) !== source.entry.valueBytes.toString("hex")
      )
        throw new Error(
          "validation replay withdrawal differs from its originating body and signature",
        );
      const outRef = encodeMidgardSpendInputItem({
        txId: Buffer.from(original.info.body.l2_outref.transactionId, "hex"),
        outputIndex: Number(original.info.body.l2_outref.outputIndex),
      });
      const classification = await Effect.runPromise(
        classifyWithdrawalFromLedger({
          l2Owner: original.info.body.l2_owner,
          l2ValueCbor: Data.to(original.info.body.l2_value, Value),
          eventInfoCbor: Data.to(original.info, WithdrawalInfo),
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
