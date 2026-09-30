import { MIDGARD_TRANSITION_STEP_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { encodeNativeMpfEventLog } from "../services/mpf-native-owner/index.js";
import { MpfError } from "./errors.js";
import {
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
} from "./trace-events.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "./transition-cbor.js";
import {
  countedRootFromEncodedEntries,
  type NativeMpfBuildContext,
  type TransitionTraceBuildResult,
} from "./transition-trace.apply-trace-ledger-ops-to-mpf.js";
import { validateTransitionTraceSourceEvents } from "./transition-trace.validate-transition-trace-source-events.js";

export const buildNativeTransitionTraceResult = ({
  nativeMpf,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly nativeMpf: NativeMpfBuildContext;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<TransitionTraceBuildResult, MpfError> =>
  Effect.gen(function* () {
    const validationStartedAt = performance.now();
    const { totalEventCount, eventKeyCbors } =
      yield* validateTransitionTraceSourceEvents({
        sourceEvents,
        withdrawalCount,
        forcedTransactionCount,
        l2TransactionCount,
        depositCount,
        expectedTotalEventCount,
      });
    const validationMs = performance.now() - validationStartedAt;
    const eventLogEncodeStartedAt = performance.now();
    const eventLog = yield* Effect.try({
      try: () =>
        encodeNativeMpfEventLog(
          nativeMpf.handle.baseRoot,
          sourceEvents.map((sourceEvent) => sourceEvent.ledgerOps),
        ),
      catch: (cause) => MpfError.rootBuild("Architecture G event log", cause),
    });
    const eventLogEncodeMs = performance.now() - eventLogEncodeStartedAt;
    const ownerApplyStartedAt = performance.now();
    const applied = yield* Effect.tryPromise({
      try: () => nativeMpf.client.applyEvents(nativeMpf.handle, eventLog),
      catch: (cause) =>
        MpfError.rootBuild("Architecture G native owner", cause),
    });
    const ownerApplyMs = performance.now() - ownerApplyStartedAt;
    if (applied.eventRoots.length !== sourceEvents.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "Architecture G native owner",
          new Error(
            `Native owner returned the wrong event-root count: expected=${sourceEvents.length.toString()},actual=${applied.eventRoots.length.toString()}`,
          ),
        ),
      );
    }
    nativeMpf.eventLog = eventLog;
    nativeMpf.eventLogDigest = applied.eventLogDigest;
    nativeMpf.eventRoots = applied.eventRoots;
    nativeMpf.candidateRoot = applied.candidateRoot;

    let runningUtxosRoot = nativeMpf.handle.baseRoot;
    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    const eventToStepMembers: RetainedEventToStepMember[] = [];
    const memberAssemblyStartedAt = performance.now();
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const preUtxosRoot = runningUtxosRoot;
      runningUtxosRoot = applied.eventRoots[index]!;
      const value: SDK.TransitionStep = {
        schema_version: BigInt(MIDGARD_TRANSITION_STEP_SCHEMA_VERSION),
        step_index: BigInt(index),
        event_key: sourceEvent.eventKey,
        phase: sourceEvent.phase,
        pre_utxos_root: preUtxosRoot,
        post_utxos_root: runningUtxosRoot,
      };
      transitionTraceMembers.push({
        stepIndex: value.step_index,
        keyCbor: encodeTransitionIntegerCbor(value.step_index),
        valueCbor: encodeTransitionStepCbor(value),
        value,
      });
      const eventToStepValue: SDK.EventToStepValue = {
        step_index: value.step_index,
        phase: value.phase,
      };
      eventToStepMembers.push({
        eventKey: value.event_key,
        keyCbor: eventKeyCbors[index]!,
        valueCbor: encodeEventToStepValueCbor(eventToStepValue),
        value: eventToStepValue,
      });
    }
    const memberAssemblyMs = performance.now() - memberAssemblyStartedAt;
    const retainedRootsStartedAt = performance.now();
    const [transitionTraceRoot, eventToStepRoot] = yield* Effect.all(
      [
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
      ],
      { concurrency: "unbounded" },
    );
    const retainedRootsMs = performance.now() - retainedRootsStartedAt;
    return {
      finalUtxosRoot: runningUtxosRoot,
      transitionTraceRoot,
      eventToStepRoot,
      transitionTraceMembers,
      eventToStepMembers,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
      totalEventCount,
      transitionStepCount: transitionTraceMembers.length,
      nativePhaseMs: {
        validation: validationMs,
        eventLogEncode: eventLogEncodeMs,
        ownerApply: ownerApplyMs,
        ownerProofArena: applied.proofArenaDurationNs / 1_000_000,
        ownerMutation: applied.mutationDurationNs / 1_000_000,
        memberAssembly: memberAssemblyMs,
        retainedRoots: retainedRootsMs,
      },
      pathHydration: {
        prefetchMs: 0,
        uniquePaths: new Set(
          sourceEvents.flatMap((sourceEvent) =>
            sourceEvent.ledgerOps.map((op) => op.key.toString("hex")),
          ),
        ).size,
        nodesRequested: 0,
        hydrationHits: 0,
        hydrationMisses: 0,
        loadedNodes: 0,
        maxInFlight: 0,
        maxBatchKeys: 0,
        maxFrontierPaths: 0,
        retainedBytesEstimate: 0,
        chunkCount: sourceEvents.length === 0 ? 0 : 1,
        checkpointMs: 0,
        authenticationMs: 0,
        materializeMs: 0,
        collapseMs: 0,
        checkpointSerializedNodes: 0,
        checkpointSerializedBytes: 0,
        verifiedUpperNodes: 0,
        retainedUpperNodes: 0,
        collapsedNodes: 0,
        peakDecodedNodes: 0,
      },
    };
  });

export type NativeRootProbeResult = {
  readonly utxoRoot: string;
  readonly rawTxRoot: string;
  readonly txRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transitionRoots: readonly {
    readonly pre: string;
    readonly post: string;
  }[];
  readonly durationMs: number;
  readonly phaseMs: {
    readonly transactionSourceRoot: number;
    readonly transitionTraceBuild: number;
    readonly transactionMpfApply: number;
    readonly auxiliaryRoots: number;
  };
  readonly transitionTraceBuild: TransitionTraceBuildResult;
};
