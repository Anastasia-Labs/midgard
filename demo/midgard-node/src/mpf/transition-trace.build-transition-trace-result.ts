import { MIDGARD_TRANSITION_STEP_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  getMpfPathHydrationConfig,
  type MpfPathHydrationDiagnostics,
} from "./engine-config.js";
import { MpfError } from "./errors.js";
import { MidgardMpf } from "./store.js";
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
  applyTraceLedgerOpsToMpf,
  countedRootFromEncodedEntries,
  type TransitionTraceBuildResult,
} from "./transition-trace.apply-trace-ledger-ops-to-mpf.js";
import { validateTransitionTraceSourceEvents } from "./transition-trace.validate-transition-trace-source-events.js";

export const buildTransitionTraceResult = ({
  ledgerMpf,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly ledgerMpf: MidgardMpf;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<TransitionTraceBuildResult, MpfError> =>
  Effect.gen(function* () {
    const { totalEventCount, eventKeyCbors } =
      yield* validateTransitionTraceSourceEvents({
        sourceEvents,
        withdrawalCount,
        forcedTransactionCount,
        l2TransactionCount,
        depositCount,
        expectedTotalEventCount,
      });
    const hydrationConfig = getMpfPathHydrationConfig();
    const indexedEvents = sourceEvents.map((sourceEvent, index) => ({
      index,
      sourceEvent,
    }));
    const eventChunks: (typeof indexedEvents)[] = [];
    if (hydrationConfig.mode === "whole_block") {
      if (indexedEvents.length > 0) eventChunks.push(indexedEvents);
    } else {
      let chunk: typeof indexedEvents = [];
      let chunkOps = 0;
      for (const indexedEvent of indexedEvents) {
        const eventOps = indexedEvent.sourceEvent.ledgerOps.length;
        if (
          chunk.length > 0 &&
          chunkOps + eventOps > hydrationConfig.chunkOps
        ) {
          eventChunks.push(chunk);
          chunk = [];
          chunkOps = 0;
        }
        chunk.push(indexedEvent);
        chunkOps += eventOps;
      }
      if (chunk.length > 0) eventChunks.push(chunk);
    }
    const uniqueTouchedPaths = new Set(
      sourceEvents.flatMap((sourceEvent) =>
        sourceEvent.ledgerOps.map((op) => op.key.toString("hex")),
      ),
    );
    const pathHydration: {
      -readonly [K in keyof MpfPathHydrationDiagnostics]: MpfPathHydrationDiagnostics[K];
    } = {
      prefetchMs: 0,
      uniquePaths: uniqueTouchedPaths.size,
      nodesRequested: 0,
      hydrationHits: 0,
      hydrationMisses: 0,
      loadedNodes: 0,
      maxInFlight: 0,
      maxBatchKeys: 0,
      maxFrontierPaths: 0,
      retainedBytesEstimate: 0,
      chunkCount: 0,
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
    };
    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    let runningUtxosRoot = yield* ledgerMpf.rootHex();
    let retainedUpperNodes = 0;
    if (hydrationConfig.mode === "chunked_arena") {
      const primed = yield* ledgerMpf.primeBlockPathArena(
        sourceEvents.flatMap((sourceEvent) => sourceEvent.ledgerOps),
        hydrationConfig.retainDepth,
        false,
      );
      pathHydration.prefetchMs += primed.hydration.prefetchMs;
      pathHydration.nodesRequested += primed.hydration.nodesRequested;
      pathHydration.hydrationHits += primed.hydration.hydrationHits;
      pathHydration.hydrationMisses += primed.hydration.hydrationMisses;
      pathHydration.loadedNodes += primed.hydration.loadedNodes;
      pathHydration.maxInFlight = primed.hydration.maxInFlight;
      pathHydration.maxBatchKeys = primed.hydration.maxBatchKeys;
      pathHydration.maxFrontierPaths = primed.hydration.maxFrontierPaths;
      pathHydration.retainedBytesEstimate =
        primed.hydration.retainedBytesEstimate;
      pathHydration.authenticationMs +=
        primed.authenticationMs + primed.checkpoint.authenticationMs;
      pathHydration.checkpointMs += primed.checkpoint.checkpointMs;
      pathHydration.collapseMs += primed.checkpoint.collapseMs;
      pathHydration.verifiedUpperNodes +=
        primed.verifiedNodes + primed.checkpoint.verifiedUpperNodes;
      pathHydration.collapsedNodes += primed.checkpoint.collapsedNodes;
      retainedUpperNodes = primed.checkpoint.retainedUpperNodes;
      pathHydration.retainedUpperNodes = retainedUpperNodes;
      pathHydration.peakDecodedNodes = Math.max(
        pathHydration.peakDecodedNodes,
        primed.hydration.loadedNodes,
      );
    }
    for (const eventChunk of eventChunks) {
      if (hydrationConfig.mode !== "chunked_arena") {
        const hydration = yield* ledgerMpf.prefetchTouchedPaths(
          eventChunk.flatMap(({ sourceEvent }) => sourceEvent.ledgerOps),
        );
        pathHydration.prefetchMs += hydration.prefetchMs;
        pathHydration.nodesRequested += hydration.nodesRequested;
        pathHydration.hydrationHits += hydration.hydrationHits;
        pathHydration.hydrationMisses += hydration.hydrationMisses;
        pathHydration.loadedNodes += hydration.loadedNodes;
        pathHydration.maxInFlight = Math.max(
          pathHydration.maxInFlight,
          hydration.maxInFlight,
        );
        pathHydration.maxBatchKeys = Math.max(
          pathHydration.maxBatchKeys,
          hydration.maxBatchKeys,
        );
        pathHydration.maxFrontierPaths = Math.max(
          pathHydration.maxFrontierPaths,
          hydration.maxFrontierPaths,
        );
        pathHydration.retainedBytesEstimate = Math.max(
          pathHydration.retainedBytesEstimate,
          hydration.retainedBytesEstimate,
        );
        pathHydration.peakDecodedNodes = Math.max(
          pathHydration.peakDecodedNodes,
          retainedUpperNodes + hydration.loadedNodes,
        );
      }
      pathHydration.chunkCount += 1;
      if (hydrationConfig.mode !== "whole_block") {
        const authentication = yield* ledgerMpf.authenticateDecodedArena(
          hydrationConfig.mode === "chunked_arena"
            ? 0
            : hydrationConfig.retainDepth,
        );
        pathHydration.verifiedUpperNodes += authentication.verifiedNodes;
        pathHydration.authenticationMs += authentication.authenticationMs;
      }
      for (const { index, sourceEvent } of eventChunk) {
        const preUtxosRoot = runningUtxosRoot;
        const eventKeyDescription = eventKeyCbors[index]!.toString("hex");
        if (ledgerMpf.usesStrictOverlayMutations()) {
          const postUtxosRoot = yield* ledgerMpf
            .applyBatch(sourceEvent.ledgerOps)
            .pipe(
              Effect.map((root) => root.toString("hex")),
              Effect.mapError((cause) =>
                MpfError.rootBuild(
                  "transition trace",
                  new Error(
                    `Transition event ${eventKeyDescription} failed strict ledger mutation`,
                    { cause },
                  ),
                ),
              ),
            );
          runningUtxosRoot = postUtxosRoot;
        } else {
          yield* applyTraceLedgerOpsToMpf(
            ledgerMpf,
            sourceEvent.ledgerOps,
            eventKeyDescription,
          );
          const postUtxosRoot = yield* ledgerMpf.rootHex();
          runningUtxosRoot = postUtxosRoot;
        }
        const value: SDK.TransitionStep = {
          schema_version: BigInt(MIDGARD_TRANSITION_STEP_SCHEMA_VERSION),
          step_index: BigInt(index),
          event_key: sourceEvent.eventKey,
          phase: sourceEvent.phase,
          pre_utxos_root: preUtxosRoot,
          post_utxos_root: runningUtxosRoot,
        };
        const member: RetainedTransitionTraceMember = {
          stepIndex: value.step_index,
          keyCbor: encodeTransitionIntegerCbor(value.step_index),
          valueCbor: encodeTransitionStepCbor(value),
          value,
        };
        transitionTraceMembers.push(member);
      }
      if (hydrationConfig.mode !== "whole_block") {
        const checkpoint = yield* ledgerMpf.checkpointAndCollapseDecodedArena(
          hydrationConfig.retainDepth,
          hydrationConfig.mode !== "chunked_arena",
          hydrationConfig.mode !== "chunked_arena",
        );
        pathHydration.checkpointMs += checkpoint.checkpointMs;
        pathHydration.authenticationMs += checkpoint.authenticationMs;
        pathHydration.materializeMs += checkpoint.materializeMs;
        pathHydration.collapseMs += checkpoint.collapseMs;
        pathHydration.checkpointSerializedNodes += checkpoint.serializedNodes;
        pathHydration.checkpointSerializedBytes += checkpoint.serializedBytes;
        pathHydration.verifiedUpperNodes += checkpoint.verifiedUpperNodes;
        pathHydration.collapsedNodes += checkpoint.collapsedNodes;
        retainedUpperNodes = checkpoint.retainedUpperNodes;
        pathHydration.retainedUpperNodes = Math.max(
          pathHydration.retainedUpperNodes,
          retainedUpperNodes,
        );
      }
    }

    const eventToStepMembers: RetainedEventToStepMember[] = [];
    for (const [index, traceMember] of transitionTraceMembers.entries()) {
      const value: SDK.EventToStepValue = {
        step_index: traceMember.value.step_index,
        phase: traceMember.value.phase,
      };
      eventToStepMembers.push({
        eventKey: traceMember.value.event_key,
        keyCbor: eventKeyCbors[index]!,
        valueCbor: encodeEventToStepValueCbor(value),
        value,
      });
    }
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
      pathHydration,
    };
  });
