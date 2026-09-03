/**
 * Transition-trace result construction: trace roots, the native build context, and the
 * native production-root probe.
 */

import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_TRANSITION_STEP_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Fiber, Option } from "effect";

import {
  encodeNativeMpfEventLog,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerClient,
} from "../services/mpf-native-owner/index.js";
import { keyValuePhasRootWithCount } from "../workers/utils/mpf/phas.js";
import {
  buildCountedMpfRootInWorker,
  shouldBuildMpfRootInWorker,
} from "../workers/utils/mpf-root-pool.js";
import {
  getMpfPathHydrationConfig,
  type MpfPathHydrationDiagnostics,
} from "./engine-config.js";
import { MpfError } from "./errors.js";
import { MidgardMpf } from "./store.js";
import {
  eventKeyCbor,
  eventKeyFingerprint,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
} from "./trace-events.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "./transition-cbor.js";
import { type MpfBatchOp, type MpfInsertBatchOp } from "./types.js";

export type NativeMpfBuildContext = {
  readonly client: NativeMpfOwnerClient;
  readonly handle: NativeMpfGenerationHandle;
  readonly ownerBinarySha256: string;
  eventLog?: Buffer;
  eventLogDigest?: string;
  eventRoots?: readonly string[];
  candidateRoot?: string;
};

export type NativeMpfReplayBuild = {
  readonly schema: 1;
  readonly ownerBinarySha256: Buffer;
  readonly baseRoot: Buffer;
  readonly candidateRoot: Buffer;
  readonly eventLog: Buffer;
  readonly eventLogDigest: Buffer;
  readonly eventRoots: Buffer;
  readonly eventCount: number;
};

export type TransitionTraceBuildResult = {
  readonly finalUtxosRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly totalEventCount: number;
  readonly transitionStepCount: number;
  readonly pathHydration: MpfPathHydrationDiagnostics;
  readonly nativePhaseMs?: {
    readonly validation: number;
    readonly eventLogEncode: number;
    readonly ownerApply: number;
    readonly ownerProofArena: number;
    readonly ownerMutation: number;
    readonly memberAssembly: number;
    readonly retainedRoots: number;
  };
};

export const countedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    if (shouldBuildMpfRootInWorker(entries.length)) {
      return yield* Effect.tryPromise({
        try: () => buildCountedMpfRootInWorker(domain, entries),
        catch: (cause) => MpfError.rootBuild("parallel counted root", cause),
      });
    }
    const phas = yield* keyValuePhasRootWithCount(
      entries.map((entry) => entry.key),
      entries.map((entry) => entry.value),
    );
    return yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.rootBuild(
          "count-bound transition commitment",
          new Error("Failed to commit count-bound root", { cause }),
        ),
      ),
    );
  });

export const buildTransactionsSourceRoot = (
  entries: readonly MpfInsertBatchOp[],
  domain: SDK.RootDomain = SDK.ROOT_DOMAINS.transactionsV1,
): Effect.Effect<string, MpfError> =>
  countedRootFromEncodedEntries(domain, entries);

export const indexTransitionTraceMembersByEventKey = (
  members: readonly RetainedTransitionTraceMember[],
): Effect.Effect<
  ReadonlyMap<string, RetainedTransitionTraceMember>,
  MpfError
> =>
  Effect.gen(function* () {
    const byEventKey = new Map<string, RetainedTransitionTraceMember>();
    for (const member of members) {
      const fingerprint = yield* eventKeyFingerprint(member.value.event_key);
      if (byEventKey.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "validation trace",
            new Error(
              `Transition trace contains duplicate event key ${fingerprint}`,
            ),
          ),
        );
      }
      byEventKey.set(fingerprint, member);
    }
    return byEventKey;
  });

const assertUniqueTransitionSourceEvents = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    const seen = new Set<string>();
    for (const [index, event] of sourceEvents.entries()) {
      const fingerprint = yield* eventKeyFingerprint(event.eventKey);
      if (seen.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Duplicate source event key at source index ${index.toString()}: ${fingerprint}`,
            ),
          ),
        );
      }
      seen.add(fingerprint);
    }
  });

const transitionPhaseRank = (phase: SDK.TransitionPhase): number => {
  switch (phase) {
    case "Withdrawal":
      return 0;
    case "ForcedTransaction":
      return 1;
    case "L2Transaction":
      return 2;
    case "Deposit":
      return 3;
  }
};

const assertCanonicalTransitionPhaseOrder = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    let lastRank = -1;
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const rank = transitionPhaseRank(sourceEvent.phase);
      if (rank < lastRank) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition source events are not in canonical phase order at source index ${index.toString()}: phase=${sourceEvent.phase}`,
            ),
          ),
        );
      }
      lastRank = rank;
    }
  });

export const applyTraceLedgerOpsToMpf = (
  ledgerMpf: MidgardMpf,
  ops: readonly MpfBatchOp[],
  eventKeyDescription: string,
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    if (ledgerMpf.usesStrictOverlayMutations()) {
      yield* ledgerMpf
        .applyBatch(ops)
        .pipe(
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
      return;
    }
    const eventPresenceOverlay = new Map<string, boolean>();
    const isPresent = (key: Buffer): Effect.Effect<boolean, MpfError> => {
      const keyHex = key.toString("hex");
      const overlayPresence = eventPresenceOverlay.get(keyHex);
      if (overlayPresence !== undefined) {
        return Effect.succeed(overlayPresence);
      }
      return ledgerMpf.get(key).pipe(Effect.map(Option.isSome));
    };

    for (const op of ops) {
      const keyHex = op.key.toString("hex");
      const present = yield* isPresent(op.key);
      if (op.type === "delete") {
        if (!present) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "transition trace",
              new Error(
                `Transition event ${eventKeyDescription} deletes missing UTxO ${keyHex}`,
              ),
            ),
          );
        }
        eventPresenceOverlay.set(keyHex, false);
        continue;
      }
      if (present) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition event ${eventKeyDescription} inserts duplicate UTxO ${keyHex}`,
            ),
          ),
        );
      }
      eventPresenceOverlay.set(keyHex, true);
    }

    yield* ledgerMpf.applyBatch(ops);
  });

const validateTransitionTraceSourceEvents = ({
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<
  {
    readonly totalEventCount: number;
    readonly eventKeyCbors: readonly Buffer[];
  },
  MpfError
> =>
  Effect.gen(function* () {
    const sourceCountBounds = [
      [
        "withdrawal",
        withdrawalCount,
        MIDGARD_CONSENSUS_LIMITS.maxWithdrawalCount,
      ],
      [
        "forced transaction",
        forcedTransactionCount,
        MIDGARD_CONSENSUS_LIMITS.maxForcedTransactionCount,
      ],
      [
        "L2 transaction",
        l2TransactionCount,
        MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
      ],
      ["deposit", depositCount, MIDGARD_CONSENSUS_LIMITS.maxDepositCount],
    ] as const;
    for (const [label, count, maximum] of sourceCountBounds) {
      if (!Number.isSafeInteger(count) || count < 0 || count > maximum) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `${label} count must be a safe integer between 0 and ${maximum.toString()}: ${count.toString()}`,
            ),
          ),
        );
      }
    }
    const totalEventCount =
      withdrawalCount +
      forcedTransactionCount +
      l2TransactionCount +
      depositCount;
    if (
      totalEventCount > MIDGARD_CONSENSUS_LIMITS.maxTotalEventCount ||
      totalEventCount > MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source count ${totalEventCount.toString()} exceeds the launch event/step bound`,
          ),
        ),
      );
    }
    const ledgerOperationCount = sourceEvents.reduce(
      (total, sourceEvent) => total + sourceEvent.ledgerOps.length,
      0,
    );
    if (
      ledgerOperationCount > MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Ledger operation count ${ledgerOperationCount.toString()} exceeds the V1 consensus maximum ${MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount.toString()}`,
          ),
        ),
      );
    }
    if (
      expectedTotalEventCount !== undefined &&
      totalEventCount !== expectedTotalEventCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source count mismatch: expected=${expectedTotalEventCount.toString()},actual=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    if (sourceEvents.length !== totalEventCount) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source event array length does not match source counts: source_events=${sourceEvents.length.toString()},source_count_sum=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    const eventKeyCbors: Buffer[] = [];
    const seenEventKeys = new Set<string>();
    for (const [index, event] of sourceEvents.entries()) {
      const keyCbor = yield* eventKeyCbor(event.eventKey);
      const fingerprint = keyCbor.toString("hex");
      if (seenEventKeys.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Duplicate source event key at source index ${index.toString()}: ${fingerprint}`,
            ),
          ),
        );
      }
      seenEventKeys.add(fingerprint);
      eventKeyCbors.push(keyCbor);
    }
    yield* assertCanonicalTransitionPhaseOrder(sourceEvents);
    return { totalEventCount, eventKeyCbors };
  });

export const buildEventToStepMembersFromTrace = ({
  sourceEvents,
  transitionTraceMembers,
}: {
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
}): Effect.Effect<readonly RetainedEventToStepMember[], MpfError> =>
  Effect.gen(function* () {
    yield* assertUniqueTransitionSourceEvents(sourceEvents);
    if (sourceEvents.length !== transitionTraceMembers.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Transition source event count does not match trace step count: source_events=${sourceEvents.length.toString()},trace_steps=${transitionTraceMembers.length.toString()}`,
          ),
        ),
      );
    }

    const sourceByKey = new Map<string, TransitionTraceSourceEvent>();
    for (const sourceEvent of sourceEvents) {
      sourceByKey.set(
        yield* eventKeyFingerprint(sourceEvent.eventKey),
        sourceEvent,
      );
    }

    const seenTraceEvents = new Set<string>();
    const members: RetainedEventToStepMember[] = [];
    for (const traceMember of transitionTraceMembers) {
      const step = traceMember.value;
      const fingerprint = yield* eventKeyFingerprint(step.event_key);
      const source = sourceByKey.get(fingerprint);
      if (source === undefined) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step ${step.step_index.toString()} references an event key with no source-root member: ${fingerprint}`,
            ),
          ),
        );
      }
      if (seenTraceEvents.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace contains duplicate event key ${fingerprint}`,
            ),
          ),
        );
      }
      if (source.phase !== step.phase) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step phase does not match source phase: step_index=${step.step_index.toString()},source_phase=${source.phase},step_phase=${step.phase}`,
            ),
          ),
        );
      }
      seenTraceEvents.add(fingerprint);
      const value: SDK.EventToStepValue = {
        step_index: step.step_index,
        phase: step.phase,
      };
      members.push({
        eventKey: step.event_key,
        keyCbor: yield* eventKeyCbor(step.event_key),
        valueCbor: encodeEventToStepValueCbor(value),
        value,
      });
    }

    if (seenTraceEvents.size !== sourceByKey.size) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Event-to-step root omits source events: source_events=${sourceByKey.size.toString()},mapped_events=${seenTraceEvents.size.toString()}`,
          ),
        ),
      );
    }
    return members;
  });

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
      if (
        hydrationConfig.mode !== "whole_block" &&
        !ledgerMpf.usesEventFlatEngine()
      ) {
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
      if (
        hydrationConfig.mode !== "whole_block" &&
        !ledgerMpf.usesEventFlatEngine()
      ) {
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

/**
 * Runs the complete root-building portion of the Architecture G production
 * commit path without the database-specific transaction classification phase.
 * Inputs are the exact encoded entries that processMpfs applies after decoding.
 * This is intentionally colocated with processMpfs so benchmarks cannot replace
 * production root algorithms with harness-specific approximations.
 */
export const buildNativeRootProbe = ({
  nativeMpf,
  sourceEvents,
  transactionOps,
  deposits = [],
  withdrawals = [],
  forcedTransactions = [],
}: {
  readonly nativeMpf: NativeMpfBuildContext;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transactionOps: readonly MpfInsertBatchOp[];
  readonly deposits?: readonly MpfInsertBatchOp[];
  readonly withdrawals?: readonly MpfInsertBatchOp[];
  readonly forcedTransactions?: readonly MpfInsertBatchOp[];
}): Effect.Effect<NativeRootProbeResult, MpfError> =>
  Effect.acquireUseRelease(
    MidgardMpf.createScratch("architecture-g-production-probe-transactions"),
    (transactionsMpf) =>
      Effect.gen(function* () {
        const startedAt = performance.now();
        const timedTransactionSourceRoot = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          const root = yield* buildTransactionsSourceRoot(transactionOps);
          return { root, durationMs: performance.now() - phaseStartedAt };
        }).pipe(Effect.fork);
        const timedTransactionMpfApply = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          yield* transactionsMpf.applyBatch(transactionOps);
          const rawTxRoot = yield* transactionsMpf.rootHex();
          return {
            rawTxRoot,
            durationMs: performance.now() - phaseStartedAt,
          };
        }).pipe(Effect.fork);
        const transitionTraceStartedAt = performance.now();
        const transitionTraceBuild = yield* buildNativeTransitionTraceResult({
          nativeMpf,
          sourceEvents,
          withdrawalCount: withdrawals.length,
          forcedTransactionCount: forcedTransactions.length,
          l2TransactionCount: transactionOps.length,
          depositCount: deposits.length,
        });
        const transitionTraceBuildMs =
          performance.now() - transitionTraceStartedAt;
        const [timedTxRoot, timedRawTxRoot] = yield* Effect.all(
          [
            Fiber.join(timedTransactionSourceRoot),
            Fiber.join(timedTransactionMpfApply),
          ],
          { concurrency: "unbounded" },
        );

        const auxiliaryRootsStartedAt = performance.now();
        const eventRoot = (
          domain: SDK.RootDomain,
          entries: readonly MpfInsertBatchOp[],
        ): Effect.Effect<string, MpfError> =>
          entries.length === 0
            ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
            : countedRootFromEncodedEntries(domain, entries);
        const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
          yield* Effect.all(
            [
              eventRoot(SDK.ROOT_DOMAINS.deposits, deposits),
              eventRoot(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
              eventRoot(
                SDK.ROOT_DOMAINS.forcedTransactionsV1,
                forcedTransactions,
              ),
            ],
            { concurrency: "unbounded" },
          );
        const auxiliaryRootsMs = performance.now() - auxiliaryRootsStartedAt;
        const utxoRoot = nativeMpf.candidateRoot;
        if (utxoRoot === undefined) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error("Native owner did not return a candidate UTxO root"),
            ),
          );
        }
        if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error(
                `Transition trace final root mismatch: trace=${transitionTraceBuild.finalUtxosRoot},candidate=${utxoRoot}`,
              ),
            ),
          );
        }
        return {
          utxoRoot,
          rawTxRoot: timedRawTxRoot.rawTxRoot,
          txRoot: timedTxRoot.root,
          transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
          eventToStepRoot: transitionTraceBuild.eventToStepRoot,
          depositsRoot,
          withdrawalsRoot,
          forcedTransactionsRoot,
          transitionRoots: transitionTraceBuild.transitionTraceMembers.map(
            (member) => ({
              pre: member.value.pre_utxos_root,
              post: member.value.post_utxos_root,
            }),
          ),
          durationMs: performance.now() - startedAt,
          phaseMs: {
            transactionSourceRoot: timedTxRoot.durationMs,
            transitionTraceBuild: transitionTraceBuildMs,
            transactionMpfApply: timedRawTxRoot.durationMs,
            auxiliaryRoots: auxiliaryRootsMs,
          },
          transitionTraceBuild,
        };
      }),
    (transactionsMpf) => transactionsMpf.close().pipe(Effect.orDie),
  );
