import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerClient,
} from "../services/mpf-native-owner/index.js";
import { keyValuePhasRootWithCount } from "../workers/utils/mpf/phas.js";
import {
  buildCountedMpfRootInWorker,
  shouldBuildMpfRootInWorker,
} from "../workers/utils/mpf-root-pool.js";
import { type MpfPathHydrationDiagnostics } from "./engine-config.js";
import { MpfError } from "./errors.js";
import { MidgardMpf } from "./store.js";
import {
  eventKeyFingerprint,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
} from "./trace-events.js";
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

export const assertUniqueTransitionSourceEvents = (
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

export const assertCanonicalTransitionPhaseOrder = (
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
