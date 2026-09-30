import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { MpfError } from "./errors.js";
import {
  eventKeyCbor,
  eventKeyFingerprint,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
} from "./trace-events.js";
import { encodeEventToStepValueCbor } from "./transition-cbor.js";
import {
  assertCanonicalTransitionPhaseOrder,
  assertUniqueTransitionSourceEvents,
} from "./transition-trace.apply-trace-ledger-ops-to-mpf.js";

export const validateTransitionTraceSourceEvents = ({
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
