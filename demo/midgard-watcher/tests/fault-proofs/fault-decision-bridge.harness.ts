import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  authenticatedStateQueueObservationDigest,
  type HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
import { vi } from "vitest";

import { unsafeCreateWatcherFaultDecisionBridgeForTest } from "../../src/fault-proofs/fault-decision-bridge.js";
import type { BridgeDependencies } from "../../src/fault-proofs/fault-decision-bridge.selected-target.js";
import type { WatcherPersistedFaultDecisionRecord } from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import type { WatcherFaultProofProgressRequest } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { type WatcherOperationsSink } from "../../src/runtime/operations-observability.js";
import {
  decision,
  OBSERVATION_DIGEST,
} from "./fault-decision-bridge.observation.js";

export const harness = (input: {
  readonly current: WatcherAuthenticatedStateQueueObservation;
  readonly categoryByHeader: Readonly<Record<string, string>>;
  readonly records?: readonly WatcherPersistedFaultDecisionRecord[];
  readonly classifyOverride?: (
    value: ReturnType<typeof decision>,
  ) => HeaderDecision | Promise<HeaderDecision>;
  readonly enqueueError?: Error;
  readonly operationsSink?: WatcherOperationsSink;
  readonly nowMs?: () => bigint;
  readonly monotonicNowMs?: () => number;
  readonly pendingAvailabilityHeaders?: (
    observation: WatcherAuthenticatedStateQueueObservation,
    merged?: ReadonlySet<string>,
  ) => ReadonlySet<string>;
  readonly mergedHeaders?: BridgeDependencies["mergedHeaders"];
  readonly warn?: BridgeDependencies["warn"];
  readonly deferredRetryDelayMs?: BridgeDependencies["deferredRetryDelayMs"];
  readonly resolvePredecessorOverride?: (
    header: WatcherStateQueueHeaderObservation,
  ) => Promise<WatcherStateQueueHeaderObservation | undefined>;
  readonly classificationContextIdentity?: () => Promise<string>;
  readonly decisionUsesLocalEventHistory?: boolean;
  readonly permitAuthority?:
    | "submission"
    | "reconciliation"
    | (() => "submission" | "reconciliation");
  readonly observationDigestOverride?: typeof authenticatedStateQueueObservationDigest;
  readonly deadlineOffset?: () => number;
}) => {
  const admitted = new WeakSet<object>([input.current]);
  const appended: ReturnType<typeof decision>[] = [];
  const enqueued: ReturnType<typeof decision>[] = [];
  const enqueuedGenerations: string[] = [];
  const controllerGenerations: string[] = [];
  const revocations: string[] = [];
  const restrictions: string[] = [];
  const progressRequests: WatcherFaultProofProgressRequest[] = [];
  const authorityRevocations: string[] = [];
  const permitIdentities = new WeakMap<
    object,
    {
      decision: Extract<HeaderDecision, { decision: "fault_detected" }>;
      generation: string;
    }
  >();
  const retainedDecisionAuthorities: (string | null)[] = [];
  const application = {
    deploymentFingerprint: input.current.deploymentIdentityDigest,
    installedCategories: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    classifyHeader: vi.fn(async ({ observation: header }) => {
      const category = input.categoryByHeader[header.headerHash];
      if (
        category === undefined ||
        !WATCHER_INSTALLED_WORKFLOW_CATEGORIES.includes(category as never)
      ) {
        throw new Error("test omitted category");
      }
      const fresh = decision(header.headerHash, category as never);
      return input.classifyOverride === undefined
        ? fresh
        : await input.classifyOverride(fresh);
    }),
  };
  const bridge = unsafeCreateWatcherFaultDecisionBridgeForTest({
    application,
    runtimeConfigPath: "/var/lib/midgard/watcher.json",
    maximumClassificationConcurrency: 2,
    dependencies: Object.freeze({
      ...(input.operationsSink === undefined
        ? {}
        : { operationsSink: input.operationsSink }),
      ...(input.nowMs === undefined ? {} : { nowMs: input.nowMs }),
      ...(input.monotonicNowMs === undefined
        ? {}
        : { monotonicNowMs: input.monotonicNowMs }),
      ...(input.pendingAvailabilityHeaders === undefined
        ? {}
        : { pendingAvailabilityHeaders: input.pendingAvailabilityHeaders }),
      ...(input.mergedHeaders === undefined
        ? {}
        : { mergedHeaders: input.mergedHeaders }),
      ...(input.warn === undefined ? {} : { warn: input.warn }),
      ...(input.deferredRetryDelayMs === undefined
        ? {}
        : { deferredRetryDelayMs: input.deferredRetryDelayMs }),
      retainDecisionAuthorities: (digest) =>
        retainedDecisionAuthorities.push(digest),
      decisionUsesLocalEventHistory: () =>
        input.decisionUsesLocalEventHistory ?? false,
      assertObservation: (candidate) => {
        if (!admitted.has(candidate)) throw new Error("not admitted");
      },
      observationDigest: async (observation) =>
        input.observationDigestOverride === undefined
          ? OBSERVATION_DIGEST
          : await input.observationDigestOverride({
              observation,
              minimumConfirmationDepth: 30,
            }),
      ...(input.resolvePredecessorOverride === undefined
        ? {}
        : { resolvePredecessorHeader: input.resolvePredecessorOverride }),
      classificationContextIdentity:
        input.classificationContextIdentity ?? (async () => "test-context"),
      readRecords: async () => input.records ?? Object.freeze([]),
      append: async (fresh) => {
        appended.push(fresh as ReturnType<typeof decision>);
        return Object.freeze({
          schemaVersion: "midgard-watcher-production-fault-decision-record-v1",
          revision: (appended.length - 1).toString(),
          priorRecordSha256: null,
          decision: fresh,
        });
      },
      assertActuationPermitIdentity: ({
        permit,
        category,
        rollbackGeneration,
      }) => {
        const stored = permitIdentities.get(permit);
        if (
          stored === undefined ||
          stored.decision.category !== category ||
          stored.generation !== rollbackGeneration ||
          revocations.length !== 0
        )
          throw new Error("test permit revoked or substituted");
        return {
          decisionDigest: stored.decision.decisionDigest,
          executionDecisionDigest: stored.decision.decisionDigest,
          launchScope: stored.decision.launchScope,
          deploymentFingerprint: input.current.deploymentIdentityDigest,
          headerHash: stored.decision.headerHash,
          authority:
            typeof input.permitAuthority === "function"
              ? input.permitAuthority()
              : (input.permitAuthority ?? "submission"),
        };
      },
      createActuationController: (_fresh, rollbackGeneration) => {
        controllerGenerations.push(rollbackGeneration);
        const permit = Object.freeze({
          permitVersion:
            "midgard-production-workflow-actuation-permit-v1" as const,
        });
        permitIdentities.set(permit, {
          decision: _fresh,
          generation: rollbackGeneration,
        });
        return Object.freeze({
          permit,
          restrictToReconciliation: (reason: string) => {
            restrictions.push(reason);
          },
          revoke: (reason: string) => {
            revocations.push(reason);
          },
        });
      },
      deadlineForHeader: (header) =>
        Object.freeze({
          headerHash: header.headerHash,
          headerEndTimeMs: "0",
          maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
          latestSafeStartAtMs: (
            MIDGARD_RETENTION_WINDOW.maturityMs -
            MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs +
            (input.deadlineOffset?.() ?? 0)
          ).toString(),
        }),
      requestProgress: async (request) => {
        if (input.enqueueError !== undefined) throw input.enqueueError;
        progressRequests.push(request);
        if (request.fault !== undefined) {
          enqueued.push(request.fault.decision as ReturnType<typeof decision>);
          enqueuedGenerations.push(request.rollbackGeneration);
        }
      },
      unfinishedObjectiveCount: () => enqueued.length,
      revokeAuthority: (reason) => {
        authorityRevocations.push(reason);
      },
    }),
  });
  return {
    admitted,
    appended,
    application,
    bridge,
    controllerGenerations,
    enqueued,
    enqueuedGenerations,
    revocations,
    restrictions,
    progressRequests,
    authorityRevocations,
    retainedDecisionAuthorities,
  };
};
