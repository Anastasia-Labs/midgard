import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  authenticatedStateQueueObservationDigest,
  type HeaderDecision,
  LocalKupmiosCheckpointChangedError,
} from "@al-ft/midgard-fault-proofs";
import {
  type CorrectionLockDatum,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultDecisionBridgeForTest } from "../../src/fault-proofs/fault-decision-bridge.js";
import type { WatcherPersistedFaultDecisionRecord } from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import type { WatcherFaultProofProgressRequest } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  type WatcherAuthenticatedStateQueueObservation,
  WatcherRetainedHeaderAttestationPendingError,
  type WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsSink,
} from "../../src/runtime/operations-observability.js";
import { createWatcherStateQueueRuntime } from "../../src/runtime/state-queue-runtime.js";

const DEPLOYMENT = "dd".repeat(32);
const OBSERVATION_DIGEST = "11".repeat(32);

const headerFixture = (suffix = "00"): HeaderType => ({
  prevUtxosRoot: "00".repeat(32),
  transactionsRoot: "01".repeat(32),
  utxosRoot: "02".repeat(32),
  depositsRoot: "03".repeat(32),
  withdrawalsRoot: "04".repeat(32),
  forcedTransactionsRoot: "05".repeat(32),
  transitionTraceRoot: "06".repeat(32),
  eventToStepRoot: "07".repeat(32),
  validationTracesRoot: "08".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: BigInt(`0x${suffix}`),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "08".repeat(28),
  operatorVkey: "09".repeat(28),
  protocolVersion: 1n,
});

const encodedHeader = (header: HeaderType) => {
  const cbor = Data.to(header, Header);
  return {
    cbor,
    hash: computeHash28(Buffer.from(cbor, "hex")).toString("hex"),
  };
};

const observation = (
  headers: readonly HeaderType[],
  lockDatum: CorrectionLockDatum = "Idle",
): WatcherAuthenticatedStateQueueObservation => {
  const encoded = headers.map(encodedHeader);
  return Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: DEPLOYMENT,
    protocolScriptAuthorityDigest: "10".repeat(32),
    stateQueuePolicyId: "11".repeat(28),
    hubOraclePolicyId: "12".repeat(28),
    nativePoint: Object.freeze({
      blockHash: "13".repeat(32),
      parentBlockHash: "14".repeat(32),
      slot: "1000",
      blockNo: "100",
      chainPointId: "15".repeat(32),
      finalityDepth: "30",
    }),
    sourceId: "watcher-test-local-node",
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([
      Object.freeze({ headerHash: null, outRef: `${"16".repeat(32)}#0` }),
      ...encoded.map(({ hash }, index) =>
        Object.freeze({
          headerHash: hash,
          outRef: `${"17".repeat(32)}#${index.toString()}`,
        }),
      ),
    ]),
    finalizedHeaders: Object.freeze(
      encoded.map(({ cbor, hash }, index) =>
        Object.freeze({
          headerHash: hash,
          headerCborHex: cbor,
          stateQueueNodeCborHex: "d87980",
          linkedListDatumCborHex: "d87980",
          daAvailability: "Unattested",
          queueOutRef: `${"17".repeat(32)}#${index.toString()}`,
          nextHeaderHash: encoded[index + 1]?.hash ?? null,
          observedTransactionHash: "18".repeat(32),
          observedBlockHash: "19".repeat(32),
          observedSlot: (900 + index).toString(),
          observedBlockNo: (90 + index).toString(),
          observedChainPointId: "20".repeat(32),
          finalityDepth: "30",
        }),
      ),
    ),
    finalizedCorrectionLock: Object.freeze({
      outRef: `${"21".repeat(32)}#0`,
      datum: lockDatum,
      observedTransactionHash: "22".repeat(32),
      observedBlockHash: "23".repeat(32),
      observedSlot: "950",
      observedBlockNo: "95",
      observedChainPointId: "24".repeat(32),
      finalityDepth: "30",
    }),
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: "25".repeat(32),
  });
};

const decision = (
  headerHash: string,
  category: (typeof WATCHER_INSTALLED_WORKFLOW_CATEGORIES)[number],
  decisionDigest = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${headerHash}`,
) =>
  Object.freeze({
    schemaVersion: "midgard-production-header-decision-v1" as const,
    classifierVersion: "midgard-production-header-classifier-v1" as const,
    deploymentFingerprint: DEPLOYMENT,
    headerHash,
    authenticatedObservationDigest: OBSERVATION_DIGEST,
    payloadEnvelopeSha256: "26".repeat(32),
    payloadSha256: "27".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1" as const,
    replayDigest: "28".repeat(32),
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    launchScopeDigest: "29".repeat(32),
    classificationDigest: "2a".repeat(32),
    decisionDigest,
    decision: "fault_detected" as const,
    category,
    violationId: `${category}_v1`,
    detectionId: `${category}_v1:0`,
    position: "0",
  });

const harness = (input: {
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
  readonly pendingAvailabilityHeaders?: () => ReadonlySet<string>;
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
          maturityAtMs: "604800000",
          latestSafeStartAtMs: (
            302400000 + (input.deadlineOffset?.() ?? 0)
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

describe("production fault decision bridge", () => {
  it("forwards recovery authority without scanning workflow journals", async () => {
    const current = observation([headerFixture("01")]);
    const header = current.finalizedHeaders[0]!;
    const h = harness({
      current,
      categoryByHeader: { [header.headerHash]: "doubleSpend" },
    });
    await h.bridge.prepareForRecovery(current);
    expect(await h.bridge.recoverExisting()).toBe(1);
    expect(h.progressRequests).toHaveLength(1);
    expect(h.progressRequests[0]).toMatchObject({
      observation: current,
      rollbackGeneration: "1",
      fault: { decision: { headerHash: header.headerHash } },
    });
    h.bridge.invalidateForRollback();
    expect(h.authorityRevocations).toEqual(["native_chain_rollback"]);
  });

  it("forwards a healthy observation so the supervisor can reconcile historical objectives", async () => {
    const current = observation([]);
    const h = harness({ current, categoryByHeader: {} });
    await h.bridge.prepareForRecovery(current);
    await h.bridge.recoverExisting();
    expect(h.progressRequests).toHaveLength(1);
    expect(h.progressRequests[0]!.observation).toBe(current);
    expect(h.progressRequests[0]!.fault).toBeUndefined();
  });

  it("reuses only the live selected fault across hundreds of forward observations", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [healthy, faulty] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: {
        [healthy!.headerHash]: "doubleSpend",
        [faulty!.headerHash]: "transitionTrace",
      },
      pendingAvailabilityHeaders: () => new Set(),
      classifyOverride: (fresh) =>
        fresh.headerHash === healthy!.headerHash
          ? { ...fresh, decision: "healthy" }
          : fresh,
    });
    await h.bridge.prepareForRecovery(current);
    for (let i = 1; i <= 300; i += 1) {
      const next = {
        ...current,
        nativePoint: {
          ...current.nativePoint,
          blockNo: (100 + i).toString(),
          slot: (1000 + i).toString(),
        },
        observationDigest: i.toString(16).padStart(64, "0"),
      };
      h.admitted.add(next);
      h.bridge.beforeHistoryAdvance();
      await h.bridge.reconcileAndDispatch(next);
    }
    const calls = h.application.classifyHeader.mock.calls.map(
      ([input]) => input.observation.headerHash,
    );
    expect(calls.filter((hash) => hash === faulty!.headerHash)).toHaveLength(1);
    // Healthy classifications may consume moving settlement/event context.
    expect(calls.filter((hash) => hash === healthy!.headerHash)).toHaveLength(
      301,
    );
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.restrictions).toEqual([]);
    expect(h.revocations).toEqual([]);
    expect(h.retainedDecisionAuthorities.at(-1)).toBe(
      decision(faulty!.headerHash, "transitionTrace").decisionDigest,
    );
  });

  it.each([
    "source",
    "policy",
    "queue",
    "header",
    "healthy_header",
    "availability",
    "predecessor",
    "deadline",
    "lock",
    "configuration",
    "fork",
    "backward",
  ])(
    "reclassifies rather than reusing active fault after %s evidence changes",
    async (change) => {
      const current = observation([headerFixture("01"), headerFixture("02")]);
      const [healthy, faulty] = current.finalizedHeaders;
      let changed = false;
      const h = harness({
        current,
        categoryByHeader: {
          [healthy!.headerHash]: "doubleSpend",
          [faulty!.headerHash]: "transitionTrace",
        },
        pendingAvailabilityHeaders: () => new Set(),
        classifyOverride: (fresh) =>
          fresh.headerHash === healthy!.headerHash
            ? { ...fresh, decision: "healthy" }
            : fresh,
        deadlineOffset: () => (changed && change === "deadline" ? 1 : 0),
        classificationContextIdentity: async () =>
          changed && change === "configuration" ? "changed" : "original",
        resolvePredecessorOverride: async () =>
          changed && change === "predecessor"
            ? { ...healthy!, observedBlockHash: "ee".repeat(32) }
            : healthy,
      });
      await h.bridge.prepareForRecovery(current);
      changed = true;
      const next: WatcherAuthenticatedStateQueueObservation = {
        ...current,
        sourceId: change === "source" ? "changed-source" : current.sourceId,
        protocolScriptAuthorityDigest:
          change === "policy"
            ? "ee".repeat(32)
            : current.protocolScriptAuthorityDigest,
        observationDigest: "ef".repeat(32),
        nativePoint: {
          ...current.nativePoint,
          blockNo:
            change === "backward" ? "99" : change === "fork" ? "100" : "101",
          slot:
            change === "backward" ? "999" : change === "fork" ? "1000" : "1001",
          blockHash: "ee".repeat(32),
        },
        finalizedQueue:
          change === "queue"
            ? current.finalizedQueue.map((node, i) =>
                i === 0 ? { ...node, outRef: `${"ee".repeat(32)}#0` } : node,
              )
            : current.finalizedQueue,
        finalizedHeaders: current.finalizedHeaders.map((header, index) => {
          if (
            (change === "header" && index === 1) ||
            (change === "healthy_header" && index === 0)
          )
            return { ...header, observedBlockHash: "ee".repeat(32) };
          if (change === "availability" && index === 1)
            return {
              ...header,
              daAvailability: {
                Attested: { da_bond_asset_name: "ee".repeat(32) },
              },
            };
          return header;
        }),
        finalizedCorrectionLock:
          change === "lock"
            ? {
                ...current.finalizedCorrectionLock!,
                outRef: `${"ee".repeat(32)}#0`,
              }
            : current.finalizedCorrectionLock,
      };
      h.admitted.add(next);
      await h.bridge.prepareForRecovery(next);
      expect(
        h.application.classifyHeader.mock.calls.filter(
          ([input]) => input.observation.headerHash === faulty!.headerHash,
        ),
      ).toHaveLength(2);
    },
  );

  it.each(["configuration", "permit", "history"])(
    "rejects reuse when %s changes while a healthy sibling is classifying",
    async (change) => {
      const current = observation([headerFixture("01"), headerFixture("02")]);
      const [healthy, faulty] = current.finalizedHeaders;
      let resumed = false;
      let changed = false;
      let release!: () => void;
      const gate = new Promise<void>((resolve) => {
        release = resolve;
      });
      const h = harness({
        current,
        categoryByHeader: {
          [healthy!.headerHash]: "doubleSpend",
          [faulty!.headerHash]: "transitionTrace",
        },
        pendingAvailabilityHeaders: () => new Set(),
        classificationContextIdentity: async () =>
          changed && change === "configuration" ? "changed" : "original",
        permitAuthority: () =>
          changed && change === "permit" ? "reconciliation" : "submission",
        classifyOverride: async (fresh) => {
          if (fresh.headerHash === healthy!.headerHash) {
            if (resumed) await gate;
            return { ...fresh, decision: "healthy" };
          }
          return fresh;
        },
      });
      await h.bridge.prepareForRecovery(current);
      resumed = true;
      const pending = h.bridge.prepareForRecovery(current);
      await vi.waitFor(() =>
        expect(h.application.classifyHeader).toHaveBeenCalledTimes(3),
      );
      changed = true;
      if (change === "history") h.bridge.invalidateForHistoryChange();
      release();
      await expect(pending).rejects.toThrow(
        change === "configuration"
          ? "configuration changed"
          : change === "permit"
            ? "cannot replace or revive"
            : "authority changed",
      );
      expect(h.controllerGenerations).toEqual(["1"]);
      expect(
        h.application.classifyHeader.mock.calls.filter(
          ([input]) => input.observation.headerHash === faulty!.headerHash,
        ),
      ).toHaveLength(1);
    },
  );

  it("admits a newly detected earlier fault from fresh healthy-header replay", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [earlier, later] = current.finalizedHeaders;
    let changed = false;
    const h = harness({
      current,
      categoryByHeader: {
        [earlier!.headerHash]: "doubleSpend",
        [later!.headerHash]: "transitionTrace",
      },
      pendingAvailabilityHeaders: () => new Set(),
      classifyOverride: (fresh) =>
        !changed && fresh.headerHash === earlier!.headerHash
          ? { ...fresh, decision: "healthy" }
          : fresh,
    });
    await h.bridge.prepareForRecovery(current);
    changed = true;
    expect(
      (await h.bridge.prepareForRecovery(current)).target?.headerHash,
    ).toBe(earlier!.headerHash);
    expect(
      h.application.classifyHeader.mock.calls.filter(
        ([input]) => input.observation.headerHash === later!.headerHash,
      ),
    ).toHaveLength(1);
    expect(h.controllerGenerations).toEqual(["1", "2"]);
  });

  it("reclassifies an unprovable header when new context becomes available", async () => {
    const current = observation([headerFixture("01")]);
    let available = false;
    const h = harness({
      current,
      categoryByHeader: {
        [current.finalizedHeaders[0]!.headerHash]: "transitionTrace",
      },
      pendingAvailabilityHeaders: () => new Set(),
      classifyOverride: (fresh) =>
        available
          ? fresh
          : {
              ...fresh,
              decision: "unprovable",
              reason: "predecessor_context_unavailable",
            },
    });
    expect((await h.bridge.prepareForRecovery(current)).target).toBeNull();
    available = true;
    expect((await h.bridge.prepareForRecovery(current)).target).not.toBeNull();
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
  });

  it.each(["rollback", "history", "shutdown"])(
    "discards classification reuse on %s authority loss",
    async (reason) => {
      const current = observation([headerFixture("01")]);
      const h = harness({
        current,
        categoryByHeader: {
          [current.finalizedHeaders[0]!.headerHash]: "transitionTrace",
        },
        pendingAvailabilityHeaders: () => new Set(),
      });
      await h.bridge.prepareForRecovery(current);
      if (reason === "rollback") h.bridge.invalidateForRollback();
      else if (reason === "history") h.bridge.invalidateForHistoryChange();
      else h.bridge.invalidateForShutdown();
      await h.bridge.prepareForRecovery(current);
      expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
      expect(h.controllerGenerations).toEqual(["1", "3"]);
    },
  );

  it("does not revive a restricted active permit through classification reuse", async () => {
    const current = observation([headerFixture("01")]);
    const h = harness({
      current,
      categoryByHeader: {
        [current.finalizedHeaders[0]!.headerHash]: "transitionTrace",
      },
      pendingAvailabilityHeaders: () => new Set(),
      permitAuthority: "reconciliation",
    });
    await h.bridge.prepareForRecovery(current);
    await expect(h.bridge.prepareForRecovery(current)).rejects.toThrow(
      "cannot replace or revive",
    );
    expect(h.application.classifyHeader).toHaveBeenCalledOnce();
    expect(h.controllerGenerations).toEqual(["1"]);
  });

  it("does not reuse a selected validation transcript whose history capture needs refresh", async () => {
    const current = observation([headerFixture("01")]);
    const h = harness({
      current,
      categoryByHeader: {
        [current.finalizedHeaders[0]!.headerHash]: "validationTraceDispute",
      },
      pendingAvailabilityHeaders: () => new Set(),
      decisionUsesLocalEventHistory: true,
    });
    await h.bridge.prepareForRecovery(current);
    await h.bridge.prepareForRecovery(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
  });

  it.each([
    { wallAfter: 90_000n, fails: false },
    { wallAfter: 900_000n, fails: false },
    { wallAfter: 90_000n, fails: true },
    { wallAfter: 900_000n, fails: true },
  ])(
    "preserves classification and its original error across wall movement to $wallAfter, failure=$fails",
    async ({ wallAfter, fails }) => {
      let wall = 100_000n;
      let monotonic = 100;
      const originalError = new Error("original classifier failure");
      const observability = createWatcherOperationsObservability({
        deploymentFingerprint: DEPLOYMENT,
        supervisor: {
          status: () => ({
            phase: "accepting",
            recovered: true,
            queuedJobCount: 0,
            activeJob: null,
            blockedJob: null,
            deadlineHealth: "safe",
            earliestDeadlineJob: null,
            remainingSafeStartMs: "1000",
          }),
        } as unknown as WatcherFaultProofSupervisor,
        launchScopeStatus: () => ({
          installedCategoryCount: 54,
          requiredCategoryCount: 54,
        }),
        durableProofQueueStatus: () => ({
          queuedJobCount: 0,
          oldestQueuedAtMs: null,
        }),
        nowMs: () => wall,
        monotonicNowMs: () => monotonic,
      });
      const current = observation([headerFixture("01")]);
      const first = current.finalizedHeaders[0]!;
      const subject = harness({
        current,
        categoryByHeader: { [first.headerHash]: "doubleSpend" },
        operationsSink: observability.sink,
        nowMs: () => wall,
        monotonicNowMs: () => monotonic,
        classifyOverride: (result) => {
          wall = wallAfter;
          monotonic += 2128.43;
          if (fails) throw originalError;
          return result;
        },
      });
      if (fails)
        await expect(subject.bridge.reconcileAndDispatch(current)).rejects.toBe(
          originalError,
        );
      else
        expect(
          (await subject.bridge.reconcileAndDispatch(current)).target
            ?.headerHash,
        ).toBe(first.headerHash);
      expect(
        observability.api.diagnostics({ kind: "verification" }).records,
      ).toEqual([
        expect.objectContaining({
          startedAtMs: "100000",
          completedAtMs: wallAfter.toString(),
          elapsedMs: "2129",
          outcome: fails ? "failed" : "fault_detected",
        }),
      ]);
      expect(observability.api.metrics().verificationLatencyMs).toMatchObject({
        sampleCount: "1",
        maximum: "2129",
      });
    },
  );

  it("defers a changing raw checkpoint and its suffix until a quiet canonical wake succeeds", async () => {
    const current = observation([
      headerFixture("21"),
      headerFixture("22"),
      headerFixture("23"),
    ]);
    const [healthy, pending, suffix] = current.finalizedHeaders;
    let changed = true;
    const h = harness({
      current,
      categoryByHeader: {
        [healthy!.headerHash]: "doubleSpend",
        [pending!.headerHash]: "invalidRange",
        [suffix!.headerHash]: "transitionTrace",
      },
      classifyOverride: async (fresh) => {
        if (fresh.headerHash === pending!.headerHash && changed)
          throw new LocalKupmiosCheckpointChangedError(
            "event capture checkpoint changed",
          );
        return fresh.headerHash === healthy!.headerHash
          ? { ...fresh, decision: "healthy" }
          : fresh;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target).toBeNull();
    expect(h.appended.map((entry) => entry.headerHash)).toEqual([
      healthy!.headerHash,
    ]);
    expect(h.enqueued).toEqual([]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(3);
    await h.bridge.retryDeferredClassification(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(6);
    expect(h.enqueued).toEqual([]);
    changed = false;
    await h.bridge.retryDeferredClassification(current);
    expect(h.enqueued.map((entry) => entry.headerHash)).toEqual([
      pending!.headerHash,
    ]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(9);
    await h.bridge.retryDeferredClassification(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(9);
  });

  it("keeps an earlier fault permit and deadline unchanged while later checkpoint drift waits for canonical wakes", async () => {
    const current = observation([headerFixture("25"), headerFixture("26")]);
    const [first, pending] = current.finalizedHeaders;
    let changed = false;
    const h = harness({
      current,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [pending!.headerHash]: "invalidRange",
      },
      classifyOverride: async (fresh) => {
        if (fresh.headerHash === pending!.headerHash && changed)
          throw new LocalKupmiosCheckpointChangedError(
            "event capture checkpoint changed",
          );
        return fresh;
      },
    });
    await h.bridge.reconcileAndDispatch(current);
    const selected = h.progressRequests[0]!.fault!;
    changed = true;
    await h.bridge.reconcileAndDispatch(current);
    for (let wake = 0; wake < 3; wake++)
      await h.bridge.retryDeferredClassification(current);
    expect(
      h.application.classifyHeader.mock.calls.filter(
        ([request]) => request.observation.headerHash === pending!.headerHash,
      ),
    ).toHaveLength(5);
    expect(h.progressRequests.at(-1)!.fault!.actuationPermit).toBe(
      selected.actuationPermit,
    );
    expect(h.progressRequests.at(-1)!.fault!.deadline).toEqual(
      selected.deadline,
    );
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.restrictions).toEqual([]);
    expect(h.revocations).toEqual([]);
  });

  it.each([
    new Error("Transition replay event NFT coverage changed or is ambiguous"),
    Object.assign(new Error("forged checkpoint label"), {
      name: "LocalKupmiosCheckpointChangedError",
    }),
  ])(
    "keeps malformed event/authentication failures hard: %s",
    async (failure) => {
      const current = observation([headerFixture("24")]);
      const h = harness({
        current,
        categoryByHeader: {
          [current.finalizedHeaders[0]!.headerHash]: "doubleSpend",
        },
        classifyOverride: async () => {
          throw failure;
        },
      });
      await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(
        failure,
      );
      expect(h.appended).toEqual([]);
      expect(h.enqueued).toEqual([]);
    },
  );

  it("retries deferred public DA on a quiet-block signal and stops retrying after classification", async () => {
    const original = observation([headerFixture("01")]);
    const first = original.finalizedHeaders[0]!;
    const current: WatcherAuthenticatedStateQueueObservation = {
      ...original,
      finalizedHeaders: [
        {
          ...first,
          daAvailability: { Attested: { da_bond_asset_name: "44".repeat(32) } },
        },
      ],
    };
    let pending = new Set([first.headerHash]);
    const currentHarness = harness({
      current,
      categoryByHeader: { [first.headerHash]: "doubleSpend" },
      pendingAvailabilityHeaders: () => pending,
    });
    const waiting = await currentHarness.bridge.reconcileAndDispatch(current);
    expect(waiting.target).toBeNull();
    expect(waiting.decisionDigests).toEqual([]);
    expect(currentHarness.application.classifyHeader).not.toHaveBeenCalled();
    expect(currentHarness.appended).toEqual([]);
    expect(currentHarness.enqueued).toEqual([]);
    await currentHarness.bridge.retryDeferredClassification(current);
    expect(currentHarness.application.classifyHeader).not.toHaveBeenCalled();
    pending = new Set();
    await currentHarness.bridge.retryDeferredClassification(current);
    expect(currentHarness.bridge.status().target?.headerHash).toBe(
      first.headerHash,
    );
    expect(currentHarness.application.classifyHeader).toHaveBeenCalledOnce();
    expect(currentHarness.enqueued).toHaveLength(1);
    await currentHarness.bridge.retryDeferredClassification(current);
    expect(currentHarness.application.classifyHeader).toHaveBeenCalledOnce();
    expect(currentHarness.enqueued).toHaveLength(1);
  });

  it("preserves the admitted target and replay authority through repeated availability rechecks", async () => {
    const initial = observation([headerFixture("01")]);
    const header = initial.finalizedHeaders[0]!;
    const current: WatcherAuthenticatedStateQueueObservation = {
      ...initial,
      finalizedHeaders: [
        {
          ...header,
          daAvailability: { Attested: { da_bond_asset_name: "44".repeat(32) } },
        },
      ],
    };
    let pending = new Set<string>();
    const h = harness({
      current,
      categoryByHeader: { [header.headerHash]: "transitionTrace" },
      pendingAvailabilityHeaders: () => pending,
      decisionUsesLocalEventHistory: true,
    });
    const first = await h.bridge.prepareForRecovery(current);
    await h.bridge.recoverExisting();
    pending = new Set([header.headerHash]);
    let previous = current;
    for (let index = 1; index <= 45; index += 1) {
      const next = {
        ...current,
        observationDigest: index.toString(16).padStart(64, "0"),
        previousObservationDigest: previous.observationDigest,
        nativePoint: {
          ...current.nativePoint,
          blockNo: (100 + index).toString(),
          slot: (1000 + index).toString(),
          blockHash: index.toString(16).padStart(64, "0"),
          parentBlockHash: previous.nativePoint.blockHash,
        },
      };
      h.admitted.add(next);
      // Actual follower order: fence history, complete availability recheck, dispatch queue.
      h.bridge.beforeHistoryAdvance();
      const result = await h.bridge.reconcileAndDispatch(next);
      expect(result.target).toEqual(first.target);
      expect(h.retainedDecisionAuthorities.at(-1)).toBe(
        first.target!.decisionDigest,
      );
      previous = next;
    }
    expect(h.application.classifyHeader).toHaveBeenCalledOnce();
    expect(h.restrictions).toEqual([]);
    expect(h.revocations).toEqual([]);
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(new Set(h.enqueuedGenerations)).toEqual(new Set(["1"]));
    pending = new Set();
    await h.bridge.reconcileAndDispatch(previous);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
    expect(h.controllerGenerations).toEqual(["1"]);
    h.bridge.invalidateForRollback();
    expect(h.revocations).toEqual(["native_chain_rollback"]);
  });

  it.each([
    "queue_out_ref",
    "source",
    "native_regression",
    "availability_state",
    "native_fork",
    "header_seal",
    "deadline",
    "correction_lock",
  ])(
    "does not preserve pending target authority after %s changes",
    async (change) => {
      const original = observation([headerFixture("01")]);
      const header = original.finalizedHeaders[0]!;
      const current: WatcherAuthenticatedStateQueueObservation = {
        ...original,
        finalizedHeaders: [
          {
            ...header,
            daAvailability: {
              Attested: { da_bond_asset_name: "44".repeat(32) },
            },
          },
        ],
      };
      let pending = new Set<string>();
      const h = harness({
        current,
        categoryByHeader: { [header.headerHash]: "transitionTrace" },
        pendingAvailabilityHeaders: () => pending,
        deadlineOffset: () =>
          change === "deadline" && pending.size > 0 ? 1 : 0,
      });
      await h.bridge.reconcileAndDispatch(current);
      pending = new Set([header.headerHash]);
      const changed: WatcherAuthenticatedStateQueueObservation = {
        ...current,
        observationDigest: "aa".repeat(32),
        sourceId:
          change === "source" ? "different-native-source" : current.sourceId,
        finalizedQueue: current.finalizedQueue.map((node) =>
          change === "queue_out_ref" && node.headerHash === header.headerHash
            ? { ...node, outRef: `${"ee".repeat(32)}#0` }
            : node,
        ),
        nativePoint: {
          ...current.nativePoint,
          blockNo:
            change === "native_regression"
              ? "99"
              : change === "native_fork"
                ? "100"
                : "101",
          slot:
            change === "native_regression"
              ? "999"
              : change === "native_fork"
                ? "1000"
                : "1001",
          blockHash:
            change === "native_fork"
              ? "ff".repeat(32)
              : current.nativePoint.blockHash,
        },
        finalizedCorrectionLock:
          change === "correction_lock"
            ? {
                ...current.finalizedCorrectionLock!,
                outRef: `${"ee".repeat(32)}#1`,
              }
            : current.finalizedCorrectionLock,
        finalizedHeaders: [
          {
            ...current.finalizedHeaders[0]!,
            ...(change === "header_seal"
              ? { observedBlockHash: "ff".repeat(32) }
              : {}),
            ...(change === "queue_out_ref"
              ? { queueOutRef: `${"ee".repeat(32)}#0` }
              : {}),
            ...(change === "availability_state"
              ? {
                  daAvailability: {
                    Attested: { da_bond_asset_name: "55".repeat(32) },
                  },
                }
              : {}),
          },
        ],
      };
      h.admitted.add(changed);
      h.bridge.beforeHistoryAdvance();
      expect((await h.bridge.reconcileAndDispatch(changed)).target).toBeNull();
      expect(h.restrictions).toEqual([]);
      expect(h.application.classifyHeader).toHaveBeenCalledOnce();
    },
  );

  it("never revives a restricted permit and settles other classifiers before rejecting", async () => {
    const headers = [headerFixture("01"), headerFixture("02")];
    const header = encodedHeader(headers[1]!);
    const initial = observation(headers, {
      Locked: {
        target_header_hash: header.hash,
        correction_identity: {
          FraudProof: {
            fraud_proof_asset_name: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.transitionTrace}${header.hash}`,
          },
        },
      },
    });
    const current: WatcherAuthenticatedStateQueueObservation = {
      ...initial,
      finalizedHeaders: initial.finalizedHeaders.map((row) => ({
        ...row,
        daAvailability: { Attested: { da_bond_asset_name: "44".repeat(32) } },
      })),
    };
    let pending = new Set<string>();
    let waitForFirst = false;
    let release!: () => void;
    const waiting = new Promise<void>((resolve) => {
      release = resolve;
    });
    const h = harness({
      current,
      categoryByHeader: Object.fromEntries(
        current.finalizedHeaders.map((row) => [
          row.headerHash,
          "transitionTrace",
        ]),
      ),
      pendingAvailabilityHeaders: () => pending,
      permitAuthority: "reconciliation",
      classifyOverride: async (value) => {
        if (waitForFirst && value.headerHash !== header.hash) await waiting;
        return value;
      },
    });
    await h.bridge.prepareForRecovery(current);
    pending = new Set([header.hash]);
    waitForFirst = true;
    let finished = false;
    const preparing = h.bridge.prepareForRecovery(current).finally(() => {
      finished = true;
    });
    await vi.waitFor(() =>
      expect(h.application.classifyHeader).toHaveBeenCalledTimes(3),
    );
    expect(finished).toBe(false);
    release();
    await expect(preparing).rejects.toThrow("cannot replace or revive");
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.enqueued).toEqual([]);
  });

  it("rejects a pending-availability claim for an unattested header", async () => {
    const current = observation([headerFixture("01")]);
    const first = current.finalizedHeaders[0]!;
    const currentHarness = harness({
      current,
      categoryByHeader: {},
      pendingAvailabilityHeaders: () => new Set([first.headerHash]),
    });
    await expect(
      currentHarness.bridge.reconcileAndDispatch(current),
    ).rejects.toThrow("authenticated challengeable header");
  });

  it("journals every header but dispatches only the first canonical Idle target", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [first, second] = current.finalizedHeaders;
    const currentHarness = harness({
      current,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "invalidRange",
      },
    });
    const prepared = await currentHarness.bridge.prepareForRecovery(current);
    expect(prepared.decisionDigests).toHaveLength(2);
    expect(prepared.target).toMatchObject({
      category: "doubleSpend",
      headerHash: first!.headerHash,
    });
    expect(currentHarness.appended).toHaveLength(2);
    expect(currentHarness.enqueued).toHaveLength(0);
    expect(currentHarness.retainedDecisionAuthorities).toEqual([
      prepared.target!.decisionDigest,
    ]);

    await currentHarness.bridge.dispatchPrepared();
    expect(currentHarness.enqueued.map(({ category }) => category)).toEqual([
      "doubleSpend",
    ]);
    expect(currentHarness.bridge.status().target?.decisionDigest).toBe(
      currentHarness.enqueued[0]!.decisionDigest,
    );

    currentHarness.bridge.invalidateForRollback();
    expect(currentHarness.retainedDecisionAuthorities.at(-1)).toBeNull();
    expect(currentHarness.bridge.dispatchPrepared()).toBeNull();
  });

  it("defers headers whose predecessor attestation is not yet release-final", async () => {
    const current = observation([
      headerFixture("01"),
      headerFixture("02"),
      headerFixture("03"),
    ]);
    const [first, second, third] = current.finalizedHeaders;
    const currentHarness = harness({
      current,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "invalidRange",
        [third!.headerHash]: "invalidRange",
      },
      resolvePredecessorOverride: async (header) => {
        if (header.headerHash === second!.headerHash) {
          throw new WatcherRetainedHeaderAttestationPendingError(
            first!.headerHash,
          );
        }
        return undefined;
      },
    });
    const prepared = await currentHarness.bridge.prepareForRecovery(current);
    expect(prepared.decisionDigests).toHaveLength(1);
    expect(prepared.target).toMatchObject({
      category: "doubleSpend",
      headerHash: first!.headerHash,
    });
    expect(currentHarness.appended.map(({ headerHash }) => headerHash)).toEqual(
      [first!.headerHash],
    );
  });

  it("produces no target while the first header's predecessor attestation is pending", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [first, second] = current.finalizedHeaders;
    const currentHarness = harness({
      current,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "invalidRange",
      },
      resolvePredecessorOverride: async (header) => {
        if (header.headerHash === first!.headerHash) {
          throw new WatcherRetainedHeaderAttestationPendingError(
            "ab".repeat(32),
          );
        }
        return undefined;
      },
    });
    const prepared = await currentHarness.bridge.prepareForRecovery(current);
    expect(prepared.decisionDigests).toHaveLength(0);
    expect(prepared.target).toBeNull();
    expect(currentHarness.appended).toHaveLength(0);
    expect(currentHarness.enqueued).toHaveLength(0);
  });

  it("resumes only the exact category-bound FraudProof lock target", async () => {
    const base = observation([headerFixture("03"), headerFixture("04")]);
    const [first, second] = base.finalizedHeaders;
    const locked = observation([headerFixture("03"), headerFixture("04")], {
      Locked: {
        target_header_hash: second!.headerHash,
        correction_identity: {
          FraudProof: {
            fraud_proof_asset_name: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.invalidRange}${second!.headerHash}`,
          },
        },
      },
    });
    const currentHarness = harness({
      current: locked,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "invalidRange",
      },
    });
    const result = await currentHarness.bridge.reconcileAndDispatch(locked);
    expect(result.target).toMatchObject({
      category: "invalidRange",
      headerHash: second!.headerHash,
    });
    expect(currentHarness.enqueued.map(({ category }) => category)).toEqual([
      "invalidRange",
    ]);

    const substituted = observation([headerFixture("03")], {
      Locked: {
        target_header_hash: first!.headerHash,
        correction_identity: {
          FraudProof: {
            fraud_proof_asset_name: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.invalidRange}${first!.headerHash}`,
          },
        },
      },
    });
    const hostile = harness({
      current: substituted,
      categoryByHeader: { [first!.headerHash]: "doubleSpend" },
    });
    await expect(
      hostile.bridge.prepareForRecovery(substituted),
    ).rejects.toThrow("did not reproduce an exact runnable classification");

    const availability = observation([headerFixture("03")], {
      Locked: {
        target_header_hash: first!.headerHash,
        correction_identity: {
          AvailabilityChallenge: { challenge_asset_name: "aa" },
        },
      },
    });
    const held = harness({
      current: availability,
      categoryByHeader: { [first!.headerHash]: "doubleSpend" },
    });
    expect((await held.bridge.reconcileAndDispatch(availability)).target).toBe(
      null,
    );
    expect(held.enqueued).toHaveLength(0);
  });

  it("rejects durable/fresh disagreement, structural observations, and classifier substitution", async () => {
    const current = observation([headerFixture("05")]);
    const [header] = current.finalizedHeaders;
    const fresh = decision(header!.headerHash, "doubleSpend");
    const currentHarness = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      records: [
        Object.freeze({
          schemaVersion: "midgard-watcher-production-fault-decision-record-v1",
          revision: "0",
          priorRecordSha256: null,
          decision: { ...fresh, decisionDigest: "ff".repeat(32) },
        }),
      ],
    });
    await expect(
      currentHarness.bridge.prepareForRecovery(current),
    ).rejects.toThrow("differs from durable decision evidence");

    await expect(
      currentHarness.bridge.prepareForRecovery({ ...current }),
    ).rejects.toThrow("not admitted");

    const substituted = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      classifyOverride: (value) => ({
        ...value,
        headerHash: "ee".repeat(28),
      }),
    });
    await expect(
      substituted.bridge.prepareForRecovery(current),
    ).rejects.toThrow("changed the authenticated queue identity");
  });

  it("fails closed when a classifier rejects with an undefined value", async () => {
    const current = observation([headerFixture("0a")]);
    const [header] = current.finalizedHeaders;
    const currentHarness = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      classifyOverride: async () => {
        throw undefined;
      },
    });

    await expect(
      currentHarness.bridge.prepareForRecovery(current),
    ).rejects.toBeUndefined();
    expect(currentHarness.appended).toHaveLength(0);
    expect(currentHarness.enqueued).toHaveLength(0);
  });

  it("does not enter classification after rollback during predecessor resolution", async () => {
    const current = observation([headerFixture("06")]);
    const header = current.finalizedHeaders[0]!;
    let release!: () => void;
    const waiting = new Promise<void>((resolve) => {
      release = resolve;
    });
    const resolvePredecessor = vi.fn(async () => {
      await waiting;
      return undefined;
    });
    const currentHarness = harness({
      current,
      categoryByHeader: { [header.headerHash]: "doubleSpend" },
      resolvePredecessorOverride: resolvePredecessor,
    });
    const preparing = currentHarness.bridge.prepareForRecovery(current);
    await vi.waitFor(() => expect(resolvePredecessor).toHaveBeenCalledOnce());
    expect(currentHarness.application.classifyHeader).not.toHaveBeenCalled();
    currentHarness.bridge.invalidateForRollback();
    release();
    await expect(preparing).rejects.toThrow(
      "authority changed before fault classification",
    );
    expect(currentHarness.application.classifyHeader).not.toHaveBeenCalled();
    expect(currentHarness.appended).toHaveLength(0);
    expect(currentHarness.enqueued).toHaveLength(0);
    expect(currentHarness.retainedDecisionAuthorities).toEqual([null, null]);
  });

  it.each([
    "invalidateForRollback",
    "invalidateForHistoryChange",
    "beforeHistoryAdvance",
  ] as const)(
    "%s retires authority during an awaited classification",
    async (invalidate) => {
      const current = observation([headerFixture("06")]);
      const [header] = current.finalizedHeaders;
      let release!: (value: ReturnType<typeof decision>) => void;
      const waiting = new Promise<ReturnType<typeof decision>>((resolve) => {
        release = resolve;
      });
      const currentHarness = harness({
        current,
        categoryByHeader: { [header!.headerHash]: "doubleSpend" },
        classifyOverride: async () => await waiting,
      });
      const preparing = currentHarness.bridge.prepareForRecovery(current);
      await vi.waitFor(() =>
        expect(
          currentHarness.application.classifyHeader,
        ).toHaveBeenCalledOnce(),
      );
      currentHarness.bridge[invalidate]();
      release(decision(header!.headerHash, "doubleSpend"));
      await expect(preparing).rejects.toThrow(
        "authority changed during fault classification",
      );
      expect(currentHarness.enqueued).toHaveLength(0);
      expect(currentHarness.retainedDecisionAuthorities).toEqual([null, null]);
    },
  );

  it.each([false, true])(
    "canonical queue removal preserves the active invocation until explicit authority loss (%s)",
    async (usesLocalEventHistory) => {
      const current = observation([headerFixture("0d")]);
      const header = current.finalizedHeaders[0]!;
      const currentHarness = harness({
        current,
        categoryByHeader: { [header.headerHash]: "transitionTrace" },
        decisionUsesLocalEventHistory: usesLocalEventHistory,
      });
      await currentHarness.bridge.reconcileAndDispatch(current);
      const activePermit =
        currentHarness.progressRequests[0]!.fault!.actuationPermit;
      currentHarness.bridge.beforeHistoryAdvance();
      expect(currentHarness.revocations).toEqual([]);
      const removed = Object.freeze({
        ...observation([]),
        observationDigest: "55".repeat(32),
      });
      currentHarness.admitted.add(removed);
      await currentHarness.bridge.reconcileAndDispatch(removed);
      expect(currentHarness.restrictions).toEqual([]);
      expect(currentHarness.revocations).toEqual([]);
      expect(currentHarness.progressRequests[0]!.fault!.actuationPermit).toBe(
        activePermit,
      );
      await currentHarness.bridge.dispatchPrepared();
      expect(currentHarness.progressRequests.at(-1)).toMatchObject({
        observation: removed,
      });
      expect(currentHarness.progressRequests.at(-1)!.fault).toBeUndefined();
      currentHarness.bridge.invalidateForRollback();
      expect(currentHarness.authorityRevocations).toEqual([
        "native_chain_rollback",
      ]);
    },
  );

  it.each([
    { invalidate: "invalidateForRollback", reason: "native_chain_rollback" },
    {
      invalidate: "invalidateForHistoryChange",
      reason: "local_event_history_change",
    },
  ] as const)(
    "preserves an exact target across unrelated observations, then revokes for $reason",
    async ({ invalidate, reason }) => {
      const current = observation([headerFixture("0d")]);
      const header = current.finalizedHeaders[0]!;
      const later = Object.freeze({
        ...current,
        nativePoint: Object.freeze({
          ...current.nativePoint,
          blockHash: "31".repeat(32),
          parentBlockHash: current.nativePoint.blockHash,
          slot: "1001",
          blockNo: "101",
          chainPointId: "32".repeat(32),
        }),
        previousObservationDigest: current.observationDigest,
        observationDigest: "33".repeat(32),
      });
      const currentHarness = harness({
        current,
        categoryByHeader: { [header.headerHash]: "doubleSpend" },
      });
      currentHarness.admitted.add(later);

      await currentHarness.bridge.reconcileAndDispatch(current);
      expect(currentHarness.controllerGenerations).toEqual(["1"]);
      expect(currentHarness.enqueuedGenerations).toEqual(["1"]);

      await currentHarness.bridge.prepareForRecovery(later);
      expect(currentHarness.controllerGenerations).toEqual(["1"]);
      expect(currentHarness.revocations).toEqual([]);
      expect(currentHarness.bridge.status().target?.decisionDigest).toBe(
        currentHarness.enqueued[0]!.decisionDigest,
      );

      currentHarness.bridge[invalidate]();
      expect(currentHarness.revocations).toEqual([reason]);
    },
  );

  it("surfaces enqueue failure instead of reporting a reconciled dispatch", async () => {
    const current = observation([headerFixture("07")]);
    const [header] = current.finalizedHeaders;
    const currentHarness = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      enqueueError: new Error("supervisor rejected durable job"),
    });
    await expect(
      currentHarness.bridge.reconcileAndDispatch(current),
    ).rejects.toThrow("supervisor rejected durable job");
  });

  it("dispatches the exact serialized decision when a later prepare is queued", async () => {
    const firstObservation = observation([headerFixture("0b")]);
    const secondObservation = Object.freeze({
      ...observation([headerFixture("0c")]),
      observationDigest: "34".repeat(32),
    });
    const firstHeader = firstObservation.finalizedHeaders[0]!;
    const secondHeader = secondObservation.finalizedHeaders[0]!;
    let releaseFirst!: () => void;
    const firstGate = new Promise<void>((resolve) => {
      releaseFirst = resolve;
    });
    let releaseSecond!: () => void;
    const secondGate = new Promise<void>((resolve) => {
      releaseSecond = resolve;
    });
    const currentHarness = harness({
      current: firstObservation,
      categoryByHeader: {
        [firstHeader.headerHash]: "doubleSpend",
        [secondHeader.headerHash]: "invalidRange",
      },
      classifyOverride: async (fresh) => {
        if (fresh.headerHash === firstHeader.headerHash) await firstGate;
        else await secondGate;
        return fresh;
      },
    });
    currentHarness.admitted.add(secondObservation);

    const first = currentHarness.bridge.reconcileAndDispatch(firstObservation);
    await vi.waitFor(() =>
      expect(currentHarness.application.classifyHeader).toHaveBeenCalledTimes(
        1,
      ),
    );
    const second = currentHarness.bridge.prepareForRecovery(secondObservation);
    releaseFirst();
    await expect(first).resolves.toMatchObject({
      target: { headerHash: firstHeader.headerHash },
    });
    expect(currentHarness.enqueued.map(({ headerHash }) => headerHash)).toEqual(
      [firstHeader.headerHash],
    );
    releaseSecond();
    await expect(second).resolves.toMatchObject({
      target: { headerHash: secondHeader.headerHash },
    });
    expect(currentHarness.retainedDecisionAuthorities).toEqual([
      decision(firstHeader.headerHash, "doubleSpend").decisionDigest,
      decision(secondHeader.headerHash, "invalidRange").decisionDigest,
    ]);
  });

  it("bounds concurrent classification while preserving finalized queue order", async () => {
    const headers = Array.from({ length: 20 }, (_, index) =>
      headerFixture((index + 16).toString(16)),
    );
    const current = observation(headers);
    let active = 0;
    let maximumActive = 0;
    const currentHarness = harness({
      current,
      categoryByHeader: Object.fromEntries(
        current.finalizedHeaders.map(({ headerHash }) => [
          headerHash,
          "doubleSpend",
        ]),
      ),
      classifyOverride: async (fresh) => {
        active += 1;
        maximumActive = Math.max(maximumActive, active);
        await new Promise((resolve) => setTimeout(resolve, 2));
        active -= 1;
        return fresh;
      },
    });
    await currentHarness.bridge.reconcileAndDispatch(current);
    expect(maximumActive).toBe(2);
    expect(currentHarness.appended.map(({ headerHash }) => headerHash)).toEqual(
      current.finalizedHeaders.map(({ headerHash }) => headerHash),
    );
  });
});

const retainedRunDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
it.skipIf(retainedRunDirectory === undefined).each(["pending", "forward"])(
  "retains the exact admitted target through %s follower hooks restored from retained queue snapshots",
  async (mode) => {
    const config = JSON.parse(
      await readFile(
        join(
          retainedRunDirectory!,
          "work/journeys/transition-trace/watcher-process.json",
        ),
        "utf8",
      ),
    );
    const database = new DatabaseSync(config.watcherConfig.storage.path, {
      readOnly: true,
    });
    let snapshots: WatcherAuthenticatedStateQueueObservation[];
    try {
      snapshots = database
        .prepare(
          "SELECT canonical_json FROM watcher_state_queue_observation_v1 ORDER BY sequence",
        )
        .all()
        .map((row) => {
          if (typeof row.canonical_json !== "string")
            throw new Error("missing retained queue snapshot");
          return JSON.parse(
            row.canonical_json,
          ) as WatcherAuthenticatedStateQueueObservation;
        });
    } finally {
      database.close();
    }
    const current = snapshots.at(-1);
    if (current === undefined)
      throw new Error("retained replay requires queue snapshots");
    const directory = join(config.workflowJournalDirectory, "fault-decisions");
    const records: WatcherPersistedFaultDecisionRecord[] = await Promise.all(
      (await readdir(directory))
        .filter((file) => file.endsWith(".json"))
        .sort()
        .map(
          async (file) =>
            JSON.parse(
              await readFile(join(directory, file), "utf8"),
            ) as WatcherPersistedFaultDecisionRecord,
        ),
    );
    const byHeader = new Map(
      records.map((record) => [record.decision.headerHash, record.decision]),
    );
    let pending = new Set<string>();
    let latest = current;
    const h = harness({
      current,
      categoryByHeader: Object.fromEntries(
        current.finalizedHeaders.map((row) => [
          row.headerHash,
          "transitionTrace",
        ]),
      ),
      observationDigestOverride: authenticatedStateQueueObservationDigest,
      pendingAvailabilityHeaders: () => pending,
      classifyOverride: (value) => {
        const found = byHeader.get(value.headerHash);
        if (found === undefined) throw new Error("missing retained decision");
        return found;
      },
      decisionUsesLocalEventHistory: mode === "pending",
    });
    const first = await h.bridge.prepareForRecovery(current);
    if (first.target === null)
      throw new Error("retained run has no admitted fault");
    await h.bridge.recoverExisting();
    const point = current.nativePoint;
    const restore = vi.fn(async () => ({
      previous: current,
      discardedObservationCount: 0,
      replayIntersection: point,
      catchupBoundary: { ...point, ogmiosTipBlockNo: point.blockNo },
    }));
    const append = vi.fn(async () => "appended" as const);
    const runtime = await createWatcherStateQueueRuntime({
      store: {
        readAll: async () => snapshots,
        append,
        rollbackTo: async () => undefined,
      },
      source: {
        restore,
        bootstrap: async () => {
          throw new Error("must restore retained snapshots");
        },
        observe: async () => current,
        latestFinalizedObservation: () => latest,
        resolveRetainedHeader: async () => {
          throw new Error("no new classification required");
        },
      },
    });
    const reconcileAvailability = vi.fn(async () => {
      pending =
        mode === "pending"
          ? new Set([first.target!.headerHash])
          : new Set<string>();
    });
    const hooks = runtime.bindFaultDecisionBridge(h.bridge, {
      reconcile: reconcileAvailability,
      invalidateForRollback: () => {
        pending = new Set();
      },
    });
    const callbacks = mode === "pending" ? 3 : 300;
    for (let index = 0; index < callbacks; index += 1) {
      if (mode === "forward") {
        latest = {
          ...current,
          observationDigest: index.toString(16).padStart(64, "0"),
          nativePoint: {
            ...point,
            blockNo: (BigInt(point.blockNo) + BigInt(index + 1)).toString(),
            slot: (BigInt(point.slot) + BigInt(index + 1)).toString(),
          },
        };
        h.admitted.add(latest);
      }
      h.bridge.beforeHistoryAdvance();
      await Promise.resolve();
      await hooks.onFinalized!({
        nativeBlock: {} as never,
        localObservation: {} as never,
        relevance: "touched",
      });
    }
    expect(restore).toHaveBeenCalledWith({ persistedObservations: snapshots });
    expect(append).not.toHaveBeenCalled();
    expect(reconcileAvailability).toHaveBeenCalledTimes(callbacks);
    if (mode === "forward") {
      expect(
        h.application.classifyHeader.mock.calls.filter(
          ([input]) =>
            input.observation.headerHash === first.target!.headerHash,
        ),
      ).toHaveLength(1);
      expect(h.application.classifyHeader).toHaveBeenCalledTimes(302);
    }
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.restrictions).toEqual([]);
    expect(h.bridge.status().target).toEqual(first.target);
    expect(h.retainedDecisionAuthorities.at(-1)).toBe(
      first.target.decisionDigest,
    );
    h.bridge.invalidateForRollback();
    expect(h.revocations).toEqual(["native_chain_rollback"]);
  },
);
