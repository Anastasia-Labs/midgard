import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  type CorrectionLockDatum,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultDecisionBridgeForTest } from "../../src/fault-proofs/production-fault-decision-bridge-v1.js";
import type { WatcherPersistedFaultDecisionRecord } from "../../src/fault-proofs/production-fault-decision-journal-v1.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/production-fault-proof-application-v1.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/production-state-queue-observation-v1.js";

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
  ) => ReturnType<typeof decision> | Promise<ReturnType<typeof decision>>;
  readonly enqueueError?: Error;
}) => {
  const admitted = new WeakSet<object>([input.current]);
  const appended: ReturnType<typeof decision>[] = [];
  const enqueued: ReturnType<typeof decision>[] = [];
  const enqueuedGenerations: string[] = [];
  const controllerGenerations: string[] = [];
  const revocations: string[] = [];
  const application = {
    deploymentFingerprint: DEPLOYMENT,
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
      assertObservation: (candidate) => {
        if (!admitted.has(candidate)) throw new Error("not admitted");
      },
      observationDigest: async () => OBSERVATION_DIGEST,
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
      createActuationController: (_fresh, rollbackGeneration) => {
        controllerGenerations.push(rollbackGeneration);
        return Object.freeze({
          permit: Object.freeze({
            permitVersion: "midgard-production-workflow-actuation-permit-v1",
          }),
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
          latestSafeStartAtMs: "302400000",
        }),
      enqueue: async (fresh, _permit, _deadline, rollbackGeneration) => {
        if (input.enqueueError !== undefined) throw input.enqueueError;
        enqueued.push(fresh as ReturnType<typeof decision>);
        enqueuedGenerations.push(rollbackGeneration);
      },
      recover: async () => 0,
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
  };
};

describe("production fault decision bridge", () => {
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

    await currentHarness.bridge.dispatchPrepared();
    expect(currentHarness.enqueued.map(({ category }) => category)).toEqual([
      "doubleSpend",
    ]);
    expect(
      currentHarness.bridge.isJobPermitted({
        mode: "resume",
        category: "doubleSpend",
        headerHash: first!.headerHash,
        decisionDigest: prepared.target!.decisionDigest,
        rollbackGeneration: "1",
      }),
    ).toBe(true);
    expect(
      currentHarness.bridge.isJobPermitted({
        mode: "run",
        category: "invalidRange",
        headerHash: second!.headerHash,
        decisionDigest: decision(second!.headerHash, "invalidRange")
          .decisionDigest,
        rollbackGeneration: "1",
      }),
    ).toBe(false);

    currentHarness.bridge.invalidateForRollback();
    expect(() =>
      currentHarness.bridge.isJobPermitted({
        mode: "resume",
        category: "doubleSpend",
        headerHash: first!.headerHash,
        decisionDigest: prepared.target!.decisionDigest,
        rollbackGeneration: "1",
      }),
    ).toThrow("no current authenticated");
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
    expect(
      currentHarness.bridge.isJobPermitted({
        mode: "resume",
        category: "invalidRange",
        headerHash: second!.headerHash,
        decisionDigest: "fe".repeat(32),
        rollbackGeneration: "1",
      }),
    ).toBe(false);

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

  it("invalidates authority when rollback races an awaited classification", async () => {
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
      expect(currentHarness.application.classifyHeader).toHaveBeenCalledOnce(),
    );
    currentHarness.bridge.invalidateForRollback();
    release(decision(header!.headerHash, "doubleSpend"));
    await expect(preparing).rejects.toThrow(
      "authority changed during fault classification",
    );
    expect(currentHarness.enqueued).toHaveLength(0);
  });

  it("preserves an exact active target across unrelated finalized observations but revokes on rollback", async () => {
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

    const initial = await currentHarness.bridge.reconcileAndDispatch(current);
    expect(currentHarness.controllerGenerations).toEqual(["1"]);
    expect(currentHarness.enqueuedGenerations).toEqual(["1"]);

    await currentHarness.bridge.prepareForRecovery(later);
    expect(currentHarness.controllerGenerations).toEqual(["1"]);
    expect(currentHarness.revocations).toEqual([]);
    expect(
      currentHarness.bridge.isJobPermitted({
        mode: "resume",
        category: "doubleSpend",
        headerHash: header.headerHash,
        decisionDigest: initial.target!.decisionDigest,
        rollbackGeneration: "1",
      }),
    ).toBe(true);

    currentHarness.bridge.invalidateForRollback();
    expect(currentHarness.revocations).toEqual(["native_chain_rollback"]);
  });

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
