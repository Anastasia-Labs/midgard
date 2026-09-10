import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { l1SourceAuthorityDigest } from "../src/config.js";
import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { assertLibp2pDaRetentionDays } from "../src/config.js";
import type {
  DaPayloadRecord,
  StateQueueHeaderRecord,
  StateQueueHeaderStatus,
} from "../src/domain.js";
import { availabilityRetentionSourceFromStore } from "../src/l1/availability-retention-source.js";
import {
  FileChainSyncConsumerCursorStore,
  FileChainSyncCursorStore,
  LocalNodeChainAuthority,
  LocalNodeStateQueueProvider,
} from "../src/l1/provider.js";
import { createLocalKupmiosAvailabilityRetentionInputReader } from "../src/l1/state-queue-replay-provider.js";
import { scanStateQueue } from "../src/l1/state-queue-scanner.js";
import { JsonFileWatcherStore } from "../src/store.js";
import {
  pruneExpiredDaPayloads,
  retentionCandidates,
  retentionDeadlineReport,
  runRetentionCycle,
} from "../src/store/retention.js";
import { fixtureHeaderBase, minimalConfig, tempDir } from "./helpers.js";

const FINGERPRINT = "cd".repeat(32);
const NOW = Date.UTC(2026, 7, 3);
const REQUIRED_RETENTION_MS = MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
const retentionOptions = (nowMs = NOW) => ({
  nowMs,
  deploymentFingerprint: FINGERPRINT,
  minimumFinalityDepth: 30,
  availabilityChallengeAuthority: {
    deploymentFingerprint: FINGERPRINT,
    capability: "deployed_unobserved" as const,
    activeHeaderHashes: new Set<string>(),
  },
});

const openStores = new Set<JsonFileWatcherStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close()));
  openStores.clear();
});

const openStore = async (): Promise<JsonFileWatcherStore> => {
  const store = await JsonFileWatcherStore.open(await tempDir());
  openStores.add(store);
  return store;
};

const headerRecord = (
  headerHash: string,
  endTimeMs: number | bigint,
  status: StateQueueHeaderStatus,
): StateQueueHeaderRecord => ({
  deploymentFingerprint: FINGERPRINT,
  headerHash,
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  blockAssetName: headerHash,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    endTime: typeof endTimeMs === "bigint" ? endTimeMs : BigInt(endTimeMs),
  },
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint:
    status === "merged" || status === "removed"
      ? {
          slot: 100,
          blockHash: "12".repeat(32),
          blockHeight: 90,
          depth: 30,
          finalized: true,
          providerSource: "authenticated_state_queue_transition_v1",
        }
      : { finalized: true },
  finalized: true,
  status,
  validationErrors: [],
  updatedAt: new Date(NOW).toISOString(),
});

const payloadRecord = (
  headerHash: string,
  deploymentFingerprint = FINGERPRINT,
): DaPayloadRecord => ({
  deploymentFingerprint,
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "80",
  payloadSha256: "ef".repeat(32),
  sourcePeerId: "peer-1",
  fetchedAt: new Date(NOW).toISOString(),
  validationStatus: "verified",
});

const hashOf = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);

const seed = async (
  store: JsonFileWatcherStore,
  entries: readonly {
    readonly headerHash: string;
    readonly endTimeMs?: number | bigint;
    readonly status?: StateQueueHeaderStatus;
    readonly deploymentFingerprint?: string;
    readonly withoutHeader?: boolean;
  }[],
): Promise<void> => {
  for (const entry of entries) {
    await store.saveDaPayload(
      payloadRecord(entry.headerHash, entry.deploymentFingerprint),
    );
    if (entry.withoutHeader === true) {
      continue;
    }
    await store.upsertStateQueueHeader(
      headerRecord(
        entry.headerHash,
        entry.endTimeMs ?? NOW,
        entry.status ?? "attested",
      ),
    );
  }
};

describe("retentionCandidatesV1", () => {
  it("does not infer inactivity from missing active-set entries", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: hashOf(1), endTimeMs: expired, status: "merged" },
      { headerHash: hashOf(2), endTimeMs: expired, status: "removed" },
      { headerHash: hashOf(3), endTimeMs: expired, status: "attested" },
      { headerHash: hashOf(4), endTimeMs: NOW, status: "merged" },
    ]);

    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates).toHaveLength(4);
    expect(
      candidates.map((candidate) => [
        candidate.headerHash,
        candidate.decision.decision,
        candidate.decision.reasonCode,
      ]),
    ).toEqual([
      [hashOf(1), "retain", "availability_challenge_state_unknown"],
      [hashOf(2), "retain", "availability_challenge_state_unknown"],
      [hashOf(3), "retain", "availability_challenge_state_unknown"],
      [hashOf(4), "retain", "availability_challenge_state_unknown"],
    ]);
  });

  it("retains a payload with no header row (fail closed)", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(5), withoutHeader: true }]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate).toMatchObject({
      headerPresent: false,
      headerStatus: undefined,
      blockEndTimeMs: null,
    });
    expect(candidate?.decision).toEqual({
      decision: "retain",
      reasonCode: "availability_challenge_state_unknown",
    });
  });

  it("retains a payload written under a foreign deployment fingerprint", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY;
    await seed(store, [
      {
        headerHash: hashOf(6),
        endTimeMs: expired,
        status: "merged",
        deploymentFingerprint: "ab".repeat(32),
      },
    ]);
    const [candidate] = await retentionCandidates(store, {
      ...retentionOptions(),
    });
    expect(candidate?.fingerprintMismatch).toBe(true);
    expect(candidate?.decision.decision).toBe("retain");
  });

  it("retains malformed or unusable block end times", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(7), endTimeMs: -1n, status: "merged" },
      {
        headerHash: hashOf(8),
        endTimeMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
        status: "merged",
      },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    for (const candidate of candidates) {
      expect(candidate.blockEndTimeMs).toBeNull();
      expect(candidate.decision).toEqual({
        decision: "retain",
        reasonCode: "availability_challenge_state_unknown",
      });
    }
  });

  it("retains expired headers lacking per-header availability evidence", async () => {
    const store = await openStore();
    const endTime = NOW - REQUIRED_RETENTION_MS;
    await seed(store, [
      { headerHash: hashOf(9), endTimeMs: endTime, status: "merged" },
      { headerHash: hashOf(10), endTimeMs: endTime - 1, status: "merged" },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates[0]?.decision).toMatchObject({
      decision: "retain",
      reasonCode: "availability_challenge_state_unknown",
    });
    expect(candidates[1]?.decision).toMatchObject({
      decision: "retain",
      reasonCode: "availability_challenge_state_unknown",
    });
  });
});

describe("pruneExpiredDaPayloadsV1", () => {
  it("holds generic terminal status without availability evidence", async () => {
    const store = await openStore();
    const expired = NOW - 16 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: hashOf(1), endTimeMs: expired, status: "merged" },
      { headerHash: hashOf(2), endTimeMs: expired, status: "attested" },
      { headerHash: hashOf(3), endTimeMs: expired, status: "conflicted" },
      { headerHash: hashOf(4), withoutHeader: true },
    ]);
    const result = await pruneExpiredDaPayloads(store, retentionOptions());
    expect(result).toEqual({
      scanned: 4,
      prunedHeaderHashes: [],
      retained: 4,
    });
    expect(await store.getDaPayload(hashOf(1))).toBeDefined();
    expect(await store.getDaPayload(hashOf(2))).toBeDefined();
    expect((await store.listDaPayloads()).map((row) => row.headerHash)).toEqual(
      [hashOf(1), hashOf(2), hashOf(3), hashOf(4)],
    );
  });

  it("is inert against today's scanner statuses (no merged/removed emitted)", async () => {
    // Regression lock: the L1 state-queue scanner only ever writes
    // unattested/attesting/attested/conflicted today, so the committee pruner
    // must delete nothing. Loosening the terminal-status requirement to make
    // this pruner active must fail here.
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(
      store,
      (["unattested", "attesting", "attested", "conflicted"] as const).map(
        (status, index) => ({
          headerHash: hashOf(20 + index),
          endTimeMs: expired,
          status,
        }),
      ),
    );
    const result = await pruneExpiredDaPayloads(store, retentionOptions());
    expect(result.prunedHeaderHashes).toEqual([]);
    expect(result.retained).toBe(4);
  });

  it("never deletes across a deployment fingerprint mismatch", async () => {
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(store, [
      {
        headerHash: hashOf(30),
        endTimeMs: expired,
        status: "merged",
        deploymentFingerprint: "ab".repeat(32),
      },
    ]);
    const result = await pruneExpiredDaPayloads(store, {
      ...retentionOptions(),
    });
    expect(result.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(hashOf(30))).toBeDefined();
  });

  it("requires exact finalized authenticated terminal-transition provenance", async () => {
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    const headerHash = hashOf(33);
    await seed(store, [{ headerHash, endTimeMs: expired, status: "merged" }]);
    const authenticated = (await store.getStateQueueHeader(headerHash))!;

    for (const forged of [
      { ...authenticated, finalized: false },
      {
        ...authenticated,
        observedChainPoint: {
          ...authenticated.observedChainPoint,
          finalized: false,
        },
      },
      {
        ...authenticated,
        observedChainPoint: {
          ...authenticated.observedChainPoint,
          providerSource: "caller-authored",
        },
      },
      {
        ...authenticated,
        observedChainPoint: {
          ...authenticated.observedChainPoint,
          depth: 29,
        },
      },
      { ...authenticated, computedHeaderHash: hashOf(34) },
      { ...authenticated, validationErrors: ["forged"] },
    ]) {
      await store.upsertStateQueueHeader(forged);
      const result = await pruneExpiredDaPayloads(store, retentionOptions());
      expect(result.prunedHeaderHashes).toEqual([]);
      expect(await store.getDaPayload(headerHash)).toBeDefined();
    }

    await store.upsertStateQueueHeader(authenticated);
    const missingReleaseDepth = await pruneExpiredDaPayloads(store, {
      ...retentionOptions(),
      minimumFinalityDepth: undefined,
    });
    expect(missingReleaseDepth.prunedHeaderHashes).toEqual([]);

    const exact = await pruneExpiredDaPayloads(store, retentionOptions());
    expect(exact.prunedHeaderHashes).toEqual([]);
  });

  it("treats active or unavailable availability-challenge authority as a hold", async () => {
    const store = await openStore();
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: hashOf(31), endTimeMs: expired, status: "merged" },
      { headerHash: hashOf(32), endTimeMs: expired, status: "removed" },
    ]);
    const active = await pruneExpiredDaPayloads(store, {
      ...retentionOptions(),
      availabilityChallengeAuthority: {
        deploymentFingerprint: FINGERPRINT,
        capability: "deployed_unobserved",
        activeHeaderHashes: new Set([hashOf(31)]),
      },
    });
    expect(active.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(hashOf(31))).toBeDefined();

    const unavailable = await pruneExpiredDaPayloads(store, {
      nowMs: NOW,
      deploymentFingerprint: FINGERPRINT,
    });
    expect(unavailable.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(hashOf(31))).toBeDefined();
  });
});

describe("retentionDeadlineReportV1", () => {
  it("reports derived window arithmetic and alerts on burned headroom", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(40),
        endTimeMs:
          NOW - REQUIRED_RETENTION_MS + MIDGARD_RETENTION_WINDOW.marginMs,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.requiredRetentionMs).toBe(907_200_000);
    expect(report.deployedRetentionMs).toBe(1_296_000_000);
    expect(report.marginMs).toBe(388_800_000);
    expect(report.alertThresholdMs).toBe(388_800_000);
    expect(report.entries[0]).toMatchObject({ headroomMs: 0, alerting: true });
    expect(report.alerting).toBe(1);
  });

  it("does not alert one millisecond above the threshold", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(41),
        endTimeMs:
          NOW - REQUIRED_RETENTION_MS + MIDGARD_RETENTION_WINDOW.marginMs + 1,
        status: "attested",
      },
    ]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.entries[0]).toMatchObject({ headroomMs: 1, alerting: false });
    expect(report.alerting).toBe(0);
  });

  it("alerts on records with no computable deadline", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(42), withoutHeader: true }]);
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(report.entries[0]).toEqual({
      headerHash: hashOf(42),
      reasonCode: "availability_challenge_state_unknown",
      challengeableUntilMs: null,
      remainingMs: null,
      headroomMs: null,
      alerting: true,
    });
  });

  it("rejects malformed alert thresholds", async () => {
    const store = await openStore();
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      await expect(
        retentionDeadlineReport(store, {
          ...retentionOptions(),
          alertThresholdMs: bad,
        }),
      ).rejects.toThrow(/alertThresholdMs/u);
    }
  });
});

describe("runRetentionCycleV1", () => {
  it("reports the availability hold without deleting generic terminal records", async () => {
    const store = await openStore();
    const headerHash = hashOf(43);
    await seed(store, [
      {
        headerHash,
        endTimeMs: NOW - 40 * RETENTION_MS_PER_DAY,
        status: "removed",
      },
    ]);

    const cycle = await runRetentionCycle(store, retentionOptions());
    expect(cycle.deadlines).toMatchObject({
      scanned: 1,
      retained: 1,
      prunable: 0,
      alerting: 1,
    });
    expect(cycle.prune).toEqual({
      scanned: 1,
      prunedHeaderHashes: [],
      retained: 1,
    });
    expect(await store.getDaPayload(headerHash)).toBeDefined();
  });

  it("never deletes a concurrent payload absent from the preceding report", async () => {
    const store = await openStore();
    const reported = hashOf(44);
    const concurrent = hashOf(45);
    const expired = NOW - 40 * RETENTION_MS_PER_DAY;
    await seed(store, [
      { headerHash: reported, endTimeMs: expired, status: "merged" },
    ]);
    let injected = false;
    const wrapped = new Proxy(store, {
      get(target, property, receiver) {
        if (property === "listStateQueueHeaders") {
          return async () => {
            if (!injected) {
              injected = true;
              await seed(store, [
                {
                  headerHash: concurrent,
                  endTimeMs: expired,
                  status: "removed",
                },
              ]);
            }
            return store.listStateQueueHeaders();
          };
        }
        const value = Reflect.get(target, property, receiver) as unknown;
        return typeof value === "function" ? value.bind(target) : value;
      },
    });

    const cycle = await runRetentionCycle(wrapped, retentionOptions());
    expect(cycle.deadlines.entries.map(({ headerHash }) => headerHash)).toEqual(
      [reported],
    );
    expect(cycle.prune.prunedHeaderHashes).toEqual([]);
    expect(await store.getDaPayload(concurrent)).toBeDefined();
  });
});

describe("assertLibp2pDaRetentionDaysV1", () => {
  it("accepts the canonical 15-day window matching the manifest", () => {
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
        manifestRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      }),
    ).toBe(15);
    expect(LIBP2P_DA_MIN_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays,
    );
  });

  it("rejects 14 days and accepts 15 at the boundary", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 14,
        manifestRetentionDays: 14,
      }),
    ).toThrow(/must be at least 15 days/u);
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 15,
        manifestRetentionDays: 15,
      }),
    ).toBe(15);
  });

  it("rejects a runtime window that differs from the manifest window", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 16,
        manifestRetentionDays: 15,
      }),
    ).toThrow(/must exactly equal the verified deployment manifest/u);
  });

  it("rejects malformed runtime retention days", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        assertLibp2pDaRetentionDays({
          runtimeRetentionDays: bad,
          manifestRetentionDays: 15,
        }),
      ).toThrow(/da_transport\.retention_days/u);
    }
  });
});

const h32 = (byte: string): string => byte.repeat(64);
const terminalMerge = (
  headerHash: Buffer,
  sequence: number,
): SDK.StateQueueAuthenticatedTransition => {
  const policyId = "bb".repeat(28);
  const transactionHash = h32(sequence.toString(16));
  const rootOutRef = `${h32("0")}#0`;
  const headerOutRef = `${h32((sequence + 4).toString(16))}#0`;
  const redeemer = {
    MergeToConfirmedStateV1: {
      yield_to_ref_input_index: 0n,
      header_node_key: headerHash.toString("hex"),
      confirmed_state_input_outref: {
        transactionId: h32("0"),
        outputIndex: 0n,
      },
      confirmed_state_output_index: 0n,
      m_settlement_redeemer_index: null,
      merged_block_withdrawals_root: h32("1"),
      merged_block_forced_transactions_root: h32("2"),
      merged_block_transactions_root: h32("3"),
      merged_block_deposits_root: h32("4"),
      merged_block_transition_trace_root: h32("5"),
      merged_block_event_to_step_root: h32("6"),
      merged_block_validation_traces_root: h32("7"),
      merged_block_withdrawal_count: 0n,
      merged_block_forced_transaction_count: 0n,
      merged_block_l2_transaction_count: 0n,
      merged_block_deposit_count: 0n,
      merged_block_total_event_count: 0n,
      merged_block_transition_step_count: 0n,
      merged_block_validation_trace_count: 0n,
    },
  } as const;
  const transition = SDK.deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: FINGERPRINT,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 12).toString(16)),
    finalityDepth: "30",
    mintPolicyIds: [policyId],
    referenceInputOutRefs: [`${h32("f")}#0`],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: `${h32("f")}#0`,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, SDK.StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: [rootOutRef, headerOutRef],
    previousQueue: [
      { headerHash: null, outRef: rootOutRef },
      { headerHash: headerHash.toString("hex"), outRef: headerOutRef },
    ],
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }],
  });
  if (transition === null) throw new Error("invalid terminal merge fixture");
  return transition;
};

const publishedFixture = (endTime: Date, sequence = 1) => {
  const authority: SDK.DaAvailabilityRetentionAuthority = {
    deploymentIdentityDigest: FINGERPRINT,
    stateQueuePolicyId: "bb".repeat(28),
    availabilityPolicyId: "cc".repeat(28),
    stateQueueAddress: credentialToAddress("Preprod", {
      type: "Script",
      hash: "dd".repeat(28),
    }),
    minimumFinalityDepth: 30n,
  };
  const header: SDK.Header = {
    ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(endTime.getTime() - 1000),
    endTime: BigInt(endTime.getTime()),
    blockSlot: BigInt(sequence),
    expectedNetworkId: 0n,
    minFeeA: 44n,
    minFeeB: 155381n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    Effect.runSync(SDK.hashBlockHeader(header)),
    "hex",
  );
  const transition = terminalMerge(headerHash, sequence);
  const input: import("@lucid-evolution/lucid").UTxO = {
    txHash: transition.previousQueue[1]!.outRef.split("#")[0]!,
    outputIndex: 0,
    address: authority.stateQueueAddress,
    assets: {
      lovelace: 4000000n,
      [authority.stateQueuePolicyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash.toString("hex")]: 1n,
    },
    datum: Data.to(
      {
        data: {
          Node: {
            data: Data.castTo(
              {
                header,
                da_attestation: {
                  Published: { terminal_commitment: "aa".repeat(32) },
                },
              },
              SDK.StateQueueNode,
            ),
          },
        },
        link: null,
      },
      SDK.LinkedListDatum,
    ),
  };
  const evidence = SDK.deriveDaAvailabilityRetentionEvidence(
    transition,
    input,
    authority,
  );
  if (!evidence) throw new Error("invalid published terminal fixture");
  return { header, headerHash, input, transition, evidence, authority };
};
const seedEvidence = async (
  store: JsonFileWatcherStore,
  endTimeMs = NOW - 40 * RETENTION_MS_PER_DAY,
) => {
  const fixture = publishedFixture(new Date(endTimeMs));
  const headerHash = fixture.headerHash.toString("hex");
  await store.saveDaPayload(payloadRecord(headerHash));
  const record = {
    ...headerRecord(headerHash, endTimeMs, "merged"),
    header: fixture.header,
    observedChainPoint: {
      slot: Number(fixture.transition.slot),
      blockHash: fixture.transition.blockHash,
      blockHeight: Number(fixture.transition.blockNo),
      depth: 30,
      finalized: true,
      providerSource: "authenticated_state_queue_transition_v1",
    },
  };
  await store.upsertStateQueueHeader(record);
  let current = true;
  const options = {
    ...retentionOptions(),
    availabilityChallengeAuthority: {
      ...retentionOptions().availabilityChallengeAuthority,
      terminalEvidence: new Map([[headerHash, fixture]]),
      withCurrentTerminalEvidence: async (
        hash: string,
        digest: string,
        remove: () => Promise<boolean>,
      ) =>
        current &&
        hash === headerHash &&
        digest === fixture.transition.transitionDigest
          ? remove()
          : false,
    },
  };
  return {
    ...fixture,
    headerHash,
    options,
    revoke: () => {
      current = false;
    },
  };
};
describe("availability terminal retention authority", () => {
  it("prunes only authenticated published evidence and holds at the exact horizon", async () => {
    const store = await openStore();
    const f = await seedEvidence(store, NOW - REQUIRED_RETENTION_MS);
    expect(
      (await pruneExpiredDaPayloads(store, f.options)).prunedHeaderHashes,
    ).toEqual([]);
    expect(
      (await runRetentionCycle(store, { ...f.options, nowMs: NOW + 1 })).prune
        .prunedHeaderHashes,
    ).toEqual([f.headerHash]);
  });
  it("retains evidence when the terminal point or local header timestamp changes", async () => {
    const store = await openStore();
    const f = await seedEvidence(store);
    const header = (await store.getStateQueueHeader(f.headerHash))!;
    for (const changed of [
      {
        ...header,
        observedChainPoint: { ...header.observedChainPoint, depth: 29 },
      },
      {
        ...header,
        observedChainPoint: {
          ...header.observedChainPoint,
          blockHash: "ef".repeat(32),
        },
      },
      {
        ...header,
        header: { ...header.header, endTime: header.header.endTime - 1n },
      },
    ]) {
      await store.upsertStateQueueHeader(changed);
      expect(
        (await pruneExpiredDaPayloads(store, f.options)).prunedHeaderHashes,
      ).toEqual([]);
    }
    await store.upsertStateQueueHeader(header);
    expect(
      (await pruneExpiredDaPayloads(store, f.options)).prunedHeaderHashes,
    ).toEqual([f.headerHash]);
  });
  it("rechecks rollback authority immediately before deletion", async () => {
    const store = await openStore();
    const f = await seedEvidence(store);
    f.revoke();
    expect(
      (await retentionCandidates(store, f.options))[0]?.decision.decision,
    ).toBe("prune");
    expect(
      (await pruneExpiredDaPayloads(store, f.options)).prunedHeaderHashes,
    ).toEqual([]);
    expect(await store.getDaPayload(f.headerHash)).toBeDefined();
  });
  it("retains active, foreign, malformed and unguarded evidence", async () => {
    const store = await openStore();
    const f = await seedEvidence(store);
    for (const source of [
      {
        ...f.options.availabilityChallengeAuthority,
        activeHeaderHashes: new Set([f.headerHash]),
      },
      {
        ...f.options.availabilityChallengeAuthority,
        deploymentFingerprint: "ee".repeat(32),
      },
      {
        ...f.options.availabilityChallengeAuthority,
        withCurrentTerminalEvidence: undefined,
      },
      {
        ...f.options.availabilityChallengeAuthority,
        terminalEvidence: new Map([
          [
            f.headerHash,
            { ...f, evidence: { ...f.evidence, removedQueueInputCbor: "00" } },
          ],
        ]),
      },
    ])
      expect(
        (
          await pruneExpiredDaPayloads(store, {
            ...f.options,
            availabilityChallengeAuthority: source,
          })
        ).prunedHeaderHashes,
      ).toEqual([]);
    expect(
      (await pruneExpiredDaPayloads(store, f.options)).prunedHeaderHashes,
    ).toEqual([f.headerHash]);
  });
});

const nativeRetentionFixture = async () => {
  const dir = await tempDir();
  const f = publishedFixture(new Date(NOW - 40 * RETENTION_MS_PER_DAY));
  const base = minimalConfig({
    dir,
    manifestPath: `${dir}/manifest.json`,
    deploymentInfoPath: `${dir}/contracts.json`,
    signerSeed: "00".repeat(32),
    signerPublicKey: "11".repeat(32),
  });
  const config = {
    ...base,
    deploymentFingerprint: FINGERPRINT,
    finalityDepth: 30,
    stateQueuePolicyId: f.authority.stateQueuePolicyId,
    stateQueueAddress: f.authority.stateQueueAddress,
    midgardNodeDeployment: {
      ...base.midgardNodeDeployment,
      availabilityChallenge: {
        ...base.midgardNodeDeployment.availabilityChallenge,
        policyId: f.authority.availabilityPolicyId,
      },
    },
  };
  const authorityHash = l1SourceAuthorityDigest(
    config.network,
    config.l1Source,
  );
  let rolledBack = false;
  const point = {
    network: config.network,
    slot: 200,
    blockHash: "ee".repeat(32),
    providerSource: "chain-sync:fixture-node",
    observedAt: new Date(NOW).toISOString(),
  };
  const rollbackPoint = { ...point, slot: 50, blockHash: "ff".repeat(32) };
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    ...f.transition,
    mintPolicyIds: [f.authority.stateQueuePolicyId],
    redeemers: [f.transition.stateQueueMintRedeemer],
    spentInputOutRefs: f.transition.consumedQueueOutRefs,
    referenceInputOutRefs:
      f.transition.correctionLockWitness.kind === "idle_reference"
        ? [f.transition.correctionLockWitness.referenceOutRef]
        : [],
  });
  if (checkpoint === null)
    throw new Error("invalid retention replay checkpoint");
  let reads = 0;
  const reader = createLocalKupmiosAvailabilityRetentionInputReader({
    kupoUrl: "http://localhost:1442",
    stateQueueAddress: f.authority.stateQueueAddress,
    stateQueuePolicyId: f.authority.stateQueuePolicyId,
    fetchImpl: async () => {
      reads += 1;
      return new Response(
        JSON.stringify([
          {
            transaction_id: f.input.txHash,
            output_index: f.input.outputIndex,
            address: f.input.address,
            datum_type: "inline",
            datum: f.input.datum,
            value: {
              coins: "4000000",
              assets: {
                [f.authority.stateQueuePolicyId +
                "." +
                SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
                f.headerHash.toString("hex")]: "1",
              },
            },
          },
        ]),
      );
    },
  });
  const makeProvider = () => {
    const authority = new LocalNodeChainAuthority(
      "fixture-node",
      config.network,
      {
        next: async (cursor) => {
          const tip = rolledBack ? rollbackPoint : point;
          return cursor === undefined
            ? { event: { direction: "roll_forward" as const, point: tip }, tip }
            : rolledBack && cursor.rollbackGeneration === 0
              ? {
                  event: { direction: "roll_backward" as const, point: tip },
                  tip,
                }
              : { tip };
        },
      },
      new FileChainSyncCursorStore(`${dir}/chain.json`, authorityHash),
    );
    return new LocalNodeStateQueueProvider(
      authority,
      [
        {
          currentChainPoint: async () => ({
            ...point,
            providerSource: "query:fixture-node",
          }),
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () => ({
            nodes: [],
            confirmedHeaderHash: f.headerHash.toString("hex"),
            confirmedStateOutRef: f.transition.nextQueue[0]!.outRef,
            observedChainPoint: {
              ...point,
              blockHeight: 120,
              depth: 30,
              finalized: true,
            },
          }),
          fetchStateQueueReplayCheckpoints: async () => [checkpoint],
          readAvailabilityRetentionInput: reader,
        },
      ],
      ["query:fixture-node"],
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        authorityHash,
      ),
    );
  };
  const provider = makeProvider();
  const records = await scanStateQueue(provider, {
    deploymentFingerprint: FINGERPRINT,
    deploymentIdentityDigest: FINGERPRINT,
    stateQueuePolicyId: f.authority.stateQueuePolicyId,
    daAttestationPolicyId: config.daAttestationPolicyId,
    finalityDepth: 30,
    consensusProfile: config.consensusProfile,
    previousHeaders: [
      {
        ...headerRecord(
          f.headerHash.toString("hex"),
          Number(f.header.endTime),
          "attested",
        ),
        header: f.header,
        stateQueueOutRef: `${f.input.txHash}#0`,
      },
    ],
    terminalReplayAnchor: {
      deploymentIdentityDigest: FINGERPRINT,
      stateQueuePolicyId: f.authority.stateQueuePolicyId,
      queue: f.transition.previousQueue,
      blockNo: "0",
      transactionIndex: "0",
    },
    availabilityRetentionAuthority: f.authority,
  });
  expect(reads).toBe(1);
  expect(records[0]?.availabilityRetention?.evidence).toEqual(f.evidence);
  const store = await JsonFileWatcherStore.open(`${dir}/store`);
  openStores.add(store);
  await store.saveDaPayload(payloadRecord(f.headerHash.toString("hex")));
  await store.upsertStateQueueHeader(records[0]!);
  await store.saveL1SourceState({
    schemaVersion: 1,
    sourceMode: "local_node",
    network: config.network,
    authoritySha256: authorityHash,
    status: "healthy",
    observedAt: new Date(NOW).toISOString(),
    observations: [
      {
        headerHash: f.headerHash.toString("hex"),
        stateQueueOutRef: `${f.input.txHash}#0`,
        stateQueueStatus: "merged",
        slot: Number(f.transition.slot),
        blockHash: f.transition.blockHash,
        finalized: true,
        hasPersistedDecision: false,
      },
    ],
  });
  await provider.acknowledgeChainSyncCursor(
    await provider.currentChainSyncCursor(),
  );
  const restart = async () => {
    await store.close();
    openStores.delete(store);
    const reopened = await JsonFileWatcherStore.open(`${dir}/store`);
    openStores.add(reopened);
    return { store: reopened, provider: makeProvider() };
  };
  return {
    config,
    store,
    provider,
    headerHash: f.headerHash.toString("hex"),
    restart,
    rollback: () => {
      rolledBack = true;
    },
  };
};
describe("native committee availability retention collection", () => {
  it("collects consumed Published evidence during ordered replay and prunes after restart", async () => {
    const f = await nativeRetentionFixture();
    const resumed = await f.restart();
    const source = await availabilityRetentionSourceFromStore(
      f.config,
      resumed.store,
      resumed.provider,
      NOW,
    );
    expect(source.terminalEvidence?.has(f.headerHash)).toBe(true);
    expect(
      (
        await runRetentionCycle(resumed.store, {
          ...retentionOptions(),
          availabilityChallengeAuthority: source,
        })
      ).prune.prunedHeaderHashes,
    ).toEqual([f.headerHash]);
  });
  it("holds deletion on an unconsumed rollback and revokes durable evidence on quarantine", async () => {
    const f = await nativeRetentionFixture();
    const resumed = await f.restart();
    const source = await availabilityRetentionSourceFromStore(
      f.config,
      resumed.store,
      resumed.provider,
      NOW,
    );
    f.rollback();
    expect(
      (
        await pruneExpiredDaPayloads(resumed.store, {
          ...retentionOptions(),
          availabilityChallengeAuthority: source,
        })
      ).prunedHeaderHashes,
    ).toEqual([]);
    const state = (await resumed.store.getL1SourceState())!;
    await resumed.store.quarantineL1Decisions({
      ...state,
      status: "quarantined",
      quarantineReason: "authenticated rollback",
      quarantinedAt: new Date(NOW).toISOString(),
    });
    expect(
      (await resumed.store.getStateQueueHeader(f.headerHash))
        ?.availabilityRetention,
    ).toBeUndefined();
    expect(
      (
        await availabilityRetentionSourceFromStore(
          f.config,
          resumed.store,
          resumed.provider,
          NOW,
        )
      ).terminalEvidence?.size,
    ).toBe(0);
    expect(await resumed.store.getDaPayload(f.headerHash)).toBeDefined();
  });
});
