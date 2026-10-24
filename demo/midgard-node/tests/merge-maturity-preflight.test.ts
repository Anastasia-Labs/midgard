import "./utils.js";

import { performance } from "node:perf_hooks";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Duration, Effect, Either, Exit, identity, Option, Ref } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { DatabaseError } from "../src/database/utils/common.js";
import { Database } from "../src/services/database.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FOLLOWER_VIEW_UNAPPLIED,
  FollowerWrite,
  FollowerWriteFixture,
  followerWriteHoldOf,
  type FollowerWritePermit,
} from "../src/services/follower-write-gate.js";
import {
  Globals,
  L1ControlPlaneTimeoutError,
  NodeConfig,
} from "../src/services/index.js";
import type {
  StateQueueSnapshot,
  StateQueueSnapshotReason,
} from "../src/services/landed-state-queue.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { writeFollowerTip } from "./helpers/follower-view.js";
import {
  holdFollowerWriteGate,
  openFollowerWriteGateAt,
} from "./helpers/follower-write-gate.js";
import { resetApplicationTables } from "./utils.js";

const landedStateQueueSnapshotMock = vi.hoisted(() => vi.fn());
const buildAndSubmitMergeTxMock = vi.hoisted(() => vi.fn());
const captureMergeLocalLedgerGateMock = vi.hoisted(() => vi.fn());
const fetchCanonicalMergeCandidateReadinessMock = vi.hoisted(() => vi.fn());
const finalizeLandedMergesProgramMock = vi.hoisted(() => vi.fn());
const tryWithLeaseMock = vi.hoisted(() => vi.fn());
const revalidateMock = vi.hoisted(() => vi.fn());

vi.mock("../src/services/landed-state-queue.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/services/landed-state-queue.js")
    >();
  return {
    ...actual,
    landedStateQueueSnapshot: landedStateQueueSnapshotMock,
    // The post-merge wait reads the same snapshot, as `post_merge`.
    awaitPostMergeSnapshot: (stateQueue: unknown) =>
      landedStateQueueSnapshotMock(stateQueue, "post_merge"),
  };
});

vi.mock("../src/transactions/state-queue/merge-to-confirmed-state.js", () => ({
  buildAndSubmitMergeTx: buildAndSubmitMergeTxMock,
  captureMergeLocalLedgerGate: captureMergeLocalLedgerGateMock,
  fetchCanonicalMergeCandidateReadiness:
    fetchCanonicalMergeCandidateReadinessMock,
  finalizeLandedMergesProgram: finalizeLandedMergesProgramMock,
  mergeSemanticSkipResult: (readiness: {
    readonly status:
      | "skipped_oldest_block_unattested"
      | "skipped_oldest_block_not_mature";
    readonly headerHash: string;
    readonly reason: string;
    readonly readyAfterUnixTime: number;
    readonly nowUnixTime: number;
  }) => ({
    status: readiness.status,
    headerHash: readiness.headerHash,
    reason: readiness.reason,
    readyAfterUnixTime: readiness.readyAfterUnixTime,
    nowUnixTime: readiness.nowUnixTime,
  }),
}));

// Preserve real table/status exports; replace only merge preflight members.
vi.mock("../src/database/index.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/database/index.js")>();
  const { Effect: EffectModule } = await import("effect");
  return {
    ...actual,
    MempoolDB: {
      ...actual.MempoolDB,
      retrieveTxCount: EffectModule.succeed(0n),
    },
    MutationJobsDB: {
      ...actual.MutationJobsDB,
      countUnfinished: EffectModule.succeed(0n),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      countBacklog: EffectModule.succeed(0n),
    },
    StateQueueMutationLeasesDB: {
      ...actual.StateQueueMutationLeasesDB,
      tryWithLease: tryWithLeaseMock,
      revalidate: revalidateMock,
      describeActiveLease: () => "holder=test,status=active",
    },
  };
});

import {
  mergeAction,
  type MergeActionResult,
  MergeProducerPermitUnavailable,
} from "../src/fibers/merge.js";
import { slotAwareDueWorkRegistry } from "../src/fibers/slot-aware-due-work.js";

const fakeContracts = {
  stateQueue: {
    spendingScriptAddress: "addr_test1statequeue",
    policyId: "00".repeat(28),
  },
  daAttestation: {
    policyId: "22".repeat(28),
  },
};

const switchToOperatorsMergingWalletMock = vi.fn();

const makeSnapshot = (
  parsedNodeCount: number,
  reason: StateQueueSnapshotReason = "manual_status",
): StateQueueSnapshot => ({
  snapshotId: `${reason}:root#0:tail#${parsedNodeCount.toString()}`,
  reason,
  view: { generation: 1, slot: parsedNodeCount },
  blockCount: Math.max(0, parsedNodeCount - 1),
  root: {
    outRef: "root#0",
    headerHash: SDK.GENESIS_HEADER_HASH,
    utxo: {} as StateQueueSnapshot["root"]["utxo"],
  },
  tailCommitBase: {
    outRef: `tail#${parsedNodeCount.toString()}`,
    headerHash:
      parsedNodeCount <= 1 ? SDK.GENESIS_HEADER_HASH : "11".repeat(28),
    utxo: {} as StateQueueSnapshot["tailCommitBase"]["utxo"],
    blockEndTimeMs: 0,
    roots: {
      utxosRoot: "00".repeat(32),
      transactionsRoot: "00".repeat(32),
      depositsRoot: "00".repeat(32),
      withdrawalsRoot: "00".repeat(32),
    },
  },
});

const makeCandidate = (
  status:
    | "ready"
    | "skipped_oldest_block_unattested"
    | "skipped_oldest_block_not_mature",
  identitySuffix: string = "a",
) => {
  const headerHash = identitySuffix.repeat(56).slice(0, 56);
  const readyAfterUnixTime = 1_700_000_030_000;
  const nowUnixTime =
    status === "skipped_oldest_block_not_mature"
      ? readyAfterUnixTime - 1_000
      : readyAfterUnixTime;
  const currentDaAvailability: SDK.DaAvailabilityStateQueueStatus =
    status === "skipped_oldest_block_unattested"
      ? SDK.NO_DA_ATTESTATION
      : { Published: { terminal_commitment: "22".repeat(32) } };
  const firstBlockOutRef = `${identitySuffix.repeat(64).slice(0, 64)}#0`;
  const candidateIdentity = [
    firstBlockOutRef,
    headerHash,
    SDK.daAvailabilityStateQueueStatusIdentity(currentDaAvailability),
    readyAfterUnixTime.toString(),
  ].join("|");
  return {
    status: "candidate" as const,
    confirmedUTxO: {},
    firstBlockUTxO: {},
    blockHeader: {},
    firstBlockNode: {},
    readiness: {
      provenFraud: null,
      status,
      headerHash,
      reason:
        status === "ready"
          ? `header=${headerHash}`
          : status === "skipped_oldest_block_unattested"
            ? `header=${headerHash},current_da_availability=Unattested,required_da_availability=Attested|Published`
            : `header=${headerHash},ready_after=${readyAfterUnixTime.toString()},now=${nowUnixTime.toString()}`,
      firstBlockOutRef,
      candidateIdentity,
      currentDaAvailability,
      validFromUnixTime: readyAfterUnixTime - 20_000,
      readyAfterUnixTime,
      nowUnixTime,
    },
  };
};

const noCandidate = {
  status: "no_candidate" as const,
  reason: "confirmed_state_link_empty",
};

/**
 * This process's follower write gate as the merge runs: `fixture` (no driver
 * epoch; the fixture capability runs the work unregistered, no database),
 * `applied` (a driver view; the merge registers and holds a permit, on
 * Postgres), `recomputing`, or `none` (no view and no fixture).
 */
type MergeRunOptions = {
  readonly gate?: "fixture" | "applied" | "recomputing" | "none";
  /** Also provides the fixture capability to an `applied` gate. */
  readonly fixture?: boolean;
  /** Runs once the `applied` gate is open, before the merge registers. */
  readonly afterOpen?: Effect.Effect<unknown, unknown, SqlClient.SqlClient>;
  readonly expectedHeaderHash?: string;
};

/** The permits `applied` gates opened, oldest first. */
const openedPermits: FollowerWritePermit[] = [];

/** A driver's publish: the gate opens at the follower's tip in a new epoch. */
const openAtFollowerTip = Effect.flatMap(
  writeFollowerTip(10),
  openFollowerWriteGateAt,
);

/** Opens the gate as this process's driver's. */
const applyFollowerView = Effect.gen(function* () {
  yield* resetApplicationTables;
  const permit = yield* openAtFollowerTip;
  openedPermits.push(permit);
  yield* Ref.update((yield* Globals).FOLLOWER_WRITE_GATE, (local) => ({
    ...local,
    epoch: permit.epoch,
  }));
});

/** The node database without the fixture capability (a runtime process). */
const onNodeDatabase = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  effect.pipe(Effect.provide(Database.layer), Effect.provide(NodeConfig.layer));

const mergeActionProgram = (
  force: boolean,
  {
    gate = "fixture",
    fixture = false,
    afterOpen,
    expectedHeaderHash,
  }: MergeRunOptions = {},
) => {
  const lucidService = LucidService.make({
    api: {
      unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
    } as never,
    referenceScriptsApi: {} as never,
    operatorMainAddress: "",
    operatorMergeAddress: "",
    referenceScriptsWalletAddress: "",
    referenceScriptsAddress: "addr_test1referencescripts",
    submitSlotSnapshot: () =>
      Effect.succeed({
        source: "test",
        currentSlot: 2_000_000_000,
        observedAtMs: 0,
        slotLengthMs: 1_000,
      }),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.sync(() => {
      switchToOperatorsMergingWalletMock();
    }),
    switchToReferenceScriptWallet: Effect.void,
  });

  const program = Effect.gen(function* () {
    const globals = yield* Globals;
    if (gate === "applied") {
      yield* applyFollowerView;
      if (afterOpen !== undefined) yield* afterOpen;
    }
    if (gate === "recomputing")
      yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
        ...local,
        epoch: "1",
        recomputing: true,
      }));
    const merge = mergeAction(force, { expectedHeaderHash });
    return yield* gate === "fixture" || fixture
      ? merge.pipe(Effect.provideService(FollowerWriteFixture, true))
      : merge;
  }).pipe(
    Effect.provideService(LucidService, lucidService),
    Effect.provideService(MidgardContracts, fakeContracts as never),
    Effect.provide(Globals.Default),
  );
  return (
    gate === "applied"
      ? onNodeDatabase(program)
      : program.pipe(Effect.provide(NodeConfig.layer))
  ) as Effect.Effect<MergeActionResult, unknown, never>;
};

const runMergeAction = (force: boolean, options: MergeRunOptions = {}) =>
  Effect.runPromise(mergeActionProgram(force, options));

/** A builder that reports the follower write permit it ran under and merges. */
const recordPermitAndMerge = (seen: (FollowerWritePermit | null)[]) =>
  Effect.gen(function* () {
    const permit = yield* Effect.serviceOption(FollowerWrite);
    seen.push(Option.getOrNull(permit));
    return {
      status: "merged" as const,
      headerHash: "aa".repeat(28),
      txHash: "bb".repeat(32),
    };
  });

/** The named hold a permit-unavailable merge was refused for. */
const permitRefusalReason = (outcome: Either.Either<unknown, unknown>) => {
  const left = Either.isLeft(outcome) ? outcome.left : undefined;
  expect(left).toBeInstanceOf(MergeProducerPermitUnavailable);
  return followerWriteHoldOf((left as MergeProducerPermitUnavailable).cause)
    ?.reason;
};

describe("merge maturity semantic preflight", () => {
  beforeEach(() => {
    slotAwareDueWorkRegistry.clearAll();
    landedStateQueueSnapshotMock.mockReset();
    buildAndSubmitMergeTxMock.mockReset();
    captureMergeLocalLedgerGateMock.mockReset();
    fetchCanonicalMergeCandidateReadinessMock.mockReset();
    finalizeLandedMergesProgramMock.mockReset();
    tryWithLeaseMock.mockReset();
    revalidateMock.mockReset();
    switchToOperatorsMergingWalletMock.mockReset();
    openedPermits.length = 0;
    finalizeLandedMergesProgramMock.mockImplementation(() =>
      Effect.succeed([]),
    );

    landedStateQueueSnapshotMock.mockImplementation(
      (_stateQueue: unknown, reason: StateQueueSnapshotReason) =>
        Effect.succeed(makeSnapshot(9, reason)),
    );
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("ready")),
    );
    captureMergeLocalLedgerGateMock.mockImplementation(() =>
      Effect.succeed({ status: "ready" as const }),
    );
    tryWithLeaseMock.mockImplementation(
      (
        _holder: string,
        run: (token: string) => Effect.Effect<unknown, unknown, unknown>,
      ) =>
        Effect.gen(function* () {
          const value = yield* run("test-lease-token");
          return { _tag: "Ran" as const, value };
        }),
    );
    revalidateMock.mockImplementation(() => Effect.void);
  });

  it("skips not-mature automatic and forced merges before taking the mutation lease", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_not_mature")),
    );

    const automatic = await runMergeAction(false);
    const forced = await runMergeAction(true);

    expect(automatic).toMatchObject({
      status: "skipped_oldest_block_not_mature",
      readyAfterUnixTime: 1_700_000_030_000,
      nowUnixTime: 1_700_000_029_000,
    });
    expect(forced).toMatchObject({
      status: "skipped_oldest_block_not_mature",
      readyAfterUnixTime: 1_700_000_030_000,
      nowUnixTime: 1_700_000_029_000,
    });
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(revalidateMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    expect(landedStateQueueSnapshotMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("finalizes landed merges before any skip, and a failed catch-up stops the attempt", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_not_mature")),
    );

    // An early skip still runs the catch-up first, under the permit.
    const permits: (FollowerWritePermit | null)[] = [];
    finalizeLandedMergesProgramMock.mockImplementation(() =>
      Effect.gen(function* () {
        const permit = yield* Effect.serviceOption(FollowerWrite);
        permits.push(Option.getOrNull(permit));
        return [];
      }),
    );
    expect(await runMergeAction(false, { gate: "applied" })).toMatchObject({
      status: "skipped_oldest_block_not_mature",
    });
    expect(finalizeLandedMergesProgramMock).toHaveBeenCalledTimes(1);
    expect(finalizeLandedMergesProgramMock).toHaveBeenCalledWith({
      stateQueueAddress: "addr_test1statequeue",
      stateQueuePolicyId: "00".repeat(28),
    });
    expect(permits).toEqual([openedPermits[0]]);

    // A catch-up failure fails the attempt: nothing is built on top of a
    // landed merge that is not finalized.
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("ready")),
    );
    finalizeLandedMergesProgramMock.mockImplementation(() =>
      Effect.fail(new Error("landed merge finalization failed")),
    );
    await expect(runMergeAction(true)).rejects.toThrow(
      "landed merge finalization failed",
    );
    expect(fetchCanonicalMergeCandidateReadinessMock).toHaveBeenCalledTimes(1);
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
  });

  it("skips DA-unattested candidates before taking the mutation lease", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_unattested")),
    );

    const result = await runMergeAction(false);

    expect(result).toMatchObject({
      status: "skipped_oldest_block_unattested",
      // The decoded enum reason is current_da_availability (see makeCandidate).
      reason: expect.stringContaining("current_da_availability="),
    });
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    expect(landedStateQueueSnapshotMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("revalidates under lease and skips stale ready preflight evidence when the candidate changes", async () => {
    fetchCanonicalMergeCandidateReadinessMock
      .mockImplementationOnce(() => Effect.succeed(makeCandidate("ready", "a")))
      .mockImplementationOnce(() =>
        Effect.succeed(makeCandidate("ready", "b")),
      );

    const result = await runMergeAction(false);

    expect(result).toMatchObject({
      status: "skipped_merge_candidate_changed",
      reason: expect.stringContaining("preflight_candidate="),
    });
    expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
    expect(revalidateMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    expect(landedStateQueueSnapshotMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("keeps non-semantic no-candidate cases on the existing leased planner path", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(noCandidate),
    );
    landedStateQueueSnapshotMock.mockImplementation(
      (_stateQueue: unknown, reason: StateQueueSnapshotReason) =>
        Effect.succeed(makeSnapshot(1, reason)),
    );

    const result = await runMergeAction(false);

    expect(result).toMatchObject({
      status: "no_queued_block",
      reason: "queue_length=0",
      queueLength: 0,
    });
    expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("continues to the builder only after ready semantic revalidation and leased planner readiness", async () => {
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      Effect.succeed({
        status: "skipped_oldest_block_local_ledger_not_ready" as const,
        headerHash: "aa".repeat(28),
        reason: "local_submit_ledger_still_behind_after_wait",
        readyAfterUnixTime: 1_700_000_030_000,
        nowUnixTime: 1_700_000_030_000,
      }),
    );

    const attemptStartedAt = Date.now();
    const result = await runMergeAction(false);
    const attemptEndedAt = Date.now();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      reason: "local_submit_ledger_still_behind_after_wait",
    });
    // The confirmation wait ends 30 s before the 180 s hold does, leaving
    // the local finalization room inside the hold.
    const { confirmationDeadlineMs } = buildAndSubmitMergeTxMock.mock
      .calls[0]![3] as { readonly confirmationDeadlineMs: number };
    expect(confirmationDeadlineMs).toBeGreaterThanOrEqual(
      attemptStartedAt + 150_000,
    );
    expect(confirmationDeadlineMs).toBeLessThanOrEqual(
      attemptEndedAt + 150_000,
    );
    expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
    expect(fetchCanonicalMergeCandidateReadinessMock).toHaveBeenCalledTimes(2);
    expect(landedStateQueueSnapshotMock).toHaveBeenCalledTimes(1);
    expect(switchToOperatorsMergingWalletMock).toHaveBeenCalledTimes(1);
    expect(revalidateMock).toHaveBeenCalledTimes(1);
    expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(1);
    expect(buildAndSubmitMergeTxMock).toHaveBeenCalledWith(
      expect.anything(),
      expect.objectContaining({
        stateQueueAddress: "addr_test1statequeue",
        stateQueuePolicyId: "00".repeat(28),
      }),
      fakeContracts,
      expect.objectContaining({
        bypassQueueLengthGuard: false,
        leaseToken: "test-lease-token",
      }),
    );
  });
});

describe("merge follower write permit", () => {
  beforeEach(() => {
    slotAwareDueWorkRegistry.clearAll();
    openedPermits.length = 0;
    for (const mock of [
      landedStateQueueSnapshotMock,
      buildAndSubmitMergeTxMock,
      captureMergeLocalLedgerGateMock,
      fetchCanonicalMergeCandidateReadinessMock,
      finalizeLandedMergesProgramMock,
      tryWithLeaseMock,
      revalidateMock,
      switchToOperatorsMergingWalletMock,
    ])
      mock.mockReset();
    finalizeLandedMergesProgramMock.mockImplementation(() =>
      Effect.succeed([]),
    );
    landedStateQueueSnapshotMock.mockImplementation(
      (_stateQueue: unknown, reason: StateQueueSnapshotReason) =>
        Effect.succeed(makeSnapshot(9, reason)),
    );
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("ready")),
    );
    captureMergeLocalLedgerGateMock.mockImplementation(() =>
      Effect.succeed({ status: "ready" as const }),
    );
    tryWithLeaseMock.mockImplementation(
      (
        _holder: string,
        run: (token: string) => Effect.Effect<unknown, unknown, unknown>,
      ) =>
        Effect.gen(function* () {
          const value = yield* run("test-lease-token");
          return { _tag: "Ran" as const, value };
        }),
    );
    revalidateMock.mockImplementation(() => Effect.void);
  });

  it("runs manual and scheduled merges under a follower write permit", async () => {
    const seen: (FollowerWritePermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );

    const manual = await runMergeAction(true, { gate: "applied" });
    const scheduled = await runMergeAction(false, { gate: "applied" });

    expect(manual).toMatchObject({ status: "merged", trigger: "manual" });
    expect(scheduled).toMatchObject({ status: "merged" });
    expect(openedPermits).toHaveLength(2);
    expect(seen).toEqual(openedPermits);
  });

  it("never lets the fixture bypass a driver's applied view", async () => {
    const seen: (FollowerWritePermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );

    await runMergeAction(true, { gate: "applied", fixture: true });

    expect(seen).toEqual([openedPermits[0]]);
  });

  it("refuses a merge before any L1 work while no driver applied a view", async () => {
    const outcome = await Effect.runPromise(
      Effect.either(mergeActionProgram(true, { gate: "none" })),
    );

    expect(permitRefusalReason(outcome)).toBe(FOLLOWER_VIEW_UNAPPLIED);
    expect(finalizeLandedMergesProgramMock).not.toHaveBeenCalled();
    expect(fetchCanonicalMergeCandidateReadinessMock).not.toHaveBeenCalled();
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
  });

  it.each([
    [
      "the driver is recomputing",
      { gate: "recomputing" },
      DRIVER_RECOMPUTE_PENDING,
    ],
    [
      "a recompute is pending at the gate",
      { gate: "applied", afterOpen: holdFollowerWriteGate("test recompute") },
      DRIVER_RECOMPUTE_PENDING,
    ],
    [
      "another driver took the gate",
      { gate: "applied", afterOpen: openAtFollowerTip },
      FOLLOWER_VIEW_STALE,
    ],
  ] as const)(
    "reports a registration refused before the work as permit-unavailable (%s)",
    async (_case, options, reason) => {
      const outcome = await Effect.runPromise(
        Effect.either(mergeActionProgram(true, options)),
      );

      expect(permitRefusalReason(outcome)).toBe(reason);
      expect(finalizeLandedMergesProgramMock).not.toHaveBeenCalled();
      expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    },
  );

  it("hands the builder a pre-submit check of the lease and the follower write permit", async () => {
    type SubmitCheck = () => Effect.Effect<void, unknown, never>;
    let assertSubmitAuthority: SubmitCheck | undefined;
    const seen: (FollowerWritePermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(
      (
        _lucid: unknown,
        _fetchConfig: unknown,
        _contracts: unknown,
        options: { readonly assertSubmitAuthority?: SubmitCheck },
      ) => {
        assertSubmitAuthority = options.assertSubmitAuthority;
        return recordPermitAndMerge(seen);
      },
    );
    await runMergeAction(true, { gate: "applied" });
    expect(assertSubmitAuthority).toBeDefined();
    const permit = seen[0]!;
    /** The check as the builder runs it (under the merge's permit): its failure. */
    const failure = (withPermit = true) =>
      Effect.runPromise(
        onNodeDatabase(
          assertSubmitAuthority!().pipe(
            withPermit
              ? Effect.provideService(FollowerWrite, permit)
              : identity,
            Effect.flip,
            Effect.option,
          ),
        ),
      ).then(Option.getOrUndefined);
    const message = (error: unknown) =>
      formatUnknownError(error, { includeCause: true });

    // A lost lease refuses before the permit is consulted.
    revalidateMock.mockImplementation(() =>
      Effect.fail(new Error("state-queue lease lost")),
    );
    expect(message(await failure())).toContain("state-queue lease lost");
    expect(revalidateMock).toHaveBeenLastCalledWith("test-lease-token");

    // With the lease held, the check passes while the permit is current.
    revalidateMock.mockImplementation(() => Effect.void);
    expect(await failure()).toBeUndefined();

    // It needs a permit, and refuses one a recompute since superseded.
    expect(message(await failure(false))).toContain(
      "Missing follower write permit",
    );
    await Effect.runPromise(
      onNodeDatabase(holdFollowerWriteGate("test recompute")),
    );
    expect(followerWriteHoldOf(await failure())?.reason).toBe(
      DRIVER_RECOMPUTE_PENDING,
    );
  });

  describe("past the L1 control-plane hold timeout", () => {
    const confirmedHeader = "aa".repeat(28);
    const confirmedTx = "bb".repeat(32);
    type ConfirmedFinalizationHook = (outcome: {
      readonly headerHash: string;
      readonly txHash: string;
      readonly exit: Exit.Exit<void, unknown>;
    }) => Effect.Effect<void>;
    /** Protected finalization takes 190 s, past the 180 s hold, then `exit`. */
    const slowConfirmedFinalization =
      (exit: Exit.Exit<void, unknown>) =>
      (
        _lucid: unknown,
        _fetchConfig: unknown,
        _contracts: unknown,
        options: {
          readonly onConfirmedFinalization?: ConfirmedFinalizationHook;
        },
      ) =>
        Effect.uninterruptible(
          Effect.sleep(Duration.seconds(190)).pipe(
            Effect.zipRight(
              options.onConfirmedFinalization!({
                headerHash: confirmedHeader,
                txHash: confirmedTx,
                exit,
              }),
            ),
            Effect.zipRight(exit),
          ),
        ).pipe(
          Effect.as({
            status: "merged" as const,
            headerHash: confirmedHeader,
            txHash: confirmedTx,
          }),
        );
    const runPastHold = async (force: boolean) => {
      vi.useFakeTimers();
      // Production uses perf_hooks; Vitest replaces only global performance.
      const monotonicClock = vi
        .spyOn(performance, "now")
        .mockImplementation(() => globalThis.performance.now());
      try {
        const outcome = Effect.runPromise(
          Effect.either(mergeActionProgram(force)),
        );
        await vi.advanceTimersByTimeAsync(200_000);
        return await outcome;
      } finally {
        monotonicClock.mockRestore();
        vi.useRealTimers();
      }
    };

    it.each([
      [true, "manual"],
      [false, "threshold"],
    ] as const)(
      "reports a merge whose finalization completed as merged (force=%s)",
      async (force, trigger) => {
        buildAndSubmitMergeTxMock.mockImplementation(
          slowConfirmedFinalization(Exit.void),
        );
        const outcome = await runPastHold(force);
        expect(Either.isRight(outcome) && outcome.right).toMatchObject({
          status: "merged",
          headerHash: confirmedHeader,
          txHash: confirmedTx,
          trigger,
          postMergeSnapshot: { reason: "post_merge" },
        });
      },
    );

    it("reports a failed finalization's own error, not the timeout", async () => {
      buildAndSubmitMergeTxMock.mockImplementation(
        slowConfirmedFinalization(
          Exit.fail(
            new DatabaseError({
              table: "confirmed_merge_finalization",
              message: "confirmed ledger write refused",
              cause: undefined,
            }),
          ),
        ),
      );
      const outcome = await runPastHold(true);
      expect(Either.isLeft(outcome) && outcome.left).toBeInstanceOf(
        DatabaseError,
      );
      expect(
        formatUnknownError(Either.isLeft(outcome) && outcome.left),
      ).toContain("confirmed ledger write refused");
    });

    it("still reports the timeout when no merge was confirmed", async () => {
      buildAndSubmitMergeTxMock.mockImplementation(() => Effect.never);
      const outcome = await runPastHold(true);
      expect(Either.isLeft(outcome) && outcome.left).toBeInstanceOf(
        L1ControlPlaneTimeoutError,
      );
    });
  });

  it("keeps a builder failure's own type across the permit registration", async () => {
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      Effect.fail(new Error("submit refused by the ledger")),
    );

    const outcome = await Effect.runPromise(
      Effect.either(mergeActionProgram(true)),
    );

    expect(Either.isLeft(outcome) && outcome.left).toEqual(
      new Error("submit refused by the ledger"),
    );
  });

  it("merges a targeted header only while it is the oldest queued block", async () => {
    const seen: (FollowerWritePermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );
    const oldest = makeCandidate("ready").readiness.headerHash;

    const other = await runMergeAction(true, {
      expectedHeaderHash: "bb".repeat(28),
    });
    expect(other).toMatchObject({
      status: "skipped_merge_candidate_changed",
      headerHash: oldest,
      reason: `expected_header=${"bb".repeat(28)},oldest_candidate=${oldest}`,
    });
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();

    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(noCandidate),
    );
    expect(
      await runMergeAction(true, { expectedHeaderHash: oldest }),
    ).toMatchObject({
      status: "skipped_merge_candidate_changed",
      reason: `expected_header=${oldest},oldest_candidate=${noCandidate.reason}`,
    });
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();

    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("ready")),
    );
    expect(
      await runMergeAction(true, {
        gate: "applied",
        expectedHeaderHash: oldest,
      }),
    ).toMatchObject({ status: "merged" });
    expect(seen).toEqual(openedPermits);
  });
});
