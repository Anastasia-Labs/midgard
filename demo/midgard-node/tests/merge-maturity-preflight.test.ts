import { performance } from "node:perf_hooks";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Duration,
  Effect,
  Either,
  Layer,
  Logger,
  LogLevel,
  Option,
  Ref,
} from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  FollowerWrite,
  FollowerWriteFixture,
  type FollowerWritePermit,
} from "../src/services/follower-write-gate.js";
import {
  Globals,
  L1ControlPlaneTimeoutError,
  NodeConfig,
} from "../src/services/index.js";
import {
  INTENT_JOURNAL_NO_VIEW,
  IntentJournal,
  IntentJournalWithoutFollower,
} from "../src/services/intent-journal.js";
import {
  LANDED_QUEUE_VISIBILITY_BUDGET_MS,
  type StateQueueSnapshot,
  type StateQueueSnapshotReason,
} from "../src/services/landed-state-queue.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import type { SubmittedMergeTx } from "../src/transactions/state-queue/merge-to-confirmed-state.js";
import { TEST_PLAN } from "./helpers/intent-journal.js";
import { lucidServiceStub } from "./helpers/lucid-service-stub.js";

const landedStateQueueSnapshotMock = vi.hoisted(() => vi.fn());
const requireLandedStateQueueMock = vi.hoisted(() => vi.fn());
const buildAndSubmitMergeTxMock = vi.hoisted(() => vi.fn());
const captureMergeLocalLedgerGateMock = vi.hoisted(() => vi.fn());
const fetchCanonicalMergeCandidateReadinessMock = vi.hoisted(() => vi.fn());
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
    requireLandedStateQueue: requireLandedStateQueueMock,
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
  MergeIntentJournalUnavailable,
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
 * How the merge runs: under an intent journal whose follower has a view
 * (`view`, the default) or under none (`none`, a process with no follower).
 * `fixture` adds the model-fixture capability; `recomputing` marks this
 * process's driver as mid-recompute.
 */
type MergeRunOptions = {
  readonly journal?: "view" | "none";
  readonly fixture?: boolean;
  readonly recomputing?: boolean;
  /** The process's globals, kept across runs; fresh ones by default. */
  readonly globals?: Globals;
};

/** A journal whose follower has a view: every plan opens at `TEST_PLAN`. */
const JournalWithView = Layer.succeed(IntentJournal, {
  ...Effect.runSync(
    Effect.provide(IntentJournal, IntentJournalWithoutFollower),
  ),
  openPlan: Effect.succeed(TEST_PLAN),
});

const mergeActionProgram = ({
  journal = "view",
  fixture = false,
  recomputing = false,
  globals,
}: MergeRunOptions = {}) => {
  const lucidService = lucidServiceStub({
    api: {
      unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
    } as never,
    submitSlotSnapshot: () =>
      Effect.succeed({
        source: "test",
        currentSlot: 2_000_000_000,
        observedAtMs: 0,
        slotLengthMs: 1_000,
      }),
    switchToOperatorsMergingWallet: Effect.sync(() => {
      switchToOperatorsMergingWalletMock();
    }),
  });

  return Effect.gen(function* () {
    if (recomputing)
      yield* Ref.update((yield* Globals).FOLLOWER_WRITE_GATE, (local) => ({
        ...local,
        epoch: "1",
        recomputing: true,
      }));
    const merge = mergeAction();
    return yield* fixture
      ? merge.pipe(Effect.provideService(FollowerWriteFixture, true))
      : merge;
  }).pipe(
    Effect.provide(
      journal === "view" ? JournalWithView : IntentJournalWithoutFollower,
    ),
    Effect.provideService(LucidService, lucidService),
    Effect.provideService(MidgardContracts, fakeContracts as never),
    globals === undefined
      ? Effect.provide(Globals.Default)
      : Effect.provideService(Globals, globals),
    Effect.provide(NodeConfig.layer),
  ) as Effect.Effect<MergeActionResult, unknown, never>;
};

const runMergeAction = (options: MergeRunOptions = {}) =>
  Effect.runPromise(mergeActionProgram(options));

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

const resetMergeMocks = () => {
  slotAwareDueWorkRegistry.clearAll();
  for (const mock of [
    landedStateQueueSnapshotMock,
    requireLandedStateQueueMock,
    buildAndSubmitMergeTxMock,
    captureMergeLocalLedgerGateMock,
    fetchCanonicalMergeCandidateReadinessMock,
    tryWithLeaseMock,
    revalidateMock,
    switchToOperatorsMergingWalletMock,
  ])
    mock.mockReset();
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
};

describe("merge maturity semantic preflight", () => {
  beforeEach(resetMergeMocks);

  it("skips not-mature merges before taking the mutation lease", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_not_mature")),
    );

    const result = await runMergeAction();

    expect(result).toMatchObject({
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

  it("skips DA-unattested candidates before taking the mutation lease", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_unattested")),
    );

    const result = await runMergeAction();

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

    const result = await runMergeAction();

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

    const result = await runMergeAction();

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

    const result = await runMergeAction();

    expect(result).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      reason: "local_submit_ledger_still_behind_after_wait",
    });
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

describe("merge intent journal guard", () => {
  beforeEach(resetMergeMocks);

  it("refuses a merge before any L1 work when no intent journal can record it", async () => {
    const outcome = await Effect.runPromise(
      Effect.either(mergeActionProgram({ journal: "none" })),
    );

    const refusal = Either.isLeft(outcome) ? outcome.left : undefined;
    expect(refusal).toBeInstanceOf(MergeIntentJournalUnavailable);
    expect((refusal as MergeIntentJournalUnavailable).reason).toBe(
      INTENT_JOURNAL_NO_VIEW,
    );
    expect(fetchCanonicalMergeCandidateReadinessMock).not.toHaveBeenCalled();
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
  });

  it("lets only the model fixture run a merge without a journal view", async () => {
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge([]),
    );

    expect(
      await runMergeAction({ journal: "none", fixture: true }),
    ).toMatchObject({ status: "merged" });
  });

  it("runs a merge under a journal view, with no follower write permit, while the driver recomputes", async () => {
    const seen: (FollowerWritePermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );

    const result = await runMergeAction({ recomputing: true });

    expect(result).toMatchObject({ status: "merged", trigger: "threshold" });
    expect(seen).toEqual([null]);
  });

  it("hands the builder a pre-submit check of the state-queue lease alone", async () => {
    type SubmitCheck = () => Effect.Effect<void, unknown, never>;
    let assertSubmitAuthority: SubmitCheck | undefined;
    buildAndSubmitMergeTxMock.mockImplementation(
      (
        _lucid: unknown,
        _fetchConfig: unknown,
        _contracts: unknown,
        options: { readonly assertSubmitAuthority?: SubmitCheck },
      ) => {
        assertSubmitAuthority = options.assertSubmitAuthority;
        return recordPermitAndMerge([]);
      },
    );
    await runMergeAction({ recomputing: true });
    expect(assertSubmitAuthority).toBeDefined();
    /** The check as the builder runs it: its failure. */
    const failure = () =>
      Effect.runPromise(
        assertSubmitAuthority!().pipe(Effect.flip, Effect.option),
      ).then(Option.getOrUndefined);

    revalidateMock.mockImplementation(() =>
      Effect.fail(new Error("state-queue lease lost")),
    );
    expect(
      formatUnknownError(await failure(), { includeCause: true }),
    ).toContain("state-queue lease lost");
    expect(revalidateMock).toHaveBeenLastCalledWith("test-lease-token");

    // With the lease held it passes: no follower write permit is consulted.
    revalidateMock.mockImplementation(() => Effect.void);
    expect(await failure()).toBeUndefined();
  });

  it("awaits the merge's landing in the follower's queue after releasing the L1 control plane, then refreshes the cached queue under a brief hold", async () => {
    const holders: (string | null)[] = [];
    const recordHolder = Effect.flatMap(Globals, (globals) =>
      Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY),
    ).pipe(Effect.map(({ holder }) => holders.push(holder?.scope ?? null)));
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      Effect.zipRight(recordHolder, recordPermitAndMerge([])),
    );
    landedStateQueueSnapshotMock.mockImplementation(
      (_stateQueue: unknown, reason: StateQueueSnapshotReason) =>
        Effect.zipRight(
          reason === "post_merge" ? recordHolder : Effect.void,
          Effect.succeed(makeSnapshot(9, reason)),
        ),
    );

    expect(await runMergeAction()).toMatchObject({
      status: "merged",
      postMergeSnapshot: { reason: "post_merge" },
    });
    // Submission under the hold, the landing wait outside it, the cached
    // queue's refresh under it again.
    expect(holders).toEqual(["state_queue_merge", null, "state_queue_merge"]);
  });

  describe("after a landing wait that failed", () => {
    const submittedHeader = "aa".repeat(28);
    const submittedTx = "bb".repeat(32);
    const budgetMs = LANDED_QUEUE_VISIBILITY_BUDGET_MS;
    const skippedWhileLanding = {
      status: "skipped_merge_landing_pending",
      reason: `awaiting_landing_tx=${submittedTx}`,
      headerHash: submittedHeader,
    };
    // The monotonic clock: it moves only when a test or a landing wait moves it.
    let nowMs = 1_000_000;
    let monotonicClock: { mockRestore: () => void } | undefined;
    beforeEach(() => {
      nowMs = 1_000_000;
      monotonicClock = vi
        .spyOn(performance, "now")
        .mockImplementation(() => nowMs);
    });
    afterEach(() => monotonicClock?.mockRestore());

    /**
     * One process's scheduled ticks: each merge submits `submittedHeader`,
     * and its landing wait takes `waitMs` and fails until `land()`. The
     * first wait runs `duringWait` at its end, before it fails.
     */
    const scheduledTicks = async ({
      waitMs = 0,
      duringWait,
    }: {
      waitMs?: number;
      duringWait?: () => Promise<void>;
    } = {}) => {
      const globals = await Effect.runPromise(
        Effect.provide(Globals, Globals.Default),
      );
      buildAndSubmitMergeTxMock.mockImplementation(() =>
        recordPermitAndMerge([]),
      );
      let landed = false;
      let hook = duringWait;
      landedStateQueueSnapshotMock.mockImplementation(
        (_stateQueue: unknown, reason: StateQueueSnapshotReason) => {
          if (reason !== "post_merge" || landed)
            return Effect.succeed(makeSnapshot(9, reason));
          nowMs += waitMs;
          const during = hook;
          hook = undefined;
          return Effect.promise(async () => during?.()).pipe(
            Effect.zipRight(
              Effect.fail(
                new SDK.StateQueueError({
                  message: "The landed queue never showed the merge",
                  cause: submittedHeader,
                }),
              ),
            ),
          );
        },
      );
      const warnings: string[] = [];
      const recordWarnings = Logger.add(
        Logger.make(({ logLevel, message }) => {
          if (logLevel === LogLevel.Warning) warnings.push(String(message));
        }),
      );
      return {
        tick: () =>
          Effect.runPromise(
            Effect.either(mergeActionProgram({ globals })).pipe(
              Effect.provide(recordWarnings),
            ),
          ),
        /** The follower's landed queue shows these headers. */
        queueShows: (headerHashes: readonly string[]) =>
          requireLandedStateQueueMock.mockImplementation(() =>
            Effect.succeed({
              nodes: headerHashes.map((headerHash) => ({ headerHash })),
            }),
          ),
        land: () => {
          landed = true;
        },
        warnings,
      };
    };
    const expectTimedOut = (result: Either.Either<unknown, unknown>) =>
      expect(Either.isLeft(result) && result.left).toBeInstanceOf(
        SDK.StateQueueError,
      );
    const expectSkipped = (result: Either.Either<unknown, unknown>) =>
      expect(Either.isRight(result) && result.right).toEqual(
        skippedWhileLanding,
      );
    const expectMerged = (result: Either.Either<unknown, unknown>) =>
      expect(Either.isRight(result) && result.right).toMatchObject({
        status: "merged",
      });

    it("skips scheduled merges while its merge's header is still queued, then merges once the queue advances", async () => {
      const { tick, queueShows, land } = await scheduledTicks();

      expectTimedOut(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(1);

      queueShows([submittedHeader]);
      expectSkipped(await tick());
      expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(1);

      queueShows(["cc".repeat(28)]);
      land();
      expectMerged(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(2);
    });

    it("merges again once its merge's header stayed queued for a whole landing-wait budget", async () => {
      const { tick, queueShows, land, warnings } = await scheduledTicks();
      // The merge was dropped: the queue keeps showing its header.
      queueShows([submittedHeader]);

      expectTimedOut(await tick());
      for (const elapsedMs of [0, budgetMs - 1]) {
        nowMs = 1_000_000 + elapsedMs;
        expectSkipped(await tick());
      }
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(1);

      nowMs = 1_000_000 + budgetMs;
      land();
      expectMerged(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(2);
      expect(warnings).toEqual([
        expect.stringMatching(
          new RegExp(`did not land within its budget.*tx=${submittedTx}`),
        ),
      ]);
    });

    it("counts the budget from when a timed-out landing wait failed, not from the submission", async () => {
      // A tick that runs while the wait still runs, here at its end.
      const duringWait: Either.Either<unknown, unknown>[] = [];
      const { tick, queueShows, land } = await scheduledTicks({
        waitMs: budgetMs,
        duringWait: async () => {
          duringWait.push(await tick());
        },
      });
      queueShows([submittedHeader]);

      expectTimedOut(await tick());
      const failedAtMs = nowMs;
      expectSkipped(await tick());
      nowMs = failedAtMs + budgetMs - 1;
      expectSkipped(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(1);
      // Its own wait never expires the record, however long it runs.
      expect(duringWait).toHaveLength(1);
      expectSkipped(duringWait[0]!);

      nowMs = failedAtMs + budgetMs;
      land();
      expectMerged(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(2);
    });

    it("clears the record without a warning when the queue advances after the wait failed", async () => {
      const { tick, queueShows, land, warnings } = await scheduledTicks();
      queueShows([submittedHeader]);
      expectTimedOut(await tick());

      // Landing shows up late, after the budget too: landing wins.
      nowMs += budgetMs * 2;
      queueShows(["cc".repeat(28)]);
      land();
      expectMerged(await tick());
      expect(buildAndSubmitMergeTxMock).toHaveBeenCalledTimes(2);
      expect(warnings).toEqual([]);
    });
  });

  describe("past the L1 control-plane hold timeout", () => {
    const submittedHeader = "aa".repeat(28);
    const submittedTx = "bb".repeat(32);
    /** Submits, then waits 190 s for its confirmation, past the 180 s hold. */
    const slowConfirmation = (
      _lucid: unknown,
      _fetchConfig: unknown,
      _contracts: unknown,
      options: {
        readonly onSubmitted?: (
          submitted: SubmittedMergeTx,
        ) => Effect.Effect<void>;
      },
    ) =>
      options.onSubmitted!({
        headerHash: submittedHeader,
        txHash: submittedTx,
      }).pipe(
        Effect.zipRight(Effect.sleep(Duration.seconds(190))),
        Effect.as({
          status: "merged" as const,
          headerHash: submittedHeader,
          txHash: submittedTx,
        }),
      );
    const runPastHold = async () => {
      vi.useFakeTimers();
      // Production uses perf_hooks; Vitest replaces only global performance.
      const monotonicClock = vi
        .spyOn(performance, "now")
        .mockImplementation(() => globalThis.performance.now());
      try {
        const outcome = Effect.runPromise(Effect.either(mergeActionProgram()));
        await vi.advanceTimersByTimeAsync(200_000);
        return await outcome;
      } finally {
        monotonicClock.mockRestore();
        vi.useRealTimers();
      }
    };

    it("reports a merge submitted before the timeout as merged once it lands", async () => {
      buildAndSubmitMergeTxMock.mockImplementation(slowConfirmation);
      const outcome = await runPastHold();
      expect(Either.isRight(outcome) && outcome.right).toMatchObject({
        status: "merged",
        headerHash: submittedHeader,
        txHash: submittedTx,
        trigger: "threshold",
        postMergeSnapshot: { reason: "post_merge" },
      });
    });

    it("still reports the timeout when no merge was submitted", async () => {
      buildAndSubmitMergeTxMock.mockImplementation(() => Effect.never);
      const outcome = await runPastHold();
      expect(Either.isLeft(outcome) && outcome.left).toBeInstanceOf(
        L1ControlPlaneTimeoutError,
      );
      expect(landedStateQueueSnapshotMock).not.toHaveBeenCalledWith(
        expect.anything(),
        "post_merge",
      );
    });
  });

  it("keeps a builder failure's own type", async () => {
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      Effect.fail(new Error("submit refused by the ledger")),
    );

    const outcome = await Effect.runPromise(
      Effect.either(mergeActionProgram()),
    );

    expect(Either.isLeft(outcome) && outcome.left).toEqual(
      new Error("submit refused by the ledger"),
    );
  });
});
