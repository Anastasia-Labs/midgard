import "./utils.js";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either, Option, Ref } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  HistoryProducer,
  type HistoryProducerPermit,
  UnownedHistoryFixture,
} from "../src/services/event-history-producer.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import type {
  StateQueueSnapshot,
  StateQueueSnapshotReason,
} from "../src/services/state-queue-topology.js";

const fetchStateQueueSnapshotProgramMock = vi.hoisted(() => vi.fn());
const buildAndSubmitMergeTxMock = vi.hoisted(() => vi.fn());
const captureMergeLocalLedgerGateMock = vi.hoisted(() => vi.fn());
const fetchCanonicalMergeCandidateReadinessMock = vi.hoisted(() => vi.fn());
const tryWithLeaseMock = vi.hoisted(() => vi.fn());
const revalidateMock = vi.hoisted(() => vi.fn());

vi.mock("../src/services/state-queue-topology.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/services/state-queue-topology.js")
    >();
  return {
    ...actual,
    fetchStateQueueSnapshotProgram: fetchStateQueueSnapshotProgramMock,
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

vi.mock("../src/database/index.js", async () => {
  const { Effect: EffectModule } = await import("effect");
  return {
    MempoolDB: {
      retrieveTxCount: EffectModule.succeed(0n),
    },
    MutationJobsDB: {
      countUnfinished: EffectModule.succeed(0n),
    },
    TxAdmissionsDB: {
      countBacklog: EffectModule.succeed(0n),
    },
    StateQueueMutationLeasesDB: {
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
  observedAtMs: 1_700_000_000_000 + parsedNodeCount,
  topology: {
    policyUtxoCount: parsedNodeCount,
    parsedNodeCount,
    invalidNodeCount: 0,
    rootCount: 1,
    tailCount: 1,
    initialized: true,
    healthy: true,
    reason: undefined,
  },
  root: {
    outRef: "root#0",
    headerHash: null,
    utxo: {} as StateQueueSnapshot["root"]["utxo"],
  },
  tailCommitBase: {
    outRef: `tail#${parsedNodeCount.toString()}`,
    headerHash: parsedNodeCount <= 1 ? null : "11".repeat(28),
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

const stubPermit: HistoryProducerPermit = {
  token: {
    deploymentIdentity: "33".repeat(32),
    ownerToken: "stub-owner",
    generation: "1",
  },
  coverage: {
    bindingDigest: "44".repeat(32),
    checkpointRevision: "1",
    point: { id: "55".repeat(32), slot: 1 },
    snapshotDigest: "66".repeat(32),
    includedThroughMs: 0,
  },
};

const runProducerMock = vi.fn();

/** Stands in for the node's history owner: registers the producer and hands
 * the work its permit, exactly the contract `runHistoryProducer` relies on. */
const stubHistoryOwner = {
  runProducer: <A, E, R>(
    work: (
      token: HistoryProducerPermit["token"],
      assertCurrent: Effect.Effect<void>,
      coverage: HistoryProducerPermit["coverage"],
    ) => Effect.Effect<A, E, R>,
  ) =>
    Effect.suspend(() => {
      runProducerMock();
      return work(stubPermit.token, Effect.void, stubPermit.coverage);
    }),
};

type MergeRunOptions = {
  readonly historyOwner?: "stub" | "none" | { readonly runProducer: unknown };
  readonly unownedFixture?: boolean;
  readonly expectedHeaderHash?: string;
};

const mergeActionProgram = (
  force: boolean,
  {
    historyOwner = "stub",
    unownedFixture = false,
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

  return Effect.gen(function* () {
    const globals = yield* Globals;
    if (historyOwner !== "none")
      yield* Ref.set(
        globals.EVENT_HISTORY_OWNER,
        (historyOwner === "stub" ? stubHistoryOwner : historyOwner) as never,
      );
    const program = mergeAction(force, { expectedHeaderHash });
    return yield* unownedFixture
      ? program.pipe(Effect.provideService(UnownedHistoryFixture, true))
      : program;
  }).pipe(
    Effect.provideService(LucidService, lucidService),
    Effect.provideService(MidgardContracts, fakeContracts as never),
    Effect.provide(Globals.Default),
    Effect.provide(NodeConfig.layer),
  ) as Effect.Effect<MergeActionResult, unknown, never>;
};

const runMergeAction = (force: boolean, options: MergeRunOptions = {}) =>
  Effect.runPromise(mergeActionProgram(force, options));

/** A builder that reports the history permit it ran under and merges. */
const recordPermitAndMerge = (seen: (HistoryProducerPermit | null)[]) =>
  Effect.gen(function* () {
    const permit = yield* Effect.serviceOption(HistoryProducer);
    seen.push(Option.getOrNull(permit));
    return {
      status: "merged" as const,
      headerHash: "aa".repeat(28),
      txHash: "bb".repeat(32),
    };
  });

describe("merge maturity semantic preflight", () => {
  beforeEach(() => {
    slotAwareDueWorkRegistry.clearAll();
    fetchStateQueueSnapshotProgramMock.mockReset();
    buildAndSubmitMergeTxMock.mockReset();
    captureMergeLocalLedgerGateMock.mockReset();
    fetchCanonicalMergeCandidateReadinessMock.mockReset();
    tryWithLeaseMock.mockReset();
    revalidateMock.mockReset();
    switchToOperatorsMergingWalletMock.mockReset();
    runProducerMock.mockReset();

    fetchStateQueueSnapshotProgramMock.mockImplementation(
      (
        _lucid: unknown,
        _stateQueueAuthValidator: unknown,
        reason: StateQueueSnapshotReason,
      ) => Effect.succeed(makeSnapshot(9, reason)),
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
    expect(fetchStateQueueSnapshotProgramMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("skips DA-unattested candidates before taking the mutation lease", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(makeCandidate("skipped_oldest_block_unattested")),
    );

    const result = await runMergeAction(false);

    expect(result).toMatchObject({
      status: "skipped_oldest_block_unattested",
      // The state-queue node's `da_attestation` is the
      // `DaAvailabilityStateQueueStatus` enum now, so
      // `classifyOldestQueuedBlockReadiness` reports the decoded availability
      // kind under `current_da_availability=` (see `makeCandidate` above,
      // which already builds the wave-current reason string).
      reason: expect.stringContaining("current_da_availability="),
    });
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
    expect(fetchStateQueueSnapshotProgramMock).not.toHaveBeenCalled();
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
    expect(fetchStateQueueSnapshotProgramMock).not.toHaveBeenCalled();
    expect(switchToOperatorsMergingWalletMock).not.toHaveBeenCalled();
  });

  it("keeps non-semantic no-candidate cases on the existing leased planner path", async () => {
    fetchCanonicalMergeCandidateReadinessMock.mockImplementation(() =>
      Effect.succeed(noCandidate),
    );
    fetchStateQueueSnapshotProgramMock.mockImplementation(
      (
        _lucid: unknown,
        _stateQueueAuthValidator: unknown,
        reason: StateQueueSnapshotReason,
      ) => Effect.succeed(makeSnapshot(1, reason)),
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

    const result = await runMergeAction(false);

    expect(result).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      reason: "local_submit_ledger_still_behind_after_wait",
    });
    expect(tryWithLeaseMock).toHaveBeenCalledTimes(1);
    expect(fetchCanonicalMergeCandidateReadinessMock).toHaveBeenCalledTimes(2);
    expect(fetchStateQueueSnapshotProgramMock).toHaveBeenCalledTimes(1);
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

describe("merge history producer permit", () => {
  beforeEach(() => {
    slotAwareDueWorkRegistry.clearAll();
    for (const mock of [
      fetchStateQueueSnapshotProgramMock,
      buildAndSubmitMergeTxMock,
      captureMergeLocalLedgerGateMock,
      fetchCanonicalMergeCandidateReadinessMock,
      tryWithLeaseMock,
      revalidateMock,
      switchToOperatorsMergingWalletMock,
      runProducerMock,
    ])
      mock.mockReset();
    fetchStateQueueSnapshotProgramMock.mockImplementation(
      (
        _lucid: unknown,
        _stateQueueAuthValidator: unknown,
        reason: StateQueueSnapshotReason,
      ) => Effect.succeed(makeSnapshot(9, reason)),
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

  it("runs manual and scheduled merges under the history owner's producer permit", async () => {
    const seen: (HistoryProducerPermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );

    const manual = await runMergeAction(true);
    const scheduled = await runMergeAction(false);

    expect(manual).toMatchObject({ status: "merged", trigger: "manual" });
    expect(scheduled).toMatchObject({ status: "merged" });
    expect(runProducerMock).toHaveBeenCalledTimes(2);
    expect(seen).toEqual([stubPermit, stubPermit]);
  });

  it("never lets the model fixture bypass an acquired history owner", async () => {
    const seen: (HistoryProducerPermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );

    await runMergeAction(true, { unownedFixture: true });

    expect(runProducerMock).toHaveBeenCalledTimes(1);
    expect(seen).toEqual([stubPermit]);
  });

  it("refuses a merge before any L1 work when no history owner exists", async () => {
    const outcome = await Effect.runPromise(
      Effect.either(mergeActionProgram(true, { historyOwner: "none" })),
    );

    expect(Either.isLeft(outcome)).toBe(true);
    const left = Either.isLeft(outcome) ? outcome.left : undefined;
    expect(left).toBeInstanceOf(MergeProducerPermitUnavailable);
    expect(
      formatUnknownError((left as MergeProducerPermitUnavailable).cause, {
        includeCause: true,
      }),
    ).toContain("History owner is not initialized");
    expect(fetchCanonicalMergeCandidateReadinessMock).not.toHaveBeenCalled();
    expect(tryWithLeaseMock).not.toHaveBeenCalled();
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
  });

  it("reports a registration the owner refuses before the work as permit-unavailable", async () => {
    const refusal = new Error("history owner is recovering");
    const outcome = await Effect.runPromise(
      Effect.either(
        mergeActionProgram(true, {
          historyOwner: {
            runProducer: () => Effect.fail(refusal),
          },
        }),
      ),
    );

    const left = Either.isLeft(outcome) ? outcome.left : undefined;
    expect(left).toBeInstanceOf(MergeProducerPermitUnavailable);
    expect(
      formatUnknownError((left as MergeProducerPermitUnavailable).cause, {
        includeCause: true,
      }),
    ).toContain("history owner is recovering");
    expect(buildAndSubmitMergeTxMock).not.toHaveBeenCalled();
  });

  it("keeps the merge's own failure when the owner's trailing currency check also fails", async () => {
    const superseded = new Error("History producer was superseded");
    const trailingGuardOwner = {
      runProducer: <A, E, R>(
        work: (
          token: HistoryProducerPermit["token"],
          assertCurrent: Effect.Effect<void>,
          coverage: HistoryProducerPermit["coverage"],
        ) => Effect.Effect<A, E, R>,
      ) =>
        work(stubPermit.token, Effect.void, stubPermit.coverage).pipe(
          Effect.zipRight(Effect.fail(superseded)),
        ),
    };
    const submitError = new Error("submit refused by the ledger");
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      Effect.fail(submitError),
    );

    const failed = await Effect.runPromise(
      Effect.either(
        mergeActionProgram(true, { historyOwner: trailingGuardOwner }),
      ),
    );
    expect(Either.isLeft(failed) && failed.left).toBe(submitError);

    // A merge that succeeded still reports the trailing check's failure.
    const seen: (HistoryProducerPermit | null)[] = [];
    buildAndSubmitMergeTxMock.mockImplementation(() =>
      recordPermitAndMerge(seen),
    );
    const succeeded = await Effect.runPromise(
      Effect.either(
        mergeActionProgram(true, { historyOwner: trailingGuardOwner }),
      ),
    );
    expect(
      formatUnknownError(Either.isLeft(succeeded) && succeeded.left, {
        includeCause: true,
      }),
    ).toContain("History producer was superseded");
    expect(seen).toEqual([stubPermit]);
  });

  it("hands the builder a pre-submit check of the lease and the producer permit", async () => {
    let assertSubmitAuthority:
      | (() => Effect.Effect<void, unknown, never>)
      | undefined;
    buildAndSubmitMergeTxMock.mockImplementation(
      (
        _lucid: unknown,
        _fetchConfig: unknown,
        _contracts: unknown,
        options: {
          readonly assertSubmitAuthority?: () => Effect.Effect<
            void,
            unknown,
            never
          >;
        },
      ) => {
        assertSubmitAuthority = options.assertSubmitAuthority;
        return recordPermitAndMerge([]);
      },
    );
    await runMergeAction(true);
    expect(assertSubmitAuthority).toBeDefined();
    const check = assertSubmitAuthority!;

    // A lost lease refuses before the permit is consulted.
    revalidateMock.mockImplementation(() =>
      Effect.fail(new Error("state-queue lease lost")),
    );
    const leaseLost = await Effect.runPromise(Effect.either(check()));
    expect(
      formatUnknownError(Either.isLeft(leaseLost) && leaseLost.left, {
        includeCause: true,
      }),
    ).toContain("state-queue lease lost");
    expect(revalidateMock).toHaveBeenLastCalledWith("test-lease-token");

    // With the lease held, the check still needs a producer permit.
    revalidateMock.mockImplementation(() => Effect.void);
    const noPermit = await Effect.runPromise(Effect.either(check()));
    expect(
      formatUnknownError(Either.isLeft(noPermit) && noPermit.left, {
        includeCause: true,
      }),
    ).toContain("Missing producer permit");
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
    const seen: (HistoryProducerPermit | null)[] = [];
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
      await runMergeAction(true, { expectedHeaderHash: oldest }),
    ).toMatchObject({ status: "merged" });
    expect(seen).toEqual([stubPermit]);
  });
});
