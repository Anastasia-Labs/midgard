import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { Globals, NodeConfig } from "@/services/index.js";
import { Lucid as LucidService } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import type {
  StateQueueSnapshot,
  StateQueueSnapshotReason,
} from "@/services/state-queue-topology.js";

const fetchStateQueueSnapshotProgramMock = vi.hoisted(() => vi.fn());
const buildAndSubmitMergeTxMock = vi.hoisted(() => vi.fn());
const captureMergeLocalLedgerGateMock = vi.hoisted(() => vi.fn());
const fetchCanonicalMergeCandidateReadinessMock = vi.hoisted(() => vi.fn());
const tryWithLeaseMock = vi.hoisted(() => vi.fn());
const revalidateMock = vi.hoisted(() => vi.fn());

vi.mock("@/services/state-queue-topology.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@/services/state-queue-topology.js")>();
  return {
    ...actual,
    fetchStateQueueSnapshotProgram: fetchStateQueueSnapshotProgramMock,
  };
});

vi.mock("@/transactions/state-queue/merge-to-confirmed-state.js", () => ({
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

vi.mock("@/database/index.js", async () => {
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

import { mergeAction, type MergeActionResult } from "@/fibers/merge.js";
import { slotAwareDueWorkRegistry } from "@/fibers/slot-aware-due-work.js";

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
  const currentDaAvailability: SDK.DaAvailabilityStateQueueStatusV1 =
    status === "skipped_oldest_block_unattested"
      ? SDK.NO_DA_ATTESTATION
      : { Published: { terminal_commitment: "22".repeat(32) } };
  const firstBlockOutRef = `${identitySuffix.repeat(64).slice(0, 64)}#0`;
  const candidateIdentity = [
    firstBlockOutRef,
    headerHash,
    SDK.daAvailabilityStateQueueStatusIdentityV1(currentDaAvailability),
    readyAfterUnixTime.toString(),
  ].join("|");
  return {
    status: "candidate" as const,
    confirmedUTxO: {},
    firstBlockUTxO: {},
    blockHeader: {},
    firstBlockNode: {},
    readiness: {
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

const runMergeAction = (force: boolean) => {
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

  return Effect.runPromise(
    mergeAction(force).pipe(
      Effect.provideService(LucidService, lucidService),
      Effect.provideService(MidgardContracts, fakeContracts as never),
      Effect.provide(Globals.Default),
      Effect.provide(NodeConfig.layer),
    ) as Effect.Effect<MergeActionResult, unknown, never>,
  );
};

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
      // `DaAvailabilityStateQueueStatusV1` enum now, so
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
