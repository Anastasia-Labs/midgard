import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either, Metric, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import { commitDaFrameStageGauge } from "../src/fibers/block-commitment.commit-da-frame-readiness.js";
import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.js";
import type { Globals } from "../src/services/globals.js";
import { initialL1ControlPlaneActivity } from "../src/services/globals.l1-control-plane.js";
import {
  activeLivenessReasons,
  COMMIT_DA_FRAME_EVENTS_OVERFLOW,
  COMMIT_DA_FRAME_LEDGER_CEILING,
  COMMIT_DA_FRAME_SOURCE,
  FIBER_HALT_SOURCES,
} from "../src/services/liveness-halt.js";
import { submitDepositOnlyCommit } from "../src/workers/commit-block-header/submission.submit-deposit-only-commit.js";
import { submitTxBackedCommit } from "../src/workers/commit-block-header/submission.submit-tx-backed-commit.js";
import {
  COMMIT_DA_FRAME_FITS_NOTICE,
  COMMIT_DA_FRAME_IDLE_NOTICE,
  commitDaFrameNoticeForOutcome,
} from "../src/workers/utils/commit-block-planner.commit-da-frame-notice.js";
import {
  blockContentFor,
  header,
  mkCandidate,
  MODES,
  REFUSAL,
  runQuietSubmission,
} from "./helpers/commit-da-frame-fixtures.js";

const sizingMode = vi.hoisted(() => ({
  mode: "identity" as "identity" | "zstd",
}));
vi.mock("../src/da/hardening-config.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/da/hardening-config.js")
  >("../src/da/hardening-config.js");
  return {
    ...actual,
    readDaHardeningConfig: () => ({
      ...actual.readDaHardeningConfig(),
      envelopeMode: sizingMode.mode,
    }),
  };
});

// Reach the exact size check in both production submission paths, then fail
// the first journal preparation step. No transaction is signed or submitted.
vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      assertNoUnreconciledSignedSubmission: Effect.void,
    },
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      stampLedgerPayloadAggregate: vi.fn(() =>
        Effect.fail("later journal preparation failure"),
      ),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      retrieveProgramMaterialSidecars: vi.fn((txIds: readonly Buffer[]) =>
        Effect.succeed(
          txIds.map((txId) => ({
            txId,
            sidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
          })),
        ),
      ),
    },
  };
});
vi.mock("../src/workers/commit-block-header/pending-journal.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/workers/commit-block-header/pending-journal.js")
  >("../src/workers/commit-block-header/pending-journal.js");
  return {
    ...actual,
    revalidateStateQueueLease: () => Effect.void,
    assertPendingJournalCompleteness: () => Effect.void,
    resolveLiveTailCommitBase: () => Effect.succeed({}),
  };
});
vi.mock("../src/workers/commit-block-header/build-unsigned-tx.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/workers/commit-block-header/build-unsigned-tx.js")
  >("../src/workers/commit-block-header/build-unsigned-tx.js");
  return {
    ...actual,
    buildUnsignedCommitTx: () =>
      Effect.succeed({
        newHeaderHash: "33".repeat(28),
        newHeader: header,
        newHeaderCbor: Buffer.alloc(0),
        blockEndTimeMs: Number(header.endTime),
        signAndSubmitProgram: Effect.die("must not sign"),
        txSize: 1,
      }),
  };
});
vi.mock(
  "../src/workers/commit-block-header/submission.run-with-stale-operator-wallet-retry.js",
  async () => {
    const actual = await vi.importActual<
      typeof import("../src/workers/commit-block-header/submission.run-with-stale-operator-wallet-retry.js")
    >(
      "../src/workers/commit-block-header/submission.run-with-stale-operator-wallet-retry.js",
    );
    return {
      ...actual,
      refreshCommitUserEventSourcesThroughBlockEnd: () => Effect.void,
      runWithStaleOperatorWalletRetry: ({
        attempt,
      }: {
        attempt: () => Effect.Effect<unknown>;
      }) => attempt(),
    };
  },
);

/**
 * The commit worker's DA frame notices reach /readyz through the liveness
 * reason registry: a block the frame refuses raises a reason, and the next
 * measured tick that fits, or the next tick with no work, clears it.
 * The reason holds no fiber, so the commit loop keeps ticking.
 */

const node = () =>
  ({
    LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
    COMMIT_DA_FRAME_PRESSURE: Ref.unsafeMake(null),
    L1_CONTROL_PLANE_ACTIVITY: Ref.unsafeMake(initialL1ControlPlaneActivity()),
  }) as unknown as Globals;

const reasonsOf = (globals: Globals) =>
  Effect.runSync(activeLivenessReasons(globals)).map(({ source, reason }) => ({
    source,
    reason,
  }));

const refused = (baseEmptyBlockInnerBytes: number) =>
  commitDaFrameNoticeForOutcome({
    outcome: "no_transactions_to_drop",
    passes: 3,
    baseEmptyBlockInnerBytes,
    maxInnerBytes: 100_000,
  })!;

describe("commit DA frame readiness", () => {
  it.each(
    [50, 75, 90].flatMap((stage) =>
      (["fits", "exact_check_required", "incomplete"] as const).map(
        (outcome) => [stage, outcome] as const,
      ),
    ),
  )(
    "projects stage %i on %s without clearing independent failures or provisional refusal",
    (stage, outcome) => {
      const globals = node();
      Effect.runSync(
        Ref.update(
          globals.LIVENESS_REASONS,
          (reasons) =>
            new Map([...reasons, ["commit_worker", "commit_worker_failed"]]),
        ),
      );
      if (outcome !== "fits")
        takeCommitWorkerOutput(globals, refused(100_001), 0);
      const heldReasons = reasonsOf(globals);
      const notice = commitDaFrameNoticeForOutcome({
        outcome,
        passes: 1,
        baseEmptyBlockInnerBytes: 40_000,
        maxInnerBytes: 100_000,
        measurement: {
          innerBytesUpperBound: stage * 1_000,
          acceptedTxCount: 1,
          rejectedTxIds: [],
        },
      })!;
      takeCommitWorkerOutput(globals, notice, 0);
      const pressure = Effect.runSync(
        Ref.get(globals.COMMIT_DA_FRAME_PRESSURE),
      );
      expect(pressure).toMatchObject({
        candidateStagePercent: stage,
        baseLedgerStagePercent: 0,
        requiredWorkInnerBytesUpperBound: null,
        effectiveInnerLimit: 100_000,
      });
      expect(
        Effect.runSync(
          Metric.value(
            Metric.tagged(commitDaFrameStageGauge, "kind", "candidate"),
          ),
        ),
      ).toMatchObject({ value: stage });
      expect(reasonsOf(globals)).toEqual(heldReasons);
      takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_FITS_NOTICE, 0);
      expect(Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE))).toBe(
        pressure,
      );
      expect(
        Effect.runSync(
          Metric.value(
            Metric.tagged(commitDaFrameStageGauge, "kind", "candidate"),
          ),
        ),
      ).toMatchObject({ value: stage });
      expect(reasonsOf(globals)).toEqual([
        { source: "commit_worker", reason: "commit_worker_failed" },
      ]);
      // Only the idle notice of a no-work tick clears the diagnostics.
      takeCommitWorkerOutput(globals, { type: "NothingToCommitOutput" }, 0);
      expect(Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE))).toBe(
        pressure,
      );
      takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_IDLE_NOTICE, 0);
      expect(
        Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE)),
      ).toBeNull();
      expect(
        Effect.runSync(
          Metric.value(
            Metric.tagged(commitDaFrameStageGauge, "kind", "candidate"),
          ),
        ),
      ).toMatchObject({ value: 0 });
      expect(reasonsOf(globals)).toEqual([
        { source: "commit_worker", reason: "commit_worker_failed" },
      ]);
    },
  );

  it("reports the measured required-event floor only after no ordinary transaction remains", () => {
    const globals = node();
    const notice = commitDaFrameNoticeForOutcome({
      outcome: "fits",
      passes: 2,
      baseEmptyBlockInnerBytes: 76_000,
      maxInnerBytes: 100_000,
      measurement: {
        innerBytesUpperBound: 91_000,
        acceptedTxCount: 0,
        rejectedTxIds: [],
      },
    })!;
    takeCommitWorkerOutput(globals, notice, 0);
    expect(
      Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE)),
    ).toMatchObject({
      candidateStagePercent: 90,
      baseLedgerStagePercent: 75,
      requiredWorkInnerBytesUpperBound: 91_000,
      requiredWorkStagePercent: 90,
    });
    expect(reasonsOf(globals)).toEqual([]);
  });

  it.each(
    MODES.flatMap(
      (mode) =>
        [
          [mode, "events"],
          [mode, "transactions"],
        ] as const,
    ),
  )(
    "clears exact admission before a later failure, but retains definite overflow (%s, %s)",
    async (mode, source) => {
      sizingMode.mode = mode;
      const globals = node();
      const notifications: unknown[] = [];
      const afterDaFrameAccepted = Effect.sync(() => {
        notifications.push(COMMIT_DA_FRAME_FITS_NOTICE);
        takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_FITS_NOTICE, 0);
      });
      const content = blockContentFor(
        source === "transactions" ? [mkCandidate(1)] : [],
        { witnessCount: 0 },
      );
      // A nonempty event root takes the event-only helper through its build.
      const deposit = {
        [DepositsDB.Columns.ID]: Buffer.alloc(38, 1),
        [DepositsDB.Columns.INFO]: Buffer.from("d87980", "hex"),
      };
      const params = {
        contracts: {},
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        deploymentMarker: {},
        latestBlock: {},
        endTime: new Date(Number(header.endTime)),
        ...content,
        includedDepositEntries: [deposit],
        includedDepositEventIds: [deposit[DepositsDB.Columns.ID]],
        includedForcedTransactionEventIds: [],
        includedWithdrawalEventIds: [],
        workerInput: {
          data: { mempoolTxsCountSoFar: 0, sizeOfProcessedTxsSoFar: 0 },
        },
        utxoRoot: header.utxosRoot,
        txRoot:
          source === "transactions"
            ? header.transactionsRoot
            : SDK.EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: header.transitionTraceRoot,
        eventToStepRoot: header.eventToStepRoot,
        validationTracesRoot:
          source === "transactions"
            ? header.validationTracesRoot
            : SDK.EMPTY_MERKLE_TREE_ROOT,
        transitionStepCount: source === "transactions" ? 2 : 1,
        validationTraceCount: source === "transactions" ? 1 : 0,
        utxoPayloadEntries: [],
        ledgerDelta: { spent: [], produced: [] },
        selectedBaseUtxosRoot: header.prevUtxosRoot,
        implicitGenesisEntries: [],
        nativeMpfReplay: {},
        transactionsMpf: {},
        mempoolTxHashes: [],
        mempoolTxSourceTable: "mempool",
        sizeOfProcessedTxs: 0,
        afterDaFrameAccepted,
      };
      const attempt = (overflow: boolean) => {
        const supplied = {
          ...params,
          utxoPayloadAggregate: overflow
            ? {
                entryCount: 400_000,
                encodedTupleBytes: maxDaPayloadInnerBytes(mode),
              }
            : content.utxoPayloadAggregate,
        };
        const program =
          source === "events"
            ? submitDepositOnlyCommit(
                supplied as unknown as Parameters<
                  typeof submitDepositOnlyCommit
                >[0],
              )
            : submitTxBackedCommit(
                supplied as unknown as Parameters<
                  typeof submitTxBackedCommit
                >[0],
              );
        return runQuietSubmission(program);
      };
      const measuredRefusal = commitDaFrameNoticeForOutcome({
        outcome: "no_transactions_to_drop",
        passes: 3,
        baseEmptyBlockInnerBytes: 1_000,
        maxInnerBytes: 100_000,
        measurement: {
          innerBytesUpperBound: 105_000,
          acceptedTxCount: 0,
          rejectedTxIds: [],
        },
      })!;
      takeCommitWorkerOutput(globals, measuredRefusal, 0);
      const measuredPressure = Effect.runSync(
        Ref.get(globals.COMMIT_DA_FRAME_PRESSURE),
      );
      expect(measuredPressure).toMatchObject({
        candidateStagePercent: 90,
        candidateInnerBytesUpperBound: 105_000,
      });
      const acceptedThenFailed = await attempt(false);
      expect(acceptedThenFailed).toEqual(
        Either.left("later journal preparation failure"),
      );
      expect(notifications).toEqual([COMMIT_DA_FRAME_FITS_NOTICE]);
      expect(Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE))).toBe(
        measuredPressure,
      );
      expect(
        Effect.runSync(
          Metric.value(
            Metric.tagged(commitDaFrameStageGauge, "kind", "candidate"),
          ),
        ),
      ).toMatchObject({ value: 90 });
      expect(reasonsOf(globals)).toEqual([]);

      takeCommitWorkerOutput(globals, refused(1_000), 0);
      notifications.length = 0;
      const refusedExactly = await attempt(true);
      expect(Either.isLeft(refusedExactly)).toBe(true);
      if (Either.isLeft(refusedExactly))
        expect(refusedExactly.left).toMatchObject({ message: REFUSAL });
      expect(notifications).toEqual([]);
      expect(reasonsOf(globals)).toEqual([
        {
          source: COMMIT_DA_FRAME_SOURCE,
          reason: COMMIT_DA_FRAME_EVENTS_OVERFLOW,
        },
      ]);
    },
  );
  it.each([
    ["events_overflow", 1_000, COMMIT_DA_FRAME_EVENTS_OVERFLOW],
    ["ledger_ceiling", 100_001, COMMIT_DA_FRAME_LEDGER_CEILING],
  ] as const)(
    "raises %s on a refused block and clears it on the next fitting tick",
    (status, baseEmptyBlockInnerBytes, reason) => {
      const globals = node();
      const notice = refused(baseEmptyBlockInnerBytes);
      expect(notice.status).toBe(status);
      // The notice is consumed, not handed on as the worker's output.
      expect(takeCommitWorkerOutput(globals, notice, 0)).toBeUndefined();
      expect(reasonsOf(globals)).toEqual([
        { source: COMMIT_DA_FRAME_SOURCE, reason },
      ]);
      // Every tick that refuses again keeps the one reason.
      takeCommitWorkerOutput(globals, notice, 0);
      expect(reasonsOf(globals)).toHaveLength(1);
      expect(
        takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_FITS_NOTICE, 0),
      ).toBeUndefined();
      expect(reasonsOf(globals)).toEqual([]);
    },
  );

  it("keeps the reason through a nothing-to-commit output", () => {
    const globals = node();
    // Only transactions were pending over an over-frame ledger: the step-down
    // dropped them all, the empty block still overflows, and with no event
    // left the worker commits nothing. The output is handed on and the
    // transactions stay pending, so the reason must stay too.
    takeCommitWorkerOutput(globals, refused(100_001), 0);
    const nothing = { type: "NothingToCommitOutput" } as const;
    expect(takeCommitWorkerOutput(globals, nothing, 0)).toBe(nothing);
    expect(reasonsOf(globals)).toEqual([
      {
        source: COMMIT_DA_FRAME_SOURCE,
        reason: COMMIT_DA_FRAME_LEDGER_CEILING,
      },
    ]);
    // A tick with no work posts the idle notice ahead of its output, which
    // clears the reason and the frame diagnostics.
    takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_IDLE_NOTICE, 0);
    expect(takeCommitWorkerOutput(globals, nothing, 0)).toBe(nothing);
    expect(reasonsOf(globals)).toEqual([]);
    expect(Effect.runSync(Ref.get(globals.COMMIT_DA_FRAME_PRESSURE))).toBe(
      null,
    );
  });

  it("moves between its two reasons without a fitting tick in between", () => {
    const globals = node();
    takeCommitWorkerOutput(globals, refused(1_000), 0);
    takeCommitWorkerOutput(globals, refused(100_001), 0);
    expect(reasonsOf(globals)).toEqual([
      {
        source: COMMIT_DA_FRAME_SOURCE,
        reason: COMMIT_DA_FRAME_LEDGER_CEILING,
      },
    ]);
  });

  it("posts no notice for a fitting outcome's details and none for an unmeasured block", () => {
    expect(
      commitDaFrameNoticeForOutcome({
        outcome: "fits",
        passes: 2,
        baseEmptyBlockInnerBytes: 1_000,
        maxInnerBytes: 100_000,
      }),
    ).toBe(COMMIT_DA_FRAME_FITS_NOTICE);
    expect(
      commitDaFrameNoticeForOutcome({
        outcome: "unmeasured",
        passes: 1,
        baseEmptyBlockInnerBytes: 1_000,
        maxInnerBytes: 100_000,
      }),
    ).toBeUndefined();
  });

  it("holds no fiber, so the commit loop keeps ticking toward the clear", () => {
    for (const sources of Object.values(FIBER_HALT_SOURCES)) {
      expect(sources as readonly string[]).not.toContain(
        COMMIT_DA_FRAME_SOURCE,
      );
    }
  });
});
