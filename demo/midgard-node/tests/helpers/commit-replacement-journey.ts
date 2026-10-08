/**
 * One production-owner emulator lifecycle with the node services of
 * `commit-replacement-state-queue.ts`, and the journey steps the
 * state-queue replacement journeys share: commits through the production
 * worker with the follower's journal (landed, or lost before any block),
 * deposits, the journal and database reads they assert through, and one
 * rollback to an ancestor point (the rollback transport follows one fork).
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import { LANDED_COMMIT_BASE_PENDING } from "../../src/workers/commit-block-header.resolve-commit-base-ledger-entries.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorToDueWork,
  alignCommitSchedulerBeforeTestWorker,
  fetchLatestCommittedBlock,
  runCommitWorker,
} from "../deposit-flow-emulator-shared.js";
import {
  followFork,
  openNodeServices,
} from "./commit-replacement-state-queue.js";
import {
  closeLifecycle,
  finalizeLocally,
  type Lifecycle,
  nativeRoot,
  readJournal,
  submitDeposit,
  synchronizeBounded,
} from "./correction-admission-scenario.js";
import { dropPendingEmulatorTransaction } from "./emulator-rollback.js";
import { emulatorState } from "./emulator-snapshot.js";
import { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import { makeRollbackHistoryTransport } from "./history-rollback-transport.js";

const C = Pending.Columns;

export type Journal = Readonly<{
  header: string;
  status: string;
  base: string;
  baseRoot: string;
  root: string;
  txHash: string;
  signed: Buffer;
  deposits: readonly string[];
  txs: readonly string[];
}>;

export type CommitJourney = Awaited<ReturnType<typeof openCommitJourney>>;

export const openCommitJourney = async () => {
  let source: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
  const h = await openHistoryProductionOwnerLifecycle({
    transportFactory: (recorded) =>
      (source = makeRollbackHistoryTransport(recorded)),
  });
  /** `live` is `h`, or after the rollback, its fork. `winner` is the landed
   * block the next commit must build on. */
  const state: {
    live: Lifecycle;
    observer: Pick<Lifecycle["observer"], "forgetDropped">;
    winner: string | undefined;
  } = { live: h, observer: h.observer, winner: undefined };
  const node = await openNodeServices(h);
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  await synchronizeBounded(h);
  // The confirmed-ledger frontier starts at genesis: nothing to process.
  expect(await node.processLanded()).toBeUndefined();

  /** One commit attempt through the production worker with the follower's
   * journal, up to its first output that is not a scheduler wait. */
  const commitOutput = async () => {
    const { fixture, globals, production } = state.live;
    const lucidService = node.nodeLucid;
    for (let attempt = 1; attempt <= 4; attempt += 1) {
      await alignCommitSchedulerBeforeTestWorker({
        fixture,
        lucidService,
        targetEndTimeMs: Date.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      });
      await synchronizeBounded(state.live);
      await node.mirrorTracked();
      const output = await runCommitWorker(
        fixture.contracts,
        lucidService,
        await fetchLatestCommittedBlock(
          fixture.operatorLucid,
          fixture.contracts,
        ),
        production.nodeConfig,
        fixture.runtimeOverrides!.deploymentIdentity,
        { ...production, globals },
        node.journal,
      );
      if (
        output?.type === "AwaitingCommitBaseOutput" &&
        output.detail === LANDED_COMMIT_BASE_PENDING
      )
        continue;
      if (output?.type !== "RegisteredDueWorkOutput") return output;
      await advanceEmulatorToDueWork(fixture, output.dueWork);
    }
    throw new Error("The commit was not submitted");
  };

  /** One commit through the production worker with the follower's journal. */
  const commit = async () => {
    const output = await commitOutput();
    if (output?.type === "SubmittedAwaitingConfirmationOutput") return output;
    throw new Error(`Unexpected commit output: ${JSON.stringify(output)}`);
  };

  const journalOf = async (headerHash: string): Promise<Journal> => {
    const journal = await readJournal(headerHash);
    return {
      header: headerHash,
      status: journal[C.STATUS],
      base: journal[C.BASE_TAIL_HEADER_HASH].toString("hex"),
      baseRoot: journal[C.BASE_UTXOS_ROOT],
      root: journal[C.EXPECTED_UTXOS_ROOT],
      txHash: journal[C.INTENDED_TX_HASH]!.toString("hex"),
      signed: journal[C.SIGNED_TX_CBOR]!,
      deposits: journal.depositEventIds.map((id) => id.toString("hex")).sort(),
      txs: journal.mempoolTxIds.map((id) => id.toString("hex")).sort(),
    };
  };

  /** Rows of the node's database, read through the running handle. */
  const query = <A>(
    read: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown>,
  ) => state.live.command(Effect.flatMap(SqlClient.SqlClient, read));

  /** A commit whose transaction the emulator loses before any block. */
  const commitUnlanded = async () => {
    const committed = await commit();
    dropPendingEmulatorTransaction(
      h.fixture.emulator,
      committed.submittedTxHash,
    );
    state.observer.forgetDropped(committed.submittedTxHash);
    h.fixture.operatorLucid.clearUTxOOverride();
    await synchronizeBounded(state.live);
    return journalOf(committed.submittedHeaderHash);
  };

  /** A commit that lands, then the follower run's landed-block processing. */
  const commitLanded = async () => {
    const committed = await commit();
    expect(
      await h.fixture.operatorLucid.awaitTx(committed.submittedTxHash),
    ).toBe(true);
    await synchronizeBounded(state.live);
    const landed = await journalOf(committed.submittedHeaderHash);
    await node.recordLanding(landed.signed);
    expect(await node.processLanded()).toBeUndefined();
    return landed;
  };

  return {
    h,
    node,
    source: source!,
    get live() {
      return state.live;
    },
    get winner() {
      return state.winner;
    },
    commitOutput,
    commit,
    commitUnlanded,
    commitLanded,
    journalOf,
    statusOf: async (journal: Journal) =>
      (await journalOf(journal.header)).status,
    query,
    admissionStatus: async (txId: Buffer) =>
      (
        await query(
          (sql) => sql<{ status: string }>`SELECT status FROM tx_admissions
            WHERE tx_id = ${txId}`,
        )
      )[0]?.status,
    /** A deposit, with the ledger past its inclusion time. */
    deposit: async (lovelace: bigint) => {
      const inclusion = await submitDeposit(state.live, lovelace);
      await state.live.deployment.chain.awaitLedgerTime(inclusion + 1000);
      vi.setSystemTime(h.fixture.emulator.now());
      await synchronizeBounded(state.live);
    },
    /** `journal` won: it landed, the node processed it, and it finalizes. */
    expectWinner: async (journal: Journal) => {
      expect(await node.landedRow(journal.header)).toEqual({
        kind: "own",
        state: "processed",
        applied: true,
      });
      await finalizeLocally(state.live, journal.header);
      expect(await nativeRoot(h)).toBe(journal.root);
      state.winner = journal.header;
    },
    /** The current point, to roll back to: nothing pending on L1. */
    ancestor: () => {
      expect(Object.keys(h.fixture.emulator.mempool)).toHaveLength(0);
      const point = source!.points.at(-1)!.point;
      expect(point.slot).toBe(h.fixture.emulator.slot);
      return {
        id: point.id,
        height: point.height,
        state: emulatorState(h.fixture.emulator),
      };
    },
    /** Roll the chain back to `ancestor`; the journey continues on the fork.
     * The history owner synchronizes unless `synchronize` is false (a
     * rollback past a landed block's events waits for its processing). */
    rollBackTo: async (
      ancestor: Parameters<typeof followFork>[0]["ancestor"],
      { synchronize = true } = {},
    ) => {
      ({ fork: state.live, observer: state.observer } = await followFork({
        h,
        source: source!,
        ancestor,
      }));
      if (synchronize) await synchronizeBounded(state.live);
    },
    close: async () => {
      await node.close();
      await closeLifecycle(h);
    },
  };
};
