/**
 * One production emulator lifecycle (the follower-change driver over the
 * node's follower store) with the node services of
 * `commit-replacement-state-queue.ts`, and the journey steps the
 * state-queue replacement journeys share: commits through the production
 * worker with the follower's journal (landed, or lost before any block),
 * deposits, the journal and database reads they assert through, and one
 * rollback to an ancestor emulator state (the driver's sink sees the rewind
 * and runs its recompute).
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
  retainAndAttestSubmittedHeader,
  runCommitWorker,
} from "../deposit-flow-emulator-shared.js";
import { openNodeServices } from "./commit-replacement-state-queue.js";
import {
  closeLifecycle,
  finalizeLocally,
  nativeRoot,
  readJournal,
  submitDeposit,
  synchronizeBounded,
} from "./correction-admission-scenario.js";
import { dropPendingEmulatorTransaction } from "./emulator-rollback.js";
import { emulatorState } from "./emulator-snapshot.js";
import { openProductionLifecycle } from "./production-lifecycle.js";

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
  const h = await openProductionLifecycle();
  /** `winner` is the landed block the next commit must build on. */
  const state: { winner: string | undefined } = { winner: undefined };
  const node = await openNodeServices(h);
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  await synchronizeBounded(h);
  // The confirmed-ledger frontier starts at genesis: nothing to process.
  expect(await node.processLanded()).toBeUndefined();

  /** One commit attempt through the production worker with the follower's
   * journal, up to its first output that is not a scheduler wait. */
  const commitOutput = async () => {
    const { fixture, globals, production } = h;
    const lucidService = node.nodeLucid;
    for (let attempt = 1; attempt <= 4; attempt += 1) {
      await alignCommitSchedulerBeforeTestWorker({
        fixture,
        lucidService,
        targetEndTimeMs: Date.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      });
      await synchronizeBounded(h);
      await node.followTip();
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
  ) => h.command(Effect.flatMap(SqlClient.SqlClient, read));

  /** A commit whose transaction the emulator loses before any block. */
  const commitUnlanded = async () => {
    const committed = await commit();
    dropPendingEmulatorTransaction(
      h.fixture.emulator,
      committed.submittedTxHash,
    );
    h.observer.forgetDropped(committed.submittedTxHash);
    h.fixture.operatorLucid.clearUTxOOverride();
    await synchronizeBounded(h);
    return journalOf(committed.submittedHeaderHash);
  };

  /** A commit that lands, then the follower run's landed-block processing. */
  const commitLanded = async () => {
    const committed = await commit();
    expect(
      await h.fixture.operatorLucid.awaitTx(committed.submittedTxHash),
    ).toBe(true);
    await synchronizeBounded(h);
    const landed = await journalOf(committed.submittedHeaderHash);
    await node.recordLanding(landed.signed);
    expect(await node.processLanded()).toBeUndefined();
    return landed;
  };

  return {
    h,
    node,
    live: h,
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
      const inclusion = await submitDeposit(h, lovelace);
      await h.deployment.chain.awaitLedgerTime(inclusion + 1000);
      vi.setSystemTime(h.fixture.emulator.now());
      await synchronizeBounded(h);
    },
    /** `journal` won: it landed, the node processed it, it is attested, and
     * it finalizes. The attestation stands in for the DA committee: a block
     * left unattested fences every later append from its end time plus the
     * DA attestation timeout, and the journey outlasts that span. */
    expectWinner: async (journal: Journal) => {
      expect(await node.landedRow(journal.header)).toEqual({
        kind: "own",
        state: "processed",
        applied: true,
      });
      await retainAndAttestSubmittedHeader({
        fixture: h.fixture,
        lucidService: node.nodeLucid,
        globals: h.globals,
        headerHash: journal.header,
        submittedTxHash: journal.txHash,
      });
      await synchronizeBounded(h);
      await finalizeLocally(h, journal.header);
      expect(await nativeRoot(h)).toBe(journal.root);
      state.winner = journal.header;
    },
    /** The current emulator state, to roll back to: nothing pending on L1. */
    ancestor: () => {
      expect(Object.keys(h.fixture.emulator.mempool)).toHaveLength(0);
      return emulatorState(h.fixture.emulator);
    },
    /** Roll the chain back to `ancestor`; the journey continues on the fork.
     * The node synchronizes unless `synchronize` is false (a rollback past a
     * landed block's events waits for its processing). */
    rollBackTo: async (
      ancestor: ReturnType<typeof emulatorState>,
      { synchronize = true } = {},
    ) => {
      await h.rollBackTo(ancestor, { synchronize: false });
      if (synchronize) await synchronizeBounded(h);
    },
    close: async () => {
      await node.close();
      await closeLifecycle(h);
    },
  };
};
