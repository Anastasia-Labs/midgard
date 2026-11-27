import "./helpers/follower-emulator-installed.js";

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import type { TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect, Either, Option, Ref } from "effect";
import { expect, it, vi } from "vitest";

import { type MpfAuditResult, runMpfAudit } from "../src/commands/mpf-audit.js";
import type { ReconciliationResult } from "../src/commands/reconcile.parse-reconciliation-result.js";
import { reconcileMergeCompleteProgram } from "../src/commands/reconcile.reconcile-merge-complete-program.js";
import {
  ConfirmedLedgerDB,
  MpfEngineStateDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import {
  mergeAction,
  MergeIntentJournalUnavailable,
} from "../src/fibers/merge.merge-action.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { ledgerEntryToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import type { NodeConfigDep } from "../src/services/config.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  IntentJournal,
  IntentJournalWithoutFollower,
} from "../src/services/intent-journal.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import { deserializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  BlocksDB,
  ContractDeploymentIdentity,
  Database,
  emulatorStateQueueSnapshot,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  Globals,
  LucidService,
  makeGlobalsService,
  materializeJournalLedger,
  mergeMaturityWindow,
  MidgardContracts,
  MidgardMpf,
  NodeConfig,
  runCommitWorkerUntilSubmitted,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

type LedgerEntries = readonly Ledger.MinimalEntry[];

/**
 * Actual public deposits and commits, each applied when it lands, under
 * the production follower-change driver and Architecture G native owner; the
 * merges run under the node's intent journal, and landed-block processing
 * folds each one.
 */
it("audits the native MPF root at the committed tip, and merges only under an intent journal", async () => {
  const h = await openProductionLifecycle();
  const { fixture, lucidService, globals, production } = h;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);
  const scratch = await mkdtemp(join(tmpdir(), "midgard-audit-leveldb-"));

  // The production node's services under the journal of a process with no
  // follower, without the fixture capability: exactly what the CLI gets.
  type RunOverrides = {
    readonly globals?: Globals;
    readonly nodeConfig?: NodeConfigDep;
    readonly lucid?: unknown;
  };
  const provideProduction = <A, E>(
    effect: Effect.Effect<A, E, any>,
    overrides: RunOverrides = {},
  ) =>
    effect.pipe(
      Effect.provideService(
        LucidService,
        (overrides.lucid ?? lucidService) as any,
      ),
      Effect.provideService(MidgardContracts, fixture.contracts as any),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make(
          fixture.runtimeOverrides!.deploymentIdentity,
        ),
      ),
      Effect.provideService(Globals, overrides.globals ?? globals),
      Effect.provideService(
        NodeConfig,
        overrides.nodeConfig ?? production.nodeConfig,
      ),
      Effect.provideService(MempoolLedgerCache, production.cache),
      Effect.provide(Database.layer),
      Effect.provide(IntentJournalWithoutFollower),
    ) as Effect.Effect<A, E, never>;
  const run = <A, E>(
    effect: Effect.Effect<A, E, any>,
    overrides: RunOverrides = {},
  ): Promise<A> => Effect.runPromise(provideProduction(effect, overrides));
  const sqlRun = <A>(
    statement: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown, any>,
  ): Promise<A> => run(Effect.flatMap(SqlClient.SqlClient, statement));
  const nativeDurableRoot = async () => {
    const owner = await Effect.runPromise(Ref.get(globals.NATIVE_MPF_OWNER));
    if (owner === undefined) throw new Error("Expected live native owner");
    return (await owner.diagnostics()).durableRoot;
  };
  // The ledger audit at the live native owner's durable root.
  const auditNativeRoot = () =>
    runMpfAudit({ readNativeDurableRoot: Effect.promise(nativeDurableRoot) });
  const auditHealthy = async () =>
    Either.isRight(
      await run(Effect.either(MpfEngineStateDB.assertLedgerAuditHealthy)),
    );
  const acknowledgeCleanAudit = async () => {
    const acknowledged = await run(
      runMpfAudit({
        acknowledgeClean: true,
        readNativeDurableRoot: Effect.promise(nativeDurableRoot),
      }),
    );
    expect(acknowledged).toMatchObject({ diverged: false, acknowledged: true });
    expect(await auditHealthy()).toBe(true);
  };
  const queue = () =>
    Effect.runPromise(
      emulatorStateQueueSnapshot(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
        "startup",
      ).pipe(Effect.provide(Database.layer)),
    );
  const submit = async (built: { tx: TxSignBuilder }) => {
    const signed = await built.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await wallet.awaitTx(hash)).toBe(true);
    await h.synchronize();
    return hash;
  };
  const depositCommitAndLand = async (lovelace: bigint) => {
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    const depositTxHash = await submit(
      await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          wallet,
          fixture.contracts,
          {
            l2Address: address,
            l2Datum: null,
            lovelace,
            additionalAssets: {},
            referenceScripts: fixture.referenceScripts.deposit,
          },
        ),
      ),
    );
    const admitted = (
      await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          fixture.operatorLucid,
          SDK.eventHistoryDeploymentFromContracts(histories.deposit),
        ),
      )
    ).filter((deposit) => deposit.utxo.txHash === depositTxHash);
    // The insertion re-emits its list predecessor in the same transaction.
    expect(admitted.length).toBeGreaterThan(0);
    await h.deployment.chain.awaitLedgerTime(
      Math.max(...admitted.map(({ facts }) => Number(facts.inclusion_time))) +
        1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const commit = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      ),
      nodeConfig: production.nodeConfig,
      production: { ...production, globals },
    });
    expect(await fixture.operatorLucid.awaitTx(commit.submittedTxHash)).toBe(
      true,
    );
    await h.applyLanded(commit.submittedHeaderHash);
    const tail = (await queue()).tailCommitBase;
    expect(tail.headerHash).not.toBeNull();
    // Attest before the DA window lapses; an expired unattested block pauses
    // every later commit.
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals,
      headerHash: tail.headerHash!,
    });
    const node = await Effect.runPromise(
      Effect.flatMap(deserializeStateQueueUTxO(tail.utxo), (utxo) =>
        SDK.getStateQueueNodeFromStateQueueDatum(utxo.datum),
      ),
    );
    const blockTimeMs = Number(node.block_time);
    return { headerHash: tail.headerHash!, blockTimeMs };
  };
  // A merge may first register merge-submit due work while the local ledger
  // catches up; advance to it and retry, as the scheduled fiber would.
  const untilMergeSubmitted = async <A>(
    attempt: () => Promise<A>,
    mergeStatus: (value: A) => unknown,
  ): Promise<A> => {
    for (let round = 1; round <= 3; round += 1) {
      await h.synchronize();
      const value = await attempt();
      if (
        mergeStatus(value) !== "skipped_oldest_block_local_ledger_not_ready"
      ) {
        await h.synchronize();
        return value;
      }
      const dueWork = listSlotAwareDueWork().filter(
        (entry) => entry.kind === "merge_submit_validity",
      );
      expect(dueWork).toHaveLength(1);
      await advanceEmulatorToDueWork(fixture, dueWork[0]!);
    }
    throw new Error("Merge did not submit after three rounds");
  };
  const reconcile = (headerHash: string): Promise<ReconciliationResult> =>
    run(
      reconcileMergeCompleteProgram({
        headerHash: Buffer.from(headerHash, "hex"),
      }),
    );
  /** The node tables a merge or its fold writes, as one digest. */
  const nodeStateFingerprint = () =>
    sqlRun((sql) =>
      Effect.forEach(
        [
          "confirmed_ledger",
          "pending_block_finalizations",
          "node_landed_blocks",
          "node_confirmed_merges",
          "blocks",
          "deposits_utxos",
          "state_queue_mutation_leases",
        ],
        (table) =>
          sql<{ readonly digest: string | null }>`SELECT md5(string_agg(
            t::text, ',' ORDER BY t::text)) AS digest FROM ${sql(table)} t`,
      ),
    ).then((rows) => JSON.stringify(rows));
  const ownStatus = (headerHash: string) =>
    run(
      PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
        Buffer.from(headerHash, "hex"),
      ),
    );
  const levelDbAt = async (name: string, entries: LedgerEntries) => {
    const path = join(scratch, name);
    const root = await Effect.runPromise(
      Effect.gen(function* () {
        const ledger = yield* MidgardMpf.create("ledger", path);
        return yield* ledger
          .applyBatch(entries.map(ledgerEntryToInsertBatchOp))
          .pipe(
            Effect.zipRight(ledger.rootHex()),
            Effect.ensuring(ledger.close().pipe(Effect.orDie)),
          );
      }),
    );
    return { path, root };
  };
  const offlineAudit = (path: string) =>
    run(runMpfAudit(), {
      nodeConfig: {
        ...production.nodeConfig,
        LEDGER_MPF_DB_PATH: path,
      },
    });
  const auditWithNativeRoot = (root: string) =>
    run(runMpfAudit({ readNativeDurableRoot: Effect.succeed(root) }));
  // The journal fields the multi-operator and rewind cases rewrite, so each
  // case restores exactly what it changed.
  type JournalFields = {
    readonly base_tail_header_hash: Buffer;
    readonly base_utxos_root: string;
    readonly mpf_replay_base_root: Buffer | null;
    readonly ledger_delta_produced: string;
  };
  const journalFields = async (headerHash: string) =>
    (
      await sqlRun(
        (sql) => sql<JournalFields>`SELECT base_tail_header_hash,
          base_utxos_root, mpf_replay_base_root,
          ledger_delta_produced::text AS ledger_delta_produced
          FROM pending_block_finalizations
          WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
      )
    )[0]!;
  const writeJournalFields = (headerHash: string, fields: JournalFields) =>
    sqlRun(
      (sql) => sql`UPDATE pending_block_finalizations SET
        base_tail_header_hash = ${fields.base_tail_header_hash},
        base_utxos_root = ${fields.base_utxos_root},
        mpf_replay_base_root = ${fields.mpf_replay_base_root},
        ledger_delta_produced = ${fields.ledger_delta_produced}::text::jsonb
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
    );
  /** Sets whether the working ledger took in `headerHash`'s landed block.
   * The block stays landed: an unlanded one whose journaled commit is live
   * would be the live journal, and the audit skips while one exists. */
  const setApplied = (headerHash: string, applied: boolean) =>
    sqlRun(
      (sql) => sql`UPDATE node_landed_blocks SET applied = ${applied}
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
    );
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    // --- Bug A: one finalized, unmerged block. -----------------------------
    const first = await depositCommitAndLand(12_000_000n);
    const tipRoot = await nativeDurableRoot();
    const confirmedEntries = await run(ConfirmedLedgerDB.retrieve);
    const firstJournal = await run(
      PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(first.headerHash, "hex"),
      ),
    );
    expect(Option.isSome(firstJournal)).toBe(true);
    const tipSnapshot = await run(
      materializeJournalLedger(Option.getOrThrow(firstJournal)),
    );
    expect(tipSnapshot.root).toBe(tipRoot);

    const clean: MpfAuditResult = await run(auditNativeRoot());
    expect(clean).toMatchObject({
      persistedRoot: tipRoot,
      recomputedRoot: tipRoot,
      tipRoot,
      matchedPoint: "tip",
      unmergedJournalCount: 1,
      diverged: false,
      entryCount: tipSnapshot.entries.length,
    });
    expect(clean.confirmedRoot).not.toBe(tipRoot);
    expect(clean.tipIntegrityFailure).toBeUndefined();
    expect(await auditHealthy()).toBe(true);

    // A native root that did not advance with the committed block diverges
    // even though it equals the merged ledger.
    const stale = await run(
      runMpfAudit({
        readNativeDurableRoot: Effect.succeed(clean.confirmedRoot),
      }),
    );
    expect(stale).toMatchObject({
      persistedRoot: clean.confirmedRoot,
      recomputedRoot: tipRoot,
      diverged: true,
    });
    expect(stale.matchedPoint).toBeUndefined();
    expect(await auditHealthy()).toBe(false);
    await acknowledgeCleanAudit();

    // Offline, the LevelDB store is the native owner's, so it must be at the
    // committed tip; the merged ledger alone does not satisfy it.
    const confirmedStore = await levelDbAt("confirmed", confirmedEntries);
    expect(confirmedStore.root).toBe(clean.confirmedRoot);
    const tipStore = await levelDbAt("tip", tipSnapshot.entries);
    expect(tipStore.root).toBe(tipRoot);
    expect(await offlineAudit(confirmedStore.path)).toMatchObject({
      diverged: true,
    });
    expect(await auditHealthy()).toBe(false);
    await acknowledgeCleanAudit();
    expect(await offlineAudit(tipStore.path)).toMatchObject({
      matchedPoint: "tip",
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
    // The same output under another transaction id: a ledger this node never
    // held at the tip.
    const foreignOutRef = Buffer.from(tipSnapshot.entries[0]!.outref);
    foreignOutRef[8] ^= 0xff;
    const foreignEntry = { ...tipSnapshot.entries[0]!, outref: foreignOutRef };
    const foreignStore = await levelDbAt("foreign", [
      ...tipSnapshot.entries.slice(1),
      foreignEntry,
    ]);
    expect(await offlineAudit(foreignStore.path)).toMatchObject({
      diverged: true,
      recomputedRoot: tipRoot,
    });
    expect(await auditHealthy()).toBe(false);
    await acknowledgeCleanAudit();

    // --- A second finalized block: the tip walk spans two journals. --------
    const second = await depositCommitAndLand(7_000_000n);
    const twoBlockTip = await nativeDurableRoot();
    expect(await run(auditNativeRoot())).toMatchObject({
      matchedPoint: "tip",
      unmergedJournalCount: 2,
      tipRoot: twoBlockTip,
      confirmedRoot: clean.confirmedRoot,
      diverged: false,
    });
    // One stale step behind the tip diverges.
    expect(await auditWithNativeRoot(tipRoot)).toMatchObject({
      recomputedRoot: twoBlockTip,
      diverged: true,
    });
    await acknowledgeCleanAudit();

    const secondFields = await journalFields(second.headerHash);
    expect(await ownStatus(first.headerHash)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
    );
    expect(secondFields.base_tail_header_hash.toString("hex")).toBe(
      first.headerHash,
    );
    expect(secondFields.base_utxos_root).toBe(tipRoot);

    // A correction takes the second block out of the working ledger (its
    // journal is no longer applied) and resets the native root to its base:
    // the tip is the surviving journal again.
    await setApplied(second.headerHash, false);
    expect(await auditWithNativeRoot(tipRoot)).toMatchObject({
      matchedPoint: "tip",
      recomputedRoot: tipRoot,
      tipRoot,
      unmergedJournalCount: 1,
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
    // A native root still at the unapplied block's post-state diverges.
    expect(await auditWithNativeRoot(twoBlockTip)).toMatchObject({
      recomputedRoot: tipRoot,
      diverged: true,
    });
    await setApplied(second.headerHash, true);
    await acknowledgeCleanAudit();

    // A base journal that is no longer applied cannot anchor its child.
    await setApplied(first.headerHash, false);
    const orphaned = await auditWithNativeRoot(twoBlockTip);
    expect(orphaned).toMatchObject({
      skippedReason: "tip_unverifiable",
      tipCommittedRoot: twoBlockTip,
      diverged: false,
    });
    expect(orphaned.tipUnverifiable).toContain(tipRoot);
    await setApplied(first.headerHash, true);
    await acknowledgeCleanAudit();

    // A tampered ledger delta does not fold to its journal's root.
    // The writer stores the delta as a JSON-encoded jsonb string; keep that
    // encoding so only the one outref differs.
    const storedProduced: unknown = JSON.parse(
      secondFields.ledger_delta_produced,
    );
    const tamperedProduced = (
      typeof storedProduced === "string"
        ? JSON.parse(storedProduced)
        : storedProduced
    ) as { outref: string; output: string }[];
    expect(tamperedProduced.length).toBeGreaterThan(0);
    const flipped = (
      Number.parseInt(tamperedProduced[0]!.outref.slice(16, 18), 16) ^ 0xff
    )
      .toString(16)
      .padStart(2, "0");
    tamperedProduced[0] = {
      ...tamperedProduced[0]!,
      outref: `${tamperedProduced[0]!.outref.slice(0, 16)}${flipped}${tamperedProduced[0]!.outref.slice(18)}`,
    };
    await writeJournalFields(second.headerHash, {
      ...secondFields,
      ledger_delta_produced:
        typeof storedProduced === "string"
          ? JSON.stringify(JSON.stringify(tamperedProduced))
          : JSON.stringify(tamperedProduced),
    });
    const tampered = await auditWithNativeRoot(twoBlockTip);
    expect(tampered).toMatchObject({ diverged: true });
    expect(tampered.tipIntegrityFailure).toContain(
      "does not match its expected root",
    );
    expect(await auditHealthy()).toBe(false);
    await writeJournalFields(second.headerHash, secondFields);
    await acknowledgeCleanAudit();

    // Multi-operator: the second block is rebased onto a foreign block F that
    // has no journal here (own P, foreign F, own B). F left P's ledger as it
    // was, so B's base root is P's post-state and bridges the walk.
    const foreignTail = Buffer.alloc(28, 0xee);
    const rebased = { ...secondFields, base_tail_header_hash: foreignTail };
    await writeJournalFields(second.headerHash, rebased);
    expect(await auditWithNativeRoot(twoBlockTip)).toMatchObject({
      matchedPoint: "tip",
      recomputedRoot: twoBlockTip,
      tipRoot: twoBlockTip,
      unmergedJournalCount: 2,
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
    expect(await auditWithNativeRoot(tipRoot)).toMatchObject({
      recomputedRoot: twoBlockTip,
      diverged: true,
    });
    await acknowledgeCleanAudit();
    // A foreign base this node never held: not reconstructible locally, so
    // the audit neither passes nor fails a native root at our own tip, and
    // still fails any other root.
    const unheldRoot = "ab".repeat(32);
    await writeJournalFields(second.headerHash, {
      ...rebased,
      base_utxos_root: unheldRoot,
      mpf_replay_base_root: Buffer.from(unheldRoot, "hex"),
    });
    const unverifiable = await auditWithNativeRoot(twoBlockTip);
    expect(unverifiable).toMatchObject({
      skippedReason: "tip_unverifiable",
      tipCommittedRoot: twoBlockTip,
      diverged: false,
    });
    expect(unverifiable.tipUnverifiable).toContain(unheldRoot);
    expect(unverifiable.matchedPoint).toBeUndefined();
    expect(await auditHealthy()).toBe(true);
    await expect(
      run(
        runMpfAudit({
          acknowledgeClean: true,
          readNativeDurableRoot: Effect.succeed(twoBlockTip),
        }),
      ),
    ).rejects.toThrow("committed tip is unverifiable");
    expect(await auditWithNativeRoot(tipRoot)).toMatchObject({
      diverged: true,
    });
    await writeJournalFields(second.headerHash, rebased);
    await acknowledgeCleanAudit();

    // --- Bug C: merges go out only under an intent journal, reconciliation
    // never merges, and landed-block processing alone folds each merge.
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(fixture.operatorLucid, second.blockTimeMs)
        .readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());

    // Both blocks are due, yet reconciliation only reports: it names the
    // scheduled merge fiber and writes nothing, on L1 or in the database.
    const beforeReconcile = await nodeStateFingerprint();
    for (const headerHash of [first.headerHash, second.headerHash])
      expect(await reconcile(headerHash)).toMatchObject({
        status: "pending",
        repairActions: [],
        nextAction: expect.stringMatching(
          /scheduled merge fiber.*permissionless/,
        ),
      });
    expect(await nodeStateFingerprint()).toBe(beforeReconcile);
    expect(
      listSlotAwareDueWork().filter(
        (entry) => entry.kind === "merge_submit_validity",
      ),
    ).toEqual([]);
    expect((await queue()).blockCount).toBe(2);

    // A standalone process runs no L1 follower, so no intent journal can
    // record its merge: it refuses before any L1 work.
    const standalone = await run(Effect.either(mergeAction()), {
      globals: await makeGlobalsService(),
    });
    expect(Either.isLeft(standalone) && standalone.left).toBeInstanceOf(
      MergeIntentJournalUnavailable,
    );
    expect((await queue()).blockCount).toBe(2);
    expect(await ownStatus(first.headerHash)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
    );

    // The running node's merge, journaled, over a queue of any length; the
    // synchronize after it folds the block by landed-block processing.
    const mergeOnNode = () =>
      run(
        mergeAction().pipe(
          Effect.provideService(IntentJournal, production.journal),
        ),
        {
          nodeConfig: {
            ...production.nodeConfig,
            MIN_QUEUE_LENGTH_FOR_MERGING: 1,
          },
        },
      );
    expect(
      await untilMergeSubmitted(mergeOnNode, (result) => result.status),
    ).toMatchObject({
      status: "merged",
      headerHash: first.headerHash,
      trigger: "threshold",
    });
    expect(await ownStatus(first.headerHash)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Folded,
    );
    expect((await queue()).blockCount).toBe(1);
    expect(await reconcile(first.headerHash)).toMatchObject({
      status: "satisfied",
      nextAction: null,
    });
    // One merged, one (rebased) block still committed on top.
    expect(await run(auditNativeRoot())).toMatchObject({
      matchedPoint: "tip",
      unmergedJournalCount: 1,
      tipRoot: twoBlockTip,
      confirmedRoot: tipRoot,
      diverged: false,
    });

    // The second block's journal bound to its parent again, for its merge.
    await writeJournalFields(second.headerHash, secondFields);
    expect(
      await untilMergeSubmitted(mergeOnNode, (result) => result.status),
    ).toMatchObject({ status: "merged", headerHash: second.headerHash });
    expect(await ownStatus(second.headerHash)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Folded,
    );
    expect(
      await run(
        BlocksDB.retrieveTxHashesByHeaderHash(
          Buffer.from(second.headerHash, "hex"),
        ),
      ),
    ).toEqual([]);
    expect(await reconcile(second.headerHash)).toMatchObject({
      status: "satisfied",
    });
    expect((await queue()).blockCount).toBe(0);

    // Fully merged: the confirmed ledger has caught up with the native tip.
    expect(await run(auditNativeRoot())).toMatchObject({
      persistedRoot: twoBlockTip,
      confirmedRoot: twoBlockTip,
      tipRoot: twoBlockTip,
      unmergedJournalCount: 0,
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);

    await acknowledgeCleanAudit();
  } finally {
    try {
      await h.close();
    } finally {
      await rm(scratch, { recursive: true, force: true });
      vi.useRealTimers();
    }
  }
});
