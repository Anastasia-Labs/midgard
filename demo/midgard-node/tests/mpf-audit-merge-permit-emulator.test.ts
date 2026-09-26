import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import type { TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect, Either, Exit, Fiber, Option, Ref } from "effect";
import { expect, it, vi } from "vitest";

import { getMergeHandler } from "../src/commands/listen-router.js";
import { type MpfAuditResult, runMpfAudit } from "../src/commands/mpf-audit.js";
import {
  reconcileMergeCompleteProgram,
  type ReconciliationResult,
} from "../src/commands/reconcile.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import {
  ConfirmedLedgerDB,
  MpfEngineStateDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { mergeAction } from "../src/fibers/merge.js";
import { runLedgerPayloadAudit } from "../src/fibers/mpf-payload-audit.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { hydrateLedgerMpfFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import type { NodeConfigDep } from "../src/services/config.js";
import { HistoryProducer } from "../src/services/event-history-producer.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import { fetchStateQueueSnapshotProgram } from "../src/services/state-queue-topology.js";
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
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  Globals,
  LucidService,
  makeGlobalsService,
  materializeConfirmedLedgerSnapshot,
  mergeMaturityWindow,
  MidgardContracts,
  MidgardMpf,
  NodeConfig,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

type LedgerEntries = Parameters<typeof hydrateLedgerMpfFromLedgerEntries>[1];

/**
 * Actual public deposits, commits, confirmations and local finalizations under
 * the production history owner and Architecture G native owner; the merges run
 * through the admin GET /merge handler and `reconcile merge-complete --repair`
 * with no producer permit taken by the test.
 */
it("audits the native MPF root at the committed tip, and merges manually only under the history producer permit", async () => {
  const initial = await openHistoryProductionOwnerLifecycle();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  const { fixture, lucidService } = initial;
  let { globals, production } = h;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);
  const scratch = await mkdtemp(join(tmpdir(), "midgard-audit-leveldb-"));

  // The production node's services, without a producer permit or the
  // unowned-model fixture: exactly what the admin router and the CLI get.
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
      fetchStateQueueSnapshotProgram(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
        "startup",
      ),
    );
  const submit = async (built: { tx: TxSignBuilder }) => {
    const signed = await built.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await wallet.awaitTx(hash)).toBe(true);
    wallet.overrideUTxOs(await wallet.utxosAt(address));
    await h.synchronize();
    return hash;
  };
  const depositCommitConfirmAndFinalize = async (lovelace: bigint) => {
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
    await h.synchronize();
    await runBlockConfirmation(
      globals,
      fixture.contracts,
      lucidService,
      production.nodeConfig,
      production,
    );
    const recovery = await runLocalFinalizationRecoveryWorker(
      globals,
      fixture.contracts,
      lucidService,
      fixture.runtimeOverrides!.deploymentIdentity,
      production.nodeConfig,
      { ...production, globals },
    );
    expect(recovery.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
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
    return { headerHash: tail.headerHash!, endTimeMs: tail.blockEndTimeMs };
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
  const reconcile = (
    headerHash: string,
    repair: boolean,
    overrides: { readonly globals?: Globals } = {},
  ): Promise<ReconciliationResult> =>
    run(
      reconcileMergeCompleteProgram({
        headerHash: Buffer.from(headerHash, "hex"),
        repair,
      }),
      overrides,
    );
  const evidenceDetail = (result: ReconciliationResult, kind: string) =>
    result.evidence.find((entry) => entry.kind === kind)?.detail;
  const mergeJob = (headerHash: string) =>
    run(
      MutationJobsDB.retrieveByJobId(
        MutationJobsDB.confirmedMergeFinalizationJobId(headerHash),
      ),
    );
  const levelDbAt = async (name: string, entries: LedgerEntries) => {
    const path = join(scratch, name);
    const root = await Effect.runPromise(
      Effect.gen(function* () {
        const ledger = yield* MidgardMpf.create("ledger", path);
        return yield* hydrateLedgerMpfFromLedgerEntries(ledger, entries).pipe(
          Effect.ensuring(ledger.close().pipe(Effect.orDie)),
        );
      }),
    );
    return { path, root };
  };
  const offlineAudit = (engine: "overlay" | "architecture_g", path: string) =>
    run(runMpfAudit(), {
      nodeConfig: {
        ...production.nodeConfig,
        MPF_ENGINE: engine,
        LEDGER_MPF_DB_PATH: path,
      },
    });
  const auditWithNativeRoot = (root: string) =>
    run(runMpfAudit({ readNativeDurableRoot: Effect.succeed(root) }));
  // The journal fields the multi-operator and rewind cases rewrite, so each
  // case restores exactly what it changed.
  type JournalFields = {
    readonly status: string;
    readonly base_tail_header_hash: Buffer;
    readonly base_utxos_root: string;
    readonly mpf_replay_base_root: Buffer | null;
    readonly ledger_delta_produced: string;
  };
  const journalFields = async (headerHash: string) =>
    (
      await sqlRun(
        (sql) => sql<JournalFields>`SELECT status, base_tail_header_hash,
          base_utxos_root, mpf_replay_base_root,
          ledger_delta_produced::text AS ledger_delta_produced
          FROM pending_block_finalizations
          WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
      )
    )[0]!;
  const writeJournalFields = (headerHash: string, fields: JournalFields) =>
    sqlRun(
      (sql) => sql`UPDATE pending_block_finalizations SET
        status = ${fields.status},
        base_tail_header_hash = ${fields.base_tail_header_hash},
        base_utxos_root = ${fields.base_utxos_root},
        mpf_replay_base_root = ${fields.mpf_replay_base_root},
        ledger_delta_produced = ${fields.ledger_delta_produced}::text::jsonb
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
    );
  const authorityRow = async () =>
    (
      await sqlRun(
        (sql) => sql<{
          readonly generation: string;
          readonly state: string;
          readonly reason: string;
        }>`SELECT generation::text AS generation, state, reason
          FROM event_history_authority`,
      )
    )[0]!;
  const decodeMergeBody = (response: {
    readonly status: number;
    readonly body: { readonly _tag: string; readonly body?: unknown };
  }) => {
    if (response.body._tag !== "Uint8Array")
      throw new Error(`Unexpected merge body ${response.body._tag}`);
    return JSON.parse(
      new TextDecoder().decode(response.body.body as Uint8Array),
    ) as Record<string, any>;
  };

  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    // --- Bug A: one finalized, unmerged block. -----------------------------
    const first = await depositCommitConfirmAndFinalize(12_000_000n);
    const tipRoot = await nativeDurableRoot();
    const confirmedEntries = await run(ConfirmedLedgerDB.retrieve);
    const firstJournal = await run(
      PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(first.headerHash, "hex"),
      ),
    );
    expect(Option.isSome(firstJournal)).toBe(true);
    const tipSnapshot = await run(
      materializeConfirmedLedgerSnapshot(Option.getOrThrow(firstJournal)),
    );
    expect(tipSnapshot.root).toBe(tipRoot);

    const clean: MpfAuditResult = await run(runLedgerPayloadAudit);
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

    // The overlay/event-flat LevelDB store is at the tip after a commit and at
    // the confirmed ledger after a merge or a restart; both are that store's
    // honest states, and anything else diverges.
    const confirmedStore = await levelDbAt("confirmed", confirmedEntries);
    expect(confirmedStore.root).toBe(clean.confirmedRoot);
    const tipStore = await levelDbAt("tip", tipSnapshot.entries);
    expect(tipStore.root).toBe(tipRoot);
    expect(await offlineAudit("overlay", confirmedStore.path)).toMatchObject({
      matchedPoint: "confirmed",
      diverged: false,
    });
    expect(await offlineAudit("overlay", tipStore.path)).toMatchObject({
      matchedPoint: "tip",
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
    // The same output under another transaction id: a ledger this node never
    // held at either point.
    const foreignOutRef = Buffer.from(tipSnapshot.entries[0]!.outref);
    foreignOutRef[8] ^= 0xff;
    const foreignEntry = { ...tipSnapshot.entries[0]!, outref: foreignOutRef };
    const foreignStore = await levelDbAt("foreign", [
      ...tipSnapshot.entries.slice(1),
      foreignEntry,
    ]);
    expect(await offlineAudit("overlay", foreignStore.path)).toMatchObject({
      diverged: true,
      recomputedRoot: tipRoot,
    });
    expect(await auditHealthy()).toBe(false);
    await acknowledgeCleanAudit();
    // Offline under Architecture G the store is the native owner's, so it must
    // be at the committed tip; the merged ledger alone does not satisfy it.
    expect(
      await offlineAudit("architecture_g", confirmedStore.path),
    ).toMatchObject({ diverged: true });
    expect(await offlineAudit("architecture_g", tipStore.path)).toMatchObject({
      matchedPoint: "tip",
      diverged: false,
    });
    await acknowledgeCleanAudit();

    // --- A second finalized block: the tip walk spans two journals. --------
    const second = await depositCommitConfirmAndFinalize(7_000_000n);
    const twoBlockTip = await nativeDurableRoot();
    expect(await run(runLedgerPayloadAudit)).toMatchObject({
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

    const firstFields = await journalFields(first.headerHash);
    const secondFields = await journalFields(second.headerHash);
    expect(firstFields.status).toBe(
      PendingBlockFinalizationsDB.Status.Finalized,
    );
    expect(secondFields.base_tail_header_hash.toString("hex")).toBe(
      first.headerHash,
    );
    expect(secondFields.base_utxos_root).toBe(tipRoot);

    // A correction rewind abandons the removed block's journal and resets the
    // native root to its base: the tip is the surviving journal again.
    await writeJournalFields(second.headerHash, {
      ...secondFields,
      status: PendingBlockFinalizationsDB.Status.Abandoned,
    });
    expect(await auditWithNativeRoot(tipRoot)).toMatchObject({
      matchedPoint: "tip",
      recomputedRoot: tipRoot,
      tipRoot,
      unmergedJournalCount: 1,
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
    // A native root still at the abandoned block's post-state diverges.
    expect(await auditWithNativeRoot(twoBlockTip)).toMatchObject({
      recomputedRoot: tipRoot,
      diverged: true,
    });
    await writeJournalFields(second.headerHash, secondFields);
    await acknowledgeCleanAudit();

    // A base journal that is no longer finalized cannot anchor its child.
    await writeJournalFields(first.headerHash, {
      ...firstFields,
      status: PendingBlockFinalizationsDB.Status.Abandoned,
    });
    const orphaned = await auditWithNativeRoot(twoBlockTip);
    expect(orphaned).toMatchObject({
      skippedReason: "tip_unverifiable",
      tipJournalRoot: twoBlockTip,
      diverged: false,
    });
    expect(orphaned.tipUnverifiable).toContain(tipRoot);
    await writeJournalFields(first.headerHash, firstFields);
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
      tipJournalRoot: twoBlockTip,
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
    // The rebased journal stays in place through both merges below.
    await writeJournalFields(second.headerHash, rebased);
    await acknowledgeCleanAudit();

    // --- Bug C: manual merges under the running node's producer permit. ----
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(fixture.operatorLucid, second.endTimeMs)
        .readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());

    expect(await reconcile(first.headerHash, false)).toMatchObject({
      status: "pending",
    });
    const notOldest = await reconcile(second.headerHash, true);
    expect(notOldest).toMatchObject({
      status: "blocked",
      repairActions: [],
      nextAction: expect.stringContaining(first.headerHash),
    });
    // A standalone CLI process holds no history owner, so it cannot take the
    // producer permit and must not merge.
    const standalone = await reconcile(first.headerHash, true, {
      globals: await makeGlobalsService(),
    });
    expect(standalone).toMatchObject({
      status: "blocked",
      repairActions: [],
      nextAction: expect.stringContaining("GET /merge"),
    });
    expect(evidenceDetail(standalone, "merge_producer_permit")).toMatchObject({
      available: false,
      reason: expect.stringContaining("History owner is not initialized"),
    });
    const standaloneAdmin = await run(getMergeHandler, {
      globals: await makeGlobalsService(),
    });
    expect(standaloneAdmin.status).toBe(503);
    expect(decodeMergeBody(standaloneAdmin).cause).toContain(
      "History owner is not initialized",
    );
    expect((await queue()).topology.parsedNodeCount).toBe(3);
    expect(await mergeJob(first.headerHash)).toBeUndefined();

    // History recovery revokes the permit after the merge registered: the
    // pre-submit check under the lease refuses before the transaction leaves,
    // since the local finalization after it could no longer write.
    const authorityBefore = await authorityRow();
    expect(authorityBefore.state).toBe("ready");
    let revocations = 0;
    const revokingLucid = {
      ...lucidService,
      switchToOperatorsMergingWallet: Effect.gen(function* () {
        const permit = yield* HistoryProducer;
        yield* Authority.beginRecovery(
          permit.token,
          "test: history recovery began during a merge",
        );
        revocations += 1;
        yield* lucidService.switchToOperatorsMergingWallet;
      }),
    };
    // No follower synchronization after the revocation: the owner would
    // (rightly) refuse to run until its recovery completes.
    let revoked = await run(Effect.either(mergeAction(true)), {
      lucid: revokingLucid,
    });
    for (
      let round = 1;
      round <= 3 &&
      Either.isRight(revoked) &&
      revoked.right.status === "skipped_oldest_block_local_ledger_not_ready";
      round += 1
    ) {
      const dueWork = listSlotAwareDueWork().filter(
        (entry) => entry.kind === "merge_submit_validity",
      );
      expect(dueWork).toHaveLength(1);
      await advanceEmulatorToDueWork(fixture, dueWork[0]!);
      await h.synchronize();
      revoked = await run(Effect.either(mergeAction(true)), {
        lucid: revokingLucid,
      });
    }
    expect(revocations).toBe(1);
    expect(
      Either.isLeft(revoked) &&
        formatUnknownError(revoked.left, { includeCause: true }),
    ).toContain("History authority generation or owner changed");
    expect((await queue()).topology.parsedNodeCount).toBe(3);
    expect(await mergeJob(first.headerHash)).toBeUndefined();
    expect(await authorityRow()).toMatchObject({ state: "recovering" });
    // A node whose history owner is not Ready blocks the repair with evidence.
    const notReady = await reconcile(first.headerHash, true);
    expect(notReady).toMatchObject({ status: "blocked", repairActions: [] });
    expect(evidenceDetail(notReady, "merge_producer_permit")).toMatchObject({
      available: false,
      reason: expect.stringContaining("History source gate is closed"),
    });
    expect((await queue()).topology.parsedNodeCount).toBe(3);
    // An owner that lost its generation stays closed, and its lease cannot be
    // retired under the revoked generation: the node restarts once that lease
    // lapses (expired here rather than waited out).
    h = await initial.restartRuntime({
      afterStop: async () => {
        await sqlRun(
          (sql) => sql`UPDATE event_history_authority
            SET lease_until = clock_timestamp()`,
        );
      },
    });
    ({ globals, production } = h);
    expect(await authorityRow()).toMatchObject({ state: "ready" });

    const repaired = await untilMergeSubmitted(
      () => reconcile(first.headerHash, true),
      (result) => evidenceDetail(result, "merge_result")?.status,
    );
    expect(repaired).toMatchObject({
      status: "repaired",
      repairActions: ["merge_action"],
      nextAction: null,
    });
    expect(evidenceDetail(repaired, "merge_result")).toMatchObject({
      status: "merged",
      trigger: "manual",
    });
    expect(await mergeJob(first.headerHash)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
    });
    expect((await queue()).topology.parsedNodeCount).toBe(2);
    expect(await reconcile(first.headerHash, false)).toMatchObject({
      status: "satisfied",
      nextAction: null,
    });
    // One merged, one (rebased) block still committed on top.
    expect(await run(runLedgerPayloadAudit)).toMatchObject({
      matchedPoint: "tip",
      unmergedJournalCount: 1,
      tipRoot: twoBlockTip,
      confirmedRoot: tipRoot,
      diverged: false,
    });

    // The admin merge of the second block is interrupted (as the L1 control
    // plane's hold timeout would) after the L1 confirmation, while its local
    // finalization is running: the finalization still completes.
    const liveOwner = await Effect.runPromise(
      Ref.get(globals.NATIVE_MPF_OWNER),
    );
    if (liveOwner === undefined) throw new Error("Expected live native owner");
    let signalFinalizing!: () => void;
    const finalizing = new Promise<void>((resolve) => {
      signalFinalizing = resolve;
    });
    let releaseFinalization!: () => void;
    const finalizationReleased = new Promise<void>((resolve) => {
      releaseFinalization = resolve;
    });
    const pausingOwner = new Proxy(liveOwner, {
      get(target, property) {
        if (property === "diagnostics")
          return async () => {
            const job = await mergeJob(second.headerHash);
            if (
              job?.[MutationJobsDB.Columns.STATUS] ===
              MutationJobsDB.Status.Running
            ) {
              signalFinalizing();
              await finalizationReleased;
            }
            return target.diagnostics();
          };
        const value = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    await Effect.runPromise(Ref.set(globals.NATIVE_MPF_OWNER, pausingOwner));
    try {
      let interrupted = false;
      for (let round = 1; round <= 3 && !interrupted; round += 1) {
        await h.synchronize();
        const fiber = Effect.runFork(provideProduction(getMergeHandler));
        const raced = await Promise.race([
          finalizing.then(() => "finalizing" as const),
          Effect.runPromise(Fiber.join(fiber)),
        ]);
        if (raced === "finalizing") {
          let interruptSettled = false;
          const interruption = Effect.runPromise(Fiber.interrupt(fiber)).then(
            (exit) => {
              interruptSettled = true;
              return exit;
            },
          );
          await new Promise((resolve) => setTimeout(resolve, 200));
          expect(interruptSettled).toBe(false);
          releaseFinalization();
          expect(Exit.isInterrupted(await interruption)).toBe(true);
          interrupted = true;
          break;
        }
        const body = decodeMergeBody(raced);
        expect(body.result.status).toBe(
          "skipped_oldest_block_local_ledger_not_ready",
        );
        const dueWork = listSlotAwareDueWork().filter(
          (entry) => entry.kind === "merge_submit_validity",
        );
        expect(dueWork).toHaveLength(1);
        await advanceEmulatorToDueWork(fixture, dueWork[0]!);
      }
      expect(interrupted).toBe(true);
    } finally {
      releaseFinalization();
      await Effect.runPromise(Ref.set(globals.NATIVE_MPF_OWNER, liveOwner));
    }
    await h.synchronize();
    expect(await mergeJob(second.headerHash)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
    });
    expect(
      await run(
        BlocksDB.retrieveTxHashesByHeaderHash(
          Buffer.from(second.headerHash, "hex"),
        ),
      ),
    ).toEqual([]);
    expect(await reconcile(second.headerHash, false)).toMatchObject({
      status: "satisfied",
    });
    expect((await queue()).topology.parsedNodeCount).toBe(1);

    // Fully merged: the confirmed ledger has caught up with the native tip.
    expect(await run(runLedgerPayloadAudit)).toMatchObject({
      persistedRoot: twoBlockTip,
      confirmedRoot: twoBlockTip,
      tipRoot: twoBlockTip,
      unmergedJournalCount: 0,
      diverged: false,
    });
    expect(await auditHealthy()).toBe(true);
  } finally {
    try {
      await h.close();
    } finally {
      await rm(scratch, { recursive: true, force: true });
      vi.useRealTimers();
    }
  }
});
