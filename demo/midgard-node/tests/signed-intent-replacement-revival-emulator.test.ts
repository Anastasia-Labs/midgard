import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Cause, Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { buildBlockConfirmationAction } from "../src/fibers/block-confirmation.js";
import {
  findSignedIntentReplacementIntegrityError,
  signedIntentReplacementDigest,
} from "../src/services/canonical-journal-recovery.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import type { SuccessfulConfirmationOutput } from "../src/workers/utils/confirm-block-commitments.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  runLocalFinalizationRecoveryWorker,
} from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  commitNextBlock,
  finalizeLocally,
  outputOf,
  read,
  readJournal,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  expectReplaced,
  expectUnreplaced,
  type Handle,
  landedCommitView,
  landSignedCommitAsFork,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  nextPoint,
  readDepositHeader,
  readImmutableCounts,
  readLeaseStatus,
  readPlans,
  resetSharedRows,
  seedCorrectionObserver,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
  updateJournal,
} from "./helpers/signed-intent-replacement.js";

/**
 * "Whichever lands wins" when the replaced commit is the one that won: a
 * shallow rollback brings back a chain on which E, replaced by this node,
 * holds its base's state-queue slot after all. Actual deployed validators,
 * the production history owner and Architecture G, and emulator
 * transactions. The emulator cannot roll back, so the winning chain is
 * produced by including E inside its validity window on the emulator after
 * the node replaced it (see landSignedCommitAsFork); the node's history
 * journal is not rolled back, only its authenticated view of the queue
 * changes.
 */

const C = Pending.Columns;
const NOT_A_REPLACEMENT_DIGEST = "cd".repeat(32);

/** The block local finalization replays next, named by its node's asset. */
const availableBlockAssetName = (h: Handle) => {
  const available = Effect.runSync(
    Ref.get(h.globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
  );
  return available === "" ? "" : available.assetName;
};
const nodeAssetName = (header: string) =>
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;

const readGlobals = (h: Handle) => ({
  localFinalizationPending: Effect.runSync(
    Ref.get(h.globals.LOCAL_FINALIZATION_PENDING),
  ),
  unconfirmed: Effect.runSync(
    Ref.get(h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
  ),
});

/** Commit the next block and lose its signed commit; returns its journal and
 * TTL. */
const loseNextCommit = async (
  h: Handle,
  inclusionTime?: number,
  options?: Parameters<typeof submitUnlandedBlock>[2],
) => {
  const lost = await submitUnlandedBlock(
    h as Parameters<typeof submitUnlandedBlock>[0],
    inclusionTime ?? h.fixture.emulator.now() - 1000,
    options,
  );
  const journal = await readJournal(lost.submittedHeaderHash);
  return {
    header: lost.submittedHeaderHash,
    journal,
    ttl: signedTtl(journal[C.SIGNED_TX_CBOR]!),
  };
};

/** The reference inputs of a journal's signed commit, sorted. */
const signedReferenceInputs = (journal: Pending.Record) => {
  const tx = CML.Transaction.from_cbor_bytes(journal[C.SIGNED_TX_CBOR]!);
  const body = tx.body();
  const inputs = body.reference_inputs();
  const refs = Array.from({ length: inputs?.len() ?? 0 }, (_, index) => {
    const input = inputs!.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();
  body.free();
  tx.free();
  return refs;
};

/** Every durable row the revival of a replaced journal or a refusal of it
 * could touch. */
const snapshotRevivalRows = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return {
        journals: yield* sql`SELECT header_hash, status,
          correction_transition_digest, observed_confirmed_at_ms
          FROM pending_block_finalizations ORDER BY header_hash`,
        deposits: yield* sql`SELECT event_id, projected_header_hash
          FROM deposits_utxos ORDER BY event_id`,
        ledger: yield* sql`SELECT root_hex FROM mpf_engine_state
          WHERE store_name = 'ledger'`,
        mempool: yield* sql`SELECT tx_id FROM mempool ORDER BY tx_id`,
        immutable: yield* sql`SELECT tx_id FROM immutable ORDER BY tx_id`,
      };
    }),
  );

it("revives a replaced commit that wins its base slot after all, abandons its unlanded replacement with the same members, never revives it while an admitted correction abandoned it, and locally finalizes the winner once", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const { first, second, txIds } = await admitTwoFundedTransfers(h);
    const depositInclusion = await submitDeposit(h, 12_000_000n);
    // E: both transfers and the deposit, handed to L1 and lost.
    const E = await loseNextCommit(h, depositInclusion);
    expect(
      E.journal.mempoolTxIds.map((id) => id.toString("hex")).sort(),
    ).toEqual(txIds);
    expect(E.journal.depositEventIds).toHaveLength(1);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });

    // NEW_E: the same members on the same base, handed to L1 and lost too.
    // This deliberately skips a production step. The node's pre-lease
    // alignment (alignCommitSchedulerBeforeMutationWorker in
    // src/fibers/block-commitment.ts) would Rewind the scheduler here, since
    // E's TTL sits late in the shift, so a production NEW_E references the
    // refreshed scheduler UTxO. That is harmless: E's validTo is at or before
    // its shift end (schedulerStateCoversCommitTarget), and a refresh's
    // validFrom is at or after it (resolveSchedulerRefreshValidityWindow in
    // src/workers/utils/scheduler-refresh.ts; onchain scheduler.ak
    // validate_end_of_shift_and_get_operators), so a fork that includes E
    // orders the refresh after E. The emulator cannot place an unobserved E
    // before that refresh, so the test skips the alignment to keep E's
    // reference inputs unspent and E landable below.
    const N = await loseNextCommit(h, undefined, { alignScheduler: false });
    // Harness precondition, not a production property: see above.
    expect(signedReferenceInputs(N.journal)).toEqual(
      signedReferenceInputs(E.journal),
    );
    expect(N.header).not.toBe(E.header);
    expect(N.journal[C.BASE_TAIL_OUT_REF]).toBe(E.journal[C.BASE_TAIL_OUT_REF]);
    expect(N.journal[C.BASE_UTXOS_ROOT]).toBe(E.journal[C.BASE_UTXOS_ROOT]);
    expect(
      N.journal.mempoolTxIds.map((id) => id.toString("hex")).sort(),
    ).toEqual(txIds);
    expect(N.journal.depositEventIds).toEqual(E.journal.depositEventIds);

    // A shallow rollback: the chain now followed included E before its TTL.
    await landSignedCommitAsFork(h, E.journal[C.SIGNED_TX_CBOR]!);
    // NEW_E can never land on it: D is spent.
    await expect(
      h.fixture.emulator.submitTx(N.journal[C.SIGNED_TX_CBOR]!.toString("hex")),
    ).rejects.toBeDefined();

    // Same-members negative: while E's journal carries an admitted
    // correction's digest, E holding the slot revives nothing, abandons
    // nothing, and the gate stays closed.
    const untouchedN = await snapshotUnreplaced(N.header);
    const genuine = await updateJournal(E.header, {
      [C.CORRECTION_TRANSITION_DIGEST]: NOT_A_REPLACEMENT_DIGEST,
    });
    if (h.fixture.emulator.slot < N.ttl) moveToExactSlot(h, N.ttl);
    const refused = await snapshotRevivalRows();
    expect(await h.appendTipWhileGateClosed()).toBeDefined();
    await expectUnreplaced(N.header, untouchedN);
    expect(await snapshotRevivalRows()).toEqual(refused);
    expect(await readDepositHeader(depositId)).toBeNull();

    // With its replacement digest, E is the replaced block that won: NEW_E
    // is abandoned and E is revived with its members taken back.
    await updateJournal(E.header, genuine);
    await nextPoint(h);
    const revived = await readJournal(E.header);
    expect(revived[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
    const abandoned = await readJournal(N.header);
    expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      signedIntentReplacementDigest(N.journal),
    );
    expect(await readLocalFinalizationJob(N.header)).toBeUndefined();
    expect(
      await readLeaseStatus(N.journal[C.STATE_QUEUE_LEASE_TOKEN]),
    ).not.toBe("active");
    // Members taken back (kills "skip member re-take on revive").
    expect(await readDepositHeader(depositId)).toBe(E.header);
    // SQL marker at E's candidate root; native replay follows at local
    // finalization from the base.
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      E.journal[C.EXPECTED_UTXOS_ROOT],
    );
    expect(await nativeRoot(h)).toBe(E.journal[C.BASE_UTXOS_ROOT]);
    const globals = readGlobals(h);
    expect(globals.localFinalizationPending).toBe(true);
    expect(availableBlockAssetName(h)).toBe(nodeAssetName(E.header));
    expect(globals.unconfirmed).toBe("");
    const plans = await readPlans();
    expect(
      plans
        .filter(({ state }) => state !== "applied")
        .map(({ state }) => state),
    ).toEqual([]);
    expect(plans.map(({ intent }) => intent.headerHash)).toEqual([
      E.header,
      N.header,
    ]);

    // E is locally finalized once; each member is committed once.
    await finalizeLocally(h, E.header);
    expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );
    await outputOf(h, first, 5_000_000n);
    await outputOf(h, second, 4_000_000n);
    expect((await readJournal(N.header))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

/**
 * Three generations on one base. Here the winning chain is served by the
 * source view alone (landedCommitView): each generation's commit worker ran
 * the fixture's scheduler alignment on the emulator after the previous one
 * was signed, so an earlier generation's reference inputs no longer exist
 * there and the emulator cannot include it. The node's decision reads only
 * that authenticated view; local finalization of the winner against the
 * emulator is covered by the two-generation test above.
 */
it("across generations E, NEW_E, NEW_E2 revives the one that won (NEW_E) and keeps the others replaced", async () => {
  const view = makeRewritableQueueTransport();
  const h = await openHistoryProductionOwnerLifecycle({
    transportFactory: view.transportFactory,
  });
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const E = await loseNextCommit(h, inclusion);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });
    const N = await loseNextCommit(h);
    moveToExactSlot(h, N.ttl);
    await h.synchronize();
    await expectReplaced(N.journal, { handle: h });
    const N2 = await loseNextCommit(h);
    expect(new Set([E.header, N.header, N2.header]).size).toBe(3);
    expect(N2.journal[C.BASE_TAIL_OUT_REF]).toBe(
      E.journal[C.BASE_TAIL_OUT_REF],
    );

    // The chain now followed included NEW_E before its TTL.
    view.setRewrite(landedCommitView(h, N.journal));
    moveToExactSlot(h, N2.ttl);
    await h.synchronize();

    expect((await readJournal(N.header))[C.STATUS]).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    for (const lost of [E, N2]) {
      const journal = await readJournal(lost.header);
      expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
      expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
        signedIntentReplacementDigest(lost.journal),
      );
    }
    expect(await readDepositHeader(depositId)).toBe(N.header);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      N.journal[C.EXPECTED_UTXOS_ROOT],
    );
    expect(await nativeRoot(h)).toBe(N.journal[C.BASE_UTXOS_ROOT]);
    expect(availableBlockAssetName(h)).toBe(nodeAssetName(N.header));
    expect(readGlobals(h).localFinalizationPending).toBe(true);
    expect(await readLocalFinalizationJob(N2.header)).toBeUndefined();
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

/** E's own state-queue node as its signed commit creates it. */
const signedCommitNode = async (h: Handle, journal: Pending.Record) => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const header = journal[C.HEADER_HASH].toString("hex");
  const unit = policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;
  const tx = CML.Transaction.from_cbor_bytes(journal[C.SIGNED_TX_CBOR]!);
  const body = tx.body();
  const outputs = Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  );
  body.free();
  tx.free();
  const outputIndex = outputs.findIndex((output) => output.assets[unit] === 1n);
  expect(outputIndex).toBeGreaterThanOrEqual(0);
  const node = await Effect.runPromise(
    SDK.utxoToStateQueueUTxO(
      {
        ...outputs[outputIndex]!,
        txHash: journal[C.INTENDED_TX_HASH]!.toString("hex"),
        outputIndex,
      },
      policyId,
    ),
  );
  const endTime = (
    await Effect.runPromise(SDK.getHeaderFromStateQueueDatum(node.datum))
  ).endTime;
  return {
    serialized: await Effect.runPromise(serializeStateQueueUTxO(node)),
    endTimeMs: Number(endTime),
  };
};

it("stops with an integrity error and persists nothing when a replaced commit wins its slot after its replacement was locally finalized; a correction-abandoned journal is never revived there", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const E = await loseNextCommit(h, inclusion);
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });
    // NEW_E lands and is locally finalized.
    const next = await commitNextBlock(h);
    await h.synchronize();
    await finalizeLocally(h, next.submittedHeaderHash);
    expect((await readJournal(next.submittedHeaderHash))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );

    // A rollback deeper than NEW_E's local finalization brings E back: the
    // confirmation worker reports E's node on the canonical queue.
    const node = await signedCommitNode(h, E.journal);
    const output: SuccessfulConfirmationOutput = {
      type: "SuccessfulConfirmationOutput",
      latestBlocksUTxO: node.serialized,
      matchedPendingBlocksUTxO: null,
      canonicalHeaders: [
        {
          headerHash: E.header,
          endTimeMs: node.endTimeMs,
          blockUTxO: node.serialized,
        },
      ],
    };
    const confirm = () =>
      h
        .runWithoutSynchronizing(
          buildBlockConfirmationAction(() => Effect.succeed(output)),
        )
        .then(
          () => undefined,
          (error: unknown) => error,
        );

    // Correction-kind negative: never revived, nothing fails.
    const genuine = await updateJournal(E.header, {
      [C.CORRECTION_TRANSITION_DIGEST]: NOT_A_REPLACEMENT_DIGEST,
    });
    const before = await snapshotRevivalRows();
    expect(await confirm()).toBeUndefined();
    expect(await snapshotRevivalRows()).toEqual(before);

    // Replacement kind: the explicit integrity failure, and nothing persisted.
    await updateJournal(E.header, genuine);
    const intact = await snapshotRevivalRows();
    const failure = await confirm();
    expect(failure).toBeDefined();
    const integrity = findSignedIntentReplacementIntegrityError(
      Cause.die(failure),
    );
    expect(integrity?.headerHash).toBe(E.header);
    expect(integrity?.message).toContain(next.submittedHeaderHash);
    expect(await snapshotRevivalRows()).toEqual(intact);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

/** The replaced block E revived with no journal active: its journal observed,
 * its members taken back, its SQL marker at its candidate root, and its own
 * node made available to local finalization. In the running process native
 * state is at its base and no submission is tracked; after a restart the
 * startup hydration tracks its signed commit and the native owner's startup
 * replays the observed journal to its candidate root. */
const expectRevivedWithoutActiveJournal = async (
  h: Handle,
  E: Awaited<ReturnType<typeof loseNextCommit>>,
  depositId: Buffer,
  { restarted = false }: { readonly restarted?: boolean } = {},
) => {
  const revived = await readJournal(E.header);
  expect(revived[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
  expect(await readDepositHeader(depositId)).toBe(E.header);
  expect((await readSqlLedgerRoot()).root_hex).toBe(
    E.journal[C.EXPECTED_UTXOS_ROOT],
  );
  expect(await nativeRoot(h)).toBe(
    E.journal[restarted ? C.EXPECTED_UTXOS_ROOT : C.BASE_UTXOS_ROOT],
  );
  const globals = readGlobals(h);
  expect(globals.localFinalizationPending).toBe(true);
  expect(globals.unconfirmed).toBe(
    restarted ? E.journal[C.INTENDED_TX_HASH]!.toString("hex") : "",
  );
  expect(availableBlockAssetName(h)).toBe(nodeAssetName(E.header));
};

it("revives a landed replaced block only from the history owner's authenticated view while no journal is active, never from the confirmation worker's unauthenticated one, and locally finalizes it once", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const E = await loseNextCommit(h, inclusion);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });

    // The confirmation worker's unauthenticated snapshot reports E's node on
    // the canonical queue while nothing built on E's base has landed. Only
    // the history owner, from its authenticated exact-point view, revives a
    // replaced block. (Kills "revive a replacement-abandoned journal from
    // the unauthenticated view".)
    const node = await signedCommitNode(h, E.journal);
    const output: SuccessfulConfirmationOutput = {
      type: "SuccessfulConfirmationOutput",
      latestBlocksUTxO: node.serialized,
      matchedPendingBlocksUTxO: null,
      canonicalHeaders: [
        {
          headerHash: E.header,
          endTimeMs: node.endTimeMs,
          blockUTxO: node.serialized,
        },
      ],
    };
    const before = await snapshotRevivalRows();
    const outcome = await h
      .runWithoutSynchronizing(
        buildBlockConfirmationAction(() => Effect.succeed(output)),
      )
      .then(
        () => undefined,
        (error: unknown) => error,
      );
    expect(outcome).toBeUndefined();
    expect(await snapshotRevivalRows()).toEqual(before);
    const journal = await readJournal(E.header);
    expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      signedIntentReplacementDigest(E.journal),
    );
    expect(readGlobals(h).localFinalizationPending).toBe(false);

    // Whichever lands wins: the chain followed now included E inside its
    // window, and nothing replaced it on its base, so no journal is active.
    // The history owner, from the queue captured at its checkpoint, revives E
    // exactly once; local finalization replays it natively. (Kills "no
    // revival while no journal is active": E stays abandoned and the commit
    // worker refuses to build on it for good.)
    await landSignedCommitAsFork(h, E.journal[C.SIGNED_TX_CBOR]!);
    await seedCorrectionObserver(h);
    await nextPoint(h);
    await expectRevivedWithoutActiveJournal(h, E, depositId);
    await finalizeLocally(h, E.header);
    expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
    expect((await readJournal(E.header))[C.STATUS]).not.toBe(
      Pending.Status.Abandoned,
    );
    await nextPoint(h);
    expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("revives a replaced block that landed while the node was down, with no journal active at restart, and locally finalizes it once from the recorded node alone", async () => {
  const initial = await openHistoryProductionOwnerLifecycle();
  let h: Handle & Pick<typeof initial, "close"> = initial;
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
    const inclusion = await submitDeposit(initial, 12_000_000n);
    const E = await loseNextCommit(initial, inclusion);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(initial, E.ttl);
    await initial.synchronize();
    await expectReplaced(E.journal, { handle: initial });
    // E lands while the node is down; the correction fiber's cursor sees it.
    await landSignedCommitAsFork(initial, E.journal[C.SIGNED_TX_CBOR]!);
    await seedCorrectionObserver(initial);
    const restarted = await initial.restartRuntime({ synchronize: false });
    h = restarted;
    // Bounded: the restarted owner converges and revives E. (Kills "no
    // revival while no journal is active": the restart keeps E abandoned.)
    await synchronizeWithin(restarted);
    await expectRevivedWithoutActiveJournal(restarted, E, depositId, {
      restarted: true,
    });
    // Local finalization reads only the node the owner recorded, which the
    // startup hydration re-derives from E's signed commit after the revival;
    // no confirmation pass runs first. (Kills "hydrate an observed journal
    // without its node".)
    const finalized = await runLocalFinalizationRecoveryWorker(
      restarted.globals,
      restarted.fixture.contracts,
      restarted.lucidService,
      restarted.fixture.runtimeOverrides!.deploymentIdentity,
      restarted.production.nodeConfig,
      { ...restarted.production, globals: restarted.globals },
    );
    expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
    if (finalized.type === "SuccessfulLocalFinalizationRecoveryOutput")
      expect(finalized.finalizedHeaderHash).toBe(E.header);
    await restarted.synchronize();
    expect(await nativeRoot(restarted)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
