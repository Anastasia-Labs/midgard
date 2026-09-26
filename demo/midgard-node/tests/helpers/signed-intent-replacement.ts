import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import {
  journalAbandonment,
  signedIntentReplacementDigest,
} from "../../src/services/canonical-journal-recovery.js";
import { commitConfirmRecoverAndMerge } from "../deposit-flow-emulator-shared.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  depositorL2Utxos,
  type Lifecycle,
  read,
  readJournal,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
  submitDeposit,
} from "./correction-rewind-scenario.js";
import { makeStreamingHistoryTransport } from "./history-source-owner-emulator.js";

/** Shared steps of the signed-intent replacement emulator tests ("whichever
 * lands wins"): actual deployed validators, the production history owner and
 * Architecture G, and emulator transactions. */

const C = Pending.Columns;
export const SIGNED_HEADER_RECOVERY_DOMAIN =
  "midgard-history-recovery-intent-v1";
export const UNLANDED: readonly string[] = [
  Pending.Status.PendingSubmission,
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
];

export type Handle =
  | Lifecycle
  | Awaited<ReturnType<Lifecycle["restartRuntime"]>>;

export const resetSharedRows = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
      yield* sql`DELETE FROM event_history_recovery_plans`;
    }),
  );

/** The signed upper validity bound (TTL, exclusive), in slots. */
export const signedTtl = (cbor: Buffer) => {
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const ttl = body.ttl();
  body.free();
  tx.free();
  if (ttl === undefined) throw new Error("A commit is signed with a TTL");
  return Number(ttl);
};

export const readPlans = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ state: string; intent: string }>`
        SELECT state, intent FROM event_history_recovery_plans
        ORDER BY created_at`;
      return rows.map((row) => ({
        state: row.state,
        intent: JSON.parse(row.intent) as {
          domain: string;
          headerHash: string;
          signedTransactionHash: string;
          targetRoot: string;
        },
      }));
    }),
  );

export const readLeaseStatus = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ status: string }>`
        SELECT status FROM state_queue_mutation_leases WHERE token = ${token}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.status;
    }),
  );

/** A commit process killed after handing its block to L1 never releases its
 * state-queue mutation lease. */
export const holdLeaseAsCrashed = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'active', released_at = NULL,
          expires_at = NOW() + INTERVAL '10 minutes'
        WHERE token = ${token} RETURNING token`;
      expect(updated).toHaveLength(1);
    }),
  );

export const retireCrashedLease = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'released', released_at = NOW()
        WHERE token = ${token} AND status = 'active'`;
    }),
  );

/** Two independent L2 outputs funded through a merged deposit block, each
 * spent by an admitted L2 transfer that is not committed yet. */
export const admitTwoFundedTransfers = async (lifecycle: Lifecycle) => {
  const h = lifecycle;
  const funding = [
    await submitDeposit(h, 20_000_000n),
    await submitDeposit(h, 9_000_000n),
  ];
  await lifecycle.deployment.chain.awaitLedgerTime(Math.max(...funding) + 1000);
  await nextPoint(h);
  await commitConfirmRecoverAndMerge({
    fixture: h.fixture,
    lucidService: h.lucidService,
    globals: h.globals,
    production: h.production,
  });
  await h.synchronize();
  const byAmount = async (lovelace: bigint) => {
    const found = (await depositorL2Utxos(lifecycle)).filter(
      (utxo) => utxo.assets.lovelace === lovelace,
    );
    expect(found).toHaveLength(1);
    return found[0]!;
  };
  const first = await buildDepositorTransfer(
    lifecycle,
    [await byAmount(20_000_000n)],
    5_000_000n,
  );
  const second = await buildDepositorTransfer(
    lifecycle,
    [await byAmount(9_000_000n)],
    4_000_000n,
  );
  expect(await admitTransfer(lifecycle, first)).toBe("accepted");
  expect(await admitTransfer(lifecycle, second)).toBe("accepted");
  const txIds = [first.txId, second.txId]
    .map((id) => id.toString("hex"))
    .sort();
  return { first, second, txIds };
};

/** Direct journal surgery; returns the previous values. */
export const updateJournal = (
  headerHash: string,
  fields: Readonly<Record<string, unknown>>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const key = Buffer.from(headerHash, "hex");
      const rows = yield* sql<Record<string, unknown>>`
        SELECT * FROM pending_block_finalizations WHERE header_hash = ${key}`;
      expect(rows).toHaveLength(1);
      const before = Object.fromEntries(
        Object.keys(fields).map((column) => [column, rows[0]![column]]),
      );
      const updated = yield* sql`UPDATE pending_block_finalizations
        SET ${sql.update(fields as Record<string, never>)}
        WHERE header_hash = ${key} RETURNING header_hash`;
      expect(updated).toHaveLength(1);
      return before;
    }),
  );

/** Raw journal columns: a journal whose signed bytes do not decode is
 * refused by the canonical record loader, so it is read column by column. */
export const readJournalColumns = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        status: string;
        correction_transition_digest: string | null;
      }>`SELECT status, correction_transition_digest
        FROM pending_block_finalizations
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

export const readMempoolTxIds = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool`;
      return rows.map((row) => row.tx_id.toString("hex")).sort();
    }),
  );

/** How many times each transaction is committed locally. */
export const readImmutableCounts = (txIds: readonly string[]) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM immutable
        WHERE tx_id IN ${sql.in(txIds.map((id) => Buffer.from(id, "hex")))}`;
      return Object.fromEntries(
        txIds.map((id) => [
          id,
          rows.filter((row) => row.tx_id.toString("hex") === id).length,
        ]),
      );
    }),
  );

export const readDepositHeader = (eventId: Buffer) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ projected_header_hash: Buffer | null }>`
        SELECT projected_header_hash FROM deposits_utxos
        WHERE event_id = ${eventId}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.projected_header_hash?.toString("hex") ?? null;
    }),
  );

export const nativeRoot = async (handle: Pick<Handle, "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The next authenticated source point, one L1 block later. */
export const nextPoint = async (handle: Handle) => {
  handle.fixture.emulator.awaitBlock(1);
  vi.setSystemTime(new Date(handle.fixture.emulator.now()));
  await handle.synchronize();
};

/** Advance L1 (and the faked wall clock) to `slot` without appending any
 * source point. */
export const advanceL1ToSlot = (handle: Handle, slot: number) => {
  const delta = slot - handle.fixture.emulator.slot;
  expect(delta).toBeGreaterThan(0);
  handle.fixture.emulator.awaitSlot(delta);
  vi.setSystemTime(new Date(handle.fixture.emulator.now()));
};

/** Move L1 to exactly `slot` so the next sealed source point's head is that
 * slot. The emulator derives block height from the slot, so a point inside
 * the last point's height band gets the next height; transport heights are
 * synthetic anyway. Nothing may be pending in the emulator. */
export const moveToExactSlot = (handle: Handle, slot: number) => {
  const { emulator } = handle.fixture;
  expect(Object.keys(emulator.mempool)).toHaveLength(0);
  advanceL1ToSlot(handle, slot);
  const last = handle.batches.at(-1)!;
  expect(slot).toBeGreaterThan(last.observedSlot);
  emulator.blockHeight = Math.max(
    emulator.blockHeight,
    last.observedHeight + 1,
  );
};

/** Wait (real time) for a restarted owner to open its gate without any new
 * source point after the restart. */
export const awaitOwnerReady = async (handle: Handle) => {
  const deadline = performance.now() + 120_000;
  for (;;) {
    const frontier = Effect.runSync(handle.production.owner.frontier);
    if (frontier.ready) return frontier;
    if (performance.now() >= deadline)
      throw new Error(
        `Restarted history owner never became ready: ${JSON.stringify(frontier)}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

export const snapshotUnreplaced = async (headerHash: string) => {
  const journal = await readJournalColumns(headerHash);
  return {
    status: journal.status,
    correction: journal.correction_transition_digest,
    job: (await readLocalFinalizationJob(headerHash)) !== undefined,
    plans: await readPlans(),
    sqlRoot: (await readSqlLedgerRoot()).root_hex,
  };
};

/** The signed-intent journal is exactly as it was: still active, nothing
 * replaced. */
export const expectUnreplaced = async (
  headerHash: string,
  before: Awaited<ReturnType<typeof snapshotUnreplaced>>,
) => {
  const now = await snapshotUnreplaced(headerHash);
  expect(now).toEqual(before);
  expect(UNLANDED).toContain(now.status);
};

/** The durable and in-memory state of a replaced journal: abandoned under
 * its replacement digest (its signed content kept), its job row and lease
 * retired, the native root and SQL marker back at its base, its members
 * reopened. `globalsReset` is false when a revival followed in the same
 * reconciliation. */
export const expectReplaced = async (
  journal: Pending.Record,
  { globalsReset = true, handle }: { globalsReset?: boolean; handle: Handle },
) => {
  const header = journal[C.HEADER_HASH].toString("hex");
  const replaced = await readJournal(header);
  expect(replaced[C.STATUS]).toBe(Pending.Status.Abandoned);
  expect(replaced[C.CORRECTION_TRANSITION_DIGEST]).toBe(
    signedIntentReplacementDigest(journal),
  );
  expect(journalAbandonment(replaced)).toBe("replacement");
  expect(replaced[C.SIGNED_TX_CBOR]).toEqual(journal[C.SIGNED_TX_CBOR]);
  expect(replaced[C.INTENDED_TX_HASH]).toEqual(journal[C.INTENDED_TX_HASH]);
  const plans = await readPlans();
  const plan = plans.find(({ intent }) => intent.headerHash === header);
  expect(plan?.state).toBe("applied");
  expect(plan?.intent.domain).toBe(SIGNED_HEADER_RECOVERY_DOMAIN);
  expect(plan?.intent.targetRoot).toBe(journal[C.BASE_UTXOS_ROOT]);
  expect(await readLocalFinalizationJob(header)).toBeUndefined();
  expect(await readLeaseStatus(journal[C.STATE_QUEUE_LEASE_TOKEN])).not.toBe(
    "active",
  );
  if (!globalsReset) return;
  expect(await nativeRoot(handle)).toBe(journal[C.BASE_UTXOS_ROOT]);
  expect((await readSqlLedgerRoot()).root_hex).toBe(journal[C.BASE_UTXOS_ROOT]);
  const g = handle.globals;
  expect(Effect.runSync(Ref.get(g.LOCAL_FINALIZATION_PENDING))).toBe(false);
  expect(Effect.runSync(Ref.get(g.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH))).toBe(
    "",
  );
  expect(Effect.runSync(Ref.get(g.AVAILABLE_LOCAL_FINALIZATION_BLOCK))).toBe(
    "",
  );
  const mempool = await readMempoolTxIds();
  for (const id of journal.mempoolTxIds)
    expect(mempool).toContain(id.toString("hex"));
  for (const eventId of journal.depositEventIds)
    expect(await readDepositHeader(eventId)).toBeNull();
};

/**
 * Lands a signed commit on the emulator as a fork on which it was included
 * inside its validity window would have it: the emulator cannot roll back,
 * so its slot is moved back to `ttl - 1` for the submission alone (where the
 * ledger rules and the state-queue validator accept it) and then restored;
 * the next emulator block includes it. The node's source then serves that
 * chain, so its authenticated view shows the commit holding its base's
 * state-queue slot, as after a shallow rollback to that fork. The history
 * journal itself is not rolled back.
 */
export const landSignedCommitAsFork = async (
  handle: Handle,
  signedCbor: Buffer,
) => {
  const { emulator } = handle.fixture;
  const ttl = signedTtl(signedCbor);
  const saved = {
    slot: emulator.slot,
    time: emulator.time,
    blockHeight: emulator.blockHeight,
  };
  expect(saved.slot).toBeGreaterThanOrEqual(ttl);
  emulator.slot = ttl - 1;
  emulator.time = saved.time - (saved.slot - emulator.slot) * 1000;
  let txHash: string;
  try {
    txHash = await emulator.submitTx(signedCbor.toString("hex"));
  } finally {
    emulator.slot = saved.slot;
    emulator.time = saved.time;
    emulator.blockHeight = saved.blockHeight;
  }
  expect(await handle.fixture.operatorLucid.awaitTx(txHash)).toBe(true);
  expect(
    (await handle.fixture.operatorLucid.transactionStatus(txHash)).status,
  ).toBe("confirmed");
  vi.setSystemTime(new Date(emulator.now()));
  return txHash;
};

/** The state-queue outputs a signed commit creates (its base's continuation
 * and its own node), as a chain that included it would serve them. */
export const signedCommitQueueOutputs = (
  h: Pick<Handle, "fixture">,
  journal: Pending.Record,
): LedgerSnapshotOutput[] => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const tx = CML.Transaction.from_cbor_bytes(journal[C.SIGNED_TX_CBOR]!);
  const body = tx.body();
  const outputs = Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  );
  body.free();
  tx.free();
  const txHash = journal[C.INTENDED_TX_HASH]!.toString("hex");
  return outputs.flatMap((output, outputIndex) =>
    Object.keys(output.assets).some((unit) => unit.startsWith(policyId))
      ? [
          {
            txHash,
            outputIndex,
            address: output.address,
            assets: { ...output.assets },
            ...(output.datum == null ? {} : { datum: output.datum }),
            hasReferenceScript: output.scriptRef != null,
          },
        ]
      : [],
  );
};

/** The queue view of a chain on which `journal`'s signed commit took its
 * base's slot: the base output is replaced by the commit's queue outputs. */
export const landedCommitView = (
  h: Pick<Handle, "fixture">,
  journal: Pending.Record,
) => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const created = signedCommitQueueOutputs(h, journal);
  expect(created).toHaveLength(2);
  const base = journal[C.BASE_TAIL_HEADER_HASH].toString("hex");
  const baseUnit = [
    policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + base,
    policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  ].find((unit) => created.some((output) => output.assets[unit] === 1n));
  if (baseUnit === undefined)
    throw new Error("The signed commit does not continue its base");
  return (outputs: readonly LedgerSnapshotOutput[]) => {
    const tails = outputs.filter((output) => output.assets[baseUnit] === 1n);
    expect(tails).toHaveLength(1);
    return [...outputs.filter((output) => output !== tails[0]), ...created];
  };
};

/**
 * A history transport whose served state-queue outputs a test may rewrite
 * for points not yet sealed: drives the authenticated view directly for a
 * chain the emulator cannot produce (a block this node has no journal for in
 * its base's slot). Every other output and every transaction is the
 * recorder's.
 */
export const makeRewritableQueueTransport = () => {
  let rewrite:
    | ((outputs: readonly LedgerSnapshotOutput[]) => LedgerSnapshotOutput[])
    | undefined;
  return {
    setRewrite: (next: typeof rewrite) => {
      rewrite = next;
    },
    transportFactory: (
      recorded: Parameters<typeof makeStreamingHistoryTransport>[0],
    ) => {
      let seen = recorded.batches.length;
      const transport = makeStreamingHistoryTransport(recorded);
      const append = transport.appendAccepted;
      transport.appendAccepted = () => {
        for (const batch of recorded.batches.slice(seen))
          if (rewrite !== undefined)
            (batch as { outputs: readonly LedgerSnapshotOutput[] }).outputs =
              rewrite(batch.outputs);
        seen = recorded.batches.length;
        return append();
      };
      return transport;
    },
  };
};
