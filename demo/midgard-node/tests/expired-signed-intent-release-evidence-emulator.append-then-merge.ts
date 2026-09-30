import "./expired-signed-intent-release-evidence-emulator.signed-intent-release-after-its-base-left-the-queue.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import { SIGNED_HEADER_RECOVERY_DOMAIN } from "../src/database/eventHistoryRecoveryPlans.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  appendCheckpoint,
  mergeCheckpoint,
  type ObserverContext,
} from "./expired-signed-intent-release-evidence-emulator.merge-checkpoint.js";
import {
  availableBlockAssetName,
  C,
} from "./expired-signed-intent-release-evidence-emulator.signed-intent-release-evidence.js";
import {
  read,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import {
  expectReplaced,
  type Handle,
  moveToExactSlot,
  nativeRoot,
  readEmulatorQueue,
  resetSharedRows,
  signedTtl,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

/** Every durable row and runtime flag a revival would change. */
export const snapshotRevival = async (h: Handle) => ({
  rows: await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return {
        journals: yield* sql`SELECT header_hash, status,
          correction_transition_digest FROM pending_block_finalizations
          ORDER BY header_hash`,
        deposits: yield* sql`SELECT event_id, projected_header_hash
          FROM deposits_utxos ORDER BY event_id`,
        ledger: yield* sql`SELECT root_hex FROM mpf_engine_state
          WHERE store_name = 'ledger'`,
      };
    }),
  ),
  native: await nativeRoot(h),
  localFinalizationPending: Effect.runSync(
    Ref.get(h.globals.LOCAL_FINALIZATION_PENDING),
  ),
  available: availableBlockAssetName(h),
});

export const RETAINED_SIGNED_HEADER_RECOVERY_ID = "fc".repeat(32);

/** A prepared signed-header recovery plan at the current cursor, as a crash
 * between its native restore and its SQL repair leaves it. Its header is no
 * journal of this node, so that recovery finds no candidate to resume it
 * with and the plan stays retained. */
export const retainSignedHeaderPlan = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const cursors = yield* sql<{
        binding_digest: Buffer;
        manifest_id: Buffer;
        revision: string;
        head_hash: Buffer;
        snapshot_digest: Buffer;
      }>`SELECT binding_digest, manifest_id, revision, head_hash,
          snapshot_digest FROM event_history_cursor`;
      expect(cursors).toHaveLength(1);
      const cursor = cursors[0]!;
      const headerHash = "fd".repeat(28);
      yield* sql`INSERT INTO event_history_recovery_plans
        (recovery_id, binding_digest, manifest_id, header_hash, intent,
         evidence_digest, checkpoint_revision, head_hash, snapshot_digest,
         owner_generation, state)
        VALUES (${Buffer.from(RETAINED_SIGNED_HEADER_RECOVERY_ID, "hex")},
          ${cursor.binding_digest}, ${cursor.manifest_id},
          ${Buffer.from(headerHash, "hex")},
          ${JSON.stringify({
            domain: SIGNED_HEADER_RECOVERY_DOMAIN,
            headerHash,
            expectedRoot: "fe".repeat(32),
          })},
          ${Buffer.from("fb".repeat(32), "hex")}, ${cursor.revision},
          ${cursor.head_hash}, ${cursor.snapshot_digest}, 0, 'prepared')`;
    }),
  );

export const discardSignedHeaderPlan = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM event_history_recovery_plans
        WHERE recovery_id = ${Buffer.from(RETAINED_SIGNED_HEADER_RECOVERY_ID, "hex")}`;
    }),
  );

/** Two blocks of this node built on one root output: E, replaced at its TTL
 * while the queue still showed that root as the tail, and its replacement N,
 * handed to L1 and lost (the scheduler alignment is skipped as in the revival
 * tests; the served view is synthetic anyway). */
export const replacedRootBuiltPair = async (h: Handle) => {
  await resetSharedRows();
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  const inclusion = await submitDeposit(h, 12_000_000n);
  const lost = await submitUnlandedBlock(h, inclusion);
  const header = lost.submittedHeaderHash;
  const journal = await readJournal(header);
  expect(await readEmulatorQueue(h)).toHaveLength(1);
  moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
  await synchronizeWithin(h);
  await expectReplaced(journal, { handle: h });
  const next = await submitUnlandedBlock(h, h.fixture.emulator.now() - 1000, {
    alignScheduler: false,
  });
  const replacement = await readJournal(next.submittedHeaderHash);
  expect(replacement[C.BASE_TAIL_OUT_REF]).toBe(journal[C.BASE_TAIL_OUT_REF]);
  expect(replacement[C.BASE_TAIL_HEADER_HASH]).toEqual(
    journal[C.BASE_TAIL_HEADER_HASH],
  );
  return { header, journal, replacement };
};

/** A synthetic merge of `base` (the confirmed state's block) that leaves the
 * queue empty under the root output `rootOutRef`, and the queue before it. */
export const mergeLeavingRoot = (
  context: ObserverContext,
  base: string,
  rootOutRef: string,
) => {
  const [transactionHash, index] = rootOutRef.split("#") as [string, string];
  const previous: readonly SDK.StateQueueTransitionNode[] = [
    { headerHash: null, outRef: `${"d0".repeat(32)}#0` },
    { headerHash: base, outRef: `${"d1".repeat(32)}#1` },
  ];
  const merge = mergeCheckpoint(
    context,
    previous,
    transactionHash,
    10,
    Number(index),
  );
  expect(merge.nextQueue).toEqual([{ headerHash: null, outRef: rootOutRef }]);
  return { previous, merge };
};

/** Appends of `headers` in order onto `from`, then merges of each, from block
 * `blockNo` on. Each append's transaction is `transactions[i]` when given. */
export const appendThenMerge = (
  context: ObserverContext,
  from: readonly SDK.StateQueueTransitionNode[],
  headers: readonly string[],
  blockNo: number,
  transactions: readonly string[] = [],
) => {
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  let queue = from;
  const push = (checkpoint: SDK.StateQueueAuthenticatedReplayCheckpoint) => {
    checkpoints.push(checkpoint);
    queue = checkpoint.nextQueue;
  };
  headers.forEach((header, index) =>
    push(
      appendCheckpoint(
        context,
        queue,
        transactions[index] ?? (0xb1 + index).toString(16).repeat(32),
        header,
        blockNo + index,
      ),
    ),
  );
  headers.forEach((_, index) =>
    push(
      mergeCheckpoint(
        context,
        queue,
        (0xc1 + index).toString(16).repeat(32),
        blockNo + headers.length + index,
      ),
    ),
  );
  return checkpoints;
};
