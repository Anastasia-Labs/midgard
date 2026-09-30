import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import {
  read,
  readJournal,
  readSqlLedgerRoot,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import {
  type Handle,
  nativeRoot,
  readDepositHeader,
  signedTtl,
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

export const C = Pending.Columns;

export const NOT_A_REPLACEMENT_DIGEST = "cd".repeat(32);

/** The block local finalization replays next, named by its node's asset. */
export const availableBlockAssetName = (h: Handle) => {
  const available = Effect.runSync(
    Ref.get(h.globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
  );
  return available === "" ? "" : available.assetName;
};

export const nodeAssetName = (header: string) =>
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;

export const readGlobals = (h: Handle) => ({
  localFinalizationPending: Effect.runSync(
    Ref.get(h.globals.LOCAL_FINALIZATION_PENDING),
  ),
  unconfirmed: Effect.runSync(
    Ref.get(h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
  ),
});

/** Commit the next block and lose its signed commit; returns its journal and
 * TTL. */
export const loseNextCommit = async (
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
export const signedReferenceInputs = (journal: Pending.Record) => {
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
export const snapshotRevivalRows = () =>
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

/** E's own state-queue node as its signed commit creates it. */
export const signedCommitNode = async (h: Handle, journal: Pending.Record) => {
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

/** The replaced block E revived with no journal active: its journal observed,
 * its members taken back, its SQL marker at its candidate root, and its own
 * node made available to local finalization. In the running process native
 * state is at its base and no submission is tracked; after a restart the
 * startup hydration tracks its signed commit and the native owner's startup
 * replays the observed journal to its candidate root. */
export const expectRevivedWithoutActiveJournal = async (
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
