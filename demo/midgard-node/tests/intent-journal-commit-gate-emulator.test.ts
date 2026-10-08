/**
 * The commit family's pre-broadcast gate under the follower-backed journal
 * (I1FIX-R2, review-i1fix B1/B2). The gate
 * (`PendingBlockFinalizationsDB.recordSignedIntent`, as
 * `submitWithDurableIntent` provides it) owns its outermost history-write
 * transaction, and the journal records the intent inside it: the gate is
 * never run inside a transaction the journal opened.
 *
 * The journal is the production one (`intentJournalOver`, which
 * `IntentJournalLive` builds over the node database) on a follower store
 * that followed the emulator; the gate is never stubbed. Under a Ready
 * history producer's permit (an acquired authority, published Ready at a
 * modeled cursor) the commit is journaled, its pending row's signed intent
 * is written and the tx is sent; a gate or journal refusal leaves neither.
 */
import { randomUUID } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Layer, ManagedRuntime, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../src/services/event-history-producer.js";
import {
  INTENT_INPUT_UNTRACKED,
  IntentJournal,
  IntentJournalWithoutFollower,
  journaledIntent,
} from "../src/services/intent-journal.js";
import {
  BeforeSignedTransactionSubmission,
  handleSignSubmitNoConfirmation,
} from "../src/transactions/utils.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import { submitWithDurableIntent } from "../src/workers/commit-block-header/submission.submit-with-durable-intent.js";
import {
  type IntentEmulator,
  openIntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

const INHERITED =
  "Signed intent must commit before broadcast outside an inherited transaction";

const journalRows = async (env: IntentEmulator): Promise<number> => {
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(env.connectionString) }),
  );
  try {
    return await runtime.runPromise(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        sql<{ n: string }>`SELECT count(*)::text AS n FROM l1_intents`.pipe(
          Effect.map(([row]) => Number(row?.n ?? 0)),
        ),
      ),
    );
  } finally {
    await runtime.dispose();
  }
};

/** The reviewer's repro (review-i1fix B1): a commit submitted through the
 * production gate, with no producer permit. */
const submitThroughCommitGate = async (
  env: IntentEmulator,
  journalLayer: Layer.Layer<IntentJournal>,
) => {
  expect(await env.stage.run()).toEqual([]);
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(env.connectionString) }),
  );
  const sql = await runtime.runPromise(SqlClient.SqlClient);
  const lucid = await env.wallet();
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .complete();
  const provider = lucid.config().provider!;
  const direct: string[] = [];
  const submitTx = provider.submitTx.bind(provider);
  provider.submitTx = (tx) => {
    direct.push(tx);
    return submitTx(tx);
  };
  const header = Buffer.alloc(28, 7);
  const exit = await Effect.runPromiseExit(
    handleSignSubmitNoConfirmation(
      lucid,
      unsigned,
      journaledIntent("commit", "commit:tail=x", await env.plan(), header),
    ).pipe(
      Effect.provideService(BeforeSignedTransactionSubmission, {
        persist: ({ txHash, signedTxCbor, journal }) =>
          PendingBlockFinalizationsDB.recordSignedIntent(
            header,
            Buffer.from(txHash, "hex"),
            Buffer.from(signedTxCbor, "hex"),
            journal,
          ).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
      }),
      Effect.provide(journalLayer),
    ),
  );
  await runtime.dispose();
  const text = Exit.isFailure(exit) ? Cause.pretty(exit.cause) : "success";
  return { exit, direct, text };
};

describe("the commit gate under the follower-backed journal", () => {
  it("reaches the gate's own checks (no inherited-transaction refusal), journals nothing and sends nothing when the gate refuses", async () => {
    const env = await openIntentEmulator(databases);
    opened.push(env);
    const { exit, direct, text } = await submitThroughCommitGate(
      env,
      Layer.succeed(IntentJournal, env.journal),
    );
    expect(exit._tag).toBe("Failure");
    expect(text).not.toContain(INHERITED);
    // No permit: the gate's own history-write check refuses.
    expect(text).toContain("Missing producer permit");
    expect(direct).toEqual([]);
    expect(env.sent).toEqual([]);
    expect(await journalRows(env)).toBe(0);
    expect(env.journal.holds()).toEqual([]);
  });

  it("control: the no-follower journal runs the same gate to the same check", async () => {
    const env = await openIntentEmulator(databases);
    opened.push(env);
    const { exit, direct, text } = await submitThroughCommitGate(
      env,
      IntentJournalWithoutFollower,
    );
    expect(exit._tag).toBe("Failure");
    expect(text).not.toContain(INHERITED);
    expect(text).toContain("Missing producer permit");
    expect(direct).toEqual([]);
  });
});

const digest = (n: number) => Buffer.alloc(32, n).toString("hex");

/** A Ready history producer's permit: the authority acquired and published
 * Ready at a modeled cursor (no block applied: head = anchor). */
const readyProducer = async (
  env: IntentEmulator,
): Promise<HistoryProducerPermit> => {
  const deploymentIdentity = digest(0x11);
  const binding = digest(0x12);
  const point = { id: digest(0x13), slot: 0 };
  const snapshotDigest = digest(0x14);
  const token = await env.runtime.runPromise(
    Authority.acquire({
      deploymentIdentity,
      ownerToken: randomUUID(),
      leaseDurationMs: 600_000,
    }),
  );
  await env.runtime.runPromise(
    Authority.publishReady(token, { point, snapshotDigest }),
  );
  await env.runtime.runPromise(
    env.sql`INSERT INTO event_history_cursor (binding_digest, manifest_id,
      origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot,
      anchor_height, anchor_snapshot_digest, head_hash, head_slot, head_height,
      head_application_revision, snapshot_digest, revision, addresses)
      VALUES (${Buffer.from(binding, "hex")}, ${Buffer.from(deploymentIdentity, "hex")},
        'modeled cursor; not ledger admission', ${Buffer.alloc(32, 0x15)},
        ${Buffer.from(point.id, "hex")}, 0, 0, ${Buffer.from(snapshotDigest, "hex")},
        ${Buffer.from(point.id, "hex")}, 0, 0, NULL,
        ${Buffer.from(snapshotDigest, "hex")}, 0, '[]'::jsonb)`,
  );
  return {
    token,
    coverage: {
      bindingDigest: binding,
      checkpointRevision: "0",
      point,
      snapshotDigest,
      includedThroughMs: 0,
    },
  };
};

const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

/** A modeled pending block (no members) whose prepared tx is `preparedTxHash`,
 * prepared under the producer as the commit worker prepares it. */
const preparePendingBlock = async (
  env: IntentEmulator,
  permit: HistoryProducerPermit,
  preparedTxHash: Buffer,
): Promise<Buffer> => {
  const roots = {
    utxosRoot: EMPTY_ROOT,
    forcedTransactionsRoot: EMPTY_ROOT,
    transactionsRoot: EMPTY_ROOT,
    depositsRoot: EMPTY_ROOT,
    withdrawalsRoot: EMPTY_ROOT,
  };
  const expectedRoots = {
    ...roots,
    transitionTraceRoot: EMPTY_ROOT,
    eventToStepRoot: EMPTY_ROOT,
    validationTracesRoot: EMPTY_ROOT,
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const time = new Date(1_000_000);
  const header: SDK.Header = {
    ...expectedRoots,
    ...counts,
    prevUtxosRoot: EMPTY_ROOT,
    startTime: BigInt(time.getTime()),
    endTime: BigInt(time.getTime() + 60_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "41".repeat(28),
    operatorVkey: "42".repeat(28),
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  await env.runtime.runPromise(
    Authority.withReady(
      permit.token,
      PendingBlockFinalizationsDB.preparePendingSubmission({
        headerHash,
        headerCbor: Buffer.from(Data.to(header, SDK.Header), "hex"),
        preparedTxHash,
        metadata: {
          deploymentMarker: makeDeploymentMarker(
            permit.token.deploymentIdentity,
          ),
          consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
          stateQueueLeaseToken: "modeled-pending-owner",
          baseSnapshotId: "modeled-pending-base",
          baseTailOutRef: `${digest(0x16)}#0`,
          baseTailHeaderHash: Buffer.alloc(28, 0x41),
          baseTailDatumCbor: "d87980",
          baseRoots: roots,
          blockStartTime: time,
          expectedRoots,
          expectedCounts: counts,
        },
        blockEndTime: new Date(time.getTime() + 60_000),
        depositEventIds: [],
        depositEntries: [],
        forcedTransactionEventIds: [],
        forcedTransactionEntries: [],
        withdrawalEventIds: [],
        withdrawalEntries: [],
        mempoolTxIds: [],
        mempoolTxs: [],
        mempoolTxSourceTable: "none",
        transitionTraceMembers: [],
        eventToStepMembers: [],
        validationTraceMembers: [],
        validationTraceWitnessMembers: [],
        ledgerDelta: { spent: [], produced: [] },
      }).pipe(Effect.provideService(HistoryProducer, permit)),
    ),
  );
  return headerHash;
};

/** The commit's submission as the commit worker runs it: the seam under
 * `submitWithDurableIntent`, the producer's permit and the journal. */
const submitCommit = async (
  env: IntentEmulator,
  permit: HistoryProducerPermit,
  lucid: LucidEvolution,
  unsigned: TxSignBuilder,
  headerHash: Buffer,
) => {
  const provider = lucid.config().provider!;
  const direct: string[] = [];
  const submitTx = provider.submitTx.bind(provider);
  provider.submitTx = (tx) => {
    direct.push(tx);
    return submitTx(tx);
  };
  const exit = await env.runtime.runPromiseExit(
    submitWithDurableIntent(
      headerHash,
      handleSignSubmitNoConfirmation(
        lucid,
        unsigned,
        journaledIntent(
          "commit",
          `commit:${headerHash.toString("hex")}`,
          await env.plan(),
          headerHash,
        ),
      ).pipe(Effect.provide(env.journalLayer)),
    ).pipe(Effect.provideService(HistoryProducer, permit)),
  );
  return {
    exit,
    direct,
    text: Exit.isFailure(exit) ? Cause.pretty(exit.cause) : "success",
  };
};

const pendingIntent = async (env: IntentEmulator, headerHash: Buffer) => {
  const [row] = await env.runtime.runPromise(
    env.sql<{
      intended_tx_hash: Buffer | null;
      signed_tx_cbor: Buffer | null;
    }>`SELECT intended_tx_hash, signed_tx_cbor FROM pending_block_finalizations
      WHERE header_hash = ${headerHash}`,
  );
  return row;
};

const journaled = async (env: IntentEmulator) =>
  env.runtime.runPromise(
    env.sql<{
      tx_hash: Buffer;
      family: string;
      content_ref: Buffer | null;
    }>`SELECT tx_hash, family, content_ref FROM l1_intents`,
  );

describe("the commit gate under a Ready history producer", () => {
  it("journals the commit, writes its pending row's signed intent and sends it, in one gate transaction", async () => {
    const env = await openIntentEmulator(databases, { nodeSchema: true });
    opened.push(env);
    expect(await env.stage.run()).toEqual([]);
    const permit = await readyProducer(env);
    const lucid = await env.wallet();
    const unsigned = await lucid
      .newTx()
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete();
    const txHash = unsigned.toHash();
    const headerHash = await preparePendingBlock(
      env,
      permit,
      Buffer.from(txHash, "hex"),
    );
    const { exit, direct, text } = await submitCommit(
      env,
      permit,
      lucid,
      unsigned,
      headerHash,
    );
    expect(text).toBe("success");
    expect(exit._tag).toBe("Success");
    expect(direct).toHaveLength(1);
    const rows = await journaled(env);
    expect(
      rows.map((row) => ({
        txHash: row.tx_hash.toString("hex"),
        family: row.family,
        contentRef: row.content_ref?.toString("hex"),
      })),
    ).toEqual([
      { txHash, family: "commit", contentRef: headerHash.toString("hex") },
    ]);
    const pending = await pendingIntent(env, headerHash);
    expect(pending?.intended_tx_hash?.toString("hex")).toBe(txHash);
    expect(pending?.signed_tx_cbor?.toString("hex")).toBe(direct[0]);
    expect(env.journal.holds()).toEqual([]);
    // S6 follows the sent commit to its landing.
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(env.stage.lastReport()!.intents).toEqual([
      expect.objectContaining({
        action: "follow",
        status: expect.objectContaining({ kind: "landed" }),
      }),
    ]);
    expect(env.sent).toEqual([]);
  });

  it("a gate refusal after the journal insert leaves no journal row, no signed intent and no send", async () => {
    const env = await openIntentEmulator(databases, { nodeSchema: true });
    opened.push(env);
    expect(await env.stage.run()).toEqual([]);
    const permit = await readyProducer(env);
    const lucid = await env.wallet();
    const unsigned = await lucid
      .newTx()
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete();
    // The pending row was prepared for another tx: the gate's UPDATE, run
    // after the journal insert in the same transaction, refuses.
    const headerHash = await preparePendingBlock(
      env,
      permit,
      Buffer.alloc(32, 0x17),
    );
    const { exit, direct, text } = await submitCommit(
      env,
      permit,
      lucid,
      unsigned,
      headerHash,
    );
    expect(exit._tag).toBe("Failure");
    expect(text).toContain("Signed intent conflicts with the pending journal");
    expect(direct).toEqual([]);
    expect(await journaled(env)).toEqual([]);
    expect(await pendingIntent(env, headerHash)).toMatchObject({
      intended_tx_hash: null,
      signed_tx_cbor: null,
    });
    // The gate's own refusal is the commit worker's failure, not a hold.
    expect(env.journal.holds()).toEqual([]);
    await env.stage.run();
    expect(env.stage.lastReport()!.intents).toEqual([]);
    expect(env.sent).toEqual([]);
  });

  it("a journal refusal inside the gate rolls the gate's write back, is held by name, and sends nothing", async () => {
    const env = await openIntentEmulator(databases, { nodeSchema: true });
    opened.push(env);
    expect(await env.stage.run()).toEqual([]);
    const permit = await readyProducer(env);
    // The payee's wallet is not a tracked one: its inputs are not facts,
    // so its view offers nothing and the journal refuses them at the gate.
    const lucid = await env.wallet();
    selectNodeWallet(lucid, env.payee.seedPhrase);
    const unsigned = await lucid
      .newTx()
      .pay.ToAddress(env.own.address, { lovelace: 2_000_000n })
      .complete();
    const headerHash = await preparePendingBlock(
      env,
      permit,
      Buffer.from(unsigned.toHash(), "hex"),
    );
    const { exit, direct } = await submitCommit(
      env,
      permit,
      lucid,
      unsigned,
      headerHash,
    );
    expect(exit._tag).toBe("Failure");
    expect(direct).toEqual([]);
    expect(await journaled(env)).toEqual([]);
    expect(await pendingIntent(env, headerHash)).toMatchObject({
      intended_tx_hash: null,
      signed_tx_cbor: null,
    });
    expect(env.journal.holds().map(({ reason }) => reason)).toEqual([
      INTENT_INPUT_UNTRACKED,
    ]);
    expect(env.sent).toEqual([]);
  });
});
