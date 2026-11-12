/**
 * A commit's pre-broadcast gate on the intent emulator, as the commit worker
 * runs it: a modeled pending block prepared under the producer's follower
 * permit and the worker's mutation lease, its transaction submitted through
 * `submitWithDurableIntent`, and what the gate left behind.
 */
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";

import {
  FollowerWrite,
  type FollowerWritePermit,
} from "../../src/services/follower-write-gate.js";
import { journaledIntent } from "../../src/services/intent-journal.js";
import { handleSignSubmitNoConfirmation } from "../../src/transactions/utils.js";
import { submitWithDurableIntent } from "../../src/workers/commit-block-header/submission.submit-with-durable-intent.js";
import { openFollowerWriteGateAt } from "./follower-write-gate.js";
import type { IntentEmulator } from "./intent-journal-emulator.js";
import { simCommitAnchor } from "./landed-blocks-sim.own.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./mutation-lease.js";

const digest = (n: number) => Buffer.alloc(32, n).toString("hex");

/** A producer's permit at the follower's current view: the write gate
 * opened at it, as the follower-change driver publishes the view it applied. */
export const followerPermit = async (
  env: IntentEmulator,
): Promise<FollowerWritePermit> => {
  const view = await env.store.currentView();
  if (view === null) throw new Error("the follower holds no view");
  return env.runtime.runPromise(openFollowerWriteGateAt(view));
};

const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

/** The mutation lease the modeled commit worker prepares and signs under. */
export const MODELED_LEASE_TOKEN = "modeled-pending-owner";

/** A modeled pending block (no members) whose prepared tx is `preparedTxHash`,
 * prepared under the producer as the commit worker prepares it. */
export const preparePendingBlock = async (
  env: IntentEmulator,
  permit: FollowerWritePermit,
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
    prepareUnderTestLease(
      {
        headerHash,
        headerCbor: Buffer.from(Data.to(header, SDK.Header), "hex"),
        preparedTxHash,
        metadata: {
          deploymentMarker: makeDeploymentMarker(digest(0x11)),
          consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
          stateQueueLeaseToken: MODELED_LEASE_TOKEN,
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
        blockTime: new Date(time.getTime() + 60_000),
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
        ledgerDelta: { spent: [], produced: [] },
      },
      { beforeJournalInsert: Effect.orDie(simCommitAnchor) },
    ).pipe(Effect.provideService(FollowerWrite, permit)),
  );
  return headerHash;
};

/** The commit's submission as the commit worker runs it: the seam under
 * `submitWithDurableIntent`, the producer's permit and the journal. */
export const submitCommit = async (
  env: IntentEmulator,
  permit: FollowerWritePermit,
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
    ).pipe(
      Effect.provideService(FollowerWrite, permit),
      withTestMutationLease(MODELED_LEASE_TOKEN),
    ),
  );
  return {
    exit,
    direct,
    text: Exit.isFailure(exit) ? Cause.pretty(exit.cause) : "success",
  };
};

export const pendingIntent = async (
  env: IntentEmulator,
  headerHash: Buffer,
) => {
  const [row] = await env.runtime.runPromise(
    env.sql<{
      intended_tx_hash: Buffer | null;
      signed_tx_cbor: Buffer | null;
    }>`SELECT intended_tx_hash, signed_tx_cbor FROM pending_block_finalizations
      WHERE header_hash = ${headerHash}`,
  );
  return row;
};

export const journaled = async (env: IntentEmulator) =>
  env.runtime.runPromise(
    env.sql<{
      tx_hash: Buffer;
      family: string;
      content_ref: Buffer | null;
    }>`SELECT tx_hash, family, content_ref FROM l1_intents`,
  );
