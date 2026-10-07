import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "@al-ft/midgard-fault-proofs";
import type {
  DerivationContext,
  DerivationHook,
  DialectName,
  RedeemerSummary,
  SqlTx,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { correctionLockWitness } from "../indexers/authenticated-state-queue-observation.correction-lock-witness.js";
import type { QueueNode } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  mintPolicyIds,
  outputHasPolicy,
  outputReferences,
  rawOutputHasPolicy,
} from "../indexers/authenticated-state-queue-observation.parse-persisted-observation.js";
import {
  decodedQueueOutputs,
  lockOutput,
} from "../indexers/authenticated-state-queue-observation.queue-output.js";
import {
  decodeLockOutputs,
  reconstructQueue,
} from "../indexers/authenticated-state-queue-observation.reconstruct-queue.js";
import type { WatcherProjectionDeployment } from "./projection.js";
import { parseOutRefLabel, resolveRawUtxoIn } from "./reads.js";
import { WATCHER_QUEUE_CHECKPOINTS_TABLE } from "./tables.js";
import { readWatcherQueueView } from "./view.js";

/**
 * The watcher's state-queue transition checkpoints over the follower's facts
 * (ticket W1). One append-only D-t row per state-queue transaction of a
 * canonical block holds the transition the authenticated observation
 * records for it: mint policies, redeemers, spent and referenced outrefs,
 * the CorrectionLock witness and the queue before and after. A transaction
 * the old derivation refuses is a row with its `failure`.
 *
 * The chain point id and finality depth are read-time inputs: a read at a
 * depth derives the SDK checkpoint (`deriveStateQueueAuthenticatedReplayCheckpoint`)
 * from the row, as the old observation does at its release or inclusion depth.
 *
 * Inputs resolve from the follower's tracked outputs (the queue, the lock,
 * the fraud-proof outputs). An untracked input resolves to a stand-in output
 * that no queue, lock or fraud-proof decoder accepts; the old derivation
 * reads those inputs only to find queue or lock outputs among them.
 */

export const CHECKPOINT_TEMPORAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: WATCHER_QUEUE_CHECKPOINTS_TABLE,
    shape: "append_only",
    slotColumn: "block_slot",
    retention: { kind: "created_k_deep" },
  },
];

export const checkpointMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows once block_slot is k deep
CREATE TABLE ${WATCHER_QUEUE_CHECKPOINTS_TABLE} (
  block_slot ${int8} NOT NULL,
  tx_index integer NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  transition text,
  failure text,
  PRIMARY KEY (block_slot, tx_index),
  CHECK ((transition IS NULL) <> (failure IS NULL))
);
`;
};

/** The redeemer spelling of the old L1 adapter (`witnessRedeemer`). */
const PURPOSES: Readonly<Record<RedeemerSummary["purpose"], string>> = {
  spend: "spend",
  mint: "mint",
  cert: "certificate",
  reward: "withdrawal",
  voting: "vote",
  proposing: "propose",
};
const PURPOSE_ORDER = [
  "spend",
  "mint",
  "certificate",
  "withdrawal",
  "vote",
  "propose",
];

/** The tx's redeemers as the authenticated checkpoint records them: canonical data, adapter order. */
export const transitionRedeemers = (
  redeemers: readonly RedeemerSummary[],
): readonly SDK.StateQueueTransitionRedeemer[] =>
  redeemers
    .map((redeemer) => {
      const data = CML.PlutusData.from_cbor_bytes(redeemer.data);
      try {
        return {
          purpose: PURPOSES[redeemer.purpose],
          index: redeemer.index.toString(),
          cborHex: data.to_canonical_cbor_hex(),
        };
      } finally {
        data.free();
      }
    })
    .sort(
      (left, right) =>
        PURPOSE_ORDER.indexOf(left.purpose) -
          PURPOSE_ORDER.indexOf(right.purpose) ||
        Number(left.index) - Number(right.index),
    );

/** Plain ADA at an enterprise key address: no decoder the derivation runs accepts it. */
const STAND_IN_OUTPUT_CBOR = `a200581d61${"00".repeat(28)}011a000f4240`;

const standIn = (outRef: string): FraudProofRawL1Utxo => ({
  outRef,
  outputCbor: STAND_IN_OUTPUT_CBOR,
  datumCbor: null,
  referenceScriptCbor: null,
});

/** One stored transition: everything the SDK checkpoint needs but the read-time point and depth. */
export type WatcherQueueTransition = Readonly<{
  transactionHash: string;
  transactionIndex: string;
  mintPolicyIds: readonly string[];
  redeemers: readonly SDK.StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  correctionLockWitness: SDK.StateQueueCorrectionLockWitness;
  previousQueue: readonly QueueNode[];
  nextQueue: readonly QueueNode[];
}>;

type Addresses = Readonly<{
  stateQueue: string;
  correctionLock: string;
  fraudProof: string;
}>;

const describe = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const resolveAll = async (
  tx: SqlTx,
  labels: readonly string[],
): Promise<FraudProofRawL1Utxo[]> =>
  Promise.all(
    labels.map(
      async (label) =>
        (await resolveRawUtxoIn(tx, parseOutRefLabel(label))) ?? standIn(label),
    ),
  );

/**
 * One transaction through the old derivation's steps. Null: it does not
 * touch the queue (and moves no lock). Throws what the old derivation
 * throws.
 */
const transition = async (
  tx: SqlTx,
  entry: DerivationContext["qualified"][number],
  queue: readonly QueueNode[],
  deployment: WatcherProjectionDeployment,
  addresses: Addresses,
): Promise<WatcherQueueTransition | null> => {
  const txHash = entry.tx.hash.toString("hex");
  const body = CML.TransactionBody.from_cbor_bytes(entry.tx.bodyCbor);
  try {
    const spentInputOutRefs = outputReferences(body.inputs());
    const referenceInputOutRefs = outputReferences(body.reference_inputs());
    const resolvedInputs = await resolveAll(tx, spentInputOutRefs);
    const resolvedReferenceInputs = await resolveAll(tx, referenceInputOutRefs);
    const policies = mintPolicyIds(body);
    const outputs = body.outputs();
    let outputTouchesQueue = false;
    for (let index = 0; index < outputs.len(); index += 1)
      outputTouchesQueue ||= outputHasPolicy(
        outputs.get(index),
        deployment.stateQueueMint,
      );
    const touchesQueue =
      policies.includes(deployment.stateQueueMint) ||
      outputTouchesQueue ||
      resolvedInputs.some(({ outputCbor }) =>
        rawOutputHasPolicy(outputCbor, deployment.stateQueueMint),
      );
    if (!touchesQueue) {
      const consumesLock = resolvedInputs.some(
        ({ outRef, outputCbor }) =>
          lockOutput({
            output: CML.TransactionOutput.from_cbor_hex(outputCbor),
            outRef,
            correctionLockAddress: addresses.correctionLock,
            hubOraclePolicyId: deployment.hubOracleMint,
          }) !== null,
      );
      const producesLock =
        decodeLockOutputs({
          body,
          transactionHash: txHash,
          correctionLockAddress: addresses.correctionLock,
          hubOraclePolicyId: deployment.hubOracleMint,
        }).length > 0;
      if (consumesLock || producesLock)
        throw new Error(
          "CorrectionLock changed without an authenticated state-queue transition",
        );
      return null;
    }
    const queueOutputs = decodedQueueOutputs({
      body,
      transactionHash: txHash,
      stateQueueAddress: addresses.stateQueue,
      stateQueuePolicyId: deployment.stateQueueMint,
    });
    const nextQueue =
      queue.length === 0 && queueOutputs.length === 1
        ? [queueOutputs[0]!.node]
        : reconstructQueue({
            previousQueue: queue,
            transactionHash: txHash,
            spentInputOutRefs,
            resolvedInputs,
            outputs: queueOutputs,
            stateQueueAddress: addresses.stateQueue,
            stateQueuePolicyId: deployment.stateQueueMint,
          });
    const redeemers = transitionRedeemers(entry.tx.redeemers);
    const witness = correctionLockWitness({
      raw: {
        txHash,
        resolvedInputs,
        resolvedReferenceInputs,
      } as unknown as FraudProofRawL1Transaction,
      body,
      mintPolicies: policies,
      redeemers,
      stateQueuePolicyId: deployment.stateQueueMint,
      correctionLockAddress: addresses.correctionLock,
      hubOraclePolicyId: deployment.hubOracleMint,
      fraudProofPolicyId: deployment.fraudProofMint,
      fraudProofAddress: addresses.fraudProof,
      availabilityChallengePolicyId: deployment.availabilityChallengeMint,
    });
    return {
      transactionHash: txHash,
      transactionIndex: entry.tx.index.toString(),
      mintPolicyIds: policies,
      redeemers,
      spentInputOutRefs,
      referenceInputOutRefs,
      correctionLockWitness: witness,
      previousQueue: queue.map(({ headerHash, outRef }) => ({
        headerHash,
        outRef,
      })),
      nextQueue: nextQueue.map(({ headerHash, outRef }) => ({
        headerHash,
        outRef,
      })),
    };
  } finally {
    body.free();
  }
};

const insert = async (
  context: DerivationContext,
  entry: DerivationContext["qualified"][number],
  row: Readonly<{ transition: string | null; failure: string | null }>,
): Promise<void> => {
  await context.tx.query(
    `INSERT INTO ${WATCHER_QUEUE_CHECKPOINTS_TABLE} (block_slot, tx_index, tx_hash, block_hash, block_height, transition, failure) VALUES (?, ?, ?, ?, ?, ?, ?)`,
    [
      context.block.point.slot,
      entry.tx.index,
      entry.tx.hash,
      context.block.point.hash,
      context.block.height,
      row.transition,
      row.failure,
    ],
  );
};

/**
 * The checkpoint derivation. It reads the queue at the previous block from
 * the queue projection's rows (a block's own rows start at its slot, so the
 * order of the two derivations does not matter) and evolves it through the
 * block's transactions in order.
 */
export const checkpointDerivation = (
  deployment: WatcherProjectionDeployment,
): DerivationHook => {
  const address = (hash: string): string =>
    credentialToAddress(deployment.network, scriptHashToCredential(hash));
  const addresses: Addresses = {
    stateQueue: address(deployment.stateQueueSpend),
    correctionLock: address(deployment.correctionLockSpend),
    fraudProof: address(deployment.fraudProofSpend),
  };
  return {
    name: "watcher_state_queue_checkpoints",
    writes: [WATCHER_QUEUE_CHECKPOINTS_TABLE],
    apply: async (context) => {
      let view: Awaited<ReturnType<typeof readWatcherQueueView>> | null = null;
      let queue: readonly QueueNode[] = [];
      for (const entry of context.qualified) {
        // A phase-2 failure creates only its collateral return: no queue
        // transition (the old raw reads admit valid transactions only).
        if (!entry.tx.isValid) continue;
        if (view === null) {
          view = await readWatcherQueueView(
            context.tx,
            context.previous.point.slot,
          );
          if (view.healthy) queue = view.queue;
        }
        if (!view.healthy) {
          await insert(context, entry, {
            transition: null,
            failure: `the queue before this block is unhealthy: ${view.reason}`,
          });
          continue;
        }
        try {
          const derived = await transition(
            context.tx,
            entry,
            queue,
            deployment,
            addresses,
          );
          if (derived === null) continue;
          await insert(context, entry, {
            transition: JSON.stringify(derived),
            failure: null,
          });
          queue = derived.nextQueue;
        } catch (error) {
          await insert(context, entry, {
            transition: null,
            failure: describe(error),
          });
        }
      }
    },
  };
};

/** The checkpoints of one block, read at a finality depth. */
export type WatcherBlockCheckpoints =
  | Readonly<{
      kind: "ok";
      checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[];
      correctionLockWitnesses: readonly SDK.StateQueueCorrectionLockWitness[];
    }>
  | Readonly<{ kind: "failed"; transactionHash: string; failure: string }>;

/**
 * The SDK checkpoints of the block at `block`, as the authenticated
 * observation of that block records them at `finalityDepth`.
 */
export const readWatcherCheckpoints = async (
  tx: SqlTx,
  block: Readonly<{ slot: number; hash: Buffer; height: number }>,
  options: Readonly<{
    deploymentIdentityDigest: string;
    stateQueuePolicyId: string;
    finalityDepth: number;
  }>,
): Promise<WatcherBlockCheckpoints> => {
  const rows = await tx.query(
    `SELECT tx_hash, transition, failure FROM ${WATCHER_QUEUE_CHECKPOINTS_TABLE} WHERE block_slot = ? AND block_hash = ? ORDER BY tx_index`,
    [block.slot, block.hash],
  );
  const point = {
    blockHash: block.hash.toString("hex"),
    slot: block.slot.toString(),
    blockNo: block.height.toString(),
  };
  const chainPointId = computeFraudProofRawL1PointId(point);
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  for (const row of rows) {
    const txHash = Buffer.from(row.tx_hash as Uint8Array).toString("hex");
    if (row.failure !== null)
      return {
        kind: "failed",
        transactionHash: txHash,
        failure: row.failure as string,
      };
    const stored = JSON.parse(
      row.transition as string,
    ) as WatcherQueueTransition;
    const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
      deploymentIdentityDigest: options.deploymentIdentityDigest,
      stateQueuePolicyId: options.stateQueuePolicyId,
      transactionHash: stored.transactionHash,
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
      transactionIndex: stored.transactionIndex,
      chainPointId,
      finalityDepth: options.finalityDepth.toString(),
      mintPolicyIds: stored.mintPolicyIds,
      redeemers: stored.redeemers,
      spentInputOutRefs: stored.spentInputOutRefs,
      referenceInputOutRefs: stored.referenceInputOutRefs,
      correctionLockWitness: stored.correctionLockWitness,
      previousQueue: stored.previousQueue,
      nextQueue: stored.nextQueue,
    });
    if (checkpoint === null)
      return {
        kind: "failed",
        transactionHash: txHash,
        failure:
          "state-queue transaction failed authenticated checkpoint derivation",
      };
    checkpoints.push(checkpoint);
  }
  return {
    kind: "ok",
    checkpoints,
    correctionLockWitnesses: checkpoints.map(
      ({ correctionLockWitness: witness }) => witness,
    ),
  };
};
