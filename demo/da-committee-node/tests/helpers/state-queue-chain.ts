import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  Header,
  ObservedStateQueueNode,
  ObservedStateQueueSnapshot,
} from "../../src/domain.js";
import { StateQueueHistoryNotExtendingAnchorError } from "../../src/l1/source-integrity.js";

export type ChainHeader = Readonly<{
  header: Header;
  headerHash: string;
  daAttestation?: SDK.DaAvailabilityStateQueueStatus;
}>;

type Entry = Readonly<{
  headerHash: string | null;
  outRef: string;
  createdBlock: number;
  header?: ChainHeader;
}>;

type Transaction = Readonly<{
  blockNo: number;
  transactionIndex: number;
  transactionHash: string;
  blockHash: string;
  /** Null for a transaction that only updates a node's datum. */
  redeemer: SDK.StateQueueRedeemer | null;
  spentInputOutRefs: readonly string[];
  lockOutRef: string;
  previous: readonly Entry[];
  next: readonly Entry[];
  mergedHeaderHash?: string;
}>;

const sha = (value: string): string =>
  createHash("sha256").update(value).digest("hex");
const transitionQueue = (
  queue: readonly SDK.StateQueueTransitionNode[],
): SDK.StateQueueTransitionNode[] =>
  queue.map(({ headerHash, outRef }) => ({ headerHash, outRef }));
const sameQueue = (
  left: readonly SDK.StateQueueTransitionNode[],
  right: readonly SDK.StateQueueTransitionNode[],
): boolean =>
  JSON.stringify(transitionQueue(left)) ===
  JSON.stringify(transitionQueue(right));
const outRefParts = (outRef: string) => ({
  transactionId: outRef.slice(0, 64),
  outputIndex: BigInt(outRef.split("#")[1]!),
});

/**
 * An in-memory L1 state queue with an exact, SDK-derived transaction history.
 * Blocks are mined one at a time and can be rolled back. Its replay source
 * behaves like the committee's local Kupmios replay: it walks history from an
 * anchor queue, reports each transaction's current depth, and does not know
 * outputs that a rollback undid.
 */
export const createStateQueueChain = ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  headers,
  tip,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  /** Headers queued at genesis, oldest first, created in block 1. */
  readonly headers: readonly ChainHeader[];
  readonly tip: number;
}) => {
  const genesis: readonly Entry[] = [
    { headerHash: null, outRef: `${"00".repeat(32)}#0`, createdBlock: 1 },
    ...headers.map((header, index) => ({
      headerHash: header.headerHash,
      outRef: `${"01".repeat(32)}#${index.toString()}`,
      createdBlock: 1,
      header,
    })),
  ];
  const chain = {
    tip,
    history: [] as Transaction[],
    transactionCount: 0,
    forks: 0,
  };
  const queue = (): readonly Entry[] => chain.history.at(-1)?.next ?? genesis;
  // A block keeps its hash until a rollback undoes it.
  const blockHashes = new Map<number, string>();
  const blockHash = (blockNo: number): string => {
    let hash = blockHashes.get(blockNo);
    if (hash === undefined) {
      hash = sha(`block:${blockNo.toString()}:${chain.forks.toString()}`);
      blockHashes.set(blockNo, hash);
    }
    return hash;
  };
  // An output's depth counts the blocks mined on top of its own, as the
  // providers report it; a checkpoint's finality depth counts its own block
  // too, as the SDK replay requires.
  const depth = (blockNo: number): number => chain.tip - blockNo;
  const point = (blockNo: number) => ({
    slot: blockNo * 20,
    blockHash: blockHash(blockNo),
    blockHeight: blockNo,
    depth: depth(blockNo),
    providerSource: "state-queue-chain",
  });

  const record = (
    blockNo: number,
    transactionIndex: number,
    redeemer: SDK.StateQueueRedeemer | null,
    next: (transactionHash: string, previous: readonly Entry[]) => Entry[],
    mergedHeaderHash?: string,
  ): void => {
    chain.transactionCount += 1;
    const transactionHash = sha(
      `tx:${chain.transactionCount.toString()}:${chain.forks.toString()}`,
    );
    const previous = queue();
    const nextQueue = next(transactionHash, previous);
    chain.history.push({
      blockNo,
      transactionIndex,
      transactionHash,
      blockHash: blockHash(blockNo),
      redeemer,
      spentInputOutRefs: previous
        .filter(
          (entry) =>
            !nextQueue.some(
              (candidate) =>
                candidate.headerHash === entry.headerHash &&
                candidate.outRef === entry.outRef,
            ),
        )
        .map(({ outRef }) => outRef),
      lockOutRef: `${"fe".repeat(32)}#0`,
      previous,
      next: nextQueue,
      ...(mergedHeaderHash === undefined ? {} : { mergedHeaderHash }),
    });
  };

  const append = (
    blockNo: number,
    transactionIndex: number,
    header: ChainHeader,
  ): void =>
    record(
      blockNo,
      transactionIndex,
      {
        CommitBlockHeader: {
          yield_to_ref_input_index: 0n,
          new_block_output_index: 1n,
          continued_latest_block_output_index: 0n,
          operator: "ab".repeat(28),
          scheduler_ref_input_index: 0n,
          active_operators_input_index: 0n,
          active_operators_redeemer_index: 0n,
          m_confirmed_state_ref_input_index: null,
          m_head_state_queue_node_ref_input_index: null,
        },
      },
      (transactionHash, previous) => [
        ...previous.slice(0, -1),
        {
          ...previous.at(-1)!,
          outRef: `${transactionHash}#0`,
          createdBlock: blockNo,
        },
        {
          headerHash: header.headerHash,
          outRef: `${transactionHash}#1`,
          createdBlock: blockNo,
          header,
        },
      ],
    );

  const merge = (blockNo: number, transactionIndex: number): void => {
    const previous = queue();
    const root = previous[0]!;
    const merged = previous[1]!;
    record(
      blockNo,
      transactionIndex,
      {
        MergeToConfirmedStateV1: {
          yield_to_ref_input_index: 0n,
          header_node_key: merged.headerHash!,
          confirmed_state_input_outref: outRefParts(root.outRef),
          confirmed_state_output_index: 0n,
          m_settlement_redeemer_index: null,
          merged_block_withdrawals_root: "01".repeat(32),
          merged_block_forced_transactions_root: "02".repeat(32),
          merged_block_transactions_root: "03".repeat(32),
          merged_block_deposits_root: "04".repeat(32),
          merged_block_transition_trace_root: "05".repeat(32),
          merged_block_event_to_step_root: "06".repeat(32),
          merged_block_validation_traces_root: "07".repeat(32),
          merged_block_withdrawal_count: 0n,
          merged_block_forced_transaction_count: 0n,
          merged_block_l2_transaction_count: 0n,
          merged_block_deposit_count: 0n,
          merged_block_total_event_count: 0n,
          merged_block_transition_step_count: 0n,
          merged_block_validation_trace_count: 0n,
        },
      },
      (transactionHash, queued) => [
        {
          headerHash: null,
          outRef: `${transactionHash}#0`,
          createdBlock: blockNo,
        },
        ...queued.slice(2),
      ],
      merged.headerHash!,
    );
  };

  /** Respends `headerHash`'s node with a DA attestation in its datum. */
  const attest = (
    blockNo: number,
    transactionIndex: number,
    headerHash: string,
  ): void => {
    if (!queue().some((entry) => entry.headerHash === headerHash)) {
      throw new Error("state-queue chain can only attest a queued header");
    }
    record(blockNo, transactionIndex, null, (transactionHash, previous) =>
      previous.map((entry) => {
        if (entry.headerHash !== headerHash) return entry;
        if (entry.header === undefined) {
          throw new Error("state-queue chain can only attest a header node");
        }
        return {
          ...entry,
          outRef: `${transactionHash}#0`,
          createdBlock: blockNo,
          header: {
            ...entry.header,
            daAttestation: {
              Attested: { da_bond_asset_name: "44".repeat(32) },
            },
          },
        };
      }),
    );
  };

  const checkpoint = (
    transaction: Transaction,
    tipBlockNo: number,
  ): SDK.StateQueueAuthenticatedReplayCheckpoint => {
    const derived = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
      deploymentIdentityDigest,
      stateQueuePolicyId,
      transactionHash: transaction.transactionHash,
      blockHash: transaction.blockHash,
      slot: (transaction.blockNo * 20).toString(),
      blockNo: transaction.blockNo.toString(),
      transactionIndex: transaction.transactionIndex.toString(),
      chainPointId: sha(`point:${transaction.transactionHash}`),
      finalityDepth: (tipBlockNo - transaction.blockNo + 1).toString(),
      ...(transaction.redeemer === null
        ? {
            mintPolicyIds: [],
            redeemers: [],
            referenceInputOutRefs: [],
            correctionLockWitness: { kind: "none" as const },
          }
        : {
            mintPolicyIds: [stateQueuePolicyId],
            redeemers: [
              {
                purpose: "mint",
                index: "0",
                cborHex: Data.to(transaction.redeemer, SDK.StateQueueRedeemer),
              },
            ],
            referenceInputOutRefs: [transaction.lockOutRef],
            correctionLockWitness: {
              kind: "idle_reference" as const,
              referenceOutRef: transaction.lockOutRef,
              datum: "Idle" as const,
            },
          }),
      spentInputOutRefs: transaction.spentInputOutRefs,
      previousQueue: transitionQueue(transaction.previous),
      nextQueue: transitionQueue(transaction.next),
    });
    if (derived === null) {
      throw new Error("state-queue chain produced an invalid checkpoint");
    }
    return derived;
  };

  return {
    get tip() {
      return chain.tip;
    },
    /**
     * Mines one block holding `transactions`, in order: appending a header,
     * merging the oldest one, or attesting a queued header by a datum update.
     */
    mine(
      ...transactions: readonly (
        | { append: ChainHeader }
        | { attest: string }
        | "merge"
      )[]
    ): void {
      chain.tip += 1;
      for (const [index, transaction] of transactions.entries()) {
        if (transaction === "merge") {
          merge(chain.tip, index);
        } else if ("attest" in transaction) {
          attest(chain.tip, index, transaction.attest);
        } else {
          append(chain.tip, index, transaction.append);
        }
      }
    },
    /** Rolls back the newest `blocks` blocks; later blocks form a new fork. */
    rollback(blocks: number): void {
      chain.tip -= blocks;
      chain.history = chain.history.filter(
        ({ blockNo }) => blockNo <= chain.tip,
      );
      for (const blockNo of [...blockHashes.keys()]) {
        if (blockNo > chain.tip) blockHashes.delete(blockNo);
      }
      chain.forks += 1;
    },
    queue: () => transitionQueue(queue()),
    /** Every output of `queue` is at least `finalityDepth` blocks deep. */
    isFinal: (
      anchor: readonly SDK.StateQueueTransitionNode[],
      finalityDepth: number,
    ): boolean => {
      const created = new Map<string, number>(
        [...genesis, ...chain.history.flatMap(({ next }) => next)].map(
          ({ outRef, createdBlock }) => [outRef, createdBlock],
        ),
      );
      return anchor.every(({ outRef }) => {
        const block = created.get(outRef);
        return block !== undefined && depth(block) >= finalityDepth;
      });
    },
    snapshot: (): ObservedStateQueueSnapshot => {
      const entries = queue();
      const root = entries[0]!;
      return {
        nodes: entries.slice(1).map(
          (entry): ObservedStateQueueNode => ({
            outRef: entry.outRef,
            assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${entry.headerHash!}`,
            linkedListKey: entry.headerHash!,
            header: entry.header!.header,
            daAttestation: entry.header!.daAttestation ?? SDK.NO_DA_ATTESTATION,
            chainPoint: point(entry.createdBlock),
          }),
        ),
        confirmedHeaderHash:
          [...chain.history]
            .reverse()
            .find(({ mergedHeaderHash }) => mergedHeaderHash !== undefined)
            ?.mergedHeaderHash ?? "00".repeat(28),
        confirmedStateOutRef: root.outRef,
        tipBlockNo: chain.tip,
        observedChainPoint: point(root.createdBlock),
      };
    },
    /**
     * The committee's replay source over this chain, judging depth at
     * `tipBlockNo`, the tip the caller's snapshot was read at, and stopping
     * after `limit` checkpoints.
     */
    fetchStateQueueReplayCheckpoints: async (
      anchor: readonly SDK.StateQueueTransitionNode[],
      current: readonly SDK.StateQueueTransitionNode[],
      tipBlockNo: number,
      limit: number,
    ): Promise<readonly SDK.StateQueueAuthenticatedReplayCheckpoint[]> => {
      if (sameQueue(anchor, current)) {
        return [];
      }
      const known = new Set(
        [...genesis, ...chain.history.flatMap(({ next }) => next)].map(
          ({ outRef }) => outRef,
        ),
      );
      for (const { outRef } of anchor) {
        if (!known.has(outRef)) {
          throw new StateQueueHistoryNotExtendingAnchorError(
            `Kupo does not know state-queue output ${outRef} exactly once (0 matches)`,
          );
        }
      }
      const states = [genesis, ...chain.history.map(({ next }) => next)];
      let from = -1;
      for (const [index, state] of states.entries()) {
        if (sameQueue(state, anchor)) from = index;
      }
      if (from < 0) {
        throw new StateQueueHistoryNotExtendingAnchorError(
          "committee replay cannot advance its durable queue",
        );
      }
      return chain.history
        .slice(from, from + limit)
        .map((transaction) => checkpoint(transaction, tipBlockNo));
    },
  };
};

export type StateQueueChain = ReturnType<typeof createStateQueueChain>;
