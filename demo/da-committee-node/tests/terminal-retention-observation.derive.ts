import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { fixtureHeaderBase } from "./helpers.js";

export const deployment = "aa".repeat(32);

export const policy = "bb".repeat(28);

export const h28 = (byte: string): string => byte.repeat(56);

const h32 = (byte: string): string => byte.repeat(64);

export const outRef = (byte: string, index: number): string =>
  `${h32(byte)}#${index.toString()}`;

export const point = (slot: number, depth = 30) => ({
  slot,
  blockHash: slot.toString(16).padStart(2, "0").repeat(32),
  depth,
  finalized: depth >= 30,
  observedAt: "2026-08-29T00:00:00.000Z",
  providerSource: "local-kupo,local-ogmios",
});

export const record = (
  headerHash: string,
  stateQueueOutRef: string,
): StateQueueHeaderRecord => ({
  deploymentFingerprint: deployment,
  headerHash,
  stateQueueOutRef,
  blockAssetName: `000643b0${headerHash}`,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  },
  computedHeaderHash: headerHash,
  daAttestation: { Attested: { commitment_hash: h32("c") } },
  observedChainPoint: point(1, 1),
  finalized: false,
  status: "attested",
  validationErrors: [],
  updatedAt: "2026-08-28T00:00:00.000Z",
});

export const snapshot = (
  confirmedHeaderHash: string,
  confirmedStateOutRef: string,
): ObservedStateQueueSnapshot => ({
  nodes: [],
  confirmedHeaderHash,
  confirmedStateOutRef,
  observedChainPoint: point(100),
});

export const thrownBy = (run: () => unknown): unknown => {
  try {
    run();
  } catch (error) {
    return error;
  }
  throw new Error("expected a throw");
};

const redeemers = (value: SDK.StateQueueRedeemer) => [
  {
    purpose: "mint",
    index: "0",
    cborHex: Data.to(value, SDK.StateQueueRedeemer),
  },
];

const derive = (
  sequence: number,
  previousQueue: SDK.StateQueueTransitionNode[],
  nextQueue: SDK.StateQueueTransitionNode[],
  value: SDK.StateQueueRedeemer,
  finalityDepth = 30,
): SDK.StateQueueAuthenticatedReplayCheckpoint => {
  const transactionHash = h32(sequence.toString(16));
  const lockOutRef = outRef("f", sequence);
  const timeout =
    typeof value === "object" &&
    value !== null &&
    "RemoveUnattestedBlockAfterTimeout" in value
      ? value.RemoveUnattestedBlockAfterTimeout
      : null;
  const lockWitness: SDK.StateQueueCorrectionLockWitness =
    timeout === null
      ? {
          kind: "idle_reference",
          referenceOutRef: lockOutRef,
          datum: "Idle",
        }
      : {
          kind: "correction_transition",
          consumedOutRef: lockOutRef,
          continuedOutRef: `${transactionHash}#9`,
          targetHeaderHash: timeout.timed_out_header_hash,
          correctionIdentity: "AttestationTimeout",
          previousDatum: "Idle",
          nextDatum:
            "RemoveLastUnattestedBlock" in timeout.removal_approach
              ? "Idle"
              : {
                  Locked: {
                    target_header_hash: timeout.timed_out_header_hash,
                    correction_identity: "AttestationTimeout",
                  },
                },
        };
  const transition = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 4).toString(16)),
    finalityDepth: finalityDepth.toString(),
    mintPolicyIds: [policy],
    redeemers: redeemers(value),
    spentInputOutRefs: [
      ...previousQueue
        .filter((node) =>
          nextQueue.every(
            (next) =>
              next.headerHash !== node.headerHash ||
              next.outRef !== node.outRef,
          ),
        )
        .map(({ outRef: reference }) => reference),
      ...(timeout === null ? [] : [lockOutRef]),
    ],
    referenceInputOutRefs: timeout === null ? [lockOutRef] : [],
    correctionLockWitness: lockWitness,
    previousQueue,
    nextQueue,
  });
  if (transition === null) throw new Error("invalid transition fixture");
  return transition;
};

export const merge = (
  sequence: number,
  previousQueue: SDK.StateQueueTransitionNode[],
  finalityDepth = 30,
): SDK.StateQueueAuthenticatedReplayCheckpoint => {
  const header = previousQueue[1]!;
  const txHash = h32(sequence.toString(16));
  return derive(
    sequence,
    previousQueue,
    [{ headerHash: null, outRef: `${txHash}#0` }, ...previousQueue.slice(2)],
    {
      MergeToConfirmedStateV1: {
        yield_to_ref_input_index: 0n,
        header_node_key: header.headerHash!,
        confirmed_state_input_outref: {
          transactionId: previousQueue[0]!.outRef.slice(0, 64),
          outputIndex: BigInt(previousQueue[0]!.outRef.split("#")[1]!),
        },
        confirmed_state_output_index: 0n,
        m_settlement_redeemer_index: null,
        merged_block_withdrawals_root: h32("1"),
        merged_block_forced_transactions_root: h32("2"),
        merged_block_transactions_root: h32("3"),
        merged_block_deposits_root: h32("4"),
        merged_block_transition_trace_root: h32("5"),
        merged_block_event_to_step_root: h32("6"),
        merged_block_validation_traces_root: h32("7"),
        merged_block_withdrawal_count: 0n,
        merged_block_forced_transaction_count: 0n,
        merged_block_l2_transaction_count: 0n,
        merged_block_deposit_count: 0n,
        merged_block_total_event_count: 0n,
        merged_block_transition_step_count: 0n,
        merged_block_validation_trace_count: 0n,
      },
    },
    finalityDepth,
  );
};

export const timeout = (
  sequence: number,
  previousQueue: SDK.StateQueueTransitionNode[],
): SDK.StateQueueAuthenticatedReplayCheckpoint => {
  const timedOut = previousQueue[1]!;
  const txHash = h32(sequence.toString(16));
  const descendant = previousQueue[2];
  const continued = descendant === undefined ? previousQueue[0]! : timedOut;
  return derive(
    sequence,
    previousQueue,
    descendant === undefined
      ? [{ headerHash: null, outRef: `${txHash}#0` }]
      : [
          previousQueue[0]!,
          { headerHash: timedOut.headerHash, outRef: `${txHash}#1` },
          ...previousQueue.slice(3),
        ],
    {
      RemoveUnattestedBlockAfterTimeout: {
        yield_to_ref_input_index: 0n,
        timed_out_header_hash: timedOut.headerHash!,
        removal_approach:
          descendant === undefined
            ? {
                RemoveLastUnattestedBlock: {
                  predecessor_input_outref: {
                    transactionId: continued.outRef.slice(0, 64),
                    outputIndex: BigInt(continued.outRef.split("#")[1]!),
                  },
                  predecessor_output_index: 0n,
                },
              }
            : {
                PruneUnattestedBlockDescendant: {
                  predecessor_ref_input_index: 0n,
                  timed_out_node_input_outref: {
                    transactionId: continued.outRef.slice(0, 64),
                    outputIndex: BigInt(continued.outRef.split("#")[1]!),
                  },
                  timed_out_node_output_index: 1n,
                },
              },
      },
    },
  );
};

export const config = (queue: SDK.StateQueueTransitionNode[]) => ({
  deploymentFingerprint: deployment,
  deploymentIdentityDigest: deployment,
  stateQueuePolicyId: policy,
  // Checkpoints are 30 deep counting their own block: 29 blocks on top.
  finalityDepth: 29,
  replayAnchor: {
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    queue,
    blockNo: "0",
    transactionIndex: "0",
  },
});
