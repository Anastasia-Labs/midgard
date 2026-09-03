import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type {
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { terminalRetentionOutcomes } from "../src/l1/terminal-retention-observation.js";
import { fixtureHeaderBase } from "./helpers.js";

const deployment = "aa".repeat(32);
const policy = "bb".repeat(28);
const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index: number): string =>
  `${h32(byte)}#${index.toString()}`;
const point = (slot: number, depth = 30) => ({
  slot,
  blockHash: slot.toString(16).padStart(2, "0").repeat(32),
  depth,
  finalized: depth >= 30,
  observedAt: "2026-08-29T00:00:00.000Z",
  providerSource: "local-kupo,local-ogmios",
});
const record = (
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
  daAttestation: { Attested: { da_bond_asset_name: h32("c") } },
  observedChainPoint: point(1, 1),
  finalized: false,
  status: "attested",
  validationErrors: [],
  updatedAt: "2026-08-28T00:00:00.000Z",
});
const snapshot = (
  confirmedHeaderHash: string,
  confirmedStateOutRef: string,
): ObservedStateQueueSnapshot => ({
  nodes: [],
  confirmedHeaderHash,
  confirmedStateOutRef,
  observedChainPoint: point(100),
});
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
            "RemoveTimedOutHead" in timeout.removal_approach
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
    finalityDepth: "30",
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
const merge = (
  sequence: number,
  previousQueue: SDK.StateQueueTransitionNode[],
): SDK.StateQueueAuthenticatedReplayCheckpoint => {
  const header = previousQueue[1]!;
  const txHash = h32(sequence.toString(16));
  return derive(
    sequence,
    previousQueue,
    [{ headerHash: null, outRef: `${txHash}#0` }, ...previousQueue.slice(2)],
    {
      MergeToConfirmedStateV1: {
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
  );
};
const timeout = (
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
        timed_out_header_hash: timedOut.headerHash!,
        removal_approach:
          descendant === undefined
            ? {
                RemoveTimedOutHead: {
                  confirmed_state_input_outref: {
                    transactionId: continued.outRef.slice(0, 64),
                    outputIndex: BigInt(continued.outRef.split("#")[1]!),
                  },
                  confirmed_state_output_index: 0n,
                },
              }
            : {
                PruneTimedOutBlockDescendant: {
                  confirmed_state_ref_input_index: 0n,
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
const config = (queue: SDK.StateQueueTransitionNode[]) => ({
  deploymentFingerprint: deployment,
  deploymentIdentityDigest: deployment,
  stateQueuePolicyId: policy,
  finalityDepth: 30,
  replayAnchor: {
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    queue,
    blockNo: "0",
    transactionIndex: "0",
  },
});

describe("terminalRetentionOutcomesV1", () => {
  it("does not infer a terminal outcome from disappearance", () => {
    const prior = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: prior.headerHash, outRef: prior.stateQueueOutRef },
    ];
    expect(
      terminalRetentionOutcomes(
        [prior],
        [],
        [],
        snapshot(prior.headerHash, outRef("0", 0)),
        config(initial),
      ),
    ).toEqual([]);
  });

  it("rejects unanchored and disconnected canonical histories", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    expect(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        { ...config(initial), replayAnchor: undefined },
      ),
    ).toThrow(/durable prior/u);
    expect(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        config([{ headerHash: null, outRef: outRef("9", 0) }]),
      ),
    ).toThrow(/does not extend/u);
  });

  it("replays two merges and records both exact outcomes", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial);
    const two = merge(2, [...one.nextQueue]);
    const result = terminalRetentionOutcomes(
      [first, second],
      [],
      [one, two],
      snapshot(second.headerHash, two.nextQueue[0]!.outRef),
      config(initial),
    );
    expect(
      result.map(({ headerHash, status }) => [headerHash, status]),
    ).toEqual([
      [first.headerHash, "merged"],
      [second.headerHash, "merged"],
    ]);
    expect(
      result.map(({ finalized, observedChainPoint }) => ({
        finalized,
        source: observedChainPoint.providerSource,
        depth: observedChainPoint.depth,
      })),
    ).toEqual([
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 30,
      },
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 30,
      },
    ]);
  });

  it.each(["merge_first", "removal_first"] as const)(
    "replays merge+timeout removal in %s order",
    (order) => {
      const first = record(h28("1"), outRef("1", 0));
      const second = record(h28("2"), outRef("2", 0));
      const initial = [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
        { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
      ];
      const one =
        order === "merge_first" ? merge(1, initial) : timeout(1, initial);
      const two =
        order === "merge_first"
          ? timeout(2, [...one.nextQueue])
          : merge(2, [...one.nextQueue]);
      const result = terminalRetentionOutcomes(
        [first, second],
        [],
        [one, two],
        snapshot(first.headerHash, two.nextQueue[0]!.outRef),
        config(initial),
      );
      expect(
        new Map(result.map(({ headerHash, status }) => [headerHash, status])),
      ).toEqual(
        new Map([
          [first.headerHash, "merged"],
          [second.headerHash, "removed"],
        ]),
      );
    },
  );
});
