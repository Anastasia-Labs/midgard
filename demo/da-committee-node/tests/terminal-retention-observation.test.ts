import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type {
  ObservedStateQueueSnapshot,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  catchUpRetentionOutcomes,
  terminalRetentionOutcomes,
} from "../src/l1/terminal-retention-observation.js";
import { fixtureHeaderBase } from "./helpers.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";

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
const thrownBy = (run: () => unknown): unknown => {
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
const merge = (
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
const config = (queue: SDK.StateQueueTransitionNode[]) => ({
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
      ).records,
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

  it("classifies history that fails to extend the durable cursor as integrity", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    const failure = thrownBy(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        config([{ headerHash: null, outRef: outRef("9", 0) }]),
      ),
    );
    expect((failure as Error).message).toMatch(/does not extend/u);
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
  });

  it("defers the outcome of history younger than the finality depth instead of failing", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    // The root snapshot itself is younger than the finality depth.
    const young = {
      ...snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
      observedChainPoint: point(100, 1),
    };

    // The checkpoint has 29 blocks on top; the committee requires 30.
    const early = terminalRetentionOutcomes([first], [], [transition], young, {
      ...config(initial),
      finalityDepth: 30,
    });
    expect(early).toEqual({
      records: [],
      deferredHeaderHashes: [first.headerHash],
      finalSteps: new Map(),
    });

    // Once it is final, the deferred outcome is recorded and the queue after
    // it becomes the final anchor.
    const final = terminalRetentionOutcomes(
      [first],
      [],
      [transition],
      young,
      config(initial),
    );
    expect(final.records.map(({ status }) => status)).toEqual(["merged"]);
    expect(final.deferredHeaderHashes).toEqual([]);
    expect(final.finalSteps).toEqual(
      new Map([
        [
          first.headerHash,
          [
            {
              fromOutRef: first.stateQueueOutRef,
              slot: Number(transition.slot),
              blockHash: transition.blockHash,
            },
          ],
        ],
      ]),
    );
    expect(final.finalAnchor).toEqual({
      queue: transition.nextQueue,
      blockNo: transition.blockNo,
      transactionIndex: transition.transactionIndex,
    });
  });

  it("records only the final prefix of the history and defers the rest", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 29);
    const result = terminalRetentionOutcomes(
      [first, second],
      [],
      [one, two],
      snapshot(second.headerHash, two.nextQueue[0]!.outRef),
      config(initial),
    );
    expect(
      result.records.map(({ headerHash, status }) => [headerHash, status]),
    ).toEqual([[first.headerHash, "merged"]]);
    expect(result.deferredHeaderHashes).toEqual([second.headerHash]);
    expect(result.finalAnchor).toEqual({
      queue: one.nextQueue,
      blockNo: one.blockNo,
      transactionIndex: one.transactionIndex,
    });
  });

  it("still checks history younger than the finality depth for integrity", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 1);
    // The young checkpoint is canonical but does not reproduce the snapshot.
    const mismatch = thrownBy(() =>
      terminalRetentionOutcomes(
        [first, second],
        [],
        [one, two],
        snapshot(second.headerHash, outRef("9", 0)),
        config(initial),
      ),
    );
    expect((mismatch as Error).message).toMatch(/does not match the exact/u);
    expect(mismatch).toBeInstanceOf(L1SourceIntegrityError);
    // A young checkpoint that does not extend the final one.
    const disconnected = thrownBy(() =>
      terminalRetentionOutcomes(
        [first, second],
        [],
        [one, merge(3, initial, 1)],
        snapshot(first.headerHash, outRef("2", 0)),
        config(initial),
      ),
    );
    expect((disconnected as Error).message).toMatch(/does not extend/u);
    expect(disconnected).toBeInstanceOf(L1SourceIntegrityError);
  });

  it.each([
    ["final", true],
    ["not final", false],
  ] as const)(
    "defers a header whose final history lands on an output the snapshot reports %s only when it is not final",
    async (_label, finalized) => {
      const moved = record(h28("1"), outRef("1", 0));
      const chain = createStateQueueChain({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        headers: [{ header: moved.header, headerHash: moved.headerHash }],
        tip: 1,
      });
      const anchor = chain.queue();
      chain.mine({ attest: moved.headerHash });
      const history = await chain.fetchStateQueueReplayCheckpoints(
        anchor,
        chain.queue(),
        // Final history, however the snapshot judged its output.
        40,
        64,
      );
      const current = {
        ...moved,
        stateQueueOutRef: chain.queue()[1]!.outRef,
        finalized,
      };
      const result = terminalRetentionOutcomes(
        [{ ...moved, stateQueueOutRef: anchor[1]!.outRef }],
        [current],
        history,
        snapshot("00".repeat(28), anchor[0]!.outRef),
        {
          ...config([...anchor]),
          replayAnchor: {
            ...config([...anchor]).replayAnchor,
            blockNo: "1",
          },
        },
      );
      expect(result.finalSteps.get(moved.headerHash)?.at(-1)?.toOutRef).toBe(
        current.stateQueueOutRef,
      );
      if (finalized) {
        expect(result.deferredHeaderHashes).toEqual([]);
        expect(result.finalAnchor?.queue).toEqual(chain.queue());
      } else {
        // Judged on the disagreement it would look unexplained; it waits.
        expect(result.deferredHeaderHashes).toEqual([moved.headerHash]);
        expect(result.finalAnchor).toBeUndefined();
      }
    },
  );

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
      result.records.map(({ headerHash, status }) => [headerHash, status]),
    ).toEqual([
      [first.headerHash, "merged"],
      [second.headerHash, "merged"],
    ]);
    expect(
      result.records.map(({ finalized, observedChainPoint }) => ({
        finalized,
        source: observedChainPoint.providerSource,
        depth: observedChainPoint.depth,
      })),
    ).toEqual([
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 29,
      },
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 29,
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
        new Map(
          result.records.map(({ headerHash, status }) => [headerHash, status]),
        ),
      ).toEqual(
        new Map([
          [first.headerHash, "merged"],
          [second.headerHash, "removed"],
        ]),
      );
    },
  );
});

describe("catchUpRetentionOutcomes", () => {
  const twoHeaders = () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    return { first, second, initial };
  };

  it("takes steps, outcomes, and the anchor only from the final prefix of the walk", () => {
    const { first, second, initial } = twoHeaders();
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 29);
    const result = catchUpRetentionOutcomes(
      [first, second],
      [one, two],
      config(initial),
    );
    expect([...result!.finalSteps.keys()]).toEqual([first.headerHash]);
    expect(result!.terminalStatuses).toEqual(
      new Map([[first.headerHash, "merged"]]),
    );
    expect(
      result!.terminalRecords.map(({ headerHash, status }) => [
        headerHash,
        status,
      ]),
    ).toEqual([[first.headerHash, "merged"]]);
    expect(result!.finalAnchor).toEqual({
      queue: one.nextQueue,
      blockNo: one.blockNo,
      transactionIndex: one.transactionIndex,
    });
  });

  it("makes no progress on a walk none of whose checkpoints is final", () => {
    const { first, second, initial } = twoHeaders();
    expect(
      catchUpRetentionOutcomes(
        [first, second],
        [merge(1, initial, 29)],
        config(initial),
      ),
    ).toBeUndefined();
  });

  it("refuses an anchor of another release as an integrity failure", () => {
    const { first, second, initial } = twoHeaders();
    const base = config(initial);
    const failure = thrownBy(() =>
      catchUpRetentionOutcomes([first, second], [merge(1, initial)], {
        ...base,
        replayAnchor: {
          ...base.replayAnchor,
          deploymentIdentityDigest: "cc".repeat(32),
        },
      }),
    );
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
    expect((failure as Error).message).toBe(
      "state-queue durable replay anchor release mismatch",
    );
  });

  it("refuses a stored header of another deployment as an integrity failure", () => {
    const { first, second, initial } = twoHeaders();
    const failure = thrownBy(() =>
      catchUpRetentionOutcomes(
        [first, { ...second, deploymentFingerprint: "cc".repeat(32) }],
        [merge(1, initial)],
        config(initial),
      ),
    );
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
    expect((failure as Error).message).toBe(
      `stored state-queue header ${second.headerHash} belongs to a foreign deployment`,
    );
  });
});
