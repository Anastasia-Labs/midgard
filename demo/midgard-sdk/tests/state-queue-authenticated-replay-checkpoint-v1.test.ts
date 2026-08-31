import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveStateQueueAuthenticatedReplayCheckpointV1,
  parseStateQueueAuthenticatedReplayCheckpointV1,
  replayStateQueueAuthenticatedCheckpointsV1,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueTransitionNodeV1,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index = 0): string =>
  `${h32(byte)}#${index.toString()}`;
const deployment = h32("a");
const policy = h28("b");
const header1 = h28("1");
const header2 = h28("2");

const redeemer = (value: StateQueueRedeemerType) => [
  {
    purpose: "mint",
    index: "0",
    cborHex: Data.to(value, StateQueueRedeemer),
  },
];
const derive = ({
  sequence,
  previousQueue,
  nextQueue,
  value,
  spentInputOutRefs,
  withPolicy = true,
}: {
  sequence: number;
  previousQueue: readonly StateQueueTransitionNodeV1[];
  nextQueue: readonly StateQueueTransitionNodeV1[];
  value?: StateQueueRedeemerType;
  spentInputOutRefs: readonly string[];
  withPolicy?: boolean;
}) => {
  const transactionHash = h32(sequence.toString(16));
  const lockOutRef = outRef("f", sequence);
  const lock =
    value !== undefined &&
    typeof value === "object" &&
    value !== null &&
    "InitV1" in value
      ? ({
          kind: "genesis",
          producedOutRef: `${transactionHash}#9`,
          nextDatum: "Idle",
        } as const)
      : value === "Deinit"
        ? ({
            kind: "deinit",
            consumedOutRef: lockOutRef,
            previousDatum: "Idle",
          } as const)
        : value !== undefined &&
            typeof value === "object" &&
            value !== null &&
            "CommitBlockHeader" in value
          ? ({
              kind: "idle_reference",
              referenceOutRef: lockOutRef,
              datum: "Idle",
            } as const)
          : value !== undefined &&
              typeof value === "object" &&
              value !== null &&
              "MergeToConfirmedStateV1" in value
            ? ({
                kind: "idle_reference",
                referenceOutRef: lockOutRef,
                datum: "Idle",
              } as const)
            : value !== undefined &&
                typeof value === "object" &&
                value !== null &&
                "RemoveUnattestedBlockAfterTimeout" in value
              ? ({
                  kind: "correction_transition",
                  consumedOutRef: lockOutRef,
                  continuedOutRef: `${transactionHash}#9`,
                  targetHeaderHash:
                    value.RemoveUnattestedBlockAfterTimeout
                      .timed_out_header_hash,
                  correctionIdentity: "AttestationTimeout",
                  previousDatum: "Idle",
                  nextDatum:
                    "RemoveTimedOutHead" in
                    value.RemoveUnattestedBlockAfterTimeout.removal_approach
                      ? "Idle"
                      : {
                          Locked: {
                            target_header_hash:
                              value.RemoveUnattestedBlockAfterTimeout
                                .timed_out_header_hash,
                            correction_identity: "AttestationTimeout" as const,
                          },
                        },
                } as const)
              : value !== undefined &&
                  typeof value === "object" &&
                  value !== null &&
                  "RemoveFraudulentBlockHeader" in value
                ? (() => {
                    const target =
                      value.RemoveFraudulentBlockHeader
                        .fraudulent_blocks_header_hash;
                    const identity = {
                      FraudProof: {
                        fraud_proof_asset_name: `00000001${target}`,
                      },
                    } as const;
                    return {
                      kind: "correction_transition",
                      consumedOutRef: lockOutRef,
                      continuedOutRef: `${transactionHash}#9`,
                      targetHeaderHash: target,
                      correctionIdentity: identity,
                      previousDatum: "Idle",
                      nextDatum:
                        "RemoveLastFraudulentBlock" in
                        value.RemoveFraudulentBlockHeader.block_removal_approach
                          ? "Idle"
                          : {
                              Locked: {
                                target_header_hash: target,
                                correction_identity: identity,
                              },
                            },
                    } as const;
                  })()
                : ({ kind: "none" } as const);
  const referenceInputOutRefs =
    lock.kind === "idle_reference" ? [lock.referenceOutRef] : [];
  const exactSpentInputOutRefs =
    lock.kind === "correction_transition" || lock.kind === "deinit"
      ? [...spentInputOutRefs, lock.consumedOutRef]
      : spentInputOutRefs;
  const checkpoint = deriveStateQueueAuthenticatedReplayCheckpointV1({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash,
    blockHash: h32((sequence + 4).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 8).toString(16)),
    finalityDepth: "30",
    mintPolicyIds: withPolicy ? [policy] : [],
    redeemers: value === undefined ? [] : redeemer(value),
    spentInputOutRefs: exactSpentInputOutRefs,
    referenceInputOutRefs,
    correctionLockWitness: lock,
    previousQueue,
    nextQueue,
  });
  if (checkpoint === null) throw new Error("invalid checkpoint fixture");
  return checkpoint;
};

const mergeRedeemer = (root: string): StateQueueRedeemerType => ({
  MergeToConfirmedStateV1: {
    header_node_key: header1,
    confirmed_state_input_outref: {
      transactionId: root.slice(0, 64),
      outputIndex: BigInt(root.split("#")[1]!),
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
});

describe("authenticated state-queue replay checkpoint V1", () => {
  it("embeds exact timeout and fraud-removal terminal envelopes", () => {
    const anchor = [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: header1, outRef: outRef("1") },
      { headerHash: header2, outRef: outRef("2") },
    ];
    const timeout = derive({
      sequence: 6,
      previousQueue: anchor,
      nextQueue: [anchor[0]!, { headerHash: header1, outRef: `${h32("6")}#0` }],
      value: {
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: header1,
          removal_approach: {
            PruneTimedOutBlockDescendant: {
              confirmed_state_ref_input_index: 0n,
              timed_out_node_input_outref: {
                transactionId: h32("1"),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 0n,
            },
          },
        },
      },
      spentInputOutRefs: [anchor[1]!.outRef, anchor[2]!.outRef],
    });
    const fraud = derive({
      sequence: 7,
      previousQueue: anchor,
      nextQueue: [anchor[0]!, { headerHash: header1, outRef: `${h32("7")}#0` }],
      value: {
        RemoveFraudulentBlockHeader: {
          fraudulent_operator: h28("f"),
          fraudulent_blocks_header_hash: header2,
          slashing_approach: {
            OperatorAlreadySlashed: {
              active_operators_element_ref_input_index: 0n,
              retired_operators_element_ref_input_index: 1n,
            },
          },
          fraud_proof_ref_input_index: 0n,
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: {
                transactionId: h32("1"),
                outputIndex: 0n,
              },
              anchor_element_output_index: 0n,
            },
          },
        },
      },
      spentInputOutRefs: [anchor[1]!.outRef, anchor[2]!.outRef],
    });
    expect([timeout.checkpointKind, fraud.checkpointKind]).toEqual([
      "timeout_correction",
      "fraud_removal",
    ]);
    expect(timeout.terminalTransition?.correctionTransition).not.toBeNull();
    expect(fraud.terminalTransition?.correctionTransition).toBeNull();
    expect(fraud.terminalTransition?.removedHeaderHashes).toEqual([header2]);
    expect(parseStateQueueAuthenticatedReplayCheckpointV1(fraud)).toEqual(
      fraud,
    );
    expect(
      replayStateQueueAuthenticatedCheckpointsV1({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        minimumFinalityDepth: 30n,
        anchor: { queue: anchor, blockNo: "0", transactionIndex: "0" },
        checkpoints: [fraud],
      }),
    ).toMatchObject({
      queue: fraud.nextQueue,
      terminals: [fraud.terminalTransition],
    });
  });

  it("derives init, append, no-mint datum update, merge, and deinit", () => {
    const init = derive({
      sequence: 1,
      previousQueue: [],
      nextQueue: [{ headerHash: null, outRef: `${h32("1")}#0` }],
      value: { InitV1: { output_index: 0n } },
      spentInputOutRefs: [],
    });
    const root = init.nextQueue[0]!.outRef;
    const beforeAppend = [
      { headerHash: null, outRef: root },
      { headerHash: header1, outRef: outRef("3") },
    ];
    const append = derive({
      sequence: 2,
      previousQueue: beforeAppend,
      nextQueue: [
        beforeAppend[0]!,
        { headerHash: header1, outRef: `${h32("2")}#0` },
        { headerHash: header2, outRef: `${h32("2")}#1` },
      ],
      value: {
        CommitBlockHeader: {
          new_block_output_index: 1n,
          continued_latest_block_output_index: 0n,
          operator: h28("9"),
          scheduler_ref_input_index: 0n,
          active_operators_input_index: 0n,
          active_operators_redeemer_index: 0n,
          m_confirmed_state_ref_input_index: null,
          m_head_state_queue_node_ref_input_index: null,
        },
      },
      spentInputOutRefs: [beforeAppend[1]!.outRef],
    });
    const update = derive({
      sequence: 3,
      previousQueue: append.nextQueue,
      nextQueue: [
        append.nextQueue[0]!,
        { headerHash: header1, outRef: `${h32("3")}#0` },
        append.nextQueue[2]!,
      ],
      spentInputOutRefs: [append.nextQueue[1]!.outRef],
      withPolicy: false,
    });
    const merge = derive({
      sequence: 4,
      previousQueue: update.nextQueue,
      nextQueue: [
        { headerHash: null, outRef: `${h32("4")}#0` },
        update.nextQueue[2]!,
      ],
      value: mergeRedeemer(update.nextQueue[0]!.outRef),
      spentInputOutRefs: [
        update.nextQueue[0]!.outRef,
        update.nextQueue[1]!.outRef,
      ],
    });
    const deinit = derive({
      sequence: 5,
      previousQueue: [merge.nextQueue[0]!],
      nextQueue: [],
      value: "Deinit",
      spentInputOutRefs: [merge.nextQueue[0]!.outRef],
    });
    expect(
      [init, append, update, merge, deinit].map((item) => item.checkpointKind),
    ).toEqual(["init", "append", "datum_update", "merge", "deinit"]);
    expect(merge.terminalTransition?.transitionKind).toBe("merge");
    for (const item of [init, append, update, merge, deinit]) {
      expect(parseStateQueueAuthenticatedReplayCheckpointV1(item)).toEqual(
        item,
      );
    }
  });

  it("replays interleaved nonterminal/terminal checkpoints and rejects gaps/reordering/finality", () => {
    const anchor = [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: header1, outRef: outRef("9") },
    ];
    const append = derive({
      sequence: 1,
      previousQueue: anchor,
      nextQueue: [
        anchor[0]!,
        { headerHash: header1, outRef: `${h32("1")}#0` },
        { headerHash: header2, outRef: `${h32("1")}#1` },
      ],
      value: {
        CommitBlockHeader: {
          new_block_output_index: 1n,
          continued_latest_block_output_index: 0n,
          operator: h28("9"),
          scheduler_ref_input_index: 0n,
          active_operators_input_index: 0n,
          active_operators_redeemer_index: 0n,
          m_confirmed_state_ref_input_index: null,
          m_head_state_queue_node_ref_input_index: null,
        },
      },
      spentInputOutRefs: [anchor[1]!.outRef],
    });
    const merge = derive({
      sequence: 2,
      previousQueue: append.nextQueue,
      nextQueue: [
        { headerHash: null, outRef: `${h32("2")}#0` },
        append.nextQueue[2]!,
      ],
      value: mergeRedeemer(append.nextQueue[0]!.outRef),
      spentInputOutRefs: [
        append.nextQueue[0]!.outRef,
        append.nextQueue[1]!.outRef,
      ],
    });
    const input = {
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      minimumFinalityDepth: 30n,
      anchor: { queue: anchor, blockNo: "0", transactionIndex: "0" },
    } as const;
    expect(
      replayStateQueueAuthenticatedCheckpointsV1({
        ...input,
        checkpoints: [append, merge],
      }),
    ).toMatchObject({
      queue: merge.nextQueue,
      terminals: [merge.terminalTransition],
    });
    expect(
      replayStateQueueAuthenticatedCheckpointsV1({
        ...input,
        checkpoints: [merge],
      }),
    ).toBeNull();
    expect(
      replayStateQueueAuthenticatedCheckpointsV1({
        ...input,
        checkpoints: [merge, append],
      }),
    ).toBeNull();
    expect(
      replayStateQueueAuthenticatedCheckpointsV1({
        ...input,
        minimumFinalityDepth: 31n,
        checkpoints: [append, merge],
      }),
    ).toBeNull();
  });

  it("rejects forged terminal embedding and structurally extended/reordered nodes", () => {
    const root = outRef("0");
    const checkpoint = derive({
      sequence: 1,
      previousQueue: [
        { headerHash: null, outRef: root },
        { headerHash: header1, outRef: outRef("1") },
      ],
      nextQueue: [{ headerHash: null, outRef: `${h32("1")}#0` }],
      value: mergeRedeemer(root),
      spentInputOutRefs: [root, outRef("1")],
    });
    expect(
      parseStateQueueAuthenticatedReplayCheckpointV1({
        ...checkpoint,
        terminalTransition: null,
      }),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedReplayCheckpointV1({
        ...checkpoint,
        previousQueue: [...checkpoint.previousQueue].reverse(),
      }),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedReplayCheckpointV1({
        ...checkpoint,
        trusted: true,
      }),
    ).toBeNull();
  });
});
