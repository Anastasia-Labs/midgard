import { createHash } from "node:crypto";

import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveStateQueueAuthenticatedTransitionV1,
  deriveStateQueueCorrectionTransitionV1,
  type DeriveStateQueueCorrectionTransitionV1Input,
  parseStateQueueAuthenticatedTransitionV1,
  parseStateQueueCorrectionTransitionV1,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  withStateQueueAuthenticatedTransitionFinalityDepthV1,
  withStateQueueCorrectionTransitionFinalityDepthV1,
} from "../src/index.js";

const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};

const rehash = <T extends { readonly transitionDigest: string }>(
  value: T,
): T => {
  const { transitionDigest: _ignored, ...canonical } = value;
  return {
    ...canonical,
    transitionDigest: createHash("sha256")
      .update(canonicalJson(canonical))
      .digest("hex"),
  } as T;
};

const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index: number): string =>
  `${h32(byte)}#${index.toString()}`;

const timeoutRedeemer = (
  value: StateQueueRedeemerType,
): DeriveStateQueueCorrectionTransitionV1Input["redeemers"] => [
  {
    purpose: "mint",
    index: "0",
    cborHex: Data.to(value, StateQueueRedeemer),
  },
];

const common = {
  deploymentIdentityDigest: h32("a"),
  stateQueuePolicyId: h28("b"),
  transactionHash: h32("c"),
  blockHash: h32("d"),
  slot: "100",
  blockNo: "90",
  chainPointId: h32("e"),
  finalityDepth: "2160",
  mintPolicyIds: [h28("b")],
} as const;

const timeoutLock = (target: string, terminal: boolean) => ({
  referenceInputOutRefs: [],
  correctionLockWitness: {
    kind: "correction_transition" as const,
    consumedOutRef: outRef("f", 0),
    continuedOutRef: outRef("c", 9),
    targetHeaderHash: target,
    correctionIdentity: "AttestationTimeout" as const,
    previousDatum: "Idle" as const,
    nextDatum: terminal
      ? ("Idle" as const)
      : ({
          Locked: {
            target_header_hash: target,
            correction_identity: "AttestationTimeout" as const,
          },
        } as const),
  },
});

const idleLockReference = {
  referenceInputOutRefs: [outRef("f", 0)],
  correctionLockWitness: {
    kind: "idle_reference" as const,
    referenceOutRef: outRef("f", 0),
    datum: "Idle" as const,
  },
};

const fraudLock = (target: string, terminal: boolean) => {
  const correctionIdentity = {
    FraudProof: { fraud_proof_asset_name: `00000001${target}` },
  } as const;
  return {
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition" as const,
      consumedOutRef: outRef("f", 0),
      continuedOutRef: outRef("c", 9),
      targetHeaderHash: target,
      correctionIdentity,
      previousDatum: "Idle" as const,
      nextDatum: terminal
        ? ("Idle" as const)
        : ({
            Locked: {
              target_header_hash: target,
              correction_identity: correctionIdentity,
            },
          } as const),
    },
  };
};

describe("state-queue correction transition V1", () => {
  it("derives a finalized descendant-prune record from the exact mint arm and topology", () => {
    const target = h28("1");
    const removed = h28("2");
    const input = {
      ...common,
      spentInputOutRefs: [outRef("1", 0), outRef("2", 0)],
      previousQueue: [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: target, outRef: outRef("1", 0) },
        { headerHash: removed, outRef: outRef("2", 0) },
        { headerHash: h28("3"), outRef: outRef("3", 0) },
      ],
      nextQueue: [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: target, outRef: outRef("c", 1) },
        { headerHash: h28("3"), outRef: outRef("3", 0) },
      ],
      redeemers: timeoutRedeemer({
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: target,
          removal_approach: {
            PruneTimedOutBlockDescendant: {
              confirmed_state_ref_input_index: 0n,
              timed_out_node_input_outref: {
                transactionId: h32("1"),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 1n,
            },
          },
        },
      }),
    } satisfies DeriveStateQueueCorrectionTransitionV1Input;
    const transition = deriveStateQueueCorrectionTransitionV1(input);
    expect(transition).toMatchObject({
      removalApproach: "PruneTimedOutBlockDescendant",
      timedOutHeaderHash: target,
      removedHeaderHashes: [removed],
      consumedQueueOutRefs: [outRef("1", 0), outRef("2", 0)],
      continuedQueueOutRefs: [
        {
          headerHash: target,
          consumedOutRef: outRef("1", 0),
          producedOutRef: outRef("c", 1),
        },
      ],
    });
    expect(parseStateQueueCorrectionTransitionV1(transition)).toEqual(
      transition,
    );
  });

  it("derives terminal head removal and refuses fraud, merge, or mismatched topology", () => {
    const target = h28("1");
    const terminal = {
      ...common,
      spentInputOutRefs: [outRef("0", 0), outRef("1", 0)],
      previousQueue: [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: target, outRef: outRef("1", 0) },
      ],
      nextQueue: [{ headerHash: null, outRef: outRef("c", 0) }],
      redeemers: timeoutRedeemer({
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: target,
          removal_approach: {
            RemoveTimedOutHead: {
              confirmed_state_input_outref: {
                transactionId: h32("0"),
                outputIndex: 0n,
              },
              confirmed_state_output_index: 0n,
            },
          },
        },
      }),
    } satisfies DeriveStateQueueCorrectionTransitionV1Input;
    expect(deriveStateQueueCorrectionTransitionV1(terminal)).toMatchObject({
      removalApproach: "RemoveTimedOutHead",
      removedHeaderHashes: [target],
    });
    expect(
      deriveStateQueueCorrectionTransitionV1({
        ...terminal,
        nextQueue: [
          { headerHash: null, outRef: outRef("c", 0) },
          { headerHash: target, outRef: outRef("c", 1) },
        ],
      }),
    ).toBeNull();
    expect(
      deriveStateQueueCorrectionTransitionV1({
        ...terminal,
        redeemers: timeoutRedeemer({
          MergeToConfirmedStateV1: {
            header_node_key: target,
            confirmed_state_input_outref: {
              transactionId: h32("0"),
              outputIndex: 0n,
            },
            confirmed_state_output_index: 0n,
            m_settlement_redeemer_index: null,
            merged_block_withdrawals_root: h32("0"),
            merged_block_forced_transactions_root: h32("0"),
            merged_block_transactions_root: h32("0"),
            merged_block_deposits_root: h32("0"),
            merged_block_transition_trace_root: h32("0"),
            merged_block_event_to_step_root: h32("0"),
            merged_block_validation_traces_root: h32("0"),
            merged_block_withdrawal_count: 0n,
            merged_block_forced_transaction_count: 0n,
            merged_block_l2_transaction_count: 0n,
            merged_block_deposit_count: 0n,
            merged_block_total_event_count: 0n,
            merged_block_transition_step_count: 0n,
            merged_block_validation_trace_count: 0n,
          },
        }),
      }),
    ).toBeNull();
  });

  it("rejects tampered or structurally extended durable records", () => {
    const target = h28("1");
    const transition = deriveStateQueueCorrectionTransitionV1({
      ...common,
      spentInputOutRefs: [outRef("0", 0), outRef("1", 0)],
      previousQueue: [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: target, outRef: outRef("1", 0) },
      ],
      nextQueue: [{ headerHash: null, outRef: outRef("c", 0) }],
      redeemers: timeoutRedeemer({
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: target,
          removal_approach: {
            RemoveTimedOutHead: {
              confirmed_state_input_outref: {
                transactionId: h32("0"),
                outputIndex: 0n,
              },
              confirmed_state_output_index: 0n,
            },
          },
        },
      }),
    });
    expect(transition).not.toBeNull();
    expect(
      parseStateQueueCorrectionTransitionV1({
        ...transition!,
        removedHeaderHashes: [h28("9")],
      }),
    ).toBeNull();
    expect(
      parseStateQueueCorrectionTransitionV1({
        ...transition!,
        completedAuthority: true,
      }),
    ).toBeNull();
    const advanced = withStateQueueCorrectionTransitionFinalityDepthV1(
      transition,
      "2161",
    );
    expect(advanced?.finalityDepth).toBe("2161");
    expect(advanced?.transitionDigest).not.toBe(transition?.transitionDigest);
    expect(
      withStateQueueCorrectionTransitionFinalityDepthV1(advanced, "2160"),
    ).toBeNull();
    expect(
      withStateQueueCorrectionTransitionFinalityDepthV1(
        { ...advanced, authority: true },
        "2162",
      ),
    ).toBeNull();
  });

  it("strictly parses shared removal provenance and rejects forged-but-rehashed semantics", () => {
    const target = h28("1");
    const redeemers = timeoutRedeemer({
      RemoveUnattestedBlockAfterTimeout: {
        timed_out_header_hash: target,
        removal_approach: {
          RemoveTimedOutHead: {
            confirmed_state_input_outref: {
              transactionId: h32("0"),
              outputIndex: 0n,
            },
            confirmed_state_output_index: 0n,
          },
        },
      },
    });
    const observation = deriveStateQueueAuthenticatedTransitionV1({
      ...common,
      ...timeoutLock(target, true),
      transactionIndex: "2",
      spentInputOutRefs: [outRef("0", 0), outRef("1", 0), outRef("f", 0)],
      previousQueue: [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: target, outRef: outRef("1", 0) },
      ],
      nextQueue: [{ headerHash: null, outRef: outRef("c", 0) }],
      redeemers,
    });
    expect(parseStateQueueAuthenticatedTransitionV1(observation)).toEqual(
      observation,
    );
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({ ...observation!, transitionKind: "fraud_removal" as const }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...observation!,
          finalityDepth: "2161",
        }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...observation!,
          correctionLockWitness: {
            ...observation!.correctionLockWitness,
            targetHeaderHash: h28("9"),
          },
        }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...observation!,
          correctionLockWitness: {
            ...observation!.correctionLockWitness,
            correctionIdentity: {
              FraudProof: { fraud_proof_asset_name: `00000001${target}` },
            },
          },
        }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...observation!,
          consumedQueueOutRefs: [
            ...observation!.consumedQueueOutRefs,
          ].reverse(),
        }),
      ),
    ).toBeNull();
    const advanced = withStateQueueAuthenticatedTransitionFinalityDepthV1(
      observation,
      "2161",
    );
    expect(advanced?.finalityDepth).toBe("2161");
    expect(advanced?.correctionTransition?.finalityDepth).toBe("2161");
    expect(parseStateQueueAuthenticatedTransitionV1(advanced)).toEqual(
      advanced,
    );
    expect(
      withStateQueueAuthenticatedTransitionFinalityDepthV1(advanced, "2160"),
    ).toBeNull();

    const descendant = h28("2");
    const threeNodes = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: target, outRef: outRef("1", 0) },
      { headerHash: descendant, outRef: outRef("2", 0) },
    ] as const;
    const mergePrevious = [
      ...threeNodes,
      { headerHash: h28("3"), outRef: outRef("3", 0) },
    ] as const;
    const mergeObservation = deriveStateQueueAuthenticatedTransitionV1({
      ...common,
      ...idleLockReference,
      transactionIndex: "3",
      spentInputOutRefs: [outRef("0", 0), outRef("1", 0)],
      previousQueue: mergePrevious,
      nextQueue: [
        { headerHash: null, outRef: outRef("c", 0) },
        mergePrevious[2],
        mergePrevious[3],
      ],
      redeemers: timeoutRedeemer({
        MergeToConfirmedStateV1: {
          header_node_key: target,
          confirmed_state_input_outref: {
            transactionId: h32("0"),
            outputIndex: 0n,
          },
          confirmed_state_output_index: 0n,
          m_settlement_redeemer_index: null,
          merged_block_withdrawals_root: h32("0"),
          merged_block_forced_transactions_root: h32("0"),
          merged_block_transactions_root: h32("0"),
          merged_block_deposits_root: h32("0"),
          merged_block_transition_trace_root: h32("0"),
          merged_block_event_to_step_root: h32("0"),
          merged_block_validation_traces_root: h32("0"),
          merged_block_withdrawal_count: 0n,
          merged_block_forced_transaction_count: 0n,
          merged_block_l2_transaction_count: 0n,
          merged_block_deposit_count: 0n,
          merged_block_total_event_count: 0n,
          merged_block_transition_step_count: 0n,
          merged_block_validation_trace_count: 0n,
        },
      }),
    });
    expect(mergeObservation?.transitionKind).toBe("merge");
    expect(parseStateQueueAuthenticatedTransitionV1(mergeObservation)).toEqual(
      mergeObservation,
    );
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...mergeObservation!,
          correctionLockWitness: {
            ...mergeObservation!.correctionLockWitness,
            datum: {
              Locked: {
                target_header_hash: target,
                correction_identity: "AttestationTimeout",
              },
            },
          },
        }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...mergeObservation!,
          nextQueue: [
            mergeObservation!.nextQueue[0]!,
            mergeObservation!.nextQueue[2]!,
            mergeObservation!.nextQueue[1]!,
          ],
        }),
      ),
    ).toBeNull();
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...mergeObservation!,
          previousQueue: [
            { ...mergeObservation!.previousQueue[0]!, injected: true },
            ...mergeObservation!.previousQueue.slice(1),
          ],
        }),
      ),
    ).toBeNull();

    const fraudObservation = deriveStateQueueAuthenticatedTransitionV1({
      ...common,
      ...fraudLock(descendant, true),
      transactionIndex: "4",
      spentInputOutRefs: [outRef("1", 0), outRef("2", 0), outRef("f", 0)],
      previousQueue: threeNodes,
      nextQueue: [
        threeNodes[0],
        { headerHash: target, outRef: outRef("c", 0) },
      ],
      redeemers: timeoutRedeemer({
        RemoveFraudulentBlockHeader: {
          fraudulent_operator: h28("f"),
          fraudulent_blocks_header_hash: descendant,
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
      }),
    });
    expect(fraudObservation?.transitionKind).toBe("fraud_removal");
    expect(parseStateQueueAuthenticatedTransitionV1(fraudObservation)).toEqual(
      fraudObservation,
    );
    expect(
      parseStateQueueAuthenticatedTransitionV1(
        rehash({
          ...fraudObservation!,
          correctionLockWitness: {
            ...fraudObservation!.correctionLockWitness,
            correctionIdentity: {
              FraudProof: { fraud_proof_asset_name: `00000001${target}` },
            },
          },
        }),
      ),
    ).toBeNull();
  });
});
