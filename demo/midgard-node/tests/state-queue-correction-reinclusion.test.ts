import {
  deriveStateQueueAuthenticatedTransitionV1,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { classifyStateQueueCorrectionEventReopen } from "@/database/utils/projected-events.js";
import { authorizeStateQueueCorrectionReinclusionV1 } from "@/services/state-queue-correction-recovery.js";

const removedHeader = Buffer.from("11".repeat(28), "hex");
const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index: number): string =>
  `${h32(byte)}#${index.toString()}`;
const correctionLockOutRef = outRef("9", 9);

const externalTimeoutTransition = ({
  terminal,
}: {
  readonly terminal: boolean;
}) => {
  const target = h28("1");
  const transactionHash = terminal ? h32("d") : h32("c");
  const redeemer: StateQueueRedeemerType = terminal
    ? {
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
      }
    : {
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: target,
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
      };
  return deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: h32("a"),
    stateQueuePolicyId: h28("b"),
    transactionHash,
    blockHash: terminal ? h32("8") : h32("7"),
    slot: terminal ? "101" : "100",
    blockNo: terminal ? "91" : "90",
    transactionIndex: "0",
    chainPointId: terminal ? h32("6") : h32("5"),
    finalityDepth: "2160",
    mintPolicyIds: [h28("b")],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${transactionHash}#9`,
      targetHeaderHash: target,
      correctionIdentity: "AttestationTimeout",
      previousDatum: "Idle",
      nextDatum: terminal
        ? "Idle"
        : {
            Locked: {
              target_header_hash: target,
              correction_identity: "AttestationTimeout",
            },
          },
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: terminal
      ? [outRef("0", 0), outRef("1", 0), correctionLockOutRef]
      : [outRef("1", 0), outRef("2", 0), correctionLockOutRef],
    previousQueue: terminal
      ? [
          { headerHash: null, outRef: outRef("0", 0) },
          { headerHash: target, outRef: outRef("1", 0) },
        ]
      : [
          { headerHash: null, outRef: outRef("0", 0) },
          { headerHash: target, outRef: outRef("1", 0) },
          { headerHash: h28("2"), outRef: outRef("2", 0) },
        ],
    nextQueue: terminal
      ? [{ headerHash: null, outRef: `${transactionHash}#0` }]
      : [
          { headerHash: null, outRef: outRef("0", 0) },
          { headerHash: target, outRef: `${transactionHash}#0` },
        ],
  })!;
};

const externalFraudTransition = () => {
  const fraudulent = h28("2");
  const anchor = h28("1");
  const transactionHash = h32("e");
  const redeemer: StateQueueRedeemerType = {
    RemoveFraudulentBlockHeader: {
      fraudulent_operator: h28("f"),
      fraudulent_blocks_header_hash: fraudulent,
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
  };
  return deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: h32("a"),
    stateQueuePolicyId: h28("b"),
    transactionHash,
    blockHash: h32("8"),
    slot: "101",
    blockNo: "91",
    transactionIndex: "0",
    chainPointId: h32("6"),
    finalityDepth: "2160",
    mintPolicyIds: [h28("b")],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${transactionHash}#9`,
      targetHeaderHash: fraudulent,
      correctionIdentity: {
        FraudProof: { fraud_proof_asset_name: `00000001${fraudulent}` },
      },
      previousDatum: "Idle",
      nextDatum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: [outRef("1", 0), outRef("2", 0), correctionLockOutRef],
    previousQueue: [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: anchor, outRef: outRef("1", 0) },
      { headerHash: fraudulent, outRef: outRef("2", 0) },
    ],
    nextQueue: [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: anchor, outRef: `${transactionHash}#0` },
    ],
  })!;
};

const externalMergeTransition = () => {
  const merged = h28("1");
  const transactionHash = h32("f");
  const redeemer: StateQueueRedeemerType = {
    MergeToConfirmedStateV1: {
      header_node_key: merged,
      confirmed_state_input_outref: {
        transactionId: h32("0"),
        outputIndex: 0n,
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
  };
  return deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: h32("a"),
    stateQueuePolicyId: h28("b"),
    transactionHash,
    blockHash: h32("9"),
    slot: "102",
    blockNo: "92",
    transactionIndex: "0",
    chainPointId: h32("7"),
    finalityDepth: "2160",
    mintPolicyIds: [h28("b")],
    referenceInputOutRefs: [correctionLockOutRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: correctionLockOutRef,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: [outRef("0", 0), outRef("1", 0)],
    previousQueue: [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: merged, outRef: outRef("1", 0) },
    ],
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }],
  })!;
};

describe("state-queue correction event reinclusion", () => {
  it("accepts external-winner records only after deployment-bound release finality", () => {
    const externalWinner = externalTimeoutTransition({ terminal: true });
    expect(
      authorizeStateQueueCorrectionReinclusionV1(externalWinner, {
        expectedDeploymentIdentityDigest: h32("a"),
        requiredFinalityDepth: 2_160n,
      }),
    ).toEqual(externalWinner);
    expect(() =>
      authorizeStateQueueCorrectionReinclusionV1(externalWinner, {
        expectedDeploymentIdentityDigest: h32("9"),
        requiredFinalityDepth: 2_160n,
      }),
    ).toThrow(/does not match configured deployment/);
    expect(() =>
      authorizeStateQueueCorrectionReinclusionV1(externalWinner, {
        expectedDeploymentIdentityDigest: h32("a"),
        requiredFinalityDepth: 2_161n,
      }),
    ).toThrow(/below required release depth/);
  });

  it("authorizes every finalized external mid-prune transition independently", () => {
    const pruneWinner = externalTimeoutTransition({ terminal: false });
    const terminalWinner = externalTimeoutTransition({ terminal: true });
    const authority = {
      expectedDeploymentIdentityDigest: h32("a"),
      requiredFinalityDepth: 2_160n,
    };
    expect(
      authorizeStateQueueCorrectionReinclusionV1(pruneWinner, authority)
        .removedHeaderHashes,
    ).toEqual([h28("2")]);
    expect(
      authorizeStateQueueCorrectionReinclusionV1(terminalWinner, authority)
        .removedHeaderHashes,
    ).toEqual([h28("1")]);
  });

  it("authorizes a finalized fraud removal but rejects a normal merge", () => {
    const authority = {
      expectedDeploymentIdentityDigest: h32("a"),
      requiredFinalityDepth: 2_160n,
    };
    const fraud = externalFraudTransition();
    expect(
      authorizeStateQueueCorrectionReinclusionV1(fraud, authority),
    ).toMatchObject({
      transitionKind: "fraud_removal",
      removedHeaderHashes: [h28("2")],
    });
    expect(() =>
      authorizeStateQueueCorrectionReinclusionV1(
        externalMergeTransition(),
        authority,
      ),
    ).toThrow(/merge transition must not reinclude/u);
  });

  it("rejects a forged or duplicate authenticated removal envelope", () => {
    const authority = {
      expectedDeploymentIdentityDigest: h32("a"),
      requiredFinalityDepth: 2_160n,
    };
    const fraud = externalFraudTransition();
    expect(() =>
      authorizeStateQueueCorrectionReinclusionV1(
        { ...fraud, removedHeaderHashes: [h28("2"), h28("2")] },
        authority,
      ),
    ).toThrow(/canonical digest-bound authenticated transition/u);
    expect(() =>
      authorizeStateQueueCorrectionReinclusionV1(
        { ...fraud, transitionDigest: h32("9") },
        authority,
      ),
    ).toThrow(/canonical digest-bound authenticated transition/u);
  });

  it.each([
    ["projected", "projected"],
    ["finalized", "finalized"],
    ["consumed", "consumed"],
  ])("reopens exact-header %s events", (status, terminalStatus) => {
    expect(
      classifyStateQueueCorrectionEventReopen({
        assignedHeader: removedHeader,
        removedHeaderHash: removedHeader,
        status,
        projectedStatus: "projected",
        terminalStatus,
      }),
    ).toBe("reopen");
  });

  it("is idempotent only after the event is projected and unassigned", () => {
    expect(
      classifyStateQueueCorrectionEventReopen({
        assignedHeader: null,
        removedHeaderHash: removedHeader,
        status: "projected",
        projectedStatus: "projected",
        terminalStatus: "finalized",
      }),
    ).toBe("already-reopened");
    expect(
      classifyStateQueueCorrectionEventReopen({
        assignedHeader: null,
        removedHeaderHash: removedHeader,
        status: "finalized",
        projectedStatus: "projected",
        terminalStatus: "finalized",
      }),
    ).toBe("conflict");
  });

  it("refuses to steal an event already assigned to another header", () => {
    expect(
      classifyStateQueueCorrectionEventReopen({
        assignedHeader: Buffer.from("22".repeat(28), "hex"),
        removedHeaderHash: removedHeader,
        status: "projected",
        projectedStatus: "projected",
        terminalStatus: "finalized",
      }),
    ).toBe("conflict");
  });
});
