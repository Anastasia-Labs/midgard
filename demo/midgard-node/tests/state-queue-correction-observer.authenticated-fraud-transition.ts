import { createHash } from "node:crypto";

import {
  deriveStateQueueAuthenticatedReplayCheckpoint,
  deriveStateQueueAuthenticatedTransition,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  makeLocalKupmiosStateQueueCorrectionSource,
  type StateQueueCorrectionObserverSource,
} from "../src/services/state-queue-correction-observer.js";

export const h28 = (byte: string): string => byte.repeat(56);

export const h32 = (byte: string): string => byte.repeat(64);

export const outRef = (byte: string, index = 0): string =>
  `${h32(byte)}#${index.toString()}`;

export const deployment = h32("a");

export const policy = h28("b");

export const target = h28("1");

export const descendant = h28("2");

export const transactionHash = h32("c");

const correctionLockOutRef = outRef("f");

export const hubPolicy = h28("a");

export const fraudPolicy = h28("e");

export const correctionLockAddress = "addr_test_correction_lock";

export const fraudProofAddress = "addr_test_fraud_proof";

const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};

export const sha256 = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

/** Ogmios v6: queryNetwork/tip carries no height; blockHeight carries it. */
export const ogmiosTipResponse = (
  init: RequestInit | undefined,
  point: { readonly id: string; readonly slot: number },
  height: number,
): Response => {
  const { method } = JSON.parse(String(init?.body)) as { method: string };
  if (method === "queryNetwork/tip")
    return new Response(JSON.stringify({ result: point }));
  expect(method).toBe("queryNetwork/blockHeight");
  return new Response(JSON.stringify({ result: height }));
};

export const tipOnlySource = (
  answer: (method: string, call: number) => unknown,
): {
  readonly source: StateQueueCorrectionObserverSource;
  readonly methods: string[];
} => {
  const methods: string[] = [];
  const source = makeLocalKupmiosStateQueueCorrectionSource({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    stateQueueAddress: "addr_test_state_queue",
    hubOraclePolicyId: hubPolicy,
    correctionLockAddress,
    fraudProofPolicyId: fraudPolicy,
    fraudProofAddress,
    kupoUrl: "http://kupo.test",
    ogmiosUrl: "ws://ogmios.test",
    readQueue: async () => before,
    fetchImpl: async (_url: string, init?: RequestInit) => {
      const { method } = JSON.parse(String(init?.body)) as { method: string };
      methods.push(method);
      return new Response(
        JSON.stringify({ result: answer(method, methods.length) }),
      );
    },
  });
  return { source, methods };
};

export const before: readonly StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: outRef("1") },
  { headerHash: descendant, outRef: outRef("2") },
];

export const after: readonly StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: `${transactionHash}#0` },
];

export const authenticatedTransition = (txHash = transactionHash) =>
  deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: txHash,
    blockHash: h32("7"),
    slot: "100",
    blockNo: "90",
    transactionIndex: "0",
    chainPointId: h32("5"),
    finalityDepth: "1",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${txHash}#9`,
      targetHeaderHash: target,
      correctionIdentity: "AttestationTimeout",
      previousDatum: "Idle",
      nextDatum: {
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
        cborHex: Data.to(
          {
            RemoveUnattestedBlockAfterTimeout: {
              yield_to_ref_input_index: 0n,
              timed_out_header_hash: target,
              removal_approach: {
                PruneUnattestedBlockDescendant: {
                  predecessor_ref_input_index: 0n,
                  timed_out_node_input_outref: {
                    transactionId: h32("1"),
                    outputIndex: 0n,
                  },
                  timed_out_node_output_index: 0n,
                },
              },
            },
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [outRef("1"), outRef("2"), correctionLockOutRef],
    previousQueue: before,
    nextQueue: [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: target, outRef: `${txHash}#0` },
    ],
  })!;

export const authenticatedFraudTransition = () => {
  const txHash = h32("d");
  return deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: txHash,
    blockHash: h32("8"),
    slot: "101",
    blockNo: "91",
    transactionIndex: "0",
    chainPointId: h32("6"),
    finalityDepth: "1",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${txHash}#9`,
      targetHeaderHash: descendant,
      correctionIdentity: {
        FraudProof: { fraud_proof_asset_name: `00000001${descendant}` },
      },
      previousDatum: "Idle",
      nextDatum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            RemoveFraudulentBlockHeader: {
              yield_to_ref_input_index: 0n,
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
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [outRef("1"), outRef("2"), correctionLockOutRef],
    previousQueue: before,
    nextQueue: [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: target, outRef: `${txHash}#0` },
    ],
  })!;
};

export const checkpointFromTerminal = (
  transition = authenticatedTransition(),
) =>
  deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: transition.deploymentIdentityDigest,
    stateQueuePolicyId: transition.stateQueuePolicyId,
    transactionHash: transition.transactionHash,
    blockHash: transition.blockHash,
    slot: transition.slot,
    blockNo: transition.blockNo,
    transactionIndex: transition.transactionIndex,
    chainPointId: transition.chainPointId,
    finalityDepth: transition.finalityDepth,
    mintPolicyIds: [transition.stateQueuePolicyId],
    redeemers: [transition.stateQueueMintRedeemer],
    spentInputOutRefs:
      transition.correctionLockWitness.kind === "correction_transition"
        ? [
            ...transition.consumedQueueOutRefs,
            transition.correctionLockWitness.consumedOutRef,
          ]
        : transition.consumedQueueOutRefs,
    referenceInputOutRefs: [],
    correctionLockWitness: transition.correctionLockWitness,
    previousQueue: transition.previousQueue,
    nextQueue: transition.nextQueue,
  })!;

export const appendCheckpoint = ({
  transactionByte,
  headerHash,
  previousQueue,
  blockNo,
}: {
  transactionByte: string;
  headerHash: string;
  previousQueue: readonly StateQueueTransitionNode[];
  blockNo: number;
}) => {
  const transaction = h32(transactionByte);
  const priorTail = previousQueue.at(-1)!;
  const nextQueue = [
    ...previousQueue.slice(0, -1),
    { headerHash: priorTail.headerHash, outRef: `${transaction}#0` },
    { headerHash, outRef: `${transaction}#1` },
  ];
  return deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: transaction,
    blockHash: h32(transactionByte),
    slot: (blockNo + 10).toString(),
    blockNo: blockNo.toString(),
    transactionIndex: "0",
    chainPointId: h32(transactionByte),
    finalityDepth: "30",
    mintPolicyIds: [policy],
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
        cborHex: Data.to(
          {
            CommitBlockHeader: {
              yield_to_ref_input_index: 0n,
              new_block_output_index: 1n,
              continued_latest_block_output_index: 0n,
              operator: h28("9"),
              scheduler_ref_input_index: 0n,
              active_operators_input_index: 0n,
              active_operators_redeemer_index: 0n,
              m_confirmed_state_ref_input_index: null,
              m_head_state_queue_node_ref_input_index: null,
            },
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [priorTail.outRef],
    previousQueue,
    nextQueue,
  })!;
};
