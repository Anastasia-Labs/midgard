import {
  deriveStateQueueAuthenticatedTransition,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
const outRef = (byte: number, index: number): string =>
  `${h32(byte)}#${index.toString()}`;
const correctionLockOutRef = outRef(0x99, 9);

export const externalTimeoutTransition = ({
  terminal,
}: {
  readonly terminal: boolean;
}) => {
  const target = h28(0x11);
  const transactionHash = terminal ? h32(0xdd) : h32(0xcc);
  const redeemer: StateQueueRedeemerType = terminal
    ? {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            RemoveLastUnattestedBlock: {
              predecessor_input_outref: {
                transactionId: h32(0x00),
                outputIndex: 0n,
              },
              predecessor_output_index: 0n,
            },
          },
        },
      }
    : {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            PruneUnattestedBlockDescendant: {
              predecessor_ref_input_index: 0n,
              timed_out_node_input_outref: {
                transactionId: h32(0x11),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 0n,
            },
          },
        },
      };
  return deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: h32(0xaa),
    stateQueuePolicyId: h28(0xbb),
    transactionHash,
    blockHash: terminal ? h32(0x88) : h32(0x77),
    slot: terminal ? "101" : "100",
    blockNo: terminal ? "91" : "90",
    transactionIndex: "0",
    chainPointId: terminal ? h32(0x66) : h32(0x55),
    finalityDepth: "2160",
    mintPolicyIds: [h28(0xbb)],
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
      ? [outRef(0x00, 0), outRef(0x11, 0), correctionLockOutRef]
      : [outRef(0x11, 0), outRef(0x22, 0), correctionLockOutRef],
    previousQueue: terminal
      ? [
          { headerHash: null, outRef: outRef(0x00, 0) },
          { headerHash: target, outRef: outRef(0x11, 0) },
        ]
      : [
          { headerHash: null, outRef: outRef(0x00, 0) },
          { headerHash: target, outRef: outRef(0x11, 0) },
          { headerHash: h28(0x22), outRef: outRef(0x22, 0) },
        ],
    nextQueue: terminal
      ? [{ headerHash: null, outRef: `${transactionHash}#0` }]
      : [
          { headerHash: null, outRef: outRef(0x00, 0) },
          { headerHash: target, outRef: `${transactionHash}#0` },
        ],
  })!;
};
