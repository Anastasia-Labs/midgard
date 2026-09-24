import {
  deriveStateQueueAuthenticatedTransition,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index: number): string =>
  `${h32(byte)}#${index.toString()}`;
const correctionLockOutRef = outRef("9", 9);

export const externalTimeoutTransition = ({
  terminal,
}: {
  readonly terminal: boolean;
}) => {
  const target = h28("1");
  const transactionHash = terminal ? h32("d") : h32("c");
  const redeemer: StateQueueRedeemerType = terminal
    ? {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            RemoveLastUnattestedBlock: {
              predecessor_input_outref: {
                transactionId: h32("0"),
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
                transactionId: h32("1"),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 0n,
            },
          },
        },
      };
  return deriveStateQueueAuthenticatedTransition({
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
