import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  deploymentManifest,
  h32,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";

/** SDK-authenticated timeout removal, with the same queue/point layout as merge fixtures. */
export const terminalRemoval = (
  headerHash: Buffer,
  sequence: number,
  identity = deploymentManifest.manifestId,
) => {
  const transactionHash = h32(sequence.toString(16));
  const policyId = deploymentManifest.contracts.stateQueueMint.scriptHash;
  const root = `${h32("0")}#0`;
  const node = `${h32((sequence + 4).toString(16))}#0`;
  const lock = `${h32("f")}#0`;
  const transition = SDK.deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: identity,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 12).toString(16)),
    finalityDepth: deploymentManifest.l1Finality.confirmationDepth.toString(),
    mintPolicyIds: [policyId],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: lock,
      continuedOutRef: `${transactionHash}#9`,
      targetHeaderHash: headerHash.toString("hex"),
      correctionIdentity: "AttestationTimeout",
      previousDatum: "Idle",
      nextDatum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            RemoveUnattestedBlockAfterTimeout: {
              yield_to_ref_input_index: 0n,
              timed_out_header_hash: headerHash.toString("hex"),
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
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [root, node, lock],
    previousQueue: [
      { headerHash: null, outRef: root },
      { headerHash: headerHash.toString("hex"), outRef: node },
    ],
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }],
  });
  if (transition === null) throw new Error("invalid terminal removal fixture");
  return transition;
};
