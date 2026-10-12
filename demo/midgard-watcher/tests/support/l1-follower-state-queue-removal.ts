/**
 * The fraud removal of the state queue's tail header for the L1 follower's
 * fork simulator (E1 ruling tests): a pure function of the queue it
 * extends, as the other state-queue traffic is. No Plutus evaluation,
 * signature or ledger validity is claimed.
 */
import type { SimTx, SimUtxo } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import {
  datum,
  h28,
  headerOf,
  linkOf,
  nonceOf,
  type QueueState,
  redeemer,
  SIM_WATCHER_DEPLOYMENT,
} from "./l1-follower-state-queue-traffic.js";

/**
 * Removes the tail header as fraudulent (RemoveFraudulentBlockHeader,
 * RemoveLastFraudulentBlock): its predecessor takes its link, its node unit
 * burns.
 */
export const removeTailTx = (
  state: QueueState,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): SimTx => {
  const predecessor = state.ordered.at(-2) as SimUtxo;
  const tail = state.tail;
  const header = headerOf(tail, deployment) as string;
  const predecessorDatum = Data.from(
    (predecessor.output.datum as Buffer).toString("hex"),
    SDK.LinkedListDatum,
  );
  return {
    inputs: [predecessor.outRef, tail.outRef],
    referenceInputs: [state.lock.outRef],
    outputs: [
      {
        ...predecessor.output,
        datum: datum(
          Data.to(
            { ...predecessorDatum, link: linkOf(tail) },
            SDK.LinkedListDatum,
          ),
        ),
      },
    ],
    mint: new Map([
      [
        deployment.stateQueueMint,
        new Map([[`${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`, -1n]]),
      ],
    ]),
    redeemers: redeemer({
      RemoveFraudulentBlockHeader: {
        yield_to_ref_input_index: 0n,
        fraudulent_operator: h28("aa"),
        fraudulent_blocks_header_hash: header,
        slashing_approach: {
          SlashActiveOperator: {
            active_operators_redeemer_index: 0n,
            m_fraud_prover_reward_output_index: null,
          },
        },
        fraud_proof_ref_input_index: 0n,
        block_removal_approach: {
          RemoveLastFraudulentBlock: {
            anchor_element_input_outref: {
              transactionId: predecessor.outRef.txHash.toString("hex"),
              outputIndex: BigInt(predecessor.outRef.index),
            },
            anchor_element_output_index: 0n,
          },
        },
      },
    }),
    nonce: nonceOf(tail.outRef) + 3,
  };
};
