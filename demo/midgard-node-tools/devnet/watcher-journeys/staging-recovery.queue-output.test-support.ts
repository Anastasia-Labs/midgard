import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";

import type { JourneyBlock } from "./fixture.js";

/** State queue outputs of `policyId` at `address`, as the staging reads them. */
export const stateQueueOutputs = ({
  policyId,
  address,
  commitTxHash,
}: {
  policyId: string;
  address: string;
  /** The transaction an output carries unless one is named. */
  commitTxHash: string;
}) => {
  const unit = (headerHash: string) =>
    toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash);
  /** The queue's tail node holding `block`, with its DA attestation. */
  const queueOutput = (
    block: JourneyBlock,
    daAttestation: SDK.StateQueueNode["da_attestation"],
    txHash = commitTxHash,
  ): UTxO => ({
    txHash,
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n, [unit(block.headerHash)]: 1n },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: block.headerHash } },
      next: "Empty",
      data: Data.castTo(
        {
          proven_fraud: null,
          header: block.header,
          block_time: block.header.endTime,
          da_attestation: daAttestation,
        },
        SDK.StateQueueNode,
      ),
    }),
  });
  return { unit, queueOutput };
};
