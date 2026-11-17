/**
 * The L1 origin the journey deployment records in its manifest: the point
 * immediately before the block holding the hub-oracle nonce tx, found on the
 * devnet node's own chain by following it from genesis (`findOrigin`). The
 * journey session refuses a manifest without one and proves the recorded
 * point on the same chain (`deriveL1Origin`), so the deployment records it
 * when it finalizes the manifest instead of a run directory being patched.
 */
import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";
import { findOrigin } from "@al-ft/midgard-l1-follower";

import {
  type L1NodeConnection,
  L1OriginUndeterminedError,
} from "../../src/l1-origin.js";

export const deploymentL1Origin = async (input: {
  readonly node: L1NodeConnection;
  readonly nonceTxHash: string;
}): Promise<L1Origin> => {
  const txHash = input.nonceTxHash.toLowerCase();
  const transport = new L1NodeTransport(input.node);
  let result;
  try {
    result = await findOrigin({ transport, txHash });
  } finally {
    await transport.close();
  }
  switch (result.kind) {
    case "found":
      return result.origin;
    case "not_found":
      throw new L1OriginUndeterminedError(
        `the node's chain from genesis to ${
          result.scannedTo === "genesis"
            ? "genesis"
            : formatL1Origin(result.scannedTo)
        } holds no hub-oracle nonce tx ${txHash}, so the deployment has no L1 origin to record`,
      );
    case "from_not_on_chain":
      throw new L1OriginUndeterminedError(
        `the node refused a chain-sync from genesis while finding the hub-oracle nonce tx ${txHash}`,
      );
    case "no_preceding_point":
      throw new L1OriginUndeterminedError(
        `the hub-oracle nonce tx ${txHash} is in the chain's first block, which no recorded origin can precede`,
      );
  }
};
