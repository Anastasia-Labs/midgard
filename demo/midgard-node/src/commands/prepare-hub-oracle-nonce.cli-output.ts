import { formatL1Origin } from "@al-ft/midgard-core/l1-origin";

import { formatJson } from "./command-utils.js";
import type { PreparedHubOracleNonce } from "./prepare-hub-oracle-nonce.js";

/**
 * What `prepare-hub-oracle-one-shot-nonce` prints for a nonce it prepared or
 * reconciled: the JSON result, or `KEY=value` lines. `L1_ORIGIN` is the
 * point recorded before the nonce was first signed, when one was.
 */
export const hubOracleNonceCliOutput = (
  result: PreparedHubOracleNonce,
  { json, verb }: Readonly<{ json: boolean; verb: "prepared" | "reconciled" }>,
): string =>
  json
    ? `${formatJson(result)}\n`
    : [
        `${verb} hub-oracle one-shot nonce: ${result.outRef}`,
        `HUB_ORACLE_ONE_SHOT_TX_HASH=${result.txHash}`,
        `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX=${result.outputIndex.toString()}`,
        ...(result.l1Origin === undefined
          ? []
          : [`L1_ORIGIN=${formatL1Origin(result.l1Origin)}`]),
        `confirmationStatus=${result.confirmationStatus}`,
        `address=${result.address}`,
        `lovelace=${result.lovelace}`,
      ].join("\n") + "\n";
