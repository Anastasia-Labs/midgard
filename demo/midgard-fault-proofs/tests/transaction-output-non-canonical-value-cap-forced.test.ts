/**
 * transactionOutputNonCanonical over a forced transaction whose output Value
 * is 5,001 bytes, one over the Cardano cap. The tx-order mint refuses such an
 * order, so an honest chain never carries one; a node that is handed one
 * rejects it with `OutputNonCanonical`, the arm the scan proves. That
 * rejection cannot be convicted: step 04 refuses to contradict it. A forced
 * leaf that accepts the transaction instead is convicted and its block
 * removed.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  LONG_ASSET_NAME_LOVELACE,
  longAssetNameOutput,
} from "./support/long-asset-name-value.js";
import {
  forcedOutputTransaction,
  runForcedLeaf,
} from "./transaction-output-non-canonical-duplicate-asset.scenario.js";

describe("transactionOutputNonCanonical over a forced 5,001-byte output Value", () => {
  it("refuses to convict the node's rejection at step 04, and convicts and removes a leaf accepting it", async () => {
    const forced = forcedOutputTransaction(
      encodeMidgardTxOutput(longAssetNameOutput(256n)),
      LONG_ASSET_NAME_LOVELACE,
    );
    expect(forced.admission?.check).toBe("outputs_are_admissible(outputs)");
    expect(await forced.verdict()).toStrictEqual({
      ForcedTxInvalid: {
        reason: { OutputNonCanonical: { output_index: 0n } },
      },
    });
    await runForcedLeaf({ transaction: forced.transaction, accepted: false });
    await runForcedLeaf({ transaction: forced.transaction, accepted: true });
  }, 900_000);
});
