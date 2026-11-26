import { midgardFieldCommitmentFromItems } from "@al-ft/midgard-core";

import { encodeMidgardTxOutputCanonical } from "./input-no-idx.encode-midgard-value-canonical.js";
import { MidgardTxOutput } from "./input-no-idx.input-no-idx-evidence-from-committed-transactions.js";

/**
 * The `outputs_hash` a native transaction body commits for `outputs`: §4's flat
 * `blake2b_256` over the §5.1 preimage the items assemble into.
 */
export const inputNoIdxOutputsCommitment = (
  outputs: readonly MidgardTxOutput[],
): string =>
  midgardFieldCommitmentFromItems(
    outputs.map(encodeMidgardTxOutputCanonical),
  ).toString("hex");
