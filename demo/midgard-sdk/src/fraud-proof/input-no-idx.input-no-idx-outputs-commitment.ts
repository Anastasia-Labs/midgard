import { midgardFieldCommitmentFromItems } from "@al-ft/midgard-core";

import { encodeMidgardTxOutputCanonical } from "./input-no-idx.encode-midgard-value-canonical.js";
import { MidgardTxOutput } from "./input-no-idx.input-no-idx-evidence-from-committed-transactions.js";

// ## The retired counted fold family
//
// `InputNoIdxSpendInputFoldOpeningV1`, `buildInputNoIdxSpendInputFoldOpeningsV1`
// and `verifyInputNoIdxSpendInputFoldOpeningV1` lived here and are **deleted**,
// not re-pointed. They published per-item openings against the counted
// bounded-collection Merkle root, and §4 gives a field one flat hash with no
// per-item openings at all — so the flat rebind did not move them to a new
// commitment, it deleted the concept they published. Their replacement is the
// §8.8 door (`FieldOpeningV1` + one of §8's three carriage tiers), which
// step-02's `Args` now names directly.
//
// The comment that stood here recorded that the swap could not be made in that
// lane because it would move the `fraud_proofs/input_no_idx/step_02` redeemer
// shape. #575 has since moved exactly that shape on-chain, and #604 is the
// off-chain half following it.

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
