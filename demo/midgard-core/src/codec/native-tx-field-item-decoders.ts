/**
 * The decode twins of `native-tx-field-items.ts` — one reader per §5.3
 * `enc_i` form, plus the per-field dispatch that turns an authenticated §5.1
 * preimage back into typed items.
 *
 * `native-tx-field-items.ts` deliberately holds no decoders: it is a
 * producer, and §5.3 names the Aiken *reader* functions
 * (`decode_midgard_tx_input_cbor`, `midgard_redeemer_purpose_from_tag`, …) as
 * the places that reject an out-of-set value. This module is where those
 * readers' twins live, so the producer module keeps one direction and this one
 * keeps the other, and neither has to spell the value sets twice.
 *
 * Two of the nine item decoders already exist as canonical decoders and are
 * **reused** rather than re-spelled, mirroring the producer module's own reuse:
 *
 *   * field 2 — {@link decodeMidgardTxOutput} (§5.5);
 *   * field 6 — {@link decodeMidgardVersionedScript} (§5.3's tag table).
 *
 * Everything here reads a *single item's* `enc_i` bytes. Splitting a preimage
 * into items is not this module's job — that is §5.1's one uniform byte-list
 * decode, `decodeMidgardFieldPreimage`, which all nine fields share. The
 * per-field entry point {@link decodeMidgardFieldItems} composes the two.
 */

import "./cbor.js";
import "./errors.js";
import "./native-tx-field-access.js";
import "./native-tx-field-items.js";
import "./output.js";
import "./versioned-script.js";
import "./native-tx-field-item-decoders.decode-midgard-mint-policy-item.js";
import "./native-tx-field-item-decoders.decode-midgard-field-items.js";
export {
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardFieldItemBytes,
  decodeMidgardFieldItems,
  decodeMidgardHash28FieldPreimage,
  decodeMidgardInputFieldPreimage,
  decodeMidgardMintFieldPreimage,
  decodeMidgardOutputFieldPreimage,
  decodeMidgardRedeemerWitnessFieldPreimage,
  decodeMidgardRedeemerWitnessItem,
  decodeMidgardScriptWitnessFieldPreimage,
  type MidgardDecodedFieldItems,
} from "./native-tx-field-item-decoders.decode-midgard-field-items.js";
export {
  decodeMidgardAddressWitnessItem,
  decodeMidgardHash28Item,
  decodeMidgardMintPolicyItem,
  decodeMidgardSpendInputItem,
  midgardRedeemerPurposeFromTag,
} from "./native-tx-field-item-decoders.decode-midgard-mint-policy-item.js";
