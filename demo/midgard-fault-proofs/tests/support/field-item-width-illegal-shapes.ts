/**
 * Field shapes, block fixtures, and raw (guard-free) submitters for the
 * `fieldItemWidthIllegal` lifecycle suite.
 *
 * The family's rule is one comparison per field: a field-2 output item is
 * illegal strictly above `max_serialized_output_preimage_bytes` (16,384
 * payload bytes) and a field-5 mint item is illegal only when empty. The
 * shapes below pin both rules at both boundaries in both directions, and pin
 * the field-2 shapes at the §5.4 aggregate ceiling (a 32,768-byte field opened
 * through three certified chunks) so the fit ledger measures the most
 * expensive carriage the family can be asked to open.
 *
 * The raw submitters exist so a negative reaches local UPLC evaluation. The
 * production builders refuse an honest verdict and a mutated opening before
 * building; a lifecycle refusal has to come from the validator, so these
 * bypass exactly those off-chain guards and nothing else.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "effect";
import "../../src/field-item-width-illegal/schemas.js";
import "../../src/field-opening.js";
import "../../src/linear-fault-family.js";
import "../../src/linear-fault-finalize.js";
import "../../src/linear-fault-submit.js";
import "../../src/step-support.js";
import "../../src/transition-trace/phas.js";
import "../../src/transition-trace/reconstruct.js";
import "../../src/tx-layout.js";
import "./emulator/header-fixtures.js";
import "./emulator/native-tx.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./field-item-width-illegal-shapes.build-accepted-width-inclusions.js";
import "./field-item-width-illegal-shapes.build-width-forced-fixture.js";
import "./field-item-width-illegal-shapes.submit-width-step02-raw.js";
export {
  boundaryLegalOutputShape,
  buildAcceptedWidthInclusions,
  compactCborHex,
  emptyMintItemShape,
  FORCED_FILLER_OUTPUT_BYTES,
  forcedIllegalOutputShape,
  forcedMaximumLegalOutputShape,
  MAXIMUM_FIELD_BYTES,
  MAXIMUM_LEGAL_OUTPUT_BYTES,
  MAXIMUM_OUTPUT_ITEM_BYTES,
  maximumIllegalOutputShape,
  mintFieldTx,
  nonEmptyMintItemShape,
  widthNativeTx,
  type WidthShape,
  witnessSetCompactCborHex,
} from "./field-item-width-illegal-shapes.build-accepted-width-inclusions.js";
export { buildWidthForcedFixture } from "./field-item-width-illegal-shapes.build-width-forced-fixture.js";
export {
  submitWidthStep02Raw,
  submitWidthStep03Raw,
} from "./field-item-width-illegal-shapes.submit-width-step02-raw.js";
