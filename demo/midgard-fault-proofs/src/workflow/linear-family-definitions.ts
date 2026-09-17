import { CANONICAL_DECODABILITY_FAMILY_DEFINITION } from "./canonical-decodability.js";
import { COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION } from "./committed-field-shape.js";
import { DA_HASH_PREIMAGE_FAMILY_DEFINITION } from "./da-hash-preimage.js";
import { DOUBLE_WITHDRAW_FAMILY_DEFINITION } from "./double-withdraw.js";
import { FABRICATED_DEPOSIT_FAMILY_DEFINITION } from "./fabricated-deposit.js";
import { FABRICATED_WITHDRAWAL_FAMILY_DEFINITION } from "./fabricated-withdrawal.js";
import type { LinearFamilyDefinitionOf } from "./family-definition.js";
import { INPUT_NO_IDX_FAMILY_DEFINITION } from "./input-no-idx.js";
import { INPUT_SET_UNIQUENESS_FAMILY_DEFINITION } from "./input-set-uniqueness.js";
import { INVALID_SIGNATURE_FAMILY_DEFINITION } from "./invalid-signature.js";
import { L2_TX_MISTAG_FAMILY_DEFINITION } from "./l2-tx-mistag.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import { MIN_FEE_FAMILY_DEFINITION } from "./min-fee.js";
import {
  INVALID_RANGE_FAMILY_DEFINITION,
  ZERO_INPUT_FAMILY_DEFINITION,
} from "./native-inclusion-two-step.js";
import { NO_REFERENCE_INPUT_FAMILY_DEFINITION } from "./no-reference-input.js";
import { NON_EXISTENT_INPUT_FAMILY_DEFINITION } from "./non-existent-input.js";
import { REFERENCE_INPUT_NO_IDX_FAMILY_DEFINITION } from "./reference-input-no-idx.js";
import { WITHDRAWN_INPUT_FAMILY_DEFINITION } from "./withdrawn-input.js";
import { WITHDRAWN_REFERENCE_INPUT_FAMILY_DEFINITION } from "./withdrawn-reference-input.js";

/**
 * Linear-spec categories whose live workflow is not a linear-arm assembly.
 * `crossBlockDuplicateEvent` has a two-step row in the linear spec, but its
 * workflow (`src/cross-block-duplicate-event/workflow.ts`) runs on the cursor
 * adapter over `CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC`; whether it moves onto
 * the cursor arm or leaves the linear spec is an owner call.
 */
export const UNMIGRATED_LINEAR_FAMILY_CATEGORIES = Object.freeze([
  "crossBlockDuplicateEvent",
] as const satisfies readonly LinearFamilyCategory[]);

export type UnmigratedLinearFamilyCategory =
  (typeof UNMIGRATED_LINEAR_FAMILY_CATEGORIES)[number];

/**
 * The manifest-bound linear family definitions, keyed by category. The
 * `satisfies` guard requires exactly one definition per linear category
 * outside the list above, with the definition's own category as its key, so
 * an omitted family fails typecheck.
 */
export const LINEAR_FAMILY_DEFINITIONS = Object.freeze({
  nonExistentInput: NON_EXISTENT_INPUT_FAMILY_DEFINITION,
  nonExistentInputNoIndex: INPUT_NO_IDX_FAMILY_DEFINITION,
  invalidRange: INVALID_RANGE_FAMILY_DEFINITION,
  zeroInput: ZERO_INPUT_FAMILY_DEFINITION,
  daHashPreimage: DA_HASH_PREIMAGE_FAMILY_DEFINITION,
  noReferenceInput: NO_REFERENCE_INPUT_FAMILY_DEFINITION,
  referenceInputNoIdx: REFERENCE_INPUT_NO_IDX_FAMILY_DEFINITION,
  invalidSignature: INVALID_SIGNATURE_FAMILY_DEFINITION,
  fabricatedDeposit: FABRICATED_DEPOSIT_FAMILY_DEFINITION,
  fabricatedWithdrawal: FABRICATED_WITHDRAWAL_FAMILY_DEFINITION,
  withdrawnReferenceInput: WITHDRAWN_REFERENCE_INPUT_FAMILY_DEFINITION,
  canonicalDecodability: CANONICAL_DECODABILITY_FAMILY_DEFINITION,
  committedFieldShape: COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION,
  minFee: MIN_FEE_FAMILY_DEFINITION,
  doubleWithdraw: DOUBLE_WITHDRAW_FAMILY_DEFINITION,
  l2TxMistag: L2_TX_MISTAG_FAMILY_DEFINITION,
  withdrawnInput: WITHDRAWN_INPUT_FAMILY_DEFINITION,
  inputSetUniqueness: INPUT_SET_UNIQUENESS_FAMILY_DEFINITION,
} satisfies {
  readonly [Category in Exclude<
    LinearFamilyCategory,
    UnmigratedLinearFamilyCategory
  >]: LinearFamilyDefinitionOf<Category>;
});

export type MigratedLinearFamilyCategory =
  keyof typeof LINEAR_FAMILY_DEFINITIONS;
