import { FABRICATED_DEPOSIT_FAMILY_DEFINITION } from "./fabricated-deposit.js";
import { FABRICATED_WITHDRAWAL_FAMILY_DEFINITION } from "./fabricated-withdrawal.js";
import type { LinearFamilyDefinitionOf } from "./family-definition.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import { NO_REFERENCE_INPUT_FAMILY_DEFINITION } from "./no-reference-input.js";

/**
 * Linear families whose workflow module has not yet been reduced to a
 * transaction port plus a family definition. Each still assembles its own
 * manifest-bound workflow by hand; the migration tickets shrink this list
 * until the definitions table covers every linear category and the
 * allow-list is deleted.
 */
export const UNMIGRATED_LINEAR_FAMILY_CATEGORIES = Object.freeze([
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "zeroInput",
  "daHashPreimage",
  "referenceInputNoIdx",
  "invalidSignature",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "inputSetUniqueness",
] as const satisfies readonly LinearFamilyCategory[]);

export type UnmigratedLinearFamilyCategory =
  (typeof UNMIGRATED_LINEAR_FAMILY_CATEGORIES)[number];

/**
 * The manifest-bound family definitions, keyed by category. The `satisfies`
 * guard requires exactly one definition per linear category outside the
 * allow-list above, with the definition's own category as its key.
 */
export const LINEAR_FAMILY_DEFINITIONS = Object.freeze({
  noReferenceInput: NO_REFERENCE_INPUT_FAMILY_DEFINITION,
  fabricatedDeposit: FABRICATED_DEPOSIT_FAMILY_DEFINITION,
  fabricatedWithdrawal: FABRICATED_WITHDRAWAL_FAMILY_DEFINITION,
} satisfies {
  readonly [Category in Exclude<
    LinearFamilyCategory,
    UnmigratedLinearFamilyCategory
  >]: LinearFamilyDefinitionOf<Category>;
});

export type MigratedLinearFamilyCategory =
  keyof typeof LINEAR_FAMILY_DEFINITIONS;
