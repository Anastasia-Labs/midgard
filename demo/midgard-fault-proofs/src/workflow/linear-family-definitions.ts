import { DA_HASH_PREIMAGE_FAMILY_DEFINITION } from "./da-hash-preimage.create-bound-da-hash-preimage-transaction-port.js";
import { DOUBLE_WITHDRAW_FAMILY_DEFINITION } from "./double-withdraw.double-withdraw-family-definition.js";
import { FABRICATED_DEPOSIT_FAMILY_DEFINITION } from "./fabricated-deposit.js";
import { FABRICATED_WITHDRAWAL_FAMILY_DEFINITION } from "./fabricated-withdrawal.js";
import type { LinearFamilyDefinitionOf } from "./family-definition.js";
import { INPUT_NO_IDX_FAMILY_DEFINITION } from "./input-no-idx.create-transaction-port.js";
import { INPUT_SET_UNIQUENESS_FAMILY_DEFINITION } from "./input-set-uniqueness.input-set-uniqueness-family-definition.js";
import { INVALID_SIGNATURE_FAMILY_DEFINITION } from "./invalid-signature.invalid-signature-family-definition.js";
import { L2_TX_MISTAG_FAMILY_DEFINITION } from "./l2-tx-mistag.create-transaction-port.js";
import type { LinearFamilyCategory } from "./linear-family-spec.js";
import { MIN_FEE_FAMILY_DEFINITION } from "./min-fee.min-fee-family-definition.js";
import {
  INVALID_RANGE_FAMILY_DEFINITION,
  ZERO_INPUT_FAMILY_DEFINITION,
} from "./native-inclusion-two-step.transaction-port.js";
import { NO_REFERENCE_INPUT_FAMILY_DEFINITION } from "./no-reference-input.no-reference-input-family-definition.js";
import { NON_EXISTENT_INPUT_FAMILY_DEFINITION } from "./non-existent-input.non-existent-input-family-definition.js";
import { REFERENCE_INPUT_NO_IDX_FAMILY_DEFINITION } from "./reference-input-no-idx.reference-input-no-idx-family-definition.js";
import { WITHDRAWN_INPUT_FAMILY_DEFINITION } from "./withdrawn-input.create-transaction-port.js";
import { WITHDRAWN_REFERENCE_INPUT_FAMILY_DEFINITION } from "./withdrawn-reference-input.withdrawn-reference-input-family-definition.js";

/**
 * The manifest-bound linear family definitions, keyed by category. The
 * `satisfies` guard requires exactly one definition per linear category, with
 * the definition's own category as its key, so an omitted family fails
 * typecheck.
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
  minFee: MIN_FEE_FAMILY_DEFINITION,
  doubleWithdraw: DOUBLE_WITHDRAW_FAMILY_DEFINITION,
  l2TxMistag: L2_TX_MISTAG_FAMILY_DEFINITION,
  withdrawnInput: WITHDRAWN_INPUT_FAMILY_DEFINITION,
  inputSetUniqueness: INPUT_SET_UNIQUENESS_FAMILY_DEFINITION,
} satisfies {
  readonly [Category in LinearFamilyCategory]: LinearFamilyDefinitionOf<Category>;
});

/**
 * Any exact linear definition: the distributed union the table's `satisfies`
 * guard admits, so each member keeps its own category, roles and polarity.
 * Readers that iterate the table work from this union, because a union
 * member's polarity cannot be correlated with its own callbacks at the type
 * level; each reader restores the exact row types in its own result type.
 */
export type AnyLinearFamilyDefinition = {
  readonly [Category in LinearFamilyCategory]: LinearFamilyDefinitionOf<Category>;
}[LinearFamilyCategory];
