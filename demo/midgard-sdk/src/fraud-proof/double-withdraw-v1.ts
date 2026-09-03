/**
 * `double-withdraw` family (work item W-C3) — off-chain codec and rule twin.
 *
 * Proves a block header's counted `withdrawals_root` commits two **distinct**
 * withdrawal leaves that both drain the same L2 UTxO (`body.l2_outref`) and
 * are both tagged `WithdrawalIsValid` — a committed double payout of the same
 * L2 funds. The both-payable clause is what keeps honest operators out of
 * reach: due L1 withdrawal events MUST be included, so an honest block commits
 * a same-outref duplicate with a non-payable verdict, and only
 * `WithdrawalIsValid` leaves pay out at settlement.
 *
 * Violation: `double-withdraw`.
 * Catalogue category: `doubleWithdraw` — **not registered yet**. Catalogue
 * registration is parent-owned integration work, so this module is reached by
 * direct import rather than through `fraud-proof/catalogue.ts`, and no
 * category-id constant is exported: like `native-script-decoding` (and unlike
 * the older contingent-pin families) the family pins no id on chain — Init's
 * catalogue-membership proof of the step-01 hash is the binding, and test
 * harnesses carry their own reserved id.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/double-withdraw/step-0{1,2}.ak`
 * field for field and constructor index for constructor index.
 *
 * Committed withdrawal leaves bind in `serialiseData` (definite-map) form on
 * both planes — see `fabricated-withdrawal-v1.ts`'s module note. This family
 * reuses that module's canonical byte helpers
 * (`committedWithdrawalKeyBytes`/`committedWithdrawalValueBytes`) rather
 * than duplicating them.
 */
import { Data } from "@lucid-evolution/lucid";

import { OutputReference, OutputReferenceSchema } from "../common.js";
import type { WithdrawalInfo } from "../ledger-state.js";
import {
  type RootMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
} from "./fabricated-withdrawal-v1.js";

/** Normative violation identifier. */
export const DOUBLE_WITHDRAW_VIOLATION_ID = "double-withdraw" as const;

/**
 * 28-byte hash of the challenged block header. Family-scoped to keep the
 * `fraud-proof` barrel unambiguous.
 */
export const DoubleWithdrawChallengedHeaderHashSchema = Data.Bytes({
  minLength: 28,
  maxLength: 28,
});
export type DoubleWithdrawChallengedHeaderHash = Data.Static<
  typeof DoubleWithdrawChallengedHeaderHashSchema
>;

/**
 * A double-withdraw computation-thread token's asset name under a given
 * (deployment-chosen) category id: the id followed by the challenged header
 * hash. The id is a parameter — this family has no registered production id.
 */
export const doubleWithdrawThreadTokenAssetName = (
  categoryId: string,
  challengedHeaderHash: DoubleWithdrawChallengedHeaderHash,
): string => `${categoryId}${challengedHeaderHash}`;

// ## Membership witness

/**
 * Membership witness for one `(WithdrawalId, WithdrawalInfo)` leaf of
 * `withdrawals_root` — the same canonical counted-root walk the
 * transition-trace and fabricated-withdrawal families use.
 */
export const DoubleWithdrawSourceProofSchema =
  WithdrawalSourceMembershipProofSchema;
export type DoubleWithdrawSourceProof = RootMembershipProof<
  OutputReference,
  WithdrawalInfo
>;

// ## Step 01 — first payable withdrawal leaf

export const DoubleWithdrawStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type DoubleWithdrawStep01Datum = Data.Static<
  typeof DoubleWithdrawStep01DatumSchema
>;
export const DoubleWithdrawStep01Datum =
  DoubleWithdrawStep01DatumSchema as unknown as DoubleWithdrawStep01Datum;

export const DoubleWithdrawStep01ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Reference-input index of the hub oracle. */
  hub_ref_input_index: Data.Integer(),
  /** Reference-input index of the challenged block's state-queue node. */
  state_queue_node_ref_input_index: Data.Integer(),
  /** The first committed payable withdrawal leaf this thread challenges. */
  committed_withdrawal: DoubleWithdrawSourceProofSchema,
});
export type DoubleWithdrawStep01Args = Data.Static<
  typeof DoubleWithdrawStep01ArgsSchema
>;
export const DoubleWithdrawStep01Args =
  DoubleWithdrawStep01ArgsSchema as unknown as DoubleWithdrawStep01Args;

export const DoubleWithdrawStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DoubleWithdrawStep01ArgsSchema);
export type DoubleWithdrawStep01SpendRedeemer = Data.Static<
  typeof DoubleWithdrawStep01SpendRedeemerSchema
>;
export const DoubleWithdrawStep01SpendRedeemer =
  DoubleWithdrawStep01SpendRedeemerSchema as unknown as DoubleWithdrawStep01SpendRedeemer;

// ## Step 02 — second payable leaf and finalization

export const DoubleWithdrawStep02StateSchema = Data.Object({
  /** 28-byte hash of the challenged block header. */
  challenged_header_hash: DoubleWithdrawChallengedHeaderHashSchema,
  /** The first payable leaf's identity — an L1 output reference. */
  first_withdrawal_id: OutputReferenceSchema,
  /** The L2 UTxO the first payable leaf drains. */
  first_l2_outref: OutputReferenceSchema,
});
export type DoubleWithdrawStep02State = Data.Static<
  typeof DoubleWithdrawStep02StateSchema
>;
export const DoubleWithdrawStep02State =
  DoubleWithdrawStep02StateSchema as unknown as DoubleWithdrawStep02State;

export const DoubleWithdrawStep02DatumSchema = faultProofStepDatumSchema(
  DoubleWithdrawStep02StateSchema,
);
export type DoubleWithdrawStep02Datum = Data.Static<
  typeof DoubleWithdrawStep02DatumSchema
>;
export const DoubleWithdrawStep02Datum =
  DoubleWithdrawStep02DatumSchema as unknown as DoubleWithdrawStep02Datum;

export const DoubleWithdrawStep02ArgsSchema = Data.Object({
  /** Own input index. */
  input_index: Data.Integer(),
  /** Produced output index. */
  output_index: Data.Integer(),
  /** Index of the fraud-proof mint redeemer. */
  fraud_proof_mint_redeemer_index: Data.Integer(),
  /** Reference-input index of the hub oracle. */
  hub_ref_input_index: Data.Integer(),
  /** Reference-input index of the challenged block's state-queue node. */
  state_queue_node_ref_input_index: Data.Integer(),
  /** The second committed payable withdrawal leaf. */
  committed_withdrawal: DoubleWithdrawSourceProofSchema,
});
export type DoubleWithdrawStep02Args = Data.Static<
  typeof DoubleWithdrawStep02ArgsSchema
>;
export const DoubleWithdrawStep02Args =
  DoubleWithdrawStep02ArgsSchema as unknown as DoubleWithdrawStep02Args;

export const DoubleWithdrawStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DoubleWithdrawStep02ArgsSchema);
export type DoubleWithdrawStep02SpendRedeemer = Data.Static<
  typeof DoubleWithdrawStep02SpendRedeemerSchema
>;
export const DoubleWithdrawStep02SpendRedeemer =
  DoubleWithdrawStep02SpendRedeemerSchema as unknown as DoubleWithdrawStep02SpendRedeemer;

// ## Step resolver

export const DOUBLE_WITHDRAW_STEP_NAMES = ["step_01", "step_02"] as const;
export type DoubleWithdrawStepName =
  (typeof DOUBLE_WITHDRAW_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch:
 * adding a step without adding its schema fails to compile.
 */
export const doubleWithdrawStepDatumSchema = (step: DoubleWithdrawStepName) => {
  switch (step) {
    case "step_01":
      return DoubleWithdrawStep01DatumSchema;
    case "step_02":
      return DoubleWithdrawStep02DatumSchema;
  }
};

// ## Handoffs

/**
 * The step-01 → step-02 handoff, derived from the authenticated header hash
 * and the first committed leaf the membership witness opens. Twin of the
 * step-01 validator's `expected_output_state`.
 */
export const doubleWithdrawStep02State = ({
  challengedHeaderHash,
  committedWithdrawal,
}: {
  readonly challengedHeaderHash: DoubleWithdrawChallengedHeaderHash;
  readonly committedWithdrawal: DoubleWithdrawSourceProof;
}): DoubleWithdrawStep02State => ({
  challenged_header_hash: challengedHeaderHash,
  first_withdrawal_id: committedWithdrawal.key,
  first_l2_outref: committedWithdrawal.value.body.l2_outref,
});

// ## The rule

const sameOutputReference = (a: OutputReference, b: OutputReference): boolean =>
  a.transactionId.toLowerCase() === b.transactionId.toLowerCase() &&
  a.outputIndex === b.outputIndex;

/**
 * Twin of the step-01 validator's entry condition: a leaf may enter the
 * thread only when its committed verdict is payable.
 */
export const isPayableWithdrawalLeaf = (info: WithdrawalInfo): boolean =>
  info.validity === "WithdrawalIsValid";

/**
 * Twin of `step_02.double_withdraw_fault_is_established_v1`: the second
 * committed leaf convicts exactly when its identity differs from the first's,
 * it drains the L2 UTxO the first payable leaf drained, and it is itself
 * payable. (The first leaf's payability is step-01's entry condition —
 * `isPayableWithdrawalLeaf` — and is not restated in the state.)
 */
export const isDoubleWithdrawFault = (
  state: DoubleWithdrawStep02State,
  second: DoubleWithdrawSourceProof,
): boolean =>
  !sameOutputReference(second.key, state.first_withdrawal_id) &&
  sameOutputReference(second.value.body.l2_outref, state.first_l2_outref) &&
  isPayableWithdrawalLeaf(second.value);
