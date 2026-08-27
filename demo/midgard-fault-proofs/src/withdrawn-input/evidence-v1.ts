import {
  type MidgardTxInput,
  type OutputReference,
  type WithdrawalInfo,
  type WithdrawalSourceMembershipProof,
} from "@al-ft/midgard-sdk";

import type { SubmitStep01TxInclusion } from "../submit-step-01.js";

export const WITHDRAWN_INPUT_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-withdrawn-input-evidence-v1" as const;

/** Submit-ready evidence for all three steps, bound to one header. */
export type PreparedWithdrawnInputEvidenceV1 = {
  readonly schemaVersion: typeof WITHDRAWN_INPUT_EVIDENCE_V1_SCHEMA_VERSION;
  readonly headerHash: string;
  readonly badTxInclusion: SubmitStep01TxInclusion;
  readonly spendInputs: readonly MidgardTxInput[];
  readonly badInputIndex: number;
  readonly withdrawnInput: MidgardTxInput;
  readonly withdrawalId: OutputReference;
  readonly withdrawal: WithdrawalInfo;
  readonly withdrawalMembership: WithdrawalSourceMembershipProof;
};

export class WithdrawnInputEvidenceRejectionV1 extends Error {
  constructor(
    readonly code:
      | "no_valid_withdrawn_input"
      | "transactions_root_mismatch"
      | "withdrawals_root_mismatch"
      | "bad_tx_not_committed"
      | "bad_input_index_out_of_range",
    detail: string,
  ) {
    super(`${code}: ${detail}`);
    this.name = "WithdrawnInputEvidenceRejectionV1";
  }
}

export const withdrawnInputEvidenceRejectV1 = (
  code: WithdrawnInputEvidenceRejectionV1["code"],
  detail: string,
): never => {
  throw new WithdrawnInputEvidenceRejectionV1(code, detail);
};
