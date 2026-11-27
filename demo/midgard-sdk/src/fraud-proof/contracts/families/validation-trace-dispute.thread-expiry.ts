import { normalizeOutRef, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core/validation-dispute";
import { Data } from "@lucid-evolution/lucid";

import { OutputReference, type WithdrawalValidator } from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeWithdrawalValidator,
} from "../blueprint.js";
import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "./validation-trace-dispute.validation-trace-dispute-fault-proof-titles.js";

/** An output reference as a blueprint parameter. */
export const outRefData = (outRef: OutRefLike): Data => {
  const { txHash, outputIndex } = normalizeOutRef(outRef);
  return Data.from(
    Data.to(
      { transactionId: txHash, outputIndex: BigInt(outputIndex) },
      OutputReference,
    ),
  );
};

/**
 * The thread expiry script of a case policy. Its case duration must be the
 * D_case the case validator itself applies
 * (`MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS`): the case reads the expiry
 * hash from its configuration datum and never checks the duration, so an
 * expiry built with any other duration would release threads the case can
 * still take, or hold them past the case's end. Any other duration is
 * refused.
 */
export const buildValidationThreadExpiry = (
  blueprint: FaultProofBlueprint,
  threadPolicyId: string,
  caseDurationMs: number,
): WithdrawalValidator => {
  if (caseDurationMs !== MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS) {
    throw new Error(
      `Thread expiry case duration ${caseDurationMs.toString()} ms differs ` +
        `from the case's D_case ` +
        `${MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS.toString()} ms`,
    );
  }
  return makeWithdrawalValidator(
    applyBlueprintParams(
      blueprint,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.threadExpiry,
      [threadPolicyId, BigInt(caseDurationMs)],
    ),
  );
};
