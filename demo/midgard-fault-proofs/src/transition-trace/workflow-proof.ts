import * as SDK from "@al-ft/midgard-sdk";

import type { TransitionTraceL1Event } from "./l1-events.js";
import {
  readTransitionProof,
  type TransitionProofInput,
} from "./proof-material.js";
import { eventKeyFingerprint } from "./reconstruct.js";

export const transitionTraceProofEventKey = (
  proof: TransitionProofInput,
): SDK.EventKey | null => {
  const fault = readTransitionProof(proof).fault;
  if (
    "InvalidOneStepTransition" in fault &&
    "ValidDepositTransition" in fault.InvalidOneStepTransition.witness
  )
    return {
      DepositEventKey: {
        deposit_id:
          fault.InvalidOneStepTransition.witness.ValidDepositTransition
            .source_membership.key,
      },
    };
  if ("OmittedDueL1Event" in fault) {
    const witness = fault.OmittedDueL1Event.witness;
    if ("OmittedDueDeposit" in witness)
      return {
        DepositEventKey: {
          deposit_id: witness.OmittedDueDeposit.source_non_membership.key,
        },
      };
    if ("OmittedDueWithdrawal" in witness)
      return {
        WithdrawalEventKey: {
          withdrawal_id: witness.OmittedDueWithdrawal.source_non_membership.key,
        },
      };
    return {
      ForcedTransactionEventKey: {
        tx_order_id:
          witness.OmittedDueForcedTransaction.source_non_membership.key,
      },
    };
  }
  if ("OutOfWindowSourceEvent" in fault) {
    const witness = fault.OutOfWindowSourceEvent.witness;
    if ("OutOfWindowDeposit" in witness)
      return {
        DepositEventKey: {
          deposit_id: witness.OutOfWindowDeposit.source_membership.key,
        },
      };
    if ("OutOfWindowWithdrawal" in witness)
      return {
        WithdrawalEventKey: {
          withdrawal_id: witness.OutOfWindowWithdrawal.source_membership.key,
        },
      };
    return {
      ForcedTransactionEventKey: {
        tx_order_id: witness.OutOfWindowForcedTransaction.source_membership.key,
      },
    };
  }
  return null;
};

/** A routed proof fixes semantic evidence. Mutable references are resolved from
 * the actual final transaction, including the forced-order reference index. */
export const bindTransitionTraceProofEvent = ({
  proof,
  events,
}: {
  proof: TransitionProofInput;
  events: ReadonlyMap<string, TransitionTraceL1Event>;
}): {
  proof: TransitionProofInput;
  event: TransitionTraceL1Event | null;
} => {
  const key = transitionTraceProofEventKey(proof);
  if (key === null) return { proof, event: null };
  const event = events.get(eventKeyFingerprint(key));
  if (event === undefined)
    throw new Error(
      "Transition proof lacks its freshly authenticated L1 event",
    );
  return { proof, event };
};
