import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { TransitionTraceL1Event } from "./l1-events.js";
import { eventKeyFingerprint } from "./reconstruct.js";

export const transitionTraceProofEventKey = (
  proof: SDK.TransitionFaultProof,
): SDK.EventKey | null => {
  const fault = proof.fault;
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

/** Timed-event finals carry an inline proof and a fixed reference set. Bind its
 * pointer before the route freezes that proof; deposit continuations instead
 * resolve their pointer from the actual final transaction redeemer context. */
export const bindTransitionTraceProofEvent = ({
  proof,
  events,
  finalReferences,
}: {
  proof: SDK.TransitionFaultProof;
  events: ReadonlyMap<string, TransitionTraceL1Event>;
  finalReferences: readonly UTxO[];
}): {
  proof: SDK.TransitionFaultProof;
  event: TransitionTraceL1Event | null;
} => {
  const key = transitionTraceProofEventKey(proof);
  if (key === null) return { proof, event: null };
  const event = events.get(eventKeyFingerprint(key));
  if (event === undefined)
    throw new Error(
      "Transition proof lacks its freshly authenticated L1 event",
    );
  const ordered = [
    ...new Map(
      [...finalReferences, event.utxo].map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex}`,
        utxo,
      ]),
    ).values(),
  ].sort(compareOutRefs);
  const index = BigInt(
    ordered.findIndex(
      (utxo) =>
        utxo.txHash === event.utxo.txHash &&
        utxo.outputIndex === event.utxo.outputIndex,
    ),
  );
  const fault = proof.fault;
  if ("OmittedDueL1Event" in fault) {
    const witness = fault.OmittedDueL1Event.witness;
    const bound: SDK.OmittedDueL1EventWitness =
      "OmittedDueDeposit" in witness
        ? {
            OmittedDueDeposit: {
              ...witness.OmittedDueDeposit,
              event_ref_input_index: index,
            },
          }
        : "OmittedDueWithdrawal" in witness
          ? {
              OmittedDueWithdrawal: {
                ...witness.OmittedDueWithdrawal,
                event_ref_input_index: index,
              },
            }
          : {
              OmittedDueForcedTransaction: {
                ...witness.OmittedDueForcedTransaction,
                event_ref_input_index: index,
              },
            };
    return {
      proof: { ...proof, fault: { OmittedDueL1Event: { witness: bound } } },
      event,
    };
  }
  if ("OutOfWindowSourceEvent" in fault) {
    const witness = fault.OutOfWindowSourceEvent.witness;
    const bound: SDK.OutOfWindowSourceEventWitness =
      "OutOfWindowDeposit" in witness
        ? {
            OutOfWindowDeposit: {
              ...witness.OutOfWindowDeposit,
              event_ref_input_index: index,
            },
          }
        : "OutOfWindowWithdrawal" in witness
          ? {
              OutOfWindowWithdrawal: {
                ...witness.OutOfWindowWithdrawal,
                event_ref_input_index: index,
              },
            }
          : {
              OutOfWindowForcedTransaction: {
                ...witness.OutOfWindowForcedTransaction,
                event_ref_input_index: index,
              },
            };
    return {
      proof: {
        ...proof,
        fault: { OutOfWindowSourceEvent: { witness: bound } },
      },
      event,
    };
  }
  return { proof, event };
};
