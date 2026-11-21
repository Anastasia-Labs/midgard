import {
  type FraudProofWorkflowJournalEvent,
  requireExactKeys,
  requireOptionalExactKeys,
} from "./journal.fraud-proof-workflow-journal-event.js";
import {
  requireOutRef,
  requireTxHash,
} from "./journal.require-transaction-identity.js";

type Forfeited = Extract<
  FraudProofWorkflowJournalEvent,
  { readonly kind: "collateral_forfeited" }
>;

/**
 * The journal fold's check of collateral forfeitures. A forfeiture names
 * the exact signed intent of its action and the collateral that attempt
 * posted, at most one stands per transaction, and a reversal undoes the
 * standing one.
 */
export const collateralForfeitures = () => {
  const intentActionByTransaction = new Map<string, string>();
  const forfeitedActionByTransaction = new Map<string, string>();
  const forfeited = (event: Forfeited): void => {
    requireOptionalExactKeys(
      event,
      ["kind", "actionId", "txHash", "collateralOutRefs"],
      ["collateralReturnOutRef"],
      "journal collateral forfeiture",
    );
    requireTxHash(event.txHash, "journal collateral forfeiture txHash");
    if (intentActionByTransaction.get(event.txHash) !== event.actionId)
      throw new Error(
        "journal collateral forfeiture lacks its exact signed intent",
      );
    if (forfeitedActionByTransaction.has(event.txHash))
      throw new Error("journal collateral forfeiture repeats a standing one");
    if (
      !Array.isArray(event.collateralOutRefs) ||
      event.collateralOutRefs.length === 0
    )
      throw new Error("journal collateral forfeiture names no collateral");
    event.collateralOutRefs.forEach((outRef, index) => {
      requireOutRef(outRef, "journal forfeited collateral");
      if (index > 0 && event.collateralOutRefs[index - 1]! >= outRef)
        throw new Error("journal forfeited collateral is not strictly sorted");
    });
    if (event.collateralReturnOutRef !== undefined) {
      requireOutRef(event.collateralReturnOutRef, "journal collateral return");
      if (!event.collateralReturnOutRef.startsWith(`${event.txHash}#`))
        throw new Error(
          "journal collateral return is not an output of its transaction",
        );
    }
    forfeitedActionByTransaction.set(event.txHash, event.actionId);
  };
  return Object.freeze({
    /**
     * Checks a forfeiture or its reversal and says it was one; any other
     * event passes through, a submission intent noted for a later
     * forfeiture to name.
     */
    step: (value: unknown): boolean => {
      if (typeof value !== "object" || value === null) return false;
      const event = value as FraudProofWorkflowJournalEvent;
      if (event.kind === "submission_intent") {
        intentActionByTransaction.set(event.txHash, event.actionId);
        return false;
      }
      if (event.kind === "collateral_forfeited") {
        forfeited(event);
        return true;
      }
      if (event.kind !== "collateral_forfeiture_reversed") return false;
      requireExactKeys(
        event,
        ["kind", "actionId", "txHash"],
        "journal collateral forfeiture reversal",
      );
      if (forfeitedActionByTransaction.get(event.txHash) !== event.actionId)
        throw new Error(
          "journal collateral forfeiture reversal lacks its standing forfeiture",
        );
      forfeitedActionByTransaction.delete(event.txHash);
      return true;
    },
  });
};
