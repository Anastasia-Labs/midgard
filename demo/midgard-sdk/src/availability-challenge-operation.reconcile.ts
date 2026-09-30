import type {
  AvailabilityOperationIntent,
  AvailabilityOperationLease,
} from "@al-ft/midgard-core/availability-operation-journal";

import { assertTerminalIntent } from "./availability-challenge-operation.build-da-availability-funding-preparation-tx.js";
import {
  assertDaAvailabilitySignedLimits,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationResult,
  inspectDaAvailabilitySignedIntent,
} from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";

export const reconcile = async (
  context: DaAvailabilityOperationContext,
  lease: AvailabilityOperationLease,
  intent: AvailabilityOperationIntent,
  assertCurrent: () => Promise<void>,
): Promise<DaAvailabilityOperationResult> => {
  const checked = inspectDaAvailabilitySignedIntent(intent);
  if (
    JSON.stringify(checked) !== JSON.stringify(intent) ||
    intent.deploymentIdentity !== context.deploymentIdentity ||
    intent.actor !== context.actor
  ) {
    throw new Error(
      "Persisted availability operation metadata does not match signed transaction",
    );
  }
  assertTerminalIntent(context, intent);
  const result = (
    status: DaAvailabilityOperationResult["status"],
  ): DaAvailabilityOperationResult => ({
    status,
    txHash: intent.txHash,
    expectedOutRefs: intent.expectedOutRefs,
  });
  await assertCurrent();
  const observation = await context.observe(intent);
  await assertCurrent();
  const now = context.nowMs ?? Date.now;
  if (context.journal.get(intent.id)?.state === "confirmed") {
    if (
      observation.status === "unknown" ||
      observation.status === "inputs_missing"
    )
      return result("waiting");
    if (
      observation.status !== "included" ||
      observation.txHash !== intent.txHash ||
      !observation.inclusionPoint ||
      !Number.isSafeInteger(observation.confirmationDepth) ||
      observation.confirmationDepth < context.minimumConfirmationDepth
    ) {
      context.journal.halt(
        "A finalized availability transaction lost canonical finality; authenticated recovery required",
      );
      throw new Error("Finalized availability transaction rolled back");
    }
    return result("confirmed");
  }
  switch (observation.status) {
    case "included":
      if (
        observation.txHash !== intent.txHash ||
        !observation.inclusionPoint ||
        !Number.isSafeInteger(observation.confirmationDepth) ||
        observation.confirmationDepth < 0
      ) {
        throw new Error(
          "Availability operation inclusion does not authenticate the signed intent",
        );
      }
      if (observation.confirmationDepth < context.minimumConfirmationDepth) {
        context.journal.transition(
          lease,
          intent.id,
          "included",
          observation.inclusionPoint,
          null,
          now(),
        );
        return result("included");
      }
      context.journal.transition(
        lease,
        intent.id,
        "confirmed",
        observation.inclusionPoint,
        null,
        now(),
      );
      return result("confirmed");
    case "unknown":
      if (context.journal.get(intent.id)?.state === "included") {
        context.journal.transition(
          lease,
          intent.id,
          "pending",
          null,
          observation.reason,
          now(),
        );
      }
      return result("waiting");
    case "conflicting_spend":
      context.journal.transition(
        lease,
        intent.id,
        "conflict",
        null,
        observation.reason,
        now(),
      );
      return result("conflict");
    case "inputs_missing": {
      const foreignSpends = observation.foreignSpends ?? [];
      if (
        !Number.isSafeInteger(observation.currentSlot) ||
        observation.currentSlot < 0 ||
        observation.missingOutRefs.length === 0 ||
        observation.missingOutRefs.some(
          (ref) =>
            !intent.spentOutRefs.includes(ref) &&
            !intent.collateralOutRefs.includes(ref),
        ) ||
        !Array.isArray(foreignSpends) ||
        foreignSpends.some(
          (spend) =>
            !observation.missingOutRefs.includes(spend.outRef) ||
            typeof spend.spendingTxHash !== "string" ||
            !/^[0-9a-f]{64}$/u.test(spend.spendingTxHash) ||
            typeof spend.spendPoint !== "string" ||
            spend.spendPoint.length === 0 ||
            !Number.isSafeInteger(spend.confirmationDepth) ||
            spend.confirmationDepth < 0 ||
            // Our own transaction spending a normal input is inclusion the
            // source failed to report: it contradicts itself.
            (intent.spentOutRefs.includes(spend.outRef) &&
              spend.spendingTxHash === intent.txHash),
        )
      ) {
        throw new Error("Invalid canonical missing-input observation");
      }
      const orphaned =
        observation.currentSlot >= intent.validUntilSlot &&
        observation.missingOutRefs.every((ref) => {
          const parent = context.journal.findTransaction(ref.split("#")[0]!);
          return (
            parent?.state === "expired" &&
            parent.intent.expectedOutRefs.includes(ref) &&
            parent.intent.validUntilSlot <= observation.currentSlot
          );
        });
      if (orphaned) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired child of canonically expired parent",
          now(),
        );
        return result("expired");
      }
      // A transaction spends all of its normal inputs or none of them, so one
      // missing while another is still unspent at the same point proves the
      // intent is not included there; past its validity it never can be. This
      // is how a shared input spent by someone else (a pool TopUp, another
      // header's Timeout) releases the intent instead of waiting forever.
      const missingNormal = intent.spentOutRefs.filter((ref) =>
        observation.missingOutRefs.includes(ref),
      );
      if (
        observation.currentSlot >= intent.validUntilSlot &&
        missingNormal.length > 0 &&
        missingNormal.length < intent.spentOutRefs.length
      ) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with a normal input spent elsewhere and another still unspent",
          now(),
        );
        return result("expired");
      }
      // Every normal input may be gone, as when another watcher's Timeout on
      // the same header landed first. Absence never proves anything, but one
      // normal input consumed by another valid canonical transaction at
      // finality does: a ledger input is spent once, so ours can never land.
      if (
        observation.currentSlot >= intent.validUntilSlot &&
        foreignSpends.some(
          (spend) =>
            intent.spentOutRefs.includes(spend.outRef) &&
            spend.spendingTxHash !== intent.txHash &&
            spend.confirmationDepth >= context.minimumConfirmationDepth,
        )
      ) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with a normal input finally spent by another transaction",
          now(),
        );
        return result("expired");
      }
      if (context.journal.get(intent.id)?.state === "included") {
        context.journal.transition(
          lease,
          intent.id,
          "pending",
          null,
          "Canonical input/inclusion evidence is unresolved",
          now(),
        );
      }
      return result("waiting");
    }
    case "unspent": {
      if (
        !Number.isSafeInteger(observation.currentSlot) ||
        observation.currentSlot < 0
      ) {
        throw new Error(
          "Availability operation reconciliation requires an authoritative slot",
        );
      }
      if (observation.currentSlot >= intent.validUntilSlot) {
        context.journal.transition(
          lease,
          intent.id,
          "expired",
          null,
          "Expired with every normal input canonically unspent",
          now(),
        );
        return result("expired");
      }
      context.journal.transition(
        lease,
        intent.id,
        "pending",
        null,
        "Rebroadcasting canonically unspent intent",
        now(),
      );
      assertDaAvailabilitySignedLimits(intent, context.transactionLimits);
      await assertCurrent();
      // A crash or timeout here is recovered by observing or re-broadcasting the
      // exact same transaction. Never replace signed bytes after an error.
      const txHash = await context.submit(intent.signedCbor);
      if (txHash !== intent.txHash)
        throw new Error(
          "Provider returned a different availability transaction hash",
        );
      return result("submitted");
    }
  }
};
