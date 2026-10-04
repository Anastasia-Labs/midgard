import type {
  AvailabilityOperationIntent,
  AvailabilityOperationLease,
} from "@al-ft/midgard-core/availability-operation-journal";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import { assertTerminalIntent } from "./availability-challenge-operation.build-da-availability-funding-preparation-tx.js";
import {
  assertDaAvailabilitySignedLimits,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationObservation,
  type DaAvailabilityOperationResult,
  inspectDaAvailabilitySignedIntent,
} from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import {
  createDaAvailabilityReadScope,
  type DaAvailabilityReadScope,
  DaAvailabilityReadScopeExpiredError,
} from "./availability-challenge-operation.read-scope.js";

type Observed<S extends DaAvailabilityOperationObservation["status"]> = Extract<
  DaAvailabilityOperationObservation,
  { status: S }
>;

export const authenticates = (
  intent: AvailabilityOperationIntent,
  observation: Observed<"included">,
): boolean =>
  observation.txHash === intent.txHash &&
  Boolean(observation.inclusionPoint) &&
  Number.isSafeInteger(observation.confirmationDepth) &&
  observation.confirmationDepth >= 0;

/** Applies the journal's retention to `intent` from inclusion that authenticates it. */
export const retireIncluded = (
  context: DaAvailabilityOperationContext,
  lease: AvailabilityOperationLease,
  intent: AvailabilityOperationIntent,
  observed: Observed<"included">,
): void =>
  context.journal.retire(
    lease,
    intent.id,
    {
      confirmationDepth: observed.confirmationDepth,
      inclusionPoint: observed.inclusionPoint,
      ...(observed.currentBlockNo === undefined
        ? {}
        : { currentBlockNo: observed.currentBlockNo }),
      ...(Number.isSafeInteger(observed.currentSlot) &&
      observed.currentSlot! >= 0
        ? { currentSlot: observed.currentSlot }
        : {}),
      recoveryDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
    },
    (context.nowMs ?? Date.now)(),
  );

/**
 * Whether missing inputs prove the intent can never land: the detail its
 * expiry records, `undefined` while they prove nothing, or `invalid` for an
 * observation that is not canonical missing-input evidence.
 */
const missingInputExpiry = (
  context: DaAvailabilityOperationContext,
  intent: AvailabilityOperationIntent,
  observation: Observed<"inputs_missing">,
): string | undefined | "invalid" => {
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
  )
    return "invalid";
  if (observation.currentSlot < intent.validUntilSlot) return undefined;
  if (
    observation.missingOutRefs.every((ref) => {
      const parent = context.journal.findTransaction(ref.split("#")[0]!);
      return (
        parent?.state === "expired" &&
        parent.intent.expectedOutRefs.includes(ref) &&
        parent.intent.validUntilSlot <= observation.currentSlot
      );
    })
  )
    return "Expired child of canonically expired parent";
  // A transaction spends all of its normal inputs or none of them, so one
  // missing while another is still unspent at the same point proves the
  // intent is not included there; past its validity it never can be. This
  // is how a shared input spent by someone else (a pool TopUp, another
  // header's Timeout) releases the intent instead of waiting forever.
  const missingNormal = intent.spentOutRefs.filter((ref) =>
    observation.missingOutRefs.includes(ref),
  );
  if (
    missingNormal.length > 0 &&
    missingNormal.length < intent.spentOutRefs.length
  )
    return "Expired with a normal input spent elsewhere and another still unspent";
  // Every normal input may be gone, as when another watcher's Timeout on
  // the same header landed first. Absence never proves anything, but one
  // normal input consumed by another valid canonical transaction at
  // finality does: a ledger input is spent once, so ours can never land.
  if (
    foreignSpends.some(
      (spend) =>
        intent.spentOutRefs.includes(spend.outRef) &&
        spend.spendingTxHash !== intent.txHash &&
        spend.confirmationDepth >= context.minimumConfirmationDepth,
    )
  )
    return "Expired with a normal input finally spent by another transaction";
  return undefined;
};

export const reconcile = async (
  context: DaAvailabilityOperationContext,
  lease: AvailabilityOperationLease,
  intent: AvailabilityOperationIntent,
  assertCurrent: (scope?: DaAvailabilityReadScope) => Promise<void>,
  observationScope?: DaAvailabilityReadScope,
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
    detail?: string,
  ): DaAvailabilityOperationResult => ({
    status,
    txHash: intent.txHash,
    expectedOutRefs: intent.expectedOutRefs,
    ...(detail === undefined ? {} : { detail }),
  });
  const scope =
    observationScope ??
    createDaAvailabilityReadScope({
      attemptTimeoutMs:
        context.observationTimeoutMs ?? context.leaseDurationMs ?? 300_000,
      signal: context.observationSignal,
      nowMs: context.nowMs,
      monotonicMs: context.monotonicMs,
    });
  const attempt = async (): Promise<DaAvailabilityOperationResult> => {
    try {
      const observation = await scope.read(async () => {
        await assertCurrent(scope);
        const observed = await context.observe(intent, scope);
        scope.assertCurrent();
        await assertCurrent(scope);
        return observed;
      });
      const now = context.nowMs ?? Date.now;
      const retire = (observed: Observed<"included">): void =>
        retireIncluded(context, lease, intent, observed);
      if (context.journal.get(intent.id)?.state === "confirmed") {
        // Confirmation depth is not finality. Positive evidence that the
        // transaction left the canonical chain returns it to pending, and the
        // cases below land the same signed bytes again or prove they never can.
        // Evidence that neither confirms nor contradicts it holds this intent
        // alone, changes nothing, and is read afresh on the next pass.
        switch (observation.status) {
          case "unknown":
            return result("waiting");
          case "included":
            if (!authenticates(intent, observation))
              return result(
                "held",
                "Inclusion evidence for a confirmed availability transaction does not authenticate it",
              );
            if (
              observation.confirmationDepth >= context.minimumConfirmationDepth
            ) {
              retire(observation);
              return result("confirmed");
            }
            break;
          case "inputs_missing": {
            const expiry = missingInputExpiry(context, intent, observation);
            if (expiry === "invalid")
              return result(
                "held",
                "Missing-input evidence for a confirmed availability transaction is not canonical",
              );
            if (expiry === undefined) return result("waiting");
            break;
          }
          case "unspent":
          case "conflicting_spend":
            break;
        }
        // An input another intent reserved since its release is a double
        // claim: it is surfaced as a conflict, and both intents keep their
        // reservations.
        const conflict = context.journal.rewind(
          lease,
          intent.id,
          `Confirmed availability transaction left the canonical chain (${observation.status})`,
          now(),
        );
        if (conflict !== null) return result("conflict", conflict);
      }
      switch (observation.status) {
        case "included":
          if (!authenticates(intent, observation)) {
            throw new Error(
              "Availability operation inclusion does not authenticate the signed intent",
            );
          }
          if (
            observation.confirmationDepth < context.minimumConfirmationDepth
          ) {
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
          retire(observation);
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
          const expiry = missingInputExpiry(context, intent, observation);
          if (expiry === "invalid")
            throw new Error("Invalid canonical missing-input observation");
          if (expiry !== undefined) {
            context.journal.transition(
              lease,
              intent.id,
              "expired",
              null,
              expiry,
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
          await scope.read(() => assertCurrent(scope));
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
    } catch (error) {
      if (
        error instanceof DaAvailabilityReadScopeExpiredError ||
        (scope.signal.aborted && error === scope.signal.reason)
      )
        return result(
          "waiting",
          "Signed availability evidence attempt ended unresolved; exact bytes and reservations retained",
        );
      throw error;
    }
  };
  return attempt().finally(() => {
    if (observationScope === undefined) scope.close();
  });
};
