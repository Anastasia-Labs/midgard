import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import { CML, type TxSignBuilder } from "@lucid-evolution/lucid";

import {
  assertTerminalIntent,
  withLease,
} from "./availability-challenge-operation.build-da-availability-funding-preparation-tx.js";
import { type DaAvailabilityOperationBuild } from "./availability-challenge-operation.create-da-availability-operation-observer.js";
import {
  assertDaAvailabilitySignedLimits,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationResult,
  inspectDaAvailabilitySignedIntent,
} from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import {
  createDaAvailabilityReadScope,
  type DaAvailabilityReadScope,
} from "./availability-challenge-operation.read-scope.js";
import { reconcile } from "./availability-challenge-operation.reconcile.js";
import { assertUnsignedAvailabilityDeadline } from "./availability-challenge-operation.unsigned-deadline.js";

/** Reconciles existing actor intent before constructing any new transaction. */
export const runDaAvailabilityOperation = (
  context: DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: AvailabilityOperationIntent["action"];
    /** Timeout is terminal only when it removes the challenged head itself. */
    completesWorkflow?: boolean;
    /** Absolute protocol deadline for fresh unsigned work only. Existing
     * signed bytes reconcile first, even after this deadline. */
    unsignedDeadlineMs?: number;
    /** Shared scope created before discovery/funding; retries reuse it. */
    preparationScope?: DaAvailabilityReadScope;
    /**
     * The unsigned transaction, or a built one carrying its slashed fee part
     * (a `BuiltDaAvailabilityTransaction` qualifies). A timeout must return
     * its fee part: its fee ceiling caps only `fee - feePart`.
     */
    build: (
      signal: AbortSignal,
      scope?: DaAvailabilityReadScope,
    ) => Promise<TxSignBuilder | DaAvailabilityOperationBuild>;
  }>,
): Promise<DaAvailabilityOperationResult> =>
  withLease(context, async (lease, assertCurrent, observationScope) => {
    const pending = context.journal.pending(
      context.deploymentIdentity,
      context.actor,
    );
    if (pending.length > 0)
      return reconcile(
        context,
        lease,
        pending[0]!.intent,
        assertCurrent,
        observationScope(),
      );
    for (const anchor of context.journal.finalizedAnchors(
      context.deploymentIdentity,
      context.actor,
    )) {
      const audit = await reconcile(
        context,
        lease,
        anchor.intent,
        assertCurrent,
        observationScope(),
      );
      if (audit.status !== "confirmed") return audit;
    }
    const now = context.nowMs ?? Date.now;
    assertUnsignedAvailabilityDeadline(operation.unsignedDeadlineMs, now);
    if (
      operation.preparationScope !== undefined &&
      operation.unsignedDeadlineMs !== undefined &&
      (operation.preparationScope.deadlineEpochMs === undefined ||
        operation.preparationScope.deadlineEpochMs >
          operation.unsignedDeadlineMs)
    )
      throw new Error(
        "Shared availability scope exceeds the operation deadline",
      );
    const scope =
      operation.preparationScope ??
      createDaAvailabilityReadScope({
        deadlineEpochMs: operation.unsignedDeadlineMs,
        attemptTimeoutMs: context.leaseDurationMs ?? 300_000,
        nowMs: now,
        monotonicMs: context.monotonicMs,
      });
    try {
      const built = await scope.read(async (signal) => {
        await assertCurrent(scope);
        context.journal.assertWorkflow(
          lease,
          context.deploymentIdentity,
          operation.headerHash,
          operation.action,
          now(),
        );
        const result = await operation.build(signal, scope);
        scope.assertCurrent();
        await assertCurrent(scope);
        scope.assertCurrent();
        return result;
      });
      const { tx, timeoutFeePartLovelace } =
        "toTransaction" in built
          ? { tx: built, timeoutFeePartLovelace: undefined }
          : built;
      if (
        operation.action === "timeout" &&
        timeoutFeePartLovelace === undefined
      )
        throw new Error(
          "A timeout operation must report its slashed fee part to be signed",
        );
      const unsignedHash = CML.hash_transaction(
        tx.toTransaction().body(),
      ).to_hex();
      assertUnsignedAvailabilityDeadline(operation.unsignedDeadlineMs, now);
      scope.assertCurrent();
      const signed = await tx.sign.withWallet().complete();
      await assertCurrent();
      const intent = inspectDaAvailabilitySignedIntent({
        deploymentIdentity: context.deploymentIdentity,
        actor: context.actor,
        headerHash: operation.headerHash,
        action: operation.action,
        signedCbor: signed.toCBOR(),
        completesWorkflow: operation.completesWorkflow,
      });
      if (intent.txHash !== unsignedHash)
        throw new Error("Signing changed the availability transaction body");
      assertTerminalIntent(context, intent);
      assertDaAvailabilitySignedLimits(
        intent,
        context.transactionLimits,
        timeoutFeePartLovelace,
      );
      const existing = context.journal.get(intent.id);
      if (existing?.state === "included" || existing?.state === "confirmed")
        return reconcile(
          context,
          lease,
          intent,
          assertCurrent,
          observationScope(),
        );
      context.journal.persist(lease, intent, (context.nowMs ?? Date.now)());
      return reconcile(
        context,
        lease,
        intent,
        assertCurrent,
        observationScope(),
      );
    } finally {
      // The caller owns a shared scope used by discovery/rebuilds too.
      if (operation.preparationScope === undefined) scope.close();
    }
  });
