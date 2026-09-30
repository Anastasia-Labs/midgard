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
import { reconcile } from "./availability-challenge-operation.reconcile.js";

/** Reconciles existing actor intent before constructing any new transaction. */
export const runDaAvailabilityOperation = (
  context: DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: AvailabilityOperationIntent["action"];
    /** Timeout is terminal only when it removes the challenged head itself. */
    completesWorkflow?: boolean;
    /**
     * The unsigned transaction, or a built one carrying its slashed fee part
     * (a `BuiltDaAvailabilityTransaction` qualifies). A timeout must return
     * its fee part: its fee ceiling caps only `fee - feePart`.
     */
    build: () => Promise<TxSignBuilder | DaAvailabilityOperationBuild>;
  }>,
): Promise<DaAvailabilityOperationResult> =>
  withLease(context, async (lease, assertCurrent) => {
    const pending = context.journal.pending(
      context.deploymentIdentity,
      context.actor,
    );
    if (pending.length > 0)
      return reconcile(context, lease, pending[0]!.intent, assertCurrent);
    for (const anchor of context.journal.finalizedAnchors(
      context.deploymentIdentity,
      context.actor,
    )) {
      const audit = await reconcile(
        context,
        lease,
        anchor.intent,
        assertCurrent,
      );
      if (audit.status !== "confirmed") return audit;
    }
    context.journal.assertWorkflow(
      lease,
      context.deploymentIdentity,
      operation.headerHash,
      operation.action,
      (context.nowMs ?? Date.now)(),
    );
    const built = await operation.build();
    const { tx, timeoutFeePartLovelace } =
      "toTransaction" in built
        ? { tx: built, timeoutFeePartLovelace: undefined }
        : built;
    if (operation.action === "timeout" && timeoutFeePartLovelace === undefined)
      throw new Error(
        "A timeout operation must report its slashed fee part to be signed",
      );
    await assertCurrent();
    const unsignedHash = CML.hash_transaction(
      tx.toTransaction().body(),
    ).to_hex();
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
      return reconcile(context, lease, intent, assertCurrent);
    context.journal.persist(lease, intent, (context.nowMs ?? Date.now)());
    return reconcile(context, lease, intent, assertCurrent);
  });
