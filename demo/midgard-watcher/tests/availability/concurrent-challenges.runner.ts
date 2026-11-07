import type * as SDK from "@al-ft/midgard-sdk";

export const stubRunner = async (
  context: SDK.DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: string;
    completesWorkflow?: boolean;
    unsignedDeadlineMs?: number;
    preparationScope?: SDK.DaAvailabilityReadScope;
    build: () => Promise<unknown>;
  }>,
): Promise<SDK.DaAvailabilityOperationResult> => {
  const now = Date.now();
  const lease = context.journal.acquire(context.actor, "runner", now, 60_000);
  try {
    context.journal.assertWorkflow(
      lease,
      context.deploymentIdentity,
      operation.headerHash,
      operation.action,
      now,
    );
    await operation.build();
    const id = `${operation.action}-${operation.headerHash}`;
    context.journal.persist(
      lease,
      {
        id,
        deploymentIdentity: context.deploymentIdentity,
        actor: context.actor,
        headerHash: operation.headerHash,
        action: operation.action,
        signedCbor: id,
        txHash: id,
        spentOutRefs: [`${id}#0`],
        collateralOutRefs: [],
        expectedOutRefs: [],
        validUntilSlot: 1,
        completesWorkflow: operation.completesWorkflow ?? false,
      },
      now,
    );
    context.journal.transition(lease, id, "confirmed", "block", null, now);
    return { status: "submitted", txHash: id, expectedOutRefs: [] };
  } finally {
    context.journal.release(lease);
  }
};
