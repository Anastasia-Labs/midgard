import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import { promiseAdmissionActorMetadata } from "./promise-actor-metadata.js";

/** The follower view a receipt was read under (plan §8.1). */
type Boundary = Readonly<{
  pointId: string;
  blockNo: number;
  generation: number;
}>;
type Reconciled = "ready" | "pending" | Readonly<{ held: string }>;

/** A read-only admission receipt from real SDK reconciliation. SQL state alone
 * cannot mint it, and it never releases claims or exempts an actor's ownership. */
export const committeeClaimReconciliation = (args: {
  journal: AvailabilityOperationJournal;
  actorId: string;
  deploymentIdentity: string;
  openReadScope: () => DaAvailabilityReadScope;
  readBoundary: (scope: DaAvailabilityReadScope) => Promise<Boundary>;
  reconcile: (scope: DaAvailabilityReadScope) => Promise<Reconciled>;
  /** Owning runtime tracks every still-running unsigned callback, even after timeout. */
  assertRuntimeIdle: () => void;
  maximumRetainedRecords?: number;
}) => {
  let receipt:
    | Readonly<{ boundary: Boundary; stateDigest: string }>
    | undefined;
  const { snapshot, busy: unresolved } = promiseAdmissionActorMetadata({
    journal: args.journal,
    actorId: args.actorId,
    deploymentIdentity: args.deploymentIdentity,
    maximumRetainedRecords: args.maximumRetainedRecords,
    hasCanonicalCompatibilityAuthority: true,
  });

  const reconcile = async (
    inheritedScope?: DaAvailabilityReadScope,
  ): Promise<Reconciled> => {
    receipt = undefined;
    const scope = inheritedScope ?? args.openReadScope();
    try {
      const before = await args.readBoundary(scope);
      // Reconciliation owns durable mutations; never race this callback.
      const result = await args.reconcile(scope);
      if (result !== "ready") return result;
      const reconciled = snapshot();
      const after = await args.readBoundary(scope);
      scope.assertCurrent();
      args.assertRuntimeIdle();
      const final = snapshot();
      if (
        before.pointId !== after.pointId ||
        before.blockNo !== after.blockNo ||
        before.generation !== after.generation ||
        unresolved(final) ||
        reconciled.stateDigest !== final.stateDigest
      )
        throw new Error(
          "Canonical actor reconciliation changed before its admission receipt",
        );
      receipt = Object.freeze({
        boundary: Object.freeze({
          pointId: after.pointId,
          blockNo: after.blockNo,
          generation: after.generation,
        }),
        stateDigest: final.stateDigest,
      });
      return result;
    } finally {
      if (inheritedScope === undefined) scope.close();
    }
  };

  const assertCompatibleClaimsCurrent = async (
    boundary: Boundary,
    scope?: DaAvailabilityReadScope,
    metadata = snapshot(),
  ): Promise<void> => {
    scope?.assertCurrent();
    args.assertRuntimeIdle();
    const current = snapshot();
    if (
      receipt === undefined ||
      receipt.boundary.pointId !== boundary.pointId ||
      receipt.boundary.blockNo !== boundary.blockNo ||
      receipt.boundary.generation !== boundary.generation ||
      unresolved(current) ||
      metadata.stateDigest !== current.stateDigest ||
      current.stateDigest !== receipt.stateDigest
    )
      throw new Error(
        "Retained claims lack a current exact canonical reconciliation receipt",
      );
  };
  return { reconcile, assertCompatibleClaimsCurrent };
};
