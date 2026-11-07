import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { CommitteeL1ClientConfig } from "../config.js";
import type {
  CommitteeAvailabilityReads,
  FollowerReadBoundary,
} from "../l1/follower/availability-reads.js";
import {
  type CommitteeStore,
  retirementMetadataGrowthReserve,
} from "../store.js";
import {
  committeeRetirementSource,
  type CommitteeRetirementSourceDependencies,
} from "../store/retirement-source.js";
import type { availabilityResponderOperations } from "./factory.availability-responder-operations.js";
import type { loadCommitteePromiseCausalAdoption } from "./promise-causal-adoption.js";
import type { committeeClaimReconciliation } from "./promise-claim-reconciliation.js";
import type { committeePromiseExecutionScopes } from "./promise-execution-scopes.js";
import { committeePromiseJoinedReads } from "./promise-owned-read.js";
import { committeeRetirementBinding } from "./promise-retirement-binding.js";

/**
 * The retirement source's boundary and point reads over the committee
 * follower: each point is read at the boundary its scope last read, with the
 * follower view generation that read saw. A later read of the same point at
 * another generation means the view was rebuilt under the scope: its reads
 * refuse.
 */
export const committeeFollowerRetirementReads = (
  reads: Pick<
    CommitteeAvailabilityReads,
    "canonicalPoint" | "submissionPoint" | "landingPoint"
  >,
  readBoundary: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<FollowerReadBoundary>,
): Pick<
  CommitteeRetirementSourceDependencies,
  | "readBoundary"
  | "readCanonicalPoint"
  | "readSubmissionPoint"
  | "readLandingPoint"
> => {
  const views = new WeakMap<
    SDK.DaAvailabilityReadScope,
    Readonly<{ pointId: string; generation: number }>
  >();
  const at = (
    boundary: Readonly<{ slot: number; blockHash: string; blockNo: number }>,
    scope: SDK.DaAvailabilityReadScope,
  ): FollowerReadBoundary => {
    const view = views.get(scope);
    if (view?.pointId !== `${boundary.slot}:${boundary.blockHash}`)
      throw new Error("Retirement read is not at its scope's boundary");
    return { ...boundary, generation: view.generation };
  };
  return {
    readBoundary: async (scope) => {
      const { slot, blockHash, blockNo, generation } =
        await readBoundary(scope);
      const pointId = `${slot}:${blockHash}`;
      const prior = views.get(scope);
      if (prior?.pointId === pointId && prior.generation !== generation)
        throw new Error("Retirement boundary view was rebuilt");
      views.set(scope, { pointId, generation });
      return { slot, blockHash, blockNo };
    },
    readCanonicalPoint: (point, boundary, scope) =>
      scope.read(() => reads.canonicalPoint(point, at(boundary, scope))),
    readSubmissionPoint: (txHash, boundary, scope) =>
      scope.read(() => reads.submissionPoint(txHash, at(boundary, scope))),
    readLandingPoint: (headerHash, boundary, scope) =>
      scope.read(() => reads.landingPoint(headerHash, at(boundary, scope))),
  };
};

/** The actual startup/service owner supplies operational pins. An absent
 * service never becomes an empty complete query. Cleanup also runs when full.
 * Canonical and submission points, and the raw snapshot, are read from the
 * committee follower's facts under the retirement boundary. */
export const committeePromiseRetirementRuntime = (args: {
  config: CommitteeL1ClientConfig;
  deployment: SDK.DaAvailabilityDeployment;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  actorId: string;
  lucid: Pick<LucidEvolution, "utxosAt">;
  reads: Pick<
    CommitteeAvailabilityReads,
    "canonicalPoint" | "submissionPoint" | "landingPoint"
  >;
  adoption: Awaited<ReturnType<typeof loadCommitteePromiseCausalAdoption>>;
  claims: ReturnType<typeof committeeClaimReconciliation>;
  scopes: ReturnType<typeof committeePromiseExecutionScopes>;
  ops: ReturnType<typeof availabilityResponderOperations>;
  assertIdle: () => void;
  joinStoreReads: () => Promise<void>;
}) => {
  let operationalPins: (() => readonly string[]) | undefined;
  const port = committeeRetirementSource({
    binding: committeeRetirementBinding(args.config, args.actorId),
    deployment: args.deployment,
    store: args.store,
    journal: args.journal,
    ...committeeFollowerRetirementReads(args.reads, (scope) =>
      args.ops.readBoundary(scope),
    ),
    slotTimeMs: args.adoption.slotTimeMs,
    readRawSnapshot: async (scope) => {
      const at = (address: string) =>
        scope.read(() => args.lucid.utxosAt(address));
      const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
        await committeePromiseJoinedReads([
          at(
            args.deployment.contracts.availabilityChallenge
              .spendingScriptAddress,
          ),
          at(args.deployment.contracts.stateQueue.spendingScriptAddress),
          at(args.deployment.contracts.correctionLock.spendingScriptAddress),
        ]);
      return { availabilityUtxos, stateQueueUtxos, correctionLockUtxos };
    },
    readOperationalPins: async (scope) => {
      scope.assertCurrent();
      args.assertIdle();
      if (!operationalPins)
        throw new Error("Retirement service pins are not bound");
      return operationalPins();
    },
    assertClaimsCurrent: async (boundary, scope, actor) => {
      // The receipt is bound to the follower view's generation as well.
      const { generation } = await args.ops.readBoundary(scope);
      await args.claims.assertCompatibleClaimsCurrent(
        {
          pointId: `${boundary.slot}:${boundary.blockHash}`,
          blockNo: boundary.blockNo,
          generation,
        },
        scope,
        actor,
      );
    },
    assertCurrent: async (scope) => {
      await args.ops.assertActuationCurrent(scope);
      await args.adoption.readProtocolDigest(scope);
      args.assertIdle();
      scope.assertCurrent();
    },
  });
  const compact = async () => {
    if (!operationalPins)
      throw new Error("Retirement service pins are not bound");
    const scope = args.scopes.open();
    try {
      args.assertIdle();
      await args.scopes.refresh(scope);
      if ((await args.claims.reconcile(scope)) !== "ready")
        throw new Error("Retirement financial reconciliation is not ready");
      await args.adoption.readProtocolDigest(scope);
      return await port.compact(scope);
    } finally {
      await args.joinStoreReads();
      scope.close();
    }
  };
  return {
    port,
    bindOperationalPins: (read: () => readonly string[]) => {
      operationalPins = read;
    },
    compact,
    /** The named retention reasons the last compaction held at. */
    holds: port.holds,
    growthReserve: async (scope?: SDK.DaAvailabilityReadScope) => {
      const floor = await args.store.getRetirementFloor();
      scope?.assertCurrent();
      if (!floor) throw new Error("Retirement singleton is not initialized");
      return retirementMetadataGrowthReserve(floor);
    },
  };
};
