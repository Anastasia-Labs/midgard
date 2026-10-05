import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import { readCommitteeRetirementPoint } from "../l1/retirement-canonical-point.js";
import { readCommitteeRetirementSubmission } from "../l1/retirement-submission-point.js";
import {
  type CommitteeStore,
  retirementMetadataGrowthReserve,
} from "../store.js";
import { committeeRetirementSource } from "../store/retirement-source.js";
import { drainCommitteeReadResources } from "./committee-owned-read-transports.js";
import type { availabilityResponderOperations } from "./factory.availability-responder-operations.js";
import type { loadCommitteePromiseCausalAdoption } from "./promise-causal-adoption.js";
import type { committeeClaimReconciliation } from "./promise-claim-reconciliation.js";
import type { committeePromiseExecutionScopes } from "./promise-execution-scopes.js";
import { committeePromiseJoinedReads } from "./promise-owned-read.js";
import { committeeScopedWebSocketFactory } from "./scoped-transports.js";

/** The actual startup/service owner supplies operational pins. An absent
 * service never becomes an empty complete query. Cleanup also runs when full. */
export const committeePromiseRetirementRuntime = (args: {
  config: CommitteeL1ClientConfig;
  deployment: SDK.DaAvailabilityDeployment;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  actorId: string;
  kupoUrl: string;
  ogmiosUrl: string;
  adoption: Awaited<ReturnType<typeof loadCommitteePromiseCausalAdoption>>;
  claims: ReturnType<typeof committeeClaimReconciliation>;
  scopes: ReturnType<typeof committeePromiseExecutionScopes>;
  ops: ReturnType<typeof availabilityResponderOperations>;
  readUtxos: (
    address: string,
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<UTxO[]>;
  assertIdle: () => void;
  joinStoreReads: () => Promise<void>;
}) => {
  let operationalPins: (() => readonly string[]) | undefined;
  const limits = args.adoption.limits;
  const port = committeeRetirementSource({
    binding: {
      deploymentFingerprint: args.config.deploymentFingerprint,
      manifestSha256: args.config.deploymentManifestSha256,
      contractManifestId: String(args.config.contractDeploymentInfo.manifestId),
      committeeSignersHash: args.config.daParams.committeeSignersHash,
      actorId: args.actorId,
      sourceAuthoritySha256: l1SourceAuthorityDigest(
        args.config.network,
        args.config.l1Source,
      ),
      peerIds: args.config.daTransport.peers.map((peer) => peer.peerId),
      retentionDays: args.config.daTransport.retentionDays,
      recoveryDepth: args.config.automaticRecoveryMaxDepth,
      maximumRecords: 512,
      maximumEncodedBytes: 8388608,
    },
    deployment: args.deployment,
    store: args.store,
    journal: args.journal,
    readBoundary: async (scope) => {
      const { slot, blockHash, blockNo } = await args.ops.readBoundary(scope);
      return { slot, blockHash, blockNo };
    },
    slotTimeMs: args.adoption.slotTimeMs,
    readRawSnapshot: async (scope) => {
      const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
        await committeePromiseJoinedReads([
          args.readUtxos(
            args.deployment.contracts.availabilityChallenge
              .spendingScriptAddress,
            scope,
          ),
          args.readUtxos(
            args.deployment.contracts.stateQueue.spendingScriptAddress,
            scope,
          ),
          args.readUtxos(
            args.deployment.contracts.correctionLock.spendingScriptAddress,
            scope,
          ),
        ]);
      return { availabilityUtxos, stateQueueUtxos, correctionLockUtxos };
    },
    readCanonicalPoint: (point, boundary, scope) =>
      readCommitteeRetirementPoint({
        ogmiosUrl: args.ogmiosUrl,
        point,
        boundary,
        scope,
        limits,
        webSocketFactory: committeeScopedWebSocketFactory(scope, limits),
      }),
    readSubmissionPoint: (txHash, boundary, scope) =>
      readCommitteeRetirementSubmission({
        kupoUrl: args.kupoUrl,
        ogmiosUrl: args.ogmiosUrl,
        txHash,
        boundary,
        scope,
        limits,
      }),
    readOperationalPins: async (scope) => {
      scope.assertCurrent();
      args.assertIdle();
      if (!operationalPins)
        throw new Error("Retirement service pins are not bound");
      return operationalPins();
    },
    assertClaimsCurrent: (boundary, scope, actor) =>
      args.claims.assertCompatibleClaimsCurrent(
        {
          pointId: `${boundary.slot}:${boundary.blockHash}`,
          blockNo: boundary.blockNo,
        },
        scope,
        actor,
      ),
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
      await drainCommitteeReadResources(scope);
      scope.close();
    }
  };
  return {
    port,
    bindOperationalPins: (read: () => readonly string[]) => {
      operationalPins = read;
    },
    compact,
    growthReserve: async (scope?: SDK.DaAvailabilityReadScope) => {
      const floor = await args.store.getRetirementFloor();
      scope?.assertCurrent();
      if (!floor) throw new Error("Retirement singleton is not initialized");
      return retirementMetadataGrowthReserve(floor);
    },
  };
};
