import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { CommitteeL1ClientConfig } from "../config.js";
import { promiseCanonicalPointReader } from "../l1/promise-capacity-point.js";
import type { ChainSyncCursor } from "../l1/provider.js";
import type { CommitteeStore } from "../store.js";
import { terminalRecoveryFinal } from "../store/retention.js";
import type { CommitteeRetirementPort } from "../store/retirement-model.js";
import type { availabilityResponderOperations } from "./factory.availability-responder-operations.js";
import { discoverAvailabilityResponderChallenges } from "./factory.discover-availability-responder-challenges.js";
import { promiseAdmissionActorMetadata } from "./promise-actor-metadata.js";
import type { CommitteePromiseAdmissionSource } from "./promise-admission.js";
import type { PromiseSchedulingLiability } from "./promise-current-scheduling.js";
import { retiredPromiseCutoffs } from "./promise-cutoff-source.js";
import { committeePromiseLiveProtocol } from "./promise-live-protocol.js";
import {
  committeePromiseJoinedReads,
  committeePromiseOwnedRead,
} from "./promise-owned-read.js";
import { committeePromiseRetirementAdmission } from "./promise-retirement-admission.js";
import type { CommitteePromiseRuntimePolicyAuthority } from "./promise-runtime-policy.js";
import { promiseSchedulingSource } from "./promise-scheduling-source.js";
import { promiseSignatureResourceReserve } from "./promise-signature-resource-reserve.js";
import {
  committeeScopedWebSocketFactory,
  type CommitteeSourceReadLimits,
} from "./scoped-transports.js";

/** Production source construction, shared by the configured factory and probes. */
export const createCommitteePromiseAdmissionSource = (args: {
  config: CommitteeL1ClientConfig;
  deployment: SDK.DaAvailabilityDeployment;
  actorId: string;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  lucid: LucidEvolution;
  ogmiosUrl: string;
  currentCursor: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<ChainSyncCursor>;
  readBoundary: ReturnType<
    typeof availabilityResponderOperations
  >["readBoundary"];
  assertActuationCurrent: ReturnType<
    typeof availabilityResponderOperations
  >["assertActuationCurrent"];
  policyAuthority?: CommitteePromiseRuntimePolicyAuthority;
  retirementPort?: CommitteeRetirementPort;
  retirementGrowthReserve?: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<number>;
  openReadScope?: () => SDK.DaAvailabilityReadScope;
  sourceReadLimits?: CommitteeSourceReadLimits;
  /** Explicit adopted dual-set model; never inferred from elapsed wall time. */
  currentSchedulingEnabled?: boolean;
  scopedReadUtxos?: (
    address: string,
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<UTxO[]>;
  walletAddress?: string;
  readProtocolDigest?: (scope: SDK.DaAvailabilityReadScope) => Promise<string>;
  drainReadResources?: (scope?: SDK.DaAvailabilityReadScope) => Promise<void>;
  assertCollateralCurrent?: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<void>;
  /** Fresh real reconciliation must cover compatible retained claims at this boundary. */
  assertCompatibleClaimsCurrent?: (
    boundary: Readonly<{ pointId: string; blockNo: number }>,
    scope?: SDK.DaAvailabilityReadScope,
    metadata?: ReturnType<AvailabilityOperationJournal["actorSnapshot"]>,
  ) => Promise<void>;
  /** Installed runtime retains timed-out unsigned work until its callback settles. */
  assertActorRuntimeIdle?: () => void;
  /** Real SDK reconciliation shares this source scope and owns its awaited writes. */
  prepareCanonicalClaims?: (
    scope: SDK.DaAvailabilityReadScope,
  ) => Promise<void>;
  sourceResourceLimits?: Readonly<{
    storeRecords: number;
    storeEncodedBytes: number;
    journalEntries: number;
  }>;
}): CommitteePromiseAdmissionSource => {
  const {
    config,
    deployment,
    actorId,
    store,
    journal,
    lucid,
    ogmiosUrl,
    currentCursor,
    readBoundary,
    assertActuationCurrent,
    sourceReadLimits,
    scopedReadUtxos,
  } = args;
  const contractManifestId = config.contractDeploymentInfo.manifestId;
  if (typeof contractManifestId !== "string")
    throw new Error("Promise source manifest identity is unavailable");
  const schedulingSource = args.currentSchedulingEnabled
    ? promiseSchedulingSource({
        lucid,
        deployment,
        ogmiosUrl,
        currentCursor,
        readBoundary,
        assertActuationCurrent,
        limits: sourceReadLimits,
        readUtxos: scopedReadUtxos,
        openWindowMs: SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
      })
    : undefined;
  const assertProtocol = committeePromiseLiveProtocol(args);
  const retirement = committeePromiseRetirementAdmission(args.retirementPort);
  const assertActorRuntimeIdle = () => {
    if (
      args.policyAuthority !== undefined &&
      args.assertActorRuntimeIdle === undefined
    )
      throw new Error("Actor runtime drainage authority is unavailable");
    args.assertActorRuntimeIdle?.();
  };
  const { busy: actorBusy, snapshot: actorSnapshot } =
    promiseAdmissionActorMetadata({
      journal,
      actorId,
      deploymentIdentity: contractManifestId,
      maximumRetainedRecords: args.sourceResourceLimits?.journalEntries,
      hasCanonicalCompatibilityAuthority:
        args.assertCompatibleClaimsCurrent !== undefined,
    });
  const readStoreUsage = async (scope?: SDK.DaAvailabilityReadScope) => {
    const usage = await committeePromiseOwnedRead(scope)(() =>
      store.promiseStoreResourceUsage(args.sourceResourceLimits),
    );
    const reserve = (await args.retirementGrowthReserve?.(scope)) ?? 0;
    scope?.assertCurrent();
    return { ...usage, storeEncodedBytes: usage.storeEncodedBytes + reserve };
  };
  const readResourceWorkload = async (scope?: SDK.DaAvailabilityReadScope) => {
    assertActorRuntimeIdle();
    const read = committeePromiseOwnedRead(scope);
    const actor = actorSnapshot();
    if (
      args.sourceResourceLimits &&
      actor.retainedRecordCount > args.sourceResourceLimits.journalEntries
    )
      throw new Error("Journal exceeds the adopted retained-row domain");
    const [usage, wallet] = await committeePromiseJoinedReads([
      readStoreUsage(scope),
      read(() =>
        scope && scopedReadUtxos && args.walletAddress
          ? scopedReadUtxos(args.walletAddress, scope)
          : lucid.wallet().getUtxos(),
      ),
    ]);
    scope?.assertCurrent();
    assertActorRuntimeIdle();
    if (actorSnapshot().stateDigest !== actor.stateDigest)
      throw new Error("Actor journal changed during resource discovery");
    return {
      ...usage,
      journalEntries: actor.retainedRecordCount,
      walletInputs: wallet.length,
    };
  };
  return {
    readResourceWorkload,
    policyAuthority: args.policyAuthority,
    openReadScope: args.openReadScope,
    drainReadResources: args.drainReadResources,
    ...(args.policyAuthority === undefined
      ? {}
      : {
          projectResources: async (candidate, scope) => {
            // Successful serialization reserve is read-only; no signature or outbox is created.
            const reserve = await committeePromiseOwnedRead(scope)(() =>
              promiseSignatureResourceReserve({ candidate, config, store }),
            );
            scope?.assertCurrent();
            return reserve;
          },
        }),

    readSnapshot: async (scope) => {
      assertActorRuntimeIdle();
      actorSnapshot();
      if (args.prepareCanonicalClaims) {
        if (!scope)
          throw new Error("Canonical claims require the shared source scope");
        // Durable SDK mutations finish before capturing the actor identity.
        await args.prepareCanonicalClaims(scope);
        scope.assertCurrent();
      }
      const read = committeePromiseOwnedRead(scope);
      const assertResources = async () => {
        const usage = await readStoreUsage(scope);
        const journalEntries = journal.retainedRecordCount();
        if (
          args.sourceResourceLimits &&
          journalEntries > args.sourceResourceLimits.journalEntries
        )
          throw new Error("Journal exceeds the adopted retained-row domain");
        scope?.assertCurrent();
        return usage;
      };
      const actorBefore = actorSnapshot();
      await assertResources();
      await assertActuationCurrent(scope);
      const retirementGuard = await retirement.capture(scope);
      const cursorBefore = await read(() => currentCursor(scope));
      const before = await readBoundary(scope);
      if (actorBefore.reservedResourceCount > 0 && !actorBusy(actorBefore))
        await args.assertCompatibleClaimsCurrent!(before, scope, actorBefore);
      await assertProtocol(scope);
      let complete = true;
      let rawSnapshot: SDK.DaAvailabilitySnapshotUtxos | undefined;
      const challenges = await discoverAvailabilityResponderChallenges(
        lucid,
        deployment,
        () => {
          complete = false;
        },
        scope === undefined
          ? undefined
          : {
              scope,
              readUtxos: scopedReadUtxos,
              onSnapshot: (snapshot) => {
                rawSnapshot = snapshot;
              },
            },
      );
      const liabilities = [];
      const schedulingLiabilities: PromiseSchedulingLiability[] = [];
      // Durable certificate writes below are awaited without racing cancellation.
      for (const signature of await read(() => store.listDaSignatures())) {
        scope?.assertCurrent();
        if (signature.signerIndex !== config.signerIndex) continue;
        const header = await read(() =>
          store.getStateQueueHeader(signature.headerHash),
        );
        if (
          header === undefined ||
          header.deploymentFingerprint !== config.deploymentFingerprint ||
          signature.deploymentFingerprint !== config.deploymentFingerprint
        )
          throw new Error("Signed header cutoff evidence is unavailable");
        const commitment = SDK.parseDaAvailabilityCommitmentCbor(
          signature.availabilityCommitmentCbor,
          SDK.availabilityResponseGeometry(
            config.availabilityChallenge.responseGeometry,
          ),
        );
        const digest = computeDaSha256Hash(
          Buffer.from(SDK.encodeDaAvailabilityCommitment(commitment), "hex"),
        ).toString("hex");
        if (
          digest !== signature.availabilityCommitmentDigest ||
          commitment.header_hash !== header.headerHash ||
          commitment.deployment_identity !== config.hubOraclePolicyId
        )
          throw new Error("Cutoff commitment identity mismatch");
        schedulingLiabilities.push({
          header,
          commitment,
          commitmentDigest: digest,
        });
        const cutoff =
          header.header.endTime +
          BigInt(SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms);
        if (cutoff < 0n || cutoff > BigInt(Number.MAX_SAFE_INTEGER))
          throw new Error("Promise cutoff time is out of range");
        liabilities.push({
          headerHash: header.headerHash,
          commitmentDigest: digest,
          cutoffTimeMs: Number(cutoff),
          ...(terminalRecoveryFinal(
            header,
            {
              automaticRecoveryMaxDepth: config.automaticRecoveryMaxDepth,
              deploymentFingerprint: config.deploymentFingerprint,
            },
            signature.deploymentFingerprint,
          )
            ? {
                terminalPoint: {
                  slot: header.observedChainPoint.slot!,
                  blockHash: header.observedChainPoint.blockHash!,
                  blockNo: header.observedChainPoint.blockHeight!,
                },
              }
            : {}),
          hasActiveChallenge: challenges.some(
            (item) =>
              computeDaSha256Hash(
                Buffer.from(
                  SDK.encodeDaAvailabilityCommitment(
                    item.record.datum.commitment,
                  ),
                  "hex",
                ),
              ).toString("hex") === digest,
          ),
        });
      }
      const retiredCommitmentDigests =
        complete && !actorBusy(actorBefore)
          ? await retiredPromiseCutoffs({
              store,
              deploymentFingerprint: config.deploymentFingerprint,
              contractManifestId,
              actorId: actorId,
              recoveryDepth: config.automaticRecoveryMaxDepth,
              boundary: {
                slot: before.slot,
                blockHash: before.blockHash,
                blockNo: before.blockNo,
              },
              canonicalTimeMs: lucid.slotToUnixTime(before.slot),
              slotTimeMs: (slot) => lucid.slotToUnixTime(slot),
              liabilities,
              readCanonicalPoint: (point) =>
                read(() =>
                  promiseCanonicalPointReader(
                    ogmiosUrl,
                    scope && sourceReadLimits
                      ? committeeScopedWebSocketFactory(scope, sourceReadLimits)
                      : undefined,
                    {
                      slot: before.slot,
                      blockHash: before.blockHash,
                      blockNo: before.blockNo,
                    },
                  )(point),
                ),
              assertCurrent: async () => {
                await assertActuationCurrent(scope);
                const current = await readBoundary(scope);
                const cursor = await read(() => currentCursor(scope));
                if (
                  current.pointId !== before.pointId ||
                  current.blockNo !== before.blockNo ||
                  cursor.rollbackGeneration !==
                    cursorBefore.rollbackGeneration ||
                  cursor.sequence !== cursorBefore.sequence
                )
                  throw new Error(
                    "Canonical generation changed during capacity retirement proof",
                  );
              },
            })
          : new Set<string>();
      const scheduling =
        schedulingSource && complete && !actorBusy(actorBefore)
          ? await schedulingSource.capture({
              rawSnapshot,
              complete,
              scope,
              cursor: cursorBefore,
              point: {
                slot: before.slot,
                blockHash: before.blockHash,
                blockNo: before.blockNo,
              },
              liabilities: schedulingLiabilities.filter(
                (row) => !retiredCommitmentDigests.has(row.commitmentDigest),
              ),
            })
          : undefined;
      const after = await readBoundary(scope);
      const cursorAfter = await read(() => currentCursor(scope));
      if (
        before.pointId !== after.pointId ||
        cursorBefore.sequence !== cursorAfter.sequence ||
        cursorBefore.rollbackGeneration !== cursorAfter.rollbackGeneration
      )
        throw new Error("Canonical source changed during promise discovery");
      const actorAfter = actorSnapshot();
      if (actorAfter.reservedResourceCount > 0 && !actorBusy(actorAfter))
        await args.assertCompatibleClaimsCurrent!(after, scope, actorAfter);
      const [walletInputs, storeUsage] = await committeePromiseJoinedReads([
        read(() =>
          scope && scopedReadUtxos && args.walletAddress
            ? scopedReadUtxos(args.walletAddress, scope)
            : lucid.wallet().getUtxos(),
        ),
        assertResources(),
      ]);
      await assertActuationCurrent(scope);
      await retirement.prove(retirementGuard, scope);
      await args.drainReadResources?.(scope);
      scope?.assertCurrent();
      // No await follows this exact journal fence before returning the receipt.
      assertActorRuntimeIdle();
      const actorFinal = actorSnapshot();
      const unresolved =
        actorBusy(actorBefore) ||
        actorBusy(actorAfter) ||
        actorBusy(actorFinal) ||
        actorBefore.stateDigest !== actorAfter.stateDigest ||
        actorAfter.stateDigest !== actorFinal.stateDigest;
      return {
        boundary: {
          pointId: before.pointId,
          rollbackGeneration: cursorAfter.rollbackGeneration,
          observedAtMs: Date.now(),
          actorStateDigest: actorFinal.stateDigest,
          retirementGuard,
          schedulingEvidenceDigest: scheduling?.digest,
        },
        canonicalTimeMs: lucid.slotToUnixTime(before.slot),
        retiredCommitmentDigests,
        currentSchedulingCommitmentDigests: scheduling?.current,
        challenges,
        retainedAttempts: actorFinal.retainedAttempts,
        resourceWorkload: {
          walletInputs: walletInputs.length,
          journalEntries: actorFinal.retainedRecordCount,
          challengeRecords: challenges.length,
          ...storeUsage,
        },
        complete,
        blocking: unresolved
          ? {
              kind: "unresolved",
              reason: "durable_actor_lease_intents_or_resources_unresolved",
            }
          : { kind: "bounded", remainingMs: 0 },
      };
    },
    assertCurrent: async (boundary, scope, candidate) => {
      assertActorRuntimeIdle();
      const read = committeePromiseOwnedRead(scope);
      const actorBefore = actorSnapshot();
      if (
        actorBusy(actorBefore) ||
        actorBefore.stateDigest !== boundary.actorStateDigest
      )
        throw new Error(
          "Actor lease or durable resources changed before signing",
        );
      await readStoreUsage(scope);
      if (
        args.sourceResourceLimits &&
        journal.retainedRecordCount() > args.sourceResourceLimits.journalEntries
      )
        throw new Error("Journal grew beyond the adopted retained-row domain");
      await assertActuationCurrent(scope);
      const point = await readBoundary(scope);
      await assertProtocol(scope);
      const metadata = actorSnapshot();
      if (actorBusy(metadata))
        throw new Error(
          "Actor lease or durable resources changed before signing",
        );
      if (metadata.reservedResourceCount > 0)
        await args.assertCompatibleClaimsCurrent!(point, scope, metadata);
      if (schedulingSource)
        await schedulingSource.assertCurrent(
          boundary.schedulingEvidenceDigest,
          scope,
        );
      const assertRetirement = await retirement.prove(
        boundary.retirementGuard,
        scope,
        candidate?.record,
      );
      const cursor = await read(() => currentCursor(scope));
      if (
        point.pointId !== boundary.pointId ||
        cursor.rollbackGeneration !== boundary.rollbackGeneration
      )
        throw new Error("Promise admission canonical boundary changed");
      await args.drainReadResources?.(scope);
      scope?.assertCurrent();
      // All asynchronous source and compatibility reads finish before this fence.
      assertActorRuntimeIdle();
      const finalMetadata = actorSnapshot();
      if (
        actorBusy(finalMetadata) ||
        finalMetadata.stateDigest !== boundary.actorStateDigest
      )
        throw new Error(
          "Actor lease or durable resources changed before signing",
        );
      assertRetirement();
    },
  };
};
