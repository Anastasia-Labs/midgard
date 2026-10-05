import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { AvailabilityResponseLoopEnforcement } from "../availability-response-loop.js";
import type { CommitteeL1ClientConfig } from "../config.js";
import { readAvailabilityCursor } from "../l1/availability-cursor.js";
import { committeeOwnedAvailabilitySubmit } from "../l1/availability-owned-submit.js";
import { committeeScopedAttemptLucid } from "../l1/availability-scoped-lucid.js";
import { committeeScopedUtxos } from "../l1/availability-scoped-utxos.js";
import type { ChainSyncReplayProvider } from "../l1/provider.js";
import type { StateQueueProvider } from "../l1/state-queue-scanner.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../store.js";
import {
  committeeBoundReadContext,
  drainCommitteeReadResources,
  inheritCommitteeReadOwner,
} from "./committee-owned-read-transports.js";
import { createCommitteePromiseAdmissionSource } from "./create-promise-admission-source.js";
import {
  availabilityResponderCollateral,
  availabilityResponderL1ReadersFromConfig,
  availabilityResponderOperations,
} from "./factory.availability-responder-operations.js";
import {
  availabilityResponderTransactionOperation,
  discoverAvailabilityResponderChallenges,
} from "./factory.discover-availability-responder-challenges.js";
import { committeePromiseActorRuntime } from "./promise-actor-runtime.js";
import { committeeRunningPromiseBuildDigest } from "./promise-adoption-evidence.js";
import { loadCommitteePromiseCausalAdoption } from "./promise-causal-adoption.js";
import { committeeClaimReconciliation } from "./promise-claim-reconciliation.js";
import { committeePromiseExecutionScopes } from "./promise-execution-scopes.js";
import { committeePromiseStoreReadOwner } from "./promise-owned-read.js";
import { committeePromiseRetirementRuntime } from "./promise-retirement-runtime.js";
import type { CommitteePromiseRuntimePolicyAuthority } from "./promise-runtime-policy.js";
import { AvailabilityResponder } from "./responder.js";
import { assertAvailabilityResponderSourceHealthy } from "./source-authority.js";

const isReplayProvider = (
  provider: StateQueueProvider,
): provider is StateQueueProvider & ChainSyncReplayProvider =>
  [
    "refreshAvailabilityCursor",
    "currentChainSyncCursor",
    "loadConsumedChainSyncCursor",
    "acknowledgeChainSyncCursor",
    "replayChainSyncEvents",
  ].every((key) => typeof Reflect.get(provider, key) === "function");

/** Actual configured owner for the explicitly adopted private profile. No
 * artifact or fixture can substitute for installed cursor/transport/actor gates. */
export const configuredCommitteePromiseRuntime = async (input: {
  config: CommitteeL1ClientConfig;
  lucid: LucidEvolution;
  deployment: SDK.DaAvailabilityDeployment;
  actorId: string;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  chainProvider: StateQueueProvider;
  kupoUrl: string;
  ogmiosUrl: string;
}) => {
  const { config, lucid, deployment, journal, store, actorId, chainProvider } =
    input;
  if (!(store instanceof JsonFileCommitteeStore))
    throw new Error("Controlled admission requires the calibrated JSON store");
  if (!isReplayProvider(chainProvider))
    throw new Error(
      "Adopted profile requires bounded canonical cursor authority",
    );
  let generation: number | undefined;
  let authority: CommitteePromiseRuntimePolicyAuthority | undefined;
  let loopInstalled = false;
  const breach = (reason: string) => authority?.breach(reason);
  const limits = {
    requestRefusalMs: 10000,
    httpResponseBytes: 4194304,
    webSocketMessageBytes: 4194304,
    rawUtxos: 1024,
  };
  const scopes = committeePromiseExecutionScopes({
    provider: chainProvider,
    limits,
    breach,
  });
  const actorRuntime = committeePromiseActorRuntime(breach);
  const storeReads = committeePromiseStoreReadOwner();
  const currentCursor = async (scope?: SDK.DaAvailabilityReadScope) => {
    const owned = scope ?? scopes.open();
    try {
      const cursor = await readAvailabilityCursor(chainProvider, owned);
      generation = cursor.rollbackGeneration;
      return cursor;
    } finally {
      if (!scope) {
        await storeReads.join();
        await drainCommitteeReadResources(owned);
        owned.close();
      }
    }
  };
  const ops = availabilityResponderOperations({
    lucid,
    assertSourceHealthy: () =>
      storeReads.read(() =>
        assertAvailabilityResponderSourceHealthy(store, config),
      ),
    readers: availabilityResponderL1ReadersFromConfig({
      ...input,
      config,
      currentCursor,
      sourceReadLimits: limits,
    }),
    context: {
      deploymentIdentity: String(config.contractDeploymentInfo.manifestId),
      actor: actorId,
      journal,
      stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
      minimumConfirmationDepth: config.finalityDepth,
      transactionLimits: SDK.daAvailabilityOperationLimits(
        lucid,
        deployment.parameters,
      ),
      submit: (signedCbor) =>
        committeeOwnedAvailabilitySubmit({ ...input, signedCbor, breach }),
    },
  });
  const setup = scopes.open();
  let adoption: Awaited<ReturnType<typeof loadCommitteePromiseCausalAdoption>>;
  try {
    await scopes.refresh(setup);
    await currentCursor(setup);
    const runtimeBuildDigest = await committeeRunningPromiseBuildDigest();
    setup.assertCurrent();
    adoption = await loadCommitteePromiseCausalAdoption({
      ...input,
      storeBackend: store.constructor.name,
      runtimeBuildDigest,
      currentRollbackGeneration: () => {
        if (generation === undefined)
          throw new Error("Canonical rollback generation unavailable");
        return generation;
      },
      installedEnforcement: Object.entries({
        cursor: 5000,
        poll: 15000,
        scheduling_lag: 2000,
        source: 10000,
        build_sign_persist: 2000,
        submit: 3000,
      }).map(([stage, refusalCapMs]) => ({
        stage,
        refusalCapMs,
        implementationDigest: runtimeBuildDigest,
        mode:
          stage === "cursor" || stage === "source"
            ? ("abort_and_fence" as const)
            : ("late_result_fence" as const),
      })),
      scope: setup,
    });
    authority = adoption.authority;
  } finally {
    await storeReads.join();
    await drainCommitteeReadResources(setup);
    setup.close();
  }
  const claims = committeeClaimReconciliation({
    journal,
    actorId,
    deploymentIdentity: String(config.contractDeploymentInfo.manifestId),
    openReadScope: scopes.open,
    readBoundary: ops.readBoundary,
    currentCursor,
    reconcile: ops.reconcile,
    assertRuntimeIdle: actorRuntime.assertIdle,
    maximumRetainedRecords: 1024,
  });
  const readUtxos = committeeScopedUtxos({ kupoUrl: input.kupoUrl, limits });
  const walletAddress = await lucid.wallet().address();
  const scopedClients = new WeakMap<
    SDK.DaAvailabilityReadScope,
    Promise<LucidEvolution>
  >();
  const assertCollateralCurrent = async (
    scope: SDK.DaAvailabilityReadScope,
  ) => {
    let client = scopedClients.get(scope);
    if (!client) {
      client = committeeScopedAttemptLucid({
        ...input,
        original: lucid,
        scope,
        limits,
      });
      scopedClients.set(scope, client);
    }
    const attempt = await client;
    const metadata = journal.actorSnapshot(
      actorId,
      String(config.contractDeploymentInfo.manifestId),
    );
    const reserved = new Set(journal.reservedOutRefs(actorId));
    const normal = new Set(
      metadata.retainedAttempts.flatMap((item) => {
        const intent = journal.get(item.id)?.intent;
        if (!intent)
          throw new Error("Retained collateral claim identity disappeared");
        return intent.spentOutRefs.filter(
          (ref) =>
            reserved.has(ref) ||
            item.state === "included" ||
            item.state === "confirmed",
        );
      }),
    );
    const collateral = await availabilityResponderCollateral(
      {
        config: attempt.config,
        wallet: () => ({ address: async () => walletAddress }),
        utxosAt: async (address) => {
          if (typeof address !== "string")
            throw new Error("Collateral requires the exact actor address");
          return (await readUtxos(address, scope)).filter(
            (row) => !normal.has(`${row.txHash}#${row.outputIndex}`),
          );
        },
      },
      [
        deployment.parameters.max_publication_fee_lovelace,
        deployment.parameters.max_settlement_fee_lovelace,
        deployment.parameters.max_close_fee_lovelace,
      ].reduce((max, fee) => (fee > max ? fee : max)),
    );
    if (collateral.length !== 1)
      throw new Error("Exact compatible live collateral is unavailable");
    scope.assertCurrent();
  };
  const retirement = committeePromiseRetirementRuntime({
    ...input,
    adoption,
    claims,
    scopes,
    ops,
    readUtxos,
    assertIdle: actorRuntime.assertIdle,
    joinStoreReads: storeReads.join,
  });
  const source = createCommitteePromiseAdmissionSource({
    ...input,
    actorId,
    retirementPort: retirement.port,
    retirementGrowthReserve: retirement.growthReserve,
    currentCursor,
    readBoundary: ops.readBoundary,
    assertActuationCurrent: ops.assertActuationCurrent,
    policyAuthority: authority,
    openReadScope: scopes.open,
    sourceReadLimits: limits,
    currentSchedulingEnabled: true,
    scopedReadUtxos: readUtxos,
    walletAddress,
    readProtocolDigest: adoption.readProtocolDigest,
    assertCollateralCurrent,
    assertCompatibleClaimsCurrent: claims.assertCompatibleClaimsCurrent,
    assertActorRuntimeIdle: actorRuntime.assertIdle,
    drainReadResources: async (scope) => {
      if (!scope) throw new Error("Owned admission read scope is unavailable");
      await storeReads.join();
      await drainCommitteeReadResources(scope);
      scopes.assertDrained();
    },
    prepareCanonicalClaims: async (scope) => {
      if (!loopInstalled)
        throw new Error("Adopted responder loop is not installed");
      await scopes.refresh(scope);
      const result = await claims.reconcile(scope);
      if (result !== "ready")
        throw new Error("Canonical retained claims remain unresolved");
    },
    sourceResourceLimits: {
      storeRecords: 512,
      storeEncodedBytes: 8388608,
      journalEntries: 1024,
    },
  });
  const promiseLoopEnforcement: AvailabilityResponseLoopEnforcement = {
    pollIntervalCapMs: 15000,
    installed: () => {
      loopInstalled = true;
    },
    scheduled: (expected) => {
      if (performance.now() - expected > 2000)
        breach("response_scheduling_lag_exceeded");
    },
  };
  const responder = new AvailabilityResponder({
    deploymentFingerprint: config.deploymentFingerprint,
    deploymentIdentity: deployment.hubOraclePolicyId,
    store,
    reconcile: async () => {
      const scope = scopes.open();
      try {
        await scopes.refresh(scope);
        return await ops.reconcile(scope);
      } finally {
        await storeReads.join();
        await drainCommitteeReadResources(scope);
        scope.close();
      }
    },
    discover: async () => {
      const scope = scopes.open();
      try {
        await scopes.refresh(scope);
        await ops.assertActuationCurrent(scope);
        const before = await ops.readBoundary(scope);
        let complete = true;
        const found = await discoverAvailabilityResponderChallenges(
          lucid,
          deployment,
          () => {
            complete = false;
          },
          { scope, readUtxos },
        );
        const after = await ops.readBoundary(scope);
        scope.assertCurrent();
        if (!complete || before.pointId !== after.pointId)
          throw new Error(
            "Complete response discovery changed or was unavailable",
          );
        return found;
      } finally {
        await storeReads.join();
        await drainCommitteeReadResources(scope);
        scope.close();
      }
    },
    execute: async (action) => {
      const failed = journal
        .actorSnapshot(
          actorId,
          String(config.contractDeploymentInfo.manifestId),
        )
        .retainedAttempts.filter(
          (row) =>
            row.headerHash ===
              action.challenge.record.datum.commitment.header_hash &&
            row.state === "expired",
        );
      const operation = availabilityResponderTransactionOperation(
        lucid,
        deployment,
        action,
      );
      const scope = scopes.open(operation.unsignedDeadlineMs);
      let builtTtl: number | undefined;
      let freshBody = true;
      let buildStageStart: number | undefined;
      let buildStageTimer: ReturnType<typeof setTimeout> | undefined;
      const finishBuildStage = () => {
        if (buildStageTimer) clearTimeout(buildStageTimer);
        if (
          buildStageStart !== undefined &&
          performance.now() - buildStageStart > 2000
        )
          breach("build_sign_persist_budget_exceeded");
        buildStageStart = undefined;
      };
      const context = {
        ...committeeBoundReadContext(ops.context, scope),
        submit: async (signedCbor: string) => {
          finishBuildStage();
          freshBody = false;
          return ops.context.submit(signedCbor);
        },
        observationSignal: scope.signal,
        observationTimeoutMs: Math.max(1, Math.ceil(scope.remainingMs())),
        assertActuationCurrent: async (child?: SDK.DaAvailabilityReadScope) => {
          inheritCommitteeReadOwner(scope, child);
          await ops.assertActuationCurrent(child ?? scope);
          if (
            buildStageStart !== undefined &&
            performance.now() - buildStageStart >= 2000
          )
            throw new Error(
              "Build/sign/persist stage exceeded its adopted allowance",
            );
          if (
            freshBody &&
            builtTtl !== undefined &&
            adoption.clock.eligibleFutureSlots(builtTtl, 3000) < 50
          )
            throw new Error(
              "Actual signed-body TTL has fewer than fifty future eligible slots",
            );
        },
      };
      try {
        await scopes.refresh(scope);
        const result = await SDK.runDaAvailabilityOperation(context, {
          ...operation,
          preparationScope: scope,
          build: async (_signal, shared) => {
            if (failed.length >= 5) {
              breach("aggregate_response_retry_budget_exhausted");
              throw new Error(
                "Aggregate response retry budget exhausted; signed evidence is retained",
              );
            }

            buildStageStart = performance.now();
            buildStageTimer = setTimeout(
              () => breach("build_sign_persist_budget_exceeded"),
              2000,
            );
            buildStageTimer.unref();
            const builderScope = SDK.createDaAvailabilityReadScope({
              attemptTimeoutMs: Math.max(
                1,
                Math.ceil(Math.min(2000, (shared ?? scope).remainingMs())),
              ),
              signal: (shared ?? scope).signal,
            });
            inheritCommitteeReadOwner(scope, builderScope);
            try {
              return await actorRuntime.trackUnsigned(
                builderScope,
                async () => {
                  try {
                    const attempt = await committeeScopedAttemptLucid({
                      ...input,
                      original: lucid,
                      scope: builderScope,
                      limits,
                    });
                    const tx = await availabilityResponderTransactionOperation(
                      attempt,
                      deployment,
                      action,
                    ).build(builderScope.signal, builderScope);
                    const signedBuilder = "toTransaction" in tx ? tx : tx.tx;
                    const ttl = signedBuilder.toTransaction().body().ttl();
                    if (ttl === undefined)
                      throw new Error("Built response has no exclusive TTL");
                    builtTtl = Number(ttl);
                    if (
                      !Number.isSafeInteger(builtTtl) ||
                      adoption.clock.eligibleFutureSlots(builtTtl, 3000) < 50
                    )
                      throw new Error(
                        "Built response TTL has insufficient future slots",
                      );
                    return tx;
                  } finally {
                    await storeReads.join();
                    await drainCommitteeReadResources(builderScope);
                  }
                },
              );
            } finally {
              builderScope.close();
            }
          },
        });
        if (result.status === "conflict")
          throw new Error("Response conflicts with canonical history");
        return result.status === "included" || result.status === "confirmed"
          ? result.status
          : "pending";
      } finally {
        finishBuildStage();
        await actorRuntime.join();
        await storeReads.join();
        await drainCommitteeReadResources(scope);
        scope.close();
      }
    },
  });
  return {
    responder,
    promiseAdmissionSource: source,
    promiseLoopEnforcement,
    bindRetirementOperationalPins: retirement.bindOperationalPins,
    compactRetainedPromises: retirement.compact,
    close: () => journal.close(),
  };
};
