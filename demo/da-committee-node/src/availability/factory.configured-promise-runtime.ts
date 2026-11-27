import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk/da";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { AvailabilityResponseLoopEnforcement } from "../availability-response-loop.js";
import type { CommitteeL1ClientConfig } from "../config.js";
import type { CommitteeAvailabilityReads } from "../l1/follower/availability-reads.js";
import {
  type CommitteeL1Clock,
  committeeL1NowMs,
} from "../l1/follower/l1-clock.js";
import type { CommitteeStore } from "../store.js";
import { createCommitteePromiseAdmissionSource } from "./create-promise-admission-source.js";
import {
  availabilityResponderCollateral,
  availabilityResponderOperations,
  committeeBoundReadContext,
} from "./factory.availability-responder-operations.js";
import {
  availabilityResponderPublicationDeadlineMs,
  availabilityResponderTransactionOperation,
  discoverAvailabilityResponderChallenges,
} from "./factory.discover-availability-responder-challenges.js";
import { committeePromiseActorRuntime } from "./promise-actor-runtime.js";
import { committeeRunningPromiseBuildDigest } from "./promise-adoption-evidence.js";
import {
  type CommitteePromiseLedger,
  loadCommitteePromiseCausalAdoption,
} from "./promise-causal-adoption.js";
import { committeeClaimReconciliation } from "./promise-claim-reconciliation.js";
import {
  committeePromiseExecutionScopes,
  committeePromiseOwnedStage,
} from "./promise-execution-scopes.js";
import { committeePromiseStoreReadOwner } from "./promise-owned-read.js";
import {
  PROMISE_RESPONSE_BUILD_STAGE_MS,
  PROMISE_RESPONSE_PROPAGATION_MS,
  promiseResponseBuildTime,
  promiseResponseTtl,
} from "./promise-response-build.js";
import { committeePromiseRetirementRuntime } from "./promise-retirement-runtime.js";
import type { CommitteePromiseRuntimePolicyAuthority } from "./promise-runtime-policy.js";
import { AvailabilityResponder } from "./responder.js";
import { assertAvailabilityResponderSourceHealthy } from "./source-authority.js";

/** Actual configured owner for the explicitly adopted private profile. No
 * artifact or fixture can substitute for installed cursor/source/actor gates.
 * Every L1 read is the committee follower's (its view bounds each stage) or
 * the node's ledger state; every submit is the node's LocalTxSubmission. */
export const configuredCommitteePromiseRuntime = async (input: {
  config: CommitteeL1ClientConfig;
  lucid: LucidEvolution;
  reads: CommitteeAvailabilityReads;
  /** The node's protocol parameters and slot configuration, by local state query. */
  ledger: Omit<CommitteePromiseLedger, "viewValid">;
  deployment: SDK.DaAvailabilityDeployment;
  actorId: string;
  store: CommitteeStore;
  journal: AvailabilityOperationJournal;
  /** The committee's L1 clock: every response validity and deadline
   * decision, and the response read scope's deadline, are taken on it. */
  l1Clock: CommitteeL1Clock;
}) => {
  const { config, lucid, reads, deployment, journal, store, actorId, l1Clock } =
    input;
  let authority: CommitteePromiseRuntimePolicyAuthority | undefined;
  let loopInstalled = false;
  const breach = (reason: string) => authority?.breach(reason);
  const scopes = committeePromiseExecutionScopes({
    readCursor: () => reads.readBoundary(),
    breach,
    nowMs: () => committeeL1NowMs(l1Clock),
  });
  const actorRuntime = committeePromiseActorRuntime(breach);
  const storeReads = committeePromiseStoreReadOwner();
  const provider = lucid.config().provider;
  if (provider === undefined)
    throw new Error("Adopted responder has no transaction provider");
  /** A refusal leaves the SDK's exact signed intent unresolved; nothing here
   * releases it or supplies replacement bytes. */
  const submit = (signedCbor: string): Promise<string> => {
    if (
      !journal
        .pending(config.hubOraclePolicyId, actorId)
        .some((record) => record.intent.signedCbor === signedCbor)
    )
      throw new Error(
        "Owned submit requires the exact persisted pending bytes",
      );
    return committeePromiseOwnedStage({
      stage: "submit",
      capMs: 3000,
      breach,
      run: () => provider.submitTx(signedCbor),
    });
  };
  const ops = availabilityResponderOperations({
    lucid,
    reads,
    assertSourceHealthy: () =>
      storeReads.read(() =>
        assertAvailabilityResponderSourceHealthy(store, config),
      ),
    context: {
      hubOraclePolicyId: config.hubOraclePolicyId,
      actor: actorId,
      journal,
      stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
      minimumConfirmationDepth: config.finalityDepth,
      transactionLimits: SDK.daAvailabilityOperationLimits(
        lucid,
        deployment.parameters,
      ),
      submit: async (signedCbor) => submit(signedCbor),
    },
    l1Clock,
  });
  const setup = scopes.open();
  let adoption: Awaited<ReturnType<typeof loadCommitteePromiseCausalAdoption>>;
  try {
    await scopes.refresh(setup);
    const runtimeBuildDigest = await committeeRunningPromiseBuildDigest();
    setup.assertCurrent();
    adoption = await loadCommitteePromiseCausalAdoption({
      config,
      actorId,
      storeBackend: store.constructor.name,
      runtimeBuildDigest,
      ledger: { ...input.ledger, viewValid: reads.viewValid },
      installedEnforcement: Object.entries({
        cursor: 5000,
        poll: 15000,
        scheduling_lag: 2000,
        source: 10000,
        build_sign_persist: PROMISE_RESPONSE_BUILD_STAGE_MS,
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
    setup.close();
  }
  // The adopted calibration's TTL rule, read by the build and both TTL checks.
  const ttlRule = adoption.authority.causal;
  if (!ttlRule) throw new Error("Adopted causal policy has no TTL rule");
  const claims = committeeClaimReconciliation({
    journal,
    actorId,
    hubOraclePolicyId: config.hubOraclePolicyId,
    openReadScope: scopes.open,
    readBoundary: ops.readBoundary,
    reconcile: ops.reconcile,
    assertRuntimeIdle: actorRuntime.assertIdle,
    maximumRetainedRecords: 1024,
  });
  const walletAddress = await lucid.wallet().address();
  const assertCollateralCurrent = async (
    scope: SDK.DaAvailabilityReadScope,
  ) => {
    const metadata = journal.actorSnapshot(actorId, config.hubOraclePolicyId);
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
        config: () => lucid.config(),
        wallet: () => ({ address: async () => walletAddress }),
        utxosAt: async (address) => {
          if (typeof address !== "string")
            throw new Error("Collateral requires the exact actor address");
          return (await scope.read(() => lucid.utxosAt(address))).filter(
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
    assertIdle: actorRuntime.assertIdle,
    joinStoreReads: storeReads.join,
  });
  const source = createCommitteePromiseAdmissionSource({
    ...input,
    actorId,
    retirementPort: retirement.port,
    retirementGrowthReserve: retirement.growthReserve,
    readBoundary: ops.readBoundary,
    assertActuationCurrent: ops.assertActuationCurrent,
    policyAuthority: authority,
    openReadScope: scopes.open,
    currentSchedulingEnabled: true,
    readProtocolDigest: adoption.readProtocolDigest,
    assertCollateralCurrent,
    assertCompatibleClaimsCurrent: claims.assertCompatibleClaimsCurrent,
    assertActorRuntimeIdle: actorRuntime.assertIdle,
    drainReadResources: async (scope) => {
      if (!scope) throw new Error("Owned admission read scope is unavailable");
      await storeReads.join();
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
    now: () => committeeL1NowMs(l1Clock),
    reconcile: async () => {
      const scope = scopes.open();
      try {
        await scopes.refresh(scope);
        return await ops.reconcile(scope);
      } finally {
        await storeReads.join();
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
          { scope },
        );
        const after = await ops.readBoundary(scope);
        scope.assertCurrent();
        if (
          !complete ||
          before.pointId !== after.pointId ||
          before.generation !== after.generation
        )
          throw new Error(
            "Complete response discovery changed or was unavailable",
          );
        return found;
      } finally {
        await storeReads.join();
        scope.close();
      }
    },
    execute: async (action) => {
      const failed = journal
        .actorSnapshot(actorId, config.hubOraclePolicyId)
        .retainedAttempts.filter(
          (row) =>
            row.headerHash ===
              action.challenge.record.datum.commitment.header_hash &&
            row.state === "expired",
        );
      const buildTime = promiseResponseBuildTime(
        l1Clock,
        adoption.clock,
        ttlRule,
      );
      const operation = availabilityResponderTransactionOperation(
        lucid,
        deployment,
        action,
        buildTime,
      );
      const scope = scopes.open(
        availabilityResponderPublicationDeadlineMs(action),
      );
      let builtTtl: number | undefined;
      let freshBody = true;
      let buildStageStart: number | undefined;
      let buildStageTimer: ReturnType<typeof setTimeout> | undefined;
      const finishBuildStage = () => {
        if (buildStageTimer) clearTimeout(buildStageTimer);
        if (
          buildStageStart !== undefined &&
          performance.now() - buildStageStart > PROMISE_RESPONSE_BUILD_STAGE_MS
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
          await ops.assertActuationCurrent(child ?? scope);
          if (
            buildStageStart !== undefined &&
            performance.now() - buildStageStart >=
              PROMISE_RESPONSE_BUILD_STAGE_MS
          )
            throw new Error(
              "Build/sign/persist stage exceeded its adopted allowance",
            );
          if (
            freshBody &&
            builtTtl !== undefined &&
            adoption.clock.eligibleFutureSlots(
              builtTtl,
              PROMISE_RESPONSE_PROPAGATION_MS,
            ) < ttlRule.minimumEligibleFutureSlots
          )
            throw new Error(
              `Actual signed-body TTL has fewer than ${ttlRule.minimumEligibleFutureSlots.toString()} future eligible slots`,
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
              PROMISE_RESPONSE_BUILD_STAGE_MS,
            );
            buildStageTimer.unref();
            const builderScope = SDK.createDaAvailabilityReadScope({
              attemptTimeoutMs: Math.max(
                1,
                Math.ceil(
                  Math.min(
                    PROMISE_RESPONSE_BUILD_STAGE_MS,
                    (shared ?? scope).remainingMs(),
                  ),
                ),
              ),
              signal: (shared ?? scope).signal,
            });
            try {
              return await actorRuntime.trackUnsigned(
                builderScope,
                async () => {
                  try {
                    const tx = await availabilityResponderTransactionOperation(
                      lucid,
                      deployment,
                      action,
                      buildTime,
                    ).build(builderScope.signal, builderScope);
                    builtTtl = promiseResponseTtl(tx, adoption.clock, ttlRule);
                    return tx;
                  } finally {
                    await storeReads.join();
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
    retirementHolds: retirement.holds,
    close: () => journal.close(),
  };
};
