import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import path from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  assertDeploymentMarkerMatches,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Layer } from "effect";

import { resolveLucidSlotMapping } from "../custom-slot-mapping.js";
import { resolveHousekeepingRetentionDays } from "../database/retention-policy.js";
import {
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
} from "../e2e/run-state.js";
import { AlwaysSucceedsContract } from "./always-succeeds.js";
import { ConfigError, NodeConfig } from "./config.js";
import type { NodeConfigDep } from "./config.node-config-dep.js";
import {
  asStartupConfigError,
  FollowerL1AdapterLive,
  L1Adapter,
} from "./l1-adapter.js";
import { assertDeploymentManifestMatchesConfig } from "./midgard-contracts.assert-deployment-manifest-matches-config.js";
import { type HubOracleOneShotOutRef } from "./midgard-contracts.build-real-hub-oracle-validator.js";
import {
  availabilityParametersFromExplicitEnvironment,
  derivedDeploymentL1SlotConfig,
  eventHistoryBoundsFromExplicitEnvironment,
  eventHistoryProtectionDurationFromExplicitEnvironment,
  loadReferenceScriptAuthValidator,
  type MidgardContractRuntimeValue,
  readConfiguredDeploymentManifest,
} from "./midgard-contracts.load-reference-script-auth-validator.js";
import { midgardContractsFromDeploymentManifest } from "./midgard-contracts.midgard-contracts-from-deployment-manifest.js";
import { withRealStateQueueAndOperatorContracts } from "./midgard-contracts.with-real-state-queue-and-operator-contracts.js";
import {
  L1_SLOT_CONFIG_MISMATCH,
  startupStepFailed,
} from "./startup-waiting.js";

/** The startup step a derived bundle's slot mapping is read under. */
const DERIVED_L1_SLOT_MAPPING_STEP = "derived_l1_slot_mapping";

/**
 * A derived bundle's slot mapping, read through the process's L1 port: the
 * `L1Adapter` the process provides is opened for this one read and closed.
 * A transient L1 read failure is waited out under `l1_slot_mapping_pending`
 * for at most the L1 node budget (`resolveLucidSlotMapping`); past it the
 * step fails as exhausted, so `listen` exits non-zero and its supervisor's
 * restart is the backoff. A named network whose ledger reports a mapping
 * other than its static one fails the step under `l1_slot_config_mismatch`,
 * which no restart repairs, so `listen` holds unready. Each failure is a
 * `ConfigError` over the step's `StartupStepFailedError`.
 */
const readDerivedL1SlotConfig = (
  nodeConfig: NodeConfigDep,
): Effect.Effect<L1SlotConfig, ConfigError, L1Adapter> =>
  Effect.scoped(
    Effect.gen(function* () {
      const access = yield* (yield* L1Adapter).open(nodeConfig);
      const ledger = yield* resolveLucidSlotMapping({
        read: access.slotConfig,
      }).pipe(
        Effect.mapError(
          (cause) =>
            new ConfigError({
              message:
                "Failed to read the derived contract bundle's L1 slot mapping",
              cause,
              fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
            }),
        ),
      );
      return yield* Effect.try({
        try: () => derivedDeploymentL1SlotConfig(nodeConfig.NETWORK, ledger),
        catch: (cause) =>
          asStartupConfigError(nodeConfig.NETWORK)(
            startupStepFailed({
              step: DERIVED_L1_SLOT_MAPPING_STEP,
              reason: L1_SLOT_CONFIG_MISMATCH,
              cause,
            }),
          ),
      });
    }),
  );

/**
 * Resolves the production validator bundle from node configuration.
 *
 * The effect fails fast if the one-shot hub-oracle parameters are missing so a
 * node cannot boot into an ambiguous real-contract configuration. A finalized
 * manifest carries its slot mapping and never opens L1; a derived bundle
 * reads it through the L1 port (`readDerivedL1SlotConfig`), the one failure
 * this service reports as a `ConfigError` rather than a defect.
 */
const makeMidgardContractRuntime = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const baseContracts = yield* AlwaysSucceedsContract;
  const configuredManifest = yield* Effect.try({
    try: () => readConfiguredDeploymentManifest(),
    catch: (cause) =>
      new Error(
        `Failed to read configured deployment manifest: ${formatUnknownError(
          cause,
        )}`,
      ),
  });
  if (configuredManifest !== undefined) {
    yield* Effect.try({
      try: () =>
        assertDeploymentManifestMatchesConfig(
          configuredManifest.manifest,
          configuredManifest.path,
          nodeConfig,
        ),
      catch: (cause) =>
        new Error(
          `Configured deployment manifest cannot be used as contract source: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const manifestContracts = yield* Effect.try({
      try: () =>
        midgardContractsFromDeploymentManifest(
          nodeConfig.NETWORK,
          configuredManifest.manifest,
          configuredManifest.path,
        ),
      catch: (cause) =>
        new Error(
          `Failed to derive contracts from configured deployment manifest: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const runStatePath = defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(
          `Failed to inspect deployment run state at ${runStatePath}: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    if (runState !== null) {
      yield* Effect.try({
        try: () => {
          const marker = makeDeploymentMarker(
            configuredManifest.manifest.manifestId,
          );
          assertDeploymentMarkerMatches(
            marker,
            runState.identity.deploymentMarker,
            "node deployment run state",
          );
          const expectedManifestSha256 = createHash("sha256")
            .update(readFileSync(configuredManifest.path))
            .digest("hex");
          if (
            runState.identity.manifestSha256 !== expectedManifestSha256 ||
            runState.identity.manifestPath === undefined ||
            path.resolve(runState.identity.manifestPath) !==
              path.resolve(configuredManifest.path)
          ) {
            throw new Error(
              `deployment run-state manifest binding does not match configured manifest path/hash`,
            );
          }
        },
        catch: (cause) =>
          new Error(
            `Configured deployment manifest cannot use run state ${runStatePath}: ${formatUnknownError(
              cause,
            )}`,
          ),
      });
    }
    yield* Effect.logInfo(
      `🔐 Contract source selected: deployment-manifest path=${configuredManifest.path},manifestId=${String(
        configuredManifest.manifest.manifestId ?? "unknown",
      )}`,
    );
    const runtime: MidgardContractRuntimeValue = {
      contracts: manifestContracts,
      identity: {
        kind: "manifest",
        manifestId: configuredManifest.manifest.manifestId,
        deploymentMarker: makeDeploymentMarker(
          configuredManifest.manifest.manifestId,
        ),
        path: configuredManifest.path,
        consensusProfile: configuredManifest.manifest.consensusProfile,
        l1Finality: configuredManifest.manifest.l1Finality,
        manifest: configuredManifest.manifest,
      },
    };
    return runtime;
  }
  // Without a manifest the compiled bundle owns the transport window. Refuse
  // an invalid explicit override now, before a sweep would disable pruning.
  yield* Effect.try({
    try: () =>
      resolveHousekeepingRetentionDays({
        configured: nodeConfig.RETENTION_DAYS,
        manifestRetentionDays: undefined,
      }),
    catch: (cause) =>
      new Error(
        `Derived contract bundle cannot use housekeeping retention: ${formatUnknownError(cause)}`,
      ),
  });
  const oneShotOutRef: HubOracleOneShotOutRef = {
    txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
  };
  const referenceScriptAuth = yield* loadReferenceScriptAuthValidator();
  const l1SlotConfig = yield* readDerivedL1SlotConfig(nodeConfig);
  const resolvedContracts = yield* withRealStateQueueAndOperatorContracts(
    nodeConfig.NETWORK,
    baseContracts,
    oneShotOutRef,
    {
      referenceScriptAuth,
      eventHistoryBounds: eventHistoryBoundsFromExplicitEnvironment(),
      eventHistoryProtectionDurationMs:
        eventHistoryProtectionDurationFromExplicitEnvironment(),
      availabilityChallengeParameters:
        availabilityParametersFromExplicitEnvironment(),
      l1SlotConfig,
    },
  );
  yield* Effect.logInfo(
    "🔐 Contract source selected: state_queue=real, da_attestation=real, da_params_governor=real, da_bond_pool=real, hub_oracle=real, deposit=real, tx_order=real, withdrawal=real, settlement=real, reserve=real, payout=real, registered_operators=real, active_operators=real, retired_operators=real, scheduler=real, fraud_proofs.all_registered_chains=real",
  );
  const runtime: MidgardContractRuntimeValue = {
    contracts: resolvedContracts,
    identity: {
      kind: "derived",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      l1SlotConfig,
    },
  };
  return runtime;
}).pipe(
  // The L1 slot mapping read fails as a named startup step; every other
  // refusal here is a configuration defect.
  Effect.catchAll((error) =>
    error instanceof ConfigError ? Effect.fail(error) : Effect.die(error),
  ),
);

class MidgardContractRuntime extends Effect.Service<MidgardContractRuntime>()(
  "MidgardContractRuntime",
  {
    effect: makeMidgardContractRuntime,
    dependencies: [AlwaysSucceedsContract.Default, NodeConfig.layer],
  },
) {}

/**
 * Service providing the validator bundle used by the node.
 */
export class MidgardContracts extends Effect.Service<MidgardContracts>()(
  "MidgardContracts",
  {
    effect: Effect.map(MidgardContractRuntime, ({ contracts, identity }) => ({
      ...contracts,
      consensusProfile: identity.consensusProfile,
    })),
    dependencies: [MidgardContractRuntime.Default],
  },
) {}

/** Identity of the exact contract source selected by {@link MidgardContracts}. */
export class ContractDeploymentIdentity extends Effect.Service<ContractDeploymentIdentity>()(
  "ContractDeploymentIdentity",
  {
    effect: Effect.map(MidgardContractRuntime, ({ identity }) => identity),
    dependencies: [MidgardContractRuntime.Default],
  },
) {}

/**
 * Shared layer so contract bytes and their deployment identity resolve once.
 * It requires an `L1Adapter`, which a derived bundle reads its slot mapping
 * through: a role provides `FollowerMidgardContractServices`, a command
 * `ToolMidgardContractServices` (`../commands/l1-tool-adapter.ts`).
 */
export const MidgardContractServices = Layer.merge(
  MidgardContracts.Default,
  ContractDeploymentIdentity.Default,
);

/** The contract services of a role process: over the follower adapter. */
export const FollowerMidgardContractServices = MidgardContractServices.pipe(
  Layer.provide(FollowerL1AdapterLive),
);
