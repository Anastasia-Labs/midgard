import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as LE from "@lucid-evolution/lucid";
import { Effect, Layer, type Scope } from "effect";

import {
  resolveLucidSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../custom-slot-mapping.js";
import { isRetryableProviderError } from "../provider-retry.js";
import { configureReferencePublication } from "../transactions/reference-publication.js";
import { selectNodeWallet } from "../transactions/utils.wallet-view.js";
import { ConfigError, NodeConfig } from "./config.js";
import {
  asStartupConfigError,
  FollowerL1AdapterLive,
  L1Adapter,
} from "./l1-adapter.js";
import {
  LUCID_INITIALIZATION_PENDING,
  retryStartupStep,
  STARTUP_L1_NODE_BUDGET,
  type StartupStepBudget,
} from "./startup-waiting.js";

const asError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause), { cause });

/**
 * One Lucid client's construction (`construct`, which reads the provider's
 * protocol parameters): a retryable provider failure
 * (`isRetryableProviderError`) is waited out on a capped backoff, the
 * startup waiting under `lucid_initialization_pending`, for at most
 * `budget` (`STARTUP_L1_NODE_BUDGET` by default). Past it, or on any other
 * failure, the construction fails with a `ConfigError` over the step's
 * `StartupStepFailedError`.
 */
export const constructLucidOnStartup = (
  key: string,
  message: string,
  network: LE.Network,
  construct: () => Promise<LE.LucidEvolution>,
  budget: StartupStepBudget = { maxElapsed: STARTUP_L1_NODE_BUDGET },
): Effect.Effect<LE.LucidEvolution, ConfigError> =>
  retryStartupStep(
    Effect.tryPromise({
      try: construct,
      catch: (cause) =>
        new ConfigError({
          message,
          cause,
          fieldsAndValues: [["NETWORK", network]],
        }),
    }),
    {
      key,
      reason: LUCID_INITIALIZATION_PENDING,
      retryable: isRetryableProviderError,
      budget,
    },
  ).pipe(Effect.mapError(asStartupConfigError(network)));

/**
 * Builds the Lucid service bundle used by the node, including reference-script
 * and operator-wallet specializations. Both clients read through the one
 * L1 access the process's `L1Adapter` opens (the follower store's in a
 * role, a tool adapter's in a command), on that adapter's slot mapping.
 */
const makeLucid: Effect.Effect<
  {
    api: LE.LucidEvolution;
    referenceScriptsApi: LE.LucidEvolution;
    operatorMainAddress: string;
    operatorMergeAddress: string;
    referenceScriptsWalletAddress: string;
    referenceScriptsAddress: string;
    submitSlotSnapshot: () => Effect.Effect<SubmitSlotSnapshot, Error>;
    // Optional so hand-built test services need not supply them.
    readSubmitSlotSnapshotOnce?: () => Effect.Effect<SubmitSlotSnapshot, Error>;
    /** The local node transport's readiness, for `/readyz`. */
    l1TransportReadiness?: () => TransportReadiness;
    /** The local node's socket, for diagnostics. */
    l1Endpoint?: string;
    /** The ledger's raw `protocol_params` answer, read now. */
    l1ProtocolParametersCbor?: () => Promise<Uint8Array>;
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    switchToReferenceScriptWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig | L1Adapter | Scope.Scope
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  // The adapter the process provides: the follower in a role, a tool access
  // in a command (`l1-adapter.ts`).
  const access = yield* (yield* L1Adapter).open(nodeConfig);
  // A node or sidecar that is not reachable makes this wait with a logged
  // unready reason, for at most the L1 node budget.
  const slotConfig = yield* resolveLucidSlotMapping({
    read: access.slotConfig,
  }).pipe(
    Effect.mapError(
      (cause) =>
        new ConfigError({
          message: "Failed to initialize the Lucid slot mapping",
          cause,
          fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
        }),
    ),
  );
  const readLedgerTip = () =>
    Effect.tryPromise({ try: access.submitSlotSnapshot, catch: asError });
  const operatorMainAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE,
    {
      network: nodeConfig.NETWORK,
    },
  ).address;
  const operatorMergeAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
    {
      network: nodeConfig.NETWORK,
    },
  ).address;
  yield* Effect.logInfo("Initializing Lucid...");
  yield* Effect.logInfo(
    `L1 provider route: ${JSON.stringify({
      access: access.kind,
      network: nodeConfig.NETWORK,
      endpoint: access.endpoint,
    })}`,
  );
  const lucid = yield* constructLucidOnStartup(
    "lucid_initialization",
    "An error occurred on lucid initialization",
    nodeConfig.NETWORK,
    () => LE.Lucid(access.provider, nodeConfig.NETWORK, { slotConfig }),
  );
  const referenceScriptsApi = yield* constructLucidOnStartup(
    "reference_scripts_lucid_initialization",
    "An error occurred while initializing reference-scripts Lucid",
    nodeConfig.NETWORK,
    () => LE.Lucid(lucid.config().provider, nodeConfig.NETWORK, { slotConfig }),
  );
  const switchToReferenceScriptWallet = Effect.sync(() =>
    selectNodeWallet(
      referenceScriptsApi,
      nodeConfig.L1_REFERENCE_SCRIPT_SEED_PHRASE,
    ),
  );
  yield* switchToReferenceScriptWallet;
  // Both clients are built over the access's provider, so both read its one
  // L1 view: `l1SlotNow` from its tip, view points and the submit slot from
  // it (`l1-access.ts`). The ledger-tip submit-slot read only bounds a new
  // tx's validity interval; it never moves `l1SlotNow`.
  const readSubmitSlotSnapshotOnce = readLedgerTip;
  const referenceScriptsWalletAddress = yield* Effect.tryPromise({
    try: () => referenceScriptsApi.wallet().address(),
    catch: (e) =>
      new ConfigError({
        message: "Failed to derive reference-scripts wallet address",
        cause: e,
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
  });
  if (
    nodeConfig.L1_REFERENCE_SCRIPT_ADDRESS !== referenceScriptsWalletAddress
  ) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Configured L1_REFERENCE_SCRIPT_ADDRESS does not match the address derived from L1_REFERENCE_SCRIPT_SEED_PHRASE",
        cause: "reference-script-address-seed-mismatch",
        fieldsAndValues: [
          [
            "L1_REFERENCE_SCRIPT_ADDRESS",
            nodeConfig.L1_REFERENCE_SCRIPT_ADDRESS,
          ],
          ["derived_address", referenceScriptsWalletAddress],
        ],
      }),
    );
  }
  const referenceScriptsAddress = nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS;
  const distinctOperationalWallets = new Map<string, string[]>();
  for (const [role, address] of [
    ["operator-main", operatorMainAddress],
    ["operator-merge", operatorMergeAddress],
    ["reference-scripts", referenceScriptsWalletAddress],
  ] as const) {
    const existingRoles = distinctOperationalWallets.get(address) ?? [];
    distinctOperationalWallets.set(address, [...existingRoles, role]);
  }
  const overlappingOperationalWalletRoles = [...distinctOperationalWallets]
    .filter(([, roles]) => roles.length > 1)
    .map(([address, roles]) => `${roles.join("+")}=${address}`);
  if (overlappingOperationalWalletRoles.length > 0) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Operational wallet roles must use distinct L1 addresses for production-safe local wallet views",
        cause: overlappingOperationalWalletRoles.join(","),
        fieldsAndValues: [
          ["L1_OPERATOR_ADDRESS", operatorMainAddress],
          ["L1_OPERATOR_MERGE_ADDRESS", operatorMergeAddress],
          ["L1_REFERENCE_SCRIPT_ADDRESS", referenceScriptsWalletAddress],
        ],
      }),
    );
  }
  configureReferencePublication(referenceScriptsApi, {
    mode: "chained",
    synchronize: async () => (await access.synchronizedViewPoint()).slot,
  });
  yield* Effect.logInfo("Lucid built successfully.");
  return {
    api: lucid,
    referenceScriptsApi,
    operatorMainAddress,
    operatorMergeAddress,
    referenceScriptsWalletAddress,
    referenceScriptsAddress,
    // Submit time re-reads briefly on a transient stale tip, then refuses.
    submitSlotSnapshot: () =>
      retryTransientSubmitSlotSnapshot(readSubmitSlotSnapshotOnce),
    // One read under the same bound, for probes that schedule their own
    // retries (the readiness refresher).
    readSubmitSlotSnapshotOnce,
    l1TransportReadiness: access.transportReadiness,
    l1Endpoint: access.endpoint,
    l1ProtocolParametersCbor: access.protocolParametersCbor,
    switchToOperatorsMainWallet: Effect.sync(() =>
      selectNodeWallet(lucid, nodeConfig.L1_OPERATOR_SEED_PHRASE),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      selectNodeWallet(lucid, nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX),
    ),
    switchToReferenceScriptWallet,
  };
});

/**
 * Service exposing the fully-initialized Lucid clients and wallet-switching
 * helpers used by the node. It requires an `L1Adapter`: a role provides
 * `FollowerL1AdapterLive`, a command `ToolL1AdapterLive`.
 */
export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  scoped: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}

/** The Lucid service of a role process: over the follower adapter. */
export const FollowerLucidLive = Lucid.Default.pipe(
  Layer.provide(FollowerL1AdapterLive),
);
