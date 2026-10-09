import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as LE from "@lucid-evolution/lucid";
import { Effect, type Scope } from "effect";

import {
  resolveLucidSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../custom-slot-mapping.js";
import { readL1FollowerTipSlot, registerL1TipSource } from "../l1-heads.js";
import { registerL1ProviderView } from "../l1-provider-view.js";
import { hasCauseCode, isRetryableProviderError } from "../provider-retry.js";
import { configureReferencePublication } from "../transactions/reference-publication.js";
import { selectNodeWallet } from "../transactions/utils.wallet-view.js";
import { ConfigError, NodeConfig } from "./config.js";
import {
  NODE_L1_ACCESS_UNCONFIGURED,
  openNodeL1AccessFromConfig,
} from "./l1-provider.js";
import {
  L1_NODE_CONFIG_PENDING,
  LUCID_INITIALIZATION_PENDING,
  retryStartupStep,
  STARTUP_L1_NODE_BUDGET,
  type StartupStepBudget,
  type StartupStepFailedError,
} from "./startup-waiting.js";

const asError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause), { cause });

/** The local node's configuration files are not there yet (a node still
 * starting writes them): the one open failure waited out. */
export const isL1NodeConfigPending = (error: unknown): boolean =>
  hasCauseCode(error, "ENOENT");

/** A step's terminal failure, as the Lucid service's `ConfigError`; the
 * startup still finds the step behind it (`findStartupStepFailure`). */
const asConfigError =
  (network: LE.Network) =>
  (failure: StartupStepFailedError): ConfigError =>
    new ConfigError({
      message: failure.message,
      cause: failure,
      fieldsAndValues: [["NETWORK", network]],
    });

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
  ).pipe(Effect.mapError(asConfigError(network)));

/**
 * Builds the Lucid service bundle used by the node, including reference-script
 * and operator-wallet specializations. Both clients read through one
 * `L1FollowerProvider` over the node's follower store and the local node's
 * transport (`services/l1-provider.ts`), on the slot mapping the ledger
 * reports.
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
  NodeConfig | Scope.Scope
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  if (nodeConfig.L1_NATIVE_LEDGER === undefined)
    return yield* Effect.fail(
      new ConfigError({
        message: `The node's L1 provider needs ${NODE_L1_ACCESS_UNCONFIGURED}`,
        cause: "l1-node-unconfigured",
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
    );
  // Opening reads only the node's config files (for its network magic);
  // while they are not there yet this waits under `l1_node_config_pending`
  // for at most the L1 node budget. Any other failure (a wrong network
  // magic, an unconfigured node) fails at once.
  const access = yield* Effect.acquireRelease(
    retryStartupStep(
      Effect.tryPromise({
        try: () => openNodeL1AccessFromConfig(nodeConfig),
        catch: asError,
      }),
      {
        key: "l1_node_config",
        reason: L1_NODE_CONFIG_PENDING,
        retryable: isL1NodeConfigPending,
        budget: { maxElapsed: STARTUP_L1_NODE_BUDGET },
        initialMs: 500,
      },
    ).pipe(Effect.mapError(asConfigError(nodeConfig.NETWORK))),
    (opened) => Effect.promise(opened.close),
  );
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
      primary: "l1_node",
      network: nodeConfig.NETWORK,
      socket: access.endpoint,
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
  // Both clients read one L1 view: their `l1SlotNow` comes from the L1
  // follower's covered tip (N1). The ledger-tip submit-slot read below only
  // bounds a new tx's validity interval; it never moves `l1SlotNow`.
  registerL1TipSource([lucid, referenceScriptsApi], readL1FollowerTipSlot, {
    slotLengthMs: slotConfig.slotLength,
  });
  const readSubmitSlotSnapshotOnce = readLedgerTip;
  registerL1ProviderView([lucid, referenceScriptsApi], {
    submitSlotSnapshot: readSubmitSlotSnapshotOnce,
    viewPoint: () =>
      Effect.tryPromise({ try: access.synchronizedViewPoint, catch: asError }),
  });
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
 * helpers used by the node.
 */
export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  scoped: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}
