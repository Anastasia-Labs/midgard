import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as LE from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule, type Scope } from "effect";

import {
  resolveLucidSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../custom-slot-mapping.js";
import { readL1FollowerTipSlot, registerL1TipSource } from "../l1-heads.js";
import { registerL1ProviderView } from "../l1-provider-view.js";
import { configureReferencePublication } from "../transactions/reference-publication.js";
import { selectNodeWallet } from "../transactions/utils.wallet-view.js";
import { ConfigError, NodeConfig } from "./config.js";
import {
  NODE_L1_ACCESS_UNCONFIGURED,
  openNodeL1AccessFromConfig,
} from "./l1-provider.js";
import {
  LUCID_INITIALIZATION_PENDING,
  retryStartupStep,
} from "./startup-waiting.js";

const asError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause), { cause });

/** Capped backoff while the node's config files do not yield its network magic. */
const OPEN_RETRY = Schedule.exponential(Duration.millis(500)).pipe(
  Schedule.union(Schedule.spaced(Duration.seconds(30))),
);

/**
 * One Lucid client's construction (`construct`, which reads the provider's
 * protocol parameters): while it fails it is retried on a capped backoff
 * with no deadline, the startup waiting under
 * `lucid_initialization_pending` (`retryStartupStep`).
 */
export const constructLucidOnStartup = (
  key: string,
  message: string,
  network: LE.Network,
  construct: () => Promise<LE.LucidEvolution>,
): Effect.Effect<LE.LucidEvolution> =>
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
    { key, reason: LUCID_INITIALIZATION_PENDING },
  ).pipe(
    // Every failure is retried above; nothing reaches here.
    Effect.catchAll(() => Effect.never),
  );

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
  // while they are unreadable this waits with a logged reason, never exits.
  const access = yield* Effect.acquireRelease(
    Effect.tryPromise({
      try: () => openNodeL1AccessFromConfig(nodeConfig),
      catch: asError,
    }).pipe(
      Effect.tapError((error) =>
        Effect.logWarning(
          `L1 provider unready: the local node's config is unreadable; waits and re-reads. cause=${error.message}`,
        ),
      ),
      Effect.retry(OPEN_RETRY),
      Effect.mapError(
        (cause) =>
          new ConfigError({
            message: "Failed to open the node's L1 access",
            cause,
            fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
          }),
      ),
    ),
    (opened) => Effect.promise(opened.close),
  );
  // A node or sidecar that is not reachable makes this wait with a logged
  // unready reason, never exit.
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
