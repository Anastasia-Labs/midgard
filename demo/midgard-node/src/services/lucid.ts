import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as LE from "@lucid-evolution/lucid";
import { Config, Effect, Option, Schedule } from "effect";

import {
  resolveCustomSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../custom-slot-mapping.js";
import {
  fetchLocalOgmiosSubmitSlotSnapshot,
  readL1FollowerTipSlot,
  registerL1TipSource,
} from "../l1-heads.js";
import { providerRouteSummary } from "../provider-diagnostics.js";
import { configureReferencePublication } from "../transactions/reference-publication.js";
import { synchronizePublicationIndexer } from "../transactions/reference-publication-provider.js";
import { ConfigError, NodeConfig } from "./config.js";
import { makeNodeKupmios } from "./native-ledger.js";

/**
 * Builds the Lucid service bundle used by the node, including reference-script
 * and operator-wallet specializations. Live runtime uses only the configured
 * local Kupmios route, and Custom networks use an authoritative per-instance
 * slot mapping shared by both clients.
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
    ogmiosTipMaxAgeMs?: number;
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    switchToReferenceScriptWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  // Optional override of the genesis-derived local-Ogmios tip-age bound.
  const tipMaxAgeOverride = yield* Config.option(
    Config.integer("L1_OGMIOS_TIP_MAX_AGE_MS").pipe(
      Config.validate({
        message: "L1_OGMIOS_TIP_MAX_AGE_MS must be a positive integer",
        validation: (value) => value > 0,
      }),
    ),
  ).pipe(
    Effect.mapError(
      (cause) =>
        new ConfigError({
          message: "Invalid L1_OGMIOS_TIP_MAX_AGE_MS",
          cause,
          fieldsAndValues: [],
        }),
    ),
  );
  // The genesis-derived mapping is built once in the main thread and
  // inherited by every worker; a block gap or an Ogmios restart makes this
  // wait with a logged unready reason, never exit.
  const slotMapping = yield* resolveCustomSlotMapping({
    ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
    timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    custom: nodeConfig.NETWORK === "Custom",
    ...(Option.isSome(tipMaxAgeOverride)
      ? { tipMaxAgeMs: tipMaxAgeOverride.value }
      : {}),
  }).pipe(
    Effect.mapError(
      (cause) =>
        new ConfigError({
          message: "Failed to initialize the Lucid slot mapping",
          cause,
          fieldsAndValues: [
            ["NETWORK", nodeConfig.NETWORK],
            ["L1_OGMIOS_KEY", nodeConfig.L1_OGMIOS_KEY],
          ],
        }),
    ),
  );
  const slotConfig = slotMapping.slotConfig;
  const ogmiosTipMaxAgeMs = slotMapping.tipMaxAgeMs;
  const readLedgerTip = () =>
    fetchLocalOgmiosSubmitSlotSnapshot({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
      maxHealthAgeMs: ogmiosTipMaxAgeMs,
    });
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
    `L1 provider route: ${JSON.stringify(
      providerRouteSummary({
        provider: nodeConfig.L1_PROVIDER,
        network: nodeConfig.NETWORK,
      }),
    )}`,
  );
  const lucid: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => {
      const kupmiosProvider = makeNodeKupmios({
        kupoUrl: nodeConfig.L1_KUPO_KEY,
        ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
        network: nodeConfig.NETWORK,
        nativeLedger: nodeConfig.L1_NATIVE_LEDGER,
      });
      return LE.Lucid(
        kupmiosProvider,
        nodeConfig.NETWORK,
        slotConfig === undefined ? undefined : { slotConfig },
      );
    },
    catch: (e) =>
      new ConfigError({
        message: `An error occurred on lucid initialization`,
        cause: e,
        fieldsAndValues: [
          ["L1_PROVIDER", nodeConfig.L1_PROVIDER],
          ["NETWORK", nodeConfig.NETWORK],
        ],
      }),
  }).pipe(
    Effect.tapError(Effect.logInfo),
    Effect.retry(Schedule.fixed("1000 millis")),
  );
  const referenceScriptsApi: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () =>
      LE.Lucid(
        lucid.config().provider,
        nodeConfig.NETWORK,
        slotConfig === undefined ? undefined : { slotConfig },
      ),
    catch: (e) =>
      new ConfigError({
        message: "An error occurred while initializing reference-scripts Lucid",
        cause: e,
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
  });
  const switchToReferenceScriptWallet = Effect.sync(() =>
    referenceScriptsApi.selectWallet.fromSeed(
      nodeConfig.L1_REFERENCE_SCRIPT_SEED_PHRASE,
    ),
  );
  yield* switchToReferenceScriptWallet;
  // Both clients read one L1 view: their `l1SlotNow` comes from the L1
  // follower's covered tip (N1). The Ogmios submit-slot read below only
  // bounds a new tx's validity interval; it never moves `l1SlotNow`.
  registerL1TipSource(
    [lucid, referenceScriptsApi],
    readL1FollowerTipSlot,
    slotConfig === undefined ? {} : { slotLengthMs: slotConfig.slotLength },
  );
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
    synchronize: () =>
      synchronizePublicationIndexer(
        nodeConfig.L1_OGMIOS_KEY,
        nodeConfig.L1_KUPO_KEY,
      ),
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
    ogmiosTipMaxAgeMs,
    switchToOperatorsMainWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      ),
    ),
    switchToReferenceScriptWallet,
  };
});

/**
 * Service exposing the fully-initialized Lucid clients and wallet-switching
 * helpers used by the node.
 */
export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  effect: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}
