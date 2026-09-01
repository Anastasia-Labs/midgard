import * as LE from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { fetchLocalOgmiosShelleyGenesisSlotConfig } from "@/local-ledger-slot.js";
import {
  fetchLocalOgmiosSubmitSlotSnapshot,
  type SubmitSlotSnapshot,
} from "@/local-ogmios-slot.js";
import { customSlotConfigFromShelleyGenesis } from "@/lucid-time.js";
import { providerRouteSummary } from "@/provider-diagnostics.js";

import { ConfigError, NodeConfig } from "./config.js";
import { createProviderBackedEvaluator } from "./provider-backed-evaluator.js";

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
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    switchToReferenceScriptWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  let slotConfig: LE.SlotConfig | undefined;
  if (nodeConfig.NETWORK === "Custom") {
    const snapshot = yield* fetchLocalOgmiosSubmitSlotSnapshot({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    }).pipe(
      Effect.mapError(
        (cause) =>
          new ConfigError({
            message: "Failed to initialize the Custom Lucid slot mapping",
            cause,
            fieldsAndValues: [
              ["NETWORK", nodeConfig.NETWORK],
              ["L1_OGMIOS_KEY", nodeConfig.L1_OGMIOS_KEY],
            ],
          }),
      ),
    );
    const genesis = yield* fetchLocalOgmiosShelleyGenesisSlotConfig({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    }).pipe(
      Effect.mapError(
        (cause) =>
          new ConfigError({
            message:
              "Failed to query the authoritative Custom network slot epoch",
            cause,
            fieldsAndValues: [
              ["NETWORK", nodeConfig.NETWORK],
              ["L1_OGMIOS_KEY", nodeConfig.L1_OGMIOS_KEY],
            ],
          }),
      ),
    );
    slotConfig = yield* Effect.try({
      try: () => customSlotConfigFromShelleyGenesis(genesis, snapshot),
      catch: (cause) =>
        new ConfigError({
          message: "Failed to validate the Custom Lucid slot mapping",
          cause,
          fieldsAndValues: [
            ["NETWORK", nodeConfig.NETWORK],
            ["L1_OGMIOS_KEY", nodeConfig.L1_OGMIOS_KEY],
          ],
        }),
    });
  }
  const kupmiosProvider = new LE.Kupmios(
    nodeConfig.L1_KUPO_KEY,
    nodeConfig.L1_OGMIOS_KEY,
  );
  const lucidOptions: LE.LucidOptions = {
    evaluator: createProviderBackedEvaluator((tx, additionalUTxOs) =>
      kupmiosProvider.evaluateTx(tx, additionalUTxOs),
    ),
    ...(slotConfig === undefined ? {} : { slotConfig }),
  };
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
    try: () => LE.Lucid(kupmiosProvider, nodeConfig.NETWORK, lucidOptions),
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
      LE.Lucid(lucid.config().provider, nodeConfig.NETWORK, lucidOptions),
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
  yield* Effect.logInfo("Lucid built successfully.");
  return {
    api: lucid,
    referenceScriptsApi,
    operatorMainAddress,
    operatorMergeAddress,
    referenceScriptsWalletAddress,
    referenceScriptsAddress,
    submitSlotSnapshot: () =>
      fetchLocalOgmiosSubmitSlotSnapshot({
        ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
        timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
      }),
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
