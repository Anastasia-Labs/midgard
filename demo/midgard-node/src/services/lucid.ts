import { Effect, Schedule } from "effect";
import { ConfigError, NodeConfig } from "./config.js";
import * as LE from "@lucid-evolution/lucid";

const makeLucid: Effect.Effect<
  {
    api: LE.LucidEvolution;
    referenceScriptsApi: LE.LucidEvolution;
    operatorMainAddress: string;
    operatorMergeAddress: string;
    referenceScriptsAddress: string;
    switchToOperatorsMainWallet: Effect.Effect<void>;
    switchToOperatorsBlockCommitmentWallet: Effect.Effect<void>;
    switchToOperatorsMergingWallet: Effect.Effect<void>;
    switchToReferenceScriptWallet: Effect.Effect<void>;
  },
  ConfigError,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => {
      switch (nodeConfig.L1_PROVIDER) {
        case "Kupmios":
          return LE.Lucid(
            new LE.Kupmios(nodeConfig.L1_KUPO_KEY, nodeConfig.L1_OGMIOS_KEY),
            nodeConfig.NETWORK,
          );
        case "Blockfrost":
          return LE.Lucid(
            new LE.Blockfrost(
              nodeConfig.L1_BLOCKFROST_API_URL,
              nodeConfig.L1_BLOCKFROST_KEY,
            ),
            nodeConfig.NETWORK,
          );
      }
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
  const operatorMainAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE,
    { network: nodeConfig.NETWORK },
  ).address;
  const operatorMergeAddress = LE.walletFromSeed(
    nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
    { network: nodeConfig.NETWORK },
  ).address;
  const referenceScriptsApi: LE.LucidEvolution = yield* Effect.tryPromise({
    try: () => LE.Lucid(lucid.config().provider, nodeConfig.NETWORK),
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
  const referenceScriptsAddress = yield* Effect.tryPromise({
    try: () => referenceScriptsApi.wallet().address(),
    catch: (e) =>
      new ConfigError({
        message: "Failed to derive reference-scripts wallet address",
        cause: e,
        fieldsAndValues: [["NETWORK", nodeConfig.NETWORK]],
      }),
  });
  if (
    nodeConfig.L1_REFERENCE_SCRIPT_ADDRESS.trim() !== referenceScriptsAddress
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
          ["derived_address", referenceScriptsAddress],
        ],
      }),
    );
  }
  const distinctOperationalWallets = new Map<string, string[]>();
  for (const [role, address] of [
    ["operator-main", operatorMainAddress],
    ["operator-merge", operatorMergeAddress],
    ["reference-scripts", referenceScriptsAddress],
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
          "Operational wallets must be distinct: operator-main, operator-merge, and reference-scripts must each use a different address",
        cause: `overlapping_roles=[${overlappingOperationalWalletRoles.join(", ")}]`,
        fieldsAndValues: [
          ["L1_OPERATOR_ADDRESS", operatorMainAddress],
          ["L1_OPERATOR_MERGE_ADDRESS", operatorMergeAddress],
          ["L1_REFERENCE_SCRIPT_ADDRESS", referenceScriptsAddress],
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
    referenceScriptsAddress,
    switchToOperatorsMainWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE),
    ),
    switchToOperatorsBlockCommitmentWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT,
      ),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      lucid.selectWallet.fromSeed(
        nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
      ),
    ),
    switchToReferenceScriptWallet,
  };
});

export class Lucid extends Effect.Service<Lucid>()("Lucid", {
  effect: makeLucid,
  dependencies: [NodeConfig.layer],
}) {}
