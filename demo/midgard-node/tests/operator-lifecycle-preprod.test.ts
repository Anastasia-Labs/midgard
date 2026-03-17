import { beforeAll, describe, expect, it } from "vitest";
import { Effect } from "effect";
import {
  LucidEvolution,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import dotenv from "dotenv";
import * as Services from "@/services/index.js";
import {
  activateOperatorProgram,
  deregisterOperatorProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";

dotenv.config();

const PREPROD_FLOW_FLAG = "MIDGARD_RUN_PREPROD_OPERATOR_LIFECYCLE_TESTS";
const runPreprodFlow = process.env[PREPROD_FLOW_FLAG] === "1";
const describePreprod = runPreprodFlow ? describe : describe.skip;

const REQUIRED_PREPROD_ENV = [
  "L1_PROVIDER",
  "L1_BLOCKFROST_API_URL",
  "L1_BLOCKFROST_KEY",
  "L1_OPERATOR_SEED_PHRASE",
  "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  "L1_REFERENCE_SCRIPT_ADDRESS",
  "NETWORK",
  "HUB_ORACLE_ONE_SHOT_TX_HASH",
  "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX",
] as const;

type LifecycleRuntime = {
  readonly lucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly requiredBondLovelace: bigint;
  readonly provider: string;
  readonly network: string;
  readonly switchToMainWallet: Effect.Effect<void>;
  readonly switchToMergeWallet: Effect.Effect<void>;
};

const readOperatorKeyHash = async (lucid: LucidEvolution): Promise<string> => {
  const operatorAddress = await lucid.wallet().address();
  const paymentCredential = paymentCredentialOf(operatorAddress);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected operator wallet payment credential to be Key");
  }
  return paymentCredential.hash;
};

const readOperatorNodeState = async (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<{
  readonly operatorKeyHash: string;
  readonly registeredCount: number;
  readonly activeCount: number;
}> => {
  const operatorKeyHash = await readOperatorKeyHash(lucid);
  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    SDK.NODE_ASSET_NAME + operatorKeyHash,
  );
  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    SDK.NODE_ASSET_NAME + operatorKeyHash,
  );

  const [registeredNodes, activeNodes] = await Promise.all([
    lucid.utxosAtWithUnit(
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
    ),
    lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
    ),
  ]);

  return {
    operatorKeyHash,
    registeredCount: registeredNodes.length,
    activeCount: activeNodes.length,
  };
};

const expectHashOrNull = (value: string | null, label: string): void => {
  if (value === null) {
    return;
  }
  expect(value, `${label} should be a tx hash when present`).toMatch(
    /^[0-9a-f]{64}$/i,
  );
};

const loadRuntime = (): Promise<LifecycleRuntime> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const lucidService = yield* Services.Lucid;
      const contracts = yield* Services.MidgardContracts;
      const config = yield* Services.NodeConfig;
      return {
        lucid: lucidService.api,
        referenceScriptsLucid: lucidService.referenceScriptsApi,
        contracts,
        requiredBondLovelace: config.OPERATOR_REQUIRED_BOND_LOVELACE,
        provider: config.L1_PROVIDER,
        network: config.NETWORK,
        switchToMainWallet: lucidService.switchToOperatorsMainWallet,
        switchToMergeWallet: lucidService.switchToOperatorsMergingWallet,
      };
    }).pipe(
      Effect.provide(Services.NodeConfig.layer),
      Effect.provide(Services.MidgardContracts.Default),
      Effect.provide(Services.Lucid.Default),
    ),
  );

describePreprod("operator lifecycle preprod blockfrost", () => {
  let runtime: LifecycleRuntime;

  beforeAll(async () => {
    const missing = REQUIRED_PREPROD_ENV.filter((name) => {
      const value = process.env[name];
      return value === undefined || value.trim().length === 0;
    });
    if (missing.length > 0) {
      throw new Error(
        `Missing required env for preprod lifecycle tests: ${missing.join(", ")}`,
      );
    }
    runtime = await loadRuntime();
    if (runtime.provider !== "Blockfrost") {
      throw new Error(
        `Expected L1_PROVIDER=Blockfrost for preprod lifecycle tests, got ${runtime.provider}`,
      );
    }
    if (runtime.network !== "Preprod") {
      throw new Error(
        `Expected NETWORK=Preprod for preprod lifecycle tests, got ${runtime.network}`,
      );
    }
  });

  it(
    "runs register-only then activate-only with the main operator wallet",
    async () => {
      await Effect.runPromise(runtime.switchToMainWallet);
      const initialState = await readOperatorNodeState(
        runtime.lucid,
        runtime.contracts,
      );

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          runtime.lucid,
          runtime.contracts,
          runtime.requiredBondLovelace,
          runtime.referenceScriptsLucid,
        ),
      );
      expectHashOrNull(registerResult.registerTxHash, "registerTxHash");

      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          runtime.lucid,
          runtime.contracts,
          runtime.requiredBondLovelace,
          runtime.referenceScriptsLucid,
        ),
      );
      expectHashOrNull(activateResult.activateTxHash, "activateTxHash");

      const finalState = await readOperatorNodeState(
        runtime.lucid,
        runtime.contracts,
      );
      expect(finalState.operatorKeyHash).toEqual(initialState.operatorKeyHash);
      expect(finalState.activeCount).toBeGreaterThan(0);
      expect(finalState.registeredCount).toEqual(0);
    },
    1_200_000,
  );

  it(
    "runs register-only then deregister-only with the merge operator wallet",
    async () => {
      await Effect.runPromise(runtime.switchToMergeWallet);
      const initialState = await readOperatorNodeState(
        runtime.lucid,
        runtime.contracts,
      );

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          runtime.lucid,
          runtime.contracts,
          runtime.requiredBondLovelace,
          runtime.referenceScriptsLucid,
        ),
      );
      expectHashOrNull(registerResult.registerTxHash, "registerTxHash");

      const deregisterResult = await Effect.runPromise(
        deregisterOperatorProgram(
          runtime.lucid,
          runtime.contracts,
          runtime.requiredBondLovelace,
          runtime.referenceScriptsLucid,
        ),
      );
      expectHashOrNull(deregisterResult.deregisterTxHash, "deregisterTxHash");

      const finalState = await readOperatorNodeState(
        runtime.lucid,
        runtime.contracts,
      );
      expect(finalState.operatorKeyHash).toEqual(initialState.operatorKeyHash);
      expect(finalState.registeredCount).toEqual(0);
    },
    1_200_000,
  );
});
