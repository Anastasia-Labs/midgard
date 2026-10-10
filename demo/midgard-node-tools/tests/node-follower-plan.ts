/**
 * Reads a builder's node environment with the node's own parsers and asks the
 * node whether its L1 follower runs (`l1FollowerPlan`), for a deployed
 * contracts fixture carrying the event-history lists, the hub oracle, the
 * state queue, the operator set and the forced-order contracts.
 */
import { ConfigProvider, Effect } from "effect";
import { hubOracleOriginConfig } from "midgard-node/services/config.hub-oracle-origin";
import { l1ContentSourcesConfig } from "midgard-node/services/config.l1-content-sources";
import { l1FollowerPlan } from "midgard-node/services/l1-follower.plan";
import { nativeLedgerSettingsFromEnv } from "midgard-node/services/native-ledger";

type PlanInput = Parameters<typeof l1FollowerPlan>[0];

const SCRIPT_ADDRESS =
  "addr_test1wq3vnggra5ljl2tunqkhd4hz4agvt422cvrfswcedj8um2cwsu3l3";

const eventList = (byte: string) => ({
  list: { policyId: byte.repeat(28), spendingScriptAddress: SCRIPT_ADDRESS },
  retention: { spendingScriptAddress: SCRIPT_ADDRESS },
  retirement: { withdrawalScriptHash: byte.repeat(28) },
});

const operatorContract = (byte: string) => ({
  policyId: byte.repeat(28),
  spendingScriptAddress: SCRIPT_ADDRESS,
});

/** A deployment with both event lists, the hub oracle, the state queue, the
 * operator set and the forced-order contracts. */
export const deployedContracts = {
  eventHistory: { deposit: eventList("d1"), withdrawal: eventList("e2") },
  hubOracle: operatorContract("ab"),
  registeredOperators: operatorContract("b6"),
  activeOperators: operatorContract("b7"),
  retiredOperators: operatorContract("b8"),
  scheduler: operatorContract("b9"),
  stateQueue: {
    policyId: "c3".repeat(28),
    spendingScriptAddress: SCRIPT_ADDRESS,
  },
  txOrder: { policyId: "f4".repeat(28), spendingScriptAddress: SCRIPT_ADDRESS },
  cekProgramMaterial: { spendingScriptHash: "a5".repeat(28) },
} as unknown as PlanInput["contracts"];

/** The node's follower plan for `env`, as the node would read it. */
export const nodeFollowerPlan = (
  env: Readonly<Record<string, string | undefined>>,
) => {
  const defined = Object.entries(env).filter(
    (entry): entry is [string, string] => entry[1] !== undefined,
  );
  const provider = ConfigProvider.fromMap(new Map(defined));
  const origin = Effect.runSync(
    Effect.withConfigProvider(hubOracleOriginConfig, provider),
  );
  const contentSources = Effect.runSync(
    Effect.withConfigProvider(l1ContentSourcesConfig, provider),
  );
  return l1FollowerPlan({
    config: {
      ...origin,
      ...contentSources,
      L1_NATIVE_LEDGER: nativeLedgerSettingsFromEnv(env),
      NETWORK: (env.NETWORK ?? "Preprod") as PlanInput["config"]["NETWORK"],
      POSTGRES_HOST: env.POSTGRES_HOST ?? "",
      POSTGRES_PORT: Number(env.POSTGRES_PORT ?? "5432"),
      POSTGRES_USER: env.POSTGRES_USER ?? "",
      POSTGRES_PASSWORD: env.POSTGRES_PASSWORD ?? "",
      POSTGRES_DB: env.POSTGRES_DB ?? "",
    },
    contracts: deployedContracts,
    securityParameter: 2160,
  });
};
