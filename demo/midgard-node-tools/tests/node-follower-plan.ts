/**
 * Reads a builder's node environment with the node's own parsers and asks the
 * node whether its L1 follower runs (`l1FollowerPlan`), for a deployed
 * contracts fixture carrying event-history lists and the hub oracle.
 */
import { ConfigProvider, Effect } from "effect";
import { hubOracleOriginConfig } from "midgard-node/services/config.hub-oracle-origin";
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

/** A deployment with both event lists and the hub oracle. */
export const deployedContracts = {
  eventHistory: { deposit: eventList("d1"), withdrawal: eventList("e2") },
  hubOracle: { policyId: "ab".repeat(28) },
} as unknown as PlanInput["contracts"];

/** The node's follower plan for `env`, as the node would read it. */
export const nodeFollowerPlan = (
  env: Readonly<Record<string, string | undefined>>,
) => {
  const defined = Object.entries(env).filter(
    (entry): entry is [string, string] => entry[1] !== undefined,
  );
  const origin = Effect.runSync(
    Effect.withConfigProvider(
      hubOracleOriginConfig,
      ConfigProvider.fromMap(new Map(defined)),
    ),
  );
  return l1FollowerPlan({
    config: {
      ...origin,
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
