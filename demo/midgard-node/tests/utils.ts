import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";
import { Lucid } from "@/services/lucid.js";
import { Effect } from "effect";

const TEST_ENV_DEFAULTS: Record<string, string> = {
  L1_PROVIDER: "Kupmios",
  L1_BLOCKFROST_API_URL: "https://blockfrost.invalid",
  L1_BLOCKFROST_KEY: "test-key",
  L1_BLOCKFROST_KEY_FALLBACK: "",
  L1_OGMIOS_KEY: "http://127.0.0.1:1337",
  L1_KUPO_KEY: "http://127.0.0.1:1442",
  L1_OPERATOR_SEED_PHRASE:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  L1_REFERENCE_SCRIPT_SEED_PHRASE:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  L1_REFERENCE_SCRIPT_ADDRESS:
    "addr_test1qrtuk9vwzuyj2ly4fp2e0fdc36xzk7j2n34jeygl9n38yce7qqpmjursw62tm3acwl6z2kw38cxau355ukc9cxqyhk0sjy7d2s",
  NETWORK: "Preprod",
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: "5432",
  POSTGRES_USER: "postgres",
  POSTGRES_PASSWORD: "postgres",
  POSTGRES_DB: "midgard",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_C:
    "cactus chalk grit reopen true slight whale sand law sibling silver fringe cement twist process bracket history leopard churn federal coral three hockey fossil",
};

for (const [key, value] of Object.entries(TEST_ENV_DEFAULTS)) {
  if (process.env[key] === undefined || process.env[key] === "") {
    process.env[key] = value;
  }
}

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );
