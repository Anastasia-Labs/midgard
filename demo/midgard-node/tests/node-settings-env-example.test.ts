import { readFileSync } from "node:fs";

import dotenv from "dotenv";
import { describe, expect, it } from "vitest";

import { resolveValidationWorkerPoolSize } from "../src/services/config.js";
import { parseNodeConfig } from "./helpers/test-node-config.js";
import { TEST_ENV_DEFAULTS } from "./test-env.js";

const EXAMPLE_TEXT = readFileSync(
  new URL("../.env.example", import.meta.url),
  "utf8",
);
const ENV_EXAMPLE = dotenv.parse(EXAMPLE_TEXT);

/** Every key `.env.example` sets or shows commented out (`# KEY=`). */
const DOCUMENTED_KEYS = new Set(
  EXAMPLE_TEXT.split("\n").flatMap(
    (line) => /^(?:# )?([A-Z][A-Z0-9_]*)=/u.exec(line)?.[1] ?? [],
  ),
);

/**
 * The keys `.env.example` documents that the node settings parse does not
 * read, by their reader.
 */
const READ_ELSEWHERE: Readonly<Record<string, readonly string[]>> = {
  "docker-compose.kupmios.yaml and its bootstrap scripts": [
    "CARDANO_NODE_IMAGE_TAG",
    "CARDANO_NODE_IMAGE_DIGEST",
    "OGMIOS_IMAGE_TAG",
    "KUPO_IMAGE_TAG",
    "MITHRIL_CLIENT_IMAGE_TAG",
    "MITHRIL_AGGREGATOR_ENDPOINT",
    "MITHRIL_GENESIS_VERIFICATION_KEY_URL",
    "MITHRIL_ANCILLARY_VERIFICATION_KEY_URL",
    "MITHRIL_SNAPSHOT_DIGEST",
    "KUPO_BOOTSTRAP_MODE",
    "KUPO_SNAPSHOT_RELEASE_TAG",
    "KUPO_SNAPSHOT_URL",
    "KUPO_SNAPSHOT_MD5",
    "OGMIOS_PORT",
    "KUPO_PORT",
    "CARDANO_NODE_EKG_PORT",
    "CARDANO_NODE_PROM_PORT",
  ],
  "src/environment.ts": [
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
    "MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH",
    "MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH",
    "MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT",
    "MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE",
    "MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE",
    "MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE",
    "MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE",
    "MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE",
    "MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE",
  ],
  "the DA producer (src/da/libp2p-producer.*) and da-committee-node": [
    "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
    "DA_LIBP2P_PRIVATE_KEY_SOURCE",
  ],
  "src/services/midgard-contracts.load-reference-script-auth-validator.ts": [
    "MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES",
    "MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES",
    "MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES",
    "MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS",
  ],
  "the wallet commands (src/commands/command-utils.ts)": [
    "MIDGARD_NODE_URL",
    "USER_WALLET",
    "USER_SEED_PHRASE",
  ],
  "the midgard-node-tools devnet scripts": [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ],
};

/** The wallet seeds `.env.example` leaves empty for the operator to fill. */
const REQUIRED_SEEDS = {
  L1_OPERATOR_SEED_PHRASE: TEST_ENV_DEFAULTS.L1_OPERATOR_SEED_PHRASE!,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX:
    TEST_ENV_DEFAULTS.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX!,
  L1_REFERENCE_SCRIPT_SEED_PHRASE:
    TEST_ENV_DEFAULTS.L1_REFERENCE_SCRIPT_SEED_PHRASE!,
};

describe(".env.example", () => {
  // An operator who copies the template and fills the wallet seeds gets this
  // configuration. A change to a default, to the parse or to the template
  // changes the snapshot, so it is reviewed rather than shipped silently.
  it("resolves to the pinned node configuration", () => {
    const { VALIDATION_WORKER_POOL_SIZE, ...config } = parseNodeConfig({
      ...ENV_EXAMPLE,
      ...REQUIRED_SEEDS,
    });

    // The template leaves the pool size to the machine's CPU count.
    expect(VALIDATION_WORKER_POOL_SIZE).toBe(
      resolveValidationWorkerPoolSize(undefined),
    );
    expect(config).toMatchSnapshot();
  });

  // Every key the parse reads is documented, and every documented key is
  // read: by the parse, or by a reader listed in READ_ELSEWHERE.
  it("documents exactly the keys the node reads", () => {
    const parsed = new Set<string>();
    parseNodeConfig({ ...ENV_EXAMPLE, ...REQUIRED_SEEDS }, (key) =>
      parsed.add(key),
    );
    const elsewhere = Object.values(READ_ELSEWHERE).flat();

    expect(elsewhere.filter((key) => parsed.has(key))).toEqual([]);
    expect([...parsed, ...elsewhere].sort()).toEqual(
      [...DOCUMENTED_KEYS].sort(),
    );
  });
});
