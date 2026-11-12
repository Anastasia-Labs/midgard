import { readFileSync } from "node:fs";

import dotenv from "dotenv";
import { describe, expect, it } from "vitest";

import { resolveValidationWorkerPoolSize } from "../src/services/config.js";
import { parseNodeConfig } from "./helpers/test-node-config.js";
import { TEST_ENV_DEFAULTS } from "./test-env.js";

const ENV_EXAMPLE = dotenv.parse(
  readFileSync(new URL("../.env.example", import.meta.url), "utf8"),
);

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
});
