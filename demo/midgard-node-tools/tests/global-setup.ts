/**
 * Provision the tooling suite's Postgres shards once per run.
 *
 * The shard scheme, env defaults, and migration runner are midgard-node's
 * (`tests/test-env.ts`, `tests/global-setup.ts`); this package only selects a
 * distinct database prefix (see vitest.config.ts) so its shards never collide
 * with a midgard-node run on the same server.
 */
import { provisionMidgardNodeTestDatabaseShards } from "midgard-node/tests/global-setup";

export const setup = async (): Promise<void> => {
  if (process.env.MIDGARD_SKIP_DB_TESTS === "1") {
    return;
  }
  await provisionMidgardNodeTestDatabaseShards();
};
