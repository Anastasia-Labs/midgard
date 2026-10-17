/**
 * Provision the tooling suite's Postgres shards once per run.
 *
 * The shard scheme, env defaults, and migration runner are midgard-node's
 * (`tests/test-env.ts`, `tests/global-setup.ts`); this package only selects a
 * distinct database prefix (see vitest.config.ts) so its shards never collide
 * with a midgard-node run on the same server.
 */
import { provisionMidgardNodeTestDatabaseShards } from "midgard-node/tests/global-setup";

const ONCE = Symbol.for("midgard-node-tools/tests/global-setup");

/**
 * Vitest runs a root `globalSetup` once for the root config and once more for
 * every `extends: true` project (vitest.config.ts declares the suite as a
 * workspace-bundle project plus, when needed, a source project), all in this
 * one process. Provisioning drops and recreates the shard databases, so it
 * must happen exactly once per run: every call shares the first one's promise.
 */
export const setup = (): Promise<void> => {
  if (process.env.MIDGARD_SKIP_DB_TESTS === "1") {
    return Promise.resolve();
  }
  const registry = globalThis as { [ONCE]?: Promise<void> };
  registry[ONCE] ??= provisionMidgardNodeTestDatabaseShards();
  return registry[ONCE];
};
