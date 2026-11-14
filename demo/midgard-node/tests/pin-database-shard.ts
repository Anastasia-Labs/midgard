/**
 * Pin every test file to its worker's database shard, whatever it imports.
 *
 * The `setupFiles` entry of the node and node-tools Vitest configs. It runs in
 * the worker before each file loads, after Vitest has set `VITEST_POOL_ID`,
 * so no file inherits the `_w0` database the main process's global setup
 * leaves in the environment. It names the shard and connects to nothing, so a
 * `MIDGARD_SKIP_DB_TESTS=1` run, which provisions no shard, needs no Postgres
 * here either.
 */
import { applyMidgardNodeTestEnv } from "./test-env.js";

applyMidgardNodeTestEnv();
