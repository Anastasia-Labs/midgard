import { availableParallelism } from "node:os";

import { Config } from "effect";

import {
  nonNegativeSafeIntegerConfig,
  positiveSafeIntegerConfig,
  stringConfig,
} from "./config.parse.js";

const NATIVE_MAX_FRAME_BYTES = 64 * 1024 * 1024;
const NATIVE_MAX_CHUNK_BYTES = 16 * 1024 * 1024;

/**
 * The ledger's Merkle Patricia forests: the root build, the ledger database
 * path and the native Architecture G owner process that holds it.
 */
export const mpfConfig = Config.all({
  MPF_SCRATCH_BUILD: Config.literal(
    "insert",
    "fromlist",
  )("MPF_SCRATCH_BUILD").pipe(Config.withDefault("insert")),
  MPF_PARALLEL_ROOTS: Config.boolean("MPF_PARALLEL_ROOTS").pipe(
    Config.withDefault(false),
  ),
  MPF_ROOT_WORKERS: positiveSafeIntegerConfig(
    "MPF_ROOT_WORKERS",
    Math.max(1, Math.min(4, availableParallelism() - 2)),
  ),
  MPF_PARALLEL_ROOT_MIN_ENTRIES: positiveSafeIntegerConfig(
    "MPF_PARALLEL_ROOT_MIN_ENTRIES",
    5_000,
  ),
  MPF_RECORD_CORPUS: stringConfig("MPF_RECORD_CORPUS", ""),
  LEDGER_MPF_DB_PATH: stringConfig(
    "LEDGER_MPF_DB_PATH",
    "midgard-ledger-mpf-db",
  ),
  MPF_NATIVE_OWNER_BINARY_PATH: stringConfig(
    "MPF_NATIVE_OWNER_BINARY_PATH",
    "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  ),
  MPF_NATIVE_OWNER_BINARY_SHA256: stringConfig(
    "MPF_NATIVE_OWNER_BINARY_SHA256",
    "",
  ),
  MPF_NATIVE_OWNER_MAX_FRAME_BYTES: positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_MAX_FRAME_BYTES",
    NATIVE_MAX_FRAME_BYTES,
  ),
  MPF_NATIVE_OWNER_MAX_CHUNK_BYTES: positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_MAX_CHUNK_BYTES",
    NATIVE_MAX_CHUNK_BYTES,
  ),
  MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS: positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS",
    120_000,
  ),
  MPF_NATIVE_OWNER_RESTART_LIMIT: nonNegativeSafeIntegerConfig(
    "MPF_NATIVE_OWNER_RESTART_LIMIT",
    3,
  ),
}).pipe(
  Config.mapAttempt((settings) => {
    const frameBytes = settings.MPF_NATIVE_OWNER_MAX_FRAME_BYTES;
    const chunkBytes = settings.MPF_NATIVE_OWNER_MAX_CHUNK_BYTES;
    const caps = `chunk_bytes=${chunkBytes.toString()},frame_bytes=${frameBytes.toString()}`;
    if (
      frameBytes > NATIVE_MAX_FRAME_BYTES ||
      chunkBytes > NATIVE_MAX_CHUNK_BYTES
    ) {
      throw new Error(
        `Architecture G RPC caps exceed the compiled native limits (${caps})`,
      );
    }
    if (chunkBytes > frameBytes - 68) {
      throw new Error(
        `Architecture G chunk cap must fit inside the RPC frame cap (${caps})`,
      );
    }
    return settings;
  }),
);
