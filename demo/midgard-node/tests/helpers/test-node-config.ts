import { ConfigProvider, Effect } from "effect";

import type { NodeConfigDep } from "../../src/services/config.js";
import { makeConfig } from "../../src/services/config.make-config.js";
import { TEST_ENV_DEFAULTS, testDatabaseName } from "../test-env.js";

/**
 * The node's real settings parse, reading its settings from `env`. Each key
 * the parse reads, set or not, is passed to `onRead`.
 */
export const parseNodeConfig = (
  env: Readonly<Record<string, string>>,
  onRead: (key: string) => void = () => {},
): NodeConfigDep => {
  const settings = ConfigProvider.fromMap(
    new Map(Object.entries(env)),
  ).flattened;
  const recording = ConfigProvider.makeFlat({
    load: (path, primitive, split) => {
      onRead(path.join("."));
      return settings.load(path, primitive, split);
    },
    enumerateChildren: (path) => settings.enumerateChildren(path),
    patch: settings.patch,
  });
  return Effect.runSync(
    makeConfig.pipe(
      Effect.withConfigProvider(ConfigProvider.fromFlat(recording)),
    ),
  );
};

let fixtureConfig: NodeConfigDep | undefined;

/** The ambient value of `key`, or the suite default when it is unset or blank. */
const ambientOrDefault = (key: string): string => {
  const value = process.env[key];
  return value === undefined || value === "" ? TEST_ENV_DEFAULTS[key]! : value;
};

/**
 * A complete node configuration for a test: the real parse over the suite's
 * required settings, on the deployment profile, network and Postgres server
 * this test project runs under and this worker's database shard, with
 * `overrides` laid over it. Every other setting holds the default an operator
 * who leaves it unset would get, so a test states only the settings it depends
 * on and a new setting needs no edit here.
 *
 * `overrides` are raw values laid over the parsed config, not settings fed to
 * the parse: a setting derived from another is not recomputed, and no check
 * across settings runs again. Overriding `L1_REFERENCE_SCRIPT_SEED_PHRASE`,
 * say, leaves `L1_REFERENCE_SCRIPT_ADDRESS` and
 * `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS` as the fixture's, and overriding the
 * wallet seeds skips the distinct-wallet check. A test that depends on a
 * derived setting overrides it too.
 */
export const makeTestNodeConfig = (
  overrides: Partial<NodeConfigDep> = {},
): NodeConfigDep => {
  fixtureConfig ??= parseNodeConfig({
    ...TEST_ENV_DEFAULTS,
    // The setup file or the project's env sets both before any test loads.
    MIDGARD_DEPLOYMENT_PROFILE: process.env.MIDGARD_DEPLOYMENT_PROFILE!,
    NETWORK: process.env.NETWORK!,
    // CI points the suite at its own server (POSTGRES_PORT=5432).
    POSTGRES_HOST: ambientOrDefault("POSTGRES_HOST"),
    POSTGRES_PORT: ambientOrDefault("POSTGRES_PORT"),
    POSTGRES_USER: ambientOrDefault("POSTGRES_USER"),
    POSTGRES_PASSWORD: ambientOrDefault("POSTGRES_PASSWORD"),
    POSTGRES_DB: testDatabaseName(),
  });
  return { ...fixtureConfig, ...overrides };
};
