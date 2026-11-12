import { ConfigProvider, Effect } from "effect";

import { makeConfig } from "../../src/services/config.make-config.js";
import type { NodeConfigDep } from "../../src/services/config.node-config-dep.js";
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

/**
 * A complete node configuration for a test: the real parse over the suite's
 * required settings, on the deployment profile and network this test project
 * runs under and this worker's database shard, with `overrides` laid over it.
 * Every other setting holds the default an operator who leaves it unset would
 * get, so a test states only the settings it depends on and a new setting
 * needs no edit here.
 */
export const makeTestNodeConfig = (
  overrides: Partial<NodeConfigDep> = {},
): NodeConfigDep => {
  fixtureConfig ??= parseNodeConfig({
    ...TEST_ENV_DEFAULTS,
    // The setup file or the project's env sets both before any test loads.
    MIDGARD_DEPLOYMENT_PROFILE: process.env.MIDGARD_DEPLOYMENT_PROFILE!,
    NETWORK: process.env.NETWORK!,
    POSTGRES_DB: testDatabaseName(),
  });
  return { ...fixtureConfig, ...overrides };
};
