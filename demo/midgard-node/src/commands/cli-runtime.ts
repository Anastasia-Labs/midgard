/**
 * CLI runtime glue shared by the operator binary (`src/index.ts`) and the
 * tooling binary in `midgard-node-tools`: option parsers, JSON output, the
 * Effect runtime entry, the service-layer providers each command family needs,
 * and the operational wallet-isolation guard.
 *
 * Nothing here registers a command. Keep it free of test-tooling concerns so
 * the operator binary never carries e2e, stress, or benchmark behavior.
 */
import { NodeRuntime } from "@effect/platform-node";
import { getAddressDetails, type Network } from "@lucid-evolution/lucid";
import { Effect, pipe } from "effect";

import type { E2EEnvInheritance } from "../e2e/env.js";
import * as Services from "../services/index.js";
import { formatJson } from "./command-utils.js";

export const parsePositiveIntegerOption = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer`);
  }
  return parsed;
};

export const parseNonNegativeIntegerOption = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer`);
  }
  return parsed;
};

export const parsePositiveBigIntOption = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer`);
  }
  const parsed = BigInt(value);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero`);
  }
  return parsed;
};

export const collectStringOption = (
  value: string,
  previous: string[] = [],
): string[] => [...previous, value];

export const parseStringListOption = (
  values: unknown,
  label: string,
): string[] =>
  Array.isArray(values)
    ? values.map((value) => {
        if (typeof value !== "string" || value.length === 0) {
          throw new Error(`${label} must be a non-empty string.`);
        }
        return value;
      })
    : [];

export const parseE2EEnvInheritanceOption = (
  value: unknown,
): E2EEnvInheritance | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (value === "process" || value === "none") {
    return value;
  }
  throw new Error("--env-inheritance must be process or none");
};

export const expectedNetworkIdForAddress = (
  network: Network,
): number | undefined => {
  if (network === "Mainnet") {
    return 1;
  }
  if (network === "Preprod" || network === "Preview") {
    return 0;
  }
  return undefined;
};

export const parseL1AddressOption = (
  value: unknown,
  label: string,
  network: Network,
): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty Cardano address`);
  }
  const normalized = value.trim();
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(normalized);
  } catch (cause) {
    throw new Error(`Invalid ${label} "${normalized}": ${String(cause)}`);
  }
  const expectedNetworkId = expectedNetworkIdForAddress(network);
  if (
    expectedNetworkId !== undefined &&
    details.networkId !== expectedNetworkId
  ) {
    throw new Error(`${label} must target the configured ${network} network`);
  }
  return details.address.bech32;
};

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export const failCli = (label: string, error: unknown): void => {
  console.error(`${label}: ${errorMessage(error)}`);
  process.exitCode = 1;
};

export const writeJson = (value: unknown): void => {
  process.stdout.write(`${formatJson(value)}\n`);
};

export function tapJson(): <A, E, R>(
  effect: Effect.Effect<A, E, R>,
) => Effect.Effect<A, E, R>;

export function tapJson<A>(
  project: (value: A) => unknown,
): <E, R>(effect: Effect.Effect<A, E, R>) => Effect.Effect<A, E, R>;

export function tapJson<A>(project?: (value: A) => unknown) {
  return <E, R>(effect: Effect.Effect<A, E, R>) =>
    effect.pipe(
      Effect.tap((value: A) =>
        Effect.sync(() =>
          writeJson(project === undefined ? value : project(value)),
        ),
      ),
    );
}

export const runCliEffect = <A, E>(
  effect: Effect.Effect<A, E, never>,
): void => {
  NodeRuntime.runMain(effect, { teardown: undefined });
};

export const provideTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Services.NodeConfig | Services.MidgardContracts | Services.Lucid
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.MidgardContracts.Default),
    Effect.provide(Services.Lucid.Default),
  );

export const provideReferenceScriptDeploymentServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Services.NodeConfig | Services.Lucid | Services.AlwaysSucceedsContract
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.AlwaysSucceedsContract.Default),
    Effect.provide(Services.Lucid.Default),
  );

export const provideLucidOnlyServices = <A, E>(
  effect: Effect.Effect<A, E, Services.NodeConfig | Services.Lucid>,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Lucid.Default),
  );

export const provideDatabaseServices = <A, E>(
  effect: Effect.Effect<A, E, Services.Database>,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> => pipe(effect, Effect.provide(Services.Database.layer));

export const provideNodeRuntimeServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
    | Services.AdmissionWriter
    | Services.AdmissionSql
    | Services.BatchSql
    | Services.WriteBehind
    | Services.ContractDeploymentIdentity
    | Services.MidgardContracts
    | Services.Lucid
    | Services.Globals
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(Services.AdmissionWriterLive),
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(Services.Lucid.Default),
    Effect.provide(Services.Globals.Default),
  );

export const provideDatabaseTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Database
    | Services.WriteBehind
    | Services.ContractDeploymentIdentity
    | Services.MidgardContracts
    | Services.Lucid
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(Services.Lucid.Default),
  );

export const assertUserCliWalletIsOperationallyIsolated = ({
  commandName,
  walletAddress,
  operatorMainAddress,
  operatorMergeAddress,
  referenceScriptsAddress,
}: {
  readonly commandName: string;
  readonly walletAddress: string;
  readonly operatorMainAddress: string;
  readonly operatorMergeAddress: string;
  readonly referenceScriptsAddress: string;
}): void => {
  const conflictingRoles = [
    ["operator-main", operatorMainAddress],
    ["operator-merge", operatorMergeAddress],
    ["reference-scripts", referenceScriptsAddress],
  ]
    .filter(([, address]) => address === walletAddress)
    .map(([role]) => role);
  if (conflictingRoles.length > 0) {
    throw new Error(
      `${commandName} requires a user wallet that is distinct from operational node wallets; conflicting roles=${conflictingRoles.join(",")}, address=${walletAddress}`,
    );
  }
};
