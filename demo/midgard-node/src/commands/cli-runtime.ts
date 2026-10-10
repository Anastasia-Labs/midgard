/**
 * CLI runtime glue shared by the operator binary (`src/index.ts`) and the
 * tooling binary in `midgard-node-tools`: option parsers, JSON output, the
 * Effect runtime entry, the service-layer providers each command family needs,
 * and the operational wallet-isolation guard.
 *
 * Nothing here registers a command. Keep it free of test-tooling concerns so
 * the operator binary never carries e2e, stress, or benchmark behavior.
 *
 * Every provider here builds the Lucid service over a tool L1 access
 * (`--l1`, `l1-tool-adapter.ts`). The node's own runtime (`listen`) provides
 * its services in `listen.cli-runtime.ts`, over its follower.
 */
import { constants as osConstants } from "node:os";

import { NodeRuntime } from "@effect/platform-node";
import { getAddressDetails, type Network } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, pipe } from "effect";

import type { E2EEnvInheritance } from "../e2e/env.js";
import * as Services from "../services/index.js";
import {
  type IntentJournal,
  IntentJournalWithoutFollower,
} from "../services/intent-journal.js";
import { errorMessage } from "./cli-options.js";
import { formatJson } from "./command-utils.js";
import { ToolLucidLive } from "./l1-tool-adapter.js";
import { assertPayerIsNotOperationalWallet } from "./operational-wallet-refusal.js";

export { ToolLucidLive };

export {
  errorMessage,
  parseNonNegativeIntegerOption,
  parsePositiveIntegerOption,
} from "./cli-options.js";

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

/**
 * Exit status for a finished CLI effect. An interrupted command did not
 * complete, so it must not report success the way Effect's default teardown
 * does: a signal maps to the shell convention `128 + signal number`.
 */
export const cliExitCode = (
  exit: Exit.Exit<unknown, unknown>,
  signal: NodeJS.Signals | undefined,
): number => {
  if (Exit.isSuccess(exit)) return 0;
  if (Cause.isInterruptedOnly(exit.cause) && signal !== undefined)
    return 128 + osConstants.signals[signal];
  return 1;
};

export const runCliEffect = <A, E>(
  effect: Effect.Effect<A, E, never>,
): void => {
  let signal: NodeJS.Signals | undefined;
  const record = (received: NodeJS.Signals) => {
    signal ??= received;
  };
  process.once("SIGINT", record);
  process.once("SIGTERM", record);
  NodeRuntime.runMain(effect, {
    teardown: (exit, onExit) => onExit(cliExitCode(exit, signal)),
  });
};

export const provideTxServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.MidgardContracts
    | Services.Lucid
    | IntentJournal
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.MidgardContracts.Default),
    Effect.provide(ToolLucidLive),
  );

export const provideReferenceScriptDeploymentServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.Lucid
    | Services.AlwaysSucceedsContract
    | IntentJournal
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.AlwaysSucceedsContract.Default),
    Effect.provide(ToolLucidLive),
  );

export const provideLucidOnlyServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Services.NodeConfig | Services.Lucid | IntentJournal
  >,
): Effect.Effect<A, E | Services.ConfigError, never> =>
  pipe(
    effect,
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(ToolLucidLive),
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
    | IntentJournal
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    // A CLI process runs no follower; `runNode` provides its own journal.
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.AdmissionWriterLive),
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(ToolLucidLive),
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
    | IntentJournal
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(Services.MidgardContractServices),
    Effect.provide(ToolLucidLive),
  );

/**
 * Refuses a user command's wallet when it is one of the node's operational
 * wallets (`OperationalWalletPayerRefusedError`, `operational-wallet-refusal.ts`).
 */
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
}): void =>
  assertPayerIsNotOperationalWallet({
    command: commandName,
    payerAddress: walletAddress,
    operational: [
      { role: "operator-main", address: operatorMainAddress },
      { role: "operator-merge", address: operatorMergeAddress },
      { role: "reference-scripts", address: referenceScriptsAddress },
    ],
  });
