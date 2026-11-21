#!/usr/bin/env node

// Run the operator compose stack (docker-compose*.yaml beside this directory)
// so two checkouts' stacks can run at once.
//
// Compose names a stack after its directory, `midgard-node`, and the compose
// files publish fixed host ports, so a second checkout's `docker compose up`
// takes over the first one's containers or fails on its ports. Following
// `scripts/lib/worktree-identity.mjs`:
//
// - the MAIN checkout gets nothing from this script. Its project name,
//   container names and host ports stay exactly what the compose files give
//   with no variables set, so running `docker compose` directly there is
//   unchanged too;
// - a LINKED worktree gets project `midgard-node-<hash>`, container names
//   prefixed with `<project>-`, and every host port moved into its own block
//   of 100 ports starting at 20000 + 100 * slot, where the slot comes from the
//   worktree hash;
// - a variable already set in the environment or in the env file compose
//   reads (`.env`, or each `--env-file`), and `-p`/`--project-name`, always
//   win over the derived value.
//
// A per-service port block, not phase 4's single offset: these defaults
// differ by multiples of 10 (12788 and 12798, 3100 and 3200), so one offset
// per worktree lands one worktree's port on another's. Blocks from 20000 to
// 29999 sit below every default here and below the Linux ephemeral range.
// Two worktrees whose hashes share a slot (1 in 100) still collide; set the
// port variables by hand for one of them.
//
// CLI (from any directory; `-f` paths resolve against demo/midgard-node):
//   demo/midgard-node/scripts/operator-compose.sh <docker compose args>
//   demo/midgard-node/scripts/operator-compose.sh --print-env [args]
// `--print-env` prints the variables it would set, one KEY=VALUE per line
// (nothing in the main checkout), and runs nothing. Calling this file with
// node directly needs a `--` before the arguments: node reads `--env-file`
// anywhere on its command line as its own option.

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, realpathSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { worktreeIdentity } from "../../../scripts/lib/worktree-identity.mjs";

export const composeDirectory = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "..",
);

/** The project name compose derives from the directory today. */
export const MAIN_PROJECT_NAME = "midgard-node";

/**
 * Every host port the operator compose files publish, in block order, with
 * the value the main checkout keeps. `docker-compose.yaml`,
 * `docker-compose.dev.yaml` and `docker-compose.kupmios.yaml` read each
 * variable as `${VARIABLE:-mainPort}`; the API port defaults to `${PORT}`.
 * Append new ports at the end so existing worktrees keep their numbers.
 */
export const OPERATOR_HOST_PORTS = [
  { variable: "MIDGARD_NODE_API_HOST_PORT", mainPort: 3000 },
  { variable: "MIDGARD_NODE_METRICS_HOST_PORT", mainPort: 9464 },
  { variable: "MIDGARD_NODE_DA_HOST_PORT", mainPort: 39002 },
  { variable: "MIDGARD_POSTGRES_HOST_PORT", mainPort: 5433 },
  { variable: "MIDGARD_PROMETHEUS_HOST_PORT", mainPort: 9090 },
  { variable: "MIDGARD_LOKI_HOST_PORT", mainPort: 3100 },
  { variable: "MIDGARD_CADVISOR_HOST_PORT", mainPort: 8080 },
  { variable: "MIDGARD_GRAFANA_HOST_PORT", mainPort: 3001 },
  { variable: "MIDGARD_TEMPO_JAEGER_HOST_PORT", mainPort: 14268 },
  { variable: "MIDGARD_TEMPO_HTTP_HOST_PORT", mainPort: 3200 },
  { variable: "MIDGARD_TEMPO_GRPC_HOST_PORT", mainPort: 9095 },
  { variable: "MIDGARD_TEMPO_OTLP_GRPC_HOST_PORT", mainPort: 4317 },
  { variable: "MIDGARD_TEMPO_OTLP_HTTP_HOST_PORT", mainPort: 4318 },
  { variable: "MIDGARD_TEMPO_ZIPKIN_HOST_PORT", mainPort: 9411 },
  { variable: "OGMIOS_PORT", mainPort: 1337 },
  { variable: "KUPO_PORT", mainPort: 1442 },
  { variable: "CARDANO_NODE_EKG_PORT", mainPort: 12788 },
  { variable: "CARDANO_NODE_PROM_PORT", mainPort: 12798 },
];

export const PORT_BLOCK_BASE = 20000;
export const PORT_BLOCK_SIZE = 100;
export const PORT_BLOCK_SLOTS = 100;

/** The first host port of a linked worktree's block. */
export const worktreePortBlock = (identity) =>
  PORT_BLOCK_BASE +
  PORT_BLOCK_SIZE *
    (Number.parseInt(identity.hash.slice(0, 6), 16) % PORT_BLOCK_SLOTS);

/** KEY=VALUE lines of a compose env file; a missing file reads as empty. */
export const readEnvFile = (path) => {
  if (!existsSync(path)) return {};
  const values = {};
  for (const line of readFileSync(path, "utf8").split(/\r?\n/u)) {
    const match = /^\s*(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=(.*)$/u.exec(
      line,
    );
    if (match === null) continue;
    values[match[1]] = match[2].trim().replace(/^(["'])(.*)\1$/u, "$2");
  }
  return values;
};

/** The env files compose reads for interpolation, given its arguments. */
export const composeEnvFiles = (args, directory = composeDirectory) => {
  const files = [];
  args.forEach((argument, index) => {
    if (argument === "--env-file" && index + 1 < args.length) {
      files.push(args[index + 1]);
    } else if (argument.startsWith("--env-file=")) {
      files.push(argument.slice("--env-file=".length));
    }
  });
  const chosen = files.length > 0 ? files : [".env"];
  return chosen.map((file) => resolve(directory, file));
};

/** The project named by `-p`/`--project-name`, if any. */
const projectFromArgs = (args) => {
  for (let index = 0; index < args.length; index += 1) {
    const argument = args[index];
    if (argument === "-p" || argument === "--project-name") {
      return args[index + 1];
    }
    if (argument.startsWith("--project-name=")) {
      return argument.slice("--project-name=".length);
    }
  }
  return undefined;
};

/**
 * The variables to add to `docker compose`'s environment. Empty for the main
 * checkout. `environment` is the caller's environment and `envFile` the
 * merged env-file values; a non-empty value in either wins over a derived one.
 */
export const operatorComposeVariables = ({
  identity,
  environment = {},
  envFile = {},
  args = [],
}) => {
  if (identity.isMainCheckout) return {};
  const given = (name) => {
    const value = environment[name] ?? envFile[name];
    return value === undefined || value === "" ? undefined : value;
  };
  const variables = {};
  let project = projectFromArgs(args) ?? given("COMPOSE_PROJECT_NAME");
  if (project === undefined) {
    project = `${MAIN_PROJECT_NAME}-${identity.hash}`;
    variables.COMPOSE_PROJECT_NAME = project;
  }
  if (given("MIDGARD_OPERATOR_CONTAINER_PREFIX") === undefined) {
    variables.MIDGARD_OPERATOR_CONTAINER_PREFIX = `${project}-`;
  }
  const block = worktreePortBlock(identity);
  OPERATOR_HOST_PORTS.forEach(({ variable }, index) => {
    if (given(variable) === undefined) {
      variables[variable] = String(block + index);
    }
  });
  return variables;
};

const main = (argv) => {
  const args = argv[0] === "--" ? argv.slice(1) : argv;
  const printOnly = args[0] === "--print-env";
  const composeArgs = printOnly ? args.slice(1) : args;
  const identity = worktreeIdentity(composeDirectory);
  const envFile = Object.assign(
    {},
    ...composeEnvFiles(composeArgs).map(readEnvFile),
  );
  const variables = operatorComposeVariables({
    identity,
    environment: process.env,
    envFile,
    args: composeArgs,
  });
  if (printOnly) {
    for (const [name, value] of Object.entries(variables)) {
      console.log(`${name}=${value}`);
    }
    return 0;
  }
  if (!identity.isMainCheckout) {
    console.error(
      `operator-compose: linked worktree ${identity.slug}: ${Object.entries(
        variables,
      )
        .map(([name, value]) => `${name}=${value}`)
        .join(" ")}`,
    );
  }
  const result = spawnSync("docker", ["compose", ...composeArgs], {
    cwd: composeDirectory,
    env: { ...process.env, ...variables },
    stdio: "inherit",
  });
  if (result.error) {
    console.error(`operator-compose: ${result.error.message}`);
    return 3;
  }
  return result.status ?? 1;
};

if (
  process.argv[1] &&
  realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
