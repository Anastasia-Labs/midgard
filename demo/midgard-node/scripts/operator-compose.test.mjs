import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { worktreeIdentity } from "../../../scripts/lib/worktree-identity.mjs";
import {
  MAIN_PROJECT_NAME,
  OPERATOR_HOST_PORTS,
  PORT_BLOCK_BASE,
  PORT_BLOCK_SIZE,
  PORT_BLOCK_SLOTS,
  composeDirectory,
  composeEnvFiles,
  operatorComposeVariables,
  readEnvFile,
  worktreePortBlock,
} from "./operator-compose.mjs";

// Today's operator stack, as the main checkout renders it. The live session
// in the main checkout relies on these exact values; change them only on
// purpose.
const MAIN_CONTAINER_NAMES = {
  prometheus: "prometheus",
  loki: "loki",
  promtail: "promtail",
  grafana: "grafana",
};
const MAIN_PUBLISHED_PORTS = {
  "midgard-node": [3000, 9464, 39002],
  postgres: [5433],
  prometheus: [9090],
  loki: [3100],
  cadvisor: [8080],
  grafana: [3001],
  tempo: [14268, 3200, 9095, 4317, 4318, 9411],
  "cardano-node": [12788, 12798],
  "cardano-node-ogmios": [1337],
  kupo: [1442],
};
const MAIN_PORTS = Object.values(MAIN_PUBLISHED_PORTS).flat();

const sha8 = (text) =>
  createHash("sha256").update(text).digest("hex").slice(0, 8);

// Two linked worktree paths whose hashes land in different port blocks. The
// temporary directories below are random, so each linked identity takes its
// hash from one of these fixed paths and the test never depends on chance.
const FIRST_PATH = "/work/midgard-env";
const SECOND_PATH = "/work/midgard-lint";

// A main checkout (`.git` directory) and two linked worktrees (`.git` file).
const withCheckouts = (callback) => {
  const base = realpathSync(mkdtempSync(join(tmpdir(), "operator-compose-")));
  try {
    const main = join(base, "midgard");
    const first = join(base, "midgard-env");
    const second = join(base, "midgard-lint");
    mkdirSync(join(main, ".git"), { recursive: true });
    for (const linked of [first, second]) {
      mkdirSync(linked);
      writeFileSync(join(linked, ".git"), `gitdir: ${main}/.git/worktrees/x\n`);
    }
    return callback({
      main: worktreeIdentity(main),
      first: { ...worktreeIdentity(first), hash: sha8(FIRST_PATH) },
      second: { ...worktreeIdentity(second), hash: sha8(SECOND_PATH) },
    });
  } finally {
    rmSync(base, { recursive: true, force: true });
  }
};

const derivedPorts = (variables) =>
  OPERATOR_HOST_PORTS.map(({ variable }) => Number(variables[variable]));

test("the main checkout gets no variables, whatever it is asked", () => {
  withCheckouts(({ main }) => {
    assert.equal(main.isMainCheckout, true);
    assert.deepEqual(operatorComposeVariables({ identity: main }), {});
    assert.deepEqual(
      operatorComposeVariables({
        identity: main,
        args: ["-f", "docker-compose.yaml", "up", "-d"],
      }),
      {},
    );
  });
});

test("two linked worktrees get different projects and disjoint ports", () => {
  withCheckouts(({ first, second }) => {
    assert.equal(first.isMainCheckout, false);
    assert.equal(second.isMainCheckout, false);
    const a = operatorComposeVariables({ identity: first });
    const b = operatorComposeVariables({ identity: second });
    assert.equal(worktreePortBlock(first), 25100);
    assert.equal(worktreePortBlock(second), 29000);
    assert.equal(a.COMPOSE_PROJECT_NAME, `${MAIN_PROJECT_NAME}-${first.hash}`);
    assert.equal(b.COMPOSE_PROJECT_NAME, `${MAIN_PROJECT_NAME}-${second.hash}`);
    assert.notEqual(a.COMPOSE_PROJECT_NAME, b.COMPOSE_PROJECT_NAME);
    assert.equal(
      a.MIDGARD_OPERATOR_CONTAINER_PREFIX,
      `${a.COMPOSE_PROJECT_NAME}-`,
    );
    const portsA = derivedPorts(a);
    const portsB = derivedPorts(b);
    assert.equal(new Set(portsA).size, OPERATOR_HOST_PORTS.length);
    for (const port of portsA) {
      assert.ok(!portsB.includes(port), `port ${port} in both worktrees`);
      assert.ok(!MAIN_PORTS.includes(port), `port ${port} is a main port`);
    }
  });
});

test("no worktree block reaches a main-checkout port or the next block", () => {
  assert.ok(OPERATOR_HOST_PORTS.length <= PORT_BLOCK_SIZE);
  const lowest = PORT_BLOCK_BASE;
  const highest = PORT_BLOCK_BASE + PORT_BLOCK_SIZE * PORT_BLOCK_SLOTS - 1;
  assert.ok(highest < 32768, "blocks must stay below the ephemeral range");
  for (const port of MAIN_PORTS) {
    assert.ok(port < lowest || port > highest, `${port} inside the blocks`);
  }
  assert.deepEqual(
    OPERATOR_HOST_PORTS.map(({ mainPort }) => mainPort).sort(),
    [...MAIN_PORTS].sort(),
  );
});

test("an explicit setting wins over the derived one", () => {
  withCheckouts(({ first }) => {
    const fromEnvironment = operatorComposeVariables({
      identity: first,
      environment: { OGMIOS_PORT: "4000", COMPOSE_PROJECT_NAME: "mine" },
    });
    assert.equal(fromEnvironment.OGMIOS_PORT, undefined);
    assert.equal(fromEnvironment.COMPOSE_PROJECT_NAME, undefined);
    assert.equal(fromEnvironment.MIDGARD_OPERATOR_CONTAINER_PREFIX, "mine-");
    assert.ok(fromEnvironment.KUPO_PORT !== undefined);

    const fromEnvFile = operatorComposeVariables({
      identity: first,
      envFile: { MIDGARD_POSTGRES_HOST_PORT: "6543", KUPO_PORT: "" },
      args: ["-p", "other", "up"],
    });
    assert.equal(fromEnvFile.MIDGARD_POSTGRES_HOST_PORT, undefined);
    assert.ok(fromEnvFile.KUPO_PORT !== undefined, "empty counts as unset");
    assert.equal(fromEnvFile.COMPOSE_PROJECT_NAME, undefined);
    assert.equal(fromEnvFile.MIDGARD_OPERATOR_CONTAINER_PREFIX, "other-");
  });
});

test("env files: .env by default, --env-file when given", () => {
  const base = realpathSync(mkdtempSync(join(tmpdir(), "operator-env-")));
  try {
    assert.deepEqual(composeEnvFiles(["up"], base), [join(base, ".env")]);
    assert.deepEqual(
      composeEnvFiles(["--env-file", "a.env", "--env-file=b.env"], base),
      [join(base, "a.env"), join(base, "b.env")],
    );
    writeFileSync(
      join(base, ".env"),
      "# comment\nexport KUPO_PORT=5000\nPORT='3005'\nEMPTY=\n",
    );
    assert.deepEqual(readEnvFile(join(base, ".env")), {
      KUPO_PORT: "5000",
      PORT: "3005",
      EMPTY: "",
    });
    assert.deepEqual(readEnvFile(join(base, "missing.env")), {});
  } finally {
    rmSync(base, { recursive: true, force: true });
  }
});

test("the CLI prints what it derives for this checkout", () => {
  const result = spawnSync(
    fileURLToPath(new URL("./operator-compose.sh", import.meta.url)),
    ["--print-env", "--env-file", "/nonexistent/operator-compose.env"],
    { encoding: "utf8", env: { PATH: process.env.PATH } },
  );
  assert.equal(result.status, 0, result.stderr);
  const expected = operatorComposeVariables({
    identity: worktreeIdentity(composeDirectory),
  });
  assert.equal(
    result.stdout,
    Object.entries(expected)
      .map(([name, value]) => `${name}=${value}\n`)
      .join(""),
  );
});

// Render the real compose files, from a copy of this directory named
// `midgard-node` so compose picks today's default project name, with
// `.env.example` standing in for the operator's `.env`.
const dockerCompose = spawnSync("docker", ["compose", "version"]);
const dockerMissing = dockerCompose.status !== 0 && !process.env.CI;

const render = (variables) => {
  const base = realpathSync(mkdtempSync(join(tmpdir(), "operator-render-")));
  try {
    const directory = join(base, MAIN_PROJECT_NAME);
    mkdirSync(directory);
    for (const file of [
      "docker-compose.yaml",
      "docker-compose.kupmios.yaml",
      "docker-compose.benchmark.yaml",
      "docker-compose.dev.yaml",
      ".env.example",
    ]) {
      copyFileSync(join(composeDirectory, file), join(directory, file));
    }
    copyFileSync(join(directory, ".env.example"), join(directory, ".env"));
    const environment = { PATH: process.env.PATH, HOME: process.env.HOME };
    const run = (files) => {
      const result = spawnSync(
        "docker",
        [
          "compose",
          ...files.flatMap((file) => ["-f", file]),
          "--profile",
          "observability",
          "--profile",
          "test",
          "config",
          "--format",
          "json",
        ],
        {
          cwd: directory,
          encoding: "utf8",
          env: { ...environment, ...variables },
        },
      );
      assert.equal(result.status, 0, result.stderr);
      return JSON.parse(result.stdout);
    };
    return {
      full: run([
        "docker-compose.yaml",
        "docker-compose.kupmios.yaml",
        "docker-compose.benchmark.yaml",
      ]),
      dev: run(["docker-compose.dev.yaml"]),
    };
  } finally {
    rmSync(base, { recursive: true, force: true });
  }
};

const published = (config) =>
  Object.fromEntries(
    Object.entries(config.services)
      .filter(([, service]) => (service.ports ?? []).length > 0)
      .map(([name, service]) => [
        name,
        service.ports.map(({ published: port }) => Number(port)),
      ]),
  );

const containerNames = (config) =>
  Object.fromEntries(
    Object.entries(config.services)
      .filter(([, service]) => service.container_name !== undefined)
      .map(([name, service]) => [name, service.container_name]),
  );

test(
  "rendered: the main checkout keeps today's names and ports",
  { skip: dockerMissing && "docker compose is not available" },
  () => {
    withCheckouts(({ main }) => {
      const { full, dev } = render(
        operatorComposeVariables({ identity: main }),
      );
      assert.equal(full.name, MAIN_PROJECT_NAME);
      assert.deepEqual(containerNames(full), MAIN_CONTAINER_NAMES);
      assert.deepEqual(published(full), MAIN_PUBLISHED_PORTS);
      assert.equal(dev.name, MAIN_PROJECT_NAME);
      assert.deepEqual(published(dev), {
        "midgard-node": [3000],
        postgres: [5433],
      });
    });
  },
);

test(
  "rendered: a linked worktree moves every name and host port",
  { skip: dockerMissing && "docker compose is not available" },
  () => {
    withCheckouts(({ first, second }) => {
      const a = operatorComposeVariables({ identity: first });
      const b = operatorComposeVariables({ identity: second });
      const renderedA = render(a);
      const renderedB = render(b);
      assert.equal(renderedA.full.name, a.COMPOSE_PROJECT_NAME);
      assert.equal(renderedA.dev.name, a.COMPOSE_PROJECT_NAME);
      for (const [service, name] of Object.entries(
        containerNames(renderedA.full),
      )) {
        assert.equal(name, `${a.COMPOSE_PROJECT_NAME}-${service}`);
      }
      const portsA = Object.values(published(renderedA.full)).flat();
      const portsB = Object.values(published(renderedB.full)).flat();
      assert.deepEqual([...portsA].sort(), derivedPorts(a).sort());
      for (const port of [
        ...portsA,
        ...Object.values(published(renderedA.dev)).flat(),
      ]) {
        assert.ok(!MAIN_PORTS.includes(port), `${port} still hard-coded`);
        assert.ok(!portsB.includes(port), `${port} shared with the other`);
      }
    });
  },
);
