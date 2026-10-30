import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { parse } from "dotenv";
import { afterEach, describe, expect, it, vi } from "vitest";

import { configureStackCommittee } from "../src/full-stack/committee.js";
import { loadStackConfig, parseStackConfig } from "../src/full-stack/config.js";
import { runStackController } from "../src/full-stack/controller.js";
import {
  configFixture as fixture,
  removeConfigFixtures,
} from "./full-stack-config.fixtures.js";

afterEach(removeConfigFixtures);

describe("stack intent", () => {
  it("binds deployment identity", async () => {
    const value = await fixture();
    const first = (await loadStackConfig(value.path)).intentDigest;
    expect(first).toMatch(/^[0-9a-f]{64}$/);
    value.env[value.config.wallets.user!.seedEnv] = entropyToMnemonic(
      "a2".repeat(16),
    );
    await writeFile(value.config.envFile, value.envText());
    const second = (await loadStackConfig(value.path)).intentDigest;
    expect(second).not.toBe(first);
    const watcherEnv = parse(
      await readFile(value.config.watcher.composeEnvFile),
    );
    await writeFile(watcherEnv.WATCHER_ROLLBACK_KEY_FILE!, "c".repeat(64));
    expect((await loadStackConfig(value.path)).intentDigest).not.toBe(second);
  });
  it("lets operational settings change between runs", async () => {
    const value = await fixture();
    const first = (await loadStackConfig(value.path)).intentDigest;
    value.config.timeoutMs += 1;
    value.config.journey.cycles += 1;
    value.config.wallets.user!.minimumLovelace = "900000000";
    await value.writeConfig();
    value.env.MIN_FEE_A = "11";
    await writeFile(value.config.envFile, value.envText());
    expect((await loadStackConfig(value.path)).intentDigest).toBe(first);
  });
});
it("permits measured profiles to be corrected before signing while binding the signer and programs", async () => {
  const value = await fixture();
  const first = await loadStackConfig(value.path);
  value.release.fundingProfiles.push({
    measured: "deployment-specific measurement",
  });
  await writeFile(
    value.config.watcher.releaseInput!,
    JSON.stringify(value.release),
  );
  expect((await loadStackConfig(value.path)).intentDigest).toBe(
    first.intentDigest,
  );
  value.release.programCommitments.program = "b".repeat(64);
  await writeFile(
    value.config.watcher.releaseInput!,
    JSON.stringify(value.release),
  );
  expect((await loadStackConfig(value.path)).intentDigest).not.toBe(
    first.intentDigest,
  );
});
it("rejects invalid wallet seeds before spending", async () => {
  const value = await fixture();
  value.env[value.config.wallets.user!.seedEnv] = "invalid wallet words";
  await writeFile(value.config.envFile, value.envText());
  await expect(loadStackConfig(value.path)).rejects.toThrow(
    "Invalid wallet mnemonic",
  );
});

it("assigns the same sorted committee indexes independently of input order", async () => {
  const value = await fixture();
  const first = await configureStackCommittee(value.config, value.env);
  value.config.da.members.reverse();
  const second = await configureStackCommittee(value.config, value.env);
  expect(second).toEqual(first);
  expect(first.map((member) => member.signerIndex)).toEqual([0, 1]);
  expect(first.map((member) => member.daVkey).join("")).toBe(
    value.env.DA_COMMITTEE_HEX,
  );
});

it("refuses duplicate committee keys, governed threshold violations and malformed owners before deployment", async () => {
  const value = await fixture();
  const seedEnv = value.config.da.members[1]!.seedEnv;
  const original = value.env[seedEnv]!;
  value.env[seedEnv] = value.env[value.config.da.members[0]!.seedEnv]!;
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("distinct signing keys");
  value.env[seedEnv] = original;
  value.env.DA_THRESHOLD = "1";
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("Invalid DA threshold configuration");
  value.env.DA_THRESHOLD = "2";
  value.env.DA_OWNERS_HEX = "ff";
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("Invalid DA owner configuration");
});

type Fixture = Awaited<ReturnType<typeof fixture>>;
describe("configuration refused before any build or spend", () => {
  const seedOf = (value: Fixture, role: string) =>
    value.config.wallets[role]!.seedEnv;
  const rows: [string, (value: Fixture) => void | Promise<void>, string][] = [
    [
      "a deposit that cannot cover the transfer and fees",
      ({ config }) => {
        config.journey.depositLovelace = config.journey.transferLovelace;
      },
      "Deposit must cover",
    ],
    [
      "overlapping DA ports",
      ({ config }) => {
        config.da.ports.database = config.da.ports.committeeApiBase;
      },
      "DA ports must be distinct",
    ],
    [
      "a DA port beyond TCP",
      ({ config }) => {
        config.da.ports.committeeTransportBase = 65535;
      },
      "DA ports must be distinct",
    ],
    [
      "a DA port an operator service publishes",
      ({ config }) => {
        config.da.ports.database = 1442;
      },
      "collides with KUPO_PORT",
    ],
    [
      "a DA threshold above the committee size",
      ({ env }) => {
        env.DA_THRESHOLD = "3";
      },
      "Invalid DA threshold configuration",
    ],
    [
      "a zero DA threshold",
      ({ env }) => {
        env.DA_THRESHOLD = "0";
      },
      "DA_THRESHOLD must be a positive integer",
    ],
    [
      "a non-numeric DA threshold",
      ({ env }) => {
        env.DA_THRESHOLD = "two";
      },
      "DA_THRESHOLD must be a positive integer",
    ],
    [
      "equal DA database passwords",
      ({ config, env }) => {
        env[config.da.readerPasswordEnv] = env[config.da.databasePasswordEnv]!;
      },
      "passwords must differ",
    ],
    [
      "a container file transport",
      ({ config, env }) => {
        env[config.da.members[0]!.transportEnv] = "file:/run/secrets/key";
      },
      "persistent seed/hex transport",
    ],
    [
      "a user budget below every deposit",
      ({ config }) => {
        config.wallets.user!.minimumLovelace = config.journey.depositLovelace;
      },
      "User funding budget",
    ],
    [
      "two wallet roles sharing a key",
      (value) => {
        value.env[seedOf(value, "recipient")] =
          value.env[seedOf(value, "user")]!;
      },
      "shares a payment key",
    ],
    [
      "two DA services sharing a transport identity",
      ({ config, env }) => {
        env[config.da.members[1]!.transportEnv] =
          env[config.da.members[0]!.transportEnv]!;
      },
      "distinct persistent identities",
    ],
    [
      "a preset committee that differs from the members",
      ({ env }) => {
        env.DA_COMMITTEE_HEX = "ab".repeat(32);
      },
      "differ from the configured sorted committee",
    ],
    [
      "an env file other than the node directory's",
      async ({ config, directory, envText }) => {
        config.envFile = join(directory, "other.env");
        await writeFile(config.envFile, envText());
      },
      "envFile must be the node directory's .env",
    ],
    [
      "a DA member seed that shadows a node wallet",
      ({ config, env }) => {
        config.da.members[0]!.seedEnv = "L1_OPERATOR_SEED_PHRASE";
        env.L1_OPERATOR_SEED_PHRASE = entropyToMnemonic("10".repeat(16));
      },
      "must start with STACK_",
    ],
    [
      "one variable naming two stack secrets",
      ({ config }) => {
        config.da.members[1]!.transportEnv = config.da.members[0]!.transportEnv;
      },
      "Stack secret variables must be distinct",
    ],
    [
      "a run directory inside the Docker build context",
      ({ config, directory }) => {
        config.runDirectory = join(directory, "preprod-stack");
      },
      "inside the Docker build context",
    ],
    [
      "the shared test Postgres port",
      ({ env }) => {
        env.MIDGARD_POSTGRES_HOST_PORT = "5433";
      },
      "MIDGARD_POSTGRES_HOST_PORT",
    ],
    [
      "no explicit Postgres port",
      ({ env }) => {
        delete env.MIDGARD_POSTGRES_HOST_PORT;
      },
      "MIDGARD_POSTGRES_HOST_PORT",
    ],
    [
      "a node environment that sets a host tool variable",
      ({ env }) => {
        env.NODE_OPTIONS = "--require /tmp/hook.js";
      },
      "must not set host variable NODE_OPTIONS",
    ],
  ];
  it.each(rows)("refuses %s", async (_, mutate, message) => {
    const value = await fixture();
    await mutate(value);
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(message);
  });
});

describe("--check", () => {
  it("runs the offline environment checks and refuses a shared wallet key", async () => {
    const value = await fixture();
    const log = vi.spyOn(console, "log").mockImplementation(() => {});
    try {
      await runStackController({ config: value.path, check: true });
      expect(log).toHaveBeenCalledOnce();
      value.env[value.config.wallets.recipient!.seedEnv] =
        value.env[value.config.wallets.user!.seedEnv]!;
      await writeFile(value.config.envFile, value.envText());
      await expect(
        runStackController({ config: value.path, check: true }),
      ).rejects.toThrow("shares a payment key");
    } finally {
      log.mockRestore();
    }
  });
  it("refuses a relative configuration path", () =>
    expect(
      runStackController({ config: "stack.json", check: true }),
    ).rejects.toThrow("--config must be an absolute path"));
});

describe("preprod-testing keeps its own inputs", () => {
  it("refuses a public section", async () => {
    const value = await fixture();
    value.config.public = parseStackConfig(
      JSON.parse(
        await readFile(
          new URL(
            "../config/preprod-public-stack.example.json",
            import.meta.url,
          ),
          "utf8",
        ),
      ),
    ).public;
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(
      "The public section belongs only to a public stack",
    );
  });
  it("refuses an HTTPS endpoint", async () => {
    const value = await fixture();
    value.config.endpoint = "https://127.0.0.1:3000";
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(
      "A preprod-testing node serves plain HTTP",
    );
  });
  it("accepts a WebSocket endpoint", async () => {
    const value = await fixture();
    value.config.endpoint = "ws://127.0.0.1:3000";
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).resolves.toBeDefined();
  });
  it("loads the fast-timer twin beside the default stack, on its own host ports", async () => {
    const main = await fixture();
    const twin = await fixture("preprod-testing-twin-stack.example.json");
    // The twin's .env publishes the node API on the port its endpoint names.
    twin.env.MIDGARD_NODE_API_HOST_PORT = new URL(twin.config.endpoint).port;
    await writeFile(twin.config.envFile, twin.envText());
    const loaded = await loadStackConfig(twin.path);
    expect(loaded.config.public).toBeUndefined();
    const ports = ({ config }: typeof main) => [
      new URL(config.endpoint).port || "80",
      ...Object.values(config.da.ports).map(String),
    ];
    expect(ports(twin).filter((port) => ports(main).includes(port))).toEqual(
      [],
    );
  });
});

describe("the compiled deployment profile", () => {
  it("refuses a preprod-public environment in this preprod-testing build, naming the rebuild", async () => {
    const value = await fixture();
    value.env.MIDGARD_DEPLOYMENT_PROFILE = "preprod-public";
    await writeFile(value.config.envFile, value.envText());
    for (const command of [
      () => loadStackConfig(value.path),
      () => runStackController({ config: value.path, check: true }),
    ])
      await expect(command()).rejects.toThrow(
        "This checkout is compiled for preprod-testing, but " +
          `${value.config.envFile} sets MIDGARD_DEPLOYMENT_PROFILE=preprod-public; ` +
          "build that profile first: node demo/scripts/deployment-profiles.mjs generate preprod-public && pnpm --dir demo/midgard-node-tools run build",
      );
  });
});
