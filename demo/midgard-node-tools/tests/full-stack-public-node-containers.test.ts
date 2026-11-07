import { execFile } from "node:child_process";
import { randomBytes } from "node:crypto";
import { appendFile, mkdir, readFile, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { promisify } from "node:util";

import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import type { StackConfig } from "../src/full-stack/config.js";
import { publicRolesPath } from "../src/full-stack/process.js";
import {
  preparePublicImages,
  publicNodeServices,
  publicPostgresPasswordPath,
} from "../src/full-stack/public-runtime.js";
import { runtimeInputDigest } from "../src/full-stack/runtime.js";
import {
  createPublicReleaseFixture,
  nodeRoot,
  publicStack,
  removePublicReleaseFixture,
  ROLE_IMAGES,
} from "./full-stack-public-release.fixtures.js";

// Runs in the `preprod-public-release` project, which compiles the
// preprod-public profile against a blueprint built for it.

let fixture: Awaited<ReturnType<typeof createPublicReleaseFixture>>;
beforeAll(async () => {
  fixture = await createPublicReleaseFixture();
}, 300_000);
afterAll(removePublicReleaseFixture);

/**
 * The Compose model Docker renders from the files the stack passes to Compose
 * (StackProcesses.compose), in the stack's node directory with its env file:
 * the base and Kupmios node files, the public overlay and, once generated, the
 * runtime services file. Each env_file is resolved into the environment a
 * container receives.
 */
async function renderedCompose(config: StackConfig, servicesFile?: string) {
  const { stdout } = await promisify(execFile)(
    "docker",
    [
      "compose",
      "--project-directory",
      config.nodeRoot,
      "--env-file",
      config.envFile,
      ...[
        join(nodeRoot, "docker-compose.yaml"),
        join(nodeRoot, "docker-compose.kupmios.yaml"),
        publicRolesPath(config),
        ...(servicesFile === undefined ? [] : [servicesFile]),
      ].flatMap((file) => ["-f", file]),
      "config",
      "--format",
      "json",
    ],
    { env: { PATH: process.env.PATH!, HOME: process.env.HOME! } },
  );
  return JSON.parse(stdout) as {
    services: Record<
      string,
      {
        image?: string;
        ports?: { host_ip?: string; target: number; published: string }[];
        environment?: Record<string, string>;
        volumes?: { source: string; target: string; read_only?: boolean }[];
        networks?: Record<string, unknown>;
      }
    >;
  };
}

/** Compose's configuration hash of one service in the files renderedCompose reads. */
async function renderedHash(
  config: StackConfig,
  servicesFile: string,
  service: string,
) {
  const { stdout } = await promisify(execFile)(
    "docker",
    [
      "compose",
      "--project-directory",
      config.nodeRoot,
      "--env-file",
      config.envFile,
      ...[
        join(nodeRoot, "docker-compose.yaml"),
        join(nodeRoot, "docker-compose.kupmios.yaml"),
        publicRolesPath(config),
        servicesFile,
      ].flatMap((file) => ["-f", file]),
      "config",
      "--hash",
      service,
    ],
    { env: { PATH: process.env.PATH!, HOME: process.env.HOME! } },
  );
  return stdout.trim();
}

describe("public node containers", () => {
  it.each([
    [
      "without public.ingressHostPort, the API port on every interface",
      null,
      undefined,
    ],
    [
      "with public.ingressHostPort, the API port on the loopback",
      18443,
      "127.0.0.1",
    ],
  ])(
    "publishes the node API only through the TLS ingress, %s",
    async (_, ingressHostPort, apiHost) => {
      const stack = await publicStack(
        `ports-${ingressHostPort}`,
        ingressHostPort,
      );
      const { config } = stack.processes;
      await preparePublicImages(stack.processes);
      const services = join(config.runDirectory, "services");
      await mkdir(services, { recursive: true });
      await writeFile(join(services, "producer.json"), "{}");
      const node = await publicNodeServices(stack.processes, {
        servicesDirectory: services,
        envFile: [{ path: join(services, "node.env"), required: false }],
        producer: join(services, "producer.json"),
      });
      const compose = join(services, "compose.json");
      await writeFile(
        compose,
        JSON.stringify({ services: node.services, networks: node.networks }),
      );
      const model = await renderedCompose(config, compose);
      const ports = (service: string) =>
        model.services[service]!.ports!.map((port) => ({
          host: port.host_ip,
          published: Number(port.published),
          target: port.target,
        }));
      // The node publishes no HTTP port: it has no TCP listener. Metrics and
      // DA stay on the loopback.
      expect(
        ports("midgard-node").map(({ host, target }) => ({ host, target })),
      ).toEqual([
        { host: "127.0.0.1", target: 9464 },
        { host: "127.0.0.1", target: 39002 },
      ]);
      expect(ports("public-ingress")).toEqual([
        {
          host: apiHost,
          published: Number(new URL(config.endpoint).port),
          target: 8443,
        },
        ...(ingressHostPort === null
          ? []
          : [{ host: undefined, published: ingressHostPort, target: 8443 }]),
      ]);
      expect(ports("postgres").map(({ host }) => host)).toEqual(["127.0.0.1"]);
      // The two share only the socket directory, private to the host user;
      // the ingress has a network of its own, with no route to the node.
      const socketDirectory = join(services, "http-socket");
      expect((await stat(socketDirectory)).mode & 0o777).toBe(0o700);
      for (const service of ["midgard-node", "public-ingress"])
        expect(model.services[service]!.volumes).toContainEqual(
          expect.objectContaining({
            source: socketDirectory,
            target: "/run/midgard-http",
          }),
        );
      expect(Object.keys(model.services["public-ingress"]!.networks!)).toEqual([
        "public_ingress",
      ]);
      expect(
        Object.keys(model.services["midgard-node"]!.networks!),
      ).not.toContain("public_ingress");
      expect(model.services["midgard-node"]!.image).toBe(ROLE_IMAGES.node);
      expect(model.services["public-ingress"]!.image).toBe(ROLE_IMAGES.ingress);
      expect(model.services.postgres!.image).toBe(ROLE_IMAGES.postgres);
    },
  );

  it("keeps every secret out of node.env and the containers' environment", async () => {
    const stack = await publicStack("secrets", null);
    const { processes } = stack;
    const { config } = processes;
    // An operator's .env copied from .env.example also names a node DA identity.
    const daIdentity = `seed:${randomBytes(32).toString("hex")}`;
    await appendFile(
      config.envFile,
      `DA_LIBP2P_PRIVATE_KEY_SOURCE=${daIdentity}\n`,
    );
    processes.env.DA_LIBP2P_PRIVATE_KEY_SOURCE = daIdentity;
    const secrets = [
      ...PUBLIC_NODE_SECRET_NAMES.map((name) => processes.env[name]!),
      processes.env[config.da.producerTransportEnv]!,
      daIdentity,
      ...Object.entries(processes.env)
        .filter(([name, value]) => name.startsWith("STACK_") && value !== "")
        .map(([, value]) => value),
    ];
    await preparePublicImages(processes);
    // Postgres reads its password from a private file, not its environment.
    const password = publicPostgresPasswordPath(config);
    expect(await readFile(password, "utf8")).toBe(
      processes.env.POSTGRES_PASSWORD,
    );
    expect((await stat(password)).mode & 0o777).toBe(0o600);
    await stack.release.execute(undefined);
    // The node's part of the services file the configuration step generates,
    // with the node.env the services step writes.
    const services = join(config.runDirectory, "services");
    await mkdir(services, { recursive: true });
    await writeFile(join(services, "producer.json"), "{}");
    const compose = join(services, "compose.json");
    const node = await publicNodeServices(processes, {
      servicesDirectory: services,
      envFile: [{ path: join(services, "node.env"), required: false }],
      producer: join(services, "producer.json"),
    });
    await writeFile(
      compose,
      JSON.stringify({ services: node.services, networks: node.networks }),
    );
    /** What the node, its migration and Postgres hold in their environment. */
    const environments = async () => {
      const model = await renderedCompose(config, compose);
      expect(model.services.postgres!.environment).toMatchObject({
        POSTGRES_PASSWORD_FILE: "/run/secrets/postgres-password",
      });
      expect(model.services.postgres!.environment).not.toHaveProperty(
        "POSTGRES_PASSWORD",
      );
      expect(model.services.postgres!.volumes).toContainEqual(
        expect.objectContaining({
          source: password,
          target: "/run/secrets/postgres-password",
          read_only: true,
        }),
      );
      return JSON.stringify(
        ["midgard-node", "midgard-node-migrate", "postgres"].map(
          (service) => model.services[service]!.environment ?? {},
        ),
      );
    };
    // The services step runs the node image for its owner pin before it
    // writes node.env.
    const beforeNodeEnv = await environments();
    await writeFile(
      join(services, "committee-0.json"),
      JSON.stringify({
        deployment: { fingerprint: fixture.manifest.manifestId },
        da_committee: { members: [{ signer_index: 0, peer_id: "peer-0" }] },
      }),
    );
    await writeFile(
      join(config.runDirectory, "runtime.json"),
      JSON.stringify({
        inputDigest: await runtimeInputDigest(processes),
        compose,
        operationsEndpoint: "http://127.0.0.1:1",
        committeeServices: ["da-committee-0"],
      }),
    );
    // It writes node.env before it waits for the committee.
    processes.responses["native-owner-pin"] = { sha: "c".repeat(64) };
    await expect(stack.services.execute(undefined)).rejects.toThrow(
      "committee readiness",
    );
    const nodeEnv = await readFile(join(services, "node.env"), "utf8");
    const afterNodeEnv = await environments();
    for (const secret of secrets) {
      expect(nodeEnv).not.toContain(secret);
      expect(beforeNodeEnv).not.toContain(secret);
      expect(afterNodeEnv).not.toContain(secret);
    }
    // The node reads them from its private configuration file instead.
    const nodeConfig = join(services, "node-config.json");
    expect((await stat(nodeConfig)).mode & 0o777).toBe(0o600);
  });

  it("recreates the node when its private node configuration changes", async () => {
    const stack = await publicStack("node-config", null);
    const { processes } = stack;
    const { config } = processes;
    await preparePublicImages(processes);
    const services = join(config.runDirectory, "services");
    await mkdir(services, { recursive: true });
    await writeFile(join(services, "producer.json"), "{}");
    const compose = join(services, "compose.json");
    /** Compose's configuration hash of the node as the configuration step generates it: `up -d` recreates its container exactly when this changes. */
    const hash = async () => {
      const node = await publicNodeServices(processes, {
        servicesDirectory: services,
        envFile: [{ path: join(services, "node.env"), required: false }],
        producer: join(services, "producer.json"),
      });
      await writeFile(
        compose,
        JSON.stringify({ services: node.services, networks: node.networks }),
      );
      return renderedHash(config, compose, "midgard-node");
    };
    const before = await hash();
    // A rotated admin key: the same mounts and image, a different node-config.json.
    processes.env.ADMIN_API_KEY = randomBytes(32).toString("hex");
    expect(await hash()).not.toBe(before);
  });
});
