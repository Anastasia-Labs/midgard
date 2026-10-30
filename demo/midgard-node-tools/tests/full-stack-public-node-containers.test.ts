import { execFile } from "node:child_process";
import { mkdir, readFile, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { promisify } from "node:util";

import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { publicRolesPath } from "../src/full-stack/process.js";
import {
  preparePublicImages,
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

/** The Compose model Docker renders from the base node file and the public overlay. */
async function renderedCompose(roles: string) {
  const { stdout } = await promisify(execFile)(
    "docker",
    [
      "compose",
      "--project-directory",
      nodeRoot,
      "-f",
      join(nodeRoot, "docker-compose.yaml"),
      "-f",
      roles,
      "config",
      "--format",
      "json",
      "--no-env-resolution",
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
      }
    >;
  };
}

describe("public node containers", () => {
  it.each([
    ["without the ingress, the node API on every interface", null, undefined],
    [
      "behind the ingress, the node API on the loopback only",
      18443,
      "127.0.0.1",
    ],
  ])(
    "binds host ports to the loopback, %s",
    async (_, ingressHostPort, apiHost) => {
      const stack = await publicStack(
        `ports-${ingressHostPort}`,
        ingressHostPort,
      );
      await preparePublicImages(stack.processes);
      const model = await renderedCompose(
        publicRolesPath(stack.processes.config),
      );
      const hosts = (service: string) =>
        Object.fromEntries(
          model.services[service]!.ports!.map((port) => [
            port.target,
            port.host_ip,
          ]),
        );
      expect(hosts("midgard-node")).toEqual({
        3000: apiHost,
        9464: "127.0.0.1",
        39002: "127.0.0.1",
      });
      expect(hosts("postgres")).toEqual({ 5432: "127.0.0.1" });
      expect(model.services["midgard-node"]!.image).toBe(ROLE_IMAGES.node);
      expect(model.services.postgres!.image).toBe(ROLE_IMAGES.postgres);
    },
  );

  it("keeps every secret out of node.env and the containers' environment", async () => {
    const stack = await publicStack("secrets", null);
    const { processes } = stack;
    const secrets = [
      ...PUBLIC_NODE_SECRET_NAMES.map((name) => processes.env[name]!),
      processes.env[processes.config.da.producerTransportEnv]!,
      ...Object.entries(processes.env)
        .filter(([name, value]) => name.startsWith("STACK_") && value !== "")
        .map(([, value]) => value),
    ];
    await preparePublicImages(processes);
    // Postgres reads its password from a private file, not its environment.
    const password = publicPostgresPasswordPath(processes.config);
    expect(await readFile(password, "utf8")).toBe(
      processes.env.POSTGRES_PASSWORD,
    );
    expect((await stat(password)).mode & 0o777).toBe(0o600);
    const model = await renderedCompose(publicRolesPath(processes.config));
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
    // The services step writes node.env before it waits for the committee.
    await stack.release.execute(undefined);
    const services = join(processes.config.runDirectory, "services");
    await mkdir(services, { recursive: true });
    await writeFile(
      join(services, "committee-0.json"),
      JSON.stringify({
        deployment: { fingerprint: fixture.manifest.manifestId },
        da_committee: { members: [{ signer_index: 0, peer_id: "peer-0" }] },
      }),
    );
    await writeFile(
      join(processes.config.runDirectory, "runtime.json"),
      JSON.stringify({
        inputDigest: await runtimeInputDigest(processes),
        compose: join(services, "compose.json"),
        operationsEndpoint: "http://127.0.0.1:1",
        committeeServices: ["da-committee-0"],
      }),
    );
    processes.responses["native-owner-pin"] = { sha: "c".repeat(64) };
    await expect(stack.services.execute(undefined)).rejects.toThrow(
      "committee readiness",
    );
    const nodeEnv = await readFile(join(services, "node.env"), "utf8");
    const rendered = JSON.stringify(model);
    for (const secret of secrets) {
      expect(nodeEnv).not.toContain(secret);
      expect(rendered).not.toContain(secret);
    }
    // The node reads them from its private configuration file instead.
    const nodeConfig = join(services, "node-config.json");
    expect((await stat(nodeConfig)).mode & 0o777).toBe(0o600);
  });
});
