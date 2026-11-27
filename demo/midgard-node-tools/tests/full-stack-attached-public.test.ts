import { createHash } from "node:crypto";
import { copyFile, mkdir, readdir, readFile, rm } from "node:fs/promises";
import { join } from "node:path";

import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import { parseAttachedStackConfig } from "../src/full-stack/attached-config.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import { trustNodeTls } from "../src/full-stack/node-tls.js";
import { publicNodeServices } from "../src/full-stack/public-runtime.js";
import {
  attachedFixture,
  removeAttachedFixtures,
  runAttached,
  SHARED_RELEASE,
} from "./full-stack-attached.fixtures.js";
import { RecordingProcesses } from "./full-stack-fixtures.js";
import {
  createPublicTls,
  removePublicTls,
  tls,
} from "./full-stack-public-config.fixtures.js";
import {
  createPublicReleaseFixture,
  nodeRoot,
  removePublicReleaseFixture,
} from "./full-stack-public-release.fixtures.js";

// Runs in the `preprod-public-release` project: this build compiles the
// preprod-public profile against a blueprint built for it, which a public
// deployment manifest needs. The preprod-rehearsal profile takes the same
// public route; full-stack-attached-public-config.test.ts loads it.
let genuine: Record<string, unknown>;
beforeAll(async () => {
  await createPublicTls();
  genuine = (await createPublicReleaseFixture()).manifest as unknown as Record<
    string,
    unknown
  >;
}, 300_000);
afterAll(async () => {
  await removePublicTls();
  await removePublicReleaseFixture();
});
afterEach(removeAttachedFixtures);

/**
 * Operator-2's recorded workflow beside a deployed public stack. This build's
 * allow-list holds only the real operators' keys, so the run starts from the
 * configuration and environment loading leaves (its own node secrets and the
 * stack's release settings; full-stack-attached-public-config.test.ts).
 */
async function attachedPublicRun() {
  const fixture = await attachedFixture({ public: { manifest: genuine } });
  const read = async (path: string) =>
    JSON.parse(await readFile(path, "utf8")) as unknown;
  const config = parseAttachedStackConfig(
    await read(fixture.path),
    parseStackConfig(await read(fixture.primary.path)),
  );
  const env = { ...fixture.env };
  for (const name of SHARED_RELEASE) env[name] = fixture.primary.env[name]!;
  return runAttached(fixture, { config, env, intentDigest: "ab".repeat(32) });
}

/** Every file under a directory, with a digest of its bytes. */
async function snapshot(directory: string) {
  const entries = await readdir(directory, { recursive: true });
  return Promise.all(
    entries.sort().map(async (name) => {
      const bytes = await readFile(join(directory, name)).catch(() => null);
      return [
        name,
        bytes === null
          ? "directory"
          : createHash("sha256").update(bytes).digest("hex"),
      ];
    }),
  );
}

describe("a public stack's second-instance workflow", () => {
  it("runs operator-2's node from the stack's release image behind its own TLS ingress on loopback only, reading the release read-only", async () => {
    const { fixture, processes, run, rendered } = await attachedPublicRun();
    const inputs = fixture.primary.config.public!;
    // The controller trusts the stack's CA for the stack's origin.
    trustNodeTls(inputs.tlsCaFile, tls.host);
    const release = await snapshot(inputs.releaseDirectory);
    const journal = await run();
    expect(journal.steps.services?.status).toBe("complete");
    // It builds nothing: it runs the stack's release images.
    expect(processes.calls.map((call) => call.id)).not.toContain(
      "services-build",
    );
    const started = processes.calls
      .filter((call) => call.args.includes("up"))
      .flatMap((call) =>
        call.args
          .slice(call.args.indexOf("up") + 1)
          .filter((arg) => !arg.startsWith("-")),
      );
    expect(started).toEqual(["postgres", "midgard-node", "public-ingress"]);
    const model = rendered();
    const node = model.services["midgard-node"]!;
    const ingress = model.services["public-ingress"]!;
    expect(node.image).toBe(fixture.primary.env.MIDGARD_PUBLIC_NODE_IMAGE);
    expect(model.services.postgres!.image).toBe(
      fixture.primary.env.MIDGARD_PUBLIC_POSTGRES_IMAGE,
    );
    // Every host port is on the loopback only; the API's is the ingress's,
    // and the node itself publishes no HTTP port.
    expect(node.ports!.map((port) => port.target)).not.toContain(3000);
    expect(
      [...node.ports!, ...ingress.ports!].map((port) => port.host_ip),
    ).toEqual(["127.0.0.1", "127.0.0.1", "127.0.0.1"]);
    expect(ingress.ports!.map((port) => port.target)).toEqual([8443]);
    for (const source of [inputs.tlsCertFile, inputs.tlsKeyFile])
      expect(
        ingress.volumes!.find((volume) => volume.source === source)?.read_only,
      ).toBe(true);
    for (const source of [
      inputs.releaseDirectory,
      inputs.trustRootsFile,
      // The stack's Cardano node socket: the node connects, never writes.
      join(fixture.primary.directory, "cardano/ipc"),
    ])
      expect(
        node.volumes!.find((volume) => volume.source === source)?.read_only,
      ).toBe(true);
    // No node secret reaches node.env; and the stack's release is unchanged.
    const nodeEnv = await readFile(
      join(fixture.attached.runDirectory as string, "services/node.env"),
      "utf8",
    );
    for (const name of PUBLIC_NODE_SECRET_NAMES)
      expect(nodeEnv.includes(fixture.env[name]!)).toBe(false);
    expect(await snapshot(inputs.releaseDirectory)).toEqual(release);
  });
  it("shares L2 transactions with the stack, each node dialing the other's TLS ingress at the certificate's host name and trusting the stack CA", async () => {
    const { fixture, run, rendered } = await attachedPublicRun();
    const inputs = fixture.primary.config.public!;
    trustNodeTls(inputs.tlsCaFile, tls.host);
    await run();
    const model = rendered();
    const node = model.services["midgard-node"]!;
    const ingress = model.services["public-ingress"]!;
    expect(node.environment).toMatchObject({
      L2_TX_PEERS: `https://${tls.host}:8443`,
      NODE_EXTRA_CA_CERTS: "/run/public/tls-ca.pem",
    });
    expect(
      node.volumes!.find(
        (volume) => volume.target === "/run/public/tls-ca.pem",
      ),
    ).toMatchObject({ source: inputs.tlsCaFile, read_only: true });
    // The stack's services, as its configuration step generates them.
    const primary = fixture.primary.config;
    const primaryServices = join(primary.runDirectory, "services");
    await mkdir(join(primary.nodeRoot, "scripts"), { recursive: true });
    await copyFile(
      join(nodeRoot, "scripts/public-ingress.mjs"),
      join(primary.nodeRoot, "scripts/public-ingress.mjs"),
    );
    const stack = await publicNodeServices(
      new RecordingProcesses(primary, {
        ...fixture.primary.env,
        ...(fixture.primary as { secrets?: Record<string, string> }).secrets,
      }),
      {
        servicesDirectory: primaryServices,
        envFile: [{ path: join(primaryServices, "node.env"), required: false }],
        producer: join(primaryServices, "producer.json"),
      },
    );
    const stackNetworks = stack.networks as Record<string, { name?: string }>;
    const stackService = (name: string) =>
      (
        stack.services[name] as {
          networks?: Record<string, { aliases?: string[] }>;
        }
      ).networks ?? {};
    /** The services on one Docker network, by its name, across both projects. */
    const members = (name: string) => [
      ...Object.entries(model.services)
        .filter(([, service]) =>
          Object.keys(service.networks ?? {}).some(
            (key) => model.networks![key]!.name === name,
          ),
        )
        .map(([service]) => `operator-2 ${service}`),
      ...["midgard-node", "public-ingress"]
        .filter((service) =>
          Object.keys(stackService(service)).some(
            (key) => stackNetworks[key]!.name === name,
          ),
        )
        .map((service) => `stack ${service}`),
    ];
    const networkOf = (
      networks: Record<string, unknown>,
      names: Record<string, { name?: string }>,
      exclude: string[],
    ) =>
      Object.keys(networks)
        .filter((key) => !exclude.includes(key))
        .map((key) => names[key]!.name!);
    // Operator-2's node reaches the stack's ingress, and the stack's node
    // operator-2's: each network holds one ingress and the other's node.
    const [toStack] = networkOf(node.networks!, model.networks!, [
      "midgard_net",
    ]);
    const [toOperator2] = networkOf(ingress.networks!, model.networks!, [
      "public_ingress",
    ]);
    expect(members(toStack!).sort()).toEqual([
      "operator-2 midgard-node",
      "stack public-ingress",
    ]);
    expect(members(toOperator2!).sort()).toEqual([
      "operator-2 public-ingress",
      "stack midgard-node",
    ]);
    // Each ingress answers there to the certificate's host name.
    expect(Object.values(ingress.networks!)).toContainEqual(
      expect.objectContaining({ aliases: [tls.host] }),
    );
    expect(Object.values(stackService("public-ingress"))).toContainEqual(
      expect.objectContaining({ aliases: [tls.host] }),
    );
    // The stack's project creates both networks; operator-2's joins them.
    for (const network of Object.values(model.networks!).filter(
      ({ name }) => name === toStack || name === toOperator2,
    ))
      expect(network.external).toBe(true);
  });
  it("refuses to start before the stack signed its public release, creating nothing in its place", async () => {
    const { fixture, processes, run } = await attachedPublicRun();
    const release = fixture.primary.config.public!.releaseDirectory;
    await rm(release, { recursive: true });
    await expect(run()).rejects.toThrow(
      `The stack this instance attaches to has not signed its public release in ${release} yet`,
    );
    await expect(readdir(release)).rejects.toThrow("ENOENT");
    // Docker never saw operator-2's services: no Compose call names a file
    // of its services directory.
    const services = join(fixture.attached.runDirectory as string, "services");
    expect(
      processes.calls
        .filter((call) => call.args.some((arg) => arg.startsWith(services)))
        .map((call) => call.id),
    ).toEqual([]);
  });
});
