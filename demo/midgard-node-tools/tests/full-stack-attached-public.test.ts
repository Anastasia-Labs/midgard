import { createHash } from "node:crypto";
import { readdir, readFile, rm } from "node:fs/promises";
import { join } from "node:path";

import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import { parseAttachedStackConfig } from "../src/full-stack/attached-config.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import { trustNodeTls } from "../src/full-stack/node-tls.js";
import {
  attachedFixture,
  removeAttachedFixtures,
  runAttached,
  SHARED_RELEASE,
} from "./full-stack-attached.fixtures.js";
import {
  createPublicTls,
  removePublicTls,
  tls,
} from "./full-stack-public-config.fixtures.js";
import {
  createPublicReleaseFixture,
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
  it("runs operator-2's node from the stack's release image on loopback only, reading the release and TLS, with no ingress", async () => {
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
    expect(started).toEqual(["postgres", "midgard-node"]);
    const model = rendered();
    expect(Object.keys(model.services)).not.toContain("public-ingress");
    const node = model.services["midgard-node"]!;
    expect(node.image).toBe(fixture.primary.env.MIDGARD_PUBLIC_NODE_IMAGE);
    expect(model.services.postgres!.image).toBe(
      fixture.primary.env.MIDGARD_PUBLIC_POSTGRES_IMAGE,
    );
    // Every host port, the API's included, is on the loopback only.
    expect(node.ports!.map((port) => port.host_ip)).toEqual([
      "127.0.0.1",
      "127.0.0.1",
      "127.0.0.1",
    ]);
    for (const source of [
      inputs.releaseDirectory,
      inputs.tlsCertFile,
      inputs.tlsKeyFile,
      inputs.tlsCaFile,
      inputs.trustRootsFile,
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
  it("refuses to start before the stack signed its public release, creating nothing in its place", async () => {
    const { fixture, run } = await attachedPublicRun();
    const release = fixture.primary.config.public!.releaseDirectory;
    await rm(release, { recursive: true });
    await expect(run()).rejects.toThrow(
      `The stack this instance attaches to has not signed its public release in ${release} yet`,
    );
    await expect(readdir(release)).rejects.toThrow("ENOENT");
  });
});
