import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { Command } from "commander";
import { expect, it, vi } from "vitest";

const generators = vi.hoisted(() => ({
  identities: vi.fn(),
  artifacts: vi.fn(),
}));
vi.mock("../src/devnet-stack/identities.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/identities.js")>()),
  loadIdentities: generators.identities,
}));
vi.mock("../src/devnet-stack/deploy.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/deploy.js")>()),
  ensureArtifacts: generators.artifacts,
}));
vi.mock("../src/devnet-stack/dist-freshness.js", () => ({
  codeStamp: () => "fixture-code",
  requireFreshDists: () => {},
  runtimeDistTargets: () => [],
}));
vi.mock("../src/devnet-stack/stack.js", () => ({
  recordedOneShot: () => ({}),
  runningSupervisor: () => 123,
  supervisorRuns: () => true,
}));
vi.mock("../src/devnet-stack/services.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/services.js")>()),
  serviceSpecs: () => [
    {
      name: "role",
      command: process.execPath,
      args: [],
      cwd: process.cwd(),
      env: {},
      readyUrl: "http://127.0.0.1:1/readyz",
    },
  ],
}));

import {
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import { makeLayout } from "../src/devnet-stack/layout.js";
import { acquireLock } from "../src/devnet-stack/lock.js";
import { registerServiceRecovery } from "../src/devnet-stack/service-recovery-cli.js";
import { refuseService } from "../src/devnet-stack/service-refusal.js";
import { supervisorPaths } from "../src/devnet-stack/services.js";

const snapshot = (directory: string): Record<string, string> =>
  Object.fromEntries(
    readdirSync(directory, { recursive: true, withFileTypes: true })
      .filter((entry) => entry.isFile())
      .map((entry) => {
        const path = join(entry.parentPath, entry.name);
        return [path, readFileSync(path).toString("hex")];
      }),
  );

it.each([
  "identities",
  "blueprint",
  "build-record",
  "owner",
  "chain-sync",
  "manifest",
])(
  "refuses missing recorded %s before any generation or durable mutation, even for an invalid token",
  async (missing) => {
    const directory = mkdtempSync(join(tmpdir(), "devnet-recovery-existing-"));
    const layout = makeLayout(directory);
    const syntheticIdentities = {
      schemaVersion: "midgard-devnet-identities-v1",
      seeds: Object.fromEntries(
        WALLET_ROLES.map((role) => [
          role,
          "synthetic placeholder never derived",
        ]),
      ),
      libp2p: Object.fromEntries(
        LIBP2P_IDENTITIES.map((role) => [role, "synthetic placeholder"]),
      ),
      adminApiKey: "synthetic placeholder",
      publicReaderPassword: "synthetic placeholder",
    };
    const artifacts = {
      nativeOwnerBinary: join(layout.bin, "architecture-g-owner"),
      nativeOwnerSha256: "fixture",
      transportBinary: join(layout.bin, "midgard-l1-node-transport"),
    };
    const files = {
      identities: layout.identities,
      blueprint: layout.blueprint,
      "build-record": `${layout.blueprint}.deployment.json`,
      owner: artifacts.nativeOwnerBinary,
      "chain-sync": artifacts.transportBinary,
      manifest: layout.contractManifest,
    };
    for (const [name, path] of Object.entries(files)) {
      if (name === missing) continue;
      mkdirSync(dirname(path), { recursive: true });
      writeFileSync(
        path,
        name === "identities" ? JSON.stringify(syntheticIdentities) : "{}",
      );
    }
    mkdirSync(layout.state, { recursive: true });
    writeFileSync(
      layout.runEnv,
      `MIDGARD_PHASE4_RUN_DIR=${directory}\nMIDGARD_PHASE4_RUN_ID=fixture\nMIDGARD_PHASE4_COMPOSE_PROJECT=fixture\nMIDGARD_PHASE4_NETWORK_MAGIC=42\nMIDGARD_PHASE4_OGMIOS_PORT=2337\nMIDGARD_PHASE4_KUPO_PORT=1442\nMIDGARD_PHASE4_POSTGRES_PORT=5433\nMIDGARD_PHASE4_POSTGRES_USER=fixture\nMIDGARD_PHASE4_POSTGRES_PASSWORD=synthetic-placeholder\nMIDGARD_PHASE4_POSTGRES_DATABASE=fixture\nMIDGARD_PHASE4_CARDANO_NODE_IMAGE=fixture\nMIDGARD_PHASE4_POSTGRES_IMAGE=fixture\n`,
    );
    refuseService(supervisorPaths({ layout }), "role");
    // The old CLI's generation seams are stubbed: no credential material is generated.
    generators.identities.mockImplementation(() => {
      if (!existsSync(layout.identities))
        writeFileSync(layout.identities, "generation marker");
      return syntheticIdentities;
    });
    generators.artifacts.mockImplementation(() => {
      for (const [name, path] of Object.entries(files)) {
        if (name !== "identities" && !existsSync(path)) {
          mkdirSync(dirname(path), { recursive: true });
          writeFileSync(path, "copy marker");
        }
      }
      return artifacts;
    });
    generators.identities.mockClear();
    generators.artifacts.mockClear();
    // Existing runs retain their administrative mutex after controller release.
    const release = acquireLock(layout.lock);
    release();
    const before = snapshot(directory);
    const program = new Command();
    registerServiceRecovery(program);
    try {
      const error = await program
        .parseAsync(
          [
            "recover-service",
            "--run-dir",
            directory,
            "--service",
            "role",
            "--refusal-id",
            "invalid-token",
            "--note",
            "synthetic test",
          ],
          { from: "user" },
        )
        .catch((error: unknown) => error);
      expect(snapshot(directory)).toEqual(before);
      expect(error).toMatchObject({
        message: expect.stringMatching(/existing deployment state missing/),
      });
      expect(generators.identities).not.toHaveBeenCalled();
      expect(generators.artifacts).not.toHaveBeenCalled();
      expect(snapshot(directory)).toEqual(before);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  },
);
