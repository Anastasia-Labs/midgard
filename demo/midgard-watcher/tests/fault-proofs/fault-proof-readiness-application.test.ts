import { createHash, generateKeyPairSync } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "@al-ft/midgard-core/atomic-write";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherFaultProofReadinessApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
} from "../../src/fault-proofs/fault-proof-application.js";
import { authorWatcherDeploymentRelease } from "../../src/runtime/deployment-release-authoring.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import {
  HEADER,
  rawConfig,
  testFaultProofL1,
} from "./fault-proof-application.raw-config.js";

const readPaths = vi.hoisted(() => [] as string[]);

vi.mock("node:fs/promises", async (load) => {
  const actual = await load<typeof import("node:fs/promises")>();
  return {
    ...actual,
    readFile: (async (...args: Parameters<typeof actual.readFile>) => {
      readPaths.push(String(args[0]));
      return await actual.readFile(...args);
    }) as typeof actual.readFile,
  };
});

const BLUEPRINT_JSON = '{"preamble":{"title":"readiness-application-test"}}';
const { manifest } = makeWatcherDeploymentAuthorityFixture({
  blueprintHash: createHash("sha256").update(BLUEPRINT_JSON).digest("hex"),
}).signedIdentity;

const directories: string[] = [];
afterEach(async () => {
  readPaths.length = 0;
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const release = async () => {
  const directory = await mkdtemp("/var/tmp/midgard-readiness-application-");
  directories.push(directory);
  const paths = {
    authority: join(directory, "deployment-authority.json"),
    rules: join(directory, "rules.json"),
    funding: join(directory, "funding-profiles.json"),
    manifest: join(directory, "deployment-manifest.json"),
    blueprint: join(directory, "plutus.json"),
    deploymentInfo: join(directory, "contract-deployment-info.json"),
  };
  const { deploymentAuthority, fundingProfileOverlay } =
    await authorWatcherDeploymentRelease({
      manifest,
      blueprintJson: BLUEPRINT_JSON,
      programCommitments: { "computation-thread-policy-v1": "cd".repeat(32) },
      fundingProfiles: [],
      fundingPaymentKeyHash: "ee".repeat(28),
      signingKey: generateKeyPairSync("ed25519").privateKey,
      paths,
      existingAuthority: "refuse",
      writer: {
        replace: (path, contents) => writeTextFileAtomic(path, contents),
        create: (path, contents) =>
          writeTextFileAtomicNoReplace(path, contents),
      },
    });
  const runtimeConfigPath = join(directory, "watcher.json");
  await writeFile(runtimeConfigPath, JSON.stringify(rawConfig()));
  return {
    runtimeConfigPath,
    options: {
      l1: testFaultProofL1(),
      deploymentAuthority,
      fundingProfileOverlay,
      infrastructure: {
        manifestPath: paths.manifest,
        blueprintPath: paths.blueprint,
        deploymentInfoPath: paths.deploymentInfo,
      },
    },
  };
};

describe("watcher fault-proof readiness application", () => {
  it("admits the runtime configuration once for every category it checks", async () => {
    const { runtimeConfigPath, options } = await release();
    const application = createWatcherFaultProofReadinessApplication(options);
    const check = (
      category: (typeof WATCHER_INSTALLED_WORKFLOW_CATEGORIES)[number],
      path = runtimeConfigPath,
    ) =>
      application
        .assertStartupReady({
          mode: "resume",
          category,
          deploymentFingerprint: manifest.manifestId,
          headerHash: HEADER,
          runtimeConfigPath: path,
        })
        .catch((error: unknown) => error);
    const [first, second] = WATCHER_INSTALLED_WORKFLOW_CATEGORIES;
    await check(first!);
    await check(second!);
    expect(readPaths.filter((path) => path === runtimeConfigPath)).toEqual([
      runtimeConfigPath,
    ]);
    expect(await check(first!, `${runtimeConfigPath}.other`)).toMatchObject({
      message: "watcher readiness checks name different runtime configurations",
    });
    await application.close();
    expect(await check(first!)).toMatchObject({
      message: "watcher fault-proof application is closed",
    });
    expect(readPaths.filter((path) => path === runtimeConfigPath)).toHaveLength(
      1,
    );
  });

  it("refuses an invalid funding overlay or infrastructure at construction", async () => {
    const { options } = await release();
    expect(() =>
      createWatcherFaultProofReadinessApplication({
        ...options,
        fundingProfileOverlay: {
          ...options.fundingProfileOverlay,
          deploymentFingerprint: "00".repeat(32),
        },
      }),
    ).toThrow();
    expect(() =>
      createWatcherFaultProofReadinessApplication({
        ...options,
        infrastructure: {
          ...options.infrastructure,
          manifestPath: "deployment-manifest.json",
        },
      }),
    ).toThrow();
  });
});
