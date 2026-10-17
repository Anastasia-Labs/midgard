import { generateKeyPairSync } from "node:crypto";
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { authorWatcherDeploymentRelease } from "midgard-watcher";
import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { prepareStackRelease } from "../src/full-stack/release.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

// The watcher's release authoring has its own tests; here only its use is checked.
vi.mock("midgard-watcher", async (importOriginal) => ({
  ...(await importOriginal<typeof import("midgard-watcher")>()),
  authorWatcherDeploymentRelease: vi.fn(),
  createWatcherWorkflowFundingProfileBundle: vi.fn(() => ({})),
  parseWatcherProcessConfig: vi.fn(() => ({
    deploymentAuthorityPath: "/etc/midgard/bundles/deployment-authority.json",
    ruleBundlePath: "/etc/midgard/bundles/rules.json",
    fundingProfileBundlePath: "/etc/midgard/bundles/funding-profiles.json",
    faultProofInfrastructure: {
      manifestPath: "/etc/midgard/bundles/deployment-manifest.json",
      blueprintPath: "/etc/midgard/bundles/plutus.json",
      deploymentInfoPath: "/etc/midgard/bundles/contract-deployment-info.json",
    },
  })),
}));
const author = vi.mocked(authorWatcherDeploymentRelease);

beforeEach(() => author.mockReset());
afterEach(removeStackFixtures);

const everyCategory = Object.fromEntries(
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [category, {}]),
);
const PROGRAMS = { "computation-thread-policy-v1": "cd".repeat(32) };

/** A stack whose release directory already holds a signed authority. */
async function stackWithSavedAuthority() {
  const { directory, config } = await stackFixture((sample, root) => {
    sample.watcher.releaseDirectory = join(root, "release");
    sample.watcher.releaseInput = join(root, "release-input.json");
  });
  // The node root sits two levels below the checkout that holds the blueprint.
  config.nodeRoot = join(directory, "demo/midgard-node");
  await mkdir(join(directory, "onchain/aiken"), { recursive: true });
  await writeFile(join(directory, "onchain/aiken/plutus.json"), "{}");
  await mkdir(config.watcher.releaseDirectory);
  await writeFile(
    join(config.watcher.releaseDirectory, "deployment-authority.json"),
    "{}",
  );
  const signingKeyFile = join(directory, "release.key");
  await writeFile(
    signingKeyFile,
    generateKeyPairSync("ed25519").privateKey.export({
      format: "pem",
      type: "pkcs8",
    }),
  );
  await writeFile(
    config.watcher.releaseInput!,
    JSON.stringify({
      signingKeyFile,
      programCommitments: PROGRAMS,
      fundingProfiles: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => ({
        scope: { kind: "fraud_proof_category", category },
      })),
    }),
  );
  return new RecordingProcesses(config, stackEnvironment(config));
}

it("reopens a saved release through the watcher's authoring, refusing any other signer", async () => {
  const processes = await stackWithSavedAuthority();
  author.mockRejectedValueOnce(
    new Error(
      "Saved watcher authority differs from the requested deployment or release",
    ),
  );
  await expect(prepareStackRelease(processes)).rejects.toThrow(
    "Saved watcher authority differs",
  );
  expect(author).toHaveBeenCalledExactlyOnceWith(
    expect.objectContaining({
      existingAuthority: "refuse",
      programCommitments: PROGRAMS,
      blueprintJson: Buffer.from("{}"),
      paths: expect.objectContaining({
        authority: join(
          processes.config.watcher.releaseDirectory,
          "deployment-authority.json",
        ),
      }),
    }),
  );
});

it("requires a measured profile for every launch category in the reopened release", async () => {
  const processes = await stackWithSavedAuthority();
  const deploymentAuthority = { deploymentIdentity: {} };
  author.mockResolvedValue({
    deploymentAuthority,
    fundingProfileOverlay: { profiles: everyCategory },
  } as never);
  await expect(prepareStackRelease(processes)).resolves.toBe(
    deploymentAuthority,
  );
  const [missing] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER;
  author.mockResolvedValue({
    deploymentAuthority,
    fundingProfileOverlay: {
      profiles: { ...everyCategory, [missing!]: undefined },
    },
  } as never);
  await expect(prepareStackRelease(processes)).rejects.toThrow(
    "Release lacks a launch-category funding profile",
  );
});
