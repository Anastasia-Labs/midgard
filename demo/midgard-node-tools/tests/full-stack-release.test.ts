import { generateKeyPairSync } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import { afterAll, beforeAll, expect, it } from "vitest";

import {
  prepareStackRelease,
  proverFundingKeyHash,
} from "../src/full-stack/release.js";
import {
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";
import {
  RecordingProcesses,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

const exampleTemplate = fileURLToPath(
  new URL(
    "../../midgard-watcher/watcher-process.example.json",
    import.meta.url,
  ),
);

let fixture: Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
let directory: string;
beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-stack-release-"));
}, 300_000);
afterAll(() => rm(directory, { recursive: true, force: true }));

/**
 * A stack checkout whose deployment is final: the blueprint above the node
 * directory, the finalized manifest in it, and a release input that leaves
 * every program commitment to the derivation.
 */
async function finalizedStack(name: string) {
  const checkout = join(directory, name);
  // The example watcher process, following at the compiled profile's depth.
  const template = JSON.parse(await readFile(exampleTemplate, "utf8"));
  template.watcherConfig.l1.finality.depth =
    SELECTED_DEPLOYMENT_PROFILE.l1_finality.confirmation_depth;
  const processTemplate = join(directory, `${name}-watcher-process.json`);
  await writeFile(processTemplate, JSON.stringify(template));
  const { config } = await stackFixture((sample) => {
    sample.watcher.processTemplate = processTemplate;
    sample.watcher.releaseDirectory = join(checkout, "release");
    sample.watcher.releaseInput = join(checkout, "release-input.json");
  });
  config.nodeRoot = join(checkout, "demo/midgard-node");
  const blueprint = join(checkout, "onchain/aiken/plutus.json");
  const manifest = join(
    config.nodeRoot,
    "deploymentInfo/contract-deployment-info.json",
  );
  await mkdir(join(checkout, "onchain/aiken"), { recursive: true });
  await mkdir(join(config.nodeRoot, "deploymentInfo"), { recursive: true });
  await writeFile(blueprint, fixture.blueprintJson);
  await writeFile(manifest, JSON.stringify(fixture.manifest));
  let signers = 0;
  const writeInput = async () => {
    signers += 1;
    const signingKeyFile = join(checkout, `signer-${signers}.pem`);
    await writeFile(
      signingKeyFile,
      generateKeyPairSync("ed25519").privateKey.export({
        format: "pem",
        type: "pkcs8",
      }),
      { mode: 0o600 },
    );
    await writeFile(
      config.watcher.releaseInput!,
      JSON.stringify({
        signingKeyFile,
        programCommitments: {},
        fundingProfiles: [],
      }),
    );
  };
  await writeInput();
  const processes = new RecordingProcesses(config, stackEnvironment(config));
  return { checkout, blueprint, manifest, processes, writeInput };
}

it("signs the same watcher release as the release signing command from the same input", async () => {
  const stack = await finalizedStack("same-release");
  await prepareStackRelease(stack.processes);
  const standalone = join(stack.checkout, "standalone");
  await signDeploymentRelease({
    releaseInput: stack.processes.config.watcher.releaseInput!,
    deploymentInfo: stack.manifest,
    blueprint: stack.blueprint,
    output: standalone,
    fundingPaymentKeyHash: proverFundingKeyHash(stack.processes),
  });
  for (const name of ["authority", "rules", "funding"] as const)
    expect(
      await readFile(
        join(
          stack.processes.config.watcher.releaseDirectory,
          SIGNED_RELEASE_FILES[name],
        ),
        "utf8",
      ),
    ).toBe(
      await readFile(join(standalone, SIGNED_RELEASE_FILES[name]), "utf8"),
    );
});

it("reopens its saved release for the same signer and refuses another", async () => {
  const stack = await finalizedStack("reopen");
  const authority = join(
    stack.processes.config.watcher.releaseDirectory,
    SIGNED_RELEASE_FILES.authority,
  );
  await prepareStackRelease(stack.processes);
  const signed = await readFile(authority, "utf8");
  await expect(prepareStackRelease(stack.processes)).resolves.toBeDefined();
  await stack.writeInput();
  await expect(prepareStackRelease(stack.processes)).rejects.toThrow(
    "not the configured signing key",
  );
  expect(await readFile(authority, "utf8")).toBe(signed);
});
