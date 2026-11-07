import { execFile } from "node:child_process";
import { generateKeyPairSync } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { formatL1Origin } from "@al-ft/midgard-core/l1-origin";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import { afterAll, beforeAll, expect, it } from "vitest";

import {
  prepareStackRelease,
  proverFundingKeyHash,
} from "../src/full-stack/release.js";
import { generateWatcherServices } from "../src/full-stack/watcher-services.js";
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

const RETAINED =
  "/ip4/127.0.0.1/tcp/39003/p2p/12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN";

/**
 * A finalized preprod-testing stack whose watcher services generate as the
 * runtime-configuration step generates them: its origin restored, its
 * watcher keys and Cardano genesis on disk, and Compose rendering the
 * watcher's own compose.yaml.
 */
async function watcherStack(name: string) {
  const stack = await finalizedStack(name);
  const { config } = stack.processes;
  // The origin step restores the run's L1 origin before this one runs.
  stack.processes.env.L1_ORIGIN = formatL1Origin({
    slot: 7,
    blockHash: "cd".repeat(32),
  });
  const keys = {
    WATCHER_ROLLBACK_KEY_FILE: join(stack.checkout, "rollback.key"),
    WATCHER_PROVER_KEY_FILE: join(stack.checkout, "prover.key"),
    WATCHER_AVAILABILITY_KEY_FILE: join(stack.checkout, "availability.key"),
  };
  await writeFile(
    config.watcher.composeEnvFile,
    Object.entries(keys)
      .map(([key, value]) => `${key}=${value}\n`)
      .join(""),
  );
  await mkdir(join(config.watcher.configDirectory, "cardano"), {
    recursive: true,
  });
  await writeFile(
    join(config.watcher.configDirectory, "cardano/shelley-genesis.json"),
    "{}",
  );
  const bind = (source: string, target: string) => ({
    type: "bind",
    source,
    target,
  });
  stack.processes.responses["watcher-compose-configuration"] = () => ({
    services: {
      watcher: {
        image: "midgard-watcher:local",
        volumes: [
          bind("/c/process.json", "/etc/midgard/watcher-process.json"),
          bind("/c/runtime.json", "/etc/midgard/watcher-runtime.json"),
          bind("/c/bundles", "/etc/midgard/bundles"),
          bind("/c/ipc", "/ipc"),
          bind("/c/cardano", "/cardano-config"),
          bind(keys.WATCHER_ROLLBACK_KEY_FILE, "/run/secrets/rollback_key"),
          bind(keys.WATCHER_PROVER_KEY_FILE, "/run/secrets/prover_key"),
          bind(
            keys.WATCHER_AVAILABILITY_KEY_FILE,
            "/run/secrets/availability_key",
          ),
        ],
      },
    },
    volumes: { "watcher-state": {} },
  });
  return { ...stack, config };
}

it("generates a preprod-testing stack's watcher configuration that dials its retained DA on loopback", async () => {
  const { config, processes } = await watcherStack("loopback-watcher");
  await generateWatcherServices(processes, RETAINED);
  const runtime = JSON.parse(
    await readFile(
      join(config.watcher.configDirectory, "watcher-runtime.json"),
      "utf8",
    ),
  ) as {
    da: { peers: { multiaddr: string }[]; stackLoopbackPeers?: boolean };
  };
  expect(runtime.da.peers.map((peer) => peer.multiaddr)).toEqual([RETAINED]);
  expect(runtime.da.stackLoopbackPeers).toBe(true);
});

it("recreates the watcher when its generated process or runtime configuration changes", async () => {
  const { checkout, config, processes } =
    await watcherStack("watcher-recreate");
  /** Compose's configuration hash of the generated watcher service: `up -d` recreates its container exactly when this changes. */
  const hash = async () => {
    const watcher = await generateWatcherServices(processes, RETAINED);
    const file = join(checkout, "watcher-compose.json");
    await writeFile(
      file,
      JSON.stringify({ services: watcher.services, volumes: watcher.volumes }),
    );
    const { stdout } = await promisify(execFile)(
      "docker",
      [
        "compose",
        "--project-directory",
        checkout,
        "-f",
        file,
        "config",
        "--hash",
        "watcher",
      ],
      {
        env: {
          PATH: process.env.PATH!,
          HOME: process.env.HOME!,
          COMPOSE_PROJECT_NAME: "midgard-watcher-recreate",
        },
      },
    );
    return stdout.trim();
  };
  const before = await hash();
  // The same mounts and image, a different generated configuration.
  const template = JSON.parse(
    await readFile(config.watcher.processTemplate, "utf8"),
  ) as { watcherConfig: { deadlines: { proofSubmitMs: number } } };
  template.watcherConfig.deadlines.proofSubmitMs += 1;
  await writeFile(config.watcher.processTemplate, JSON.stringify(template));
  expect(await hash()).not.toBe(before);
});
