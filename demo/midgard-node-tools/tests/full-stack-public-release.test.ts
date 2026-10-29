import { execFile } from "node:child_process";
import { createHash, generateKeyPairSync, randomBytes } from "node:crypto";
import {
  access,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { publicRolesPath } from "../src/full-stack/process.js";
import {
  preparePublicImages,
  publicPostgresPasswordPath,
} from "../src/full-stack/public-runtime.js";
import { runtimeInputDigest, runtimeSteps } from "../src/full-stack/runtime.js";
import {
  classificationOf,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";
import {
  RecordingProcesses,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

// Runs in the `preprod-public-release` project, which compiles the
// preprod-public profile against a blueprint built for it.

const nodeRoot = fileURLToPath(new URL("../../midgard-node", import.meta.url));

let fixture: Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
let directory: string;
beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-stack-public-release-"));
}, 300_000);
afterAll(() => rm(directory, { recursive: true, force: true }));

const SOURCE_REVISION = "a1".repeat(20);
const image = (name: string, byte: string) =>
  `registry.example/midgard/${name}@sha256:${byte.repeat(32)}`;
const ROLE_IMAGES = {
  node: image("node", "01"),
  postgres: image("postgres", "02"),
  ingress: image("ingress", "03"),
};

/**
 * A preprod-public stack whose deployment is final: the blueprint above the
 * node directory, the finalized manifest in it, the node's secrets in the
 * stack environment as phase 1 leaves them, and a release input naming the
 * signing key, the provenance and the deployment's parameter registry.
 */
async function publicStack(name: string, ingressHostPort: number | null) {
  const checkout = join(directory, name);
  const { config } = await stackFixture((sample) => {
    sample.watcher.releaseInput = join(checkout, "release-input.json");
    sample.da.members = sample.da.members.slice(0, 1);
    sample.timeoutMs = 1;
  });
  config.nodeRoot = join(checkout, "demo/midgard-node");
  config.runDirectory = join(checkout, "run");
  const file = (path: string) => join(checkout, "private", path);
  await mkdir(join(checkout, "private"), { recursive: true });
  await mkdir(join(checkout, "onchain/aiken"), { recursive: true });
  await mkdir(join(config.nodeRoot, "deploymentInfo"), { recursive: true });
  await writeFile(
    join(checkout, "onchain/aiken/plutus.json"),
    fixture.blueprintJson,
  );
  await writeFile(
    join(config.nodeRoot, "deploymentInfo/contract-deployment-info.json"),
    JSON.stringify(fixture.manifest),
  );
  const signer = generateKeyPairSync("ed25519");
  const der = signer.publicKey.export({ format: "der", type: "spki" });
  await writeFile(
    file("signer.pem"),
    signer.privateKey.export({ format: "pem", type: "pkcs8" }),
    { mode: 0o600 },
  );
  await writeFile(
    file("trust-roots.json"),
    JSON.stringify([
      {
        trustRootId: createHash("sha256").update(der).digest("hex"),
        publicKeySpkiDerHex: der.toString("hex"),
      },
    ]),
  );
  await writeFile(file("parameters.json"), JSON.stringify(fixture.retained));
  await writeFile(
    file("provenance.json"),
    JSON.stringify({
      schemaVersion: "midgard-deployment-release-provenance-v1",
      sourceRevision: SOURCE_REVISION,
      deploymentProfileName: fixture.manifest.deploymentProfile.name,
      deploymentProfileDigest: fixture.manifest.deploymentProfileDigest,
      blueprintHash: fixture.manifest.artifacts.blueprintHash,
      aikenSourceHash: "11".repeat(32),
      compiler: {
        forkRepository: "https://github.com/example/aiken",
        forkRevision: "b2".repeat(20),
        cargoLockHash: "22".repeat(32),
      },
      roleImages: ROLE_IMAGES,
      evidence: {
        independentBlueprintRebuildDigest: "33".repeat(32),
        frozenBuildDigest: "44".repeat(32),
        parameterRegistryDigest: computeDeploymentManifestJsonDigest(
          fixture.retained,
        ),
      },
    }),
  );
  await writeFile(
    config.watcher.releaseInput!,
    JSON.stringify({
      signingKeyFile: file("signer.pem"),
      provenanceFile: file("provenance.json"),
      parameterRegistryFile: file("parameters.json"),
      programCommitments: {},
      fundingProfiles: [],
    }),
  );
  // The classification the reference-script deployment used.
  await writeFile(
    file("blueprint-classification.json"),
    JSON.stringify(classificationOf(fixture.retained)),
  );
  config.public = {
    nodeConfigFile: file("node-secrets.yaml"),
    tlsCertFile: file("node.pem"),
    tlsKeyFile: file("node.key"),
    tlsCaFile: file("ca.pem"),
    trustRootsFile: file("trust-roots.json"),
    blueprintClassificationFile: file("blueprint-classification.json"),
    releaseDirectory: file("release"),
    ingressHostPort,
  };
  const env: Record<string, string> = {
    ...stackEnvironment(config),
    MIDGARD_DEPLOYMENT_PROFILE: "preprod-public",
    MIDGARD_PUBLIC_API_ORIGIN: "https://node.example.test",
    MIDGARD_PUBLIC_RELEASE_REVISION: SOURCE_REVISION,
    MIDGARD_PUBLIC_NODE_IMAGE: ROLE_IMAGES.node,
    MIDGARD_PUBLIC_POSTGRES_IMAGE: ROLE_IMAGES.postgres,
    MIDGARD_PUBLIC_INGRESS_IMAGE: ROLE_IMAGES.ingress,
    POSTGRES_USER: "midgard",
    POSTGRES_DB: "midgard",
    // What the deployment steps restore before the services run.
    HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
    L1_ORIGIN: `7.${"cd".repeat(32)}`,
  };
  // Phase 1 moves the node's secrets from its private file into the stack environment.
  for (const secret of PUBLIC_NODE_SECRET_NAMES)
    env[secret] ??= randomBytes(24).toString("base64url");
  const processes = new RecordingProcesses(config, env);
  const [release, configuration, , services] = runtimeSteps(processes);
  return {
    processes,
    release: release!,
    configuration: configuration!,
    services: services!,
    releaseDirectory: config.public.releaseDirectory,
    classification: file("blueprint-classification.json"),
  };
}

const exists = (path: string) =>
  access(path).then(
    () => true,
    () => false,
  );

describe("public-release step", () => {
  it("signs the public release once; a resume reopens it and never signs again", async () => {
    const stack = await publicStack("sign-once", null);
    expect(stack.release.id).toBe("public-release");
    expect(stack.configuration.id).toBe("runtime-configuration");
    await expect(stack.release.reconcile(undefined)).resolves.toEqual({
      status: "retry",
    });
    const signed = (await stack.release.execute(undefined)) as {
      manifestId: string;
    };
    expect(signed.manifestId).toBe(fixture.manifest.manifestId);
    expect((await readdir(stack.releaseDirectory)).sort()).toEqual(
      Object.values(SIGNED_RELEASE_FILES).sort(),
    );
    const authority = join(
      stack.releaseDirectory,
      SIGNED_RELEASE_FILES.authority,
    );
    const before = await readFile(authority, "utf8");
    // A key the trust roots do not list: signing again could not verify.
    const input = JSON.parse(
      await readFile(stack.processes.config.watcher.releaseInput!, "utf8"),
    );
    await writeFile(
      input.signingKeyFile,
      generateKeyPairSync("ed25519").privateKey.export({
        format: "pem",
        type: "pkcs8",
      }),
    );
    await expect(stack.release.reconcile(undefined)).resolves.toEqual({
      status: "complete",
      data: signed,
    });
    await expect(stack.release.execute(undefined)).resolves.toEqual(signed);
    expect(await readFile(authority, "utf8")).toBe(before);
  });

  it("refuses a partly written release directory, naming it", async () => {
    const stack = await publicStack("partial", null);
    await stack.release.execute(undefined);
    await rm(join(stack.releaseDirectory, SIGNED_RELEASE_FILES.provenance));
    await expect(stack.release.reconcile(undefined)).rejects.toThrow(
      `public.releaseDirectory ${stack.releaseDirectory} holds a release this deployment cannot use`,
    );
  });

  it("refuses to sign a release whose classification differs from the deployed one", async () => {
    const stack = await publicStack("classification", null);
    const classification = JSON.parse(
      await readFile(stack.classification, "utf8"),
    ) as Record<string, string>;
    const [title] = Object.keys(classification);
    await writeFile(
      stack.classification,
      JSON.stringify({ ...classification, [title!]: "Edited reason" }),
    );
    await expect(stack.release.execute(undefined)).rejects.toThrow(
      "classifies the unused validators differently from public.blueprintClassificationFile",
    );
    expect(await exists(stack.releaseDirectory)).toBe(false);
  });
});

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
