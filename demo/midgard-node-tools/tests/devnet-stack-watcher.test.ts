import { createHash, randomBytes } from "node:crypto";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { h32ForOrdinal } from "@al-ft/midgard-test-support/hex";
import { type Network, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
} from "midgard-node/commands/contract-deployment-info";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "midgard-node/deployment-manifest";
import { TEST_AVAILABILITY_CHALLENGE } from "midgard-node/tests/helpers/availability-challenge";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "midgard-node/tests/helpers/cardano-protocol-parameters";
import { loadRealMidgardContractsForTest } from "midgard-node/tests/helpers/real-midgard-contracts";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "midgard-node/transactions/initialization";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import type { DeployContext } from "../src/devnet-stack/deploy.js";
import { loadIdentities } from "../src/devnet-stack/identities.js";
import {
  type Layout,
  makeLayout,
  type RunEnv,
  servicePorts,
} from "../src/devnet-stack/layout.js";
import type { HubOracleOneShot } from "../src/devnet-stack/node-env.js";
import {
  ensureWatcherRelease,
  watcherServiceSpecs,
} from "../src/devnet-stack/watcher.js";
import {
  loadWatcherModule,
  releasePaths,
} from "../src/devnet-stack/watcher-release.js";

// An isolated /tmp checkout supplies its owned canonical test-storage root.
const toolsRoot = join(dirname(new URL(import.meta.url).pathname), "..");
const scratchRoot =
  process.env.MIDGARD_TEST_STORAGE_ROOT ??
  join(toolsRoot, "node_modules/.cache/devnet-stack-watcher-test");
const runDir = join(scratchRoot, `run-${randomBytes(4).toString("hex")}`);
const PEER_ID = "12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN";

/**
 * A finalized deployment manifest over the real contracts, as
 * `midgard-node/tests/helpers/finalized-deployment-manifest` builds it, but on
 * the compiled profile's network (the devnet's `Custom`) and bound to the
 * blueprint the release will carry.
 */
const makeDevnetManifest = async (
  blueprintHash: string,
  genesisHeaderHash = "00".repeat(28),
): Promise<DeploymentManifest> => {
  const oneShot = { txHash: "ab".repeat(32), outputIndex: 0 };
  const contracts = await loadRealMidgardContractsForTest(oneShot);
  const nativeScriptCbor = "820500";
  const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId: validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    }),
    nativeScript: {
      type: "Native",
      cborHex: nativeScriptCbor,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "test fixture" },
  };
  const referenceScriptOutRefs = new Map(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, index) => [
        contractName,
        { txHash: h32ForOrdinal(index + 1), outputIndex: 0 },
      ],
    ),
  );
  const catalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    ),
  );
  const daVkey = "44".repeat(32);
  return parseDeploymentManifestValue(
    buildDeploymentManifest(
      buildContractDeploymentInfoFromContracts(
        contracts,
        referenceScriptAuthPolicy,
        referenceScriptOutRefs,
        catalogue,
      ),
      {
        network: SELECTED_DEPLOYMENT_PROFILE.network,
        availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
        economics: SELECTED_DEPLOYMENT_PROFILE.economics,
        cardanoProtocolParameters: {
          snapshot: TEST_CARDANO_PROTOCOL_PARAMETERS,
          digest: computeDeploymentManifestJsonDigest(
            TEST_CARDANO_PROTOCOL_PARAMETERS,
          ),
        },
        genesis: {
          headerHash: genesisHeaderHash,
          utxoSetDigest: computeDeploymentManifestJsonDigest(
            normalizeDeploymentManifestJsonValue([]),
          ),
        },
        da: {
          committeeVkeys: [daVkey],
          committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash(
            [daVkey],
          ),
          threshold: 1,
          transportProfile: {
            protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
            runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
            envelopeEncoding: "identity",
            zstdLevel: 3,
            limits: DA_TRANSPORT_LIMITS,
            retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
          },
        },
        artifacts: { blueprintHash },
        referenceScriptDeployAddress: "addr_test1reference",
        hubOracleOneShotTxHash: oneShot.txHash,
        hubOracleOneShotOutputIndex: oneShot.outputIndex,
        hubOracleOneShotStatus: "consumed_by_init",
        steps: {
          initProtocol: { status: "complete" },
          availabilityRegistration: { status: "complete" },
        },
      },
    ),
  );
};

let layout: Layout;
let context: DeployContext;
let oneShot: HubOracleOneShot;
let manifest: DeploymentManifest;
let otherManifest: DeploymentManifest;

const writeJson = (path: string, value: unknown) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(value));
};

const walk = (directory: string): string[] =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) =>
    entry.isDirectory()
      ? walk(join(directory, entry.name))
      : [join(directory, entry.name)],
  );

const snapshot = (directory: string) =>
  new Map(
    walk(directory).map((path) => {
      const stat = statSync(path);
      return [
        path,
        `${stat.mtimeMs}:${stat.mode}:${createHash("sha256").update(readFileSync(path)).digest("hex")}`,
      ];
    }),
  );

const isCustomNetwork = (network: Network): boolean => network === "Custom";

beforeAll(async () => {
  if (!isCustomNetwork(SELECTED_DEPLOYMENT_PROFILE.network)) return;
  layout = makeLayout(runDir);
  // Every shifted service port (the highest base is 39006) must stay a valid port.
  const portOffset = 20_000 + Math.floor(Math.random() * 6_000);
  const run: RunEnv = {
    runId: "wt1",
    composeProject: "midgard-wt1",
    networkMagic: 424242,
    ogmiosPort: 2337 + portOffset,
    kupoPort: 1442 + portOffset,
    postgresPort: 5432 + portOffset,
    postgresUser: "midgard",
    postgresPassword: "unused",
    postgresDatabase: "midgard",
    cardanoImage: "unused",
    postgresImage: "unused",
    portOffset,
  };
  mkdirSync(layout.state, { recursive: true });
  context = {
    layout,
    run,
    identities: loadIdentities(layout),
    artifacts: {
      nativeOwnerBinary: join(layout.bin, "native-owner"),
      nativeOwnerSha256: "00".repeat(32),
      transportBinary: join(layout.bin, "midgard-l1-node-transport"),
    },
  };

  mkdirSync(dirname(layout.blueprint), { recursive: true });
  copyFileSync(
    join(layout.repoRoot, "onchain/aiken/plutus.json"),
    layout.blueprint,
  );
  const blueprintHash = createHash("sha256")
    .update(readFileSync(layout.blueprint))
    .digest("hex");
  manifest = await makeDevnetManifest(blueprintHash);
  otherManifest = await makeDevnetManifest(blueprintHash, "01".repeat(28));
  const [txHash, outputIndex] = manifest.hubOracleOneShot.outRef.split("#");
  oneShot = { txHash: txHash!, outputIndex: Number(outputIndex) };

  writeJson(layout.contractManifest, manifest);
  writeJson(layout.shelleyGenesis, {
    networkMagic: run.networkMagic,
    systemStart: "2026-09-30T00:00:00Z",
    slotLength: 0.1,
  });
  writeJson(layout.hostCardanoConfig, {});
  writeJson(layout.committeeManifest(0), {
    public_retained_da: {
      announce_multiaddrs: [
        `/ip4/127.0.0.1/tcp/${servicePorts(run).retainedLibp2p}/p2p/${PEER_ID}`,
      ],
    },
  });
}, 300_000);

afterAll(() => {
  rmSync(runDir, { recursive: true, force: true });
});

// A devnet release is a `Custom`-network release, and a manifest can only be
// built on the compiled deployment profile's network: this suite needs the
// profile the controller builds (`deployment:build local-devnet-testing`).
describe.skipIf(!isCustomNetwork(SELECTED_DEPLOYMENT_PROFILE.network))(
  "devnet watcher release (needs the local-devnet-testing profile compiled)",
  () => {
    it("generates a release and configurations the watcher's own loaders accept", async () => {
      await ensureWatcherRelease(context, oneShot);
      const watcher = await loadWatcherModule(layout);
      const paths = releasePaths(layout);

      const authority = await watcher.loadWatcherVerifiedDeploymentAuthority({
        path: paths.authority,
        ruleBundlePath: paths.rules,
      });
      expect(authority.deploymentIdentity.manifestId).toBe(manifest.manifestId);
      expect(statSync(layout.watcherTrustRootKey).mode & 0o777).toBe(0o600);

      const processConfig = watcher.parseWatcherProcessConfig(
        JSON.parse(readFileSync(layout.watcherProcessConfig, "utf8")),
      );
      const runtime = JSON.parse(
        readFileSync(layout.watcherRuntimeConfig, "utf8"),
      );
      expect(runtime).toEqual(
        JSON.parse(readFileSync(layout.watcherProcessConfig, "utf8"))
          .watcherConfig,
      );
      expect(watcher.parseWatcherConfig(runtime)).toEqual(
        processConfig.watcherConfig,
      );

      const l1 = runtime.l1 as { finality: unknown; source: object };
      expect(l1.finality).toEqual({
        depth: manifest.l1Finality.confirmationDepth,
      });
      expect(l1.source).not.toHaveProperty("queryServices");
      expect(processConfig).not.toHaveProperty("trustedHeadAuthorityEndpoint");
      expect(processConfig).not.toHaveProperty("httpBearerSecretSource");
      expect(runtime.customNetwork).toEqual({
        networkMagic: 424242,
        slotConfig: {
          zeroTime: Date.parse("2026-09-30T00:00:00Z"),
          zeroSlot: 0,
          slotLength: 100,
        },
      });

      const secrets = readdirSync(dirname(layout.watcherSecret("x"))).map(
        (name) => layout.watcherSecret(name),
      );
      expect(secrets).toHaveLength(3);
      const values = secrets.map((path) => {
        expect(statSync(path).mode & 0o777).toBe(0o600);
        const text = readFileSync(path, "utf8");
        expect(text.endsWith("\n")).toBe(false);
        return text;
      });
      expect(new Set(values).size).toBe(3);
      expect(
        readFileSync(layout.watcherSecret("rollback-authority.key"), "utf8"),
      ).toMatch(/^[0-9a-f]{64}$/u);
      expect(readFileSync(layout.watcherSecret("prover.seed"), "utf8")).toBe(
        context.identities.seeds.watcherProver,
      );
      expect(
        readFileSync(layout.watcherSecret("availability.seed"), "utf8"),
      ).toBe(context.identities.seeds.watcherAvailability);
      for (const path of secrets)
        expect(
          (await watcher.loadWatcherSecretText({ kind: "file", path })).length,
        ).toBeGreaterThan(0);
    }, 300_000);

    it("changes nothing on a second call", async () => {
      const before = snapshot(layout.watcher);
      await ensureWatcherRelease(context, oneShot);
      expect(snapshot(layout.watcher)).toEqual(before);
    }, 300_000);

    it("refuses a release signed for another deployment", async () => {
      const original = readFileSync(layout.contractManifest);
      const before = snapshot(layout.watcher);
      writeJson(layout.contractManifest, otherManifest);
      try {
        await expect(ensureWatcherRelease(context, oneShot)).rejects.toThrow(
          `was signed for deployment ${manifest.manifestId}, not ${otherManifest.manifestId}`,
        );
      } finally {
        writeFileSync(layout.contractManifest, original);
      }
      expect(snapshot(layout.watcher)).toEqual(before);
    }, 300_000);

    it("never regenerates an established watcher's missing secret", async () => {
      const path = layout.watcherSecret("rollback-authority.key");
      const original = readFileSync(path);
      rmSync(path);
      try {
        await expect(ensureWatcherRelease(context, oneShot)).rejects.toThrow(
          "established watcher secret is missing; it is never regenerated",
        );
        expect(existsSync(path)).toBe(false);
      } finally {
        writeFileSync(path, original, { mode: 0o600 });
      }
    }, 300_000);

    it("wires the watcher's service", () => {
      const specs = watcherServiceSpecs(context);
      expect(specs.map((spec) => spec.name)).toEqual(["watcher"]);
      const [watcher] = specs;
      expect(watcher!.args.slice(-3)).toEqual([
        "start",
        "--config",
        layout.watcherProcessConfig,
      ]);
      expect(watcher!.healthUrl).toBe(
        `http://127.0.0.1:${servicePorts(context.run).watcherOperations}/v1/status`,
      );
      expect(watcher!.readyUrl).toBe(
        `http://127.0.0.1:${servicePorts(context.run).watcherOperations}/readyz`,
      );
      expect(watcher!.readyProbe).toBeUndefined();
      expect(watcher!.env).not.toHaveProperty("HTTPS_PROXY");
      expect(watcher!.env).not.toHaveProperty("NODE_EXTRA_CA_CERTS");
      expect(watcher!.prestart).toBeUndefined();
    });
  },
);
