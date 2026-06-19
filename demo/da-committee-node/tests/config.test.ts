import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
  loadWatcherConfig,
} from "../src/config.js";
import { parseMidgardNodeDeploymentInfo } from "../src/l1/deployment.js";
import { bytesToHex } from "../src/utils/hex.js";
import { tempDir } from "./helpers.js";

describe("loadWatcherConfig", () => {
  it("loads deployment files and DA params from the manifest", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = {
      network: "Preview",
      contracts: {
        stateQueue: {
          policyId: "02".repeat(28),
          spendingScriptAddress: "addr_test1statequeue",
        },
        daAttestation: {
          policyId: "03".repeat(28),
          spendingScriptAddress: "addr_test1daattestation",
        },
        daParamsGovernor: {
          policyId: "04".repeat(28),
          spendingScriptAddress: "addr_test1daparams",
        },
      },
      da: {
        threshold: 1,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(member, "hex"), { dkLen: 32 }),
        ),
        members: [{ index: 0, vkey: member }],
      },
    };
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeFile(deploymentInfoPath, JSON.stringify({}));
    const config = await loadWatcherConfig({
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
      CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
      WATCHER_DB_PATH: join(dir, "db"),
    });
    expect(config.network).toBe("Preview");
    expect(config.daParams.committeeHex).toBe(member);
    expect(config.daParams.threshold).toBe(1);
  });

  it("derives contracts from the Midgard node deployment-info format", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    const expectedDeployment = parseMidgardNodeDeploymentInfo(
      JSON.parse(await readFile(deploymentInfoPath, "utf8")) as Record<
        string,
        unknown
      >,
      "Preview",
    );
    if (expectedDeployment === undefined) {
      throw new Error("real Midgard deployment fixture did not parse");
    }
    await writeFile(manifestPath, JSON.stringify({}));
    const config = await loadWatcherConfig({
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      MIDGARD_NETWORK: "Preview",
      CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
      CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
      WATCHER_DB_PATH: join(dir, "db"),
      DA_COMMITTEE_HEX: member,
      DA_THRESHOLD: "1",
    });

    expect(config.daAttestationPolicyId).toBe(
      expectedDeployment.daAttestation.policyId,
    );
    expect(config.daAttestationAddress).toBe(
      expectedDeployment.daAttestation.spendingScriptAddress,
    );
    expect(config.daParamsGovernorPolicyId).toBe(
      expectedDeployment.daParamsGovernor.policyId,
    );
    expect(config.daParamsGovernorAddress).toBe(
      expectedDeployment.daParamsGovernor.spendingScriptAddress,
    );
    expect(config.stateQueuePolicyId).toBe(expectedDeployment.stateQueue.policyId);
    expect(config.stateQueueAddress).toBe(
      expectedDeployment.stateQueue.spendingScriptAddress,
    );
    expect(config.midgardNodeDeployment?.daAttestation.mint.refScriptOutRef).toEqual({
      txHash: "01".repeat(32),
      outputIndex: 0,
    });
    expect(config.midgardNodeDeployment?.stateQueue.spend.scriptHash).toBe(
      expectedDeployment.stateQueue.spend.scriptHash,
    );
  });

  it("requires an L1 submitter key source when L1 submission is enabled", async () => {
    const dir = await tempDir();
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));
    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
        MIDGARD_NETWORK: "Preview",
        CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
        CARDANO_FINALITY_DEPTH: "2",
        DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        DA_L1_SUBMISSION_ENABLED: "true",
        WATCHER_DB_PATH: join(dir, "db"),
      }),
    ).rejects.toThrow(/L1_SUBMITTER_KEY_SOURCE/);
  });

  it("requires a live Cardano provider in self-submitting coordinator mode", async () => {
    const dir = await tempDir();
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));
    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
        MIDGARD_NETWORK: "Preview",
        CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
        CARDANO_FINALITY_DEPTH: "2",
        DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
        WATCHER_DB_PATH: join(dir, "db"),
      }),
    ).rejects.toThrow(/blockfrost: or kupmios:/);
  });

  it("requires script CBOR and reference-script UTxOs in self-submitting coordinator mode", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifest = {
      network: "Preview",
      contracts: {
        stateQueue: {
          policyId: "02".repeat(28),
          spendingScriptAddress: "addr_test1statequeue",
        },
        daAttestation: {
          policyId: "03".repeat(28),
          spendingScriptAddress: "addr_test1daattestation",
        },
        daParamsGovernor: {
          policyId: "04".repeat(28),
          spendingScriptAddress: "addr_test1daparams",
        },
      },
      da: { threshold: 1, members: [{ index: 0, vkey: member }] },
    };
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    await writeFile(manifestPath, JSON.stringify(manifest));
    await writeFile(deploymentInfoPath, JSON.stringify({}));
    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
        CARDANO_PROVIDER_URLS:
          "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
        CARDANO_FINALITY_DEPTH: "2",
        DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
        WATCHER_DB_PATH: join(dir, "db"),
      }),
    ).rejects.toThrow(/script CBOR and reference-script UTxOs/);
  });

  it("accepts real Midgard deployment info for self-submitting coordinator mode", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));
    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
        MIDGARD_NETWORK: "Preview",
        CARDANO_PROVIDER_URLS:
          "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
        CARDANO_FINALITY_DEPTH: "2",
        DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
        DA_SIGNER_INDEX: "0",
        DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
        L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
        DA_L1_SUBMISSION_ENABLED: "true",
        WATCHER_DB_PATH: join(dir, "db"),
        DA_COMMITTEE_HEX: member,
        DA_THRESHOLD: "1",
      }),
    ).resolves.toMatchObject({
      l1SubmissionEnabled: true,
      l1SubmitterKeySource: "private-key:ed25519_sk_test",
      l1SubmitterPreflight: {
        enabled: true,
        minPlainAdaLovelace:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.minPlainAdaLovelace,
        minCollateralLovelace:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace,
        minSpendableUtxoCount:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.minSpendableUtxoCount,
        autoFundBufferLovelace:
          DEFAULT_L1_SUBMITTER_PREFLIGHT.autoFundBufferLovelace,
        retryCount: DEFAULT_L1_SUBMITTER_PREFLIGHT.retryCount,
        retryDelayMs: DEFAULT_L1_SUBMITTER_PREFLIGHT.retryDelayMs,
      },
    });
  });

  it("accepts explicit L1 wallet preflight and auto-fund settings", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));

    const config = await loadWatcherConfig({
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      MIDGARD_NETWORK: "Preview",
      CARDANO_PROVIDER_URLS:
        "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
      CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
      DA_L1_MIN_PLAIN_ADA_LOVELACE: "75000000",
      DA_L1_MIN_COLLATERAL_LOVELACE: "6000000",
      DA_L1_MIN_SPENDABLE_UTXO_COUNT: "3",
      DA_L1_AUTO_FUND_KEY_SOURCE: "file:/tmp/funder.seed",
      DA_L1_AUTO_FUND_BUFFER_LOVELACE: "12000000",
      DA_L1_PREFLIGHT_RETRY_COUNT: "5",
      DA_L1_PREFLIGHT_RETRY_DELAY_MS: "250",
      WATCHER_DB_PATH: join(dir, "db"),
      DA_COMMITTEE_HEX: member,
      DA_THRESHOLD: "1",
    });

    expect(config.l1SubmitterPreflight).toEqual({
      enabled: true,
      minPlainAdaLovelace: 75_000_000n,
      minCollateralLovelace: 6_000_000n,
      minSpendableUtxoCount: 3,
      autoFundKeySource: "file:/tmp/funder.seed",
      autoFundBufferLovelace: 12_000_000n,
      retryCount: 5,
      retryDelayMs: 250,
    });
  });

  it("rejects malformed L1 wallet preflight config before network work", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));
    const baseEnv = {
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      MIDGARD_NETWORK: "Preview",
      CARDANO_PROVIDER_URLS:
        "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
      CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
      WATCHER_DB_PATH: join(dir, "db"),
      DA_COMMITTEE_HEX: member,
      DA_THRESHOLD: "1",
    };

    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_MIN_PLAIN_ADA_LOVELACE: "not-a-number",
      }),
    ).rejects.toThrow(/DA_L1_MIN_PLAIN_ADA_LOVELACE/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_MIN_SPENDABLE_UTXO_COUNT: "0",
      }),
    ).rejects.toThrow(/DA_L1_MIN_SPENDABLE_UTXO_COUNT/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_PREFLIGHT_RETRY_DELAY_MS: "-1",
      }),
    ).rejects.toThrow(/DA_L1_PREFLIGHT_RETRY_DELAY_MS/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_AUTO_FUND_KEY_SOURCE: "file:",
      }),
    ).rejects.toThrow(/DA_L1_AUTO_FUND_KEY_SOURCE/);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_AUTO_FUND_KEY_SOURCE: "private-key:ed25519_sk_test",
      }),
    ).rejects.toThrow(/must not equal/);
  });

  it("accepts submitter-only L1 mode with optional relayer ids", async () => {
    const dir = await tempDir();
    const member = "01".repeat(32);
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = await writeRealDeploymentFixture(dir);
    await writeFile(manifestPath, JSON.stringify({}));
    const baseEnv = {
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
      MIDGARD_NETWORK: "Preview",
      CARDANO_PROVIDER_URLS:
        "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
      CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      L1_SUBMITTER_KEY_SOURCE: "private-key:ed25519_sk_test",
      DA_L1_SUBMISSION_ENABLED: "true",
      WATCHER_DB_PATH: join(dir, "db"),
      DA_COMMITTEE_HEX: member,
      DA_THRESHOLD: "1",
    };
    const config = await loadWatcherConfig({
      ...baseEnv,
      DA_L1_SUBMITTER_ID: "relayer-a",
      DA_L1_SUBMITTER_IDS: "relayer-a,relayer-b",
    });
    expect(config).toMatchObject({
      l1SubmissionEnabled: true,
      l1SubmitterId: "relayer-a",
      l1SubmitterIds: ["relayer-a", "relayer-b"],
    });
    expect("signerIndex" in config).toBe(false);
    expect("signerKeySource" in config).toBe(false);
    await expect(
      loadWatcherConfig({
        ...baseEnv,
        DA_L1_SUBMITTER_ID: "relayer-c",
        DA_L1_SUBMITTER_IDS: "relayer-a,relayer-b",
      }),
    ).rejects.toThrow(/DA_L1_SUBMITTER_ID/);
  });

  it("fails closed when required deployment contract fields are absent", async () => {
    const dir = await tempDir();
    const manifestPath = join(dir, "manifest.json");
    const deploymentInfoPath = join(dir, "deployment.json");
    await writeFile(manifestPath, JSON.stringify({ network: "Preview" }));
    await writeFile(deploymentInfoPath, JSON.stringify({}));
    await expect(
      loadWatcherConfig({
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: manifestPath,
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: deploymentInfoPath,
        CARDANO_PROVIDER_URLS: "fixture:/tmp/state.json",
        CARDANO_FINALITY_DEPTH: "2",
      DA_PAYLOAD_ENDPOINTS: "http://da-0.example",
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
      WATCHER_DB_PATH: join(dir, "db"),
    }),
    ).rejects.toThrow(/DA attestation policy id/);
  });
});

const writeRealDeploymentFixture = async (dir: string): Promise<string> => {
  const sourcePath = join(
    process.cwd(),
    "../midgard-node/deploymentInfo/contract-deployment-info.json",
  );
  const parsed = JSON.parse(await readFile(sourcePath, "utf8")) as Record<
    string,
    unknown
  >;
  const fixture = withReferenceScriptOutRefs(parsed);
  const fixturePath = join(dir, "contract-deployment-info.with-refs.json");
  await writeFile(fixturePath, JSON.stringify(fixture));
  return fixturePath;
};

const withReferenceScriptOutRefs = (
  deploymentInfo: Record<string, unknown>,
): Record<string, unknown> => {
  const clone = structuredClone(deploymentInfo) as Record<string, unknown>;
  const contracts = clone.contracts;
  if (typeof contracts !== "object" || contracts === null) {
    return clone;
  }
  [
    "daAttestationMint",
    "daAttestationSpend",
    "daParamsGovernorMint",
    "daParamsGovernorSpend",
    "stateQueueMint",
    "stateQueueSpend",
  ].forEach((key, index) => {
    const entry = (contracts as Record<string, unknown>)[key];
    if (typeof entry !== "object" || entry === null) {
      return;
    }
    (entry as Record<string, unknown>).refScriptUTxO = {
      txHash: (index + 1).toString(16).padStart(2, "0").repeat(32),
      outputIndex: index,
    };
  });
  return clone;
};
