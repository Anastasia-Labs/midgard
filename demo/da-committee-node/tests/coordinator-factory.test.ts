import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { onChainCoordinatorFromConfig } from "../src/coordinator/factory.js";
import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
import type { L1SubmitterPreflightOptions } from "../src/l1/submitter.js";
import { minimalConfig } from "./helpers.js";

describe("onChainCoordinatorFromConfig", () => {
  it("fails closed without an L1 submitter key source", async () => {
    const config = {
      ...minimalConfig({
        dir: "/tmp",
        manifestPath: "/tmp/manifest.json",
        deploymentInfoPath: "/tmp/deployment.json",
        signerSeed: "00".repeat(32),
        signerPublicKey: "11".repeat(32),
      }),
      mode: "coordinator" as const,
      midgardNodeDeployment: {
        hubOraclePolicyId: "99".repeat(28),
        correctionLockAddress: "addr_test1correctionlock",
        hubOracle: fakeDeployment("99".repeat(28)),
        availabilityChallenge: fakeDeployment("ee".repeat(28)),
        fraudProof: fakeDeployment("dd".repeat(28)),
        daAttestation: fakeDeployment("aa".repeat(28)),
        daParamsGovernor: fakeDeployment("bb".repeat(28)),
        stateQueue: fakeDeployment("cc".repeat(28)),
      },
      cardanoProviderUrls: [
        "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
      ],
    };

    await expect(onChainCoordinatorFromConfig(config)).rejects.toThrow(
      /L1_SUBMITTER_KEY_SOURCE/,
    );
  });

  it("fails closed before reference-script resolution when wallet preflight fails", async () => {
    const config = l1ReadyConfig();
    let referenceScriptsResolved = false;

    await expect(
      onChainCoordinatorFromConfig(config, fakeChainReader, undefined, {
        lucidFromProviderUrl: async () => ({
          lucid: {} as LucidEvolution,
          providerSource: "test",
        }),
        selectL1SubmitterWallet: async () => ({
          kind: "private_key",
          value: "ed25519_sk_test",
        }),
        assertL1SubmitterWalletPreflight: async () => {
          throw new Error("wallet preflight failed");
        },
        preflightL1SubmitterWallet: async () => readyPreflightResult,
        fetchDaAttestationReferenceScripts: async () => {
          referenceScriptsResolved = true;
          return fakeReferenceScripts;
        },
      }),
    ).rejects.toThrow(/wallet preflight failed/);

    expect(referenceScriptsResolved).toBe(false);
  });

  it("constructs the coordinator only after submitter wallet preflight succeeds", async () => {
    const calls: string[] = [];
    const config = {
      ...l1ReadyConfig(),
      l1SubmitterPreflight: {
        ...l1ReadyConfig().l1SubmitterPreflight,
        autoFundKeySource: "private-key:funder",
      },
    };

    await expect(
      onChainCoordinatorFromConfig(config, fakeChainReader, undefined, {
        lucidFromProviderUrl: async () => {
          calls.push("lucid");
          return {
            lucid: {} as LucidEvolution,
            providerSource: "test",
          };
        },
        selectL1SubmitterWallet: async () => {
          calls.push("select");
          return {
            kind: "private_key",
            value: "ed25519_sk_test",
          };
        },
        assertL1SubmitterWalletPreflight: async (
          _lucid,
          options: L1SubmitterPreflightOptions,
        ) => {
          calls.push(`preflight:${options.autoFundKeySource ?? ""}`);
          return readyPreflightResult;
        },
        preflightL1SubmitterWallet: async () => readyPreflightResult,
        fetchDaAttestationReferenceScripts: async () => {
          calls.push("reference-scripts");
          return fakeReferenceScripts;
        },
      }),
    ).resolves.toBeDefined();

    expect(calls).toEqual([
      "lucid",
      "select",
      "preflight:private-key:funder",
      "reference-scripts",
    ]);
  });

  it("does not construct an unproven fallback chain reader", async () => {
    const config = l1ReadyConfig();
    await expect(onChainCoordinatorFromConfig(config)).rejects.toThrow(
      /canonical configured DA chain reader/u,
    );
  });
});

const l1ReadyConfig = () => ({
  ...minimalConfig({
    dir: "/tmp",
    manifestPath: "/tmp/manifest.json",
    deploymentInfoPath: "/tmp/deployment.json",
    signerSeed: "00".repeat(32),
    signerPublicKey: "11".repeat(32),
  }),
  l1SubmissionEnabled: true,
  l1SubmitterKeySource: "private-key:ed25519_sk_test",
  daAttestationPolicyId: "aa".repeat(28),
  l1SubmitterPreflight: {
    enabled: true,
    minPlainAdaLovelace: 50_000_000n,
    minCollateralLovelace: 5_000_000n,
    minSpendableUtxoCount: 2,
    autoFundBufferLovelace: 10_000_000n,
    retryCount: 3,
    retryDelayMs: 5_000,
  },
  midgardNodeDeployment: {
    hubOraclePolicyId: "99".repeat(28),
    correctionLockAddress: "addr_test1correctionlock",
    hubOracle: fakeDeployment("99".repeat(28)),
    availabilityChallenge: fakeDeployment("ee".repeat(28)),
    fraudProof: fakeDeployment("dd".repeat(28)),
    daAttestation: fakeDeployment("aa".repeat(28)),
    daParamsGovernor: fakeDeployment("bb".repeat(28)),
    stateQueue: fakeDeployment("cc".repeat(28)),
  },
  cardanoProviderUrls: [
    "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
  ],
});

const readyPreflightResult = {
  status: "ready" as const,
  address: "addr_test1submitter",
  totalLiveLovelace: 60_000_000n,
  plainAdaLovelace: 60_000_000n,
  plainAdaUtxoCount: 2,
  collateralCandidateLovelace: 50_000_000n,
  spendableOutRefs: [],
  ignoredOutRefs: [],
  requiredPlainLovelace: 50_000_000n,
  requiredCollateralLovelace: 5_000_000n,
  requiredSpendableUtxoCount: 2,
  missingPlainLovelace: 0n,
  missingCollateralLovelace: 0n,
  missingSpendableUtxoCount: 0,
  errors: [],
};

const fakeChainReader = {} as DaAttestationChainReader;

const fakeReferenceUtxo = {
  txHash: "99".repeat(32),
  outputIndex: 0,
  address: "addr_test1ref",
  assets: { lovelace: 5_000_000n },
  scriptRef: { type: "PlutusV3", script: "00" },
} as UTxO;

const fakeReferenceScripts = {
  availabilityChallengeMinting: fakeReferenceUtxo,
  daAttestationMinting: fakeReferenceUtxo,
  daAttestationSpending: fakeReferenceUtxo,
  stateQueueMinting: fakeReferenceUtxo,
  stateQueueSpending: fakeReferenceUtxo,
};

const fakeDeployment = (policyId: string) => ({
  mint: {
    key: "mint",
    purpose: "mint" as const,
    script: { type: "PlutusV3" as const, script: "00" },
    scriptHash: policyId,
    refScriptOutRef: { txHash: "00".repeat(32), outputIndex: 0 },
  },
  spend: {
    key: "spend",
    purpose: "spend" as const,
    script: { type: "PlutusV3" as const, script: "00" },
    scriptHash: policyId,
    refScriptOutRef: { txHash: "00".repeat(32), outputIndex: 1 },
  },
  policyId,
  spendingScriptHash: policyId,
  spendingScriptAddress: "addr_test1fixture",
});
