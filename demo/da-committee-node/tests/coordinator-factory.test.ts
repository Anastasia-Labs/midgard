import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { onChainCoordinatorFromConfig } from "../src/coordinator/factory.js";
import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
import type { L1SubmitterPreflightOptions } from "../src/l1/submitter.js";
import {
  minimalAvailabilityChallengeYields,
  minimalConfig,
  minimalStateQueueYields,
} from "./helpers.js";

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
        referenceScriptAuthPolicyId: "f0".repeat(28),
        hubOraclePolicyId: "99".repeat(28),
        correctionLockAddress: "addr_test1correctionlock",
        hubOracle: fakeDeployment("99".repeat(28)),
        availabilityChallenge: fakeDeployment("ee".repeat(28)),
        fraudProof: fakeDeployment("dd".repeat(28)),
        daAttestation: fakeDeployment("aa".repeat(28)),
        daParamsGovernor: fakeDeployment("bb".repeat(28)),
        stateQueue: fakeDeployment("cc".repeat(28)),
        availabilityChallengeYields: minimalAvailabilityChallengeYields(),
        stateQueueYields: minimalStateQueueYields(),
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

  it("threads one wallet-bearing lucid through preflight and into reference-script resolution", async () => {
    const calls: string[] = [];
    const config = {
      ...l1ReadyConfig(),
      l1SubmitterPreflight: {
        ...l1ReadyConfig().l1SubmitterPreflight,
        autoFundKeySource: "private-key:funder",
      },
    };
    const lucid = {} as LucidEvolution;
    const seen: {
      providerUrl?: string;
      network?: string;
      selectLucid?: unknown;
      keySource?: unknown;
      preflightLucid?: unknown;
      preflightOptions?: L1SubmitterPreflightOptions;
      referenceLucid?: unknown;
      referenceDeployment?: unknown;
    } = {};

    const coordinator = await onChainCoordinatorFromConfig(
      config,
      fakeChainReader,
      undefined,
      {
        lucidFromProviderUrl: async (providerUrl, network) => {
          calls.push("lucid");
          seen.providerUrl = providerUrl;
          seen.network = network;
          return { lucid, providerSource: "test" };
        },
        selectL1SubmitterWallet: async (selectLucid, keySource) => {
          calls.push("select");
          seen.selectLucid = selectLucid;
          seen.keySource = keySource;
          return { kind: "private_key", value: "ed25519_sk_test" };
        },
        assertL1SubmitterWalletPreflight: async (
          preflightLucid,
          options: L1SubmitterPreflightOptions,
        ) => {
          calls.push("preflight");
          seen.preflightLucid = preflightLucid;
          seen.preflightOptions = options;
          return readyPreflightResult;
        },
        preflightL1SubmitterWallet: async () => readyPreflightResult,
        fetchDaAttestationReferenceScripts: async (
          referenceLucid,
          deployment,
        ) => {
          calls.push("reference-scripts");
          seen.referenceLucid = referenceLucid;
          seen.referenceDeployment = deployment;
          return fakeReferenceScripts;
        },
      },
    );

    expect(coordinator).toBeInstanceOf(OnChainLifecycleCoordinator);
    // The ordering contract, stated as the two relations it exists to
    // protect rather than as a transcript of every internal step: no wallet
    // work before a provider, and no on-chain reference-script reads before
    // the wallet has passed preflight.
    expect(calls.indexOf("lucid")).toBeLessThan(calls.indexOf("select"));
    expect(calls.indexOf("select")).toBeLessThan(calls.indexOf("preflight"));
    expect(calls.indexOf("preflight")).toBeLessThan(
      calls.indexOf("reference-scripts"),
    );
    expect(calls.filter((call) => call === "preflight")).toHaveLength(1);
    // Every collaborator must receive the SAME lucid instance: a second
    // client would carry no selected wallet, so preflight and the submitter
    // would be proving properties of the wrong account.
    expect([
      seen.selectLucid,
      seen.preflightLucid,
      seen.referenceLucid,
    ]).toEqual([lucid, lucid, lucid]);
    expect({
      providerUrl: seen.providerUrl,
      network: seen.network,
      keySource: seen.keySource,
      referenceDeployment: seen.referenceDeployment,
    }).toEqual({
      providerUrl: config.cardanoProviderUrls[0],
      network: config.network,
      keySource: config.l1SubmitterKeySource,
      referenceDeployment: config.midgardNodeDeployment,
    });
    expect(seen.preflightOptions).toEqual({
      submitterKeySource: config.l1SubmitterKeySource,
      minPlainAdaLovelace: config.l1SubmitterPreflight.minPlainAdaLovelace,
      minCollateralLovelace: config.l1SubmitterPreflight.minCollateralLovelace,
      minSpendableUtxoCount: config.l1SubmitterPreflight.minSpendableUtxoCount,
      autoFundKeySource: "private-key:funder",
      autoFundBufferLovelace:
        config.l1SubmitterPreflight.autoFundBufferLovelace,
      retryCount: config.l1SubmitterPreflight.retryCount,
      retryDelayMs: config.l1SubmitterPreflight.retryDelayMs,
    });
  });

  it("omits the auto-funding key source when the operator configured none", async () => {
    const config = l1ReadyConfig();
    let options: L1SubmitterPreflightOptions | undefined;
    await onChainCoordinatorFromConfig(config, fakeChainReader, undefined, {
      ...passingDeps(),
      assertL1SubmitterWalletPreflight: async (_lucid, received) => {
        options = received;
        return readyPreflightResult;
      },
    });
    expect(options).toBeDefined();
    expect(Object.hasOwn(options!, "autoFundKeySource")).toBe(false);
  });

  it("skips wallet preflight only when the operator disabled it", async () => {
    const config = {
      ...l1ReadyConfig(),
      l1SubmitterPreflight: {
        ...l1ReadyConfig().l1SubmitterPreflight,
        enabled: false,
      },
    };
    let preflighted = false;
    let referenceScriptsResolved = false;
    const coordinator = await onChainCoordinatorFromConfig(
      config,
      fakeChainReader,
      undefined,
      {
        ...passingDeps(),
        assertL1SubmitterWalletPreflight: async () => {
          preflighted = true;
          return readyPreflightResult;
        },
        fetchDaAttestationReferenceScripts: async () => {
          referenceScriptsResolved = true;
          return fakeReferenceScripts;
        },
      },
    );
    expect(coordinator).toBeInstanceOf(OnChainLifecycleCoordinator);
    expect(preflighted).toBe(false);
    expect(referenceScriptsResolved).toBe(true);
  });

  it("refuses a deployment whose DA attestation policy id is not the configured one", async () => {
    // The real cross-check runs here: the policy id is derived from the
    // deployment by production code, not supplied by a stub.
    const base = l1ReadyConfig();
    const config = {
      ...base,
      daAttestationPolicyId: "ab".repeat(28),
    };
    await expect(
      onChainCoordinatorFromConfig(
        config,
        fakeChainReader,
        undefined,
        passingDeps(),
      ),
    ).rejects.toThrow(
      "configured DA attestation policy id does not match Midgard node deployment-info",
    );
    // ...and the identical configuration with the deployment's own policy id
    // constructs, so the refusal is caused by the mismatch and nothing else.
    await expect(
      onChainCoordinatorFromConfig(
        base,
        fakeChainReader,
        undefined,
        passingDeps(),
      ),
    ).resolves.toBeInstanceOf(OnChainLifecycleCoordinator);
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
    referenceScriptAuthPolicyId: "f0".repeat(28),
    hubOraclePolicyId: "99".repeat(28),
    correctionLockAddress: "addr_test1correctionlock",
    hubOracle: fakeDeployment("99".repeat(28)),
    availabilityChallenge: fakeDeployment("ee".repeat(28)),
    fraudProof: fakeDeployment("dd".repeat(28)),
    daAttestation: fakeDeployment("aa".repeat(28)),
    daParamsGovernor: fakeDeployment("bb".repeat(28)),
    stateQueue: fakeDeployment("cc".repeat(28)),
    availabilityChallengeYields: minimalAvailabilityChallengeYields(),
    stateQueueYields: minimalStateQueueYields(),
  },
  cardanoProviderUrls: [
    "blockfrost:https://cardano-preview.blockfrost.io/api/v0#project",
  ],
});

const passingDeps = () => ({
  lucidFromProviderUrl: async () => ({
    lucid: {} as LucidEvolution,
    providerSource: "test",
  }),
  selectL1SubmitterWallet: async () => ({
    kind: "private_key" as const,
    value: "ed25519_sk_test",
  }),
  assertL1SubmitterWalletPreflight: async () => readyPreflightResult,
  preflightL1SubmitterWallet: async () => readyPreflightResult,
  fetchDaAttestationReferenceScripts: async () => fakeReferenceScripts,
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
  availabilityChallengeBondWithdrawal: fakeReferenceUtxo,
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
