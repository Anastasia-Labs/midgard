import { createHash } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  createWatcherWorkflowFundingProfileBundle,
  loadWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileBody,
  workflowFundingProfileFromOverlay,
} from "../../src/funding/workflow-funding-profile-overlay.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

// Ordinary signed wallet transfer, used only to test bundle serialization and
// authentication. This is not evidence for a production workflow measurement.
const profileBodies = (): readonly WatcherWorkflowFundingProfileBody[] => {
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  ).to_address();
  const input = CML.TransactionInput.new(
    CML.TransactionHash.from_hex("66".repeat(32)),
    0n,
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(address, CML.Value.from_coin(3_000_000n)),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  return FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => ({
    scope: { kind: "fraud_proof_category", category },
    blueprintSha256: "22".repeat(32),
    protocolParametersDigest: "33".repeat(32),
    economicsPolicyDigest: "44".repeat(32),
    fundingPaymentKeyHash: key.to_public().hash().to_hex(),
    measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
    measurementArtifactSha256: "55".repeat(32),
    actions: [
      {
        actionKind: "wallet-transfer",
        signedTransactionCborHex: transaction.to_canonical_cbor_hex(),
        fundingControlledInputs: [
          {
            outRef: `${"66".repeat(32)}#0`,
            resolvedOutputCborHex: CML.TransactionOutput.new(
              address,
              CML.Value.from_coin(3_170_000n),
            ).to_canonical_cbor_hex(),
            role: "wallet_funding",
            semanticRole: "wallet_funding",
            contractAddress: address.to_bech32(),
            identityAssets: [],
            fundingLovelace: "3170000",
            fundingAssets: [],
            sourceActionKind: null,
            sourceOutputIndex: null,
          },
        ],
        fundingControlledOutputs: [
          {
            outputIndex: 0,
            role: "wallet_change",
            custodyRole: "none",
            semanticRole: "wallet_change",
            contractAddress: address.to_bech32(),
            fundingLovelace: "3000000",
            fundingAssets: [],
          },
        ],
        referenceInputs: [],
        referenceScriptBytes: 0,
        requiredBondLovelace: "0",
        requiredRewardCustodyLovelace: "0",
        requiredNativeAssets: [],
        collateralRequired: false,
        conflictRetryCount: 0,
      },
    ],
  }));
};

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const fixture = async (profiles = profileBodies()) => {
  const bundle = createWatcherWorkflowFundingProfileBundle({
    profiles,
  });
  const deployment = makeWatcherDeploymentAuthorityFixture({
    fundingProfileBundleDigest: bundle.fundingProfileBundleDigest,
  });
  const directory = await mkdtemp(join(tmpdir(), "watcher-funding-bundle-"));
  directories.push(directory);
  const bundlePath = join(directory, "funding.json");
  await writeFile(bundlePath, bundle.fundingProfileBundleBytes);
  return { bundle, bundlePath, deploymentIdentity: deployment.result };
};

describe("signed deployment funding bundle", () => {
  it("loads a measured subset and requires funding only for a requested category", async () => {
    const input = await fixture(profileBodies().slice(0, 1));
    const overlay = await loadWatcherWorkflowFundingProfileOverlay(input);
    expect(Object.keys(overlay.profiles)).toEqual(["doubleSpend"]);
    expect(
      workflowFundingProfileFromOverlay({ overlay, category: "doubleSpend" })
        .scope,
    ).toEqual({
      kind: "fraud_proof_category",
      category: "doubleSpend",
    });
    expect(() =>
      workflowFundingProfileFromOverlay({
        overlay,
        category: "validationTraceDispute",
      }),
    ).toThrow("validationTraceDispute has no signed measured funding profile");
  });

  it("loads an empty signed bundle without inventing measurements", async () => {
    const input = await fixture([]);
    const overlay = await loadWatcherWorkflowFundingProfileOverlay(input);
    expect(Object.keys(overlay.profiles)).toEqual([]);
  });

  it("canonicalizes subsets and rejects duplicate categories", () => {
    const profiles = profileBodies().slice(0, 2);
    expect(
      createWatcherWorkflowFundingProfileBundle({
        profiles: [...profiles].reverse(),
      }),
    ).toEqual(createWatcherWorkflowFundingProfileBundle({ profiles }));
    expect(() =>
      createWatcherWorkflowFundingProfileBundle({
        profiles: [profiles[0]!, profiles[0]!],
      }),
    ).toThrow("repeats category doubleSpend");
  });

  it("round trips all categories while preserving the contract blueprint hash", async () => {
    const input = await fixture();
    const overlay = await loadWatcherWorkflowFundingProfileOverlay(input);
    expect(overlay.fundingProfileBundleDigest).toBe(
      input.bundle.fundingProfileBundleDigest,
    );
    expect(overlay.blueprintHash).toBe(input.deploymentIdentity.blueprintHash);
    expect(overlay.blueprintHash).not.toBe(overlay.fundingProfileBundleDigest);
    for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      expect(
        workflowFundingProfileFromOverlay({ overlay, category })
          .deploymentFingerprint,
      ).toBe(input.deploymentIdentity.manifestId);
    }
  });

  it("rejects a replacement bundle even when its envelope digest is recomputed", async () => {
    const input = await fixture();
    const replacement = createWatcherWorkflowFundingProfileBundle({
      profiles: profileBodies().map((profile) => ({
        ...profile,
        measurementArtifactSha256: "77".repeat(32),
      })),
    });
    await writeFile(input.bundlePath, replacement.fundingProfileBundleBytes);
    await expect(
      loadWatcherWorkflowFundingProfileOverlay(input),
    ).rejects.toThrow("does not match signed deployment identity");
  });

  it("rejects a changed envelope digest", async () => {
    const input = await fixture();
    const value = JSON.parse(
      Buffer.from(input.bundle.fundingProfileBundleBytes).toString(),
    ) as Record<string, unknown>;
    value.fundingProfileBundleDigest = "00".repeat(32);
    await writeFile(input.bundlePath, watcherCanonicalJson(value));
    await expect(
      loadWatcherWorkflowFundingProfileOverlay(input),
    ).rejects.toThrow("digest differs");
  });

  it("rejects funding profiles whose declared signer differs from the signed transaction", () => {
    const profiles = profileBodies().map((profile) => ({
      ...profile,
      fundingPaymentKeyHash: "99".repeat(28),
    }));
    expect(() =>
      createWatcherWorkflowFundingProfileBundle({ profiles }),
    ).toThrow();
  });

  it("requires canonical bytes without conflating the envelope hash with the bundle digest", async () => {
    const input = await fixture();
    expect(
      createHash("sha256")
        .update(input.bundle.fundingProfileBundleBytes)
        .digest("hex"),
    ).not.toBe(input.bundle.fundingProfileBundleDigest);
    await writeFile(
      input.bundlePath,
      Buffer.concat([
        Buffer.from(input.bundle.fundingProfileBundleBytes),
        Buffer.from("\n"),
      ]),
    );
    await expect(
      loadWatcherWorkflowFundingProfileOverlay(input),
    ).rejects.toThrow("not canonical JSON");
  });
});
