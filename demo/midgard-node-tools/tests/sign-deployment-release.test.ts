import { createHash, generateKeyPairSync, type KeyObject } from "node:crypto";
import {
  access,
  mkdtemp,
  readdir,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  parseDeploymentManifestEconomics,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DeploymentReleaseTrustRoot,
  verifySignedDeploymentRelease,
} from "@al-ft/midgard-core/deployment-release";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import {
  loadWatcherVerifiedDeploymentAuthority,
  loadWatcherWorkflowFundingProfileOverlay,
  type WatcherWorkflowFundingProfileBody,
  workflowFundingProfileFromOverlay,
} from "midgard-watcher";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";

type Fixture = Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
type Authority = {
  signedIdentity: {
    manifest: Record<string, unknown>;
    releaseBindings: { programCommitments: Record<string, string> };
    attestation: { signature: string; trustRootId: string };
  };
  trustRoots: DeploymentReleaseTrustRoot[];
};

let fixture: Fixture;
let directory: string;
let inputs: { deploymentInfo: string; blueprint: string };

/** The computation-thread commitment of a deployment, as the watcher journeys bind it. */
const computationThreadCommitment = () =>
  createHash("sha256")
    .update(
      JSON.stringify({
        computationThreadPolicyId:
          fixture.manifest.contracts.computationThreadMint!.scriptHash,
      }),
    )
    .digest("hex");

const pem = (key: KeyObject) =>
  key.export({ format: "pem", type: "pkcs8" }) as string;

/** A release input naming a persistent signing key file, as an operator writes it. */
const releaseInput = async (
  name: string,
  key: KeyObject,
  overrides: Record<string, unknown> = {},
) => {
  const signingKeyFile = join(directory, `${name}.pem`);
  await writeFile(signingKeyFile, pem(key), { mode: 0o600 });
  const registryFile = join(directory, `${name}-parameters.json`);
  await writeFile(registryFile, JSON.stringify(fixture.retained));
  const path = join(directory, `${name}-release-input.json`);
  await writeFile(
    path,
    JSON.stringify({
      signingKeyFile,
      programCommitments: {
        "computation-thread-policy-v1": computationThreadCommitment(),
      },
      fundingProfiles: [],
      parameterRegistryFile: registryFile,
      ...overrides,
    }),
  );
  return path;
};

const sign = async (
  name: string,
  key: KeyObject,
  overrides: Record<string, unknown> = {},
) => {
  const output = join(directory, name);
  const result = await signDeploymentRelease({
    releaseInput: await releaseInput(name, key, overrides),
    ...inputs,
    output,
  });
  return { output, result };
};

const exists = (path: string) =>
  access(path).then(
    () => true,
    () => false,
  );

const readJson = async <T = unknown>(path: string) =>
  JSON.parse(await readFile(path, "utf8")) as T;

const file = (output: string, name: keyof typeof SIGNED_RELEASE_FILES) =>
  join(output, SIGNED_RELEASE_FILES[name]);

/** Loads a release the way the watcher process does. */
const loadInWatcher = async (
  output: string,
  authority = file(output, "authority"),
) => {
  const verified = await loadWatcherVerifiedDeploymentAuthority({
    path: authority,
    ruleBundlePath: file(output, "rules"),
  });
  const overlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: file(output, "funding"),
    deploymentIdentity: verified.deploymentIdentity,
  });
  return { ...verified.deploymentIdentity, overlay };
};

/**
 * One measured funding profile for this deployment and the given funding
 * wallet: an ordinary signed wallet transfer, standing in for a measurement.
 */
const fundingProfile = (): {
  profile: WatcherWorkflowFundingProfileBody;
  paymentKeyHash: string;
} => {
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
  const paymentKeyHash = key.to_public().hash().to_hex();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  ).to_address();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
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
  const economics = parseDeploymentManifestEconomics(
    fixture.manifest.economics,
  );
  return {
    paymentKeyHash,
    profile: {
      scope: { kind: "fraud_proof_category", category: "invalidSignature" },
      blueprintSha256: fixture.manifest.artifacts.blueprintHash,
      protocolParametersDigest:
        fixture.manifest.cardanoProtocolParameters.digest,
      economicsPolicyDigest: computeFraudProofReleaseEconomicsPolicyDigest({
        profile: economics.profile,
        requiredBondLovelace: economics.requiredBondLovelace.toString(),
        slashingPenaltyLovelace: economics.slashingPenaltyLovelace.toString(),
        fraudProverRewardLovelace:
          economics.fraudProverRewardLovelace.toString(),
        inactivitySlashingPenaltyLovelace:
          economics.inactivitySlashingPenaltyLovelace.toString(),
        proverCollateralFloorLovelace:
          economics.proverCollateralFloorLovelace.toString(),
      }),
      fundingPaymentKeyHash: paymentKeyHash,
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
    } as WatcherWorkflowFundingProfileBody,
  };
};

beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-sign-release-"));
  inputs = {
    deploymentInfo: join(directory, "contract-deployment-info.json"),
    blueprint: join(directory, "plutus.json"),
  };
  await writeFile(inputs.deploymentInfo, JSON.stringify(fixture.manifest));
  await writeFile(inputs.blueprint, fixture.blueprintJson);
}, 120_000);
afterAll(() => rm(directory, { recursive: true, force: true }));

describe("sign-deployment-release", () => {
  it("writes a release that the node's release verifiers and the watcher accept", async () => {
    const { output, result } = await sign(
      "accepted",
      generateKeyPairSync("ed25519").privateKey,
    );
    expect(result).toMatchObject({
      manifestId: fixture.manifest.manifestId,
      deploymentProfile: fixture.manifest.deploymentProfile.name,
    });
    // No provenance: the test build compiles a testing profile, which has none.
    expect(await readdir(output)).toEqual(
      Object.entries(SIGNED_RELEASE_FILES)
        .filter(([name]) => name !== "provenance")
        .map(([, file]) => file)
        .sort(),
    );
    const authority = await readJson<Authority>(file(output, "authority"));
    const trustRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(output, "trustRoots"),
    );
    const release = verifySignedDeploymentRelease({
      signedIdentity: authority.signedIdentity,
      trustRoots,
    });
    expect(release.trustRootId).toBe(result.trustRootId);
    expect(release.manifest.manifestId).toBe(fixture.manifest.manifestId);
    for (const manifest of ["manifest", "deploymentInfo"] as const)
      expect(
        verifyFinalizedDeploymentManifest(
          await readJson(file(output, manifest)),
        ).manifestId,
      ).toBe(fixture.manifest.manifestId);
    expect((await loadInWatcher(output)).manifestId).toBe(
      fixture.manifest.manifestId,
    );
  });

  it("refuses a release with a tampered signed field", async () => {
    const { output } = await sign(
      "tampered",
      generateKeyPairSync("ed25519").privateKey,
    );
    const authority = await readJson<Authority>(file(output, "authority"));
    const trustRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(output, "trustRoots"),
    );
    authority.signedIdentity.releaseBindings.programCommitments[
      "computation-thread-policy-v1"
    ] = "ce".repeat(32);
    const tampered = join(directory, "tampered-authority.json");
    await writeFile(tampered, JSON.stringify(authority));
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: authority.signedIdentity,
        trustRoots,
      }),
    ).toThrow("Invalid deployment release signature");
    await expect(loadInWatcher(output, tampered)).rejects.toThrow(
      "invalid_signature at $.attestation.signature",
    );
  });

  it("refuses a release signed by a key other than the trusted one", async () => {
    const trusted = await sign(
      "trusted",
      generateKeyPairSync("ed25519").privateKey,
    );
    const other = await sign(
      "other",
      generateKeyPairSync("ed25519").privateKey,
    );
    const otherAuthority = await readJson<Authority>(
      file(other.output, "authority"),
    );
    const trustedRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(trusted.output, "trustRoots"),
    );
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: otherAuthority.signedIdentity,
        trustRoots: trustedRoots,
      }),
    ).toThrow("Untrusted deployment release signer");
    // The other key's signature presented under the trusted key's root.
    const trustedAuthority = await readJson<Authority>(
      file(trusted.output, "authority"),
    );
    trustedAuthority.signedIdentity.attestation.signature =
      otherAuthority.signedIdentity.attestation.signature;
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: trustedAuthority.signedIdentity,
        trustRoots: trustedRoots,
      }),
    ).toThrow("Invalid deployment release signature");
    const forged = join(directory, "forged-authority.json");
    await writeFile(forged, JSON.stringify(trustedAuthority));
    await expect(loadInWatcher(trusted.output, forged)).rejects.toThrow(
      "invalid_signature at $.attestation.signature",
    );
  });

  it("refuses a signing key that is not ed25519 and an existing output directory", async () => {
    await expect(
      signDeploymentRelease({
        releaseInput: await releaseInput(
          "ec-key",
          generateKeyPairSync("ec", { namedCurve: "P-256" }).privateKey,
        ),
        ...inputs,
        output: join(directory, "ec-key"),
      }),
    ).rejects.toThrow("Release signing key must be a persistent ed25519 key");
    const { output } = await sign(
      "existing",
      generateKeyPairSync("ed25519").privateKey,
    );
    await expect(
      signDeploymentRelease({
        releaseInput: await releaseInput(
          "existing-again",
          generateKeyPairSync("ed25519").privateKey,
        ),
        ...inputs,
        output,
      }),
    ).rejects.toThrow("EEXIST");
  });

  it("signs the computation-thread commitment the deployment derives and refuses another", async () => {
    const { output } = await sign(
      "derived-commitment",
      generateKeyPairSync("ed25519").privateKey,
      { programCommitments: {} },
    );
    const authority = await readJson<Authority>(file(output, "authority"));
    expect(authority.signedIdentity.releaseBindings.programCommitments).toEqual(
      { "computation-thread-policy-v1": computationThreadCommitment() },
    );
    await expect(
      sign("mistyped-commitment", generateKeyPairSync("ed25519").privateKey, {
        programCommitments: { "computation-thread-policy-v1": "cd".repeat(32) },
      }),
    ).rejects.toThrow(
      "programCommitments.computation-thread-policy-v1 differs from the deployment's computation-thread policy",
    );
    expect(await exists(join(directory, "mistyped-commitment"))).toBe(false);
  });

  it("leaves no output directory behind a refusal, so a retry into it succeeds", async () => {
    const key = generateKeyPairSync("ed25519").privateKey;
    const output = join(directory, "retried");
    const otherBlueprint = join(directory, "other-plutus.json");
    await writeFile(otherBlueprint, `${fixture.blueprintJson}\n`);
    const input = await releaseInput("retried", key);
    await expect(
      signDeploymentRelease({
        releaseInput: input,
        deploymentInfo: inputs.deploymentInfo,
        blueprint: otherBlueprint,
        output,
      }),
    ).rejects.toThrow("Blueprint differs from the deployed release");
    expect(await exists(output)).toBe(false);
    const retried = await signDeploymentRelease({
      releaseInput: input,
      ...inputs,
      output,
    });
    expect(retried.manifestId).toBe(fixture.manifest.manifestId);
    expect((await loadInWatcher(output)).manifestId).toBe(
      fixture.manifest.manifestId,
    );
  });

  it("signs measured funding profiles for the named funding wallet only", async () => {
    const { profile, paymentKeyHash } = fundingProfile();
    const key = generateKeyPairSync("ed25519").privateKey;
    const withProfiles = await releaseInput("funded", key, {
      fundingProfiles: [profile],
    });
    await expect(
      signDeploymentRelease({
        releaseInput: withProfiles,
        ...inputs,
        output: join(directory, "funded-unnamed"),
      }),
    ).rejects.toThrow(
      "Funding profiles require --funding-payment-key-hash of the prover funding wallet",
    );
    await expect(
      signDeploymentRelease({
        releaseInput: withProfiles,
        ...inputs,
        output: join(directory, "funded-other-wallet"),
        fundingPaymentKeyHash: "99".repeat(28),
      }),
    ).rejects.toThrow(
      "Measured funding profiles differ from this deployment or funding wallet",
    );
    const output = join(directory, "funded");
    await signDeploymentRelease({
      releaseInput: withProfiles,
      ...inputs,
      output,
      fundingPaymentKeyHash: paymentKeyHash,
    });
    const { overlay } = await loadInWatcher(output);
    expect(
      workflowFundingProfileFromOverlay({
        overlay,
        category: "invalidSignature",
      }).fundingPaymentKeyHash,
    ).toBe(paymentKeyHash);
  });
});
