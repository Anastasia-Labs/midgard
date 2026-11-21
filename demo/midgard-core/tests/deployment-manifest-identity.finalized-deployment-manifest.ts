import {
  credentialToAddress,
  scriptFromNative,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import { deploymentDaRetentionDays } from "../src/da-transport.js";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { validateFinalizedDa } from "../src/deployment-manifest-identity/finalized.validate-finalized-da.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

describe("finalized deployment manifest", () => {
  it("returns a typed view of the original document, including on repeated verification", () => {
    const document = finalizedManifest();
    const verified = verifyFinalizedDeploymentManifest(document);
    expectTypeOf(verified.network).toEqualTypeOf<
      "Mainnet" | "Preprod" | "Preview" | "Custom"
    >();
    expectTypeOf(verified.artifacts.blueprintHash).toEqualTypeOf<string>();
    expectTypeOf(
      verified.cardanoProtocolParameters.snapshot.maxTxSize,
    ).toEqualTypeOf<string>();
    expect(verified).toBe(document);
    expect(verifyFinalizedDeploymentManifest(document)).toBe(document);
    expect(Object.isFrozen(document)).toBe(false);
    expect(Object.isFrozen(document.artifacts)).toBe(false);
  });

  it("binds the manifest DA minimum to the actual compiled deployment profile", () => {
    const da = finalizedManifest().da;
    const withRetention = (retentionDays: number) => ({
      ...da,
      transportProfile: { ...da.transportProfile, retentionDays },
    });
    const minimum = deploymentDaRetentionDays();
    expect(() => validateFinalizedDa(withRetention(minimum))).not.toThrow();
    expect(() => validateFinalizedDa(withRetention(minimum - 1))).toThrow();
  });

  it.each([
    [
      "network",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.network = "unsupported";
      },
      /compiled profile/,
    ],
    [
      "blueprint identity",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.artifacts.blueprintHash = "00";
      },
      /blueprintHash/,
    ],
    [
      "protocol parameter snapshot",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.cardanoProtocolParameters.snapshot.maxTxSize = "0";
        manifest.cardanoProtocolParameters.digest =
          computeDeploymentManifestJsonDigest(
            manifest.cardanoProtocolParameters.snapshot,
          );
      },
      /bounds must be positive/,
    ],
    [
      "protocol parameter digest",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.cardanoProtocolParameters.digest = "66".repeat(32);
      },
      /cardanoProtocolParameters\.digest mismatch/,
    ],
    [
      "contract hash",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.hubOracleMint.scriptHash = "00".repeat(28);
      },
      /scriptHash mismatch/,
    ],
    [
      "missing DA bond pool contract",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        delete manifest.contracts.daBondPoolMint;
      },
      /contracts\.daBondPoolMint is required/,
    ],
    [
      "availability contract outside the four arms",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.availabilityChallengeExtraWithdraw = {
          ...manifest.contracts.availabilityChallengeTimeoutWithdraw!,
        };
      },
      /contracts\.availabilityChallengeExtraWithdraw is unexpected/,
    ],
    [
      "retired unsupported escape-hatch scaffold",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.escapeHatchSpend = {
          ...manifest.contracts.txOrderSpend!,
        };
      },
      /contracts\.escapeHatchSpend is unexpected/,
    ],
    [
      "noncanonical dispute maturity",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        Object.assign(manifest.validationDispute, { maturityMs: 39_600_000 });
      },
      /validationDispute\.maturityMs must equal/,
    ],
    [
      "removed dispute response window",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        Object.assign(manifest.validationDispute, {
          responseWindowMs: 300_000,
        });
      },
      /validationDispute\.responseWindowMs is unexpected/,
    ],
    [
      "fraud-proof catalogue root",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        const mint = manifest.contracts.fraudProofCatalogueMint!;
        mint.fraudProofCatalogue = {
          ...mint.fraudProofCatalogue!,
          root: "33".repeat(32),
        };
      },
      /catalogue root mismatch/,
    ],
    [
      "DA committee with a duplicate key",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.da.committeeVkeys.push(manifest.da.committeeVkeys[0]!);
      },
      /committeeVkeys must be unique/,
    ],
    [
      "DA threshold above the committee size",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.da.threshold = manifest.da.committeeVkeys.length + 1;
      },
      /threshold exceeds committee size/,
    ],
    [
      "DA zstd level above the highest standard level",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.da.transportProfile.zstdLevel = 22;
      },
      /zstdLevel must not exceed 19/,
    ],
    [
      "reference publication",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.referenceScripts["hub-oracle minting"].status = "pending";
      },
      /status must be confirmed/,
    ],
    [
      "required initialization",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.initProtocol.status = "pending";
      },
      /initProtocol.status must be complete/,
    ],
    [
      "optional step status",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.operatorActivation.status = "unsupported";
      },
      /operatorActivation.status is unsupported/,
    ],
    [
      "non-string step status",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        (manifest.steps.operatorActivation as { status: unknown }).status = [
          "pending",
        ];
      },
      /operatorActivation.status is unsupported/,
    ],
  ] as const)(
    "rejects invalid %s even with a matching document ID",
    (_name, mutate, error) => {
      const manifest = finalizedManifest();
      mutate(manifest);
      const { manifestId: _manifestId, ...identity } = manifest;
      manifest.manifestId = computeDeploymentManifestId(identity);
      // Identity-only verification deliberately does not claim finalization.
      if (_name !== "network") {
        expect(verifyDeploymentManifestIdentity(manifest)).toBe(manifest);
      }
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(error);
    },
  );

  it("refuses a noncanonical consensus profile or profile digest", () => {
    const manifest = finalizedManifest();
    expect(() =>
      verifyFinalizedDeploymentManifest({
        ...manifest,
        consensusProfile: {
          ...manifest.consensusProfile,
          profileId: "unsupported-profile-99",
        },
      }),
    ).toThrow(/consensusProfile must exactly match canonical V1/u);
    expect(() =>
      verifyFinalizedDeploymentManifest({
        ...manifest,
        consensusProfileDigest: "77".repeat(32),
      }),
    ).toThrow(/consensusProfileDigest must exactly match canonical V1/u);
  });

  it("accepts publisher-signed authority with a distinct script reference recipient", () => {
    const manifest = finalizedManifest();
    const policy = scriptFromNative({
      type: "all",
      scripts: [
        { type: "sig", keyHash: "ab".repeat(28) },
        {
          type: "before",
          slot: manifest.referenceScriptAuthPolicy.nativeScript.expiresAtSlot,
        },
      ],
    });
    const policyId = validatorToScriptHash(policy);
    manifest.referenceScriptDeployAddress = credentialToAddress("Preview", {
      type: "Script",
      hash: "cd".repeat(28),
    });
    manifest.referenceScriptAuthPolicy.policyId = policyId;
    manifest.referenceScriptAuthPolicy.nativeScript.cborHex = policy.script;
    manifest.referenceScriptAuthPolicy.postTimelockAudit = {
      required: false,
      rule: "Audit after publisher-authorized publication completes.",
    };
    manifest.contracts.referenceScriptAuthMint = {
      ...manifest.contracts.referenceScriptAuthMint!,
      contract: { type: "Native", cborHex: policy.script },
      scriptHash: policyId,
    };
    for (const [role, reference] of Object.entries(manifest.referenceScripts)) {
      reference.roleUnit = `${policyId}${Buffer.from(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
        ],
      ).toString("hex")}`;
      if (role === "reference-script-auth minting")
        reference.scriptHash = policyId;
    }
    const { manifestId: _manifestId, ...identity } = manifest;
    manifest.manifestId = computeDeploymentManifestId(identity);
    expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
  });

  it("rechecks document identity after a previously verified caller-owned value changes", () => {
    const manifest = finalizedManifest();
    verifyFinalizedDeploymentManifest(manifest);
    manifest.artifacts.blueprintHash = "66".repeat(32);
    expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
      /id mismatch/,
    );
  });
});
