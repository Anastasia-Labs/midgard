import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { canonicalJson } from "../src/canonical-json.js";
import { DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "../src/deployment-manifest-identity.js";
import {
  AllowListRefusal,
  assertValidationDisputeFitsProfile,
  daBondManifestAmounts,
  DEPLOYMENT_PROFILES,
  deploymentAllowList,
  deploymentFraudProofCatalogueCategories,
  type DeploymentProfile,
  minValidationDisputeMaturityMs,
  requireAllowListed,
  requireSelectedDeploymentProfile,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
  validationDisputeCaseDurationMs,
  validationTraceDisputeInstalled,
  verifyDeploymentProfileBinding,
} from "../src/deployment-profile.js";

describe("deployment profile binding", () => {
  it("uses the SHA-256 digest of the canonical resolved profile", () => {
    expect(
      createHash("sha256")
        .update(canonicalJson(SELECTED_DEPLOYMENT_PROFILE, "profile"))
        .digest("hex"),
    ).toBe(SELECTED_DEPLOYMENT_PROFILE_DIGEST);
  });
  it("accepts the selected profile and rejects the other Preprod deployment", () => {
    expect(
      requireSelectedDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name),
    ).toBe(SELECTED_DEPLOYMENT_PROFILE);
    expect(() =>
      verifyDeploymentProfileBinding(
        SELECTED_DEPLOYMENT_PROFILE,
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        SELECTED_DEPLOYMENT_PROFILE.network,
      ),
    ).not.toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(
        DEPLOYMENT_PROFILES["preprod-public"],
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        "Preprod",
      ),
    ).toThrow();
  });

  it("rejects changed contents even when supplied with the expected digest", () => {
    const profile = structuredClone(SELECTED_DEPLOYMENT_PROFILE);
    expect(() =>
      verifyDeploymentProfileBinding(
        { ...profile, timing: { ...profile.timing, block_maturity_ms: 1 } },
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        profile.network,
      ),
    ).toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(profile, "00".repeat(32), profile.network),
    ).toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(
        profile,
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        "Mainnet",
      ),
    ).toThrow();
    expect(() => requireSelectedDeploymentProfile(undefined)).toThrow();
    expect(() => requireSelectedDeploymentProfile("preprod-public")).toThrow();
  });

  it("maps each DA bond amount to its own manifest key", () => {
    // The testing profiles set the minimum top-up equal to the pool floor, so
    // only a public profile, where every amount differs, catches a swap.
    expect(daBondManifestAmounts(DEPLOYMENT_PROFILES.mainnet)).toEqual({
      daBondLovelace: 100_000_000_000,
      daSlashPenaltyLovelace: 25_000_000_000,
      daBondMinTopUpLovelace: 1_000_000_000,
      daBondPoolFloorLovelace: 5_000_000,
      challengeRecordLovelace: 27_000_000,
    });
  });

  it("derives each deployment's fraud-proof catalogue from its exclusions", () => {
    expect(deploymentFraudProofCatalogueCategories()).toEqual(
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
        (category) => category !== "validationTraceDispute",
      ),
    );
    const curated = deploymentFraudProofCatalogueCategories(
      DEPLOYMENT_PROFILES["preprod-public"],
    );
    const excluded = DEPLOYMENT_PROFILES["preprod-public"]
      .excluded_fraud_proof_categories as readonly string[];
    expect(excluded).not.toContain("validationTraceDispute");
    expect(excluded).toEqual(
      expect.arrayContaining([
        "unusedRedeemer",
        "missingRedeemer",
        "missingScriptSource",
        "unusedScriptWitness",
        "scriptIntegrityHashMismatch",
        "receivePurposeLanguage",
        "executionNativeScriptInvalid",
        "executionSourceScriptDecoding",
        "distinctAssetAccumulationLimit",
      ]),
    );
    expect(excluded).toHaveLength(9);
    expect(
      DEPLOYMENT_PROFILES.mainnet.excluded_fraud_proof_categories,
    ).toContain("validationTraceDispute");
    expect(curated).toEqual(
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
        (category) => !excluded.includes(category),
      ),
    );
    for (const name of ["preprod-testing", "local-devnet-testing"] as const)
      expect(DEPLOYMENT_PROFILES[name].excluded_fraud_proof_categories).toEqual(
        ["validationTraceDispute"],
      );
    expect(
      DEPLOYMENT_PROFILES["preprod-emulator-testing"]
        .excluded_fraud_proof_categories,
    ).toEqual([]);
    expect(
      DEPLOYMENT_PROFILES["preprod-emulator-curated"]
        .excluded_fraud_proof_categories,
    ).toEqual(excluded);
  });
});

describe("execution dispute fit", () => {
  const withTiming = (
    timing: Partial<Record<keyof DeploymentProfile["timing"], number>>,
    excluded: readonly string[] = [],
  ) => ({
    ...DEPLOYMENT_PROFILES["preprod-public"],
    timing: { ...DEPLOYMENT_PROFILES["preprod-public"].timing, ...timing },
    excluded_fraud_proof_categories: excluded,
  });

  it("derives 2 D_case + 1 as validation-dispute-v1.ak does", () => {
    // 2 * 86,400,000 + 2 * (2 * 32 + 3) * 120,000 + 21,600,000
    expect(
      validationDisputeCaseDurationMs(DEPLOYMENT_PROFILES["preprod-public"]),
    ).toBe(210_480_000);
    expect(
      minValidationDisputeMaturityMs(DEPLOYMENT_PROFILES["preprod-public"]),
    ).toBe(420_960_001);
    // 2 * 3,600,000 + 2 * 67 * 20,000 + 900,000
    expect(
      minValidationDisputeMaturityMs(DEPLOYMENT_PROFILES["preprod-testing"]),
    ).toBe(21_560_001);
  });

  it("requires the latest DA answer strictly before half maturity, exactly", () => {
    // 86,400,000 + 480,000 + full < 302,400,000.
    expect(() =>
      assertValidationDisputeFitsProfile(
        "p",
        withTiming({ da_full_response_window_ms: 215_519_999 }),
      ),
    ).not.toThrow();
    expect(() =>
      assertValidationDisputeFitsProfile(
        "p",
        withTiming({ da_full_response_window_ms: 215_520_000 }),
      ),
    ).toThrow(/case open deadline/u);
  });

  it("requires maturity of at least 2 D_case + 1, exactly", () => {
    // R = 113,519,999 puts D_case at 302,399,999: the last M = 604,800,000 admits.
    expect(() =>
      assertValidationDisputeFitsProfile(
        "p",
        withTiming({ dispute_resolution_allowance_ms: 113_519_999 }),
      ),
    ).not.toThrow();
    expect(() =>
      assertValidationDisputeFitsProfile(
        "p",
        withTiming({ dispute_resolution_allowance_ms: 113_520_000 }),
      ),
    ).toThrow(/below 2 D_case \+ 1 = 604800001/u);
  });

  it("admits maturity equal to 2 D_case + 1", () => {
    const emulator = DEPLOYMENT_PROFILES["preprod-emulator-testing"];
    const withMaturity = (block_maturity_ms: number) => ({
      ...emulator,
      timing: { ...emulator.timing, block_maturity_ms },
    });
    expect(() =>
      assertValidationDisputeFitsProfile("p", withMaturity(21_560_001)),
    ).not.toThrow();
    expect(() =>
      assertValidationDisputeFitsProfile("p", withMaturity(21_560_000)),
    ).toThrow(/below 2 D_case \+ 1 = 21560001/u);
  });

  it("loads a 900 s profile that excludes the execution dispute, and refuses it installed", () => {
    const testing = DEPLOYMENT_PROFILES["preprod-testing"];
    expect(testing.timing.block_maturity_ms).toBe(900_000);
    expect(validationTraceDisputeInstalled(testing)).toBe(false);
    expect(() =>
      assertValidationDisputeFitsProfile(testing.name, testing),
    ).not.toThrow();
    expect(() =>
      assertValidationDisputeFitsProfile(testing.name, {
        ...testing,
        excluded_fraud_proof_categories: [],
      }),
    ).toThrow(/installs validationTraceDispute/u);
  });
});

describe("deployment allow-lists", () => {
  const curated = DEPLOYMENT_PROFILES["preprod-emulator-curated"];
  const listed = curated.allow_lists.fraud_provers[0];
  const unlisted = "ab".repeat(28);

  it("admits a listed key and refuses an unlisted one before any build", () => {
    const list = deploymentAllowList("fraud_provers", curated);
    expect(() =>
      requireAllowListed("fraud_provers", listed, list),
    ).not.toThrow();
    expect(() =>
      requireAllowListed("fraud_provers", listed.toUpperCase(), list),
    ).not.toThrow();
    const refusal = (() => {
      try {
        requireAllowListed("fraud_provers", unlisted, list);
      } catch (error) {
        return error;
      }
      return undefined;
    })();
    expect(refusal).toBeInstanceOf(AllowListRefusal);
    expect(refusal).toMatchObject({ role: "fraud_provers", keyHash: unlisted });
  });

  it("admits every key when the list is None, the selected testing profile's", () => {
    expect(deploymentAllowList("operators")).toBeNull();
    for (const role of [
      "operators",
      "fraud_provers",
      "da_challengers",
    ] as const)
      expect(() => requireAllowListed(role, unlisted)).not.toThrow();
    expect(() => requireAllowListed("operators", unlisted, [])).toThrow(
      /not on the deployment's operators allow-list/u,
    );
  });
});
