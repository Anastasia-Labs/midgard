import { createHash } from "node:crypto";

import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

// Isolate propagation across the binding boundary. Manifest verification and
// contract compilation have their own suites; the document parsers, release
// policy derivation, and removal economics reader here are real.
const dependencies = vi.hoisted(() => ({
  verify: vi.fn<(value: unknown) => Record<string, unknown>>(),
  resolve: vi.fn(),
  resolveDispute: vi.fn(),
}));
vi.mock("@al-ft/midgard-core/deployment-manifest-identity", async (load) => ({
  ...(await load<
    typeof import("@al-ft/midgard-core/deployment-manifest-identity")
  >()),
  verifyFinalizedDeploymentManifest: dependencies.verify,
}));
vi.mock("../src/runtime.js", async (load) => ({
  ...(await load<typeof import("../src/runtime.js")>()),
  resolveFaultProofDeploymentContracts: dependencies.resolve,
  resolveValidationTraceDisputeDeploymentContracts: dependencies.resolveDispute,
}));

import {
  parseContractDeploymentInfo,
  parseContractDeploymentReferenceScriptAuthPolicyId,
} from "../src/inspect-contracts.js";
import { fraudSlashEconomicsFromDeploymentManifest } from "../src/remove-fraudulent-block.js";
import { bindValidationTraceDisputeWorkflowDeployment } from "../src/validation-dispute/workflow-binding.js";
import {
  bindFraudProofTerminalDeployment,
  bindFraudProofWorkflowDeployment,
} from "../src/workflow/deployment-manifest-binding.js";

const blueprintJson = "{}";
const script = {
  type: "Native" as const,
  script: `8200581c${"55".repeat(28)}`,
};
const scriptHash = validatorToScriptHash(script);
const fixture = () => {
  const catalogue = {
    root: "66".repeat(32),
    categories: Object.fromEntries(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [
        category,
        {
          categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category],
          scriptHash,
          membershipProofCbor: "80",
        },
      ]),
    ),
  };
  const contracts = Object.fromEntries(
    [
      "stateQueueSpend",
      "stateQueueMint",
      "fraudProofSpend",
      "fraudProofMint",
      "fraudProofCatalogueSpend",
      "fraudProofCatalogueMint",
      "activeOperatorsSpend",
      "activeOperatorsMint",
      "retiredOperatorsSpend",
      "retiredOperatorsMint",
      "schedulerSpend",
    ].map((name, index) => [
      name,
      {
        scriptHash,
        contract: { type: script.type, cborHex: script.script },
        refScriptUTxO: { txHash: "77".repeat(32), outputIndex: index },
        ...(name === "fraudProofCatalogueMint"
          ? { fraudProofCatalogue: catalogue }
          : {}),
      },
    ]),
  );
  const manifest = {
    manifestId: "88".repeat(32),
    network: "Preprod",
    artifacts: {
      blueprintHash: createHash("sha256").update(blueprintJson).digest("hex"),
    },
    l1Finality: { ...DEPLOYMENT_MANIFEST_L1_FINALITY },
    economics: {
      ...DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    },
    cardanoProtocolParameters: { snapshot: { minFeeA: "44" } },
    contracts,
    referenceScriptAuthPolicy: {
      policyId: scriptHash,
      nativeScript: { type: "Native", cborHex: script.script },
      tokenNames: { ...DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES },
    },
  };
  const deploymentInfo = structuredClone({
    contracts,
    referenceScriptAuthPolicy: manifest.referenceScriptAuthPolicy,
  });
  const resolved = {
    category: catalogue.categories.doubleSpend,
    fraudProofCataloguePolicyId: scriptHash,
    contracts: {
      doubleSpend: { steps: [], firstStep: { spendingScriptHash: scriptHash } },
      computationThread: { policyId: scriptHash },
    },
  };
  dependencies.verify.mockReturnValue(manifest);
  dependencies.resolve.mockResolvedValue(resolved);
  return {
    manifest,
    deploymentInfo,
    resolved,
    bind: (document: unknown = deploymentInfo) =>
      bindFraudProofWorkflowDeployment({
        manifest,
        blueprintJson,
        deploymentInfo: document,
        category: "doubleSpend",
        headerHash: "99".repeat(28),
        proverCredential: "aa".repeat(28),
        stepDatumSchemas: [],
      }),
  };
};

beforeEach(() => vi.resetAllMocks());

describe("manifest-bound builder document", () => {
  it("supplies the removal reader with verified economics when contract metadata omits it", async () => {
    const current = fixture();
    expect(() =>
      fraudSlashEconomicsFromDeploymentManifest(current.deploymentInfo),
    ).toThrow("economics must be an object");
    const binding = await current.bind();
    expect(dependencies.verify).toHaveBeenCalledExactlyOnceWith(
      current.manifest,
    );
    expect(binding.deploymentInfo).toEqual(current.manifest);
    expect(binding.deploymentInfo).not.toBe(current.manifest);
    expect(dependencies.resolve).toHaveBeenCalledWith(
      expect.objectContaining({ deploymentInfo: binding.deploymentInfo }),
    );
    expect(parseContractDeploymentInfo(binding.deploymentInfo)).toEqual(
      binding.contractEntries,
    );
    expect(
      parseContractDeploymentReferenceScriptAuthPolicyId(
        binding.deploymentInfo,
        "reference-script-auth minting",
      ),
    ).toBe(scriptHash);
    expect(
      fraudSlashEconomicsFromDeploymentManifest(binding.deploymentInfo),
    ).toEqual({
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900_000_000n,
      slashingPenaltyLovelace: 500_000_000n,
      inactivitySlashingPenaltyLovelace: 100_000_000n,
      fraudProverRewardLovelace: 400_000_000n,
      proverCollateralFloorLovelace: 5_000_000n,
    });
    await expect(
      current.bind({
        ...current.deploymentInfo,
        economics: current.manifest.economics,
      }),
    ).resolves.toMatchObject({
      deploymentFingerprint: current.manifest.manifestId,
    });
  });

  it("rejects conflicting or malformed caller economics and cannot fill absent verified economics from it", async () => {
    const current = fixture();
    await expect(
      current.bind({
        ...current.deploymentInfo,
        economics:
          DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["public-preprod-launch-v1"],
      }),
    ).rejects.toThrow("changed the finalized manifest economics");
    await expect(
      current.bind({ ...current.deploymentInfo, economics: undefined }),
    ).rejects.toThrow("economics must be an object");
    const { economics: _economics, ...missingEconomics } = current.manifest;
    dependencies.verify.mockReturnValue(missingEconomics);
    await expect(
      current.bind({
        ...current.deploymentInfo,
        economics: current.manifest.economics,
      }),
    ).rejects.toThrow("economics must be an object");
    expect(dependencies.resolve).not.toHaveBeenCalled();
  });

  it("preserves contract, reference-output, and reference-authority mismatch refusal", async () => {
    const current = fixture();
    const substituted = structuredClone(current.deploymentInfo);
    substituted.contracts.stateQueueSpend!.refScriptUTxO.outputIndex += 1;
    await expect(current.bind(substituted)).rejects.toThrow(
      "changed finalized manifest contract stateQueueSpend",
    );
    const foreignScript = {
      type: "Native" as const,
      script: `8200581c${"11".repeat(28)}`,
    };
    await expect(
      current.bind({
        ...current.deploymentInfo,
        referenceScriptAuthPolicy: {
          ...current.deploymentInfo.referenceScriptAuthPolicy,
          policyId: validatorToScriptHash(foreignScript),
          nativeScript: {
            type: foreignScript.type,
            cborHex: foreignScript.script,
          },
        },
      }),
    ).rejects.toThrow("changed the finalized reference-script authority");
    expect(dependencies.resolve).not.toHaveBeenCalled();
  });

  it("detaches and freezes the verified builder document before asynchronous compilation", async () => {
    const current = fixture();
    let finish!: (value: unknown) => void;
    dependencies.resolve.mockImplementation(
      () =>
        new Promise((resolve) => {
          finish = resolve;
        }),
    );
    const running = current.bind();
    current.manifest.economics.requiredBondLovelace = 1 as 900000000;
    current.manifest.contracts.stateQueueSpend!.refScriptUTxO.outputIndex = 999;
    current.deploymentInfo.referenceScriptAuthPolicy.policyId = "ff".repeat(28);
    finish(current.resolved);
    const binding = await running;
    const document = binding.deploymentInfo as typeof current.manifest;
    expect(Object.isFrozen(document)).toBe(true);
    expect(Object.isFrozen(document.economics)).toBe(true);
    expect(
      Object.isFrozen(document.contracts.stateQueueSpend!.refScriptUTxO),
    ).toBe(true);
    expect(() =>
      Object.assign(document.economics, { requiredBondLovelace: 1 }),
    ).toThrow();
    expect(
      fraudSlashEconomicsFromDeploymentManifest(document).requiredBondLovelace,
    ).toBe(900_000_000n);
    expect(document.contracts.stateQueueSpend!.refScriptUTxO.outputIndex).toBe(
      0,
    );
    expect(binding.releaseEconomics.policy.requiredBondLovelace).toBe(
      "900000000",
    );
  });

  it.each(["linear", "dispute"] as const)(
    "%s binding rejects a supplied blueprint that differs from the verified manifest",
    async (family) => {
      const current = fixture();
      const input = {
        manifest: current.manifest,
        blueprintJson: '{ "different": true }',
        deploymentInfo: current.deploymentInfo,
        headerHash: "99".repeat(28),
        proverCredential: "aa".repeat(28),
      };
      const running =
        family === "dispute"
          ? bindValidationTraceDisputeWorkflowDeployment(input)
          : bindFraudProofWorkflowDeployment({
              ...input,
              category: "doubleSpend",
              stepDatumSchemas: [],
            });
      await expect(running).rejects.toThrow("blueprint SHA-256 does not match");
      expect(dependencies.resolve).not.toHaveBeenCalled();
      expect(dependencies.resolveDispute).not.toHaveBeenCalled();
    },
  );

  it("preserves dispute manifest script validation after asynchronous compilation", async () => {
    const current = fixture();
    let finish!: (value: unknown) => void;
    dependencies.resolveDispute.mockImplementation(
      () =>
        new Promise((resolve) => {
          finish = resolve;
        }),
    );
    const running = bindValidationTraceDisputeWorkflowDeployment({
      manifest: current.manifest,
      blueprintJson,
      deploymentInfo: current.deploymentInfo,
      headerHash: "99".repeat(28),
      proverCredential: "aa".repeat(28),
    });
    current.manifest.contracts.stateQueueSpend!.scriptHash = "ff".repeat(28);
    finish({
      validationTraceDisputeCategory:
        current.manifest.contracts.fraudProofCatalogueMint!.fraudProofCatalogue!
          .categories.validationTraceDispute,
      contracts: {
        validationTraceDispute: {
          firstStep: { spendingScriptHash: scriptHash },
        },
      },
    });
    await expect(running).rejects.toThrow(
      "deployment manifest stateQueueSpend script bytes/hash disagree",
    );
    expect(Object.isFrozen(current.manifest)).toBe(false);
  });

  it("stops before parsing builder metadata when manifest verification fails", async () => {
    const current = fixture();
    dependencies.verify.mockImplementation(() => {
      throw new Error("manifest identity mismatch");
    });
    await expect(current.bind()).rejects.toThrow("manifest identity mismatch");
    expect(dependencies.resolve).not.toHaveBeenCalled();
  });
});

it("binds completed observation metadata without admitting an executable thread decoder", async () => {
  const current = fixture();
  dependencies.resolve.mockResolvedValue({
    ...current.resolved,
    contracts: {
      ...current.resolved.contracts,
      doubleSpend: {
        ...current.resolved.contracts.doubleSpend,
        steps: [{ spendingScriptAddress: "published-step-address" }],
      },
    },
  });
  await expect(current.bind()).rejects.toThrow("expected 0 computation steps");
  const terminal = await bindFraudProofTerminalDeployment({
    manifest: current.manifest,
    blueprintJson,
    deploymentInfo: current.deploymentInfo,
    category: "doubleSpend",
    headerHash: "99".repeat(28),
    proverCredential: "aa".repeat(28),
  });
  expect(terminal.definition.computationThread.steps).toEqual([
    { role: "computation_thread_step_01", address: "published-step-address" },
  ]);
  expect(Object.keys(terminal).sort()).toEqual([
    "definition",
    "deploymentFingerprint",
    "releaseEconomics",
    "releaseFinality",
  ]);
});
