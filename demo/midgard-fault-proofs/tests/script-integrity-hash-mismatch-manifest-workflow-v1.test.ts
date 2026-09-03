import { beforeEach, describe, expect, it, vi } from "vitest";

const mocks = vi.hoisted(() => ({
  bindDeployment: vi.fn(),
  assertSigner: vi.fn(),
  bindReference: vi.fn(
    ({ utxo }: { utxo: unknown; contractName: string }) => utxo,
  ),
  createL1: vi.fn(() => ({ kind: "l1" })),
  createActuator: vi.fn((_input: unknown) => ({ kind: "actuator" })),
}));

vi.mock("../src/workflow/deployment-manifest-binding-v1.js", () => ({
  bindFraudProofWorkflowDeployment: mocks.bindDeployment,
  assertManifestBoundWorkflowSigner: mocks.assertSigner,
  requireManifestBoundReferenceScriptUtxo: mocks.bindReference,
}));
vi.mock("../src/workflow/family-l1-observation-v1.js", () => ({
  createFraudProofFamilyLocalKupmiosL1ObservationPort: mocks.createL1,
}));
vi.mock("../src/script-integrity-hash-mismatch/lucid-actuator-v1.js", () => ({
  createScriptIntegrityHashMismatchLucidActuator: mocks.createActuator,
}));

import {
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS,
} from "../src/script-integrity-hash-mismatch/manifest-workflow-v1.js";

const utxo = (index: number) => ({
  txHash: index.toString(16).padStart(64, "0"),
  outputIndex: 0,
});

const config = () => ({
  manifest: {},
  blueprintJson: "{}",
  deploymentInfo: {},
  headerHash: "11".repeat(32),
  lucid: {},
  signer: { paymentKeyHash: "22".repeat(28), address: "addr_test1_prover" },
  source: {},
  decisionDigest: "33".repeat(32),
  stateQueueMutationLeaseCoordinator: {},
  referenceScripts: {
    steps: [utxo(1), utxo(2), utxo(3), utxo(4), utxo(5)],
    witnesses: {
      computationThreadMint: utxo(6),
      fraudProofMint: utxo(7),
      phasMembershipWithdraw: utxo(8),
      chunkedVerifyWithdraw: utxo(9),
      pexcludesWithdraw: utxo(10),
    },
    removal: {
      correctionLockSpend: utxo(11),
      stateQueueSpend: utxo(12),
      stateQueueMint: utxo(13),
      stateQueueFraudRemovalWithdraw: utxo(14),
      activeOperatorsSpend: utxo(15),
      activeOperatorsMint: utxo(16),
      retiredOperatorsSpend: utxo(17),
      retiredOperatorsMint: utxo(18),
      schedulerSpend: utxo(19),
    },
  },
});

describe("scriptIntegrityHashMismatch manifest workflow", () => {
  beforeEach(() => {
    Object.values(mocks).forEach((mock) => mock.mockClear());
    mocks.bindDeployment.mockResolvedValue({
      network: "Preview",
      blueprint: {},
      deploymentInfo: { hubOracleMint: { scriptHash: "44".repeat(28) } },
      definition: {
        category: "scriptIntegrityHashMismatch",
        headerHash: "11".repeat(32),
      },
      deploymentFingerprint: "55".repeat(32),
      releaseFinality: {},
      releaseEconomics: {
        policy: { fraudProverRewardLovelace: "1" },
      },
      resolvedContracts: {
        stateQueuePolicyId: "66".repeat(28),
        category: { categoryId: "00000033" },
        contracts: {
          scriptIntegrityHashMismatch: {
            steps: Array.from({ length: 5 }, (_, index) => ({
              spendingScript: { type: "PlutusV3", script: "00" },
              spendingScriptHash: (index + 1).toString(16).padStart(56, "0"),
              spendingScriptAddress: "addr_test1_step",
            })),
          },
          computationThread: {
            policyId: "77".repeat(28),
            mintingScript: { type: "PlutusV3", script: "00" },
          },
          fraudProof: {
            policyId: "88".repeat(28),
            mintingScript: { type: "PlutusV3", script: "00" },
            spendingScriptAddress: "addr_test1_proof",
          },
        },
      },
    });
  });

  it("binds exactly five steps, five witnesses, and nine removal roles", async () => {
    await expect(
      createManifestBoundScriptIntegrityHashMismatchWorkflow(config() as never),
    ).resolves.toMatchObject({ decisionDigest: "33".repeat(32) });
    expect(mocks.bindReference).toHaveBeenCalledTimes(19);
    expect(
      mocks.bindReference.mock.calls.map(([input]) => input.contractName),
    ).toEqual([
      "fraudProofScriptIntegrityHashMismatch",
      "fraudProofScriptIntegrityHashMismatchStep02",
      "fraudProofScriptIntegrityHashMismatchStep03",
      "fraudProofScriptIntegrityHashMismatchStep04",
      "fraudProofScriptIntegrityHashMismatchStep05",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "chunkedVerifyWithdraw",
      "pexcludesWithdraw",
      "correctionLockSpend",
      "stateQueueSpend",
      "stateQueueMint",
      "stateQueueFraudRemovalWithdraw",
      "activeOperatorsSpend",
      "activeOperatorsMint",
      "retiredOperatorsSpend",
      "retiredOperatorsMint",
      "schedulerSpend",
    ]);
    expect(mocks.createActuator).toHaveBeenCalledOnce();
    expect(mocks.createActuator.mock.calls[0]![0]).toMatchObject({
      references: { steps: { length: 5 }, witnesses: {} },
      contracts: { stateQueuePolicyId: "66".repeat(28) },
    });
  });

  it("rejects caller-supplied proof or callback authority before binding", async () => {
    expect(SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS).not.toEqual(
      expect.arrayContaining(["evidence", "actuator", "verdict", "submit"]),
    );
    await expect(
      createManifestBoundScriptIntegrityHashMismatchWorkflow({
        ...config(),
        evidence: {},
      } as never),
    ).rejects.toThrow(/callback authority/u);
    expect(mocks.bindDeployment).not.toHaveBeenCalled();
  });
});
