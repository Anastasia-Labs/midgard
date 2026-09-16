import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import * as deployment from "../src/workflow/deployment-manifest-binding.js";
import {
  createManifestBoundFabricatedDepositWorkflow,
  type ManifestBoundFabricatedDepositWorkflowConfig,
} from "../src/workflow/fabricated-deposit.js";
import {
  createManifestBoundFabricatedWithdrawalWorkflow,
  type ManifestBoundFabricatedWithdrawalWorkflowConfig,
} from "../src/workflow/fabricated-withdrawal.js";
import * as observations from "../src/workflow/family-l1-observation.js";

afterEach(() => vi.restoreAllMocks());

const families = ["fabricatedDeposit", "fabricatedWithdrawal"] as const;

const fixture = (category: (typeof families)[number]) => {
  const owner = "11".repeat(28);
  const address = credentialToAddress("Preprod", { type: "Key", hash: owner });
  const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
  const firstStep =
    category === "fabricatedDeposit"
      ? "fraudProofFabricatedDeposit"
      : "fraudProofFabricatedWithdrawal";
  const names = [
    firstStep,
    `${firstStep}Step02`,
    `${firstStep}Step03`,
    `${firstStep}Step04`,
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ];
  const references: UTxO[] = names.map((_name, index) => ({
    txHash: "22".repeat(32),
    outputIndex: index,
    address,
    assets: { lovelace: 2_000_000n },
    scriptRef: script,
  }));
  const binding = {
    network: "Preprod",
    resolvedContracts: {
      contracts: {
        [category]: { steps: references.slice(0, 4) },
        fraudProof: {},
      },
      stateQueuePolicyId: "33".repeat(28),
      category: { categoryId: "0000000b" },
    },
    referenceScriptsByContract: Object.fromEntries(
      names.map((name, index) => [
        name,
        {
          outRef: `${references[index]!.txHash}#${index}`,
          scriptHash: validatorToScriptHash(script),
        },
      ]),
    ),
  };
  // Isolate verified-manifest loading and provider startup. The production
  // factory and exact manifest out-ref/script checks between them stay real.
  vi.spyOn(deployment, "bindFraudProofWorkflowDeployment").mockResolvedValue(
    binding as unknown as Awaited<
      ReturnType<typeof deployment.bindFraudProofWorkflowDeployment>
    >,
  );
  const bindReference = vi.spyOn(
    deployment,
    "requireManifestBoundReferenceScriptUtxo",
  );
  const reachedProviderBoundary = new Error(
    "reference binding completed before provider startup",
  );
  const observe = vi
    .spyOn(observations, "createFraudProofFamilyLocalKupmiosL1ObservationPort")
    .mockImplementation(() => {
      throw reachedProviderBoundary;
    });
  const config = {
    signer: { address, paymentKeyHash: owner },
    referenceScripts: {
      steps: references.slice(0, 4),
      witnesses: {
        computationThreadMint: references[4],
        fraudProofMint: references[5],
        phasMembershipWithdraw: references[6],
      },
    },
  };
  const construct = () =>
    category === "fabricatedDeposit"
      ? createManifestBoundFabricatedDepositWorkflow(
          config as unknown as ManifestBoundFabricatedDepositWorkflowConfig,
        )
      : createManifestBoundFabricatedWithdrawalWorkflow(
          config as unknown as ManifestBoundFabricatedWithdrawalWorkflowConfig,
        );
  return {
    binding,
    config,
    references,
    construct,
    bindReference,
    observe,
    reachedProviderBoundary,
  };
};

it.each(families)(
  "%s binds the exact published PHAS reference before provider startup",
  async (category) => {
    const f = fixture(category);
    await expect(f.construct()).rejects.toBe(f.reachedProviderBoundary);
    expect(f.bindReference).toHaveBeenCalledWith({
      binding: f.binding,
      contractName: "phasMembershipWithdraw",
      utxo: f.references[6],
    });
  },
);

it.each(families)(
  "%s refuses a missing PHAS reference before provider startup",
  async (category) => {
    const f = fixture(category);
    Reflect.deleteProperty(
      f.config.referenceScripts.witnesses,
      "phasMembershipWithdraw",
    );
    await expect(f.construct()).rejects.toThrow();
    expect(f.observe).not.toHaveBeenCalled();
  },
);

it.each(families)(
  "%s refuses a PHAS reference from a different output",
  async (category) => {
    const f = fixture(category);
    f.config.referenceScripts.witnesses.phasMembershipWithdraw = {
      ...f.references[6]!,
      outputIndex: 99,
    };
    await expect(f.construct()).rejects.toThrow(
      "phasMembershipWithdraw reference UTxO differs from finalized manifest identity",
    );
    expect(f.observe).not.toHaveBeenCalled();
  },
);

it.each(families)(
  "%s refuses a different script at the published PHAS output",
  async (category) => {
    const f = fixture(category);
    f.config.referenceScripts.witnesses.phasMembershipWithdraw = {
      ...f.references[6]!,
      scriptRef: { type: "Native", script: `8200581c${"44".repeat(28)}` },
    };
    await expect(f.construct()).rejects.toThrow(
      "phasMembershipWithdraw reference UTxO script differs from finalized manifest identity",
    );
    expect(f.observe).not.toHaveBeenCalled();
  },
);
