import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  bindValidationTraceDisputeWorkflowDeployment,
  createReferenceScriptPublisher,
  createValidationDisputeParties,
  publishAuthenticatedValidationDisputeControl,
  readCanonicalCheckpoint,
  realBlueprintPath,
  withRealL1MaxTxSize,
} from "@al-ft/midgard-fault-proofs/testing/installed-workflow";
import {
  canonicalValidationTraceReferenceScripts,
  referenceScriptAuthPolicyDeploymentInfo,
  type ReferenceScriptAuthTokenTarget,
  referenceScriptAuthUnit,
  sharedRedeemerItemReferenceScripts,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { manifestDeployableScripts } from "../src/deployable-scripts.js";
import { validationTraceDisputeFromManifest } from "../src/services/midgard-contracts.validation-trace-dispute-from-manifest.js";
import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

it("binds the eight canonical roles through the closed production manifest and acquires their signed publications", async () => {
  const { emulator, challengerLucid } = await createValidationDisputeParties();
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(challengerLucid, emulator.now());
  const contracts = await loadRealMidgardContractsForTest(
    nonceUtxo,
    referenceScriptAuth,
  );
  const canonical = canonicalValidationTraceReferenceScripts(
    contracts.fraudProofContracts.validationTraceDispute,
  );
  expect(canonical).toHaveLength(8);
  const targets = nodeRuntimeReferenceScriptTargets(contracts);
  expect(manifestDeployableScripts(contracts)).toHaveLength(544);
  expect(targets).toHaveLength(537);
  const invalidData = sharedRedeemerItemReferenceScripts(
    contracts.fraudProofContracts.validationTraceDispute
      .scriptSourcesStageOneRedeemerStages,
  ).find(
    ({ deploymentEntry }) =>
      deploymentEntry ===
      "validationTraceDisputeRedeemerItemInvalidDataExecutor",
  )!;
  const invalidTarget = targets.find(({ name }) => name === invalidData.role)!;
  expect(validatorToScriptHash(invalidTarget.script)).toBe(
    invalidData.validator.spendingScriptHash,
  );
  expect(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[invalidData.role],
  ).toBe(invalidData.deploymentEntry);
  expect(
    referenceScriptAuthUnit(referenceScriptAuth.policyId, invalidData.role),
  ).toBe(
    referenceScriptAuth.policyId +
      Buffer.from("V1VtRiInvalidData").toString("hex"),
  );
  const roleByContract = new Map<string, ReferenceScriptAuthTokenTarget>(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contract]) => [contract, role as ReferenceScriptAuthTokenTarget],
    ),
  );
  // Other families are synthetic coordinates; these eight are actual signed,
  // authenticated publications acquired through the ledger provider below.
  const outRefs = new Map<string, { txHash: string; outputIndex: number }>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contract, index) => [
        contract,
        { txHash: (index + 1).toString(16).padStart(64, "0"), outputIndex: 0 },
      ],
    ),
  );
  for (const { deploymentEntry, validator } of canonical) {
    const role = roleByContract.get(deploymentEntry)!;
    const target = targets.find(({ name }) => name === role);
    expect(target).toBeDefined();
    expect(validatorToScriptHash(target!.script)).toBe(
      validator.spendingScriptHash,
    );
    const publication = await withRealL1MaxTxSize(emulator, () =>
      publishAuthenticatedValidationDisputeControl({
        lucid: challengerLucid,
        target: {
          control: deploymentEntry,
          name: role,
          script: target!.script,
        },
        authPolicy: referenceScriptAuth,
        publisher: referenceScriptPublisher,
      }),
    );
    expect(
      publication.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(15_872);
    expect(
      publication.utxo.assets[
        referenceScriptAuthUnit(referenceScriptAuth.policyId, role)
      ],
    ).toBe(1n);
    outRefs.set(deploymentEntry, {
      txHash: publication.utxo.txHash,
      outputIndex: publication.utxo.outputIndex,
    });
    console.log(
      JSON.stringify(
        {
          contract: deploymentEntry,
          scriptHash: validator.spendingScriptHash,
          ...publication.publicationMeasurement,
        },
        (_, value) => (typeof value === "bigint" ? value.toString() : value),
      ),
    );
  }
  const blueprintJson = readFileSync(realBlueprintPath, "utf8");
  const manifest = await makeFinalizedDeploymentManifestFixture({
    contracts,
    authPolicy: referenceScriptAuthPolicyDeploymentInfo(referenceScriptAuth),
    outRefs,
    nonce: nonceUtxo,
    deployAddress: await challengerLucid.wallet().address(),
    blueprintHash: createHash("sha256").update(blueprintJson).digest("hex"),
  });
  verifyFinalizedDeploymentManifest(manifest);
  const input = {
    manifest,
    blueprintJson,
    deploymentInfo: {
      contracts: manifest.contracts,
      referenceScriptAuthPolicy: manifest.referenceScriptAuthPolicy,
    },
    headerHash: "99".repeat(28),
    proverCredential: "aa".repeat(28),
  };
  const binding = await bindValidationTraceDisputeWorkflowDeployment(input);
  const restored = validationTraceDisputeFromManifest(
    "Preprod",
    manifest,
    "fabricated canonical finalized manifest",
    binding.resolvedContracts.contracts.validationTraceDispute
      .cekProgramMaterial,
  );
  expect(() => restored.prepareResolvers).toThrow(/does not record/u);
  expect(restored.canonicalDecodePrepare.spendingScriptHash).toBe(
    binding.resolvedContracts.contracts.validationTraceDispute
      .prepareResolvers[0].spendingScriptHash,
  );
  for (const {
    deploymentEntry,
    validator,
  } of canonicalValidationTraceReferenceScripts(restored)) {
    const entry = binding.referenceScriptsByContract[deploymentEntry]!;
    const utxos = await challengerLucid.utxosByOutRef([
      manifest.contracts[deploymentEntry]!.refScriptUTxO!,
    ]);
    expect(utxos).toHaveLength(1);
    expect(readCanonicalCheckpoint(utxos[0]!, restored)).toBeUndefined();
    expect(entry.scriptHash).toBe(validator.spendingScriptHash);
    expect(validatorToScriptHash(utxos[0]!.scriptRef!)).toBe(
      validator.spendingScriptHash,
    );
  }
  const { manifestId: _originalId, ...identity } = manifest;
  const selected =
    identity.contracts.validationTraceDisputeCanonicalDecodePrepare!;
  const other =
    identity.contracts.validationTraceDisputeCanonicalDecodeEmptySemantic!;
  const substitutionInput = {
    ...identity,
    referenceScripts: {
      ...identity.referenceScripts,
      "V1 validation-trace canonical-decode prepare": {
        ...identity.referenceScripts[
          "V1 validation-trace canonical-decode prepare"
        ]!,
        scriptHash: other.scriptHash,
      },
    },
    contracts: {
      ...identity.contracts,
      validationTraceDisputeCanonicalDecodePrepare: {
        ...selected,
        contract: other.contract,
        scriptHash: other.scriptHash,
      },
    },
  };
  const substituted = {
    ...substitutionInput,
    manifestId: computeDeploymentManifestId(substitutionInput),
  };
  // Internal manifest/hash consistency alone cannot certify the applied role.
  verifyFinalizedDeploymentManifest(substituted);
  const refusal = await bindValidationTraceDisputeWorkflowDeployment({
    ...input,
    manifest: substituted,
    deploymentInfo: {
      contracts: substituted.contracts,
      referenceScriptAuthPolicy: substituted.referenceScriptAuthPolicy,
    },
  }).then(
    () => undefined,
    (error: unknown) =>
      error instanceof Error ? error.message : String(error),
  );
  expect(refusal).toBe(
    "validationTraceDisputeCanonicalDecodePrepare differs from the finalized manifest",
  );
  const omitted = {
    ...manifest,
    contracts: Object.fromEntries(
      Object.entries(manifest.contracts).filter(
        ([name]) => name !== "validationTraceDisputeCanonicalDecodeItemSource",
      ),
    ),
  };
  await expect(
    bindValidationTraceDisputeWorkflowDeployment({
      ...input,
      manifest: omitted,
    }),
  ).rejects.toThrow();
}, 120_000);
