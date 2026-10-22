import "./utils.js";

import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  cekContextItemReferenceScripts,
  restoreValidationTraceDisputeScripts,
  verifyDeploymentScriptBindings,
} from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import { faultProofMemberFromManifest } from "../src/services/midgard-contracts.linear-fault-proof-chain-from-manifest.js";
import {
  isRecordedValidationTracePrepare,
  isRecordedValidationTraceSemantic,
  VALIDATION_TRACE_PREPARE_KEYS,
  VALIDATION_TRACE_SEMANTIC_KEYS,
} from "./helpers/deployment-member-baseline.js";
import { makeGenuineDeploymentRegistryForTest } from "./helpers/deployment-parameter-registry.js";

describe("deployment member blueprint bindings", () => {
  let genuine: Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
  beforeAll(async () => {
    genuine = await makeGenuineDeploymentRegistryForTest();
  }, 120_000);

  it("admits every member constructed from the real blueprint", () => {
    expect(() =>
      verifyDeploymentScriptBindings(
        genuine.blueprint,
        genuine.registry.contracts,
      ),
    ).not.toThrow();
  });

  it("restores a complete usable validation-trace SDK surface with explicit unavailable members", () => {
    const restored = restoreValidationTraceDisputeScripts({
      resolve: (contract) =>
        faultProofMemberFromManifest(
          "Preprod",
          genuine.manifest,
          "sdk-restoration-fixture",
          contract,
        ),
      missingMember: (member) => {
        throw new Error(`Unrecorded SDK member: ${member}`);
      },
    });
    const original =
      genuine.contracts.fraudProofContracts.validationTraceDispute;
    const itemReferences = (
      item: Parameters<typeof cekContextItemReferenceScripts>[0],
    ) =>
      cekContextItemReferenceScripts(item).map(
        ({ deploymentEntry, validator }) => [
          deploymentEntry,
          validator.spendingScriptHash,
        ],
      );
    expect(itemReferences(restored.cekContextItemStages)).toEqual(
      itemReferences(original.cekContextItemStages),
    );
    expect(() => restored.steps).toThrow("Unrecorded SDK member:");
    for (const [list, label] of [
      ["prepareResolvers", "prepare resolver"],
      ["resolvers", "resolver"],
    ] as const)
      VALIDATION_TRACE_PREPARE_KEYS.forEach((key, index) => {
        if (isRecordedValidationTracePrepare(key))
          expect(restored[list][index]?.spendingScriptHash).toBe(
            original[list][index]?.spendingScriptHash,
          );
        else
          expect(() => restored[list][index]).toThrow(
            `Unrecorded SDK member: ${label} ${key}`,
          );
      });
    VALIDATION_TRACE_SEMANTIC_KEYS.forEach((key, index) => {
      if (isRecordedValidationTraceSemantic(key))
        expect(restored.semanticResolvers[index]?.spendingScriptHash).toBe(
          original.semanticResolvers[index]?.spendingScriptHash,
        );
      else
        expect(() => restored.semanticResolvers[index]).toThrow(
          `Unrecorded SDK member: semantic resolver ${key}`,
        );
    });
  });

  it("rejects the min-Ada yield omitted from the complete deployment", () => {
    const withoutYield = genuine.registry.contracts.filter(
      (entry) => entry.contract !== "fraudProofMinAdaStep02TxWithdraw",
    );
    expect(() =>
      verifyDeploymentScriptBindings(genuine.blueprint, withoutYield),
    ).toThrow("Missing deployment bindings: fraudProofMinAdaStep02TxWithdraw");
  });

  it("rejects an omitted raw blueprint yield independently of the member views", () => {
    const incompleteBlueprint = {
      ...genuine.blueprint,
      validators: genuine.blueprint.validators.filter(
        (validator) =>
          validator.title !==
          DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw.validatorTitle,
      ),
    };
    expect(() =>
      verifyDeploymentScriptBindings(
        incompleteBlueprint,
        genuine.registry.contracts,
      ),
    ).toThrow(
      "Deployment validator absent from blueprint: fraud_proofs/min_ada/step_02_yields.tx.withdraw",
    );
  });

  it("rejects swapped valid min-Ada steps using their real applied bytes", () => {
    const second = genuine.registry.contracts.find(
      (entry) => entry.contract === "fraudProofMinAdaStep02",
    )!;
    const third = genuine.registry.contracts.find(
      (entry) => entry.contract === "fraudProofMinAdaStep03",
    )!;
    expect(second.scriptHash).not.toBe(third.scriptHash);
    const swapped = genuine.registry.contracts.map((entry) =>
      entry === second
        ? { ...entry, script: third.script, scriptHash: third.scriptHash }
        : entry === third
          ? { ...entry, script: second.script, scriptHash: second.scriptHash }
          : entry,
    );
    expect(() =>
      verifyDeploymentScriptBindings(genuine.blueprint, swapped),
    ).toThrow(
      "Deployment script differs from its declared blueprint application: fraudProofMinAdaStep02",
    );
  });
});
