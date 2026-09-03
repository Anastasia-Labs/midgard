import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE,
  FAULT_PROOF_SHARED_TITLES,
  parseFaultProofBlueprint,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "../src/index.js";

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");
/**
 * #609: this leg used to be dormant unless `MIDGARD_REAL_BLUEPRINT_PATH` was
 * set, so its assertions ran nowhere by default and went stale unnoticed (it
 * still claimed the CEK direct resolver declared four parameters long after the
 * blueprint declared five). It now defaults to the in-tree blueprint, and the
 * env var only redirects it.
 */
const currentTreeBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repositoryRoot, "onchain/aiken/plutus.json");

const fieldPreimageCertificatePolicyId = (
  blueprint: ReturnType<typeof parseFaultProofBlueprint>,
): string =>
  mintingPolicyToId({
    type: "PlutusV3",
    script: blueprint.validators.find(
      (entry) =>
        entry.title === FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    )!.compiledCode,
  });
describe("validation resolver production-builder parameter application", () => {
  it("applies immutable CEK material identity as the exact third parameter of the CEK execution-selection semantic resolver", async () => {
    const currentTreeBlueprint = parseFaultProofBlueprint(
      JSON.parse(readFileSync(currentTreeBlueprintPath, "utf8")) as unknown,
    );
    const selectionValidator = currentTreeBlueprint.validators.find(
      (entry) =>
        entry.title ===
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics
          .cekExecutionSelection,
    );
    const contextStepValidator = currentTreeBlueprint.validators.find(
      (entry) =>
        entry.title ===
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.cekContextStep,
    );
    const materialValidator = currentTreeBlueprint.validators.find(
      (entry) => entry.title === CEK_PROGRAM_MATERIAL_SPEND_TITLE,
    );
    if (
      selectionValidator === undefined ||
      contextStepValidator === undefined ||
      materialValidator === undefined
    ) {
      throw new Error("CEK semantic or program-material validator is missing");
    }
    // The CEK direct resolver (five parameters since #592) was split into a
    // prepare validator plus four semantic resolvers. Complete program
    // material is admitted only at the execution-selection boundary, so the
    // material identity is that resolver's third parameter; the field-access
    // door's certificate policy belongs to the context-step resolver alone.
    expect(selectionValidator.parameters).toHaveLength(3);
    expect(contextStepValidator.parameters).toHaveLength(3);
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint: currentTreeBlueprint,
        network: "Preprod",
        hubOraclePolicyId: "bb".repeat(28),
        fraudProofCataloguePolicyId: "cc".repeat(28),
        referenceScriptAuthPolicyId: "dd".repeat(28),
      }),
    );
    const materialHash = validatorToScriptHash({
      type: "PlutusV3",
      script: materialValidator.compiledCode,
    });
    expect(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptHash,
    ).toBe(materialHash);
    const awardHash = contracts.validationTraceDispute.award.spendingScriptHash;
    const selectionSemantic =
      contracts.validationTraceDispute.semanticResolvers[68];
    const contextStepSemantic =
      contracts.validationTraceDispute.semanticResolvers[69];
    expect(selectionSemantic.spendingScriptHash).toBe(
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyParamsToScript(selectionValidator.compiledCode, [
          awardHash,
          contracts.computationThread.policyId,
          materialHash,
        ]),
      }),
    );
    expect(contextStepSemantic.spendingScriptHash).toBe(
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyParamsToScript(contextStepValidator.compiledCode, [
          awardHash,
          contracts.computationThread.policyId,
          fieldPreimageCertificatePolicyId(currentTreeBlueprint),
        ]),
      }),
    );
    // Every shorter application is an always-succeeds script under Plutus V3
    // (#605): the deployment must be neither.
    for (const [validator, deployed] of [
      [selectionValidator, selectionSemantic],
      [contextStepValidator, contextStepSemantic],
    ] as const) {
      for (const shortParams of [
        [awardHash],
        [awardHash, contracts.computationThread.policyId],
      ]) {
        expect(deployed.spendingScriptHash).not.toBe(
          validatorToScriptHash({
            type: "PlutusV3",
            script: applyParamsToScript(validator.compiledCode, shortParams),
          }),
        );
      }
    }
  });
});
