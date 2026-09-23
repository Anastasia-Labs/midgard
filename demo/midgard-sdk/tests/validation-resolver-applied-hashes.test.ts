import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE,
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

const referenceScriptAuthPolicyId = "dd".repeat(28);

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
    // The one claim this file exists to hold, stated by hand because it is a
    // reviewed property of the deployment and not something a table can
    // derive: complete CEK program material is admitted only at the
    // execution-selection boundary, so the material identity is that
    // resolver's THIRD parameter (#592/#605). Context execution instead
    // delegates to its control validator, whose identity is the context-step
    // resolver's FIRST parameter.
    const selectionTitles = (selectionValidator.parameters ?? []).map(
      ({ title }) => title,
    );
    const contextStepTitles = (contextStepValidator.parameters ?? []).map(
      ({ title }) => title,
    );
    expect(selectionTitles[2]).toBe("cek_program_material_script_hash");
    expect(contextStepTitles[0]).toBe("cek_context_control_script_hash");

    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        eventHistoryBounds: {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 5000n,
          maxPayloadNodes: 512n,
        },
        blueprint: currentTreeBlueprint,
        network: "Preprod",
        hubOraclePolicyId: "bb".repeat(28),
        fraudProofCataloguePolicyId: "cc".repeat(28),
        referenceScriptAuthPolicyId: referenceScriptAuthPolicyId,
      }),
    );
    const dispute = contracts.validationTraceDispute;
    const materialHash = validatorToScriptHash({
      type: "PlutusV3",
      script: materialValidator.compiledCode,
    });
    expect(dispute.cekProgramMaterial.spendingScriptHash).toBe(materialHash);

    // The value each declared parameter name must be bound to. The blueprint's
    // declared order is the only authority on *positions*, so a builder that
    // passes the same values in a different order fails, and a parameter this
    // deployment has no reviewed value for fails closed rather than defaulting.
    const bindings: Readonly<Record<string, string>> = {
      award_script_hash: dispute.award.spendingScriptHash,
      computation_thread_policy_id: contracts.computationThread.policyId,
      cek_program_material_script_hash: materialHash,
      reference_script_auth_policy_id: referenceScriptAuthPolicyId,
      cek_material_traversal_script_hash:
        dispute.cekMaterialTraversal.spendingScriptHash,
      cek_context_control_script_hash:
        dispute.cekContextStages.control.spendingScriptHash,
    };
    const applied = (
      validator: { readonly compiledCode: string },
      titles: readonly string[],
    ): string =>
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyParamsToScript(
          validator.compiledCode,
          titles.map((title) => {
            const value = bindings[title];
            if (value === undefined) {
              throw new Error(`Unreviewed resolver parameter ${title}`);
            }
            return value;
          }),
        ),
      });

    // Resolver positions are derived from the production title table rather
    // than hard-coded, so appending a semantic resolver cannot silently move
    // this check onto a different validator.
    const semanticNames = Object.keys(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
    );
    const selectionSemantic =
      dispute.semanticResolvers[semanticNames.indexOf("cekExecutionSelection")];
    const contextStepSemantic =
      dispute.semanticResolvers[semanticNames.indexOf("cekContextStep")];
    if (selectionSemantic === undefined || contextStepSemantic === undefined) {
      throw new Error("CEK semantic resolvers are not deployed");
    }
    expect(selectionSemantic.spendingScriptHash).toBe(
      applied(selectionValidator, selectionTitles),
    );
    expect(contextStepSemantic.spendingScriptHash).toBe(
      applied(contextStepValidator, contextStepTitles),
    );

    // #605: under Plutus V3 an under-applied script is an always-succeeds
    // script -- it would let any prover collect the award. Every strict prefix
    // of the declared parameter list must therefore differ from what is
    // deployed, which is exactly the fault the stale four-vs-five parameter
    // claim let through.
    for (const [validator, titles, deployed] of [
      [selectionValidator, selectionTitles, selectionSemantic],
      [contextStepValidator, contextStepTitles, contextStepSemantic],
    ] as const) {
      expect(titles.length).toBeGreaterThan(1);
      for (let count = 0; count < titles.length; count += 1) {
        expect(
          deployed.spendingScriptHash,
          `under-applied with ${count.toString()} of ${titles.length.toString()} parameters`,
        ).not.toBe(applied(validator, titles.slice(0, count)));
      }
    }
  });
});
