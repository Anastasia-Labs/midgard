import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { applyBlueprintParams } from "../src/fraud-proof/contracts/blueprint.js";
import {
  buildFaultProofContracts,
  parseFaultProofBlueprint,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "../src/index.js";
import { applyExpectedScriptParams } from "./fault-proof.expected-parameter-application.js";

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
  it("applies the script-decode policy to the CEK selection successor and every declared parameter of the CEK semantic resolvers", async () => {
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
    const successorValidator = currentTreeBlueprint.validators.find(
      (entry) =>
        entry.title ===
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.yields
          .cekSelectionSuccessor,
    );
    if (
      selectionValidator === undefined ||
      contextStepValidator === undefined ||
      successorValidator === undefined
    ) {
      throw new Error("CEK semantic or selection successor is missing");
    }
    // The claims this file exists to hold, stated by hand because they are
    // reviewed properties of the deployment and not something a table can
    // derive: a selected Plutus execution takes its program from a
    // script-decode record, so the record policy is the selection
    // successor's SECOND parameter, after its dispatcher. Context execution
    // delegates to its control validator, whose identity is the context-step
    // resolver's FIRST parameter.
    const selectionTitles = (selectionValidator.parameters ?? []).map(
      ({ title }) => title,
    );
    const contextStepTitles = (contextStepValidator.parameters ?? []).map(
      ({ title }) => title,
    );
    const successorTitles = (successorValidator.parameters ?? []).map(
      ({ title }) => title,
    );
    expect(successorTitles).toEqual([
      "dispatcher_script_hash",
      "script_decode_policy_id",
    ]);
    expect(contextStepTitles[0]).toBe("cek_context_control_script_hash");

    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        disputeBondLovelace: 10_000_000_000n,
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
        caseConfigInitializationNonce: {
          txHash: "ab".repeat(32),
          outputIndex: 0,
        },
      }),
    );
    const dispute = contracts.validationTraceDispute;
    const boundaryValidator = currentTreeBlueprint.validators.find(
      (entry) =>
        entry.title === VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
    );
    if (boundaryValidator === undefined)
      throw new Error("Validation boundary is missing");
    expect(
      (boundaryValidator.parameters ?? []).map(({ title }) => title),
    ).toEqual([
      "one_step_resolver_script_hashes",
      "award_validator_script_hash",
      "computation_thread_token_policy_id",
      "thread_expiry_script_hash",
    ]);
    // Full application is essential: an unapplied parameter produces a lambda
    // that succeeds without executing the terminal-padding refusal guards.
    const appliedBoundary = applyBlueprintParams(
      currentTreeBlueprint,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
      [
        dispute.resolvers.map(({ spendingScriptHash }) => spendingScriptHash),
        dispute.award.spendingScriptHash,
        dispute.case.policyId,
        dispute.threadExpiry.withdrawalScriptHash,
      ],
    );
    expect(
      validatorToScriptHash({ type: "PlutusV3", script: appliedBoundary }),
    ).toBe(dispute.boundary.spendingScriptHash);
    const appliedGame = applyBlueprintParams(
      currentTreeBlueprint,
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
      [
        dispute.boundary.spendingScriptHash,
        dispute.timeout.spendingScriptHash,
        dispute.case.policyId,
        dispute.threadExpiry.withdrawalScriptHash,
      ],
    );
    expect(
      validatorToScriptHash({ type: "PlutusV3", script: appliedGame }),
    ).toBe(dispute.game.spendingScriptHash);

    // The value each declared parameter name must be bound to. The blueprint's
    // declared order is the only authority on *positions*, so a builder that
    // passes the same values in a different order fails, and a parameter this
    // deployment has no reviewed value for fails closed rather than defaulting.
    const bindings: Readonly<Record<string, string>> = {
      award_script_hash: dispute.award.spendingScriptHash,
      computation_thread_policy_id: dispute.case.policyId,
      thread_expiry_script_hash: dispute.threadExpiry.withdrawalScriptHash,
      reference_script_auth_policy_id: referenceScriptAuthPolicyId,
      cek_context_control_script_hash:
        dispute.cekContextStages.control.spendingScriptHash,
    };
    const applied = (
      validator: { readonly compiledCode: string },
      titles: readonly string[],
    ): string =>
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyExpectedScriptParams(
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
    const appliedSuccessor = applyExpectedScriptParams(
      successorValidator.compiledCode,
      [selectionSemantic.spendingScriptHash, contracts.scriptDecode.policyId],
    );
    expect(
      validatorToScriptHash({ type: "PlutusV3", script: appliedSuccessor }),
    ).toBe(dispute.yields.cekSelectionSuccessor.withdrawalScriptHash);

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
  }, 120_000);
});
