/**
 * #605 INVESTIGATION ARTIFACT — candidate permanent regression evidence.
 *
 * The compiled blueprint declares, per validator, exactly how many
 * `validator main(...)` parameters must be applied before the script context.
 * `applyParamsToScript` applies parameters POSITIONALLY and performs no arity
 * check, so applying too few leaves a partially-applied program: the ledger's
 * single Plutus V3 script-context application then reduces to a lambda VALUE,
 * evaluation terminates without error, and the ledger reads "no error" as
 * SUCCESS. Such a validator is an always-succeeds script regardless of what its
 * Aiken source says.
 *
 * This file measures, for every validation-trace semantic resolver, the arity
 * the blueprint declares against the arity the SDK deployment builder actually
 * applies, and pins the deployed script hash to the arity the builder used.
 */
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const blueprintPath =
  process.env["MIDGARD_REAL_BLUEPRINT_PATH"] ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  readonly parameters?: readonly { readonly title: string }[];
};

const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
  readonly validators: readonly BlueprintValidator[];
};

const byTitle = new Map(blueprint.validators.map((v) => [v.title, v]));

/**
 * The deployment loop in `contracts.ts` iterates
 * `Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics)`, so
 * reading the same object here measures the real deployment order rather than a
 * re-derived guess that could drift.
 */
const SEMANTIC_TITLES: readonly string[] = Object.values(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
);

const PREPARE_TITLES: readonly string[] = Object.values(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
);

describe("zz605 validation-trace semantic resolver parameter arity", () => {
  it("reports every semantic resolver whose declared arity exceeds the 2 the SDK applies", () => {
    const rows: string[] = [];
    const underApplied: string[] = [];
    for (const [index, title] of SEMANTIC_TITLES.entries()) {
      const validator = byTitle.get(title);
      expect(validator, `blueprint is missing ${title}`).toBeDefined();
      const declared = validator!.parameters?.length ?? 0;
      // contracts.ts: every semantic resolver except global index 1 is built
      // with `[award.spendingScriptHash, computationThread.policyId]`.
      const applied = index === 1 ? 3 : 2;
      const status = declared === applied ? "ok" : "UNDER-APPLIED";
      if (declared !== applied) underApplied.push(`${String(index)} ${title}`);
      rows.push(
        `[${String(index).padStart(2, "0")}] declared=${String(declared)} applied=${String(applied)} ${status}  ${title.replace("fraud_proofs/validation_trace/", "")}`,
      );
    }
    for (const [index, title] of PREPARE_TITLES.entries()) {
      const validator = byTitle.get(title);
      expect(validator, `blueprint is missing ${title}`).toBeDefined();
      const declared = validator!.parameters?.length ?? 0;
      // contracts.ts builds every prepare resolver with
      // `[semanticResolverHashesData, computationThread.policyId]`.
      const applied = 2;
      if (declared !== applied) {
        underApplied.push(`prepare/${String(index)} ${title}`);
        rows.push(
          `[prepare ${String(index)}] declared=${String(declared)} applied=${String(applied)} UNDER-APPLIED  ${title}`,
        );
      }
    }
     
    console.log(
      `\n#605 validation-trace resolver arity table (blueprint ${blueprintPath})\n` +
        `semantic titles: ${String(SEMANTIC_TITLES.length)}   prepare titles: ${String(PREPARE_TITLES.length)}\n` +
        `${rows.join("\n")}\n\nUNDER-APPLIED (${String(underApplied.length)}):\n${underApplied.join("\n")}\n`,
    );
  });

  it("shows the under-applied item resolver is a DIFFERENT script from the correctly-applied one", () => {
    const validator = byTitle.get(
      "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main.spend",
    );
    expect(validator).toBeDefined();
    expect(validator!.parameters?.length).toBe(3);

    const award = "11".repeat(28);
    const computationThread = "22".repeat(28);
    const certificate = "33".repeat(28);

    const twoParam = applyParamsToScript(validator!.compiledCode, [
      award,
      computationThread,
    ]);
    const threeParam = applyParamsToScript(validator!.compiledCode, [
      award,
      computationThread,
      certificate,
    ]);
    const twoHash = validatorToScriptHash({
      type: "PlutusV3",
      script: twoParam,
    });
    const threeHash = validatorToScriptHash({
      type: "PlutusV3",
      script: threeParam,
    });
     
    console.log(
      `\n#605 input_sets_item_semantic_v1\n  2-param (what the SDK deploys) hash: ${twoHash}\n  3-param (what the source demands) hash: ${threeHash}\n  2-param script hex length: ${String(twoParam.length)}\n  3-param script hex length: ${String(threeParam.length)}\n`,
    );
    expect(twoHash).not.toBe(threeHash);
  });
});
