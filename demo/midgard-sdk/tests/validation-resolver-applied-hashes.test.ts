import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "../src/index.js";

type BlueprintValidator = Readonly<{
  compiledCode: string;
  title: string;
}>;

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");
const blueprint = JSON.parse(
  readFileSync(resolve(repositoryRoot, "onchain/aiken/plutus.json"), "utf8"),
) as Readonly<{ validators: readonly BlueprintValidator[] }>;
const resolverTestSource = readFileSync(
  resolve(
    repositoryRoot,
    "onchain/aiken/lib/midgard/validation-resolver-v1.test.ak",
  ),
  "utf8",
);

const extractResolverGroup = (
  functionName: string,
  nextFunctionName: string,
): readonly string[] => {
  const start = resolverTestSource.indexOf(`fn ${functionName}(`);
  const end = resolverTestSource.indexOf(`fn ${nextFunctionName}(`, start);
  if (start < 0 || end < 0) {
    throw new Error(`Aiken resolver fixture ${functionName} is missing`);
  }
  return [
    ...resolverTestSource.slice(start, end).matchAll(/#"([0-9a-f]{56})"/gu),
  ].map((match) => match[1]!);
};

const semanticTitles = Object.values(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
);
const awardScriptHash = "66".repeat(28);
const computationThreadPolicyId = "22".repeat(28);
const appliedSemanticHashes = semanticTitles.map((title) => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) {
    throw new Error(`Blueprint semantic resolver ${title} is missing`);
  }
  const appliedScript = applyParamsToScript(validator.compiledCode, [
    awardScriptHash,
    computationThreadPolicyId,
  ]);
  return validatorToScriptHash({
    type: "PlutusV3",
    script: appliedScript,
  });
});

describe("validation resolver applied-hash Aiken fixture", () => {
  it("matches the SDK deployment order and exact parameterized blueprint hashes", () => {
    expect(semanticTitles).toHaveLength(75);
    expect(
      extractResolverGroup(
        "phase_a_script_precondition_resolvers",
        "input_sets_resolvers",
      ),
    ).toEqual(appliedSemanticHashes.slice(24, 26));
    expect(
      extractResolverGroup("script_source_resolvers", "script_address"),
    ).toEqual(appliedSemanticHashes.slice(32, 60));
  });
});
