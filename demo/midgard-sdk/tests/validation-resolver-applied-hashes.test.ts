import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Data,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildFaultProofContracts,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "../src/index.js";

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

const appliedPrepareHash = (
  title: string,
  semanticHashes: readonly string[],
  computationThreadPolicyId: string,
): string => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) {
    throw new Error(`Blueprint prepare resolver ${title} is missing`);
  }
  const hashesSchema = Data.Array(Data.Bytes());
  type Hashes = Data.Static<typeof hashesSchema>;
  const Hashes = hashesSchema as unknown as Hashes;
  const semanticHashesData = Data.from(
    Data.to([...semanticHashes], Hashes),
  ) as Data;
  return validatorToScriptHash({
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [
      semanticHashesData,
      computationThreadPolicyId,
    ]),
  });
};

describe("validation resolver production-builder applied-hash Aiken fixture", () => {
  it("matches the production SDK builder's exact applied semantic hashes", async () => {
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: "bb".repeat(28),
        fraudProofCataloguePolicyId: "cc".repeat(28),
      }),
    );
    const deployedSemanticHashes =
      contracts.validationTraceDispute.semanticResolvers.map(
        ({ spendingScriptHash }) => spendingScriptHash,
      );

    expect(deployedSemanticHashes).toHaveLength(75);
    expect(contracts.validationTraceDispute.prepareResolvers).toHaveLength(12);
    expect(
      extractResolverGroup(
        "phase_a_script_precondition_resolvers",
        "input_sets_resolvers",
      ),
    ).toEqual(deployedSemanticHashes.slice(24, 26));
    expect(
      extractResolverGroup("script_source_resolvers", "script_address"),
    ).toEqual(deployedSemanticHashes.slice(32, 60));
    expect(
      contracts.validationTraceDispute.prepareResolvers[6].spendingScriptHash,
    ).toBe(
      appliedPrepareHash(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares
          .phaseAScriptPreconditions,
        deployedSemanticHashes.slice(24, 26),
        contracts.computationThread.policyId,
      ),
    );
    expect(
      contracts.validationTraceDispute.prepareResolvers[8].spendingScriptHash,
    ).toBe(
      appliedPrepareHash(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares.scriptSources,
        deployedSemanticHashes.slice(32, 60),
        contracts.computationThread.policyId,
      ),
    );
  });
});
