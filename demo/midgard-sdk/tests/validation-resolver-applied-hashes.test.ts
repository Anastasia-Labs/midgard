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
  AddressData,
  addressDataFromBech32,
  buildFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "../src/index.js";

type BlueprintValidator = Readonly<{
  compiledCode: string;
  title: string;
}>;

const moduleDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(moduleDirectory, "../../..");
const currentTreeBlueprintPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
const protectedBlueprint = JSON.parse(
  readFileSync(resolve(repositoryRoot, "onchain/aiken/plutus.json"), "utf8"),
) as Readonly<{
  validators: readonly (BlueprintValidator & {
    readonly parameters?: readonly unknown[];
  })[];
}>;
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
  const validator = protectedBlueprint.validators.find(
    (entry) => entry.title === title,
  );
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
        blueprint: protectedBlueprint,
        network: "Preprod",
        hubOraclePolicyId: "bb".repeat(28),
        fraudProofCataloguePolicyId: "cc".repeat(28),
      }),
    );
    const deployedSemanticHashes =
      contracts.validationTraceDispute.semanticResolvers.map(
        ({ spendingScriptHash }) => spendingScriptHash,
      );

    expect(deployedSemanticHashes).toHaveLength(76);
    expect(contracts.validationTraceDispute.prepareResolvers).toHaveLength(12);
    expect(
      extractResolverGroup(
        "phase_a_script_precondition_resolvers",
        "input_sets_resolvers",
      ),
    ).toEqual(deployedSemanticHashes.slice(24, 26));
    expect(
      extractResolverGroup("script_source_resolvers", "script_address"),
    ).toEqual([
      ...deployedSemanticHashes.slice(32, 60),
      deployedSemanticHashes[75]!,
    ]);
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
        [...deployedSemanticHashes.slice(32, 60), deployedSemanticHashes[75]!],
        contracts.computationThread.policyId,
      ),
    );

    const scriptSourcesNonOutput = protectedBlueprint.validators.find(
      (entry) =>
        entry.title ===
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics
          .scriptSourcesNonOutput,
    );
    if (scriptSourcesNonOutput === undefined) {
      throw new Error("ScriptSources non-output semantic validator is missing");
    }
    // C21-STAGE4 Option B′: resolver 8 / semantic 0 remains the two-parameter
    // generic resolver. Its ABI deliberately has no proof-item reference
    // variant because stage four now authenticates tag-29 proof-only evidence.
    expect(deployedSemanticHashes[32]).toBe(
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyParamsToScript(scriptSourcesNonOutput.compiledCode, [
          contracts.validationTraceDispute.award.spendingScriptHash,
          contracts.computationThread.policyId,
        ]),
      }),
    );
  });

  it("applies immutable CEK material identity as the exact fourth direct-resolver parameter", async () => {
    if (currentTreeBlueprintPath === undefined) {
      return;
    }
    const currentTreeBlueprint = JSON.parse(
      readFileSync(currentTreeBlueprintPath, "utf8"),
    ) as Readonly<{
      validators: readonly (BlueprintValidator & {
        readonly parameters?: readonly unknown[];
      })[];
    }>;
    const cekValidator = currentTreeBlueprint.validators.find(
      (entry) =>
        entry.title ===
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers.cek,
    );
    const materialValidator = currentTreeBlueprint.validators.find(
      (entry) => entry.title === CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
    );
    if (cekValidator === undefined || materialValidator === undefined) {
      throw new Error("CEK direct or program-material validator is missing");
    }
    expect(cekValidator.parameters).toHaveLength(4);
    const contracts = await Effect.runPromise(
      buildFaultProofContracts({
        blueprint: currentTreeBlueprint,
        network: "Preprod",
        hubOraclePolicyId: "bb".repeat(28),
        fraudProofCataloguePolicyId: "cc".repeat(28),
      }),
    );
    const fraudProofAddressData = Data.from(
      Data.to(
        await Effect.runPromise(
          addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
        ),
        AddressData,
      ),
    );
    const materialHash = validatorToScriptHash({
      type: "PlutusV3",
      script: materialValidator.compiledCode,
    });
    expect(
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptHash,
    ).toBe(materialHash);
    expect(
      contracts.validationTraceDispute.directResolvers[0].spendingScriptHash,
    ).toBe(
      validatorToScriptHash({
        type: "PlutusV3",
        script: applyParamsToScript(cekValidator.compiledCode, [
          contracts.computationThread.policyId,
          contracts.fraudProof.policyId,
          fraudProofAddressData,
          materialHash,
        ]),
      }),
    );
    const legacyThreeParameterCekHash = validatorToScriptHash({
      type: "PlutusV3",
      script: applyParamsToScript(cekValidator.compiledCode, [
        contracts.computationThread.policyId,
        contracts.fraudProof.policyId,
        fraudProofAddressData,
      ]),
    });
    expect(
      contracts.validationTraceDispute.directResolvers[0].spendingScriptHash,
    ).not.toBe(legacyThreeParameterCekHash);
  });
});
