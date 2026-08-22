import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Data,
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
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
const protectedBlueprint = parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(resolve(repositoryRoot, "onchain/aiken/plutus.json"), "utf8"),
  ) as unknown,
);

const fieldPreimageCertificatePolicyId = (
  blueprint: typeof protectedBlueprint,
): string =>
  mintingPolicyToId({
    type: "PlutusV3",
    script: blueprint.validators.find(
      (entry) =>
        entry.title === FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    )!.compiledCode,
  });
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

    expect(deployedSemanticHashes).toHaveLength(91);
    expect(contracts.validationTraceDispute.prepareResolvers).toHaveLength(14);
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
      deployedSemanticHashes[90]!,
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
        [...deployedSemanticHashes.slice(32, 60), deployedSemanticHashes[90]!],
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
    // C21-STAGE4 Option B′: resolver 8 / semantic 0 has no proof-item reference
    // variant because stage four authenticates tag-29 proof-only evidence.
    // #592 gave it a third parameter, `field_preimage_certificate_policy_id`,
    // for the field-access door — so the two-parameter application this row
    // used to assert was an UNDER-APPLIED, always-succeeds script (#605). The
    // pair below is deliberate: the deployment must equal the full application
    // AND must not be the two-parameter one, so re-pinning a broken deployment
    // against itself cannot green this row again.
    expect(scriptSourcesNonOutput.parameters).toHaveLength(3);
    const scriptSourcesNonOutputFullyApplied = validatorToScriptHash({
      type: "PlutusV3",
      script: applyParamsToScript(scriptSourcesNonOutput.compiledCode, [
        contracts.validationTraceDispute.award.spendingScriptHash,
        contracts.computationThread.policyId,
        fieldPreimageCertificatePolicyId(protectedBlueprint),
      ]),
    });
    const scriptSourcesNonOutputUnderApplied = validatorToScriptHash({
      type: "PlutusV3",
      script: applyParamsToScript(scriptSourcesNonOutput.compiledCode, [
        contracts.validationTraceDispute.award.spendingScriptHash,
        contracts.computationThread.policyId,
      ]),
    });
    expect(deployedSemanticHashes[32]).toBe(scriptSourcesNonOutputFullyApplied);
    expect(deployedSemanticHashes[32]).not.toBe(
      scriptSourcesNonOutputUnderApplied,
    );
  });

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
      (entry) => entry.title === CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
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
