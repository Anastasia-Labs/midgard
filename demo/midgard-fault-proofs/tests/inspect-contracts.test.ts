import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  buildFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  parseFaultProofBlueprint,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { inspectContracts } from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const h28 = "11".repeat(28);
const h28b = "22".repeat(28);
const placeholderDoubleSpend = "00".repeat(28);
const placeholderNonExistentInput = "02".repeat(28);
const placeholderInvalidRange = "01".repeat(28);
const placeholderZeroInput = "03".repeat(28);
// Re-pinned 2026-08-14 (#579): the regeneration
// (`onchain/aiken/plutus.json` md5 b20c9a14a8fe445cdddbe5305b3857c1, 398
// validators, aiken v1.1.23+2a78108) recompiled these four step validators and
// #594 gave every field-opening step a trailing
// `field_preimage_certificate_policy_id` parameter, which the SDK applies from
// the blueprint itself. Both move the applied hashes. Derived by running this
// suite's own producer with `MIDGARD_PRINT_PROOF_FIT=1` (the
// `q13AppliedIdentities.stepHashes` line), not read off an assertion diff.
// Re-pinned 2026-08-16 (#606): the E2 repair regeneration
// (`onchain/aiken/plutus.json` md5 5e38d7c6ccb7987d0aca710307dcaea7, 398
// validators, same fork) moved the certificate policy id
// (c3682abd… -> f030476f…), which is the applied trailing parameter of every
// field-opening step — so all four applied hashes move even where a step's
// compiled code did not. Same derivation route as #579's re-pin.
const Q13_APPLIED_STEP_HASHES = [
  "063831d17f9f26542608df346319a333917f14345d2c2df876f593af",
  "5438e14789271ca13e0ec1cebe00b9d7c28e64a9d2e54d6d9f54013f",
  "f88c7a7937df342a37eee3132139110d0a7689b037ab99b512937c30",
  "393aeccc152ae74b067ede8670274e3994bc9577ef0bbd8bdc60f77c",
] as const;
// Re-pinned 2026-08-05 (#544): the original-epoch root d88f9829…bcca394
// (blueprint f5ae651e…, 380 validators) moved with the #521 renames — the
// catalogue root folds every category's applied step-01 hash, and #521
// moved foundational-family step scripts while this family's four applied
// hashes (above) are measured unchanged. Current value measured by this
// suite's own derivation under blueprint 605c8b8d… (391 validators).
// Re-pinned 2026-08-06 (#547): the catalogue gained three appended categories
// (`noReferenceInput` 00000008, `referenceInputNoIdx` 00000009,
// `invalidSignature` 0000000a), which moves the folded root without shifting
// any existing category id. Measured by this suite's own derivation under
// blueprint 2b5973fe… (393 validators): d1a70a1b… -> 32e29b6d….
// Re-pinned 2026-08-14 (#579): the root folds every category's applied step-01
// hash, and the regeneration moved those hashes (see the note on
// `Q13_APPLIED_STEP_HASHES` above), so the fold moves with them without any
// category being added, removed, or renumbered. Measured by this suite's own
// derivation under blueprint b20c9a14… (398 validators, aiken v1.1.23+2a78108):
// 32e29b6d… -> 173cabdb….
// Re-pinned 2026-08-15 (#609): NO blueprint movement this time — b20c9a14… is
// byte-identical before and after. The fold moved because the validation-trace
// category's applied step-01 hash (the dispute opener) moved: ten semantic
// resolvers were being deployed one parameter short of what they declare, which
// under Plutus V3 made them always-succeeds scripts, and correcting the
// application cascaded through their five prepare resolvers into
// boundary -> game -> source -> dispute. `Q13_APPLIED_STEP_HASHES` above is
// measured UNCHANGED (the input-no-idx family was never under-applied), so this
// row is the only one that moves — checked independently rather than assumed,
// because a stale pin hiding behind another failing pin is how #579 lost one.
// Measured by this suite's own derivation, which agrees with the live catalogue
// derivation on the preceding line: 173cabdb… -> 61f11db3….
// Re-pinned 2026-08-16 (#606): the root folds every category's applied
// step-01 hash and the certificate policy id is an applied parameter of every
// field-opening step-01, so the moved policy (c3682abd… -> f030476f…) moves
// every folded leaf and the root with it: 61f11db3… -> 53f5fc3a…. Derived by
// this suite's own producer with `MIDGARD_PRINT_PROOF_FIT=1`
// (`q13AppliedIdentities.catalogueRoot`).
const Q13_CATALOGUE_ROOT =
  "53f5fc3aab9ddd758ad82a4d27321c2b3c8213312e71bbe5636dfec22de21c52";
const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

const deploymentManifest = (contracts: Record<string, unknown>) => ({
  referenceScriptAuthPolicy: {},
  contracts,
});

const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    "hex",
  );

const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

const readBlueprintJson = (): unknown =>
  JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown;

const buildCatalogueFixture = async (
  scriptHashes: Partial<Record<string, string>>,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: categoryId(index),
        scriptHash:
          scriptHashes[name] ??
          `${(index + 3).toString(16).padStart(2, "0")}`.repeat(28),
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }

  const withProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    withProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }

  return {
    root: trieRootHex(trie),
    categories: withProofs,
  };
};

const buildInspectionFixture = async () => {
  const blueprintJson = readBlueprintJson();
  const contracts = await Effect.runPromise(
    buildFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprintJson),
      network: "Preprod",
      hubOraclePolicyId: h28,
      fraudProofCataloguePolicyId: h28b,
    }),
  );
  const fraudProofCatalogue = await buildCatalogueFixture({
    doubleSpend: contracts.doubleSpend.firstStep.spendingScriptHash,
    nonExistentInput: contracts.nonExistentInput.firstStep.spendingScriptHash,
    nonExistentInputNoIndex:
      contracts.nonExistentInputNoIndex.firstStep.spendingScriptHash,
    invalidRange: contracts.invalidRange.firstStep.spendingScriptHash,
    zeroInput: contracts.zeroInput.firstStep.spendingScriptHash,
    transitionTrace: contracts.transitionTrace.firstStep.spendingScriptHash,
    validationTraceDispute:
      contracts.validationTraceDispute.firstStep.spendingScriptHash,
    daHashPreimage: contracts.daHashPreimage.firstStep.spendingScriptHash,
    noReferenceInput: contracts.noReferenceInput.firstStep.spendingScriptHash,
    referenceInputNoIdx:
      contracts.referenceInputNoIdx.firstStep.spendingScriptHash,
    invalidSignature: contracts.invalidSignature.firstStep.spendingScriptHash,
  });
  return { blueprintJson, contracts, fraudProofCatalogue };
};

let inspectionFixture: Awaited<ReturnType<typeof buildInspectionFixture>>;

beforeAll(async () => {
  inspectionFixture = await buildInspectionFixture();
}, 30_000);

const deploymentInfoFor = (
  {
    contracts,
    fraudProofCatalogue,
  }: Awaited<ReturnType<typeof buildInspectionFixture>>,
  doubleSpendScriptHash = contracts.doubleSpend.firstStep.spendingScriptHash,
  invalidRangeScriptHash = contracts.invalidRange.firstStep.spendingScriptHash,
  transitionTraceScriptHash = contracts.transitionTrace.firstStep
    .spendingScriptHash,
  nonExistentInputScriptHash = contracts.nonExistentInput.firstStep
    .spendingScriptHash,
  zeroInputScriptHash = contracts.zeroInput.firstStep.spendingScriptHash,
) => ({
  referenceScriptAuthPolicy: {},
  contracts: {
    hubOracleMint: { scriptHash: h28 },
    fraudProofCatalogueMint: {
      scriptHash: h28b,
      fraudProofCatalogue,
    },
    fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
    fraudProofSpend: {
      scriptHash: contracts.fraudProof.spendingScriptHash,
    },
    fraudProofDoubleSpend: { scriptHash: doubleSpendScriptHash },
    fraudProofNonExistentInput: {
      scriptHash: nonExistentInputScriptHash,
    },
    fraudProofNonExistentInputNoIndex: {
      scriptHash:
        contracts.nonExistentInputNoIndex.firstStep.spendingScriptHash,
      contract: {
        type: "PlutusV3" as const,
        cborHex:
          contracts.nonExistentInputNoIndex.firstStep.spendingScript.script,
      },
    },
    fraudProofInvalidRange: { scriptHash: invalidRangeScriptHash },
    fraudProofZeroInput: { scriptHash: zeroInputScriptHash },
    fraudProofTransitionTrace: { scriptHash: transitionTraceScriptHash },
    validationTraceDispute: {
      scriptHash: contracts.validationTraceDispute.firstStep.spendingScriptHash,
    },
    fraudProofDaHashPreimage: {
      scriptHash: contracts.daHashPreimage.firstStep.spendingScriptHash,
    },
    fraudProofNoReferenceInput: {
      scriptHash: contracts.noReferenceInput.firstStep.spendingScriptHash,
    },
    fraudProofReferenceInputNoIdx: {
      scriptHash: contracts.referenceInputNoIdx.firstStep.spendingScriptHash,
    },
    fraudProofInvalidSignature: {
      scriptHash: contracts.invalidSignature.firstStep.spendingScriptHash,
    },
  },
});

describe("inspect-contracts", { timeout: 30_000 }, () => {
  it("emits stable implemented-category inspection JSON with catalogue readiness", async () => {
    const fixture = inspectionFixture;
    const { blueprintJson, contracts, fraudProofCatalogue } = fixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(fixture),
      }),
    );

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          q13AppliedIdentities: {
            computationThreadPolicyId: output.computationThread.policyId,
            fraudProofPolicyId: output.fraudProof.policyId,
            fraudProofSpendingScriptHash: output.fraudProof.spendingScriptHash,
            stepHashes: output.nonExistentInputNoIndex.steps.map(
              (step) => step.scriptHash,
            ),
            catalogue: output.fraudProofCatalogue.nonExistentInputNoIndex,
            catalogueRoot: output.fraudProofCatalogue.root,
          },
        }),
      );
    }

    expect(output.network).toBe("Preprod");
    expect(output.computationThread.policyId).toBe(
      contracts.computationThread.policyId,
    );
    expect(output.fraudProof.policyId).toBe(contracts.fraudProof.policyId);
    expect(output.doubleSpend.categoryFirstStepHash).toBe(
      contracts.doubleSpend.firstStep.spendingScriptHash,
    );
    expect(output.doubleSpend.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
      "step03",
      "step04",
    ]);
    expect(output.doubleSpend.deploymentDoubleSpendScriptHash).toBe(
      contracts.doubleSpend.firstStep.spendingScriptHash,
    );
    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(true);
    expect(output.nonExistentInput.categoryFirstStepHash).toBe(
      contracts.nonExistentInput.firstStep.spendingScriptHash,
    );
    expect(output.nonExistentInput.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
      "step03",
      "step04",
    ]);
    expect(output.nonExistentInput.deploymentNonExistentInputScriptHash).toBe(
      contracts.nonExistentInput.firstStep.spendingScriptHash,
    );
    expect(
      output.nonExistentInput.deploymentNonExistentInputMatchesFirstStep,
    ).toBe(true);
    expect(
      output.nonExistentInputNoIndex.steps.map((step) => step.scriptHash),
    ).toEqual(Q13_APPLIED_STEP_HASHES);
    expect(output.invalidRange.categoryFirstStepHash).toBe(
      contracts.invalidRange.firstStep.spendingScriptHash,
    );
    expect(output.invalidRange.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
    ]);
    expect(output.invalidRange.deploymentInvalidRangeScriptHash).toBe(
      contracts.invalidRange.firstStep.spendingScriptHash,
    );
    expect(output.invalidRange.deploymentInvalidRangeMatchesFirstStep).toBe(
      true,
    );
    expect(output.zeroInput.categoryFirstStepHash).toBe(
      contracts.zeroInput.firstStep.spendingScriptHash,
    );
    expect(output.zeroInput.steps.map((step) => step.name)).toEqual([
      "step01",
      "step02",
    ]);
    expect(output.zeroInput.deploymentZeroInputScriptHash).toBe(
      contracts.zeroInput.firstStep.spendingScriptHash,
    );
    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBe(true);
    expect(output.transitionTrace.categoryFirstStepHash).toBe(
      contracts.transitionTrace.firstStep.spendingScriptHash,
    );
    expect(output.transitionTrace.steps.map((step) => step.name)).toEqual([
      "route",
      "control",
      "source",
      "withdrawal",
      "forced",
      "accepted",
      "deposit",
      "l1Event",
      "duplicate",
    ]);
    expect(output.transitionTrace.deploymentTransitionTraceScriptHash).toBe(
      contracts.transitionTrace.firstStep.spendingScriptHash,
    );
    expect(
      output.transitionTrace.deploymentTransitionTraceMatchesFirstStep,
    ).toBe(true);
    expect(
      output.validationTraceDispute.steps.map((step) => step.name),
    ).toEqual([
      "dispute",
      "source",
      "game",
      "boundary",
      "timeout",
      "award",
      ...Array.from({ length: 76 }, (_, index) => `semantic-resolver-${index}`),
      ...Array.from({ length: 12 }, (_, index) => `prepare-resolver-${index}`),
      ...Array.from({ length: 2 }, (_, index) => `direct-resolver-${index}`),
    ]);
    const appliedSpendingScripts = [
      ...output.doubleSpend.steps.map((step) => ({
        category: "doubleSpend",
        step,
      })),
      ...output.nonExistentInput.steps.map((step) => ({
        category: "nonExistentInput",
        step,
      })),
      ...output.invalidRange.steps.map((step) => ({
        category: "invalidRange",
        step,
      })),
      ...output.zeroInput.steps.map((step) => ({
        category: "zeroInput",
        step,
      })),
      ...output.daHashPreimage.steps.map((step) => ({
        category: "daHashPreimage",
        step,
      })),
      ...output.nonExistentInputNoIndex.steps.map((step) => ({
        category: "nonExistentInputNoIndex",
        step,
      })),
      ...output.noReferenceInput.steps.map((step) => ({
        category: "noReferenceInput",
        step,
      })),
      ...output.referenceInputNoIdx.steps.map((step) => ({
        category: "referenceInputNoIdx",
        step,
      })),
      ...output.invalidSignature.steps.map((step) => ({
        category: "invalidSignature",
        step,
      })),
      ...output.transitionTrace.steps.map((step) => ({
        category: "transitionTrace",
        step,
      })),
      ...output.validationTraceDispute.steps.map((step) => ({
        category: "validationTraceDispute",
        step,
      })),
    ];
    const selectedParameterizedValidators = [
      ...contracts.doubleSpend.steps,
      ...contracts.nonExistentInput.steps,
      ...contracts.invalidRange.steps,
      ...contracts.zeroInput.steps,
      ...contracts.daHashPreimage.steps,
      ...contracts.nonExistentInputNoIndex.steps,
      ...contracts.noReferenceInput.steps,
      ...contracts.referenceInputNoIdx.steps,
      ...contracts.invalidSignature.steps,
      contracts.transitionTrace.route,
      ...contracts.transitionTrace.finals,
      contracts.validationTraceDispute.opener,
      contracts.validationTraceDispute.source,
      contracts.validationTraceDispute.game,
      contracts.validationTraceDispute.boundary,
      contracts.validationTraceDispute.timeout,
      contracts.validationTraceDispute.award,
      ...contracts.validationTraceDispute.semanticResolvers,
      ...contracts.validationTraceDispute.prepareResolvers,
      ...contracts.validationTraceDispute.directResolvers,
    ];
    expect(appliedSpendingScripts).toHaveLength(
      selectedParameterizedValidators.length,
    );
    appliedSpendingScripts.forEach(({ step }, index) => {
      const selectedValidator = selectedParameterizedValidators[index];
      expect(selectedValidator).toBeDefined();
      const standaloneScriptBytes = Buffer.from(
        selectedValidator?.spendingScriptCBOR ?? "",
        "hex",
      ).byteLength;
      expect(step.standaloneScriptBytes).toBe(standaloneScriptBytes);
      expect(step.withinL1TransactionByteEnvelopeNecessaryCondition).toBe(
        standaloneScriptBytes <
          MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
      );
    });
    const expectedOversized = appliedSpendingScripts.flatMap(
      ({ category, step }) =>
        step.withinL1TransactionByteEnvelopeNecessaryCondition
          ? []
          : [
              {
                category,
                name: step.name,
                scriptHash: step.scriptHash,
                standaloneScriptBytes: step.standaloneScriptBytes,
              },
            ],
    );
    expect(output.l1SpendingScriptEnvelopeNecessaryCondition).toEqual({
      maxTransactionBytes: MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
      appliedSpendingScriptCount: appliedSpendingScripts.length,
      allAppliedSpendingScriptsWithinEnvelope: expectedOversized.length === 0,
      oversizedAppliedSpendingScripts: expectedOversized,
    });
    expect(output.fraudProofCatalogue.root).toBe(fraudProofCatalogue.root);
    expect(output.fraudProofCatalogue.root).toBe(Q13_CATALOGUE_ROOT);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.doubleSpend.categoryId).toBe("00000000");
    expect(output.fraudProofCatalogue.doubleSpend.expectedCategoryId).toBe(
      "00000000",
    );
    expect(
      output.fraudProofCatalogue.doubleSpend.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.doubleSpend.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.doubleSpend.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.categoryId).toBe(
      "00000001",
    );
    expect(output.fraudProofCatalogue.nonExistentInput.expectedCategoryId).toBe(
      "00000001",
    );
    expect(
      output.fraudProofCatalogue.nonExistentInput.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInput.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInput.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInputNoIndex.categoryId).toBe(
      "00000002",
    );
    expect(
      output.fraudProofCatalogue.nonExistentInputNoIndex.expectedCategoryId,
    ).toBe("00000002");
    expect(
      output.fraudProofCatalogue.nonExistentInputNoIndex
        .categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInputNoIndex
        .scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.nonExistentInputNoIndex
        .membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInputNoIndex.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.categoryId).toBe("00000003");
    expect(output.fraudProofCatalogue.invalidRange.expectedCategoryId).toBe(
      "00000003",
    );
    expect(
      output.fraudProofCatalogue.invalidRange.categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.invalidRange.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.invalidRange.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.zeroInput.categoryId).toBe("00000005");
    expect(output.fraudProofCatalogue.zeroInput.expectedCategoryId).toBe(
      "00000005",
    );
    expect(output.fraudProofCatalogue.zeroInput.categoryIdMatchesExpected).toBe(
      true,
    );
    expect(
      output.fraudProofCatalogue.zeroInput.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.zeroInput.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.categoryId).toBe(
      "00000004",
    );
    expect(output.fraudProofCatalogue.transitionTrace.expectedCategoryId).toBe(
      "00000004",
    );
    expect(
      output.fraudProofCatalogue.transitionTrace.categoryIdMatchesExpected,
    ).toBe(true);
    expect(output.fraudProofCatalogue.validationTraceDispute.categoryId).toBe(
      "00000006",
    );
    expect(
      output.fraudProofCatalogue.validationTraceDispute.expectedCategoryId,
    ).toBe("00000006");
    expect(
      output.fraudProofCatalogue.validationTraceDispute
        .categoryIdMatchesExpected,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.transitionTrace.scriptHashMatchesFirstStep,
    ).toBe(true);
    expect(
      output.fraudProofCatalogue.transitionTrace.membershipProofMatchesDerived,
    ).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.validationTraceDispute.ready).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(true);
  });

  it("marks catalogue init as not ready when deployment still points at the placeholder", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(fixture, placeholderDoubleSpend),
      }),
    );

    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(
      false,
    );
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(false);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("marks catalogue init as not ready when zero-input deployment is stale", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(
          fixture,
          fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
          fixture.contracts.invalidRange.firstStep.spendingScriptHash,
          fixture.contracts.transitionTrace.firstStep.spendingScriptHash,
          fixture.contracts.nonExistentInput.firstStep.spendingScriptHash,
          placeholderZeroInput,
        ),
      }),
    );

    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBe(false);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(false);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("marks catalogue init as not ready when zero-input catalogue authorization is stale", async () => {
    const fixture = inspectionFixture;
    const staleCatalogue = await buildCatalogueFixture({
      doubleSpend: fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
      nonExistentInput:
        fixture.contracts.nonExistentInput.firstStep.spendingScriptHash,
      invalidRange: fixture.contracts.invalidRange.firstStep.spendingScriptHash,
      zeroInput: placeholderZeroInput,
      transitionTrace:
        fixture.contracts.transitionTrace.firstStep.spendingScriptHash,
    });
    const staleFixture = {
      ...fixture,
      fraudProofCatalogue: staleCatalogue,
    };

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(staleFixture),
      }),
    );

    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBe(true);
    expect(
      output.fraudProofCatalogue.zeroInput.scriptHashMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(false);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("marks catalogue init as not ready when zero-input deployment is missing", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = deploymentInfoFor(fixture);
    const { fraudProofZeroInput: _omitted, ...contractsWithoutZeroInput } =
      deploymentInfo.contracts;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: {
          ...deploymentInfo,
          contracts: contractsWithoutZeroInput,
        },
      }),
    );

    expect(output.zeroInput.deploymentZeroInputScriptHash).toBeNull();
    expect(output.zeroInput.deploymentZeroInputMatchesFirstStep).toBeNull();
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(false);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("rejects deployment info with a mismatched fraud-proof policy", async () => {
    const blueprintJson = readBlueprintJson();
    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: blueprintJson,
          network: "Preprod",
          deploymentInfo: deploymentManifest({
            hubOracleMint: { scriptHash: h28 },
            fraudProofCatalogueMint: { scriptHash: h28b },
            fraudProofMint: { scriptHash: "33".repeat(28) },
            fraudProofSpend: { scriptHash: "44".repeat(28) },
            fraudProofDoubleSpend: { scriptHash: "55".repeat(28) },
            fraudProofInvalidRange: { scriptHash: "66".repeat(28) },
          }),
        }),
      ),
    ).rejects.toThrow("fraudProofMint.scriptHash mismatch");
  });

  it("rejects a contracts-only deployment-info object", async () => {
    const fixture = inspectionFixture;
    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: deploymentInfoFor(fixture).contracts,
        }),
      ),
    ).rejects.toThrow(
      "Contract deployment info is missing referenceScriptAuthPolicy.",
    );
  });

  it("marks catalogue init as not ready when invalid-range deployment is stale", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(
          fixture,
          fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
          placeholderInvalidRange,
        ),
      }),
    );

    expect(output.doubleSpend.deploymentDoubleSpendMatchesFirstStep).toBe(true);
    expect(output.invalidRange.deploymentInvalidRangeMatchesFirstStep).toBe(
      false,
    );
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(false);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("marks catalogue init as not ready when non-existent-input deployment is stale", async () => {
    const fixture = inspectionFixture;

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: deploymentInfoFor(
          fixture,
          fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
          fixture.contracts.invalidRange.firstStep.spendingScriptHash,
          fixture.contracts.transitionTrace.firstStep.spendingScriptHash,
          placeholderNonExistentInput,
        ),
      }),
    );

    expect(
      output.nonExistentInput.deploymentNonExistentInputMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.doubleSpend.ready).toBe(true);
    expect(output.fraudProofCatalogue.nonExistentInput.ready).toBe(false);
    expect(output.fraudProofCatalogue.invalidRange.ready).toBe(true);
    expect(output.fraudProofCatalogue.zeroInput.ready).toBe(true);
    expect(output.fraudProofCatalogue.transitionTrace.ready).toBe(true);
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("fails closed when no-index catalogue identity is not backed by its deployed script bytes", async () => {
    const fixture = inspectionFixture;
    const staleNoIndexHash = "77".repeat(28);
    const staleCatalogue = await buildCatalogueFixture({
      doubleSpend: fixture.contracts.doubleSpend.firstStep.spendingScriptHash,
      nonExistentInput:
        fixture.contracts.nonExistentInput.firstStep.spendingScriptHash,
      nonExistentInputNoIndex: staleNoIndexHash,
      invalidRange: fixture.contracts.invalidRange.firstStep.spendingScriptHash,
      transitionTrace:
        fixture.contracts.transitionTrace.firstStep.spendingScriptHash,
      zeroInput: fixture.contracts.zeroInput.firstStep.spendingScriptHash,
      validationTraceDispute:
        fixture.contracts.validationTraceDispute.firstStep.spendingScriptHash,
    });
    const deploymentInfo = deploymentInfoFor({
      ...fixture,
      fraudProofCatalogue: staleCatalogue,
    });

    const output = await Effect.runPromise(
      inspectContracts({
        blueprint: fixture.blueprintJson,
        network: "Preprod",
        deploymentInfo: {
          ...deploymentInfo,
          contracts: {
            ...deploymentInfo.contracts,
            fraudProofNonExistentInputNoIndex: {
              ...deploymentInfo.contracts.fraudProofNonExistentInputNoIndex,
              scriptHash: staleNoIndexHash,
            },
          },
        },
      }),
    );

    expect(
      output.fraudProofCatalogue.nonExistentInputNoIndex
        .scriptHashMatchesFirstStep,
    ).toBe(false);
    expect(output.fraudProofCatalogue.nonExistentInputNoIndex.ready).toBe(
      false,
    );
    expect(output.fraudProofCatalogue.rootMatchesDerived).toBe(true);
    expect(output.fraudProofCatalogue.initReady).toBe(false);
  });

  it("rejects deployment info with non-canonical fraud-proof category IDs", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = deploymentInfoFor(fixture);
    const invalidCatalogue: FraudProofCatalogueDeploymentInfo = {
      ...fixture.fraudProofCatalogue,
      categories: {
        ...fixture.fraudProofCatalogue.categories,
        invalidRange: {
          ...fixture.fraudProofCatalogue.categories.invalidRange,
          categoryId: "ffffffff",
        },
      },
    };

    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: {
            ...deploymentInfo,
            contracts: {
              ...deploymentInfo.contracts,
              fraudProofCatalogueMint: {
                ...deploymentInfo.contracts.fraudProofCatalogueMint,
                fraudProofCatalogue: invalidCatalogue,
              },
            },
          },
        }),
      ),
    ).rejects.toThrow(
      "fraudProofCatalogue.categories.invalidRange.categoryId must be 00000003",
    );
  });

  it("rejects deployment info with duplicated fraud-proof category IDs", async () => {
    const fixture = inspectionFixture;
    const deploymentInfo = deploymentInfoFor(fixture);
    const invalidCatalogue: FraudProofCatalogueDeploymentInfo = {
      ...fixture.fraudProofCatalogue,
      categories: {
        ...fixture.fraudProofCatalogue.categories,
        invalidRange: {
          ...fixture.fraudProofCatalogue.categories.invalidRange,
          categoryId:
            fixture.fraudProofCatalogue.categories.doubleSpend.categoryId,
        },
      },
    };

    await expect(
      Effect.runPromise(
        inspectContracts({
          blueprint: fixture.blueprintJson,
          network: "Preprod",
          deploymentInfo: {
            ...deploymentInfo,
            contracts: {
              ...deploymentInfo.contracts,
              fraudProofCatalogueMint: {
                ...deploymentInfo.contracts.fraudProofCatalogueMint,
                fraudProofCatalogue: invalidCatalogue,
              },
            },
          },
        }),
      ),
    ).rejects.toThrow(
      "fraudProofCatalogue.categories.invalidRange.categoryId duplicates",
    );
  });
});
