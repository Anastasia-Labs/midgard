import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  buildFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  fraudProofContractsToFirstSteps,
  parseFaultProofBlueprint,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data, mintingPolicyToId } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  inspectContracts,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const h28 = "11".repeat(28);
const h28b = "22".repeat(28);
const referenceScriptAuthNativeScriptCbor = `8200581c${"00".repeat(28)}`;
const referenceScriptAuthPolicyId = mintingPolicyToId({
  type: "Native",
  script: referenceScriptAuthNativeScriptCbor,
});
const referenceScriptAuthPolicy = {
  policyId: referenceScriptAuthPolicyId,
  nativeScript: {
    type: "Native",
    cborHex: referenceScriptAuthNativeScriptCbor,
  },
  tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
};
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
// Re-pinned 2026-08-31: the flat-reversion wave's blueprint (rebuilt with
// `aiken build --env testnet`, aiken v1.1.23+5adf783) recompiled the chain this
// family applies over. Derived by running this suite's own producer with
// `MIDGARD_PRINT_PROOF_FIT=1` (the `q13AppliedIdentities.stepHashes` line),
// not read off an assertion diff. `Q13_CATALOGUE_ROOT` below moves with these
// four, and was re-derived independently from the same producer line rather
// than assumed — a stale pin hiding behind another failing pin is how #579
// lost one.
// Re-pinned 2026-08-31 (claim-registry removal). Attribution here is a
// measured blueprint diff, not a story about what probably recompiled. The
// baseline is HEAD 38ef102b built with the same `aiken build --env testnet`
// (md5 d3b62a45621157177bb0ef6039d15071, 555 validators); the current tree is
// md5 c2339794ca6f4ac637fc295a98a104ab, 553 validators, aiken v1.1.23+5adf783.
// Comparing compiled code title-by-title across the two: exactly two
// validators are gone (`claim_registry.spend.spend` and its `.else`) and
// exactly SIX compiled bodies changed — `hub_oracle.mint`,
// `computation_thread.mint` and `state_queue.mint`, each with its `.else`.
// Every other validator in the blueprint is byte-identical. So no step in this
// family was recompiled, and every applied hash that moved did so through an
// applied PARAMETER.
// Which parameter: all four of these steps apply
// `computation_thread_token_policy_id`, and steps 01 and 03 also apply
// `hub_oracle` (read off the blueprint's own declared parameter titles). Both
// of those policy ids moved — `computation_thread.mint` because it lost its
// `claim_registry_script_hash` parameter (3 -> 2) and recompiled, `hub_oracle`
// because its mint set went from three assets to two — so both reach this
// family and the move is overdetermined rather than isolable to one.
// Derived by running this suite's own producer with
// `MIDGARD_PRINT_PROOF_FIT=1` (the `q13AppliedIdentities.stepHashes` line),
// not read off an assertion diff.
// `Q13_CATALOGUE_ROOT` below moves with these four and was re-derived from the
// same producer line rather than assumed — it was still passing only because
// the assertion above it failed first, which is exactly the stale-pin trap
// the #579 note describes.
const Q13_APPLIED_STEP_HASHES = [
  "eef050f25ab9183890e1283c2942d6d4e546928b9e015da0e219b170",
  "f4715dfd8f163df3b73b88ffc04336c24637b546108bbdecca8bfd6a",
  "afdd997b50324a138014e3e9fc180790e29d1da026c5028b3ad54388",
  "e05cfc9368b5851b68ddb37cab8ca8b5c45da673c7c1acdabc17b85c",
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
// Re-pinned 2026-08-23 (#617 IG1, the wave's one sanctioned blueprint
// regeneration): 53f5fc3a… -> f117b833…. The regeneration moved 27 validators
// and none of the eleven catalogue categories was added, removed or
// renumbered, so this is a fold move, not a shape move. Six categories' own
// step-01 validators moved outright with the `mpf_chunked_verify_validator_hash`
// env re-pin that #625's empty-trie sentinel forced (double_spend/step_01,
// no_input/step_01, invalid_range/step_01, zero_input/step_01,
// canonical_decodability/step_01, committed_field_shape/step_01 are all
// native-binding-fixture readers), and the remaining openers chain over applied
// parameters that #625/#627's ValueAndMint semantic resolvers, #629's cek
// discriminators, #626's three terminal reorders and #628's transition-trace
// descriptor repoint moved. `Q13_APPLIED_STEP_HASHES` above is measured
// UNCHANGED — the input-no-idx family reads none of them — and that was checked
// independently rather than assumed, because a stale pin hiding behind another
// failing pin is how #579 lost one. Derived by this suite's own producer with
// `MIDGARD_PRINT_PROOF_FIT=1` (`q13AppliedIdentities.catalogueRoot`), and it
// agrees with the live catalogue derivation asserted on the preceding line.
// Re-pinned 2026-08-26 after production registration expanded the catalogue
// from 11 to 25 categories. The catalogue policy id is an applied parameter of
// every chain, so the input-no-index hashes and the folded root move together.
// Re-pinned again after the canonical double-withdraw terminal ABI was
// normalized, and once more when registration appended the value-not-preserved,
// input-set-uniqueness, and mint-authorization categories (25 to 28). The value
// agrees independently between the SDK catalogue builder, the deployment
// fixture, and inspect-contracts' derived fold. Re-pinned after merging the
// six-stage lifecycle with mandatory published reference-script witnesses.
// Re-pinned 2026-08-31 alongside `Q13_APPLIED_STEP_HASHES`: the root folds
// every category's applied step-01 hash, so the wave's testnet-env blueprint
// rebuild moves the fold. No category was added, removed or renumbered.
// Derived by this suite's own producer with `MIDGARD_PRINT_PROOF_FIT=1`
// (`q13AppliedIdentities.catalogueRoot`), and it agrees with the live
// catalogue derivation asserted on the preceding line.
// Re-pinned 2026-08-31 alongside `Q13_APPLIED_STEP_HASHES` again, for the
// claim-registry removal's hub-oracle recompile cascade: the root folds every
// category's applied step-01 hash, so it moves with them. No category was
// added, removed or renumbered — the registry was never a catalogue category,
// and `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` is measured unchanged at 32.
// Derived by this suite's own producer with `MIDGARD_PRINT_PROOF_FIT=1`
// (`q13AppliedIdentities.catalogueRoot`), and it agrees with the live
// catalogue derivation asserted on the preceding line.
// `914d498f…` -> `85ecf82f…`.
const Q13_CATALOGUE_ROOT =
  "e2919c1776d2c2c358f9abbff9b13dcdd8a3f2717ec49ffecd634fcc19a91d11";
const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

const deploymentManifest = (contracts: Record<string, unknown>) => ({
  referenceScriptAuthPolicy,
  contracts,
});

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
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
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
      referenceScriptAuthPolicyId,
    }),
  );
  const firstSteps = fraudProofContractsToFirstSteps(contracts);
  const fraudProofCatalogue = await buildCatalogueFixture(
    Object.fromEntries(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [
        category,
        firstSteps[category].spendingScriptHash,
      ]),
    ),
  );
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
) => {
  const registeredEntries: Record<string, unknown> = {};
  let referenceOutputIndex = 0;
  for (const category of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const chain = contracts[category];
    for (const [stepIndex, name] of FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[
      category
    ].entries()) {
      const step = chain.steps[stepIndex];
      if (step === undefined) {
        throw new Error(`${category} fixture is missing step ${stepIndex}.`);
      }
      registeredEntries[name] = {
        scriptHash: step.spendingScriptHash,
        refScriptUTxO: {
          txHash: "aa".repeat(32),
          outputIndex: referenceOutputIndex,
        },
      };
      referenceOutputIndex += 1;
    }
  }
  return {
    referenceScriptAuthPolicy,
    contracts: {
      ...registeredEntries,
      hubOracleMint: { scriptHash: h28 },
      fraudProofCatalogueMint: {
        scriptHash: h28b,
        fraudProofCatalogue,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofSpend: {
        scriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofDoubleSpend: {
        scriptHash: doubleSpendScriptHash,
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 0 },
      },
      fraudProofNonExistentInput: {
        scriptHash: nonExistentInputScriptHash,
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 1 },
      },
      fraudProofNonExistentInputNoIndex: {
        scriptHash:
          contracts.nonExistentInputNoIndex.firstStep.spendingScriptHash,
        contract: {
          type: "PlutusV3" as const,
          cborHex:
            contracts.nonExistentInputNoIndex.firstStep.spendingScript.script,
        },
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 2 },
      },
      fraudProofInvalidRange: {
        scriptHash: invalidRangeScriptHash,
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 3 },
      },
      fraudProofZeroInput: {
        scriptHash: zeroInputScriptHash,
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 13 },
      },
      fraudProofTransitionTrace: {
        scriptHash: transitionTraceScriptHash,
        refScriptUTxO: { txHash: "aa".repeat(32), outputIndex: 4 },
      },
      validationTraceDispute: {
        scriptHash:
          contracts.validationTraceDispute.firstStep.spendingScriptHash,
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
  };
};

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
      ...Array.from({ length: 91 }, (_, index) => `semantic-resolver-${index}`),
      ...Array.from({ length: 14 }, (_, index) => `prepare-resolver-${index}`),
    ]);
    const appliedSpendingScripts = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.flatMap(
      (category) => {
        const steps =
          category === "transitionTrace"
            ? output.transitionTrace.steps
            : category === "validationTraceDispute"
              ? output.validationTraceDispute.steps
              : output.registeredCategories[category].steps;
        return steps.map((step) => ({
          category,
          step,
        }));
      },
    );
    const selectedParameterizedValidators =
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.flatMap((category) =>
        category === "validationTraceDispute"
          ? [
              contracts.validationTraceDispute.opener,
              contracts.validationTraceDispute.source,
              contracts.validationTraceDispute.game,
              contracts.validationTraceDispute.boundary,
              contracts.validationTraceDispute.timeout,
              contracts.validationTraceDispute.award,
              ...contracts.validationTraceDispute.semanticResolvers,
              ...contracts.validationTraceDispute.prepareResolvers,
            ]
          : contracts[category].steps,
      );
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
    expect(
      Object.entries(output.fraudProofCatalogue.categories)
        .filter(([, category]) => !category.ready)
        .map(([name]) => name),
    ).toEqual([]);
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
