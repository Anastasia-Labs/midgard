import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  completeFraudProofCategoryRecord,
  expectedFraudProofCategoryId,
  type ImplementedFraudProofCategoryName,
  type InspectContractsCatalogueCategoryOutput,
  type InspectContractsOutput,
} from "./inspect-contracts.inspect-contracts-output.js";
import { emptyCatalogueCategoryInspection } from "./inspect-contracts.inspect-fraud-proof-catalogue-category-readiness.js";
import {
  encodeCatalogueKey,
  encodeCatalogueValue,
  fraudProofCatalogueCategories,
  trieRootHex,
} from "./inspect-contracts.parse-contract-deployment-info.js";

export const inspectFraudProofCatalogue = (
  catalogue: FraudProofCatalogueDeploymentInfo | undefined,
  expectedFirstStepHashes: Readonly<
    Record<ImplementedFraudProofCategoryName, string>
  >,
): Effect.Effect<InspectContractsOutput["fraudProofCatalogue"], Error> => {
  if (catalogue === undefined) {
    const categories = completeFraudProofCategoryRecord(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
        (name) => [name, emptyCatalogueCategoryInspection()] as const,
      ),
    );
    return Effect.succeed({
      root: null,
      derivedRoot: null,
      rootMatchesDerived: null,
      categories,
      doubleSpend: categories.doubleSpend,
      nonExistentInput: categories.nonExistentInput,
      nonExistentInputNoIndex: categories.nonExistentInputNoIndex,
      invalidRange: categories.invalidRange,
      zeroInput: categories.zeroInput,
      transitionTrace: categories.transitionTrace,
      validationTraceDispute: categories.validationTraceDispute,
      daHashPreimage: categories.daHashPreimage,
      noReferenceInput: categories.noReferenceInput,
      referenceInputNoIdx: categories.referenceInputNoIdx,
      invalidSignature: categories.invalidSignature,
    });
  }

  return Effect.tryPromise({
    try: async () => {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      const categories = fraudProofCatalogueCategories(catalogue);
      for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        const category = categories[categoryName];
        if (category === undefined) {
          throw new Error(
            `fraudProofCatalogue.categories.${categoryName} is missing`,
          );
        }
        await trie.insert(
          encodeCatalogueKey(category.categoryId),
          encodeCatalogueValue(category.scriptHash),
        );
      }

      const derivedRoot = trieRootHex(trie);
      const rootMatchesDerived = catalogue.root === derivedRoot;
      const inspectCategory = async (
        name: ImplementedFraudProofCategoryName,
      ): Promise<InspectContractsCatalogueCategoryOutput> => {
        const category = categories[name];
        if (category === undefined) {
          return emptyCatalogueCategoryInspection();
        }
        const expectedCategoryId = expectedFraudProofCategoryId(name);
        const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
        const derivedProofCbor = proof.toCBOR().toString("hex");
        const categoryIdMatchesExpected =
          category.categoryId === expectedCategoryId;
        const scriptHashMatchesFirstStep =
          category.scriptHash === expectedFirstStepHashes[name];
        const membershipProofMatchesDerived =
          category.membershipProofCbor === derivedProofCbor;
        return {
          categoryId: category.categoryId,
          expectedCategoryId,
          categoryIdMatchesExpected,
          scriptHash: category.scriptHash,
          scriptHashMatchesFirstStep,
          membershipProofCbor: category.membershipProofCbor,
          membershipProofMatchesDerived,
        };
      };

      const inspectedCategories = await Promise.all(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
          async (name) => [name, await inspectCategory(name)] as const,
        ),
      );
      const inspectedByName =
        completeFraudProofCategoryRecord(inspectedCategories);

      return {
        root: catalogue.root,
        derivedRoot,
        rootMatchesDerived,
        categories: inspectedByName,
        doubleSpend: inspectedByName.doubleSpend,
        nonExistentInput: inspectedByName.nonExistentInput,
        nonExistentInputNoIndex: inspectedByName.nonExistentInputNoIndex,
        invalidRange: inspectedByName.invalidRange,
        zeroInput: inspectedByName.zeroInput,
        transitionTrace: inspectedByName.transitionTrace,
        validationTraceDispute: inspectedByName.validationTraceDispute,
        daHashPreimage: inspectedByName.daHashPreimage,
        noReferenceInput: inspectedByName.noReferenceInput,
        referenceInputNoIdx: inspectedByName.referenceInputNoIdx,
        invalidSignature: inspectedByName.invalidSignature,
      };
    },
    catch: (cause) =>
      new Error(
        `Failed to inspect fraud-proof catalogue deployment info: ${formatUnknownError(cause)}`,
      ),
  });
};
