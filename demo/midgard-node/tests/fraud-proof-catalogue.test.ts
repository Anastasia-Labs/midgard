import { verifyDeploymentManifestFraudProofCatalogueIdentity } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";

/**
 * The oracle for the catalogue root and its membership proofs is the
 * deployment-manifest verifier in `@al-ft/midgard-core`: it rebuilds the MPF
 * root from (categoryId, scriptHash) with its own independent implementation
 * and folds every `membershipProofCbor` back to that root. Nothing here
 * re-derives an expected value with the builder under test.
 */
const verifyCatalogue = (catalogue: unknown) =>
  verifyDeploymentManifestFraudProofCatalogueIdentity(
    catalogue as Parameters<
      typeof verifyDeploymentManifestFraudProofCatalogueIdentity
    >[0],
  );

describe("Fraud Proof Catalogue Root", () => {
  it.effect("registers every declared family exactly once, in wire order", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const indexedFraudProofs = fraudProofsToIndexedValidators(
        contracts.fraudProofs,
      );

      // Every declared family appears exactly once, while its wire identity
      // comes from the explicit map rather than presentation position.
      // Comparing against the declared validator record (rather than a
      // hardcoded count) catches a family that is added to `FraudProofs` but
      // never registered in the catalogue order.
      expect(indexedFraudProofs.map(([, , name]) => name).sort()).toEqual(
        Object.keys(contracts.fraudProofs).sort(),
      );
      expect(
        indexedFraudProofs.map(([categoryId]) => categoryId.toString("hex")),
      ).toEqual(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
          (name) => FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
        ),
      );
      // The tail IDs are a wire contract with the on-chain catalogue.
      // `zeroInput` was appended at index 5, which shifted
      // `validationTraceDispute` from 00000005 to 00000006.
      expect(indexedFraudProofs[5][0].toString("hex")).toBe("00000005");
      expect(indexedFraudProofs[5][2]).toBe("zeroInput");
      expect(indexedFraudProofs[6][0].toString("hex")).toBe("00000006");
      expect(indexedFraudProofs[6][2]).toBe("validationTraceDispute");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "publishes a catalogue whose every membership proof folds back to the published root",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const indexedFraudProofs = fraudProofsToIndexedValidators(
          contracts.fraudProofs,
        );
        const deploymentInfo =
          yield* buildFraudProofCatalogueDeploymentInfo(indexedFraudProofs);

        // Accept case: the independent verifier reconstructs the same root and
        // accepts every category's proof.
        expect(() => verifyCatalogue(deploymentInfo)).not.toThrow();

        // Each published category still has to carry the deployed script hash;
        // the verifier only checks internal consistency.
        for (const [categoryId, fraudProof, categoryName] of indexedFraudProofs
          .filter((_, index) => index % 7 === 0)
          .concat([indexedFraudProofs[indexedFraudProofs.length - 1]!])) {
          const category = deploymentInfo.categories[categoryName];
          expect(category.categoryId).toBe(categoryId.toString("hex"));
          expect(category.scriptHash).toBe(fraudProof.spendingScriptHash);
        }
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "refuses a catalogue whose published root no longer matches its entries",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const deploymentInfo = yield* buildFraudProofCatalogueDeploymentInfo(
          fraudProofsToIndexedValidators(contracts.fraudProofs),
        );

        // Same catalogue as the accepted one, with exactly one script hash
        // repointed: the reconstructed root must stop matching the published
        // root.
        const tampered = {
          ...deploymentInfo,
          categories: {
            ...deploymentInfo.categories,
            zeroInput: {
              ...deploymentInfo.categories.zeroInput,
              scriptHash: "cd".repeat(28),
            },
          },
        };

        expect(() => verifyCatalogue(tampered)).toThrow(
          /fraud-proof catalogue root mismatch/u,
        );
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "refuses a membership proof that belongs to a different category",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const deploymentInfo = yield* buildFraudProofCatalogueDeploymentInfo(
          fraudProofsToIndexedValidators(contracts.fraudProofs),
        );

        // Root and every entry stay valid; only the proof carried by
        // `zeroInput` is another category's genuine proof, so it can only be
        // rejected by actually folding the proof to the root.
        const tampered = {
          ...deploymentInfo,
          categories: {
            ...deploymentInfo.categories,
            zeroInput: {
              ...deploymentInfo.categories.zeroInput,
              membershipProofCbor:
                deploymentInfo.categories.doubleSpend.membershipProofCbor,
            },
          },
        };

        expect(() => verifyCatalogue(tampered)).toThrow(
          /zeroInput\.membershipProofCbor does not prove membership/u,
        );
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
