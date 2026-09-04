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
  createFraudProofCatalogueMpf,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";

describe("Fraud Proof Catalogue Root", () => {
  it.effect(
    "computes root and verifies pre-image retrieval for full validator set",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;

        const fraudProofs = contracts.fraudProofs;

        const indexedFraudProofs = fraudProofsToIndexedValidators(fraudProofs);
        // Every declared family appears exactly once, while its wire identity
        // comes from the explicit map rather than presentation position.
        // Comparing against the declared validator record (rather than a
        // hardcoded count) catches a family that is added to `FraudProofs` but
        // never registered in the catalogue order.
        expect(indexedFraudProofs.map(([, , name]) => name).sort()).toEqual(
          Object.keys(fraudProofs).sort(),
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

        const fraudProofsMPF =
          yield* createFraudProofCatalogueMpf(indexedFraudProofs);
        const deploymentInfo =
          yield* buildFraudProofCatalogueDeploymentInfo(indexedFraudProofs);

        const rootHash = yield* fraudProofsMPF.rootHex();
        console.log(`Fraud Proofs Merkle Root: ${rootHash}`);
        expect(deploymentInfo.root).toBe(rootHash);

        const indicesToCheck = [
          0,
          1,
          Math.floor(indexedFraudProofs.length / 2),
          indexedFraudProofs.length - 1,
        ];

        for (const i of indicesToCheck) {
          const [categoryId, fraudProof, categoryName] = indexedFraudProofs[i];
          const category = deploymentInfo.categories[categoryName];
          expect(category.categoryId).toBe(categoryId.toString("hex"));
          expect(category.categoryId).toBe(
            FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
          );
          expect(category.scriptHash).toBe(fraudProof.spendingScriptHash);
          expect(category.membershipProofCbor.length).toBeGreaterThan(0);
        }
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
