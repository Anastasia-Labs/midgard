import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  createFraudProofCatalogueMpf,
  fraudProofsToIndexedValidators,
  uint32ToFraudProofID,
} from "@/transactions/initialization.js";

describe("Fraud Proof Catalogue Root", () => {
  it.effect(
    "computes root and verifies pre-image retrieval for full validator set",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;

        const fraudProofs = contracts.fraudProofs;

        const indexedFraudProofs = fraudProofsToIndexedValidators(fraudProofs);
        // Category IDs are the positional index in
        // FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER, so every declared fraud-proof
        // family must appear exactly once and every ID must equal its index.
        // Comparing against the declared validator record (rather than a
        // hardcoded count) catches a family that is added to `FraudProofs` but
        // never registered in the catalogue order.
        expect(indexedFraudProofs.map(([, , name]) => name).sort()).toEqual(
          Object.keys(fraudProofs).sort(),
        );
        expect(
          indexedFraudProofs.map(([categoryId]) => categoryId.toString("hex")),
        ).toEqual(
          indexedFraudProofs.map((_entry, index) =>
            uint32ToFraudProofID(index).toString("hex"),
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
            uint32ToFraudProofID(i).toString("hex"),
          );
          expect(category.scriptHash).toBe(fraudProof.spendingScriptHash);
          expect(category.membershipProofCbor.length).toBeGreaterThan(0);
        }
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
