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
