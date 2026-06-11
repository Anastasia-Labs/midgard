import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  defaultContractDeploymentInfoOutputPath,
} from "@/commands/contract-deployment-info.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

describe("contract deployment info", () => {
  it.effect(
    "builds explicit script entries for the current validator bundle",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const manifest = buildContractDeploymentInfoFromContracts(contracts);

        expect(manifest.hubOracleMint.contract.type).toEqual("PlutusV3");
        expect(manifest.hubOracleMint.scriptHash).toEqual(
          contracts.hubOracle.policyId,
        );
        expect(manifest.depositMint.scriptHash).toEqual(
          contracts.deposit.policyId,
        );
        expect(manifest.depositSpend.scriptHash).toEqual(
          contracts.deposit.spendingScriptHash,
        );
        expect(manifest.reserveWithdraw.scriptHash).toEqual(
          contracts.reserve.withdrawalScriptHash,
        );
        expect(manifest.reserveSpend.scriptHash).toEqual(
          contracts.reserve.spendingScriptHash,
        );
        expect(manifest.payoutSpend.scriptHash).toEqual(
          contracts.payout.spendingScriptHash,
        );
        expect(manifest.payoutMint.scriptHash).toEqual(
          contracts.payout.policyId,
        );
        expect(manifest.depositMint.refScriptUTxO).toBeNull();
        expect(manifest.fraudProofInvalidRange.refScriptUTxO).toBeNull();
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("can attach fraud-proof catalogue deployment metadata", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const manifest = buildContractDeploymentInfoFromContracts(
        contracts,
        new Map(),
        {
          root: "aa".repeat(32),
          categories: {
            doubleSpend: {
              categoryId: "00000000",
              scriptHash: contracts.fraudProofs.doubleSpend.spendingScriptHash,
              membershipProofCbor: "80",
            },
            nonExistentInput: {
              categoryId: "00000001",
              scriptHash:
                contracts.fraudProofs.nonExistentInput.spendingScriptHash,
              membershipProofCbor: "80",
            },
            nonExistentInputNoIndex: {
              categoryId: "00000002",
              scriptHash:
                contracts.fraudProofs.nonExistentInputNoIndex
                  .spendingScriptHash,
              membershipProofCbor: "80",
            },
            invalidRange: {
              categoryId: "00000003",
              scriptHash: contracts.fraudProofs.invalidRange.spendingScriptHash,
              membershipProofCbor: "80",
            },
          },
        },
      );

      expect(manifest.fraudProofCatalogueMint.fraudProofCatalogue?.root).toBe(
        "aa".repeat(32),
      );
      expect(
        manifest.fraudProofCatalogueSpend.fraudProofCatalogue,
      ).toBeUndefined();
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it("defaults init manifest output to the package-root deploymentInfo", () => {
    const packageRoot = resolvePath(
      dirname(fileURLToPath(import.meta.url)),
      "..",
    );
    expect(defaultContractDeploymentInfoOutputPath()).toEqual(
      resolvePath(
        packageRoot,
        "deploymentInfo",
        "contract-deployment-info.json",
      ),
    );
  });
});
