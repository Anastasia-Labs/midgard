import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import type { ReferenceScriptAuthPolicyDeploymentInfo } from "@/deployment/reference-script-auth.js";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthTokenName,
} from "@/deployment/reference-script-auth.js";
import {
  buildContractDeploymentInfoProgram,
  buildContractDeploymentInfoFromContracts,
  defaultContractDeploymentInfoOutputPath,
} from "@/commands/contract-deployment-info.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

const testReferenceScriptAuthPolicy = (
  policyId: string,
): ReferenceScriptAuthPolicyDeploymentInfo => ({
  policyId,
  nativeScript: {
    type: "Native",
    cborHex: "00",
    expiresAtSlot: 0,
    expiresAtUnixTime: 0,
    timelockDurationMs: 0,
  },
  tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  postTimelockAudit: {
    required: true,
    rule: "test fixture",
  },
});

describe("contract deployment info", () => {
  it.effect(
    "builds explicit script entries for the current validator bundle",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
        );
        const manifest = buildContractDeploymentInfoFromContracts(
          contracts,
          authPolicy,
        );

        expect(manifest.referenceScriptAuthPolicy.policyId).toEqual(
          contracts.referenceScriptAuth.policyId,
        );
        expect(manifest.contracts.hubOracleMint.contract.type).toEqual(
          "PlutusV3",
        );
        expect(manifest.contracts.hubOracleMint.scriptHash).toEqual(
          contracts.hubOracle.policyId,
        );
        expect(manifest.contracts.daAttestationMint.scriptHash).toEqual(
          contracts.daAttestation.policyId,
        );
        expect(manifest.contracts.daParamsGovernorMint.scriptHash).toEqual(
          contracts.daParamsGovernor.policyId,
        );
        expect(manifest.contracts.depositMint.scriptHash).toEqual(
          contracts.deposit.policyId,
        );
        expect(manifest.contracts.depositSpend.scriptHash).toEqual(
          contracts.deposit.spendingScriptHash,
        );
        expect(manifest.contracts.reserveWithdraw.scriptHash).toEqual(
          contracts.reserve.withdrawalScriptHash,
        );
        expect(manifest.contracts.reserveSpend.scriptHash).toEqual(
          contracts.reserve.spendingScriptHash,
        );
        expect(manifest.contracts.payoutSpend.scriptHash).toEqual(
          contracts.payout.spendingScriptHash,
        );
        expect(manifest.contracts.payoutMint.scriptHash).toEqual(
          contracts.payout.policyId,
        );
        expect(manifest.contracts.depositMint.refScriptUTxO).toBeNull();
        expect(
          manifest.contracts.fraudProofInvalidRange.refScriptUTxO,
        ).toBeNull();
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("can attach fraud-proof catalogue deployment metadata", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
      );
      const manifest = buildContractDeploymentInfoFromContracts(
        contracts,
        authPolicy,
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

      expect(
        manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue?.root,
      ).toBe("aa".repeat(32));
      expect(
        manifest.contracts.fraudProofCatalogueSpend.fraudProofCatalogue,
      ).toBeUndefined();
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "requires the matching role token before recording a ref script",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
        );
        const stateQueueMintRef: UTxO = {
          txHash: "01".repeat(32),
          outputIndex: 0,
          address: "addr_test1reference",
          assets: {
            lovelace: 4_000_000n,
            [toUnit(
              authPolicy.policyId,
              referenceScriptAuthTokenName("state-queue minting"),
            )]: 1n,
          },
          scriptRef: contracts.stateQueue.mintingScript,
        };
        const manifest = yield* buildContractDeploymentInfoProgram(
          contracts,
          [stateQueueMintRef],
          authPolicy,
        );

        expect(manifest.contracts.stateQueueMint.refScriptUTxO).toEqual({
          txHash: stateQueueMintRef.txHash,
          outputIndex: stateQueueMintRef.outputIndex,
        });
        expect(manifest.contracts.daAttestationMint.refScriptUTxO).toBeNull();
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
