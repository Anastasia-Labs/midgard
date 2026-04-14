import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  buildContractDeploymentInfoFromContracts,
  defaultContractDeploymentInfoOutputPath,
} from "@/commands/contract-deployment-info.js";

describe("contract deployment info", () => {
  it.effect("builds explicit script entries for the current validator bundle", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const manifest = buildContractDeploymentInfoFromContracts(contracts);

      expect(manifest.hubOracleMint.contract.type).toEqual("PlutusV3");
      expect(manifest.hubOracleMint.scriptHash).toEqual(
        contracts.hubOracle.policyId,
      );
      expect(manifest.depositMint.scriptHash).toEqual(contracts.deposit.policyId);
      expect(manifest.depositSpend.scriptHash).toEqual(
        contracts.deposit.spendingScriptHash,
      );
      expect(manifest.reserveWithdraw.scriptHash).toEqual(
        contracts.reserve.withdrawalScriptHash,
      );
      expect(manifest.depositMint.refScriptUTxO).toBeNull();
      expect(manifest.fraudProofInvalidRange.refScriptUTxO).toBeNull();
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it("defaults init manifest output to the package-root deploymentInfo", () => {
    const packageRoot = resolvePath(dirname(fileURLToPath(import.meta.url)), "..");
    expect(defaultContractDeploymentInfoOutputPath()).toEqual(
      resolvePath(packageRoot, "deploymentInfo", "contract-deployment-info.json"),
    );
  });
});
