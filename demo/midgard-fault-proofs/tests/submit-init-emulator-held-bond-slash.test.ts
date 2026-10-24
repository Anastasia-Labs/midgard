import { describe, expect, it } from "vitest";

import { fraudSlashEconomicsFromDeploymentManifest } from "../src/remove-fraudulent-block.js";
import {
  buildProvedDoubleSpendFixture,
  expectRemovedFraudProofState,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";

describe("fault-proof slash of an operator node holding more than its bond", () => {
  it("removes the block and keeps the node's excess in the directory anchor", async () => {
    const fixture = await buildProvedDoubleSpendFixture({
      bondPaddingLovelace: 1n,
    });
    const { contracts, funderLucid, setup } = fixture;
    const lovelaceAt = async (address: string, unit: string) => {
      const utxos = await funderLucid.utxosAtWithUnit(address, unit);
      expect(utxos).toHaveLength(1);
      return utxos[0]!.assets.lovelace ?? 0n;
    };
    const directory = contracts.activeOperators.spendingScriptAddress;
    const { requiredBondLovelace } = fraudSlashEconomicsFromDeploymentManifest(
      fixture.deploymentInfo,
    );
    await expect(
      lovelaceAt(directory, setup.activeOperatorNodeUnit),
    ).resolves.toBe(requiredBondLovelace + 1n);
    const anchorBefore = await lovelaceAt(
      directory,
      setup.activeOperatorsRootUnit,
    );

    const removeResult = await submitRemovalForFixture(fixture);

    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectRemovedFraudProofState(fixture);
    await expect(
      lovelaceAt(directory, setup.activeOperatorsRootUnit),
    ).resolves.toBe(anchorBefore + 1n);
  }, 300_000);
});
