import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import { initialFundingOutputs } from "../src/devnet-stack/funding.js";
import {
  FUNDING_POLICY,
  ROLE_COLLATERAL_LOVELACE,
  roleBudgetLovelace,
  roleFeeRunwayLovelace,
  totalRoleFundingLovelace,
} from "../src/devnet-stack/funding-budget.js";
import { WALLET_ROLES } from "../src/devnet-stack/identities.js";

describe("devnet genesis fee runway", () => {
  it("funds all fourteen distinct roles with planned fee runway and separate collateral", () => {
    expect(WALLET_ROLES).toEqual([
      "operator",
      "merge",
      "referenceScript",
      "settlement",
      "daCosigner",
      "daSubmitter0",
      "daSubmitter1",
      "daAvailability0",
      "daAvailability1",
      "watcherProver",
      "watcherAvailability",
      "userA",
      "userB",
      "userC",
    ]);
    const wallets = Object.fromEntries(
      WALLET_ROLES.map((role) => [role, { address: `address_${role}` }]),
    ) as Parameters<typeof initialFundingOutputs>[0];
    const args = initialFundingOutputs(wallets, "1 token");
    const outputs = args.filter((_, index) => index % 2 === 1);
    let funded = 0n;
    for (const role of WALLET_ROLES) {
      const amounts = outputs
        .filter((output) => output.startsWith(`${wallets[role].address}+`))
        .map((output) => BigInt(output.split("+")[1]!));
      expect(amounts).toContain(50_000_000n);
      const budget =
        amounts.reduce((sum, value) => sum + value, 0n) -
        ROLE_COLLATERAL_LOVELACE;
      expect(budget).toBe(roleBudgetLovelace(role));
      expect(budget).toBeGreaterThanOrEqual(roleFeeRunwayLovelace(role));
      funded += budget + ROLE_COLLATERAL_LOVELACE;
    }
    expect(funded).toBe(totalRoleFundingLovelace());
  });

  it("plans six months with fivefold margin, without calling estimates measured burn", () => {
    expect(FUNDING_POLICY.targetDays).toBe(180);
    expect(FUNDING_POLICY.margin).toBe(5);
    // 20 ADA * 4,320 transactions/day * 180 days * 5 = 77,760,000 ADA.
    expect(roleFeeRunwayLovelace("operator")).toBe(77_760_000_000_000n);
    expect(roleFeeRunwayLovelace("daSubmitter0")).toBe(77_760_000_000_000n);
    expect(roleFeeRunwayLovelace("userC")).toBe(432_000_000_000n);
    expect(roleFeeRunwayLovelace("daCosigner")).toBe(0n);
  });

  it("reserves pure ADA collateral above 150 percent of the planning fee", () => {
    expect(ROLE_COLLATERAL_LOVELACE).toBeGreaterThanOrEqual(
      (BigInt(FUNDING_POLICY.planningFeeLovelace) * 150n) / 100n,
    );
  });

  it("keeps the role outputs safely inside the genesis UTxO allocation", () => {
    const spendable =
      BigInt(FUNDING_POLICY.totalSupplyLovelace) -
      BigInt(FUNDING_POLICY.delegatedSupplyLovelace);
    // Includes startup protocol capital as well as all fee reserves/collateral.
    expect(totalRoleFundingLovelace()).toBe(701_837_700_000_000n);
    expect(totalRoleFundingLovelace() + 1_000_000_000n).toBeLessThan(spendable);
    expect(BigInt(FUNDING_POLICY.totalSupplyLovelace)).toBeLessThan(
      45_000_000_000_000_000n,
    );
    expect(
      BigInt(FUNDING_POLICY.legacyMainBudgetLovelace) * 7n +
        ROLE_COLLATERAL_LOVELACE * 7n +
        1_000_000_000n,
    ).toBeLessThan(spendable);
    const legacy = readFileSync(
      new URL(
        "../devnet/phase4-process/scripts/fund-wallets.sh",
        import.meta.url,
      ),
      "utf8",
    );
    expect(legacy).toContain("legacyMainBudgetLovelace");
    expect(legacy).toContain('"$lovelace" -ge "$required_balance"');
    const generator = readFileSync(
      new URL("../devnet/phase4-process/scripts/generate.sh", import.meta.url),
      "utf8",
    );
    expect(generator).toContain("funding-policy.json");
    expect(generator).toContain(
      '--total-supply "$total_supply" --delegated-supply "$delegated_supply"',
    );
  });
});
