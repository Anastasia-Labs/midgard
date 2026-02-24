import { describe, expect, it } from "vitest";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";

describe("shouldRunGenesisOnStartup", () => {
  it("defaults to false even on testnet", () => {
    expect(shouldRunGenesisOnStartup({ network: "Preprod", runGenesisOnStartup: false })).toBe(false);
    expect(shouldRunGenesisOnStartup({ network: "Preview", runGenesisOnStartup: false })).toBe(false);
  });

  it("never runs on mainnet", () => {
    expect(shouldRunGenesisOnStartup({ network: "Mainnet", runGenesisOnStartup: true })).toBe(false);
  });

  it("runs only when explicitly enabled on non-mainnet", () => {
    expect(shouldRunGenesisOnStartup({ network: "Preprod", runGenesisOnStartup: true })).toBe(true);
  });
});
