import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";
import {
  importComposeAndChain,
  observerTransaction,
  partialSigningAndSubmit,
  providerConvenienceAndStatusStates,
  providerSwitchingAndOverrides,
  safeAndEffectErrors,
  simpleBalancedTransfer,
} from "../examples/usage.js";

const packageRoot = resolve(import.meta.dirname, "..");
const readPackageFile = (path: string): string =>
  readFileSync(resolve(packageRoot, path), "utf8");

describe("documentation and examples", () => {
  it("keeps the package README aligned with the implemented API", () => {
    const readme = readPackageFile("README.md");
    expect(readme).not.toContain("initial scaffold phase");
    for (const required of [
      "Midgard-native L2 transaction builder",
      "Provider Switching And UTxO Overrides",
      "Observer Validators",
      "Partial Signing",
      "Local Chaining",
      "Safe And Effect APIs",
      "Durable Admission",
      "Protected Outputs",
      "supportedScriptLanguages",
    ]) {
      expect(readme).toContain(required);
    }
  });

  it("runs the package examples against the in-memory provider", async () => {
    await expect(simpleBalancedTransfer()).resolves.toMatch(/^[0-9a-f]{64}$/);
    await expect(providerSwitchingAndOverrides()).resolves.toBe(1);
    await expect(providerConvenienceAndStatusStates()).resolves.toEqual([
      "queued",
      "accepted",
      "rejected",
      "E_EXAMPLE",
      "committed",
    ]);
    await expect(observerTransaction()).resolves.toMatch(/^[0-9a-f]{64}$/);
    await expect(importComposeAndChain()).resolves.toHaveLength(2);
    await expect(partialSigningAndSubmit()).resolves.toMatch(/^[0-9a-f]{64}$/);
    await expect(safeAndEffectErrors()).resolves.toEqual({
      promiseCode: "BUILDER_INVARIANT",
      safeCode: "BUILDER_INVARIANT",
      effectCode: "BUILDER_INVARIANT",
      timeoutCode: "PROVIDER_ERROR",
    });
  });

  it("guards docs and examples against Cardano submission shortcuts", () => {
    const readme = readPackageFile("README.md");
    const usage = readPackageFile("examples/usage.ts");
    const forbiddenLocalEvalOption = ["local", "UPLC", "Eval"].join("");
    expect(`${readme}\n${usage}`).not.toMatch(
      new RegExp(
        `${forbiddenLocalEvalOption}|Blockfrost|Maestro|Kupmios|Koios|Emulator|Cardano-to-native`,
        "i",
      ),
    );
  });
});
