import { createHash } from "node:crypto";

import {
  type MidgardValidators,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  manifestDeployableScripts,
  publishedDeployableScripts,
} from "../../src/deployable-scripts.js";

/**
 * Captured with the real blueprint before replacing the node catalogue, and
 * re-taken when script-decode replaced the CEK program-material members: the
 * catalogue order pins in deployable-scripts.test.ts held across that re-take.
 */
export const expectPreMigrationDeploymentBoundary = (
  contracts: MidgardValidators,
): void => {
  const digest = (value: unknown) =>
    createHash("sha256").update(JSON.stringify(value)).digest("hex");
  const manifest = manifestDeployableScripts(contracts);
  expect(manifest).toHaveLength(523);
  expect(
    digest(
      manifest.map(({ contract, role, purpose, commands, scriptHash }) => [
        contract,
        role ?? null,
        purpose,
        commands,
        scriptHash,
      ]),
    ),
  ).toBe("c3ac54a7acb046969e249c832026152d1c1e74b730e483d7e06ceef846209671");
  expect(
    digest(
      publishedDeployableScripts(contracts).map(({ contract }) => contract),
    ),
  ).toBe("a3b2822a233699de9537322f222bde5a0d5ff01f1918f75eb741635f55a7507e");
};

/** Pre-migration admission rule, kept independent of the new member projections. */
export const VALIDATION_TRACE_SEMANTIC_KEYS = Object.keys(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
);
export const isRecordedValidationTraceSemantic = (key: string): boolean =>
  [
    "resolveInputs",
    "scriptSources",
    "phaseANativeScripts",
    "phaseAScriptPreconditions",
  ].some((prefix) => key.startsWith(prefix)) ||
  key === "canonicalDecodeEmpty" ||
  key === "canonicalDecodeItem";
