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

/** Captured with the real blueprint before replacing the node catalogue. */
export const expectPreMigrationDeploymentBoundary = (
  contracts: MidgardValidators,
): void => {
  const digest = (value: unknown) =>
    createHash("sha256").update(JSON.stringify(value)).digest("hex");
  const manifest = manifestDeployableScripts(contracts);
  expect(manifest).toHaveLength(527);
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
  ).toBe("d74d2af48c5c18aa32fe6ad036df27e985e6ba79d229b0bd521ef9ea801c2c84");
  expect(
    digest(
      publishedDeployableScripts(contracts).map(({ contract }) => contract),
    ),
  ).toBe("2e16ed8677ba531b608efad6d920e0b1f14efae370b7083d10f928e375faad9e");
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
