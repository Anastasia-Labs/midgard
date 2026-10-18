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
 * Captured with the real blueprint before replacing the node catalogue.
 *
 * Re-captured when the six validation-case members and the five output-step
 * transition-trace yields (less the three retired claim yields) joined the
 * member owner. The pre-migration catalogue that added them is gone, so the
 * values were taken from the member projections and checked two ways: the
 * order pins of `deployable-catalogue-order-pins.ts` (manifest, publication
 * and per-command orders of that catalogue) all hold, and the DA deployment
 * fixture regenerated through the member owner is byte-identical to the one
 * that catalogue produced (all 535 contracts, script hashes included).
 *
 * Re-taken again when script-decode replaced the CEK program-material members
 * (four fewer manifest entries): the catalogue order pins of
 * `deployable-catalogue-order-pins.ts` held across that re-take.
 */
export const expectPreMigrationDeploymentBoundary = (
  contracts: MidgardValidators,
): void => {
  const digest = (value: unknown) =>
    createHash("sha256").update(JSON.stringify(value)).digest("hex");
  const manifest = manifestDeployableScripts(contracts);
  expect(manifest).toHaveLength(531);
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
  ).toBe("0533396eeb45f0940c7c47b604e38a65ca8ef31e2f4ed4ddfe15dbb9193735f1");
  expect(
    digest(
      publishedDeployableScripts(contracts).map(({ contract }) => contract),
    ),
  ).toBe("d77ef9172edd1b4e78d85d247bee68812967e665e2d79c4aea679b68a7d4f7e1");
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
