import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyBlueprintParams,
  getUnappliedScript,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { type Data, type Network } from "@lucid-evolution/lucid";

export const moduleDir = dirname(fileURLToPath(import.meta.url));

export const repoRoot = resolve(moduleDir, "../../../../..");

/**
 * The compiled artifact supplied to the emulator scenarios. These helpers read
 * it but never rebuild it. Build the pinned fork's testnet blueprint first, or
 * set MIDGARD_REAL_BLUEPRINT_PATH to an explicitly prepared artifact.
 *
 * Schema agreement and successful loading do not prove correct parameter
 * application: the deploying scenarios must exercise success and refusal with
 * the same artifact. Current acceptance requirements live in
 * docs/fault-proofs/testing-status.md; prior run results belong with their
 * release evidence rather than this loader.
 */
export const realBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

export const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

export const network: Network = "Preprod";

export type BlueprintParameter = {
  readonly title: string;
  readonly schema?: { readonly $ref?: string };
};

export type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /** Raw compiler parameters; normalization belongs at the SDK boundary. */
  readonly parameters?: readonly BlueprintParameter[];
};

/** Raw compiler JSON; normalized SDK parameters use schemaRef instead of schema.$ref. */
export type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

/** Keep loading raw JSON: production family callers normalize it themselves. */
export const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

export const cloneBlueprint = (blueprint: Blueprint): Blueprint =>
  JSON.parse(JSON.stringify(blueprint)) as Blueprint;

/** Delegate bare loading to the same strict zero-parameter boundary as production. */
export const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): string => getUnappliedScript(parseFaultProofBlueprint(blueprint), title);

/** Share production arity/shape validation and its cache for every application. */
export const applyCompiledScript = (
  blueprint: Blueprint,
  title: string,
  params: readonly Data[],
): string =>
  applyBlueprintParams(parseFaultProofBlueprint(blueprint), title, params);
