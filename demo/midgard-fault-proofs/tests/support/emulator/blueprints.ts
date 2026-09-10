import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Data,
  type Network,
} from "@lucid-evolution/lucid";

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
};

export type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /**
   * The blueprint's declared parameter list, carried so the loaders below can
   * check it against what the caller actually applies (#610). Absent means the
   * validator declares none — that is the compiler's encoding for a nullary
   * validator, never "unknown, skip the check".
   */
  readonly parameters?: readonly BlueprintParameter[];
};

export type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

export const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

export const cloneBlueprint = (blueprint: Blueprint): Blueprint =>
  JSON.parse(JSON.stringify(blueprint)) as Blueprint;

/** Absent `parameters` is the compiler's encoding of "declares none" (#610). */
const declaredParametersOf = (
  validator: BlueprintValidator,
): readonly BlueprintParameter[] => validator.parameters ?? [];

const describeDeclaredParameters = (
  declaredParameters: readonly BlueprintParameter[],
): string =>
  declaredParameters.length === 0
    ? "none"
    : declaredParameters.map((parameter) => parameter.title).join(", ");

const requireBlueprintValidator = (
  blueprint: Blueprint,
  title: string,
): BlueprintValidator => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return found;
};

/**
 * The bare-load door (#610): returns `compiledCode` with nothing applied, so it
 * is only sound while the validator declares no parameters.
 *
 * A declared parameter deployed unapplied is the #605 under-application shape —
 * the unapplied `validator main(...)` parameters stay as lambdas, the ledger's
 * single Plutus V3 script-context application reduces to a lambda VALUE rather
 * than running the validator body, evaluation ends without error, and the
 * ledger reads "no error" as SUCCESS. In this harness that produces an
 * always-succeeds script standing in for an authenticated one, which is a test
 * that cannot fail. Refuse at the load boundary instead: before this check the
 * mismatch surfaced only as an opaque `→ undefined` evaluation failure a few
 * hundred milliseconds into the emulated submission.
 */
export const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): string => {
  const found = requireBlueprintValidator(blueprint, title);
  const declaredParameters = declaredParametersOf(found);
  if (declaredParameters.length !== 0) {
    throw new Error(
      `${title} declares ${declaredParameters.length} parameter(s) but this loader deploys compiledCode bare — declared: ${describeDeclaredParameters(declaredParameters)}. An unapplied declared parameter deploys an always-succeeds script; load it with applyCompiledScript instead of widening this zero-arity door (#610).`,
    );
  }
  return found.compiledCode;
};

/**
 * The parameter-applying door (#610), and the only permitted caller of
 * `applyParamsToScript` in this harness.
 *
 * `applyParamsToScript` applies whatever list it is handed, positionally, and
 * never checks it against the script's own declared arity: too few terms is the
 * silent always-succeeds shape described above, too many is a well-formed
 * script with a hash that matches nothing. Both are refused here, against the
 * blueprint's own declaration, for every validator this harness deploys.
 */
export const applyCompiledScript = (
  blueprint: Blueprint,
  title: string,
  params: readonly Data[],
): string => {
  const found = requireBlueprintValidator(blueprint, title);
  const declaredParameters = declaredParametersOf(found);
  if (declaredParameters.length !== params.length) {
    throw new Error(
      `${title} declares ${declaredParameters.length} parameter(s) but ${params.length} were applied — declared: ${describeDeclaredParameters(declaredParameters)}. Under-application deploys an always-succeeds script and over-application deploys an unusable script hash; apply exactly the declared parameters (#610).`,
    );
  }
  const cacheKey = appliedScriptCacheKey(found.compiledCode, params);
  const cached = appliedScriptCache.get(cacheKey);
  if (cached !== undefined) {
    return cached;
  }
  const applied = applyParamsToScript(found.compiledCode, [...params]);
  appliedScriptCache.set(cacheKey, applied);
  return applied;
};

/**
 * `applyParamsToScript` is pure — the applied script is a function of nothing
 * but the compiled code and the CBOR of the parameters — and it is the
 * dominant fixed cost of every emulator journey (~12–14 s of contract build
 * per test). Memoizing on the exact inputs cannot change a deployed byte: a
 * cache hit is a proof the inputs were identical. The #610 arity guard above
 * runs before the lookup on every call, so under-/over-application still
 * fails closed. The cache is per-process, and this suite runs one fresh
 * process per test file, so entries never outlive a file.
 */
const appliedScriptCache = new Map<string, string>();

const appliedScriptCacheKey = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  createHash("sha256")
    .update(
      `${compiledCode}|${params.map((param) => Data.to(param)).join("|")}`,
    )
    .digest("hex");
