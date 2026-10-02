import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/**
 * The deployment environment pins the script hashes of the MPF verifiers that
 * other validators delegate to by withdrawal. Those verifiers are compiled in
 * this project, so every pin must equal the hash the fresh blueprint gives the
 * validator it names. A stale pin makes every delegating validator demand a
 * withdrawal no deployed script can satisfy.
 *
 * The blueprint guard in `vitest.config.ts` refuses to run this suite on a
 * blueprint older than its sources, so a pass here is against a fresh build.
 */

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");

const pinnedVerifiers = [
  ["plutarch_phas_validator_hash", "phas.membership.withdraw"],
  ["plutarch_pexcludes_validator_hash", "pexcludes.exclusion.withdraw"],
  ["mpf_chunked_verify_validator_hash", "mpf_chunked_verify.verify.withdraw"],
] as const;

type Blueprint = {
  readonly validators: readonly {
    readonly title: string;
    readonly hash: string;
  }[];
};

const blueprint = JSON.parse(
  readFileSync(resolve(repoRoot, "onchain/aiken/plutus.json"), "utf8"),
) as Blueprint;

const envTemplate = readFileSync(
  resolve(repoRoot, "config/deployments/env.ak.template"),
  "utf8",
);

const pinIn = (source: string, name: string): string => {
  const matches = [
    ...source.matchAll(
      new RegExp(
        `^pub const ${name}: ScriptHash =\\s*#"([0-9a-f]{56})"`,
        "gmu",
      ),
    ),
  ];
  expect(matches, `exactly one ${name} pin`).toHaveLength(1);
  return matches[0]![1]!;
};

const blueprintHashOf = (title: string): string => {
  const matches = blueprint.validators.filter(
    (validator) => validator.title === title,
  );
  expect(matches, `exactly one blueprint validator ${title}`).toHaveLength(1);
  return matches[0]!.hash;
};

describe("verifier hash pins", () => {
  it.each(pinnedVerifiers)(
    "pins %s to the blueprint hash of %s",
    (pin, title) => {
      expect(pinIn(envTemplate, pin)).toBe(blueprintHashOf(title));
    },
  );
});
