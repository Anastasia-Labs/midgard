import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { midgardCekKnownBuiltinFailure } from "../src/cek-builtin.known-failure.js";
import * as corpus from "./fixtures/known-builtin-failure-conformance.cases.mjs";

/**
 * The TypeScript half of the known builtin failure conformance channel.
 *
 * The off-chain twin `midgardCekKnownBuiltinFailure` must match the on-chain
 * rule `known_builtin_failure_v1`. The rows are rebuilt here by running the
 * twin from `src/` over the corpus and compared with the checked-in fixture;
 * the generated Aiken module
 * (`onchain/aiken/lib/midgard/known-builtin-failure-conformance-golden.test.ak`)
 * holds the on-chain predicate to the same verdicts. Regenerate both with
 * `pnpm run fixtures:known-builtin-failure-conformance:sync`.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/known-builtin-failure-conformance.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly corpus.KnownBuiltinFailureConformanceRow[] };

const rows = corpus.buildKnownBuiltinFailureConformanceRows(
  midgardCekKnownBuiltinFailure,
);

const verdictOf = (tag: number, name: string) => {
  const matching = rows.filter((row) => row.tag === tag && row.name === name);
  expect(matching, `tag ${tag} ${name}`).toHaveLength(1);
  return matching[0]!.verdict;
};

/** Pairs of cases on each side of a limit: [tag, case name, verdict]. */
const LIMITS: readonly (readonly [number, string, string])[] = [
  [3, "compacted dividend of 4095 words by 0", "known"],
  [3, "compacted dividend of 4096 words by 0", "not-known"],
  [6, "compacted dividend of 4095 words by 0", "known"],
  [6, "compacted dividend of 4096 words by 0", "not-known"],
  [11, "cons -1", "known"],
  [11, "cons 0", "not-known"],
  [11, "cons 255", "not-known"],
  [11, "cons 256", "known"],
  [12, "slice from -9223372036854775809 length 0", "known"],
  [12, "slice from -9223372036854775808 length 0", "not-known"],
  [12, "slice from 9223372036854775807 length 0", "not-known"],
  [12, "slice from 9223372036854775808 length 0", "known"],
  [14, "index -1", "known"],
  [14, "index 2", "not-known"],
  [14, "index 3", "known"],
  [25, "UTF-8 ASCII", "not-known"],
  [25, "UTF-8 truncated c2", "known"],
  [33, "empty integer list", "known"],
  [33, "one-integer list", "not-known"],
  [60, "domain 255 bytes", "not-known"],
  [60, "domain 256 bytes", "known"],
  [67, "domain 255 bytes", "not-known"],
  [67, "domain 256 bytes", "known"],
  [52, "ECDSA key 02 generator, s=n-1", "not-known"],
  [52, "ECDSA key 02 generator, s=n", "known"],
  [59, "G1 x=p-1", "known"],
  [59, "G1 infinity", "not-known"],
  [66, "G2 infinity", "not-known"],
  [66, "G2 x0=p", "known"],
  [73, "size 8192, value 0", "not-known"],
  [73, "size 8193, value 0", "known"],
  [73, "size 0, value of 8192 bytes", "not-known"],
  [73, "size 0, value of 8193 bytes", "known"],
  [73, "size 2, value 65535", "not-known"],
  [73, "size 2, value 65536", "known"],
  [79, "read bit 15", "not-known"],
  [79, "read bit 16", "known"],
  [80, "write index 0 into 4096 bytes", "not-known"],
  [80, "write index 0 into 4097 bytes", "known"],
  [81, "replicate 0 8192 times", "not-known"],
  [81, "replicate 0 8193 times", "known"],
  [81, "replicate 255 0 times", "not-known"],
  [81, "replicate 256 0 times", "known"],
  [82, "by 9223372036854775807", "not-known"],
  [82, "by 9223372036854775808", "known"],
  [83, "by -9223372036854775808", "not-known"],
  [83, "by -9223372036854775809", "known"],
];

describe("known builtin failure conformance vectors", () => {
  it("declares exactly the exports the corpus module has", () => {
    expect(Object.keys(corpus).sort()).toEqual([
      "KNOWN_BUILTIN_FAILURE_TAGS",
      "VERDICTS",
      "buildKnownBuiltinFailureConformanceRows",
      "encodeConformanceArgument",
      "knownBuiltinFailureConformanceCases",
    ]);
  });

  it("match what the twin in src/ decides today", () => {
    expect(rows).toEqual(golden.rows);
  });

  it("give every tag of the known set every verdict", () => {
    for (const tag of corpus.KNOWN_BUILTIN_FAILURE_TAGS) {
      const verdicts = new Set(
        rows.filter((row) => row.tag === tag).map((row) => row.verdict),
      );
      expect([...verdicts].sort(), `tag ${tag}`).toEqual(
        [...corpus.VERDICTS].sort(),
      );
    }
  });

  it("never call a failure known outside the known set", () => {
    const outside = rows.filter(
      (row) => !corpus.KNOWN_BUILTIN_FAILURE_TAGS.includes(row.tag),
    );
    expect(outside.length).toBeGreaterThan(0);
    for (const row of outside) expect(row.verdict, row.name).toBe("not-known");
  });

  it("present every argument kind to every tag of the known set", () => {
    for (const tag of corpus.KNOWN_BUILTIN_FAILURE_TAGS) {
      const kinds = new Set(
        rows
          .filter((row) => row.tag === tag)
          .flatMap((row) => row.arguments.map((argument) => argument.kind)),
      );
      expect([...kinds].sort(), `tag ${tag}`).toEqual([
        "blsMillerLoop",
        "constant",
        "opaque",
        "semanticConstant",
      ]);
    }
  });

  it("straddle each limit with a case on either side", () => {
    for (const [tag, name, verdict] of LIMITS) {
      expect(verdictOf(tag, name), `tag ${tag} ${name}`).toBe(verdict);
    }
  });
});
