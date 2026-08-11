/**
 * The plumbing shared by every **cross-language golden channel** generator in
 * this package.
 *
 * A channel is one generator, one JSON fixture recomputed by a vitest suite,
 * and one Aiken module recomputed by the Aiken producers under the fork runner.
 * Three of them exist today — the shared field-access surface (#568), the
 * per-field item fan-out (#569) and the §8.6/§8.8 carriage wire surface (#574,
 * which lives in `@al-ft/midgard-sdk` because its producer is `Data.to` against
 * the SDK schemas) — and they differ only in what they compute.
 * Everything around that computation is identical: the same `--check` contract,
 * the same "regenerate, never hand-edit" emission, the same trip through
 * `aiken fmt` so a formatter change cannot show up as a spurious diff.
 *
 * That sameness is the point rather than an accident. `--check` is what closes
 * the regeneration channel: if a drifting producer could be papered over by
 * re-running the generator, the goldens would prove nothing. So the check path
 * is one implementation, used by both, and a channel added later gets it by
 * construction instead of by careful copying.
 *
 * Nothing here knows what a vector is.
 */

import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";

/** Bytes as lowercase hex — the only spelling any fixture in this package uses. */
export const hex = (value) => Buffer.from(value).toString("hex");

/** The inverse: a hex string as bytes. */
export const bytes = (hexValue) => Buffer.from(hexValue, "hex");

/** An Aiken byte-array literal: `#"…"`. */
export const aikenBytes = (hexValue) => `#"${hexValue}"`;

/**
 * The one argument every channel generator takes. `--check` asserts the
 * checked-in artifacts are exactly what the twins produce today; bare
 * regenerates them. Anything else is a usage error, because a generator that
 * quietly ignored an unrecognised flag could be invoked in CI as `--check` and
 * silently rewrite instead.
 */
export const parseGoldenChannelArguments = (usage) => {
  const commandArguments = process.argv.slice(2);
  if (
    commandArguments.length > 1 ||
    (commandArguments.length === 1 && commandArguments[0] !== "--check")
  ) {
    console.error(usage);
    process.exit(2);
  }
  return { checkOnly: commandArguments.length === 1 };
};

/**
 * Runs the generated Aiken source through `aiken fmt` before it is compared or
 * written, so the checked-in module is formatted exactly as a contributor's
 * editor would leave it and `--check` never trips on whitespace the generator
 * happened to emit differently.
 *
 * The formatter rewrites in place, so it runs against a throwaway copy in a
 * temp directory — never the checked-in file, which `--check` still has to be
 * able to read as it stands.
 */
export const formatAikenSource = ({
  source,
  fileName,
  repositoryRoot,
  tmpPrefix,
}) => {
  const directory = mkdtempSync(join(tmpdir(), tmpPrefix));
  const target = join(directory, fileName);
  const aikenBinary = process.env.MIDGARD_AIKEN_BIN ?? "aiken";
  try {
    writeFileSync(target, source, "utf8");
    const result = spawnSync(aikenBinary, ["fmt", target], {
      cwd: join(repositoryRoot, "onchain/aiken"),
      encoding: "utf8",
    });
    if (result.status !== 0) {
      throw new Error(
        `Aiken formatter (${aikenBinary}) failed: ${result.error?.message ?? result.stderr.trim()}`,
      );
    }
    return readFileSync(target, "utf8");
  } finally {
    rmSync(directory, { force: true, recursive: true });
  }
};

/**
 * Rebinds named top-level `const` declarations in a **hand-written** Aiken test
 * module, leaving everything else in the file untouched.
 *
 * The four whole-module channels above emit their `.ak` file from nothing, so
 * they never need this. The seeds #588 closes are different in kind: their
 * constants are a handful of generated values embedded in a module whose tests,
 * comments and fuzzers are written and maintained by hand. Regenerating the
 * whole module would throw that away, so the generator has to edit in place.
 *
 * Binding is by **name**, never by value. The retired value-keyed spelling
 * (`source.replaceAll(staleHex, freshHex)`) silently rewrote every other
 * constant that happened to share the stale bytes — and shared bytes are the
 * normal case here, because sibling fixtures commit the same empty-field hash.
 * A name that is absent is an error rather than a no-op: a renamed constant
 * means the generator has lost the seed it claims to own, which is exactly the
 * drift the channel exists to catch.
 *
 * Values are either `Uint8Array`/`Buffer` (emitted as `#"…"`) or a `number`
 * (emitted as a decimal literal), matching the two literal forms these modules
 * use. Re-running against already-fresh source is a no-op, so `--check` and a
 * second write see byte-identical output.
 *
 * **Why this does not then run `aiken fmt`, and what it owes the formatter
 * anyway.** The whole-module channels run it, and must: they emit source no human
 * has laid out. Here the module is hand-written and only its constants are ours,
 * so formatting on the way through would rewrite code this generator does not
 * own.
 *
 * That is *not* an exemption from the formatter's judgement. CI mandates
 * stock-formatter cleanliness over every tracked `.ak` file: the Aiken workflow
 * runs stock `aiken fmt` across `git ls-files '*.ak'`, normalises the formatter's
 * own trailing-space artifact, and fails on any remaining diff. So a rebound
 * declaration that stock `aiken fmt` would reflow breaks the gate just as surely
 * as a hand-written one. The obligation this function carries is therefore:
 * **emit output that survives stock `aiken fmt` unchanged** — which is what the
 * 80-column contract below is for. Aiken keeps a declaration on one line while it
 * fits in 80 columns and otherwise breaks after the `=` with a two-space indent,
 * so reproducing that layout exactly is precisely what the formatter would have
 * done to the lines this function touches.
 *
 * What agents must not run is the **fork's** formatter. The patched fork exists to
 * execute the test suite, and its `aiken fmt` v1.1.23 mangles the wide tuple
 * destructuring in `native-tx-v1.test.ak` into `let` plus trailing whitespace —
 * so routing anything through the fork formatter commits a formatter bug as a
 * generated diff, and disagrees with the stock binary that actually judges CI.
 * Stock `aiken fmt` is the authority; the fork is for tests only.
 */
const AIKEN_MAX_LINE_COLUMNS = 80;

export const rebindAikenConstants = ({ source, constants }) => {
  let rebound = source;
  for (const [name, value] of Object.entries(constants)) {
    if (typeof value === "number" && !Number.isInteger(value)) {
      throw new Error(`Aiken constant ${name} must be an integer, not ${value}`);
    }
    if (typeof value !== "number" && !(value instanceof Uint8Array)) {
      throw new Error(
        `Aiken constant ${name} must be an integer or byte value`,
      );
    }
    const literal =
      typeof value === "number" ? String(value) : aikenBytes(hex(value));
    const declaration = `const ${name} =`;
    // The integer alternative accepts Aiken's digit separators (`5_000_000`).
    // Matching bare `\d+` would consume only the leading group and leave the
    // rest of the old number behind as garbage after the new literal — a
    // corruption that a `--check` run would report as staleness rather than as
    // the malformed source it is.
    const pattern = new RegExp(
      String.raw`^${declaration}\s*(?:#"[0-9a-f]*"|-?\d[\d_]*)`,
      "mu",
    );
    if (!pattern.test(rebound)) {
      // Two different failures reach here and they want different repairs: the
      // constant is gone (the generator has lost a seed it claims to own, so a
      // name has to be reconciled) versus the constant is present but spelled in
      // a literal form this rebinder does not parse (nothing is lost; the
      // pattern above has to learn the form). Reporting both as "missing" sends
      // the reader hunting for a name that is sitting right there, so name the
      // line instead.
      const declarationPattern = new RegExp(String.raw`^${declaration}`, "mu");
      const declarationMatch = declarationPattern.exec(rebound);
      if (declarationMatch === null) {
        throw new Error(`missing Aiken constant ${name}`);
      }
      const line = rebound.slice(0, declarationMatch.index).split("\n").length;
      throw new Error(
        `unrecognized literal form for Aiken constant ${name} at line ${line}`,
      );
    }
    const laidOut =
      `${declaration} ${literal}`.length <= AIKEN_MAX_LINE_COLUMNS
        ? `${declaration} ${literal}`
        : `${declaration}\n  ${literal}`;
    rebound = rebound.replace(pattern, laidOut);
  }
  return rebound;
};

/**
 * The emission half of the `--check` contract, bound once per run.
 *
 * Under `--check` a missing or differing artifact throws — it is never
 * repaired, because repairing it is exactly the failure mode the channel
 * exists to prevent.
 */
export const goldenChannelEmitter = ({ repositoryRoot, checkOnly }) => (
  target,
  expected,
) => {
  const relativePath = relative(repositoryRoot, target);
  if (checkOnly) {
    let actual;
    try {
      actual = readFileSync(target, "utf8");
    } catch {
      throw new Error(`missing generated artifact: ${relativePath}`);
    }
    if (actual !== expected) {
      throw new Error(`stale generated artifact: ${relativePath}`);
    }
    console.log(`checked ${relativePath}`);
    return;
  }
  mkdirSync(dirname(target), { recursive: true });
  writeFileSync(target, expected, "utf8");
  console.log(`wrote ${relativePath}`);
};
