/**
 * The plumbing shared by every **cross-language golden channel** generator in
 * this package.
 *
 * A channel is one generator, one JSON fixture recomputed by a vitest suite,
 * and one Aiken module recomputed by the Aiken producers under the fork runner.
 * Two of them exist today — the shared field-access surface (#568) and the
 * per-field item fan-out (#569) — and they differ only in what they compute.
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
