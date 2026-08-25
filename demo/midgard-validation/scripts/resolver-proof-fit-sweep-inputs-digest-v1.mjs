#!/usr/bin/env node

/**
 * #606/§8.3 C53 resolver proof-fit sweep — declared-input-closure digest.
 *
 * The committed sweep artifact
 * (`tests/fixtures/resolver-proof-fit-sweep-v1.generated.json`) is pinned by a
 * byte-identical regeneration (`generate-resolver-proof-fit-sweep-v1.mjs
 * --check`), which re-drives the whole emulator sweep and costs ~9 minutes.
 * That regeneration is the evidence; running it on every default `pnpm test`
 * pass is not what makes it evidence. The artifact can only drift if one of
 * its inputs changed, so the default lane instead recomputes a digest of the
 * DECLARED INPUT CLOSURE in milliseconds and fails red on any mismatch
 * against the committed stamp (`tests/fixtures/
 * resolver-proof-fit-sweep-v1.inputs-digest.json`), with the evidence lane
 * (`MIDGARD_VALIDATION_EVIDENCE=1`) as the printed remedy. This is the same
 * fail-closed staleness idiom as `demo/scripts/assert-midgard-core-dist-
 * current.mjs` / `demo/midgard-core/scripts/write-dist-source-digest.mjs`:
 * skipping the expensive check is safe exactly because the cheap check
 * cannot be skipped.
 *
 * The closure is deliberately a SUPERSET of what provably feeds the
 * generator: the generator entry points, the whole fault-proofs emulator
 * harness (`tests/support/`) and package source it imports, the sdk and core
 * package sources those import in turn, the blueprint the harness deploys
 * from, and the lockfile-resolved identities of the third-party components
 * whose behaviour the measurements pin (@lucid-evolution/*, the
 * cardano-multiplatform-lib family, and the merkle-patricia-forestry trie).
 * Over-declaring costs a spurious evidence re-run; under-declaring lets a
 * stale artifact pass the cheap gate — so ties break toward inclusion.
 * A git-diff heuristic is NOT a substitute: the digest is what makes
 * skipping provably safe.
 *
 * The stamp binds the artifact too (`artifactSha256`), so editing the
 * committed artifact by hand — without any input changing — also turns the
 * gate red.
 *
 * Falsifiability: `resolver-proof-fit-sweep-digest-gate-self-test.mjs`
 * mutates each declared closure member through the `transform` hook and
 * asserts the digest moves; a digest gate without that self-test is a gate
 * that cannot fail.
 *
 * usage:
 *   node scripts/resolver-proof-fit-sweep-inputs-digest-v1.mjs          # check
 *   node scripts/resolver-proof-fit-sweep-inputs-digest-v1.mjs --stamp  # rewrite stamp
 *
 * `--stamp` is only legitimate immediately after a green
 * `generate-resolver-proof-fit-sweep-v1.mjs --check` (the evidence lane runs
 * both in that order); stamping without the regeneration asserts currency
 * the evidence never established.
 */

import { createHash } from "node:crypto";
import { readdirSync, readFileSync, writeFileSync } from "node:fs";
import { relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const packageRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repoRoot = resolve(packageRoot, "../..");

export const RESOLVER_SWEEP_STAMP_SCHEMA_V1 =
  "resolver-proof-fit-sweep-inputs-digest/v1";

export const resolverSweepArtifactPath = () =>
  resolve(
    packageRoot,
    "tests/fixtures/resolver-proof-fit-sweep-v1.generated.json",
  );

export const resolverSweepStampPath = () =>
  resolve(
    packageRoot,
    "tests/fixtures/resolver-proof-fit-sweep-v1.inputs-digest.json",
  );

/**
 * Declared closure roots, repo-root-relative. `file` entries hash one file;
 * `tree` entries hash every file under the directory, sorted by relative
 * path; the `lockSelection` pseudo-entry hashes the pnpm-lock lines naming
 * the pinned third-party components (version AND integrity lines both match,
 * so a resolution change moves the digest even at an unchanged version
 * string).
 */
export const RESOLVER_SWEEP_INPUT_CLOSURE_V1 = [
  {
    kind: "file",
    path: "demo/midgard-validation/scripts/generate-resolver-proof-fit-sweep-v1.mjs",
  },
  {
    kind: "file",
    path: "demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate-v1.test.ts",
  },
  { kind: "tree", path: "demo/midgard-fault-proofs/tests/support" },
  { kind: "tree", path: "demo/midgard-fault-proofs/src" },
  { kind: "tree", path: "demo/midgard-sdk/src" },
  { kind: "tree", path: "demo/midgard-core/src" },
  { kind: "file", path: "onchain/aiken/plutus.json" },
  {
    kind: "lockSelection",
    path: "demo/pnpm-lock.yaml",
    pattern:
      /@lucid-evolution\/|cardano-multiplatform-lib|@aiken-lang\/merkle-patricia-forestry/,
  },
];

const listTreeFiles = (absoluteRoot) =>
  readdirSync(absoluteRoot, { recursive: true, withFileTypes: true })
    .filter((entry) => entry.isFile())
    .map((entry) => resolve(entry.parentPath, entry.name))
    .sort((left, right) => (left < right ? -1 : left > right ? 1 : 0));

/**
 * Computes the closure digest. `transform(label, buffer) -> buffer` is an
 * injection point for the self-test only: it lets the self-test prove each
 * labelled member participates (mutate it, watch the digest move) without
 * touching the working tree. Labels are the repo-root-relative paths, plus
 * `lock:<path>` for the lock selection.
 */
export const computeResolverSweepInputsDigestV1 = ({
  transform = (_label, buffer) => buffer,
} = {}) => {
  const hash = createHash("sha256");
  const feed = (label, buffer) => {
    hash.update("\0member\0");
    hash.update(label);
    hash.update("\0");
    hash.update(transform(label, buffer));
  };
  for (const member of RESOLVER_SWEEP_INPUT_CLOSURE_V1) {
    const absolute = resolve(repoRoot, member.path);
    if (member.kind === "file") {
      feed(member.path, readFileSync(absolute));
    } else if (member.kind === "tree") {
      for (const file of listTreeFiles(absolute)) {
        feed(
          `${member.path}/${relative(absolute, file)}`,
          readFileSync(file),
        );
      }
    } else {
      const selected = readFileSync(absolute, "utf8")
        .split("\n")
        .filter((line) => member.pattern.test(line))
        .join("\n");
      if (selected.length === 0) {
        throw new Error(
          `resolver sweep digest: the lock selection matched nothing in ${member.path} — the pinned dependencies were renamed or removed; update RESOLVER_SWEEP_INPUT_CLOSURE_V1 alongside that change`,
        );
      }
      feed(`lock:${member.path}`, Buffer.from(selected, "utf8"));
    }
  }
  return hash.digest("hex");
};

export const computeResolverSweepArtifactSha256V1 = () =>
  createHash("sha256")
    .update(readFileSync(resolverSweepArtifactPath()))
    .digest("hex");

export const readResolverSweepStampV1 = () => {
  let raw;
  try {
    raw = readFileSync(resolverSweepStampPath(), "utf8");
  } catch {
    return undefined;
  }
  return JSON.parse(raw);
};

export const stampResolverSweepInputsDigestV1 = () => {
  const stamp = {
    schema: RESOLVER_SWEEP_STAMP_SCHEMA_V1,
    inputsSha256: computeResolverSweepInputsDigestV1(),
    artifactSha256: computeResolverSweepArtifactSha256V1(),
  };
  writeFileSync(
    resolverSweepStampPath(),
    `${JSON.stringify(stamp, null, 2)}\n`,
    "utf8",
  );
  return stamp;
};

/**
 * The check the default lane runs: stamp present, schema known, closure
 * digest and artifact bytes both unmoved. Returns a list of failure
 * messages; empty means current.
 */
export const checkResolverSweepCurrencyV1 = () => {
  const failures = [];
  const stamp = readResolverSweepStampV1();
  if (stamp === undefined) {
    return [
      "the inputs-digest stamp (tests/fixtures/resolver-proof-fit-sweep-v1.inputs-digest.json) is missing — the artifact's currency has never been established",
    ];
  }
  if (stamp.schema !== RESOLVER_SWEEP_STAMP_SCHEMA_V1) {
    failures.push(
      `stamp schema is ${JSON.stringify(stamp.schema)}, expected ${RESOLVER_SWEEP_STAMP_SCHEMA_V1}`,
    );
  }
  const inputsSha256 = computeResolverSweepInputsDigestV1();
  if (stamp.inputsSha256 !== inputsSha256) {
    failures.push(
      `a declared input changed since the artifact was last regenerated (stamped ${stamp.inputsSha256}, closure is ${inputsSha256})`,
    );
  }
  const artifactSha256 = computeResolverSweepArtifactSha256V1();
  if (stamp.artifactSha256 !== artifactSha256) {
    failures.push(
      `the committed artifact's bytes moved without a regeneration (stamped ${stamp.artifactSha256}, file is ${artifactSha256})`,
    );
  }
  return failures;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) {
  if (process.argv.includes("--stamp")) {
    const stamp = stampResolverSweepInputsDigestV1();
    process.stdout.write(
      `resolver sweep inputs digest stamped: ${stamp.inputsSha256} (artifact ${stamp.artifactSha256.slice(0, 12)}…)\n`,
    );
  } else {
    const failures = checkResolverSweepCurrencyV1();
    if (failures.length > 0) {
      process.stderr.write(
        `resolver sweep digest gate FAILED:\n${failures.map((failure) => `  - ${failure}`).join("\n")}\n  fix: MIDGARD_VALIDATION_EVIDENCE=1 pnpm --filter @al-ft/midgard-validation run test:evidence\n`,
      );
      process.exit(1);
    }
    process.stdout.write("resolver sweep digest gate PASS\n");
  }
}
