import { createHash } from "node:crypto";
import { createReadStream, existsSync } from "node:fs";
import { resolve } from "node:path";

import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  resolveArchitectureGGateConfig,
} from "./mpf-architecture-g-gate-config.mjs";

const option = (name, fallback) =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;

export const gateConfig = resolveArchitectureGGateConfig({
  mode: option("mode", "50k"),
  profile: option("profile", "formal"),
  runs: option("runs", undefined),
  transactions: option("transactions", undefined),
});

export const {
  mode,
  profile,
  runs,
  transactions: transactionCount,
} = gateConfig;

export const phase1FormalBindingPath = option(
  "phase1-formal-binding",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_PATH ?? "",
).trim();

export const phase1FormalBindingSha256 = option(
  "phase1-formal-binding-sha256",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_SHA256 ?? "",
).trim();

export const phase1FormalBinding =
  captureArchitectureGPhase1FormalBindingIdentity({
    bindingPath: phase1FormalBindingPath,
    bindingSha256: phase1FormalBindingSha256,
  });

export const expectedRuntimeVersion = option(
  "runtime-version",
  process.env.MPF_ARCH_G_RUNTIME_VERSION ?? "",
).trim();

export const expectedRuntimeExecutableSha256 = option(
  "runtime-executable-sha256",
  process.env.MPF_ARCH_G_RUNTIME_EXECUTABLE_SHA256 ?? "",
).trim();

export const runtimeIdentity = captureArchitectureGRuntimeIdentity({
  expectedVersion: expectedRuntimeVersion,
  expectedExecutableSha256: expectedRuntimeExecutableSha256,
});

export const prepareCorpusOnly =
  option("prepare-corpus-only", "false") === "true";

const fixtureRoot = option(
  "fixture-root",
  process.env.MPF_ARCH_G_FIXTURE_ROOT ?? "",
).trim();

if (fixtureRoot.length === 0) {
  throw new Error(
    "Set --fixture-root or MPF_ARCH_G_FIXTURE_ROOT to a fresh durable fixture directory",
  );
}

export const cpuSet = option(
  "cpuset",
  process.env.MPF_ARCH_G_CPUSET ?? "",
).trim();

if (cpuSet.length === 0) {
  throw new Error(
    "Set --cpuset or MPF_ARCH_G_CPUSET for reproducible CPU affinity",
  );
}

export const corpusPath = option(
  "corpus",
  process.env.MPF_ARCH_G_CORPUS_PATH ?? "",
).trim();

export const corpusManifestPath = option(
  "corpus-manifest",
  process.env.MPF_ARCH_G_CORPUS_MANIFEST_PATH ??
    (corpusPath.length === 0 ? "" : `${corpusPath}.manifest.json`),
).trim();

export const corpusSliceId = option(
  "corpus-slice-id",
  process.env.MPF_ARCH_G_CORPUS_SLICE_ID ?? "",
).trim();

export const corpusIndexPath = option(
  "corpus-index",
  process.env.MPF_ARCH_G_CORPUS_INDEX_PATH ??
    (corpusPath.length === 0 ? "" : `${corpusPath}.index.ndjson`),
).trim();

export const corpusVerificationPath = option(
  "corpus-verification",
  process.env.MPF_ARCH_G_CORPUS_VERIFICATION_PATH ?? "",
).trim();

export const walletsDirectory = option(
  "wallets-dir",
  process.env.MPF_ARCH_G_WALLETS_DIR ?? "",
).trim();

const corpusInputs = [
  corpusPath,
  corpusManifestPath,
  corpusIndexPath,
  corpusVerificationPath,
  corpusSliceId,
  walletsDirectory,
];

if (gateConfig.formal && corpusInputs.some((value) => value.length === 0)) {
  throw new Error(
    "A formal gate requires --corpus, --corpus-manifest, --corpus-index, --corpus-verification, --corpus-slice-id, and --wallets-dir",
  );
}

if (
  !gateConfig.formal &&
  corpusInputs.some((value) => value.length > 0) &&
  corpusInputs.some((value) => value.length === 0)
) {
  throw new Error(
    "A smoke gate must supply either every canonical corpus input or none of them",
  );
}

export const usesCanonicalCorpus = corpusPath.length > 0;

export const fixtures = new Map(
  (mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000]).map(
    (utxos) => [
      utxos,
      resolve(
        option(
          `fixture-${utxos.toString()}`,
          resolve(fixtureRoot, `utxos-${utxos.toString()}-level`),
        ),
      ),
    ],
  ),
);

export const fixtureCreations = new Map(
  [...fixtures.keys()].map((utxos) => [
    utxos,
    resolve(
      option(
        `fixture-creation-${utxos.toString()}`,
        resolve(fixtureRoot, "..", `fixture-create-${utxos.toString()}.json`),
      ),
    ),
  ]),
);

export const binaryPath = resolve(
  option(
    "binary",
    "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  ),
);

export const probePath = resolve(option("probe", "dist/mpf-engine-probe.js"));

for (const path of usesCanonicalCorpus
  ? [
      corpusPath,
      corpusManifestPath,
      corpusIndexPath,
      corpusVerificationPath,
      walletsDirectory,
    ]
  : []) {
  if (!existsSync(path)) {
    throw new Error(`Missing Architecture G gate input: ${path}`);
  }
}

const timestamp = new Date().toISOString().replaceAll(/[-:.]/g, "");

export const outPath = resolve(
  option(
    "out",
    `logs/phase-3-architecture-g-${mode}-${timestamp}/summary.json`,
  ),
);

export const sha256File = async (path) => {
  const hash = createHash("sha256");
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  return hash.digest("hex");
};
