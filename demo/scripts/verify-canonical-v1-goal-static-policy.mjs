#!/usr/bin/env node

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const workspacePackages = [
  "demo",
  "demo/lucid-midgard",
  "demo/midgard-core",
  "demo/midgard-fault-proofs",
  "demo/midgard-validation",
  "demo/midgard-node",
  "demo/midgard-sdk",
  "demo/midgard-watcher",
  "demo/da-committee-node",
];
const forbiddenCommandFragments = [
  "--passWithNoTests",
  "|| true",
  "&& true",
  "; true",
];
const commandViolations = [];

for (const packageDirectory of workspacePackages) {
  const packagePath = resolve(repoRoot, packageDirectory, "package.json");
  const manifest = JSON.parse(readFileSync(packagePath, "utf8"));
  for (const [name, command] of Object.entries(manifest.scripts ?? {})) {
    if (typeof command !== "string") {
      throw new Error(`${packageDirectory} script ${name} is not a string`);
    }
    for (const fragment of forbiddenCommandFragments) {
      if (command.includes(fragment)) {
        commandViolations.push(`${packageDirectory}:${name}:${fragment}`);
      }
    }
  }
}
if (commandViolations.length > 0) {
  throw new Error(
    `active workspace commands contain forbidden success shortcuts: ${commandViolations.join(", ")}`,
  );
}

const aikenToml = readFileSync(
  resolve(repoRoot, "onchain/aiken/aiken.toml"),
  "utf8",
);
const declaredCompiler = aikenToml.match(/^compiler\s*=\s*"(v[^"]+)"$/m)?.[1];
if (declaredCompiler === undefined) {
  throw new Error("aiken.toml does not declare an exact compiler");
}
const aiken = spawnSync("aiken", ["--version"], {
  cwd: resolve(repoRoot, "onchain/aiken"),
  encoding: "utf8",
  stdio: ["ignore", "pipe", "pipe"],
});
if (aiken.status !== 0) {
  throw new Error(
    `unable to execute declared Aiken compiler: ${aiken.stderr.trim()}`,
  );
}
const actualCompiler = aiken.stdout.trim().split("+")[0]?.split(" ").at(-1);
if (actualCompiler !== declaredCompiler) {
  throw new Error(
    `Aiken compiler mismatch: declared ${declaredCompiler}, received ${aiken.stdout.trim()}`,
  );
}

const forbiddenWholeItemBindings = [
  {
    path: "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
    pattern: /\bTransactionFieldPreimageWitness\b/,
  },
  {
    path: "onchain/aiken/lib/midgard/validation-machine-v1.ak",
    pattern: /\bTransactionFieldPreimageWitness\b/,
  },
  {
    path: "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
    pattern:
      /CekOutputContextItemWitness:\s*Data\.Object\(\{[^}]*\boutput_cbor:/su,
  },
  {
    path: "onchain/aiken/lib/midgard/validation-machine-v1.ak",
    pattern: /CekOutputContextItemWitness\s*\{[^}]*\boutput_cbor:/su,
  },
  {
    path: "demo/midgard-validation/src/validation-machine.ts",
    pattern: /kind:\s*"cekOutputContextItem";[^}]*\boutputCbor:/su,
  },
  {
    path: "onchain/aiken/lib/midgard/validation-machine-v1.ak",
    pattern:
      /script_context_v1\.(?:tx_out_data_v1|tx_out_summary_v1|tx_in_info_data_v1|tx_in_info_summary_v1|prepend_resolved_tx_in_info_v1|prepend_output_v1|spend_datum_summary_v1|cardano_spend_script_info_summary_v1)\s*\(/u,
  },
];
const wholeItemViolations = forbiddenWholeItemBindings
  .filter(({ path, pattern }) =>
    pattern.test(readFileSync(resolve(repoRoot, path), "utf8")),
  )
  .map(({ path }) => path);
if (wholeItemViolations.length > 0) {
  throw new Error(
    `forbidden whole-field validation ABI remains active: ${wholeItemViolations.join(", ")}`,
  );
}

process.stdout.write(
  `${JSON.stringify({
    status: "PASS",
    workspacePackages: workspacePackages.length,
    declaredCompiler,
    forbiddenWholeItemChecks: forbiddenWholeItemBindings.length,
    forbiddenWholeItemBindings: 0,
  })}\n`,
);
