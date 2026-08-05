#!/usr/bin/env node

// Issue #521. Stock `aiken v1.1.22` keys every generated expect-decoder as
// `__expect_{local_type_name}_{variant}`, with no module path, so two distinct
// types that share a local name across modules share ONE generated decoder --
// whichever is requested first wins. Upstream fixed that in `cb6105cb`, which
// the patched fork carries. `aiken check` cannot see the defect class at all:
// it compiles every test as its own program, so colliding types never co-occur
// there. The only surface that exposes it is the blueprint, and the only cheap
// way to expose it is to build the blueprint with both compilers and require
// that every compiled validator agrees byte for byte.
//
// The two preambles necessarily disagree (each compiler stamps its own
// version), so the preamble is excluded from the comparison -- and the script
// asserts the two versions differ, otherwise a misconfigured invocation could
// compare a binary against itself and pass vacuously.

import { spawnSync } from "node:child_process";
import { cpSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const projectDirectory = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const stockBinary = process.env.MIDGARD_STOCK_AIKEN_BIN ?? "aiken";
const forkBinary = process.env.MIDGARD_FORK_AIKEN_BIN;
const environment = process.env.MIDGARD_AIKEN_ENV;

const fail = (message) => {
  console.error(message);
  process.exit(1);
};

if (forkBinary === undefined || forkBinary === "") {
  fail(
    "MIDGARD_FORK_AIKEN_BIN must name the patched Aiken binary; " +
      "MIDGARD_STOCK_AIKEN_BIN defaults to `aiken`.",
  );
}
if (environment !== undefined && !/^[a-z0-9_-]+$/u.test(environment)) {
  fail("MIDGARD_AIKEN_ENV contains an invalid environment name");
}

const excluded = new Set(["build", "plutus.json", ".git"]);

const buildWith = (binary, label) => {
  const workDirectory = mkdtempSync(join(tmpdir(), `midgard-blueprint-${label}-`));
  const projectCopy = join(workDirectory, "aiken");
  cpSync(projectDirectory, projectCopy, {
    recursive: true,
    filter: (source) =>
      !(dirname(source) === projectDirectory && excluded.has(basename(source))),
  });
  const versionResult = spawnSync(binary, ["--version"], { encoding: "utf8" });
  if (versionResult.status !== 0) {
    rmSync(workDirectory, { force: true, recursive: true });
    fail(`${label}: \`${binary} --version\` failed`);
  }
  const version = versionResult.stdout.trim();
  const buildResult = spawnSync(
    binary,
    ["build", ...(environment === undefined ? [] : ["--env", environment])],
    { cwd: projectCopy, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
  );
  if (buildResult.status !== 0) {
    process.stderr.write(buildResult.stdout ?? "");
    process.stderr.write(buildResult.stderr ?? "");
    rmSync(workDirectory, { force: true, recursive: true });
    fail(`${label}: \`${binary} build\` failed`);
  }
  const blueprint = JSON.parse(
    readFileSync(join(projectCopy, "plutus.json"), "utf8"),
  );
  rmSync(workDirectory, { force: true, recursive: true });
  return { blueprint, version };
};

// A duplicate local type name across modules is the only known cause of a
// divergence here, so name the suspects instead of leaving a bare hash diff.
// Sorted by how few modules share the name: a name shared by two modules is
// almost certainly an accident, while `Datum`/`SpendRedeemer` are shared by
// scores of modules as a repo-wide convention and are useless as suspects.
const MAX_SUSPECTS = 10;
const MAX_MODULES_PER_SUSPECT = 6;
const duplicateTypeNames = (blueprint) => {
  const modulesByTypeName = new Map();
  for (const key of Object.keys(blueprint.definitions ?? {})) {
    // Skip generic instantiations such as `List<midgard/cek_data_v1/NodeV1>`;
    // they are not separately declared types.
    if (key.includes("<") || key.includes(">")) {
      continue;
    }
    const separator = key.lastIndexOf("/");
    if (separator < 0) {
      continue;
    }
    const typeName = key.slice(separator + 1);
    const modules = modulesByTypeName.get(typeName) ?? new Set();
    modules.add(key.slice(0, separator));
    modulesByTypeName.set(typeName, modules);
  }
  return [...modulesByTypeName]
    .filter(([, modules]) => modules.size > 1)
    .sort(
      ([leftName, left], [rightName, right]) =>
        left.size - right.size || leftName.localeCompare(rightName),
    )
    .slice(0, MAX_SUSPECTS)
    .map(([typeName, modules]) => {
      const sorted = [...modules].sort();
      const shown = sorted.slice(0, MAX_MODULES_PER_SUSPECT).join(", ");
      const hidden = sorted.length - MAX_MODULES_PER_SUSPECT;
      return `${typeName} (${sorted.length} modules): ${shown}${
        hidden > 0 ? `, +${hidden} more` : ""
      }`;
    });
};

const stock = buildWith(stockBinary, "released");
const fork = buildWith(forkBinary, "patched");

console.log(`released compiler: ${stock.version}`);
console.log(`patched compiler:  ${fork.version}`);
if (stock.version === fork.version) {
  fail(
    "Both binaries report the same version, so this comparison proves " +
      "nothing. Point MIDGARD_STOCK_AIKEN_BIN and MIDGARD_FORK_AIKEN_BIN at " +
      "the two different compilers.",
  );
}

const problems = [];
const stockValidators = new Map(
  stock.blueprint.validators.map((validator) => [validator.title, validator]),
);
const forkValidators = new Map(
  fork.blueprint.validators.map((validator) => [validator.title, validator]),
);

console.log(
  `validators: released ${stockValidators.size}, patched ${forkValidators.size}`,
);
for (const title of stockValidators.keys()) {
  if (!forkValidators.has(title)) {
    problems.push(`validator only in the released blueprint: ${title}`);
  }
}
for (const title of forkValidators.keys()) {
  if (!stockValidators.has(title)) {
    problems.push(`validator only in the patched blueprint: ${title}`);
  }
}

const differing = [];
for (const [title, stockValidator] of stockValidators) {
  const forkValidator = forkValidators.get(title);
  if (forkValidator === undefined) {
    continue;
  }
  if (
    stockValidator.compiledCode !== forkValidator.compiledCode ||
    stockValidator.hash !== forkValidator.hash
  ) {
    differing.push(
      `  ${title}: released ${
        (stockValidator.compiledCode?.length ?? 0) / 2
      } bytes ${stockValidator.hash ?? "-"} != patched ${
        (forkValidator.compiledCode?.length ?? 0) / 2
      } bytes ${forkValidator.hash ?? "-"}`,
    );
  }
}
if (differing.length > 0) {
  problems.push(
    `${differing.length} compiled validator(s) differ between compilers:\n${differing.join("\n")}`,
  );
}

const canonical = (value) => {
  if (Array.isArray(value)) {
    return `[${value.map(canonical).join(",")}]`;
  }
  if (value !== null && typeof value === "object") {
    return `{${Object.keys(value)
      .sort()
      .map((key) => `${JSON.stringify(key)}:${canonical(value[key])}`)
      .join(",")}}`;
  }
  return JSON.stringify(value);
};
if (
  canonical(stock.blueprint.definitions ?? {}) !==
  canonical(fork.blueprint.definitions ?? {})
) {
  problems.push("blueprint `definitions` differ between compilers");
}

if (problems.length > 0) {
  console.error(problems.join("\n"));
  const duplicates = duplicateTypeNames(fork.blueprint);
  console.error(
    "\nThe released and patched Aiken compilers disagree on the blueprint.\n" +
      "The known cause is a DUPLICATE TYPE NAME shared by two modules: the\n" +
      "released compiler keys generated expect-decoders by local type name\n" +
      "only, so both types share one decoder and at least one of them gets a\n" +
      "decoder that is wrong in both directions (see issue #521). Rename the\n" +
      "later-defined type so both compilers agree by construction.",
  );
  if (duplicates.length > 0) {
    console.error(
      "\nLikeliest suspects -- local type names defined in more than one " +
        `module, fewest-sharers first:\n  ${duplicates.join("\n  ")}`,
    );
  }
  process.exit(1);
}

console.log(
  `OK: ${stockValidators.size} validators, all compiled bytes and hashes ` +
    "identical across both compilers; `definitions` identical.",
);
