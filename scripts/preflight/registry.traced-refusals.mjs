// The traced-refusal check: which changes can move a pinned refusal, and
// which pin files a local run checks for one.
//
// A pin is a `refusedBy: "<module>"` literal under the fault-proofs tests; the
// runner (demo/midgard-fault-proofs/scripts/run-traced-refusals.mjs) swaps
// that validator module's verbose-traced build into the plain blueprint and
// checks the refusal against its trace. So a change to a validator, or to an
// Aiken module it imports, can move exactly the pins that name it, and a local
// run checks only the files that declare those pins.
//
// Anything else among the inputs (an off-chain package that builds a pinned
// transaction, the toolchain, the deployment profiles) can move any pin. That
// remainder is every pin, which Node CI runs; locally it runs only when the
// runtime is not CI's to run.
//
// This module imports Node built-ins and derive.mjs only: Node CI's decision
// step (scripts/ci/traced-refusal-inputs.mjs) loads it before setup installs
// anything.

import { readdirSync, readFileSync } from "node:fs";
import { resolve, sep } from "node:path";

import {
  AIKEN_PROJECT,
  aikenImportClosure,
  DEMO,
  deploymentProfileInputs,
  indexAikenModules,
  matchesAny,
  workspacePackages,
} from "./derive.mjs";

const FAULT_PROOFS = `${DEMO}/midgard-fault-proofs`;
const PIN_TESTS = `${FAULT_PROOFS}/tests`;
export const TRACED_REFUSAL_RUNNER = `${FAULT_PROOFS}/scripts/run-traced-refusals.mjs`;

// Directory prefixes end in `/`; anything else names one file.
const FIXED_INPUTS = [
  // The validators, their compiler and its install, and the deployment
  // configuration the blueprint is built under.
  "onchain/",
  "config/deployments/",
  `${DEMO}/flake.lock`,
  ".github/workflows/midgard-node-ci.yml",
  ".github/actions/demo-setup/",
  // The decision itself.
  "scripts/ci/traced-refusal-inputs.mjs",
  "scripts/preflight/registry.traced-refusals.mjs",
  "scripts/preflight/derive.mjs",
  // The check: the runner, its plan, the traced-blueprint builder, every pin
  // and the helper that checks it.
  `${FAULT_PROOFS}/`,
  // Measured: what the files that run a pin import beyond the package's
  // runtime dependencies, which are added from its manifest.
  `${DEMO}/lucid-midgard/`,
  `${DEMO}/midgard-node/`,
  `${DEMO}/midgard-test-support/`,
  `${DEMO}/scripts/lib/`,
  // The dependency pins, patches and vendored evaluator they run on.
  `${DEMO}/package.json`,
  `${DEMO}/pnpm-lock.yaml`,
  `${DEMO}/pnpm-workspace.yaml`,
  `${DEMO}/.nvmrc`,
  `${DEMO}/patches/`,
  `${DEMO}/vendor/`,
];

export const isInput = (inputs, path) =>
  inputs.some((input) =>
    input.endsWith("/") ? path.startsWith(input) : path === input,
  );

/**
 * Every path whose change can move a traced refusal: the fixed inputs, the
 * fault-proofs package's runtime workspace dependencies and the
 * deployment-profile generator's files. Throws when a manifest is unreadable
 * or a dependency is not a workspace package.
 */
export const tracedRefusalInputs = (root) => {
  const packages = workspacePackages(root);
  const manifest = packages.find((pkg) => pkg.directory === FAULT_PROOFS);
  if (manifest === undefined)
    throw new Error(`${FAULT_PROOFS} is not a workspace package`);
  const runtime = Object.entries(manifest.dependencies ?? {})
    .filter(([, spec]) => String(spec).startsWith("workspace:"))
    .map(([name]) => {
      const pkg = packages.find((candidate) => candidate.name === name);
      if (pkg === undefined)
        throw new Error(`${name} is not a workspace package`);
      return `${pkg.directory}/`;
    });
  const prefixes = [...new Set([...FIXED_INPUTS, ...runtime])];
  return [
    ...prefixes,
    ...deploymentProfileInputs(root).filter((path) => !isInput(prefixes, path)),
  ];
};

// The runner's own pin syntax (pinPattern in traced-refusal-plan.mjs).
const PIN = /\brefusedBy:\s*"([^"]+)"/gu;

/** Each file under the fault-proofs tests that declares a pin, with its modules. */
const declaredPins = (root) => {
  const pins = new Map();
  const directory = resolve(root, PIN_TESTS);
  for (const entry of readdirSync(directory, { recursive: true })
    .map(String)
    .sort()) {
    if (!/\.(?:ts|mts|js|mjs)$/u.test(entry)) continue;
    const source = readFileSync(resolve(directory, entry), "utf8");
    const modules = new Set([...source.matchAll(PIN)].map(([, name]) => name));
    if (modules.size > 0)
      pins.set(`${PIN_TESTS}/${entry.split(sep).join("/")}`, modules);
  }
  return pins;
};

// Inputs of every validator's compiled code outside lib/ and validators/.
const BLUEPRINT_WIDE = [
  `${AIKEN_PROJECT}/aiken.toml`,
  `${AIKEN_PROJECT}/aiken.lock`,
  `${AIKEN_PROJECT}/env/**`,
];

/**
 * `{ files, unplaced }` for changed paths: the pin files they select (a
 * changed Aiken module selects the files whose pinned validator imports it,
 * transitively; a blueprint-wide input selects every pin file; a changed pin
 * file selects itself), and the paths no pin file accounts for.
 */
export const tracedRefusalPins = (
  root,
  changed,
  index = indexAikenModules(root),
) => {
  const pins = declaredPins(root);
  const closures = new Map();
  // A pin naming no module is the runner's to refuse, so it is selected.
  const reaches = (module, file) => {
    if (!index.byAlias.has(module)) return true;
    if (!closures.has(module))
      closures.set(module, aikenImportClosure(index, [module]));
    return closures.get(module).has(file);
  };
  const files = new Set();
  const unplaced = [];
  for (const path of changed) {
    if (matchesAny(path, BLUEPRINT_WIDE)) {
      for (const file of pins.keys()) files.add(file);
    } else if (index.byFile.has(path)) {
      for (const [file, modules] of pins) {
        if ([...modules].some((module) => reaches(module, path)))
          files.add(file);
      }
    } else if (pins.has(path)) {
      files.add(path);
    } else {
      unplaced.push(path);
    }
  }
  return { files: [...files].sort(), unplaced };
};

/**
 * The registry check. While CI owns the runtime it runs only the selected pin
 * files and hands the rest (an unplaced input, or a full-scope run) to Node CI;
 * it runs every pin when it owns that remainder itself (`--full`,
 * `--full-local`, `--run`).
 */
export const tracedRefusalCheck = (root, index) => {
  const inputs = tracedRefusalInputs(root);
  const run = (...files) => ({
    argv: ["node", TRACED_REFUSAL_RUNNER, ...files],
    cwd: ".",
  });
  return {
    id: "traced-refusals",
    title:
      "Pinned emulator refusals against verbose-traced validators (the pins a change reaches)",
    triggers: inputs.map((input) =>
      input.endsWith("/") ? `${input}**` : input,
    ),
    triggerNote:
      "the inputs Node CI's traced-refusal decision imports (scripts/ci/traced-refusal-inputs.mjs)",
    capabilities: ["aiken", "traced-blueprint"],
    display: `node ${TRACED_REFUSAL_RUNNER} <pin files the changed validators reach>; every pin when an off-chain input changed and CI does not own it`,
    handOff: ({ matched, full }) =>
      full || tracedRefusalPins(root, matched, index).unplaced.length > 0,
    plan: ({ matched, full, ciOwnsRuntime = false }) => {
      const { files, unplaced } = tracedRefusalPins(root, matched, index);
      if (!ciOwnsRuntime && (full || unplaced.length > 0)) return [run()];
      return files.length === 0 ? null : [run(...files)];
    },
  };
};
