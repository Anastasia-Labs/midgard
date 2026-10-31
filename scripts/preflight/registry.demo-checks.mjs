import { existsSync } from "node:fs";
import { basename, resolve } from "node:path";
import { packageTestTask } from "../../demo/scripts/package-tasks.mjs";

import { parseSelectors } from "../../onchain/aiken/scripts/guard-focused-selector.mjs";
import { reachablePackages } from "../contrib/reached.mjs";
import { SDK_SUITES } from "./sdk-suite-evidence.mjs";
import {
  AIKEN_PROJECT,
  aikenImportClosure,
  DEMO,
  packageNeedsPostgres,
  referencedFiles,
  SCRIPT_BUDGET_JUDGE,
  SCRIPT_BUDGET_TABLE,
  scriptBudgetModules,
  workflowRunsCheck,
  workspaceDependentClosure,
} from "./derive.mjs";

// A change to any of these can move the result of every check, so it selects
// all of them at full scope. Recall beats precision: add to this list when a
// shared input is found.
export const FULL_RUN = [
  // Aiken project manifest and dependency lock.
  `${AIKEN_PROJECT}/aiken.toml`,
  `${AIKEN_PROJECT}/aiken.lock`,
  // Blueprint inputs outside lib/ and validators/: the substituted
  // environment modules and the deployment configuration that generates them.
  `${AIKEN_PROJECT}/env/**`,
  "config/deployments/**",
  `${DEMO}/scripts/deployment-profiles.mjs`,
  `${DEMO}/scripts/deployment-profile-aiken.mjs`,
  `${DEMO}/scripts/deployment-profile-identity.mjs`,
  `${DEMO}/scripts/deployment-profile-da.mjs`,
  `${DEMO}/scripts/deployment-profile-names.mjs`,
  `${DEMO}/scripts/deployment-profile-budgets.mjs`,
  `${DEMO}/scripts/deployment-profile-operator-economics.mjs`,
  // The compiler: demo/flake.lock pins the aiken every check runs.
  `${DEMO}/flake.lock`,
  // Workspace manifests, lockfile, toolchain pin, patched and vendored
  // dependencies.
  `${DEMO}/package.json`,
  `${DEMO}/pnpm-lock.yaml`,
  `${DEMO}/pnpm-workspace.yaml`,
  `${DEMO}/.nvmrc`,
  `${DEMO}/patches/**`,
  `${DEMO}/vendor/**`,
  // Shared task semantics drive every declared package build and test.
  `${DEMO}/scripts/package-tasks.mjs`,
  `${DEMO}/scripts/package-test.mjs`,
  // Shared test fixtures and harness every package's suite loads.
  `${DEMO}/midgard-test-support/**`,
  // Preflight itself: a selection bug cannot be allowed to select itself out.
  "scripts/preflight.mjs",
  "scripts/preflight/**",
  // Contributor execution and package-manager dispatch are shared by builds,
  // registered generators and all preflight children.
  "scripts/contrib.mjs",
  "scripts/contrib/**",
  "scripts/pnpm.mjs",
  "scripts/bin/**",
];

// These modules are consumed only by preflight, not package builds, artifacts
// or protocol runtimes. Their tests run in Repo Tools CI on every PR;
// derive/probes/shared execution helpers remain FULL_RUN inputs. Protocol CI
// keeps its existing source-path triggers, assertions and acceptance profile.
export const VERIFICATION_ONLY = [
  "scripts/preflight.mjs",
  "scripts/preflight/run.mjs",
  "scripts/preflight/registry.mjs",
  "scripts/preflight/registry.demo-checks.mjs",
  "scripts/preflight/registry.select-checks.mjs",
  "scripts/preflight/registry.tooling-checks.mjs",
  "scripts/preflight/docs.mjs",
  "scripts/preflight/ci-evidence.mjs",
  "scripts/preflight/sdk-suite-evidence.mjs",
  "scripts/preflight/runtime-owner.mjs",
  "scripts/preflight/*.test.mjs",
];

// The kill switch. It forces a full run; it never disables checks.
export const FULL_RUN_ENV = "MIDGARD_PREFLIGHT_FULL";

export const node = (script, ...args) => ["node", script, ...args];

export const step = (argv, options = {}) => ({ argv, cwd: ".", ...options });

const quote = (arg) => (/^[\w@%+=:,./-]+$/u.test(arg) ? arg : `'${arg}'`);

export const formatCommand = ({ argv, cwd, env }) =>
  [
    ...(cwd && cwd !== "." ? [`(cd ${cwd} &&`] : []),
    ...Object.entries(env ?? {}).map(
      ([key, value]) => `${key}=${quote(value)}`,
    ),
    ...argv.map(quote),
  ].join(" ") + (cwd && cwd !== "." ? ")" : "");

const existing = (root, paths) =>
  paths.filter((path) => existsSync(resolve(root, path)));

const pnpmRun = (filters, script, extra = []) => [
  "pnpm",
  "--dir",
  DEMO,
  ...(filters === undefined
    ? ["-r"]
    : filters.flatMap((name) => ["--filter", name])),
  ...extra,
  "run",
  "--if-present",
  script,
];

// --- Aiken ---------------------------------------------------------------

export const aikenChecks = (root, index) => {
  const sources = [`${AIKEN_PROJECT}/{lib,validators}/**/*.ak`];
  return [
    {
      id: "aiken-fmt",
      title: "Aiken formatting under the pinned fork (CI's normalized check)",
      triggers: [`${AIKEN_PROJECT}/**/*.ak`],
      capabilities: ["aiken"],
      prePush: true,
      display: "node scripts/preflight/aiken-fmt-check.mjs <touched .ak files>",
      fix: "node scripts/preflight/aiken-fmt-check.mjs --write <files>",
      plan: ({ matched, full }) => {
        if (full) {
          return [step(node("scripts/preflight/aiken-fmt-check.mjs", "--all"))];
        }
        const files = existing(root, matched);
        return files.length === 0
          ? null
          : [step(node("scripts/preflight/aiken-fmt-check.mjs", ...files))];
      },
    },
    {
      id: "aiken-focused",
      title:
        "Focused Aiken tests of every touched module (fail-closed selector guard)",
      triggers: sources,
      capabilities: ["aiken"],
      display:
        "node onchain/aiken/scripts/guard-focused-selector.mjs <selector per touched module>; full run: --all",
      plan: ({ matched, full, advise }) => {
        const selectors = new Set();
        for (const file of existing(root, matched)) {
          const module = index.byFile.get(file);
          if (module === undefined) {
            continue;
          }
          // The guard's selector alphabet has no `.`; Aiken matches the module
          // part as a substring, so the prefix before the first `.` selects
          // the module and its `.test` siblings.
          const selector = module.name.split(".")[0];
          try {
            parseSelectors([selector]);
          } catch {
            advise({
              id: `aiken-unselectable-module:${module.name}`,
              message: `module ${module.name} (${file}) cannot be spelled as a focused selector, so no focused check ran for it`,
              steps: [
                `Run its tests by name: node onchain/aiken/scripts/run-focused-check.mjs ${module.name} <test> ...`,
              ],
            });
            continue;
          }
          if (
            index.modules.some((m) => m.hasTests && m.name.includes(selector))
          ) {
            selectors.add(selector);
          } else {
            advise({
              id: `aiken-untested-module:${module.name}`,
              message: `no Aiken test selects ${module.name} (${file}); only the full suite and the blueprint build exercise it`,
              steps: [
                `If the module carries behaviour, add a test beside it (${file.replace(/\.ak$/u, ".test.ak")}).`,
                `Run it: node onchain/aiken/scripts/guard-focused-selector.mjs ${selector}`,
              ],
            });
          }
        }
        if (full || selectors.size > 64) {
          return [
            step(
              node(
                `${AIKEN_PROJECT}/scripts/guard-focused-selector.mjs`,
                "--all",
              ),
            ),
          ];
        }
        return selectors.size === 0
          ? null
          : [
              step(
                node(
                  `${AIKEN_PROJECT}/scripts/guard-focused-selector.mjs`,
                  ...[...selectors].sort(),
                ),
              ),
            ];
      },
    },
    {
      id: "aiken-blueprint",
      title:
        "Blueprint rebuild into a temporary file (never the tracked plutus.json)",
      triggers: [`${AIKEN_PROJECT}/**/*.ak`],
      capabilities: ["aiken"],
      display: "node scripts/preflight/aiken-build-check.mjs",
      plan: () => [step(node("scripts/preflight/aiken-build-check.mjs"))],
    },
  ];
};

// Every compiled validator in the stamped blueprint at or under the L1
// publication limit, which the gate reads from the built midgard-core. Its
// test first feeds it blueprints over and at the limit.
const PUBLICATION_SIZE_GATE = `${DEMO}/scripts/check-validator-publication-sizes.mjs`;
const PUBLICATION_SIZE_TEST = `${DEMO}/scripts/check-validator-publication-sizes.test.mjs`;

export const validatorPublicationSizeCheck = (ciText) => {
  const id = "validator-publication-size";
  const gated = workflowRunsCheck(ciText, id);
  const steps = [
    step(node("--test", PUBLICATION_SIZE_TEST)),
    step(node(PUBLICATION_SIZE_GATE)),
  ];
  return {
    id,
    title:
      "Compiled validators within the L1 reference-script publication limit",
    triggers: [
      `${AIKEN_PROJECT}/**/*.ak`,
      PUBLICATION_SIZE_GATE,
      PUBLICATION_SIZE_TEST,
      `${DEMO}/midgard-core/src/consensus-profile.ts`,
    ],
    capabilities: ["node-modules", "blueprint", "core-dist"],
    warnOnly: !gated,
    warnReason: gated
      ? undefined
      : "no workflow runs the publication size gate, so a red verdict does not block",
    display: steps.map(formatCommand).join(" && "),
    plan: () => steps,
  };
};

// One check for every script budget. A change to the table or the judge runs
// the whole table; a change to Aiken source runs the budgets of every module
// whose compiled code can depend on it.
export const scriptBudgetCheck = (root, index, ciText) => {
  const modules = scriptBudgetModules(root);
  const closures = new Map(
    modules.map((module) => [module, aikenImportClosure(index, [module])]),
  );
  const tooling = [
    SCRIPT_BUDGET_TABLE,
    SCRIPT_BUDGET_JUDGE,
    ...referencedFiles(root, SCRIPT_BUDGET_JUDGE),
  ];
  const gated = ciText.includes(basename(SCRIPT_BUDGET_JUDGE));
  const judge = (...args) => step(node(SCRIPT_BUDGET_JUDGE, "check", ...args));
  return {
    id: "script-budgets",
    title: "Script budgets: Aiken execution units at or under their ceilings",
    triggers: [
      ...new Set([
        ...tooling,
        ...[...closures.values()].flatMap((c) => [...c]),
      ]),
    ].sort(),
    triggerNote: `${SCRIPT_BUDGET_TABLE}, the judge, and every .ak module a budgeted module imports, transitively`,
    capabilities: ["aiken"],
    warnOnly: !gated,
    warnReason: gated
      ? undefined
      : "no workflow runs the script-budget judge, so a red reading does not block",
    display: formatCommand(judge()),
    fix: `node ${SCRIPT_BUDGET_JUDGE} raise <name> --reason <why> — only for a cost you accept; otherwise shrink the script`,
    plan: ({ matched, full }) => {
      if (full || matched.some((path) => tooling.includes(path))) {
        return [judge()];
      }
      const touched = new Set(matched);
      const reached = modules.filter((module) =>
        [...closures.get(module)].some((file) => touched.has(file)),
      );
      return reached.length === 0
        ? null
        : [judge(...reached.flatMap((module) => ["--module", module]))];
    },
  };
};

// --- TypeScript workspace -----------------------------------------------

// `ciPaths`: the paths Node CI runs the package suites for (its pull-request
// path filter), or undefined when its routing is unknown.
export const demoChecks = (root, packages, ciPaths) => {
  const triggers = packages.map((pkg) => `${pkg.directory}/**`);
  const postgres = new Set(
    packages
      .filter((pkg) => packageNeedsPostgres(root, pkg))
      .map((p) => p.name),
  );
  // Packages whose files changed, and every package depending on them.
  const affected = (matched) =>
    workspaceDependentClosure(
      packages,
      packages
        .filter((pkg) =>
          matched.some((path) => path.startsWith(`${pkg.directory}/`)),
        )
        .map((pkg) => pkg.name),
    );
  const ordered = (names) =>
    packages.map((pkg) => pkg.name).filter((name) => names.has(name));
  const task = (id, title, script, options = {}) => ({
    id,
    title,
    triggers,
    triggerNote:
      "any file of a workspace package; runs for it and every package that depends on it",
    capabilities: ["node-modules", ...(options.capabilities ?? [])],
    invalidates: options.invalidates,
    display: `${formatCommand(step(pnpmRun(["<touched package and its dependents>"], script)))}`,
    plan: ({ matched, full }) => {
      const names = full
        ? new Set(packages.map((pkg) => pkg.name))
        : affected(matched);
      const selected = ordered(names);
      if (selected.length === 0) {
        return null;
      }
      return [
        step(
          pnpmRun(
            selected.length === packages.length ? undefined : selected,
            script,
          ),
        ),
      ];
    },
  });
  // The package suites: the tests the change reaches in each package it can
  // reach (`contrib test --related`, which widens to the whole package when
  // unsure), as the package `test` script runs them. A full run is every
  // package's whole suite. A path outside the packages that Node CI runs the
  // suites for can reach them too (every path, when CI's routing is unknown).
  const suites = (id, title, include, capabilities) => ({
    id,
    title,
    triggers: [...triggers, ...(ciPaths ?? ["**"])],
    triggerNote:
      "any file of a workspace package, or any path Node CI runs the package suites for; runs the tests those files reach in every package they can reach",
    capabilities: ["node-modules", ...capabilities],
    display:
      "node scripts/contrib.mjs test --package <each package the change can reach> --related <changed paths>",
    plan: ({ matched, full }) => {
      const names = full
        ? new Set(packages.map((pkg) => pkg.name))
        : reachablePackages(root, matched);
      const selected = ordered(names).filter(include);
      if (selected.length === 0) return null;
      if (full) {
        const singles = Object.keys(SDK_SUITES).filter((name) =>
          selected.includes(name),
        );
        const others = selected.filter((name) => !singles.includes(name));
        return [
          ...(others.length
            ? [step(pnpmRun(others, "test", ["--workspace-concurrency=1"]))]
            : []),
          ...singles.map((name) =>
            step(["pnpm", "--filter", name, "test"], {
              cwd: DEMO,
              sdkSuite: name,
            }),
          ),
        ];
      }
      return selected.map((name) => {
        const pkg = packages.find((pkg) => pkg.name === name);
        // Only declared Vitest tasks support focused source selection.
        return packageTestTask(pkg) === undefined
          ? step(pnpmRun([name], "test"))
          : step(
              node(
                "scripts/contrib.mjs",
                "test",
                "--package",
                name,
                ...matched.flatMap((path) => ["--related", path]),
              ),
              { skippedTestsPass: true },
            );
      });
    },
  });
  return {
    build: task(
      "demo-build",
      "Build the touched packages and their dependents (dist)",
      "build",
      { invalidates: ["core-dist"] },
    ),
    boundary: {
      id: "demo-boundary",
      title:
        "Installed production packages resolve public Node exports and CLI help",
      triggers: [...triggers, ".github/workflows/midgard-node-ci.yml"],
      capabilities: ["node-modules"],
      invalidates: ["core-dist"],
      display:
        "node scripts/contrib.mjs boundary --package <each affected built package>",
      plan: ({ matched, full }) => {
        const names =
          full || matched.includes(".github/workflows/midgard-node-ci.yml")
            ? new Set(packages.map((pkg) => pkg.name))
            : affected(matched);
        const selected = packages.filter(
          (pkg) => names.has(pkg.name) && pkg.scripts.build,
        );
        return selected.length
          ? selected.map((pkg) =>
              step(
                node("scripts/contrib.mjs", "boundary", "--package", pkg.name),
              ),
            )
          : null;
      },
    },
    typecheck: task(
      "demo-typecheck",
      "Typecheck the touched packages and their dependents",
      "typecheck",
    ),
    format: {
      id: "demo-format",
      title: "Prettier on touched workspace files (CI's format-check scope)",
      triggers: [`${DEMO}/**/*.{ts,tsx,md}`],
      capabilities: ["node-modules"],
      prePush: true,
      display:
        "(cd demo && node_modules/.bin/prettier --check <touched files>)",
      fix: "(cd demo && node_modules/.bin/prettier --write <files>)",
      plan: ({ matched, full }) => {
        if (full) {
          return [step(["pnpm", "--dir", DEMO, "run", "format-check"])];
        }
        const files = existing(root, matched).map((path) =>
          path.slice(`${DEMO}/`.length),
        );
        return files.length === 0
          ? null
          : [
              step(["node_modules/.bin/prettier", "--check", ...files], {
                cwd: DEMO,
              }),
            ];
      },
    },
    test: suites(
      "demo-test",
      "Test suites of the reached packages that need no database",
      (name) => !postgres.has(name),
      ["blueprint"],
    ),
    testDb: suites(
      "demo-test-db",
      `Postgres-backed test suites (${[...postgres].join(", ")}) of the reached packages`,
      (name) => postgres.has(name),
      ["blueprint", "postgres", "db-prefix"],
    ),
  };
};
