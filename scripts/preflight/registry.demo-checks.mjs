import { existsSync } from "node:fs";
import { basename, resolve } from "node:path";

import { parseSelectors } from "../../onchain/aiken/scripts/guard-focused-selector.mjs";
import { SDK_SUITES } from "./sdk-suite-evidence.mjs";
import {
  AIKEN_PROJECT,
  aikenImportClosure,
  DEMO,
  execLedgers,
  packageNeedsPostgres,
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
  // The compiler pin's reader (the pin itself is compared by value, below).
  `${AIKEN_PROJECT}/scripts/pinned-compiler.mjs`,
  // Workspace manifests, lockfile, toolchain pin, patched and vendored
  // dependencies.
  `${DEMO}/package.json`,
  `${DEMO}/pnpm-lock.yaml`,
  `${DEMO}/pnpm-workspace.yaml`,
  `${DEMO}/.nvmrc`,
  `${DEMO}/patches/**`,
  `${DEMO}/vendor/**`,
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
            step(node(`${AIKEN_PROJECT}/scripts/pinned-compiler.mjs`)),
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

export const ledgerChecks = (root, index, ciText) =>
  execLedgers(root).map((ledger) => {
    const closure = [...aikenImportClosure(index, ledger.modules)].sort();
    const env =
      ledger.environment === undefined
        ? undefined
        : { MIDGARD_AIKEN_ENV: ledger.environment };
    const gated = ciText.includes(basename(ledger.verifier));
    return {
      id: `exec-ledger:${ledger.id}`,
      title: `Execution ledger ${basename(ledger.ledger)}`,
      triggers: [ledger.verifier, ledger.ledger, ...ledger.scripts, ...closure],
      triggerNote: `${ledger.verifier}, ${ledger.ledger}, and every .ak module that ${ledger.modules.join(", ")} import${ledger.modules.length === 1 ? "s" : ""}, transitively`,
      capabilities: ["aiken"],
      warnOnly: !gated,
      warnReason: gated
        ? undefined
        : "no workflow runs this verifier, so a red reading does not block",
      artifacts: [ledger.ledger],
      display: formatCommand(step(node(ledger.verifier), { env })),
      fix: `${formatCommand(step(node(ledger.verifier, "--update"), { env }))} — only for a drift you can explain; commit ${ledger.ledger} with the explanation`,
      plan: () => [step(node(ledger.verifier), { env })],
    };
  });

// --- TypeScript workspace -----------------------------------------------

export const demoChecks = (root, packages) => {
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
    display: `${formatCommand(step(pnpmRun(["<touched package and its dependents>"], script, options.extra)))}`,
    plan: ({ matched, full }) => {
      const names = full
        ? new Set(packages.map((pkg) => pkg.name))
        : affected(matched);
      const selected = ordered(names).filter(options.include ?? (() => true));
      if (selected.length === 0) {
        return null;
      }
      if (id === "demo-test") {
        const singles = Object.keys(SDK_SUITES).filter((name) =>
          selected.includes(name),
        );
        const others = selected.filter((name) => !singles.includes(name));
        return [
          // The watcher suite drives the compiled Go helper. Use its guarded
          // recipe on this checkout's current inputs, as hosted CI does.
          ...(selected.includes("midgard-watcher")
            ? [
                step([
                  "pnpm",
                  "--dir",
                  "demo/midgard-watcher",
                  "run",
                  "native:build",
                ]),
              ]
            : []),
          ...(others.length
            ? [step(pnpmRun(others, script, options.extra))]
            : []),
          ...singles.map((name) =>
            step(["pnpm", "--filter", name, "test"], {
              cwd: DEMO,
              sdkSuite: name,
            }),
          ),
        ];
      }
      return [
        step(
          pnpmRun(
            selected.length === packages.length ? undefined : selected,
            script,
            options.extra,
          ),
        ),
      ];
    },
  });
  return {
    build: task(
      "demo-build",
      "Build the touched packages and their dependents (dist)",
      "build",
      { invalidates: ["core-dist"] },
    ),
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
    test: task(
      "demo-test",
      "Test suites of the touched packages and their dependents that need no database",
      "test",
      {
        capabilities: ["blueprint"],
        extra: ["--workspace-concurrency=1"],
        include: (name) => !postgres.has(name),
      },
    ),
    testDb: task(
      "demo-test-db",
      `Postgres-backed test suites (${[...postgres].join(", ")}) of the touched packages and their dependents`,
      "test",
      {
        capabilities: ["blueprint", "postgres", "db-prefix"],
        extra: ["--workspace-concurrency=1"],
        include: (name) => postgres.has(name),
      },
    ),
  };
};
