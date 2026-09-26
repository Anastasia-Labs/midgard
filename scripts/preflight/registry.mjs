// The preflight check registry: which local checks a change can break, and the
// exact command that proves it did not.
//
// DOCTRINE — selection is never the final gate.
//   * CI runs every check on every pull request before merge. Preflight exists
//     to catch a red build before the push, not to decide what CI may skip.
//   * Selection favours recall over precision. When in doubt, add a trigger or
//     a FULL_RUN entry; a check that runs needlessly costs minutes, a check
//     that was skipped because a glob was too tight costs a red CI run.
//   * A file in FULL_RUN, a moved compiler pin, `--full`, or
//     MIDGARD_PREFLIGHT_FULL=1 selects every check at full scope.
//   * "Could not look" is never "passed": a check whose capability is missing
//     is SKIPPED WITH A REASON and the run exits 3, not 0.
//   * When CI catches a failure preflight selection missed, record it in the
//     misses log described in docs/agents/required-checks.md and widen the
//     triggers that missed it.
//
// Trigger lists are derived from the tool that owns them (see derive.mjs), so
// they cannot drift from what the tool actually reads. The four sibling
// scripts in PENDING_SIBLING_PATHS land on other branches; until they do, the
// checks that run them are skipped with that reason.

import { existsSync } from "node:fs";
import { basename, resolve } from "node:path";

import { defaultAikenBinary } from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import { parseSelectors } from "../../onchain/aiken/scripts/guard-focused-selector.mjs";
import {
  AIKEN_PROJECT,
  aikenImportClosure,
  DEMO,
  execLedgers,
  goldenChannels,
  indexAikenModules,
  matchesAny,
  packageNeedsPostgres,
  workflowText,
  workspaceDependencyClosure,
  workspaceDependentClosure,
  workspacePackages,
} from "./derive.mjs";

// Scripts owned by sibling branches of the same delivery. The registry test
// asserts every script a check runs exists, except these; integration empties
// this list.
export const PENDING_SIBLING_PATHS = [
  "scripts/ci/lint-workflows.mjs",
  "scripts/ci/workflow-triggers.test.mjs",
];

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
];

// Build outputs that are never committed; they select nothing.
export const IGNORED_PATHS = [
  `${AIKEN_PROJECT}/plutus.json`,
  `${AIKEN_PROJECT}/plutus.json.deployment.json`,
];

// The kill switch. It forces a full run; it never disables checks.
export const FULL_RUN_ENV = "MIDGARD_PREFLIGHT_FULL";

const node = (script, ...args) => ["node", script, ...args];
const step = (argv, options = {}) => ({ argv, cwd: ".", ...options });
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

const aikenChecks = (root, index) => {
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
        "node onchain/aiken/scripts/guard-focused-selector.mjs <selector per touched module>; full run: aiken check",
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
            step([defaultAikenBinary(), "check"], { cwd: AIKEN_PROJECT }),
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

const ledgerChecks = (root, index, ciText) =>
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

const demoChecks = (root, packages) => {
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

// --- golden channels -----------------------------------------------------

// The files a channel writes: the generated Aiken module or rebound constants,
// the generated JSON fixture, a generated document. Used only for advisories.
const isGeneratedArtifact = (path) =>
  /\.ak$|\.generated\.json$|\.canonical\.json$/u.test(path) ||
  /^docs\/(?!spec\/)/u.test(path) ||
  /tests\/fixtures\/[^/]+\.json$/u.test(path);

const goldenChecks = (root, packages, ciText) =>
  goldenChannels(root, packages).map((channel) => {
    const { package: pkg } = channel;
    const dependencies = workspaceDependencyClosure(packages, [pkg.name]);
    const sourceGlobs = packages
      .filter((candidate) => dependencies.has(candidate.name))
      .map((candidate) => `${candidate.directory}/src/**`);
    const readsCoreDist =
      pkg.name !== "@al-ft/midgard-core" &&
      dependencies.has("@al-ft/midgard-core") &&
      !channel.buildsCore;
    const [kind, ...rest] = channel.script
      .slice(0, -":check".length)
      .split(":");
    const gated = ciText.includes(channel.script);
    const command = step([
      "pnpm",
      "--dir",
      pkg.directory,
      "run",
      channel.script,
    ]);
    return {
      id: `${kind === "docs" ? "docs" : "golden"}:${rest.join(":")}`,
      title:
        kind === "docs"
          ? `Generated document ${rest.join(":")} matches its producer`
          : `Golden channel ${rest.join(":")} (${pkg.name})`,
      triggers: [channel.generator, ...channel.references, ...sourceGlobs],
      capabilities: [
        "node-modules",
        ...(kind === "fixtures" ? ["aiken"] : []),
        ...(readsCoreDist ? ["core-dist"] : []),
      ],
      warnOnly: !gated,
      warnReason: gated
        ? undefined
        : "no workflow runs this channel, so a red check does not block",
      artifacts: channel.references.filter(isGeneratedArtifact),
      display: formatCommand(command),
      fix:
        channel.sync === undefined
          ? undefined
          : formatCommand(
              step(["pnpm", "--dir", pkg.directory, "run", channel.sync]),
            ),
      plan: () => [command],
    };
  });

// --- repository tooling, CI and agent docs -------------------------------

const toolingChecks = () => [
  {
    id: "merge-conflicts",
    title:
      "Predicted merge conflicts with the base (git merge-tree, no checkout)",
    always: true,
    internal: "merge-tree",
    capabilities: ["git-merge-tree"],
    warnOnly: true,
    warnReason: "a predicted conflict is news, not a defect in the change",
    prePush: true,
    display:
      "git merge-tree --write-tree --name-only --no-messages HEAD <base>",
    plan: () => [],
  },
  {
    id: "required-checks-doc",
    title: "docs/agents/required-checks.md is generated from this registry",
    always: true,
    prePush: true,
    display: "node scripts/preflight.mjs --check-docs",
    fix: "node scripts/preflight.mjs --write-docs",
    plan: () => [step(node("scripts/preflight.mjs", "--check-docs"))],
  },
  {
    id: "repo-tooling-tests",
    title:
      "Repository tooling self-tests (the checks that prove the other checks can fail)",
    triggers: ["scripts/**", ".githooks/**", ".claude/settings.json"],
    prePush: true,
    display: 'node --test "scripts/**/*.test.mjs"',
    plan: () => [step(["node", "--test", "scripts/**/*.test.mjs"])],
  },
  {
    // Kept out of the pre-push slice: the focused-check tests drive a stub
    // compiler through many subprocesses and take minutes.
    id: "aiken-script-tests",
    title:
      "Self-tests of the Aiken helper scripts (pin, focused checks, ledgers)",
    triggers: [`${AIKEN_PROJECT}/scripts/**`],
    display: `node --test "${AIKEN_PROJECT}/scripts/*.test.mjs"`,
    plan: () => [
      step(["node", "--test", `${AIKEN_PROJECT}/scripts/*.test.mjs`]),
    ],
  },
  {
    id: "workflow-lint",
    title: "Workflow lint",
    triggers: [".github/workflows/**", "scripts/ci/**"],
    requiresFiles: ["scripts/ci/lint-workflows.mjs"],
    prePush: true,
    display: "node scripts/ci/lint-workflows.mjs",
    plan: () => [step(node("scripts/ci/lint-workflows.mjs"))],
  },
  {
    id: "workflow-triggers",
    title: "Workflow trigger table",
    triggers: [".github/workflows/**", "scripts/ci/**"],
    requiresFiles: ["scripts/ci/workflow-triggers.test.mjs"],
    prePush: true,
    display: "node --test scripts/ci/workflow-triggers.test.mjs",
    plan: () => [
      step(["node", "--test", "scripts/ci/workflow-triggers.test.mjs"]),
    ],
  },
  ...[
    [
      "agent-doc-links",
      "Agent documentation links resolve",
      "scripts/agents/check-doc-links.mjs",
    ],
    [
      "agent-enforcement-tags",
      "Every stated rule names its enforcement",
      "scripts/agents/check-enforcement-tags.mjs",
    ],
  ].map(([id, title, script]) => ({
    id,
    title,
    triggers: [
      "docs/**",
      "**/AGENTS.md",
      "**/CLAUDE.md",
      "CONTEXT.md",
      ".agents/**",
      "scripts/agents/**",
    ],
    requiresFiles: [script],
    prePush: true,
    display: `node ${script}`,
    plan: () => [step(node(script))],
  })),
];

// The registry, in run order: cheap and independent first, then builds, then
// the checks that consume what the builds produced.
export const buildRegistry = (root) => {
  const packages = workspacePackages(root);
  const index = indexAikenModules(root);
  const ciText = workflowText(root);
  const demo = demoChecks(root, packages);
  const [fmt, focused, blueprint] = aikenChecks(root, index);
  const checks = [
    ...toolingChecks(),
    fmt,
    demo.format,
    demo.build,
    demo.typecheck,
    ...goldenChecks(root, packages, ciText),
    focused,
    blueprint,
    ...ledgerChecks(root, index, ciText),
    demo.test,
    demo.testDb,
  ].map((check) => ({
    triggers: [],
    capabilities: [],
    warnOnly: false,
    prePush: false,
    always: false,
    ...check,
  }));
  const ids = new Set();
  for (const check of checks) {
    if (ids.has(check.id)) {
      throw new Error(`duplicate preflight check id '${check.id}'`);
    }
    ids.add(check.id);
  }
  return { checks, packages, index };
};

// Which checks a change selects. `changed` is repository-relative paths.
// Returns the selected checks with the paths that selected each, the reasons a
// full run was forced, and the changed paths no check covers.
export const selectChecks = (
  registry,
  changed,
  { full = false, fullReasons = [], prePush = false } = {},
) => {
  const relevant = changed.filter((path) => !IGNORED_PATHS.includes(path));
  const reasons = [
    ...fullReasons,
    ...relevant
      .filter((path) => matchesAny(path, FULL_RUN))
      .map((path) => `${path} is in FULL_RUN`),
  ];
  const isFull = full || reasons.length > 0;
  const selected = [];
  const covered = new Set();
  for (const check of registry.checks) {
    const matched = relevant.filter((path) => matchesAny(path, check.triggers));
    if (!check.always) {
      for (const path of matched) {
        covered.add(path);
      }
    }
    if (prePush && !check.prePush) {
      continue;
    }
    if (check.always || isFull || matched.length > 0) {
      selected.push({ check, matched });
    }
  }
  return {
    full: isFull,
    fullReasons: reasons,
    selected,
    uncovered: relevant.filter((path) => !covered.has(path)),
  };
};
