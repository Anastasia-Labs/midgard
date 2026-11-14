import { readFileSync } from "node:fs";
import { posix, resolve } from "node:path";

import {
  AIKEN_PROJECT,
  DEMO,
  deploymentProfileInputs,
  goldenChannels,
  matchesAny,
  workflowRunsCheck,
  workspaceDependencyClosure,
} from "./derive.mjs";
import { formatCommand, node, step } from "./registry.demo-checks.mjs";

// --- golden channels -----------------------------------------------------

// The files a channel writes: the generated Aiken module or rebound constants,
// the generated JSON fixture, a generated document. Used only for advisories.
const isGeneratedArtifact = (path) =>
  /\.ak$|\.generated\.json$|\.canonical\.json$/u.test(path) ||
  /^docs\/(?!spec\/)/u.test(path) ||
  /tests\/fixtures\/[^/]+\.json$/u.test(path);

// The deployment-profile and interactive-emulator cache tests, outside
// demo/scripts/lib, and the files outside demo/scripts/** they read.
const DEMO_SCRIPT_TESTS = [
  "demo/scripts/deployment-profiles.test.mjs",
  "demo/scripts/deployment-profiles.retention.test.mjs",
  "demo/scripts/interactive-emulator.test.mjs",
];
const DEMO_SCRIPT_TEST_INPUTS = [
  "config/deployments/**",
  "demo/midgard-test-support/interactive-emulator.js",
  "demo/midgard-fault-proofs/scripts/traced-blueprint.mjs",
  "demo/midgard-core/src/generated-deployment-profiles.ts",
  "onchain/aiken/env/*.ak",
  "demo/flake.lock",
];

export const goldenChecks = (root, packages, ciText) =>
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
    const id = `${kind === "docs" ? "docs" : "golden"}:${rest.join(":")}`;
    const gated =
      ciText.includes(channel.script) || workflowRunsCheck(ciText, id);
    const command = step([
      "pnpm",
      "--dir",
      pkg.directory,
      "run",
      channel.script,
    ]);
    return {
      id,
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

// --- the transaction differential's reference leg --------------------------

// The tx-fuzz rows again with the reference leg on: every generated spend is
// also evaluated by the pinned aiken's `uplc eval`, which the test-support
// Aiken evaluator resolves through scripts/lib/aiken.mjs. With the leg on,
// each row's pins include the leg's own classes per seed, so its verdicts are
// pinned too.
const VALIDATION = "@al-ft/midgard-validation";
const TX_FUZZ_TEST = "tests/fuzz/tx-fuzz.test.ts";

export const txFuzzReferenceCheck = (packages) => {
  const dependencies = workspaceDependencyClosure(packages, [VALIDATION]);
  const validation = packages.find((pkg) => pkg.name === VALIDATION);
  const command = step(
    [
      "pnpm",
      "--dir",
      validation.directory,
      "exec",
      "vitest",
      "run",
      TX_FUZZ_TEST,
    ],
    { env: { MIDGARD_TX_FUZZ_UPLC_BIN: "pinned" } },
  );
  return {
    id: "tx-fuzz-reference",
    title: "Transaction differential with the pinned aiken reference leg",
    triggers: [
      ...packages
        .filter((candidate) => dependencies.has(candidate.name))
        .map((candidate) => `${candidate.directory}/src/**`),
      `${validation.directory}/tests/fuzz/**`,
      "demo/midgard-test-support/aiken-export-cache.js",
      `${validation.directory}/vitest.config.ts`,
      `${AIKEN_PROJECT}/lib/**`,
      `${AIKEN_PROJECT}/aiken.toml`,
      "scripts/lib/aiken.mjs",
      "demo/flake.lock",
    ],
    capabilities: ["node-modules", "aiken", "blueprint"],
    display: formatCommand(command),
    plan: () => [command],
  };
};

// --- independent documentation and devnet validation ----------------------

const docsSiteTriggers = [
  "docs-site/**",
  "**/*.md",
  "**/*.mdx",
  "demo/lucid-midgard/**",
  "demo/midgard-core/**",
  ".github/workflows/docs-site-ci.yml",
];

export const independentChecks = () => [
  ...["sdk", "node", "emulator"].map((lane) => ({
    id: `tx-preparation:${lane}`,
    title: `Transaction preparation ${lane} acceptance lane`,
    triggers: [
      "demo/lucid-midgard/src/**",
      "demo/midgard-sdk/src/**",
      "demo/midgard-node/src/fibers/**",
      "demo/midgard-node/src/workers/**",
      "demo/midgard-node-tools/src/**",
    ],
    capabilities: [
      "node-modules",
      "blueprint",
      ...(lane === "sdk" ? [] : ["postgres", "db-prefix"]),
    ],
    ...(lane === "sdk"
      ? {
          requiresFiles: [
            "demo/lucid-midgard/package.json",
            "demo/midgard-sdk/package.json",
          ],
        }
      : {}),
    display: `pnpm --dir demo run test:tx-prep:${lane}`,
    plan: () => [step(["pnpm", "--dir", DEMO, "run", `test:tx-prep:${lane}`])],
  })),
  {
    id: "docs-site-links",
    title: "Repository-local Markdown and MDX links resolve",
    triggers: [...docsSiteTriggers, ".gitignore"],
    requiresFiles: ["docs-site/scripts/check-docs-links.mjs"],
    prePush: true,
    display: "node docs-site/scripts/check-docs-links.mjs",
    plan: () => [step(node("docs-site/scripts/check-docs-links.mjs"))],
  },
  ...[
    [
      "docs-site-build",
      "Build the documentation site and its SDK examples",
      "build",
    ],
    [
      "docs-site-typecheck",
      "Generate and typecheck the documentation site's types",
      "types:check",
    ],
  ].map(([id, title, script]) => ({
    id,
    title,
    triggers: docsSiteTriggers,
    capabilities: ["node-modules", "docs-site-node-modules"],
    display: `(cd docs-site && corepack pnpm run ${script})`,
    // These scripts build their SDK dependencies in prebuild/pretypes:check.
    invalidates: ["core-dist"],
    plan: () => [
      step(["corepack", "pnpm", "run", script], { cwd: "docs-site" }),
    ],
  })),
  {
    id: "spec-build",
    title:
      "Build the technical specification PDF with the repository's Nix environment",
    triggers: [
      "technical-spec/**",
      "Makefile",
      ".github/workflows/latex-ci.yml",
    ],
    capabilities: ["nix"],
    display: "make spec",
    plan: () => [step(["make", "spec"])],
  },
  {
    id: "devnet-assets",
    title: "Phase 4 devnet generator and shell asset tests (no running devnet)",
    triggers: ["demo/midgard-node-tools/devnet/phase4-process/**"],
    capabilities: ["node-modules", "blueprint"],
    requiresFiles: [
      "demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs",
    ],
    display:
      "node --test demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs demo/midgard-node-tools/devnet/phase4-process/tests/l1-follower-inputs.test.mjs demo/midgard-node-tools/devnet/phase4-process/tests/protocol-bootstrap-follower.test.mjs",
    plan: () => [
      step(
        node(
          "--test",
          "demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs",
          "demo/midgard-node-tools/devnet/phase4-process/tests/l1-follower-inputs.test.mjs",
          "demo/midgard-node-tools/devnet/phase4-process/tests/protocol-bootstrap-follower.test.mjs",
        ),
      ),
    ],
  },
];

// --- repository tooling, CI and agent docs -------------------------------

const E2E_SKILL = ".agents/skills/midgard-e2e-acceptance";

// The agent-facing files the agent-documentation checks read.
const AGENT_FILES = [
  "docs/**",
  "**/AGENTS.md",
  "**/CLAUDE.md",
  "CONTEXT.md",
  ".agents/**",
];

// A change under these can move any tooling self-test, so it runs them all.
const TOOLING_TESTS = "scripts/**/*.test.mjs";
const TOOLING_TRIGGERS = [
  "scripts/**",
  ".githooks/**",
  ".claude/settings.json",
];

// The files outside scripts/ each tooling self-test reads, so a change to one
// runs the self-tests that read it and no others: the journey files and the
// workflow a wiring check holds to each other, a module a test imports. A
// self-test needs an entry once it reads a file outside scripts/ that is
// neither under TOOLING_TRIGGERS nor a full-run path, and the entry then names
// every file it reads outside scripts/. registry.tooling-inputs.test.mjs runs
// each listed test in a copy holding only scripts/ and these files, so a read
// left off its entry fails there, and so does a glob that matches no file.
// Nothing catches a self-test that has no entry at all: whoever writes a
// self-test that reads outside scripts/ adds its entry here.
//
// Not listed: the self-tests that read the whole tree. The preflight registry
// tests, preflight-runner-filters, check-registry-paths and contrib's
// run-tests build the registry or the package graph from every file;
// check-doc-links and check-enforcement-tags resolve links and enforcer tags
// to any tracked path. Their checks of this tree are steps of their own
// (registry-paths, workflow-triggers, agent-doc-links, agent-enforcement-tags).
const toolingTestInputs = (root) => {
  const NODE_CI = ".github/workflows/midgard-node-ci.yml";
  const WORKFLOWS = ".github/workflows/**";
  const MANIFESTS = [
    `${DEMO}/package.json`,
    `${DEMO}/*/package.json`,
    `${DEMO}/pnpm-workspace.yaml`,
  ];
  const PACKAGE_TASKS = `${DEMO}/scripts/package-tasks.mjs`;
  const BLUEPRINT_STAMP = `${DEMO}/scripts/lib/blueprint-stamp.mjs`;
  const READINESS_DOC =
    ".agents/skills/running-the-devnet/references/endpoints-and-logs.md";
  // The preflight modules import these, so every test that loads them reads
  // them.
  const PREFLIGHT_IMPORTS = [
    `${AIKEN_PROJECT}/scripts/guard-focused-selector.mjs`,
    `${AIKEN_PROJECT}/scripts/complete-test-shards.mjs`,
    `${DEMO}/midgard-core/src/script-budgets.{ts,json}`,
    PACKAGE_TASKS,
  ];
  return {
    "scripts/agents/check-agent-config.test.mjs": [
      ...AGENT_FILES,
      ".claude/settings.json",
    ],
    "scripts/ci/check-readiness-reasons-doc.test.mjs": [
      `${DEMO}/midgard-node/src/**`,
      `${DEMO}/midgard-l1-follower/src/**`,
      READINESS_DOC,
    ],
    "scripts/ci/check-watcher-readiness-reasons-doc.test.mjs": [
      `${DEMO}/midgard-watcher/src/runtime/operations-observability.watcher-operations-readiness-reasons.ts`,
      READINESS_DOC,
    ],
    "scripts/ci/fork-simulator-inputs.test.mjs": [
      NODE_CI,
      ...MANIFESTS,
      PACKAGE_TASKS,
    ],
    "scripts/ci/lint-workflows.test.mjs": [WORKFLOWS, ".github/actions/**"],
    "scripts/ci/node-tools-journey-tests-wired.test.mjs": [
      NODE_CI,
      `${DEMO}/midgard-node-tools/devnet/watcher-journeys/**`,
    ],
    "scripts/ci/prepare-merge-gates.test.mjs": [WORKFLOWS],
    "scripts/ci/script-modes.test.mjs": [WORKFLOWS, ".githooks/**"],
    "scripts/ci/shard-counts.test.mjs": [
      NODE_CI,
      `${DEMO}/*/tests/duration-shards.test.ts`,
    ],
    "scripts/ci/traced-refusal-inputs.test.mjs": [
      NODE_CI,
      ".github/actions/demo-setup/**",
      `${DEMO}/midgard-fault-proofs/**`,
      ...MANIFESTS,
      PACKAGE_TASKS,
      ...deploymentProfileInputs(root),
    ],
    "scripts/ci/traced-refusal-plan.test.mjs": [
      `${DEMO}/midgard-fault-proofs/tests/**`,
      `${DEMO}/midgard-fault-proofs/scripts/traced-refusal-plan.mjs`,
    ],
    "scripts/ci/vitest-script-flags.test.mjs": ["**/package.json"],
    "scripts/ci/workflow-triggers.test.mjs": [WORKFLOWS],
    "scripts/contrib/campaign.test.mjs": [
      `${DEMO}/midgard-node-tools/config/devnet-campaign.example.json`,
      PACKAGE_TASKS,
      BLUEPRINT_STAMP,
    ],
    "scripts/contrib/registry.test.mjs": [
      ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
      ...MANIFESTS,
      "docs-site/package.json",
      `${DEMO}/midgard-core/scripts/**`,
      PACKAGE_TASKS,
      BLUEPRINT_STAMP,
    ],
    "scripts/lib/worktree-identity.test.mjs": [
      `${DEMO}/midgard-node/tests/worktree-identity.ts`,
    ],
    "scripts/preflight/deployment-profile-inputs.test.mjs": [
      ".github/workflows/aiken-ci.yml",
      `${DEMO}/package.json`,
      `${DEMO}/midgard-core/package.json`,
      ...PREFLIGHT_IMPORTS,
      ...deploymentProfileInputs(root),
    ],
    "scripts/preflight/run.test.mjs": PREFLIGHT_IMPORTS,
    "scripts/preflight/sdk-suite-evidence.test.mjs": PREFLIGHT_IMPORTS,
    "scripts/release/docker-build-plan.test.mjs": [
      `${DEMO}/*/Dockerfile`,
      `${DEMO}/*/package.json`,
      `${DEMO}/*/tsup.config.ts`,
      PACKAGE_TASKS,
    ],
    "scripts/release/pack-files.test.mjs": ["LICENSE"],
    "scripts/release/sdk-packages.test.mjs": [
      "LICENSE",
      `${DEMO}/package.json`,
      `${DEMO}/scripts/lib/vendored-imports.mjs`,
    ],
  };
};

const toolingTestsCheck = (root) => {
  const inputs = toolingTestInputs(root);
  return {
    id: "repo-tooling-tests",
    title:
      "Repository tooling self-tests (the checks that prove the other checks can fail)",
    triggers: [...TOOLING_TRIGGERS, ...new Set(Object.values(inputs).flat())],
    triggerNote: `${TOOLING_TRIGGERS.map((path) => `\`${path}\``).join(", ")}; a file outside them that a self-test declares it reads (\`toolingTestInputs\` in \`scripts/preflight/registry.tooling-checks.mjs\`) runs only the self-tests that read it`,
    testInputs: inputs,
    prePush: true,
    display: `node --test "${TOOLING_TESTS}"`,
    plan: ({ matched = [], full = false } = {}) => {
      const readers = Object.keys(inputs).filter((test) =>
        matched.some((path) => matchesAny(path, inputs[test])),
      );
      const whole =
        full ||
        readers.length === 0 ||
        matched.some((path) => matchesAny(path, TOOLING_TRIGGERS));
      return [step(["node", "--test", ...(whole ? [TOOLING_TESTS] : readers)])];
    },
  };
};

// demo/scripts/check-module-size-exceptions.mjs holds each listed file to its
// recorded line count, so an edit of one of them can fail it. Entries are
// relative to demo/; those outside it are spelled `../<path>`.
const MODULE_SIZE_CAPS = `${DEMO}/module-size-exceptions.json`;
const moduleSizeCapped = (root) => [
  MODULE_SIZE_CAPS,
  ...JSON.parse(readFileSync(resolve(root, MODULE_SIZE_CAPS), "utf8")).map(
    ({ file }) => posix.normalize(`${DEMO}/${file}`),
  ),
];

export const toolingChecks = (root) => [
  {
    id: "contributor-build-guards",
    title: "Workspace builds use the resource and provenance guard",
    triggers: [
      "scripts/contrib/**",
      "scripts/contrib.mjs",
      "demo/*/package.json",
    ],
    prePush: true,
    display: "node scripts/contrib/enroll-builds.mjs",
    fix: "node scripts/contrib/enroll-builds.mjs --write",
    plan: () => [step(node("scripts/contrib/enroll-builds.mjs"))],
  },
  {
    id: "contributor-causal-controls",
    title: "Artifact and boundary guards reject their causal mutants",
    triggers: ["scripts/contrib/**", "scripts/contrib.mjs"],
    prePush: true,
    display: "node scripts/contrib/review-controls.mjs",
    plan: () => [step(node("scripts/contrib/review-controls.mjs"))],
  },
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
    // Any deletion or rename can leave a registry naming nothing, so it runs
    // on every push; it reads files only and takes about two seconds.
    id: "registry-paths",
    title: "Registries name only files that exist",
    always: true,
    prePush: true,
    display: "node scripts/ci/check-registry-paths.mjs",
    plan: () => [step(node("scripts/ci/check-registry-paths.mjs"))],
  },
  {
    id: "toolchain-pins",
    title: "Docker, compose and CI version pins match demo/toolchain.json",
    // A superset of the files toolchain.mjs enrolls, kept out of the runner's
    // import closure.
    triggers: [
      "demo/toolchain.json",
      "demo/.nvmrc",
      "demo/*/Dockerfile",
      "demo/**/docker-compose*.yaml",
      "demo/midgard-node/.env.example",
      "demo/l1-node-transport/native/go.mod",
      "demo/midgard-node-tools/src/full-stack/prerequisites.ts",
      "demo/midgard-node-tools/devnet/phase4-process/**",
      ".github/workflows/**",
      "scripts/release/toolchain.mjs",
    ],
    prePush: true,
    display: "node scripts/release/toolchain.mjs",
    plan: () => [step(node("scripts/release/toolchain.mjs"))],
  },
  toolingTestsCheck(root),
  {
    id: "demo-lint",
    title: "Workspace ESLint rules and their reasoned baseline",
    triggers: [
      "demo/**/*.{ts,tsx,js,mjs,cjs,json}",
      ".github/workflows/midgard-node-ci.yml",
    ],
    capabilities: ["node-modules"],
    display: "pnpm --dir demo run lint",
    plan: () => [step(["pnpm", "--dir", DEMO, "run", "lint"])],
  },
  {
    id: "demo-script-tests",
    title:
      "Workspace helper, ESLint plugin, deployment-profile and interactive-emulator cache self-tests",
    triggers: [
      "demo/scripts/**",
      "demo/eslint.config.mjs",
      ".github/workflows/repo-tools-ci.yml",
      ...DEMO_SCRIPT_TEST_INPUTS,
      ...moduleSizeCapped(root),
    ],
    triggerNote: `\`demo/scripts/**\`, \`demo/eslint.config.mjs\`, \`.github/workflows/repo-tools-ci.yml\`, ${DEMO_SCRIPT_TEST_INPUTS.map((path) => `\`${path}\``).join(", ")}, \`demo/module-size-exceptions.json\` and every file it caps`,
    capabilities: ["node-modules"],
    display: `node --test "demo/scripts/lib/*.test.mjs" ${DEMO_SCRIPT_TESTS.join(" ")}`,
    plan: () => [
      step([
        "node",
        "--test",
        "demo/scripts/lib/*.test.mjs",
        ...DEMO_SCRIPT_TESTS,
      ]),
    ],
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
    title: "Workflow trigger table and the by-id runner's path filters",
    // The runner-filter test walks the by-id runner's imports, so a change
    // to any module it could reach selects it.
    triggers: [
      ".github/workflows/**",
      "scripts/ci/**",
      "scripts/preflight.mjs",
      "scripts/preflight/**",
      "scripts/contrib/**",
      "scripts/lib/**",
      "demo/scripts/lib/blueprint-stamp.mjs",
      "demo/scripts/assert-midgard-core-dist-current.mjs",
      "demo/midgard-core/scripts/write-dist-source-digest.mjs",
      "onchain/aiken/scripts/**",
    ],
    requiresFiles: [
      "scripts/ci/workflow-triggers.test.mjs",
      "scripts/ci/preflight-runner-filters.test.mjs",
    ],
    prePush: true,
    display:
      "node --test scripts/ci/workflow-triggers.test.mjs scripts/ci/preflight-runner-filters.test.mjs",
    plan: () => [
      step([
        "node",
        "--test",
        "scripts/ci/workflow-triggers.test.mjs",
        "scripts/ci/preflight-runner-filters.test.mjs",
      ]),
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
    [
      "agent-config",
      "CLAUDE.md files route to AGENTS.md; shared settings stay allowlisted",
      "scripts/agents/check-agent-config.mjs",
      [".claude/settings.json"],
    ],
  ].map(([id, title, script, extraTriggers = []]) => ({
    id,
    title,
    triggers: [...AGENT_FILES, "scripts/agents/**", ...extraTriggers],
    requiresFiles: [script],
    prePush: true,
    display: `node ${script}`,
    plan: () => [step(node(script))],
  })),
  {
    // The runbook names CLI commands, the stack's steps and its stop messages,
    // so a renamed command, step or message breaks it too.
    id: "e2e-runbook",
    title: "The e2e acceptance runbook matches the CLIs it drives",
    triggers: [
      `${E2E_SKILL}/**`,
      "demo/midgard-node/src/index*.ts",
      "demo/midgard-node/src/commands/prepare-hub-oracle-nonce.resume-signed.ts",
      "demo/midgard-node-tools/src/**",
      "demo/midgard-node-tools/docs/PREPROD_STACK.md",
      "demo/midgard-node-tools/package.json",
    ],
    requiresFiles: [`${E2E_SKILL}/scripts/validate-runbook.mjs`],
    prePush: true,
    display: `node ${E2E_SKILL}/scripts/validate-runbook.mjs`,
    plan: () => [step(node(`${E2E_SKILL}/scripts/validate-runbook.mjs`))],
  },
];
