import {
  AIKEN_PROJECT,
  DEMO,
  goldenChannels,
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
      "demo/midgard-node/src/utils/commit-submission*.ts",
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
      "node --test demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs",
    plan: () => [
      step(
        node(
          "--test",
          "demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs",
        ),
      ),
    ],
  },
];

// --- repository tooling, CI and agent docs -------------------------------

const E2E_SKILL = ".agents/skills/midgard-e2e-acceptance";

export const toolingChecks = () => [
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
    id: "repo-tooling-tests",
    title:
      "Repository tooling self-tests (the checks that prove the other checks can fail)",
    triggers: ["scripts/**", ".githooks/**", ".claude/settings.json"],
    prePush: true,
    display: 'node --test "scripts/**/*.test.mjs"',
    plan: () => [step(["node", "--test", "scripts/**/*.test.mjs"])],
  },
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
    title: "Workspace helper and ESLint plugin self-tests",
    triggers: [
      "demo/scripts/**",
      "demo/eslint.config.mjs",
      ".github/workflows/repo-tools-ci.yml",
    ],
    capabilities: ["node-modules"],
    display: 'node --test "demo/scripts/lib/*.test.mjs"',
    plan: () => [step(["node", "--test", "demo/scripts/lib/*.test.mjs"])],
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
    [
      "agent-config",
      "CLAUDE.md files route to AGENTS.md; shared settings stay allowlisted",
      "scripts/agents/check-agent-config.mjs",
      [".claude/settings.json"],
    ],
  ].map(([id, title, script, extraTriggers = []]) => ({
    id,
    title,
    triggers: [
      "docs/**",
      "**/AGENTS.md",
      "**/CLAUDE.md",
      "CONTEXT.md",
      ".agents/**",
      "scripts/agents/**",
      ...extraTriggers,
    ],
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
