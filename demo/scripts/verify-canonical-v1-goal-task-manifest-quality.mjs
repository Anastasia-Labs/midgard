#!/usr/bin/env node

// F05 quality gate. Unlike goal:tasks:ready this is deliberately gating: it
// rejects a manifest that cannot safely be assigned from the authoritative
// first task queue in GOAL_PROGRESS.md. Historical queue snapshots later in
// that file are provenance, not present scheduling authority.

import { existsSync, readFileSync, readdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const args = process.argv.slice(2);
const manifestArgument = args.find((argument) =>
  argument.startsWith("--manifest-under-test="),
);
const templatesArgument = args.find((argument) =>
  argument.startsWith("--templates-under-test="),
);
const manifestPath =
  manifestArgument === undefined
    ? resolve(
        repoRoot,
        "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
      )
    : resolve(manifestArgument.slice("--manifest-under-test=".length));
const templatesRoot =
  templatesArgument === undefined
    ? resolve(repoRoot, "docs/exec-plans/templates")
    : resolve(templatesArgument.slice("--templates-under-test=".length));
const ledgerPath = resolve(repoRoot, "GOAL_PROGRESS.md");
const goalSpecPath = resolve(repoRoot, "GOAL_SPEC.md");
const coverageMatrixPath = resolve(
  repoRoot,
  "docs/fault-proofs/coverage-matrix.md",
);
const watcherDependencyMapPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
);
const w26FocusedTestPath = resolve(
  repoRoot,
  "demo/midgard-watcher/tests/event-classification-verifier.test.ts",
);
const watcherFocusedGatePath = resolve(
  repoRoot,
  "demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs",
);
const templatePaths = {
  readme: resolve(templatesRoot, "README.md"),
  manifestRow: resolve(templatesRoot, "task-manifest-row.json"),
  necessity: resolve(templatesRoot, "necessity-artifact-template.md"),
  structuralNA: resolve(templatesRoot, "structural-na-template.md"),
  assignment: resolve(templatesRoot, "subagent-assignment-brief.md"),
};
const EXPECTED_TEMPLATE_FILES = new Set([
  "README.md",
  "necessity-artifact-template.md",
  "structural-na-template.md",
  "subagent-assignment-brief.md",
  "task-manifest-row.json",
]);
const EXPECTED_TASKS = 186;
const VALID_SIZES = new Set(["S", "M", "L", "XL"]);
const VALID_RISKS = new Set(["low", "medium", "high"]);

if (
  args.some(
    (argument) =>
      argument !== "--json" &&
      argument !== manifestArgument &&
      argument !== templatesArgument,
  ) ||
  (manifestArgument === undefined) !== (templatesArgument === undefined)
) {
  process.stderr.write(
    "usage: verify-canonical-v1-goal-task-manifest-quality.mjs [--json] [--manifest-under-test=<absolute-path> --templates-under-test=<absolute-directory>]\n",
  );
  process.exit(2);
}
const asJson = args.includes("--json");

const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const tasks = Array.isArray(manifest.tasks) ? manifest.tasks : [];
const ledgerLines = readFileSync(ledgerPath, "utf8").split("\n");
const goalSpecLines = readFileSync(goalSpecPath, "utf8").split("\n");
const coverageMatrixLines = readFileSync(coverageMatrixPath, "utf8").split(
  "\n",
);
const watcherDependencyMap = JSON.parse(
  readFileSync(watcherDependencyMapPath, "utf8"),
);
const w26FocusedTestText = readFileSync(w26FocusedTestPath, "utf8");
const watcherFocusedGateText = readFileSync(watcherFocusedGatePath, "utf8");

const sectionSevenStart = goalSpecLines.findIndex((line) =>
  /^##\s+7\./.test(line),
);
const sectionElevenStart = goalSpecLines.findIndex(
  (line, index) => index > sectionSevenStart && /^##\s+11\./.test(line),
);
if (sectionSevenStart === -1 || sectionElevenStart === -1) {
  throw new Error(`${goalSpecPath} lacks a bounded §7-§10 task region`);
}
const expectedTaskIdList = goalSpecLines
  .slice(sectionSevenStart, sectionElevenStart)
  .map((line) => line.match(/^\|\s*([A-Z][A-Z0-9]*(?:-[0-9])?)\s*\|/)?.[1])
  .filter((id) => id !== undefined && id !== "ID");
const expectedTaskIds = new Set(expectedTaskIdList);

const queueStart = ledgerLines.findIndex((line) =>
  /^##\s+Task queue\s*$/i.test(line),
);
if (queueStart === -1) {
  throw new Error(`${ledgerPath} has no authoritative first Task queue`);
}
const queueEnd = ledgerLines.findIndex(
  (line, index) => index > queueStart && /^##\s/.test(line),
);
const queueRows = ledgerLines
  .slice(queueStart, queueEnd === -1 ? ledgerLines.length : queueEnd)
  .filter((line) => line.trimStart().startsWith("|"))
  .map((line) =>
    line
      .trim()
      .replace(/^\|/, "")
      .replace(/\|$/, "")
      .split("|")
      .map((cell) => cell.trim()),
  );
const header = queueRows[0] ?? [];
const taskColumn = header.findIndex((cell) => /^task$/i.test(cell));
const statusColumn = header.findIndex((cell) => /^status$/i.test(cell));
if (taskColumn === -1 || statusColumn === -1) {
  throw new Error(`${ledgerPath} Task queue lacks Task and Status columns`);
}

const ledgerStatus = new Map();
for (const row of queueRows.slice(1)) {
  const rawId = (row[taskColumn] ?? "").replace(/[`*]/g, "").trim();
  const status = (row[statusColumn] ?? "").replace(/[`*]/g, "").trim();
  if (rawId === "" || /^-+$/.test(rawId) || status === "") continue;
  for (const id of rawId.split("/")) {
    const normalized = id.trim();
    if (normalized !== "" && !ledgerStatus.has(normalized)) {
      ledgerStatus.set(normalized, status);
    }
  }
}

// Queue statuses are decorated in the ledger (`PASS (structural N/A)`,
// `PASS (LOCAL_PASS; Q57/QG3 owns LIVE)`), so every status-keyed rule below
// classifies by the leading role token instead of comparing the whole cell to
// "PASS" — string equality silently excludes decorated rows from the rules
// that are supposed to constrain them.
const statusRole = (status) =>
  (status ?? "")
    .replace(/[`*]/g, "")
    .trim()
    .split(/[\s(;,]/)[0]
    .toUpperCase();
const isPassStatus = (status) => statusRole(status) === "PASS";
// A dependency is a *current* non-PASS blocker only when the authoritative
// first queue actually records a non-PASS status for it. IDs with no queue row
// are unscheduled, not measured, and are not counted as current blockers.
const isCurrentNonPassId = (id) =>
  ledgerStatus.has(id) && !isPassStatus(ledgerStatus.get(id));
// Derived blocked-on authority: the manifest's own blockedOn contents joined
// against those queue statuses. Published claims about the blocked-on set are
// reconciled against this derivation rather than trusted as prose.
const currentNonPassBlockers = [
  ...new Set(
    tasks.flatMap((task) =>
      (Array.isArray(task?.blockedOn) ? task.blockedOn : []).filter(
        isCurrentNonPassId,
      ),
    ),
  ),
].sort();
const currentNonPassDependenciesOf = (task) =>
  [
    ...new Set([
      ...(Array.isArray(task?.dependsOn) ? task.dependsOn : []),
      ...(Array.isArray(task?.blockedOn) ? task.blockedOn : []),
    ]),
  ]
    .filter(isCurrentNonPassId)
    .sort();

const findings = new Map();
const add = (category, taskId, detail) => {
  const entries = findings.get(category) ?? [];
  entries.push({ id: taskId, detail });
  findings.set(category, entries);
};
const hasText = (value) => typeof value === "string" && value.trim() !== "";
const idOf = (task, index) => (hasText(task?.id) ? task.id : `#${index + 1}`);
const quoteForRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
const contentPlaceholder =
  /<(?:module|focused|line|exact[^>]*|new[^>]*|paste[^>]*|what[^>]*|[a-z][a-z0-9-]*\.\.\.)>|\b(?:TBD|TODO|FIXME)\b/;
const countUnit =
  "tests?|selectors?|cases?|rows?|tasks?|IDs?|paths?|commands?|entries|features?|classes|checks?|assertions?|scenarios?|routes?|records?|inventories|artifacts?|modules?|validators?|categories|stages?|transitions?|controls?|fixtures?|providers?|processes?|sections?|snapshots?|policies|digests?|files?|milestones?|groups?|outcomes?|positions?|statuses|values|equations|consumers|services?|views?|families|lifecycles|materializations?|negatives?";
const numericCountClaims = (value) => [
  ...value.matchAll(
    new RegExp(
      `\\b([1-9]\\d*)(?:\\s*\\/\\s*([1-9]\\d*))?\\s+(?:[A-Za-z0-9_./()=–-]+\\s+){0,4}(?:${countUnit})\\b`,
      "gi",
    ),
  ),
];
const hasExactCountSyntax = (value) =>
  /\bexact(?:ly)?\s+[1-9]\d*(?:\s*\/\s*[1-9]\d*)?/i.test(value) ||
  /\b[1-9]\d*\s*\/\s*[1-9]\d*\b/.test(value) ||
  /(?:^|[;,])\s*[^;,:]+:\s*[1-9]\d*\b/.test(value);
const hasSubstantiveCountContract = (value) => {
  const claims = numericCountClaims(value);
  const exactValues = [...value.matchAll(/\bexact(?:ly)?\s+([1-9]\d*)/gi)].map(
    (claim) => Number(claim[1]),
  );
  const values = claims.flatMap((claim) =>
    [claim[1], claim[2]].filter(Boolean).map(Number),
  );
  return (
    claims.length >= 2 ||
    [...values, ...exactValues].some((count) => count > 1) ||
    exactValues.length >= 2 ||
    /\b[2-9]\d*\s*\/\s*[2-9]\d*\b/.test(value)
  );
};
const commandHasFocusedSelector = (command) =>
  /\b(?:vitest|jest)\s+run\s+[^\n]*\.(?:test|spec)\.[cm]?[jt]sx?\b/.test(
    command,
  ) ||
  /\bscripts\/run-focused-check\.mjs\s+\S+\s+\S+/.test(command) ||
  /\bnode\s+(?:demo\/)?scripts\/[A-Za-z0-9_./-]+\.mjs\b/.test(command) ||
  /\b(?:prettier|git\s+diff)\b[^\n]*\.(?:json|md|mjs|ts|ak)\b/.test(command);
const commandClauses = (command) => command.split(/\s*\|\|\s*/);
const clauseBase = (clause) => {
  const packageDirectory = clause.match(/\bpnpm\s+--dir\s+(\S+)/)?.[1];
  if (packageDirectory !== undefined)
    return resolve(repoRoot, packageDirectory);
  const changedDirectory = clause.match(/^\s*cd\s+(\S+)\s+&&/)?.[1];
  return changedDirectory === undefined
    ? repoRoot
    : resolve(repoRoot, changedDirectory);
};
const commandFileSelectors = (command) =>
  commandClauses(command).flatMap((clause) => {
    const base = clauseBase(clause);
    return [
      ...clause.matchAll(
        /(?:^|\s)(\.\.?\/[A-Za-z0-9_./-]+|[A-Za-z0-9_.-]+\/[A-Za-z0-9_./-]+|[A-Za-z0-9_.-]+)\.(?:ak|json|md|mjs|toml|tsx?|ya?ml)\b/g,
      ),
    ].map((match) => {
      const selector = normalizePath(
        match[1] + "." + match[0].split(".").at(-1),
      );
      const repoRelative =
        /^(?:demo|docs|onchain|technical-spec|\.github)\//.test(selector)
          ? selector
          : normalizePath(resolve(base, selector).slice(repoRoot.length + 1));
      return { selector, repoRelative };
    });
  });
const primaryCommandBindings = (command) =>
  commandClauses(command).flatMap((clause) => {
    const bindings = [];
    for (const match of clause.matchAll(/\b(?:vitest|jest)\s+run\s+([^|]+)/g)) {
      for (const file of match[1].matchAll(
        /(?:^|\s)([A-Za-z0-9_./-]+\.(?:test|spec)\.[cm]?[jt]sx?)\b/g,
      )) {
        bindings.push(file[1].split("/").at(-1));
      }
    }
    const focusedModule = clause.match(
      /\bscripts\/run-focused-check\.mjs\s+(\S+)/,
    )?.[1];
    if (focusedModule !== undefined) bindings.push(focusedModule);
    const aikenModule = clause.match(/\baiken\s+check\s+-m\s+(\S+)/)?.[1];
    if (aikenModule !== undefined) bindings.push(aikenModule);
    // scripts/guard-focused-selector.mjs is the fail-closed form of
    // `aiken check -m <selector>` (issue #523): it binds exactly the same
    // module selectors, so the count contract must still name them rather than
    // the wrapper's own filename.
    const guardedModules = clause.match(
      /\bscripts\/guard-focused-selector\.mjs\s+([^|]+)/,
    )?.[1];
    if (guardedModules !== undefined) {
      for (const selector of guardedModules.match(/[a-z0-9_][a-z0-9_/]*/g) ??
        []) {
        bindings.push(selector);
      }
    }
    const nodeScript = clause.match(/\bnode\s+([A-Za-z0-9_./-]+\.mjs)\b/)?.[1];
    if (
      nodeScript !== undefined &&
      !nodeScript.endsWith("run-focused-check.mjs") &&
      !nodeScript.endsWith("guard-focused-selector.mjs") &&
      !nodeScript.endsWith("verify-normalized-format.mjs")
    ) {
      bindings.push(nodeScript.split("/").at(-1));
    }
    const taskSelector = clause.match(
      /(?:^|\s)--(?:only|task)\s+([A-Z][A-Z0-9-]+)\b/,
    )?.[1];
    if (taskSelector !== undefined) bindings.push(taskSelector);
    return bindings;
  });
const hasExactSourceAnchor = (anchor, taskId) =>
  hasText(anchor) &&
  ((/GOAL_SPEC\.md(?::\d+|\s+(?:§\s*)?\d+(?:\.\d+)?)/.test(anchor) &&
    (anchor.includes(taskId) || /:\d+/.test(anchor))) ||
    /(?:^|\s)(?:demo|onchain|docs|technical-spec|\.github)\/[A-Za-z0-9_./-]+(?:\.[A-Za-z0-9_-]+)?\b/.test(
      anchor,
    ));
const hasImplementationAnchor = (anchor) =>
  hasText(anchor) &&
  /(?:^|\s)(?:demo|onchain)\/[A-Za-z0-9_./-]+(?:\.(?:ak|mjs|tsx?))?\b/.test(
    anchor,
  );
const hasExplicitEvidenceItemBreakdown = (value) => {
  const claims = numericCountClaims(value);
  const namedClaims = claims.filter((claim) =>
    /\b[A-Za-z0-9]+-[A-Za-z0-9-]+\b/.test(claim[0]),
  );
  return claims.length >= 3 && namedClaims.length >= 2 && value.includes(":");
};

if (tasks.length !== EXPECTED_TASKS) {
  add(
    "taskCardinality",
    "manifest",
    `expected ${EXPECTED_TASKS}, found ${tasks.length}`,
  );
}
const taskIds = new Map();
for (const [index, task] of tasks.entries()) {
  const id = idOf(task, index);
  const occurrences = taskIds.get(id) ?? [];
  occurrences.push(index + 1);
  taskIds.set(id, occurrences);
  if (!hasText(task?.id)) add("missingTaskId", id, "task id is empty");
}
for (const [id, occurrences] of taskIds) {
  if (occurrences.length > 1) {
    add("duplicateTaskId", id, `manifest positions ${occurrences.join(", ")}`);
  }
}
if (
  expectedTaskIdList.length !== EXPECTED_TASKS ||
  expectedTaskIds.size !== EXPECTED_TASKS
) {
  add(
    "authoritativeTaskSetInvalid",
    "GOAL_SPEC",
    `§7-§10 yields ${expectedTaskIdList.length} rows and ${expectedTaskIds.size} unique IDs; expected ${EXPECTED_TASKS}`,
  );
}
for (const expectedId of expectedTaskIds) {
  if (!taskIds.has(expectedId)) {
    add("missingAuthoritativeTaskId", expectedId, "missing from manifest");
  }
}
for (const actualId of taskIds.keys()) {
  if (!actualId.startsWith("#") && !expectedTaskIds.has(actualId)) {
    add("unexpectedTaskId", actualId, "not a GOAL_SPEC §7-§10 task ID");
  }
}

// These are unresolved lower-case template tokens, such as `<module>` and
// `<exact-test-names...>`. This deliberately does not match numeric
// comparison prose (`<= 48 hours`, `> 0`) or source-language type parameters
// such as `<Uint8Array>`.
const placeholderToken =
  /<(?:module|focused|line|exact[^>]*|new[^>]*|[a-z][a-z0-9-]*\.\.\.)>/;
const operationalTextFields = [
  "acceptance",
  "dependsOnRaw",
  "readyBecause",
  "blockedBecause",
  "schedulingNote",
  "expectedNonzeroCounts",
];
const operationalArrayFields = [
  "writablePaths",
  "pathsMustNotTouch",
  "sourceAnchors",
  "evidenceOutputs",
  "focusedCommands",
  "invalidationTriggers",
];
const requiredArrayFields = new Set(operationalArrayFields);
const structuralNATasks = new Map([
  [
    "Q32",
    /(?:fraud-proofs\/req-signer-set|validators\/fraud-proofs\/req-signer-set|req-signer-set\*\.ts)/i,
  ],
  [
    "Q43",
    /(?:fraud-proofs\/l2-tx-mistag|validators\/fraud-proofs\/l2-tx-mistag|l2-tx-mistag\*\.ts)/i,
  ],
]);
const normalizePath = (value) =>
  value
    .trim()
    .replaceAll("\\", "/")
    .replace(/^\.\/+/, "")
    .replace(/\/{2,}/g, "/");
const nonExactWritablePath = /[*?\[\]{}]|\bthrough\b|\(\s*new\s*\)/i;
const allPlannedWritablePaths = new Set(
  tasks.flatMap((task) =>
    Array.isArray(task?.writablePaths)
      ? task.writablePaths
          .filter((path) => hasText(path) && !nonExactWritablePath.test(path))
          .map(normalizePath)
      : [],
  ),
);

for (const [index, task] of tasks.entries()) {
  const id = idOf(task, index);
  for (const field of ["section", "title", "acceptance", "dependsOnRaw"]) {
    const value = task?.[field];
    if (!hasText(value)) {
      add("missingRequiredTaskContent", id, `${field} is empty`);
    } else if (contentPlaceholder.test(value)) {
      add("placeholderTaskContent", id, field);
    }
  }
  if (!VALID_SIZES.has(task?.size)) {
    add("invalidSize", id, String(task?.size ?? "missing"));
  }
  if (!VALID_RISKS.has(task?.risk)) {
    add("invalidRisk", id, String(task?.risk ?? "missing"));
  }
  if (!Array.isArray(task?.dependsOn)) {
    add("missingOrNonArrayField", id, "dependsOn");
  } else {
    for (const dependency of task.dependsOn) {
      if (!hasText(dependency) || !expectedTaskIds.has(dependency)) {
        add("invalidDependency", id, String(dependency));
      } else if (dependency === id) {
        add("selfDependency", id, dependency);
      }
    }
  }
  if (task?.detailStatus !== "DETAILED") {
    add(
      "nonDetailed",
      id,
      `detailStatus=${String(task?.detailStatus ?? "missing")}`,
    );
  }
  for (const field of operationalTextFields) {
    const value = task?.[field];
    if (
      typeof value === "string" &&
      (placeholderToken.test(value) || contentPlaceholder.test(value))
    ) {
      add(
        "placeholderToken",
        id,
        `${field} contains unresolved template content`,
      );
    }
  }
  for (const field of operationalArrayFields) {
    const value = task?.[field];
    if (!Array.isArray(value)) {
      if (requiredArrayFields.has(field))
        add("missingOrNonArrayField", id, field);
      continue;
    }
    if (value.length === 0) add("emptyOperationalArray", id, field);
    for (const entry of value) {
      if (typeof entry !== "string" || entry.trim() === "") {
        add("invalidOperationalArrayEntry", id, `${field} has non-text entry`);
      } else if (
        placeholderToken.test(entry) ||
        contentPlaceholder.test(entry)
      ) {
        add(
          "placeholderToken",
          id,
          `${field} contains unresolved template content`,
        );
      }
    }
  }
  if (
    Array.isArray(task?.sourceAnchors) &&
    !task.sourceAnchors.some((anchor) => hasExactSourceAnchor(anchor, id))
  ) {
    add(
      "missingExactSourceAnchor",
      id,
      "no path/symbol anchor or GOAL_SPEC section plus task/line anchor",
    );
  }
  const leasesImplementation = Array.isArray(task?.writablePaths)
    ? task.writablePaths.some(
        (path) =>
          hasText(path) &&
          /^(?:demo|onchain)\/.*\.(?:ak|mjs|tsx?)$/.test(normalizePath(path)),
      )
    : false;
  if (
    leasesImplementation &&
    Array.isArray(task?.sourceAnchors) &&
    !task.sourceAnchors.some(hasImplementationAnchor)
  ) {
    add(
      "missingImplementationSourceAnchor",
      id,
      "implementation lease requires an implementation/current-tree anchor",
    );
  }
  if (
    Array.isArray(task?.evidenceOutputs) &&
    !task.evidenceOutputs.some((entry) => hasText(entry) && entry.length >= 20)
  ) {
    add("missingConcreteEvidenceOutput", id, "no concrete evidence output");
  }
  if (
    Array.isArray(task?.invalidationTriggers) &&
    !task.invalidationTriggers.some(
      (entry) =>
        hasText(entry) &&
        /\bchanges?\b|\bchanged?\b|drift|invalidat|diverge|appear/i.test(entry),
    )
  ) {
    add("missingConcreteInvalidationTrigger", id, "no change condition");
  }

  // Assignment leases must resolve to exact paths. Protected paths are
  // deliberately excluded: a parent may protect a directory or a class of
  // generated files, but a worker must never be assigned such an open set.
  if (Array.isArray(task?.writablePaths)) {
    const seenWritablePaths = new Map();
    const protectedPaths = new Set(
      Array.isArray(task?.pathsMustNotTouch)
        ? task.pathsMustNotTouch
            .filter((entry) => typeof entry === "string")
            .map(normalizePath)
        : [],
    );
    for (const path of task.writablePaths) {
      if (typeof path !== "string" || path.trim() === "") continue;
      const normalizedPath = normalizePath(path);
      if (nonExactWritablePath.test(normalizedPath)) {
        add("nonExactWritablePath", id, path);
      }
      const firstPath = seenWritablePaths.get(normalizedPath);
      if (firstPath !== undefined) {
        add("duplicateWritablePath", id, `${path} duplicates ${firstPath}`);
      } else {
        seenWritablePaths.set(normalizedPath, path);
      }
      if (protectedPaths.has(normalizedPath)) {
        add("writableProtectedOverlap", id, path);
      }
    }
  }

  const focusedCommands = Array.isArray(task?.focusedCommands)
    ? task.focusedCommands.filter((command) => typeof command === "string")
    : [];
  for (const command of focusedCommands) {
    if (/\baiken\s+fmt\s+--check\b/.test(command)) {
      add("invalidRawAikenFormatCommand", id, command);
    }
  }
  const primaryBindings = [
    ...new Set(focusedCommands.flatMap(primaryCommandBindings)),
  ];
  if (primaryBindings.length === 0) {
    add(
      "missingFocusedFileSelector",
      id,
      "no command selects an exact test/module/verifier file",
    );
  }
  const exactWritablePaths = new Set(
    Array.isArray(task?.writablePaths)
      ? task.writablePaths
          .filter((path) => hasText(path) && !nonExactWritablePath.test(path))
          .map(normalizePath)
      : [],
  );
  const writableFileNames = new Set(
    [...exactWritablePaths].map((path) => path.split("/").at(-1)),
  );
  const hasWritableSelector = primaryBindings.some(
    (binding) => writableFileNames.has(binding) || binding === id,
  );
  if (!hasWritableSelector) {
    add(
      "focusedCommandOutsideWritableLease",
      id,
      "no focused command selects an exact writable path",
    );
  }
  for (const { selector, repoRelative } of focusedCommands.flatMap(
    commandFileSelectors,
  )) {
    if (existsSync(resolve(repoRoot, repoRelative))) continue;
    if (!allPlannedWritablePaths.has(repoRelative)) {
      add(
        "missingPlannedCommandSelectorOutsideLease",
        id,
        `${selector} resolves to missing ${repoRelative}, which is not writablePaths`,
      );
    }
  }
  const exactAikenPaths = Array.isArray(task?.writablePaths)
    ? task.writablePaths
        .filter((path) => typeof path === "string")
        .map(normalizePath)
        .filter(
          (path) => path.endsWith(".ak") && !nonExactWritablePath.test(path),
        )
    : [];
  if (exactAikenPaths.length > 0) {
    const normalizedFormatArguments = new Set();
    for (const command of focusedCommands) {
      if (!/\bscripts\/verify-normalized-format\.mjs\b/.test(command)) {
        continue;
      }
      for (const match of command.matchAll(/\b[A-Za-z0-9_./-]+\.ak\b/g)) {
        normalizedFormatArguments.add(normalizePath(match[0]));
      }
    }
    const missingPaths = exactAikenPaths.filter((path) => {
      const projectRelativePath = path.startsWith("onchain/aiken/")
        ? path.slice("onchain/aiken/".length)
        : path;
      return (
        !normalizedFormatArguments.has(path) &&
        !normalizedFormatArguments.has(projectRelativePath)
      );
    });
    if (missingPaths.length > 0) {
      add("missingNormalizedAikenFormatCommand", id, missingPaths.join(", "));
    }
  }

  const countContract = task?.expectedNonzeroCounts;
  if (!hasText(countContract)) {
    add("missingExpectedCountContract", id, "expectedNonzeroCounts is empty");
  } else {
    const positiveNumbers = [...countContract.matchAll(/\b([1-9]\d*)\b/g)].map(
      (match) => Number(match[1]),
    );
    if (positiveNumbers.length === 0) {
      add(
        "nonpositiveExpectedCountContract",
        id,
        "no positive count is stated",
      );
    }
    if (!hasExactCountSyntax(countContract)) {
      add("nonexactExpectedCountContract", id, countContract);
    }
    if (!hasSubstantiveCountContract(countContract)) {
      add(
        "insubstantialExpectedCountContract",
        id,
        "requires multiple count claims or one exact positive count greater than 1",
      );
    }
    const unboundSelectors = primaryBindings.filter(
      (binding) => !countContract.includes(binding),
    );
    if (
      (primaryBindings.length === 0 || unboundSelectors.length > 0) &&
      !hasExplicitEvidenceItemBreakdown(countContract)
    ) {
      add(
        "unboundExpectedCountContract",
        id,
        primaryBindings.length === 0
          ? "no primary focused selector can bind the numeric contract"
          : `count contract does not name: ${unboundSelectors.join(", ")}`,
      );
    }
  }

  const blockedOn = task?.blockedOn;
  if (blockedOn !== undefined && !Array.isArray(blockedOn)) {
    add("missingOrNonArrayField", id, "blockedOn");
  }
  const declaredBlockers = Array.isArray(blockedOn) ? blockedOn : [];
  for (const dependency of declaredBlockers) {
    const status = ledgerStatus.get(dependency);
    if (status !== undefined && isPassStatus(status)) {
      add(
        "staleBlockedOnPass",
        id,
        `${dependency} is ${status} in first task queue`,
      );
    }
  }
  // The mirror rule: blockedOn must also be complete. A dependency the first
  // queue currently records as non-PASS cannot be dropped from blockedOn, so a
  // row cannot publish a smaller blocked-on set than its own dependencies and
  // the ledger jointly establish.
  for (const dependency of Array.isArray(task?.dependsOn)
    ? task.dependsOn
    : []) {
    if (
      isCurrentNonPassId(dependency) &&
      !declaredBlockers.includes(dependency)
    ) {
      add(
        "omittedCurrentBlocker",
        id,
        `${dependency} is ${ledgerStatus.get(dependency)} in first task queue but is absent from blockedOn`,
      );
    }
  }
  if (hasText(task?.blockedBecause)) {
    for (const [dependency, status] of ledgerStatus) {
      if (!isPassStatus(status)) continue;
      // A row's prose about its own queue status is never a blocker claim: no
      // task blocks itself, and self-referential notes legitimately discuss a
      // decorated status such as `PASS (LOCAL_PASS; Q57/QG3 owns LIVE)`.
      if (dependency === task?.id) continue;
      const dependencyPattern = new RegExp(
        `\\b${quoteForRegex(dependency)}\\b`,
      );
      if (!dependencyPattern.test(task.blockedBecause)) continue;
      const clauses = task.blockedBecause
        .split(/[.;]/)
        .filter((clause) => dependencyPattern.test(clause));
      if (
        clauses.some((clause) => {
          const passDisposition = new RegExp(
            `(?:\\bPASS\\b|omit|already|recorded|deliberately excluded|waiv)[^.;]{0,80}\\b${quoteForRegex(dependency)}\\b|\\b${quoteForRegex(dependency)}\\b[^.;]{0,80}(?:\\bPASS\\b|omit|already|recorded|deliberately excluded|waiv)`,
            "i",
          );
          return (
            /block|non-PASS|remain|wait|depend/i.test(clause) &&
            !passDisposition.test(clause)
          );
        })
      ) {
        add(
          "stalePassBlockerProse",
          id,
          `${dependency} is PASS but is described as a current blocker`,
        );
      }
    }
  }

  const forbiddenStandalonePath = structuralNATasks.get(id);
  if (forbiddenStandalonePath && Array.isArray(task?.writablePaths)) {
    for (const path of task.writablePaths) {
      if (forbiddenStandalonePath.test(path)) {
        add("structuralNANewDeployedFamilyLease", id, path);
      }
    }
  }
}

const actualTemplateFiles = readdirSync(templatesRoot).sort();
for (const file of actualTemplateFiles) {
  if (!EXPECTED_TEMPLATE_FILES.has(file)) {
    add("unexpectedTemplateArtifact", "templates", file);
  }
}
for (const file of EXPECTED_TEMPLATE_FILES) {
  if (!actualTemplateFiles.includes(file)) {
    add("missingTemplateArtifact", "templates", file);
  }
}

const templateText = Object.fromEntries(
  Object.entries(templatePaths).map(([name, path]) => [
    name,
    readFileSync(path, "utf8"),
  ]),
);
let goldenRow;
try {
  goldenRow = JSON.parse(templateText.manifestRow);
} catch (error) {
  add("invalidManifestRowTemplateJson", "template", String(error));
}
if (goldenRow !== undefined) {
  for (const field of [
    "id",
    "section",
    "title",
    "acceptance",
    "dependsOnRaw",
    "expectedNonzeroCounts",
  ]) {
    if (!hasText(goldenRow?.[field])) {
      add("invalidManifestRowTemplate", "template", `${field} is empty`);
    }
  }
  for (const field of [
    "dependsOn",
    "writablePaths",
    "pathsMustNotTouch",
    "sourceAnchors",
    "evidenceOutputs",
    "focusedCommands",
    "invalidationTriggers",
    "blockedOn",
  ]) {
    if (!Array.isArray(goldenRow?.[field]) || goldenRow[field].length === 0) {
      add("invalidManifestRowTemplate", "template", `${field} is empty`);
    }
  }
  if (goldenRow?.detailStatus !== "DETAILED") {
    add(
      "invalidManifestRowTemplate",
      "template",
      "detailStatus must be DETAILED",
    );
  }
  if (!VALID_SIZES.has(goldenRow?.size) || !VALID_RISKS.has(goldenRow?.risk)) {
    add("invalidManifestRowTemplate", "template", "invalid size or risk");
  }
  if (/\bREADY\b|<[^>]+>/.test(templateText.manifestRow)) {
    add(
      "invalidManifestRowTemplate",
      "template",
      "contains READY scheduling state or placeholder token",
    );
  }
  if (
    /\b[A-Z]{1,3}\d+(?:-[A-Z]{0,3}\d+|–[A-Z]{0,3}\d+)\b/.test(
      goldenRow?.dependsOnRaw ?? "",
    )
  ) {
    add(
      "invalidManifestRowTemplate",
      "template",
      "dependsOnRaw contains an unexpanded dependency range",
    );
  }
  for (const path of goldenRow?.writablePaths ?? []) {
    if (!hasText(path) || nonExactWritablePath.test(path)) {
      add("invalidManifestRowTemplatePath", "template", String(path));
    }
  }
  const goldenBindings = (goldenRow?.focusedCommands ?? []).flatMap(
    primaryCommandBindings,
  );
  if (
    goldenBindings.length === 0 ||
    !hasExactCountSyntax(goldenRow?.expectedNonzeroCounts ?? "") ||
    !hasSubstantiveCountContract(goldenRow?.expectedNonzeroCounts ?? "") ||
    goldenBindings.some(
      (binding) => !goldenRow.expectedNonzeroCounts.includes(binding),
    )
  ) {
    add(
      "invalidManifestRowTemplateVerification",
      "template",
      "focused selector or substantive exact count contract is missing",
    );
  }
  const goldenCommandText = (goldenRow?.focusedCommands ?? []).join("\n");
  if (
    !(goldenRow?.writablePaths ?? []).some((path) =>
      goldenCommandText.includes(normalizePath(path).split("/").at(-1)),
    )
  ) {
    add(
      "invalidManifestRowTemplateVerification",
      "template",
      "focused commands do not select a writable path",
    );
  }
}

const requiredTemplateMarkers = {
  readme: [
    "# Goal artifact templates (F05)",
    "exactly four worked artifacts",
    "detailStatus: DETAILED",
    "subagent-assignment-brief.md",
    "README.md is policy",
  ],
  necessity: [
    "# §3.2 Necessity artifact",
    "## Binding",
    "## Measurements",
    "## Exact limiting constraint",
    "## Preserved complete-item path",
    "excluded from evidence aggregation",
  ],
  structuralNA: [
    "# Structural N/A claim",
    "## Executable adversarial evidence",
    "## Removal",
    "Mutation control",
    "excluded from evidence aggregation",
  ],
  assignment: [
    "# Task assignment brief",
    "## Task",
    "## Lease",
    "## Required verification",
    "## Return",
    "excluded from evidence aggregation",
  ],
};
for (const [name, markers] of Object.entries(requiredTemplateMarkers)) {
  for (const marker of markers) {
    if (!templateText[name].includes(marker)) {
      add("invalidTemplateContract", name, `missing ${marker}`);
    }
  }
}
for (const [name, content] of Object.entries(templateText)) {
  if (
    /\bREADY\b|<[^>]+>|\*\*|[A-Za-z0-9_./-]+\?[A-Za-z0-9_./?*-]*/.test(content)
  ) {
    add(
      "unresolvedTemplateSyntax",
      name,
      "contains obsolete READY, placeholder, or wildcard syntax",
    );
  }
}
if (templateText.readme.includes("task-assignment-brief.md")) {
  add(
    "obsoleteTemplateFilename",
    "readme",
    "task-assignment-brief.md must not replace the retained subagent brief",
  );
}

const templateEvidenceReferences = tasks.flatMap((task) =>
  (task.evidenceOutputs ?? [])
    .filter(
      (entry) =>
        hasText(entry) &&
        /^docs\/exec-plans\/templates\/[A-Za-z0-9_.-]+$/.test(entry.trim()),
    )
    .map((entry) => ({ task: task.id, entry })),
);
for (const reference of templateEvidenceReferences) {
  add("templateUsedAsEvidence", reference.task, reference.entry);
}

// D1 waives CG3 only for W24, W25 and W26. W27 must preserve both serial
// dependencies, carry only dependencies that are not yet PASS as blockers,
// and say that the unwaived CG3 gate is re-evaluated before work.
const byId = new Map(tasks.map((task) => [task.id, task]));
const watcherFocusedGateCardinality = Number(
  watcherFocusedGateText.match(/expectedByFile\.size\s*!==\s*(\d+)/)?.[1],
);
const watcherFocusedCounts = [];
const collectWatcherFocusedCounts = (value) => {
  if (value === null || typeof value !== "object") return;
  for (const [key, entry] of Object.entries(value)) {
    if (key === "expectedFocusedTestCount") watcherFocusedCounts.push(entry);
    else collectWatcherFocusedCounts(entry);
  }
};
collectWatcherFocusedCounts(watcherDependencyMap.requiredWatcherPackage);
const watcherFocusedGateAggregate = watcherFocusedCounts.reduce(
  (total, count) => total + count,
  0,
);
if (
  watcherFocusedGateCardinality !== watcherFocusedCounts.length ||
  watcherFocusedGateCardinality !== 19 ||
  watcherFocusedGateAggregate !== 595
) {
  add(
    "watcherFocusedGateContractMismatch",
    "watcher-focused-gate",
    `source cardinality=${watcherFocusedGateCardinality}, dependency-map files=${watcherFocusedCounts.length}, aggregate=${watcherFocusedGateAggregate}; expected exact 19/595`,
  );
}
const taskStringValues = (value) => {
  if (typeof value === "string") return [value];
  if (Array.isArray(value)) return value.flatMap(taskStringValues);
  if (value !== null && typeof value === "object") {
    return Object.values(value).flatMap(taskStringValues);
  }
  return [];
};
const watcherGateClaimDefects = (value) => {
  const tokens = value
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .split(/\s+/);
  const hasWatcherGateContext =
    (tokens.includes("focused") && tokens.includes("gate")) ||
    tokens.includes("expectedbyfile") ||
    (tokens.includes("aggregate") && tokens.includes("watcher"));
  if (!hasWatcherGateContext) return [];
  const defects = [];
  for (const [index, token] of tokens.entries()) {
    if (!/^\d+$/.test(token)) continue;
    const count = Number(token);
    const unit = tokens[index + 1];
    if (
      (unit === "file" || unit === "files") &&
      count !== watcherFocusedGateCardinality
    ) {
      defects.push(
        `file claim ${count}, source requires ${watcherFocusedGateCardinality}`,
      );
    }
    if (
      tokens[index - 1] === "size" &&
      tokens.includes("expectedbyfile") &&
      count !== watcherFocusedGateCardinality
    ) {
      defects.push(
        `map-size claim ${count}, source requires ${watcherFocusedGateCardinality}`,
      );
    }
    if (unit === "aggregate") {
      const describesWatcherFiles =
        tokens[index + 2] === "watcher" &&
        ["file", "files"].includes(tokens[index + 3]);
      const required = describesWatcherFiles
        ? watcherFocusedGateCardinality
        : watcherFocusedGateAggregate;
      if (count !== required) {
        defects.push(
          `aggregate claim ${count}, authority requires ${required}`,
        );
      }
    }
    if (tokens[index - 1] === "aggregate") {
      const describesFiles =
        ["file", "files"].includes(unit) ||
        (unit === "watcher" && ["file", "files"].includes(tokens[index + 2]));
      const required = describesFiles
        ? watcherFocusedGateCardinality
        : watcherFocusedGateAggregate;
      if (count !== required) {
        defects.push(
          `aggregate claim ${count}, authority requires ${required}`,
        );
      }
    }
    if (
      (unit === "test" || unit === "tests") &&
      tokens
        .slice(Math.max(0, index - 5), index)
        .some((nearby) => ["aggregate", "total", "gate"].includes(nearby)) &&
      count !== watcherFocusedGateAggregate
    ) {
      defects.push(
        `test-total claim ${count}, map requires ${watcherFocusedGateAggregate}`,
      );
    }
    if (tokens[index - 2] === "rises" && tokens[index - 1] === "from") {
      defects.push(
        `relative old-total claim ${count} is not operational authority`,
      );
    }
  }
  return [...new Set(defects)];
};
for (const task of tasks) {
  const defects = taskStringValues(task).flatMap(watcherGateClaimDefects);
  for (const defect of [...new Set(defects)]) {
    add("watcherFocusedGateContractMismatch", task.id, defect);
  }
}
const w26ManifestTask = byId.get("W26");
const w26LiteralFocusedTestCount = [
  ...w26FocusedTestText.matchAll(/^\s*(?:it|test)\s*\(/gm),
].length;
const w26RegisteredFocusedTestCount =
  watcherDependencyMap?.requiredWatcherPackage?.eventClassificationVerifier
    ?.expectedFocusedTestCount;
if (
  w26LiteralFocusedTestCount !== w26RegisteredFocusedTestCount ||
  !hasText(w26ManifestTask?.expectedNonzeroCounts) ||
  !w26ManifestTask.expectedNonzeroCounts.includes(
    `exactly ${w26LiteralFocusedTestCount}/${w26LiteralFocusedTestCount} watcher cases`,
  )
) {
  add(
    "w26FocusedCountContractMismatch",
    "W26",
    `source=${w26LiteralFocusedTestCount}, registration=${String(w26RegisteredFocusedTestCount)}, manifest must bind exactly ${w26LiteralFocusedTestCount}/${w26LiteralFocusedTestCount}`,
  );
}
const f21 = byId.get("F21");
const expectedF21PartialIdentities = [
  {
    physicalLine: 298,
    concern: "Cross-block replay / spend of an already-spent input",
    remainingTask: "Q49-L298",
  },
  {
    physicalLine: 302,
    concern: "Malformed validity interval",
    remainingTask: "Q49-L302",
  },
];
const f21OperationalContract = [
  ...(f21?.evidenceOutputs ?? []),
  f21?.expectedNonzeroCounts,
  ...(f21?.invalidationTriggers ?? []),
  f21?.readyBecause,
]
  .filter(hasText)
  .join("\n");
const actualF21RemainingIdentities = new Set(
  [...f21OperationalContract.matchAll(/\bQ49-L\d+\b/g)].map(
    (match) => match[0],
  ),
);
const expectedF21RemainingIdentitySet = new Set(
  expectedF21PartialIdentities.map(({ remainingTask }) => remainingTask),
);
if (
  actualF21RemainingIdentities.size !== expectedF21RemainingIdentitySet.size ||
  [...actualF21RemainingIdentities].some(
    (identity) => !expectedF21RemainingIdentitySet.has(identity),
  )
) {
  add(
    "f21PhysicalPartialIdentityMismatch",
    "F21",
    `expected exactly ${[...expectedF21RemainingIdentitySet].join(", ")}; found ${[...actualF21RemainingIdentities].join(", ") || "none"}`,
  );
}
for (const {
  physicalLine,
  concern,
  remainingTask,
} of expectedF21PartialIdentities) {
  const matrixRow = coverageMatrixLines[physicalLine - 1] ?? "";
  const exactContractBinding =
    f21OperationalContract.includes(`L${physicalLine}/${remainingTask}`) ||
    f21OperationalContract.includes(
      `L${physicalLine} cross-block replay under ${remainingTask}`,
    ) ||
    f21OperationalContract.includes(
      `L${physicalLine} malformed interval under ${remainingTask}`,
    );
  if (!matrixRow.includes(concern) || !exactContractBinding) {
    add(
      "f21PhysicalPartialIdentityMismatch",
      "F21",
      `physical L${physicalLine} ${concern} must bind exactly to ${remainingTask}`,
    );
  }
}
const watcherPlanningIds = [
  ...Array.from({ length: 17 }, (_, index) => `W${String(index + 30)}`),
  "WG1",
  "WG2",
];
for (const id of watcherPlanningIds) {
  const task = byId.get(id);
  const commands = Array.isArray(task?.focusedCommands)
    ? task.focusedCommands
    : [];
  if (!hasText(commands[0]) || !commandHasFocusedSelector(commands[0])) {
    add(
      "watcherPlannedSelectorNotPrimary",
      id,
      "the first command must select this task's exact planned file",
    );
  }
  if (
    commands.some(
      (command) => command.trim() === "pnpm --dir demo/midgard-watcher test",
    )
  ) {
    add(
      "watcherPackageWideTestUsedAsFocusedGate",
      id,
      "package-wide watcher test cannot replace the per-task selector",
    );
  }
}
for (const id of ["W24", "W25", "W26"]) {
  const task = byId.get(id);
  if (task && Array.isArray(task.blockedOn) && task.blockedOn.includes("CG3")) {
    add(
      "cg3WaiverNotApplied",
      id,
      "CG3 is waived through W26; do not list it as a current blocker",
    );
  }
}
const w25 = byId.get("W25");
if (statusRole(ledgerStatus.get("W25")) === "IN_PROGRESS") {
  const schedulingText = [
    w25?.readyBecause,
    w25?.blockedBecause,
    w25?.schedulingNote,
  ]
    .filter(hasText)
    .join(" ");
  if (/\bready\b/i.test(schedulingText)) {
    add(
      "inProgressTaskClaimedReady",
      "W25",
      "ledger is IN_PROGRESS but manifest claims ready",
    );
  }
}
const w26 = byId.get("W26");
if (!Array.isArray(w26?.dependsOn) || !w26.dependsOn.includes("W25")) {
  add("w26MissingW25Dependency", "W26", "W26 must depend on W25");
}
if (
  !isPassStatus(ledgerStatus.get("W25")) &&
  (!Array.isArray(w26?.blockedOn) || !w26.blockedOn.includes("W25"))
) {
  add(
    "w26MissingW25Blocker",
    "W26",
    "W26 must remain blocked while W25 is not PASS",
  );
}
const w27 = byId.get("W27");
for (const dependency of ["W25", "W26"]) {
  if (!Array.isArray(w27?.dependsOn) || !w27.dependsOn.includes(dependency)) {
    add("w27MissingSerialDependency", "W27", `missing ${dependency}`);
  }
  if (
    !isPassStatus(ledgerStatus.get(dependency)) &&
    (!Array.isArray(w27?.blockedOn) || !w27.blockedOn.includes(dependency))
  ) {
    add(
      "w27MissingSerialBlocker",
      "W27",
      `missing non-PASS blocker ${dependency}`,
    );
  }
}
if (
  !/CG3.*(?:re-?evaluat|recheck)|(?:re-?evaluat|recheck).*CG3/i.test(
    String(w27?.schedulingNote ?? ""),
  )
) {
  add(
    "w27MissingCg3Recheck",
    "W27",
    "schedulingNote must require CG3 re-evaluation after W26",
  );
}

// Blocked-on claim reconciliation. Every published sentence that quantifies or
// names a blocked-on set is checked against the derivation above, so a claim
// like "blockedOn contains exactly 1 current non-PASS dependency F41" fails
// closed the moment a second dependency (C26) goes non-PASS. Deleting the
// claim is not an escape: the manifest-wide claim is required to exist.
const knownTaskId = (token) => ledgerStatus.has(token) || taskIds.has(token);
const namedTaskIds = (text) =>
  [
    ...new Set(
      (text.match(/\b[A-Z][A-Z0-9]*(?:-[0-9])?\b/g) ?? []).filter(knownTaskId),
    ),
  ].sort();
const describeSet = (ids) => (ids.length === 0 ? "none" : ids.join(", "));
let manifestWideBlockedOnClaims = 0;
for (const [index, task] of tasks.entries()) {
  const id = idOf(task, index);
  const derivedForTask = currentNonPassDependenciesOf(task);
  for (const value of taskStringValues(task)) {
    for (const claim of value.matchAll(
      /blockedOn contains exactly (\d+) current non-PASS dependenc(?:y|ies)([^.;]*)/gi,
    )) {
      manifestWideBlockedOnClaims += 1;
      const claimedCount = Number(claim[1]);
      const claimedIds = namedTaskIds(claim[2]);
      if (
        claimedCount !== currentNonPassBlockers.length ||
        claimedIds.join(",") !== currentNonPassBlockers.join(",")
      ) {
        add(
          "blockedOnClaimMismatch",
          id,
          `claims ${claimedCount} current non-PASS blockedOn dependencies (${describeSet(claimedIds)}); the manifest joined against the first queue has ${currentNonPassBlockers.length} (${describeSet(currentNonPassBlockers)})`,
        );
      }
    }
    for (const claim of value.matchAll(
      /\b([A-Z][A-Z0-9]*(?:-[0-9])?) has no (?:remaining )?non-PASS dependenc(?:y|ies)/g,
    )) {
      if (claim[1] !== task?.id) continue;
      if (derivedForTask.length > 0) {
        add(
          "noBlockerClaimMismatch",
          id,
          `claims no current non-PASS dependency, but the first queue records ${describeSet(derivedForTask)}`,
        );
      }
    }
    for (const claim of value.matchAll(
      /\b([A-Z][A-Z0-9]*(?:-[0-9])?) remains dependency-blocked only on current non-PASS([^.;]*)/g,
    )) {
      if (claim[1] !== task?.id) continue;
      // "only on X" is checked for understatement, not for exact equality:
      // rows may legitimately name dependencies that have no first-queue row
      // yet (unscheduled, therefore not recorded PASS). What the row may never
      // do is leave out a dependency the queue currently records as non-PASS.
      const claimedIds = namedTaskIds(claim[2]).filter(
        (token) => token !== task.id,
      );
      const understated = derivedForTask.filter(
        (dependency) => !claimedIds.includes(dependency),
      );
      if (understated.length > 0) {
        add(
          "blockedOnlyClaimMismatch",
          id,
          `names ${describeSet(claimedIds)} as its only current non-PASS dependencies, omitting ${describeSet(understated)} which the first queue records as non-PASS`,
        );
      }
    }
  }
}
if (manifestWideBlockedOnClaims === 0) {
  add(
    "missingBlockedOnReconciliationClaim",
    "F05",
    "no manifest row publishes the reconciled 'blockedOn contains exactly N current non-PASS dependencies' claim, so the reconciliation rule has nothing to check",
  );
}

const categories = [...findings.keys()].sort();
const sortedFindings = Object.fromEntries(
  categories.map((category) => [
    category,
    [...(findings.get(category) ?? [])].sort(
      (left, right) =>
        left.id.localeCompare(right.id) ||
        left.detail.localeCompare(right.detail),
    ),
  ]),
);
const summary = {
  gate: "goal:tasks:quality:verify",
  expectedTasks: EXPECTED_TASKS,
  manifestTasks: tasks.length,
  uniqueTaskIds: taskIds.size,
  ledgerTaskIds: ledgerStatus.size,
  defects: Object.values(sortedFindings).reduce(
    (total, entries) => total + entries.length,
    0,
  ),
  categories: Object.fromEntries(
    Object.entries(sortedFindings).map(([category, entries]) => [
      category,
      entries.length,
    ]),
  ),
};

if (asJson) {
  process.stdout.write(
    `${JSON.stringify({ summary, findings: sortedFindings }, null, 2)}\n`,
  );
} else {
  process.stdout.write(
    [
      `goal:tasks:quality:verify: ${summary.defects === 0 ? "PASS" : "FAIL"}`,
      `tasks: ${summary.manifestTasks}/${summary.expectedTasks}; unique IDs: ${summary.uniqueTaskIds}; first-queue IDs: ${summary.ledgerTaskIds}`,
      `defects: ${summary.defects}`,
      ...categories.map(
        (category) =>
          `${category}: ${String(sortedFindings[category].length)} ${sortedFindings[
            category
          ]
            .map(({ id }) => id)
            .join(", ")}`,
      ),
    ].join("\n") + "\n",
  );
}
process.exit(summary.defects === 0 ? 0 : 1);
