import {
  IGNORED_PATHS,
  indexAikenModules,
  matchesAny,
  workflowText,
  workspacePackages,
} from "./derive.mjs";
import {
  aikenChecks,
  demoChecks,
  FULL_RUN,
  VERIFICATION_ONLY,
  ledgerChecks,
} from "./registry.demo-checks.mjs";
import {
  goldenChecks,
  independentChecks,
  toolingChecks,
} from "./registry.tooling-checks.mjs";

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
    ...independentChecks(),
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
      .filter(
        (path) =>
          matchesAny(path, FULL_RUN) && !matchesAny(path, VERIFICATION_ONLY),
      )
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
