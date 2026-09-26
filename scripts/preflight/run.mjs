// Preflight mechanics: what changed, which checks that selects, and running
// them with capability-aware skipping. The CLI (scripts/preflight.mjs) is a thin
// shell over this module; tests drive it directly with temporary repositories,
// injected probes and a fake command runner.

import { spawn, spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

import { pinnedAikenVersion } from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import { formatCommand, selectChecks } from "./registry.mjs";

export const DEFAULT_BASE =
  "origin/colll78/canonical-v1-watcher-l1-source-checkpoint";

export const EXIT = { passed: 0, failed: 1, usage: 2, skipped: 3 };

export class UsageError extends Error {}

const git = (root, args, { allow = [0] } = {}) => {
  const run = spawnSync("git", args, {
    cwd: root,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
  });
  if (run.error !== undefined || !allow.includes(run.status)) {
    // A git failure is never "no changes": it aborts the run.
    throw new Error(
      `git ${args.join(" ")} failed: ${run.error?.message ?? run.stderr.trim()}`,
    );
  }
  return run;
};

const verifyCommit = (root, ref) =>
  git(root, ["rev-parse", "--verify", "--quiet", `${ref}^{commit}`], {
    allow: [0, 1, 128],
  }).status === 0;

// The base a branch is judged against: `--base`, else the branch's upstream,
// else the line of work's checkpoint branch. A named base that does not
// resolve is a usage error, never an empty diff.
export const resolveBase = (root, explicit) => {
  if (explicit !== undefined) {
    if (!verifyCommit(root, explicit)) {
      throw new UsageError(`--base ${explicit} does not name a commit`);
    }
    return explicit;
  }
  const upstream = git(
    root,
    ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"],
    { allow: [0, 128] },
  );
  const candidates = [
    ...(upstream.status === 0 ? [upstream.stdout.trim()] : []),
    DEFAULT_BASE,
  ];
  const base = candidates.find((candidate) => verifyCommit(root, candidate));
  if (base === undefined) {
    throw new UsageError(
      `no base to diff against (tried ${candidates.join(", ")}); pass --base <ref>`,
    );
  }
  return base;
};

const nulList = (output) => output.split("\0").filter(Boolean);

// `strict` judges only what is committed (what a push sends); otherwise staged,
// unstaged and untracked files count too. Renames count as delete plus add so
// both paths select their checks.
export const changedFiles = (root, mergeBase, { strict }) => {
  const committed = strict
    ? git(root, [
        "diff",
        "--name-only",
        "-z",
        "--no-renames",
        mergeBase,
        "HEAD",
      ])
    : git(root, ["diff", "--name-only", "-z", "--no-renames", mergeBase]);
  const untracked = strict
    ? []
    : nulList(
        git(root, ["ls-files", "--others", "--exclude-standard", "-z"]).stdout,
      );
  return [...new Set([...nulList(committed.stdout), ...untracked])].sort();
};

// The compiler pin, compared by value: a moved AIKEN_FORK_VERSION changes what
// every Aiken check means, while any other edit to the same workflow does not.
const pinAt = (root, revision) => {
  if (revision === undefined) {
    try {
      return pinnedAikenVersion(root);
    } catch {
      return undefined;
    }
  }
  const scratch = mkdtempSync(join(tmpdir(), "midgard-preflight-pin-"));
  try {
    for (const workflow of [
      ".github/workflows/aiken-ci.yml",
      ".github/workflows/midgard-node-ci.yml",
    ]) {
      const show = git(root, ["show", `${revision}:${workflow}`], {
        allow: [0, 128],
      });
      if (show.status !== 0) {
        return undefined;
      }
      mkdirSync(dirname(join(scratch, workflow)), { recursive: true });
      writeFileSync(join(scratch, workflow), show.stdout);
    }
    return pinnedAikenVersion(scratch);
  } catch {
    return undefined;
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

export const collectChanges = (root, { base, strict }) => {
  const mergeBase = git(root, ["merge-base", "HEAD", base]).stdout.trim();
  const changed = changedFiles(root, mergeBase, { strict });
  const before = pinAt(root, mergeBase);
  const after = pinAt(root, strict ? "HEAD" : undefined);
  const fullReasons =
    before === after
      ? []
      : [
          `the Aiken compiler pin moved (${String(before)} -> ${String(after)})`,
        ];
  const dirty =
    strict &&
    git(root, [
      "status",
      "--porcelain",
      "--untracked-files=no",
    ]).stdout.trim() !== "";
  return { mergeBase, changed, fullReasons, dirty };
};

// Selection plus each selected check's concrete steps and the advisories the
// change earns. A check whose plan is null has nothing to run for this change
// (only deleted files matched, say) and drops out.
export const planPreflight = (registry, changed, options = {}) => {
  const selection = selectChecks(registry, changed, options);
  const advisories = [];
  const advise = (advisory) => {
    if (!advisories.some((existing) => existing.id === advisory.id)) {
      advisories.push(advisory);
    }
  };
  const planned = [];
  for (const { check, matched } of selection.selected) {
    const steps = check.plan({ matched, full: selection.full, advise });
    if (steps !== null) {
      planned.push({ check, matched, steps });
    }
  }
  // "You changed an input and did not regenerate the output": a golden channel
  // or execution ledger whose inputs moved while none of its artifacts did.
  if (!selection.full) {
    const changedSet = new Set(changed);
    for (const { check, matched } of planned) {
      const artifacts = check.artifacts ?? [];
      const inputs = matched.filter((path) => !artifacts.includes(path));
      if (
        artifacts.length === 0 ||
        inputs.length === 0 ||
        artifacts.some((path) => changedSet.has(path))
      ) {
        continue;
      }
      const shown =
        inputs.slice(0, 3).join(", ") +
        (inputs.length > 3 ? `, and ${String(inputs.length - 3)} more` : "");
      const isLedger = check.id.startsWith("exec-ledger:");
      advise({
        id: `regenerate:${check.id}`,
        message: `you changed ${shown}, which ${check.id} reads, and none of the files it produces (${artifacts.join(", ")}) changed`,
        steps: isLedger
          ? [
              `Re-measure: ${check.display}`,
              `If a reading moved and you can explain why, record it: ${check.fix}`,
              `Stage the ledger by explicit path: git add ${artifacts.join(" ")}`,
            ]
          : [
              `Run the check: ${check.display}`,
              ...(check.fix === undefined
                ? []
                : [
                    `If it reports drift you intended, regenerate: ${check.fix}`,
                  ]),
              `Review the regenerated diff, then stage by explicit path: git add ${artifacts.join(" ")}`,
            ],
      });
    }
  }
  return { ...selection, planned, advisories };
};

// Runs one step, teeing its output to `log` and keeping it for the verdict.
export const spawnStep = (root, step, env, log) =>
  new Promise((done) => {
    const started = Date.now();
    let output = "";
    const child = spawn(step.argv[0], step.argv.slice(1), {
      cwd: resolve(root, step.cwd ?? "."),
      env,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const collect = (chunk) => {
      const text = chunk.toString();
      output += text;
      if (output.length > 4 * 1024 * 1024) {
        output = output.slice(-2 * 1024 * 1024);
      }
      log(text);
    };
    child.stdout.on("data", collect);
    child.stderr.on("data", collect);
    child.once("error", (error) =>
      done({ status: null, output, error, durationMs: Date.now() - started }),
    );
    child.once("close", (status) =>
      done({ status, output, durationMs: Date.now() - started }),
    );
  });

// Output that means a suite ran nothing, whatever its exit code says. Vitest
// prints this when its global setup fails (Postgres unreachable) or a filter
// matches no file; either way nothing was tested.
export const EMPTY_RUN_MARKERS = [
  {
    pattern: /No test files found/u,
    reason:
      "Vitest reported 'No test files found': its global setup failed (for the node suites, usually Postgres on 5433 is unreachable) or a filter matched nothing; nothing was tested",
  },
];

const mergeTree = (root, base) => {
  const run = spawnSync(
    "git",
    [
      "merge-tree",
      "--write-tree",
      "--name-only",
      "--no-messages",
      "HEAD",
      base,
    ],
    { cwd: root, encoding: "utf8" },
  );
  const behind = spawnSync("git", ["rev-list", "--count", `HEAD..${base}`], {
    cwd: root,
    encoding: "utf8",
  }).stdout?.trim();
  const lag = behind ? ` (${behind} commit(s) behind ${base})` : "";
  if (run.status === 0) {
    return { status: "passed", reason: `merges cleanly into ${base}${lag}` };
  }
  if (run.status === 1) {
    const files = run.stdout.split("\n").slice(1).filter(Boolean);
    return {
      status: "failed",
      reason: `merging ${base} would conflict in: ${files.join(", ")}${lag}`,
    };
  }
  return {
    status: "skipped",
    reason: `could not check: git merge-tree exited ${String(run.status)} (${(run.stderr ?? "").trim()})`,
  };
};

export const runPreflight = async ({
  root,
  plan,
  probes,
  base,
  runStep = spawnStep,
  log = (text) => process.stderr.write(text),
  env = process.env,
  exists = (path) => existsSync(resolve(root, path)),
}) => {
  const results = [];
  for (const { check, steps } of plan.planned) {
    const started = Date.now();
    const command =
      check.internal === "merge-tree"
        ? check.display.replace("<base>", base)
        : steps.map(formatCommand).join(" && ");
    const record = (status, reason) => {
      results.push({
        id: check.id,
        status,
        reason,
        command,
        durationMs: Date.now() - started,
        ...(status === "failed" && check.fix !== undefined
          ? { fix: check.fix }
          : {}),
      });
    };

    const absent = (check.requiresFiles ?? []).filter((path) => !exists(path));
    if (absent.length > 0) {
      record("skipped", `could not check: ${absent.join(", ")} absent`);
      continue;
    }
    const capabilityEnv = {};
    const missing = [];
    for (const name of check.capabilities) {
      const probe = await probes.get(name);
      if (probe.status === "available") {
        Object.assign(capabilityEnv, probe.env ?? {});
      } else {
        missing.push(
          `${name} ${probe.status}: ${probe.detail}${probe.fix ? ` (fix: ${probe.fix})` : ""}`,
        );
      }
    }
    if (missing.length > 0) {
      record("skipped", missing.join("; "));
      continue;
    }

    let outcome;
    if (check.internal === "merge-tree") {
      outcome = mergeTree(root, base);
    } else {
      log(`\n==> ${check.id}: ${command}\n`);
      outcome = { status: "passed", reason: "" };
      for (const step of steps) {
        const run = await runStep(
          root,
          step,
          { ...env, ...capabilityEnv, ...step.env },
          log,
        );
        const empty = EMPTY_RUN_MARKERS.find(({ pattern }) =>
          pattern.test(run.output ?? ""),
        );
        if (empty !== undefined) {
          outcome = { status: "failed", reason: empty.reason };
          break;
        }
        if (run.error !== undefined || run.status !== 0) {
          outcome = {
            status: "failed",
            reason:
              run.error !== undefined
                ? `${formatCommand(step)} could not start: ${run.error.message}`
                : `${formatCommand(step)} exited ${String(run.status)}`,
          };
          break;
        }
      }
    }
    if (outcome.status === "failed" && check.warnOnly) {
      record(
        "warned",
        `${outcome.reason}${check.warnReason ? ` — ${check.warnReason}` : ""}`,
      );
    } else {
      record(outcome.status, outcome.reason);
    }
    probes.invalidate(check.invalidates);
  }
  const exitCode = results.some((r) => r.status === "failed")
    ? EXIT.failed
    : results.some((r) => r.status === "skipped")
      ? EXIT.skipped
      : EXIT.passed;
  return { results, exitCode };
};
