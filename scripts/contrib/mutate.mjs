// contrib mutate: put one mutant into a TypeScript or JavaScript source file
// of a workspace package, run the tests that reach it, and put the file back.
//
// It exists for two hazards of doing this by hand:
// - a replacement that changes nothing the tests can run (a sed that matches
//   nothing, or that edits a comment, a type or the layout), so the "mutant"
//   survives because there is none. The replacement must match exactly once,
//   must parse, and must change the code TypeScript emits, or it is refused.
// - a mutant left in the tree. The original bytes are journaled under the
//   checkout's git directory before the mutant is written, put back in
//   `finally` and on SIGINT, SIGTERM and SIGHUP, and verified by hash. A
//   process killed outright (SIGKILL) leaves the journal: every later
//   `contrib mutate` refuses until `contrib mutate restore` puts it back.
//   A dist the run rebuilt from the mutant is rebuilt from the original.

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { relative, resolve } from "node:path";

import { buildPackage } from "./build.mjs";
import { atomicJson, inside, sha256, workspacePackages } from "./files.mjs";
import { runTests } from "./tests.mjs";

const SOURCE = /\.(?:[cm]?[jt]s|[jt]sx)$/u;
const SIGNALS = ["SIGINT", "SIGTERM", "SIGHUP"];

/** Where the original of an unrestored mutant is kept: per worktree. */
export const journalPath = (root, env = process.env) =>
  resolve(
    root,
    execFileSync("git", ["rev-parse", "--git-path", "midgard-mutant.json"], {
      cwd: root,
      // A hook's GIT_DIR would point this at another repository.
      env: Object.fromEntries(
        Object.entries(env).filter(([key]) => !key.startsWith("GIT_")),
      ),
      encoding: "utf8",
    }).trim(),
  );

// Each guarded build's digest stamp, to find the dists a run rebuilt.
const stamps = (root) =>
  Object.fromEntries(
    workspacePackages(root)
      .filter((pkg) => pkg.scripts?.["build:contrib-raw"])
      .map((pkg) => {
        const path = resolve(
          root,
          pkg.directory,
          "dist/.contrib-build-v1.json",
        );
        return [pkg.name, existsSync(path) ? sha256(readFileSync(path)) : null];
      }),
  );

// The code a test can run, apart from layout: TypeScript's emit with types
// and comments erased, as the parser's sequence of tokens.
const emitted = (ts, fileName, text) => {
  const output = ts.transpileModule(text, {
    fileName,
    reportDiagnostics: true,
    compilerOptions: {
      target: ts.ScriptTarget.ESNext,
      module: ts.ModuleKind.ESNext,
      jsx: ts.JsxEmit.ReactJSX,
      removeComments: true,
    },
  });
  const file = ts.createSourceFile(
    "emitted.js",
    output.outputText,
    ts.ScriptTarget.Latest,
    false,
    ts.ScriptKind.JS,
  );
  const tokens = [];
  const walk = (node) => {
    const children = node.getChildren(file);
    if (children.length === 0)
      tokens.push(`${node.kind} ${node.getText(file)}`);
    else children.forEach(walk);
  };
  walk(file);
  return {
    tokens: tokens.join("\n"),
    errors: (output.diagnostics ?? []).map((diagnostic) =>
      ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"),
    ),
  };
};

/**
 * Put a journaled original back and verify its hash. Synchronous, so it
 * also runs in an exit handler. Leaves a file that is neither the original
 * nor the mutant alone: someone changed it while the mutant was in place.
 */
const putBack = (root, record, journal) => {
  const path = resolve(root, record.file);
  const current = sha256(readFileSync(path));
  if (current === record.mutantSha256)
    writeFileSync(path, Buffer.from(record.original, "base64"));
  else if (current !== record.originalSha256)
    throw new Error(
      `${record.file} changed while the mutant was in place; it was left as it is, and its original is in ${journal}`,
    );
  const restored = sha256(readFileSync(path));
  if (restored !== record.originalSha256)
    throw new Error(
      `${record.file} did not restore (sha256 ${restored}, original ${record.originalSha256}); its original is in ${journal}`,
    );
  return restored;
};

/**
 * Restore an unrestored mutant from the journal, then rebuild every dist
 * whose stamp changed while it was in place. The journal goes only when
 * both are done.
 */
export const restoreMutant = async (
  root,
  { env = process.env, build = buildPackage } = {},
) => {
  const journal = journalPath(root, env);
  if (!existsSync(journal))
    return { status: "clean", detail: "no unrestored mutant", exitCode: 0 };
  const record = JSON.parse(readFileSync(journal, "utf8"));
  const sha = putBack(root, record, journal);
  const now = stamps(root);
  const rebuilt = Object.keys(now).filter(
    (name) => now[name] !== record.stamps[name],
  );
  for (const name of rebuilt) {
    const built = await build(root, name, { env });
    if (built.exitCode !== 0)
      throw new Error(
        `${name}'s dist was built from the mutant and did not rebuild (${built.path}); run contrib mutate restore again`,
      );
  }
  rmSync(journal);
  return {
    status: "restored",
    file: record.file,
    sha256: sha,
    rebuilt,
    exitCode: 0,
  };
};

const refused = (reason) => ({ status: "refused", reason, exitCode: 2 });

const verdict = (run, expect) => {
  if (run.status === "not-reached")
    return [
      "unreached",
      1,
      "no test in the package reaches the file, so nothing runs the mutant",
    ];
  if (run.counts?.failed > 0) {
    if (
      expect &&
      !run.failures.some((failure) =>
        expect.test(
          `${failure.file} ${failure.test ?? ""} ${failure.message ?? ""}`,
        ),
      )
    )
      return [
        "killed-elsewhere",
        1,
        `tests failed, but none matches --expect ${expect}`,
      ];
    return ["killed", 0];
  }
  if (run.counts?.executed > 0 && ["passed", "incomplete"].includes(run.status))
    return [
      "survived",
      1,
      "every test that ran passed with the mutant in place",
    ];
  return [
    "inconclusive",
    3,
    run.reason ??
      "no test failed or passed: a suite or setup error stopped the run",
  ];
};

/**
 * Replace the one occurrence of `from` in `target` with `to`, run the
 * package's tests that reach `target` (or the named test files), and put the
 * original back. Exit 0 when a test kills the mutant (with `expect`, a
 * failure matching it), 1 when it survives or the restore fails, 2 when the
 * mutation is refused, 3 when the run cannot tell.
 */
export const mutate = async (
  root,
  {
    target,
    from,
    to,
    packageName,
    files = [],
    testName,
    expect,
    seed,
    signal,
    env = process.env,
  },
  { run = runTests, build = buildPackage } = {},
) => {
  let expected;
  try {
    expected = expect === undefined ? undefined : new RegExp(expect, "u");
  } catch (error) {
    return refused(`--expect is not a regular expression: ${error.message}`);
  }
  const journal = journalPath(root, env);
  if (existsSync(journal))
    return {
      status: "failed",
      reason: `a mutant of ${JSON.parse(readFileSync(journal, "utf8")).file} was never restored (its run was killed); run contrib mutate restore`,
      exitCode: 1,
    };
  if (!from) return refused("--from must name the text to replace");
  if (to === undefined) return refused("--to is required (it may be empty)");
  const path = inside(root, target);
  const file = relative(root, path);
  if (!SOURCE.test(file) || !existsSync(path))
    return refused(`not a TypeScript or JavaScript source file: ${target}`);
  const owner = workspacePackages(root).find((pkg) =>
    file.startsWith(`${pkg.directory}/`),
  );
  if (!owner) return refused(`${file} is not in a workspace package`);
  const original = readFileSync(path);
  const text = original.toString("utf8");
  const at = [];
  for (
    let index = text.indexOf(from);
    index !== -1;
    index = text.indexOf(from, index + 1)
  )
    at.push(text.slice(0, index).split("\n").length);
  if (at.length !== 1)
    return refused(
      at.length === 0
        ? `--from does not occur in ${file}`
        : `--from occurs ${at.length} times in ${file} (lines ${at.join(", ")}); lengthen it to name one`,
    );
  const mutant = text.replace(from, () => to);
  const ts = createRequire(resolve(root, owner.directory, "package.json"))(
    "typescript",
  );
  const before = emitted(ts, file, text);
  const after = emitted(ts, file, mutant);
  if (after.errors.length)
    return refused(
      `the mutant does not parse (${after.errors[0]}); a syntax error fails every test and proves nothing`,
    );
  if (after.tokens === before.tokens)
    return refused(
      "the mutant changes only comments, types or layout, so no test runs the difference",
    );

  const record = {
    file,
    line: at[0],
    originalSha256: sha256(original),
    mutantSha256: sha256(Buffer.from(mutant)),
    original: original.toString("base64"),
    stamps: stamps(root),
  };
  const controller = new AbortController();
  const abort = (name) => controller.abort(new Error(`interrupted by ${name}`));
  signal?.addEventListener("abort", () => abort("the caller"), { once: true });
  // A last resort if something calls process.exit with the mutant in place.
  const onExit = () => {
    try {
      putBack(root, record, journal);
    } catch {
      // The journal stays; the next contrib mutate refuses and says why.
    }
  };
  atomicJson(journal, record);
  for (const name of SIGNALS) process.on(name, abort);
  process.on("exit", onExit);
  let tests;
  let failure;
  try {
    writeFileSync(path, mutant);
    tests = await run(root, packageName ?? owner.name, {
      files,
      related: files.length ? [] : [file],
      testName,
      seed,
      env,
      signal: controller.signal,
      proofKind: "causal-guard-mutant",
    });
  } catch (error) {
    failure = error;
  } finally {
    try {
      putBack(root, record, journal);
    } finally {
      process.off("exit", onExit);
      for (const name of SIGNALS) process.off(name, abort);
    }
  }
  const mutation = {
    file,
    line: record.line,
    from,
    to,
    restoredSha256: record.originalSha256,
  };
  // Interrupted: the source is back; rebuilding a dist would keep the
  // person waiting, so the journal stays for `contrib mutate restore`.
  if (controller.signal.aborted) {
    const changed = Object.keys(record.stamps).filter(
      (name) => stamps(root)[name] !== record.stamps[name],
    );
    if (!changed.length) rmSync(journal);
    return {
      status: "failed",
      mutation,
      reason: `${controller.signal.reason.message}; ${file} is restored${changed.length ? `, but dists built from the mutant remain (${changed.join(", ")}): run contrib mutate restore` : ""}`,
      exitCode: 1,
    };
  }
  const restored = await restoreMutant(root, { env, build });
  // A mutant the compiler refuses (the dist build type-checks) proves
  // nothing about the tests.
  if (failure)
    return /^prerequisite build failed/u.test(failure.message)
      ? {
          status: "inconclusive",
          mutation,
          reason: `the mutant does not build, so no test ran it: ${failure.message}`,
          exitCode: 3,
        }
      : { status: "failed", mutation, reason: failure.message, exitCode: 1 };
  const [status, exitCode, reason] = verdict(tests, expected);
  return {
    schema: "midgard-contrib-mutant/v1",
    status,
    ...(reason ? { reason } : {}),
    mutation,
    package: tests.package,
    counts: tests.counts,
    failures: tests.failures?.slice(0, 10),
    receipt: tests.path,
    rebuilt: restored.rebuilt ?? [],
    exitCode,
  };
};
