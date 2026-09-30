import { spawnSync } from "node:child_process";
import {
  existsSync,
  lstatSync,
  mkdtempSync,
  realpathSync,
  rmSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { isAbsolute, join, relative, resolve, sep } from "node:path";

import {
  buildMessage,
  EXIT,
  gitEnv,
  isAikenPath,
  isPrettierPath,
  MAX_BUFFER,
  nulFields,
  parseArgs,
  refuse,
  refuseBlueprint,
  Stop,
  USAGE,
  usage,
} from "./commit-paths.parse-args.mjs";

// ------------------------------------------------------------------- git

export let repoRoot = process.cwd();

const run = (command, args, { cwd = repoRoot, env, input } = {}) =>
  spawnSync(command, args, {
    cwd,
    env,
    input,
    maxBuffer: MAX_BUFFER,
  });

// Runs git; returns stdout as a Buffer, or throws a refusal carrying stderr.
const git = (args, { index, input, what } = {}) => {
  const result = run("git", args, { env: gitEnv(index), input });
  if (result.error !== undefined) {
    throw new Stop(EXIT.refused, `could not run git: ${result.error.message}`);
  }
  if (result.status !== 0) {
    refuse(
      `${what ?? `git ${args[0]}`} failed:\n${result.stderr.toString().trim()}`,
    );
  }
  return result.stdout;
};

const gitText = (args, options) => git(args, options).toString("utf8").trim();

// path -> "mode sha" for the given index (undefined = the real index).
const indexEntries = (paths, index) => {
  const entries = new Map();
  if (paths.length === 0) return entries;
  for (const record of nulFields(
    git(["ls-files", "-s", "-z", "--", ...paths], { index }),
  )) {
    const tab = record.indexOf("\t");
    const [mode, sha, stage] = record.slice(0, tab).split(" ");
    const path = record.slice(tab + 1);
    if (stage !== "0") {
      refuse(`${path} has unresolved merge-conflict entries in the index`);
    }
    entries.set(path, `${mode} ${sha}`);
  }
  return entries;
};

const headEntries = (paths) => {
  const entries = new Map();
  if (paths.length === 0) return entries;
  for (const record of nulFields(
    git(["ls-tree", "-z", "HEAD", "--", ...paths]),
  )) {
    const tab = record.indexOf("\t");
    const [mode, type, sha] = record.slice(0, tab).split(" ");
    const path = record.slice(tab + 1);
    if (type !== "blob" && type !== "commit") {
      refuse(`${path} is a directory in HEAD; name the files inside it`);
    }
    entries.set(path, `${mode} ${sha}`);
  }
  return entries;
};

// ------------------------------------------------------------ path policy

const normalizePaths = (rawPaths, cwd) => {
  const normalized = [];
  for (const raw of rawPaths) {
    if (raw.length === 0) usage("an empty path was given");
    const absolute = resolve(cwd, raw);
    const rel = relative(repoRoot, absolute).split(sep).join("/");
    if (rel === "" || rel === ".") {
      refuse(`${raw} is the repository root; name the files to commit`);
    }
    if (rel.startsWith("../") || rel === ".." || isAbsolute(rel)) {
      refuse(`${raw} is outside the repository at ${repoRoot}`);
    }
    const stat = lstatSync(absolute, { throwIfNoEntry: false });
    if (stat?.isDirectory()) {
      refuse(`${rel} is a directory; name the files inside it explicitly`);
    }
    if (!normalized.includes(rel)) normalized.push(rel);
  }
  return normalized;
};

const refuseInProgressOperation = () => {
  for (const marker of [
    "MERGE_HEAD",
    "CHERRY_PICK_HEAD",
    "REVERT_HEAD",
    "rebase-merge",
    "rebase-apply",
  ]) {
    const path = resolve(
      repoRoot,
      gitText(["rev-parse", "--git-path", marker]),
    );
    if (existsSync(path)) {
      refuse(
        `a ${marker} is in progress; finish or abort it before committing paths`,
      );
    }
  }
};

const blob = (entry) => git(["cat-file", "blob", entry.split(" ")[1]]);

// Returns { unformatted: [], failed: [], unchecked: [] } with reasons.
const checkFormatting = (committed) => {
  const report = { unformatted: [], failed: [], unchecked: [] };

  const prettierPaths = committed.filter(({ path }) => isPrettierPath(path));
  if (prettierPaths.length > 0) {
    const prettier =
      process.env.MIDGARD_PRETTIER_BIN ??
      join(repoRoot, "demo/node_modules/.bin/prettier");
    let missing =
      process.env.MIDGARD_PRETTIER_BIN === undefined && !existsSync(prettier)
        ? `${relative(repoRoot, prettier)} is not installed (pnpm install in demo/)`
        : undefined;
    for (const { path, entry } of prettierPaths) {
      if (missing !== undefined) {
        report.unchecked.push(`${path}: ${missing}`);
        continue;
      }
      const source = blob(entry);
      const result = run(
        prettier,
        ["--stdin-filepath", path.slice("demo/".length)],
        { cwd: join(repoRoot, "demo"), input: source },
      );
      if (result.error !== undefined) {
        missing = `could not run ${prettier} (${result.error.message})`;
        report.unchecked.push(`${path}: ${missing}`);
      } else if (result.status !== 0) {
        report.failed.push(
          `${path}: prettier failed: ${result.stderr.toString().trim()}`,
        );
      } else if (!result.stdout.equals(source)) {
        report.unformatted.push(`${path} (prettier)`);
      }
    }
  }

  const aikenPaths = committed.filter(({ path }) => isAikenPath(path));
  if (aikenPaths.length > 0) {
    const aiken = process.env.MIDGARD_AIKEN_BIN ?? "aiken";
    const pinCheck = join(
      repoRoot,
      "onchain/aiken/scripts/pinned-compiler.mjs",
    );
    const probe = run(aiken, ["--version"]);
    let missing;
    if (probe.error !== undefined) {
      missing = `could not run aiken '${aiken}' (${probe.error.message}); set MIDGARD_AIKEN_BIN to the pinned fork`;
    } else if (!existsSync(pinCheck)) {
      missing =
        "onchain/aiken/scripts/pinned-compiler.mjs is missing, so the compiler identity cannot be verified";
    }
    if (missing !== undefined) {
      for (const { path } of aikenPaths) {
        report.unchecked.push(`${path}: ${missing}`);
      }
    } else {
      const pin = run(process.execPath, [pinCheck, aiken]);
      if (pin.status !== 0) {
        report.failed.push(
          `the aiken at '${aiken}' is not the pinned fork; nothing may be formatted or checked under it:\n${pin.stderr.toString().trim()}`,
        );
      } else {
        for (const { path, entry } of aikenPaths) {
          const source = blob(entry);
          const result = run(aiken, ["fmt", "--stdin"], { input: source });
          if (result.error !== undefined || result.status !== 0) {
            report.failed.push(
              `${path}: aiken fmt failed (does it parse?): ${(result.error?.message ?? result.stderr.toString()).trim()}`,
            );
            continue;
          }
          // CI (aiken-ci.yml "Run normalized Aiken auto-formatter check")
          // formats, then strips trailing whitespace the formatter itself
          // emits for monadic let/expect syntax, then compares. A bare
          // `aiken fmt --check` would refuse files CI accepts.
          const normalized = result.stdout
            .toString("utf8")
            .split("\n")
            .map((line) => line.replace(/[ \t\r\f\v]+$/u, ""))
            .join("\n");
          if (normalized !== source.toString("utf8")) {
            report.unformatted.push(`${path} (aiken fmt, CI-normalized)`);
          }
        }
      }
    }
  }
  return report;
};

// ------------------------------------------------------------------ main

export const main = (argv) => {
  const options = parseArgs(argv);
  if (options.help) {
    process.stdout.write(`${USAGE}\n`);
    return EXIT.committed;
  }
  if (process.env.GIT_INDEX_FILE !== undefined) {
    refuse(
      "GIT_INDEX_FILE is set in the environment; the real index is ambiguous. Unset it and re-run",
    );
  }
  const cwd = realpathSync(process.cwd());
  const top = run("git", ["rev-parse", "--show-toplevel"], {
    cwd,
    env: gitEnv(),
  });
  if (top.error !== undefined || top.status !== 0) {
    refuse("not inside a git working tree");
  }
  repoRoot = realpathSync(top.stdout.toString("utf8").trim());

  const message = buildMessage(options, cwd);
  if (options.patch === undefined && options.paths.length === 0) {
    usage("no paths given; name every file to commit after --");
  }
  let paths = normalizePaths(options.paths, cwd);
  refuseBlueprint(paths);
  refuseInProgressOperation();

  const base = gitText(["rev-parse", "--verify", "-q", "HEAD^{commit}"], {
    what: "resolving HEAD (an unborn branch is not supported)",
  });

  const scratch = mkdtempSync(join(tmpdir(), "commit-paths-"));
  const index = join(scratch, "index");
  try {
    git(["read-tree", "HEAD"], { index });

    if (options.patch === undefined) {
      git(["add", "--", ...paths], {
        index,
        what: "staging the named paths into the temporary index",
      });
    } else {
      const patchFile = resolve(cwd, options.patch);
      if (!existsSync(patchFile))
        usage(`patch file ${options.patch} not found`);
      git(["apply", "--cached", "--recount", "--", patchFile], {
        index,
        what: "applying the patch to HEAD in the temporary index",
      });
      const touched = nulFields(
        git(["diff", "--cached", "--name-only", "--no-renames", "-z", "HEAD"], {
          index,
        }),
      );
      if (
        paths.length > 0 &&
        [...paths].sort().join("\0") !== [...touched].sort().join("\0")
      ) {
        refuse(
          `the patch touches [${touched.join(", ")}] but the paths given were [${paths.join(", ")}]`,
        );
      }
      if (touched.length === 0)
        refuse("the patch changes nothing against HEAD");
      paths = touched;
      refuseBlueprint(paths);
    }

    const head = headEntries(paths);
    const staged = indexEntries(paths, index);
    const real = indexEntries(paths);

    const unchanged = paths.filter(
      (path) => head.get(path) === staged.get(path),
    );
    if (unchanged.length > 0) {
      refuse(`no change against HEAD in: ${unchanged.join(", ")}`);
    }

    // The real index may hold HEAD's version (nothing staged) or exactly the
    // version being committed. Anything else is a staged version this commit
    // would silently discard when the index is resynced.
    const foreign = paths.filter((path) => {
      const entry = real.get(path);
      return entry !== head.get(path) && entry !== staged.get(path);
    });
    if (foreign.length > 0) {
      refuse(
        `the real index holds a different staged version of: ${foreign.join(", ")}. ` +
          "It may be another session's work; leave it, or stage the version you mean to commit there first",
      );
    }

    const committed = paths
      .filter((path) => staged.has(path))
      .map((path) => ({ path, entry: staged.get(path) }))
      .filter(({ entry }) => /^100(?:644|755) /u.test(entry));
    const report = checkFormatting(committed);
    const refusals = [...report.failed, ...report.unformatted];
    if (refusals.length > 0) {
      refuse(
        [
          "formatting check did not pass; nothing was rewritten. Format these files, then re-run:",
          ...refusals.map((line) => `  ${line}`),
          ...(report.unchecked.length > 0
            ? [
                "could not check:",
                ...report.unchecked.map((line) => `  ${line}`),
              ]
            : []),
        ].join("\n"),
      );
    }
    if (report.unchecked.length > 0) {
      const lines = report.unchecked.map((line) => `  ${line}`).join("\n");
      if (!options.allowUnchecked) {
        throw new Stop(
          EXIT.unchecked,
          `could not check formatting (nothing committed):\n${lines}\nInstall the tool, or re-run with --allow-unchecked to commit without the check.`,
        );
      }
      process.stderr.write(
        `warning: committing WITHOUT a formatting check for:\n${lines}\n`,
      );
    }

    const stat = gitText(
      ["diff", "--cached", "--stat", "--no-color", "HEAD", "--", ...paths],
      { index },
    );
    if (options.dryRun) {
      process.stdout.write(
        `dry run: would commit on top of ${base.slice(0, 12)}:\n${stat}\n`,
      );
      return EXIT.committed;
    }

    const tree = gitText(["write-tree"], { index });
    const commit = gitText(["commit-tree", tree, "-p", base, "-F", "-"], {
      input: message,
      what: "writing the commit",
    });
    const subject = message.split("\n", 1)[0];
    const moved = run(
      "git",
      ["update-ref", "-m", `commit: ${subject}`, "HEAD", commit, base],
      { env: gitEnv() },
    );
    if (moved.status !== 0) {
      refuse(
        `HEAD moved away from ${base.slice(0, 12)} while committing (another session committed?). ` +
          `Nothing was committed; the unreferenced commit ${commit.slice(0, 12)} will be garbage-collected. Re-run.\n` +
          moved.stderr.toString().trim(),
      );
    }

    const resync = run("git", ["reset", "-q", "--", ...paths], {
      env: gitEnv(),
    });
    process.stdout.write(
      `${gitText(["show", "--stat", "--no-color", commit])}\n`,
    );
    if (resync.status !== 0) {
      process.stderr.write(
        `committed ${commit.slice(0, 12)}, but resetting the real index for the committed paths failed:\n` +
          `${resync.stderr.toString().trim()}\n` +
          `Run: git reset -q -- ${paths.map((path) => JSON.stringify(path)).join(" ")}\n`,
      );
      return EXIT.resyncFailed;
    }
    return EXIT.committed;
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};
