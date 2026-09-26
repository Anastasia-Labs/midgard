#!/usr/bin/env node
// commit-paths: commit exactly the named paths from the working tree, through a
// temporary index, without touching anything else another session has staged
// or left unstaged.
//
//   node .agents/skills/committing-safely/scripts/commit-paths.mjs \
//     -m "Subject line" [-m "Body paragraph"] [-F message-file] \
//     [--patch file.patch] [--allow-unchecked] [--dry-run] -- <paths...>
//
// What it does, in order:
//   1. Refuses: no paths, a directory, a path outside the repository,
//      onchain/aiken/plutus.json, a path with no change against HEAD, a path
//      whose real-index entry holds a staged version that is neither HEAD's
//      nor the one being committed (someone else's staged work), an
//      in-progress merge/rebase/cherry-pick/revert, and a message that carries
//      a tool attribution trailer.
//   2. Builds a temporary index from HEAD and adds only those paths (or, with
//      --patch, applies only that patch).
//   3. Checks the formatting of exactly the content being committed: prettier
//      for demo/**/*.{ts,tsx,md} (demo's own prettier), and for *.ak the
//      pinned Aiken fork's formatter plus the trailing-whitespace
//      normalization CI applies. It refuses unformatted content; it never
//      rewrites anything. A missing tool is reported as "could not check",
//      separately from "unformatted".
//   4. Writes the commit with `git commit-tree` and moves HEAD with a
//      compare-and-swap `git update-ref`, so a commit another session lands
//      in the meantime is never silently reverted. No git hooks run: neither
//      the repository pre-commit hook nor the Nix pre-commit.local shim
//      (which stashes every unstaged file mid-commit), nor post-commit.
//   5. Resets the real index for the committed paths only, so they do not
//      show as reverse-staged, and prints the commit with its stat.
//
// Exit codes:
//   0  committed (or, with --dry-run, would commit)
//   1  refused; nothing committed
//   2  usage error; nothing committed
//   3  could not check formatting (a tool is missing); nothing committed.
//      Re-run with --allow-unchecked to commit anyway.
//   4  committed, but resetting the real index for the committed paths failed;
//      the message says what to run
//
// Environment:
//   MIDGARD_PRETTIER_BIN  prettier to use (default <repo>/demo/node_modules/.bin/prettier)
//   MIDGARD_AIKEN_BIN     aiken to use (default `aiken` on PATH)

import { spawnSync } from "node:child_process";
import {
  existsSync,
  lstatSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { isAbsolute, join, relative, resolve, sep } from "node:path";

const EXIT = {
  committed: 0,
  refused: 1,
  usage: 2,
  unchecked: 3,
  resyncFailed: 4,
};
const BLUEPRINT = "onchain/aiken/plutus.json";
const MAX_BUFFER = 256 * 1024 * 1024;
const ATTRIBUTION = [
  /^\s*co-authored-by:.*\b(?:claude|anthropic|openai|codex|copilot|chatgpt|gpt-?\d|gemini|cursor)\b/imu,
  /\bgenerated (?:with|by)\b.*\b(?:claude|anthropic|openai|codex|copilot|chatgpt|gemini|cursor)\b/imu,
  /noreply@anthropic\.com/iu,
  /\u{1F916}/u,
];

const USAGE = `usage: commit-paths -m <message> [-m <paragraph>]... | -F <file>
                    [--patch <file>] [--allow-unchecked] [--dry-run] -- <paths...>

Commits exactly <paths> from the working tree through a temporary index.
With --patch, commits exactly the hunks in <file> (a diff against HEAD);
paths, if given, must match the files the patch touches.`;

class Stop extends Error {
  constructor(code, message) {
    super(message);
    this.code = code;
  }
}
const refuse = (message) => {
  throw new Stop(EXIT.refused, `refused: ${message}`);
};
const usage = (message) => {
  throw new Stop(EXIT.usage, `${message}\n\n${USAGE}`);
};

const parseArgs = (argv) => {
  const options = {
    messages: [],
    messageFile: undefined,
    patch: undefined,
    allowUnchecked: false,
    dryRun: false,
    paths: [],
    help: false,
  };
  const value = (index, flag) => {
    if (index >= argv.length) usage(`${flag} needs a value`);
    return argv[index];
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--") {
      options.paths.push(...argv.slice(index + 1));
      break;
    }
    if (arg === "-m" || arg === "--message") {
      index += 1;
      options.messages.push(value(index, arg));
    } else if (arg === "-F" || arg === "--file") {
      index += 1;
      if (options.messageFile !== undefined) usage("-F given twice");
      options.messageFile = value(index, arg);
    } else if (arg === "--patch") {
      index += 1;
      options.patch = value(index, arg);
    } else if (arg === "--allow-unchecked") {
      options.allowUnchecked = true;
    } else if (arg === "--dry-run") {
      options.dryRun = true;
    } else if (arg === "-h" || arg === "--help") {
      options.help = true;
    } else if (arg.startsWith("-")) {
      usage(`unknown option ${arg}`);
    } else {
      options.paths.push(arg);
    }
  }
  return options;
};

// ------------------------------------------------------------------- git

let repoRoot = process.cwd();

const run = (command, args, { cwd = repoRoot, env, input } = {}) =>
  spawnSync(command, args, {
    cwd,
    env,
    input,
    maxBuffer: MAX_BUFFER,
  });

const gitEnv = (index) => {
  const env = { ...process.env, GIT_LITERAL_PATHSPECS: "1" };
  delete env.GIT_INDEX_FILE;
  if (index !== undefined) env.GIT_INDEX_FILE = index;
  return env;
};

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

const nulFields = (buffer) =>
  buffer
    .toString("utf8")
    .split("\0")
    .filter((field) => field.length > 0);

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

const refuseBlueprint = (paths) => {
  if (paths.includes(BLUEPRINT)) {
    refuse(
      `${BLUEPRINT} is a build output of whichever compiler and profile last ran; it is never committed`,
    );
  }
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

// --------------------------------------------------------------- message

const buildMessage = (options, cwd) => {
  if (options.messages.length > 0 && options.messageFile !== undefined) {
    usage("give -m or -F, not both");
  }
  let message;
  if (options.messageFile !== undefined) {
    try {
      message = readFileSync(resolve(cwd, options.messageFile), "utf8");
    } catch (error) {
      usage(`cannot read message file: ${error.message}`);
    }
  } else if (options.messages.length > 0) {
    message = options.messages.join("\n\n");
  } else {
    usage("a commit message is required (-m or -F)");
  }
  message = `${message.replace(/\s+$/u, "")}\n`;
  if (message.trim().length === 0) refuse("the commit message is empty");
  for (const pattern of ATTRIBUTION) {
    if (pattern.test(message)) {
      refuse(
        "the commit message carries an tool attribution line or trailer; commits here carry no tool attribution",
      );
    }
  }
  return message;
};

// ------------------------------------------------------------ formatting

const isPrettierPath = (path) =>
  path.startsWith("demo/") && /\.(?:ts|tsx|md)$/u.test(path);
const isAikenPath = (path) => path.endsWith(".ak");

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

const main = (argv) => {
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

try {
  process.exitCode = main(process.argv.slice(2));
} catch (error) {
  if (error instanceof Stop) {
    process.stderr.write(`commit-paths: ${error.message}\n`);
    process.exitCode = error.code;
  } else {
    throw error;
  }
}
