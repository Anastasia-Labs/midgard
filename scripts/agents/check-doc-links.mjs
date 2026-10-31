#!/usr/bin/env node

// Checks that the repository paths the documentation cites exist.
//
// Files: every tracked AGENTS.md and CLAUDE.md, everything under
// .agents/skills, and every tracked Markdown file under docs/.
//
// References checked, outside fenced code:
//   - relative Markdown links and link definitions, resolved against the
//     citing file's directory (a leading `/` means the repository root);
//   - backticked repository paths: a token with no spaces that contains `/`
//     and either starts with a top-level directory of the repository or ends
//     in a known file extension (rooted and relative tokens). Both resolve
//     from the repository root, the citing file's directory, or any project
//     root (a directory holding package.json, aiken.toml, cabal.project,
//     flake.nix or a Makefile), since command examples run from those. Tokens with placeholders or globs (`<`, `*`, `{`, `$`,
//     `...`) are skipped.
// A path resolves when it is tracked by git or is a directory holding tracked
// files. A relative-kind token also resolves when it is the tail of a tracked
// path (`transition-trace/proof.ak`). Local outputs are accepted without
// existing: paths git ignores (`artifacts/`, private `config.yaml` files) and
// the generated outputs in GENERATED_OUTPUTS. Blind spot: a reference to an
// ignored path is never checked for existence, because a clean checkout (and
// CI) has none of them. A rooted token counts as ignored only from the
// repository root.
//
// A block (paragraph or list item) carrying one of these markers is skipped:
//   <!-- doc-links:future -->      a deliverable specified before it exists;
//   <!-- doc-links:external -->    paths in another repository or outside the
//                                  working tree;
//   <!-- doc-links:historical -->  a path the text itself says no longer exists.
// A file carrying `<!-- doc-links:run-relative -->` on a line of its own is a
// run log: its relative tokens name files inside a run's output directory and
// are skipped, while links and rooted tokens are still checked.
//
// Exit codes: 0 all references resolve, 1 broken references, 2 could not
// look (git or a file could not be read).
//
// Usage: node scripts/agents/check-doc-links.mjs [--root <dir>]

import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, normalize, posix, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { markdownBlocks } from "./markdown-blocks.mjs";

export const FUTURE_MARKER = "doc-links:future";
export const SKIP_MARKERS = [
  FUTURE_MARKER,
  "doc-links:external",
  "doc-links:historical",
];
export const RUN_RELATIVE_MARKER = /^<!--\s*doc-links:run-relative\s*-->$/mu;

// Build outputs that documentation names but that are neither tracked nor
// ignored.
export const GENERATED_OUTPUTS = ["onchain/aiken/plutus.json"];

export const KNOWN_EXTENSIONS = [
  "ak",
  "cabal",
  "hs",
  "js",
  "json",
  "lock",
  "md",
  "mdx",
  "mjs",
  "nix",
  "py",
  "rs",
  "sh",
  "sql",
  "tex",
  "toml",
  "ts",
  "tsx",
  "txt",
  "yaml",
  "yml",
];

const PROJECT_MARKERS = [
  "package.json",
  "aiken.toml",
  "cabal.project",
  "flake.nix",
  "Makefile",
];

export class CouldNotLook extends Error {}

export const trackedFiles = (root) => {
  try {
    return new Set(
      execFileSync("git", ["ls-files", "-z"], {
        cwd: root,
        encoding: "utf8",
        maxBuffer: 256 * 1024 * 1024,
        stdio: ["ignore", "pipe", "pipe"],
      })
        .split("\0")
        .filter(Boolean),
    );
  } catch (error) {
    throw new CouldNotLook(
      `git ls-files failed in ${root}: ${error.stderr?.toString().trim() || error.message}`,
    );
  }
};

export const documentationFiles = (tracked) =>
  [...tracked]
    .filter(
      (path) =>
        path === "AGENTS.md" ||
        path.endsWith("/AGENTS.md") ||
        path === "CLAUDE.md" ||
        path.endsWith("/CLAUDE.md") ||
        (path.startsWith(".agents/skills/") && path.endsWith(".md")) ||
        (path.startsWith("docs/") && /\.mdx?$/u.test(path)),
    )
    .sort();

// Everything a reference may resolve to: tracked files and their directories.
export const repositoryIndex = (tracked) => {
  const directories = new Set([""]);
  for (const path of tracked) {
    let directory = posix.dirname(path);
    while (directory !== "." && !directories.has(directory)) {
      directories.add(directory);
      directory = posix.dirname(directory);
    }
  }
  const topLevel = new Set(
    [...tracked]
      .filter((path) => path.includes("/"))
      .map((path) => path.split("/")[0]),
  );
  const projectRoots = [...tracked]
    .filter((path) => PROJECT_MARKERS.includes(posix.basename(path)))
    .map((path) => posix.dirname(path))
    .map((directory) => (directory === "." ? "" : directory));
  return {
    exists: (path) => tracked.has(path) || directories.has(path),
    isTail: (path) =>
      [...tracked].some((file) => file.endsWith(`/${path}`)) ||
      [...directories].some((directory) => directory.endsWith(`/${path}`)),
    topLevel,
    projectRoots: [...new Set(projectRoots)],
  };
};

const clean = (path) => {
  const normalized = posix.normalize(path).replace(/\/+$/u, "");
  return normalized === "." ? "" : normalized;
};

const inRepository = (path) => path !== ".." && !path.startsWith("../");

const markdownLinks = (text) => [
  ...[...text.matchAll(/\]\(\s*<?([^)\s>]+)>?(?:\s+"[^"]*")?\s*\)/gu)].map(
    ([, target]) => target,
  ),
  ...[...text.matchAll(/^\s*\[[^\]]+\]:\s*<?(\S+?)>?(?:\s|$)/gmu)].map(
    ([, target]) => target,
  ),
];

const isExternal = (target) =>
  /^[a-z][a-z0-9+.-]*:/iu.test(target) || target.startsWith("//");

const backtickedTokens = (text) =>
  [...text.matchAll(/(`+)([^`]+?)\1/gu)].map(([, , token]) => token.trim());

const placeholder = /[<>*{}$]|\.\.\.|…/u;

export const pathCandidate = (token, index) => {
  if (
    /[\s=]/u.test(token) ||
    isExternal(token) ||
    placeholder.test(token) ||
    /^[0-9a-f]{7,40}:/u.test(token) ||
    token.startsWith(".git/")
  )
    return undefined;
  const path = token
    .replace(/^\.\//u, "")
    .replace(/[#?].*$/u, "")
    .replace(/:\d+(?::\d+)?(?:[-–]\d+)?(?:,\d+(?:[-–]\d+)?)*$/u, "")
    .replace(/[.,;:]+$/u, "");
  if (!path.includes("/") || path.startsWith("/")) return undefined;
  const first = path.split("/")[0];
  const extension = /\.([a-z0-9]+)$/iu.exec(path)?.[1]?.toLowerCase();
  if (index.topLevel.has(first)) return { path, kind: "rooted" };
  if (extension !== undefined && KNOWN_EXTENSIONS.includes(extension))
    return { path, kind: "relative" };
  return undefined;
};

// Returns unresolved references as { where, reference, candidates }, where
// `candidates` are the repository paths the reference could have meant; the
// caller drops those git ignores.
export const unresolvedReferences = (file, source, index) => {
  const unresolved = [];
  const directory = posix.dirname(file) === "." ? "" : posix.dirname(file);
  const runRelative = RUN_RELATIVE_MARKER.test(source);
  for (const block of markdownBlocks(source)) {
    if (
      block.comments.some((comment) =>
        SKIP_MARKERS.some((marker) => comment.includes(marker)),
      )
    )
      continue;
    const where = `${file}:${block.line}`;
    // Link targets, from the text with inline code removed.
    const prose = block.text.replace(/(`+)[^`]*?\1/gu, " ");
    for (const target of markdownLinks(prose)) {
      if (isExternal(target) || target.startsWith("#")) continue;
      const bare = decodeURIComponent(target.replace(/[#?].*$/u, ""));
      if (bare === "") continue;
      const resolved = clean(
        bare.startsWith("/") ? bare.slice(1) : posix.join(directory, bare),
      );
      if (!inRepository(resolved))
        unresolved.push({ where, reference: `link ${target}`, candidates: [] });
      else if (!index.exists(resolved))
        unresolved.push({
          where,
          reference: `link ${target}`,
          candidates: [resolved],
        });
    }
    for (const token of backtickedTokens(block.text)) {
      const candidate = pathCandidate(token, index);
      if (candidate === undefined) continue;
      if (runRelative && candidate.kind === "relative") continue;
      const bases = ["", directory, ...index.projectRoots];
      const candidates = bases
        .map((base) => clean(posix.join(base, candidate.path)))
        .filter(inRepository);
      const found =
        candidates.some(
          (path) => index.exists(path) || GENERATED_OUTPUTS.includes(path),
        ) ||
        (candidate.kind === "relative" && index.isTail(clean(candidate.path)));
      // A rooted token may name an ignored path only from the repository
      // root: from a project root it would hit generated directories such as
      // onchain/aiken/docs.
      if (!found)
        unresolved.push({
          where,
          reference: `path \`${token}\``,
          candidates:
            candidate.kind === "rooted" ? [candidates[0]] : candidates,
        });
    }
  }
  return unresolved;
};

// The subset of `paths` git ignores. Each path is also asked about as a
// directory, since a pattern such as `db/*` matches only below a directory.
export const ignoredPaths = (root, paths) => {
  if (paths.length === 0) return new Set();
  const asked = paths.flatMap((path) => [path, `${path}/`]);
  let output;
  try {
    output = execFileSync(
      "git",
      ["check-ignore", "--no-index", "-z", "--stdin"],
      {
        cwd: root,
        encoding: "utf8",
        input: `${asked.join("\0")}\0`,
        maxBuffer: 64 * 1024 * 1024,
        stdio: ["pipe", "pipe", "pipe"],
      },
    );
  } catch (error) {
    // Exit 1 means none of the paths is ignored.
    if (error.status === 1) return new Set();
    throw new CouldNotLook(`git check-ignore failed: ${error.message}`);
  }
  return new Set(
    output
      .split("\0")
      .filter(Boolean)
      .map((path) => path.replace(/\/$/u, "")),
  );
};

export const brokenReferences = (root, unresolved) => {
  const ignored = ignoredPaths(root, [
    ...new Set(unresolved.flatMap(({ candidates }) => candidates)),
  ]);
  return unresolved
    .filter(({ candidates }) => !candidates.some((path) => ignored.has(path)))
    .map(({ where, reference }) => `${where}: missing ${reference}`);
};

export const checkDocLinks = (root) => {
  const tracked = trackedFiles(root);
  const index = repositoryIndex(tracked);
  const files = documentationFiles(tracked);
  if (files.length === 0)
    throw new CouldNotLook(`no documentation files are tracked under ${root}`);
  const unresolved = files.flatMap((file) => {
    let source;
    try {
      source = readFileSync(join(root, file), "utf8");
    } catch (error) {
      throw new CouldNotLook(`cannot read ${file}: ${error.message}`);
    }
    return unresolvedReferences(file, source, index);
  });
  return { files, findings: brokenReferences(root, unresolved) };
};

const main = (argv) => {
  const rootFlag = argv.indexOf("--root");
  const root = resolve(
    rootFlag >= 0
      ? argv[rootFlag + 1]
      : join(dirname(fileURLToPath(import.meta.url)), "../.."),
  );
  try {
    const { files, findings } = checkDocLinks(normalize(root));
    for (const finding of findings) console.error(finding);
    if (findings.length > 0) {
      console.error(
        `check-doc-links: ${findings.length} broken reference(s) in ${files.length} file(s); fix the path, or mark the block with one of ${SKIP_MARKERS.map((marker) => `<!-- ${marker} -->`).join(", ")} (see the header of this script)`,
      );
      return 1;
    }
    console.log(
      `check-doc-links: ${files.length} file(s), every reference resolves`,
    );
    return 0;
  } catch (error) {
    if (!(error instanceof CouldNotLook)) throw error;
    console.error(`check-doc-links: could not look: ${error.message}`);
    return 2;
  }
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
