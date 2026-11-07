import { execFileSync } from "node:child_process";
import {
  existsSync,
  readFileSync,
  realpathSync,
  lstatSync,
  unlinkSync,
  chmodSync,
  writeFileSync,
  renameSync,
  mkdirSync,
  rmSync,
} from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { randomUUID } from "node:crypto";

import {
  atomicJson,
  inside,
  json,
  sha256,
  workspacePackages,
  sourceInput,
} from "./files.mjs";
import { listResources, withResource } from "./resources.mjs";

export const git = (root, ...args) =>
  execFileSync("git", args, {
    cwd: root,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  }).trim();
const protectedPath =
  /(?:^|\/)(?:secrets)(?:\/|$)|(?:^|\/)\.env(?:\.|$)|^onchain\/aiken\/plutus\.json(?:\.|$)/u;
const packetPath = (root, path) => {
  if (
    typeof path !== "string" ||
    protectedPath.test(path) ||
    !sourceInput(root, path) ||
    path.includes(":") ||
    path.includes("\n") ||
    relative(root, inside(root, path)) !== path
  )
    throw new Error(`packet path is protected or not canonical: ${path}`);
  return inside(root, path);
};
const fileHash = (path) =>
  existsSync(path) ? sha256(readFileSync(path)) : null;

export const inspectWorkspace = (root) => {
  const raw = execFileSync("git", ["worktree", "list", "--porcelain", "-z"], {
    cwd: root,
    encoding: "utf8",
    timeout: 10000,
    maxBuffer: 64 * 1024 * 1024,
  });
  const worktrees = raw
    .split("\0\0")
    .filter(Boolean)
    .map((record) => {
      const lines = record.split("\0");
      const path = lines.find((line) => line.startsWith("worktree "))?.slice(9);
      const metadata = {
        root: path,
        head: lines.find((line) => line.startsWith("HEAD "))?.slice(5),
        branch: lines.find((line) => line.startsWith("branch "))?.slice(7),
        prunable: lines.find((line) => line.startsWith("prunable")),
      };
      try {
        const changes = execFileSync(
          "git",
          ["status", "--porcelain=v1", "-z", "--untracked-files=all"],
          {
            cwd: path,
            encoding: "utf8",
            timeout: 10000,
            maxBuffer: 64 * 1024 * 1024,
            stdio: ["ignore", "pipe", "pipe"],
          },
        )
          .split("\0")
          .filter(Boolean);
        const paths = [];
        for (let index = 0; index < changes.length; index += 1) {
          const item = changes[index];
          paths.push({
            index: item[0],
            worktree: item[1],
            path: item.slice(3),
          });
          if (/[RC]/u.test(item.slice(0, 2))) index += 1;
        }
        return { ...metadata, inspection: "available", changes: paths };
      } catch (error) {
        return {
          ...metadata,
          inspection: "unavailable",
          changes: null,
          reason: String(error.code ?? error.status ?? "status unavailable"),
        };
      }
    });
  const overlap = new Map();
  for (const tree of worktrees)
    for (const { path } of tree.changes ?? [])
      overlap.set(path, [...(overlap.get(path) ?? []), tree.root]);
  return {
    schema: "midgard-workspace/v1",
    root,
    worktrees,
    complete: worktrees.every((tree) => tree.inspection === "available"),
    overlaps: [...overlap]
      .filter(([, owners]) => owners.length > 1)
      .map(([path, owners]) => ({ path, owners })),
    resources: listResources(),
  };
};

export const createPacket = (root, { base, files, output }) => {
  const commit = git(root, "rev-parse", "--verify", `${base}^{commit}`);
  if (!files?.length || new Set(files).size !== files.length)
    throw new Error("packet requires unique explicit --file paths");
  const entries = files.map((path) => {
    const absolute = packetPath(root, path);
    const tracked = git(root, "ls-tree", commit, "--", path);
    let before = null;
    if (tracked) {
      if (!/^100(?:644|755) blob /u.test(tracked))
        throw new Error(`packet base path is not a regular file: ${path}`);
      before = sha256(
        execFileSync("git", ["show", `${commit}:${path}`], {
          cwd: root,
          maxBuffer: 64 * 1024 * 1024,
        }),
      );
    }
    if (existsSync(absolute) && !lstatSync(absolute).isFile())
      throw new Error(`not a regular file: ${path}`);
    const contents = existsSync(absolute) ? readFileSync(absolute) : undefined;
    const after = contents ? sha256(contents) : null;
    if (before === after) throw new Error(`unchanged packet path: ${path}`);
    return {
      path,
      before,
      after,
      mode: contents
        ? lstatSync(absolute).mode & 0o111
          ? "100755"
          : "100644"
        : undefined,
      content: contents?.toString("base64"),
    };
  });
  const packet = {
    schema: "midgard-integration-packet/v1",
    base: commit,
    sourceRoot: realpathSync(root),
    entries,
  };
  packet.sha256 = sha256(JSON.stringify(packet));
  atomicJson(output, packet);
  return packet;
};

export const verifyPacket = (root, path) => {
  const packet = json(path);
  const { sha256: digest, ...body } = packet;
  if (
    packet.schema !== "midgard-integration-packet/v1" ||
    digest !== sha256(JSON.stringify(body)) ||
    !Array.isArray(packet.entries) ||
    !packet.entries.length
  )
    throw new Error("packet schema/digest/entries invalid");
  if (git(root, "rev-parse", "HEAD") !== packet.base)
    throw new Error(
      `packet base ${packet.base} does not equal destination HEAD; regenerate/review against the current base`,
    );
  const seen = new Set();
  for (const entry of packet.entries) {
    const absolute = packetPath(root, entry.path);
    if (seen.has(entry.path))
      throw new Error(`duplicate packet path: ${entry.path}`);
    seen.add(entry.path);
    if (existsSync(absolute) && !lstatSync(absolute).isFile())
      throw new Error(`destination is not a regular file: ${entry.path}`);
    if (fileHash(absolute) !== entry.before)
      throw new Error(
        `destination changed: ${entry.path}; preserve the edits and regenerate the packet`,
      );
    if (
      entry.after !== null &&
      (!/^[a-f0-9]{64}$/u.test(entry.after) ||
        !["100644", "100755"].includes(entry.mode) ||
        typeof entry.content !== "string" ||
        Buffer.from(entry.content, "base64").toString("base64") !==
          entry.content ||
        sha256(Buffer.from(entry.content, "base64")) !== entry.after)
    )
      throw new Error(`invalid packet content/hash/mode: ${entry.path}`);
    if (entry.after === null && entry.content !== undefined)
      throw new Error("deleted packet entry has content");
  }
  return packet;
};

export const applyPacket = async (
  root,
  path,
  { signal, env = process.env } = {},
) =>
  withResource(
    `workspace:${realpathSync(root)}`,
    async () => {
      const packet = verifyPacket(root, path);
      // Validate the entire packet before changing any path. Refuse staged work
      // on selected paths; the normal safe commit tool remains the index owner.
      if (
        git(
          root,
          "diff",
          "--cached",
          "--name-only",
          "--",
          ...packet.entries.map((entry) => entry.path),
        )
      )
        throw new Error(
          "packet paths contain staged work; preserve it before integration",
        );
      const prepared = [];
      const applied = [];
      try {
        for (const entry of packet.entries) {
          const absolute = packetPath(root, entry.path);
          const original = existsSync(absolute)
            ? {
                bytes: readFileSync(absolute),
                mode: lstatSync(absolute).mode & 0o777,
              }
            : undefined;
          const temporary =
            entry.after === null
              ? undefined
              : `${absolute}.${randomUUID()}.tmp`;
          prepared.push({ entry, absolute, original, temporary });
          if (temporary) {
            mkdirSync(dirname(absolute), { recursive: true });
            writeFileSync(temporary, Buffer.from(entry.content, "base64"), {
              flag: "wx",
              mode: entry.mode === "100755" ? 0o755 : 0o644,
            });
          }
        }
        for (const item of prepared) {
          signal?.throwIfAborted();
          if (fileHash(item.absolute) !== item.entry.before)
            throw new Error(
              `destination changed during application: ${item.entry.path}`,
            );
          if (item.temporary) renameSync(item.temporary, item.absolute);
          else unlinkSync(item.absolute);
          applied.push(item);
        }
      } catch (error) {
        const conflicts = [];
        for (const item of applied.reverse()) {
          if (fileHash(item.absolute) !== item.entry.after) {
            conflicts.push(item.entry.path);
            continue;
          }
          if (!item.original) rmSync(item.absolute, { force: true });
          else {
            const restore = `${item.absolute}.${randomUUID()}.tmp`;
            writeFileSync(restore, item.original.bytes, {
              flag: "wx",
              mode: item.original.mode,
            });
            renameSync(restore, item.absolute);
            chmodSync(item.absolute, item.original.mode);
          }
        }
        if (conflicts.length)
          throw new Error(
            `${error.message}; preserved concurrent edits during rollback: ${conflicts.join(", ")}`,
          );
        throw error;
      } finally {
        for (const { temporary } of prepared)
          if (temporary) rmSync(temporary, { force: true });
      }
      return {
        applied: packet.entries.map((entry) => entry.path),
        base: packet.base,
      };
    },
    { signal, env },
  );

export const locate = (root, query = "") =>
  workspacePackages(root)
    .map((pkg) => ({
      package: pkg.name,
      directory: pkg.directory,
      exports: pkg.exports,
      bin: pkg.bin,
      scripts: pkg.scripts,
    }))
    .filter((entry) =>
      JSON.stringify(entry).toLowerCase().includes(query.toLowerCase()),
    );
