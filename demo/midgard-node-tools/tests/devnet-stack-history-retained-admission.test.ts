import {
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { connect, createServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncRollBackward,
} from "midgard-watcher";
import { afterEach, expect, it, vi } from "vitest";

import {
  historyCommandExitCode,
  HistoryConfigurationRefusal,
} from "../src/devnet-stack/history-configuration-refusal.js";
import { createHistoryChainFollower } from "../src/devnet-stack/watcher-history-chain.js";

vi.mock("node:fs", async (original) => {
  const fs = await original<typeof import("node:fs")>();
  return { ...fs, readFileSync: vi.fn(fs.readFileSync) };
});

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});
const hash = (n: number) => n.toString(16).padStart(64, "0");
const point = (n: number) => ({
  blockHash: hash(n),
  blockNo: String(n),
  slot: String(n * 10),
});
const setup = () => {
  const root = mkdtempSync(join(tmpdir(), "retained-admission-"));
  dirs.push(root);
  const directories = ["a", "b"].map((role) => {
    const d = join(root, role);
    mkdirSync(join(d, "canonical"), { recursive: true });
    return d;
  });
  const commitsDirectory = join(root, "commits");
  mkdirSync(commitsDirectory);
  return { root, directories, commitsDirectory };
};
const retain = (paths: ReturnType<typeof setup>, tip = 3) => {
  for (const d of paths.directories) {
    for (let n = 1; n <= tip; n += 1) {
      const p = point(n);
      writeFileSync(
        join(d, "canonical", `${n}.json`),
        JSON.stringify({
          point: { ...p, pointId: computeFraudProofRawL1PointId(p) },
          prevHash: hash(n - 1),
        }),
      );
    }
    writeFileSync(
      join(d, "canonical-ready"),
      JSON.stringify("synthetic-generation"),
    );
  }
};
const construct = (paths: ReturnType<typeof setup>) =>
  createHistoryChainFollower({
    directories: paths.directories,
    commitsDirectory: paths.commitsDirectory,
    stateQueuePolicyId: "11".repeat(28),
    admit: () => {
      throw new Error("unexpected native block admission");
    },
  });
const snapshot = (root: string) =>
  Object.fromEntries(
    readdirSync(root, { recursive: true, withFileTypes: true }).map((e) => {
      const p = join(e.parentPath, e.name);
      return [
        p,
        e.isDirectory()
          ? "directory"
          : e.isSymbolicLink()
            ? "symlink"
            : `${statSync(p).mtimeMs}:${readFileSync(p).toString("hex")}`,
      ];
    }),
  );
const backward = (
  n: number | "origin",
): WatcherNativeChainSyncRollBackward => ({
  schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  kind: "roll_backward",
  point:
    n === "origin"
      ? { kind: "origin" }
      : { kind: "point", blockHash: hash(n), slot: String(n * 10) },
  tip: { kind: "origin" },
});

it.each([
  "missing-index",
  "malformed-index",
  "malformed-canonical",
  "symlink-canonical",
])(
  "production constructor refuses %s before any archive/index writes",
  (mode) => {
    const paths = setup();
    retain(paths);
    const index = join(paths.commitsDirectory, `${"ab".repeat(28)}.json`);
    if (mode === "missing-index")
      rmSync(paths.commitsDirectory, { recursive: true });
    if (mode === "malformed-index") writeFileSync(index, "torn JSON");
    if (mode === "malformed-canonical")
      for (const d of paths.directories)
        writeFileSync(
          join(d, "canonical", "3.json"),
          JSON.stringify({
            point: { ...point(3), pointId: "00".repeat(32) },
            prevHash: hash(2),
          }),
        );
    if (mode === "symlink-canonical") {
      const path = join(paths.directories[0]!, "canonical", "3.json");
      rmSync(path);
      symlinkSync(join(paths.directories[1]!, "canonical", "3.json"), path);
    }
    const before = snapshot(paths.root);
    expect(() => construct(paths)).toThrow(HistoryConfigurationRefusal);
    expect(() => construct(paths)).toThrow(/retained history checkpoint/);
    expect(snapshot(paths.root)).toEqual(before);
  },
);

it("never offers or applies retained Origin and rejects an unknown opening intersection before index pruning", async () => {
  const paths = setup();
  retain(paths);
  writeFileSync(
    join(paths.commitsDirectory, `${"ab".repeat(28)}.json`),
    JSON.stringify(point(2)),
  );
  const follower = construct(paths);
  const before = snapshot(paths.root);
  expect(follower.intersectionCandidates).not.toContainEqual({
    kind: "origin",
  });
  await expect(follower.onEvent(backward("origin"))).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  await expect(follower.onEvent(backward("origin"))).rejects.toThrow(/Origin/);
  expect(snapshot(paths.root)).toEqual(before);
  await expect(follower.onEvent(backward(99))).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  await expect(follower.onEvent(backward(99))).rejects.toThrow(
    /admitted retained checkpoint/,
  );
  expect(snapshot(paths.root)).toEqual(before);
  await follower.onEvent(backward(3));
  const afterKnown = snapshot(paths.root);
  await expect(follower.onEvent(backward(0))).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  await expect(follower.onEvent(backward(0))).rejects.toThrow(
    /known retained checkpoint/,
  );
  expect(snapshot(paths.root)).toEqual(afterKnown);
});

it("admits coherent interrupted append with unequal tips and a pending next-block index for authenticated prefix replay", async () => {
  const paths = setup();
  retain(paths);
  const next = point(4);
  writeFileSync(
    join(paths.directories[0]!, "canonical", "4.json"),
    JSON.stringify({
      point: { ...next, pointId: computeFraudProofRawL1PointId(next) },
      prevHash: hash(3),
    }),
  );
  rmSync(join(paths.directories[1]!, "canonical-ready"));
  writeFileSync(
    join(paths.commitsDirectory, `${"ab".repeat(28)}.json`),
    JSON.stringify(point(4)),
  );
  const follower = construct(paths);
  expect(follower.intersectionCandidates[0]).toEqual({
    kind: "point",
    blockHash: hash(2),
    slot: "20",
  });
  await follower.onEvent(backward(2));
  for (const d of paths.directories)
    expect(readdirSync(join(d, "canonical"))).toEqual(["1.json", "2.json"]);
  expect(readdirSync(paths.commitsDirectory)).toEqual([]);
});

it("permits empty bootstrap and deliberately ignores unrelated/final-write temp namespaces", async () => {
  const paths = setup();
  for (const d of paths.directories) {
    writeFileSync(join(d, "canonical", ".1.json.123.tmp"), "staged");
    writeFileSync(join(d, "canonical", "notes.txt"), "unrelated");
  }
  writeFileSync(
    join(paths.commitsDirectory, "notes.json"),
    "unrelated malformed JSON",
  );
  const follower = construct(paths);
  expect(follower.intersectionCandidates).toEqual([{ kind: "origin" }]);
  await follower.onEvent(backward("origin"));
  expect(readFileSync(join(paths.commitsDirectory, "notes.json"), "utf8")).toBe(
    "unrelated malformed JSON",
  );
});

it("preserves actual retained read EIO as transient before writes instead of typing it as configuration refusal", () => {
  const paths = setup();
  retain(paths);
  const io = Object.assign(new Error("synthetic retained read failed"), {
    code: "EIO",
  });
  vi.mocked(readFileSync).mockImplementationOnce(() => {
    throw io;
  });
  let failure: unknown;
  try {
    construct(paths);
  } catch (error) {
    failure = error;
  }
  expect(failure).toBe(io);
  expect(historyCommandExitCode(failure)).toBe(70);
});
it("classifies only explicit intrinsic refusal78 and leaves actual local transport refusal70", async () => {
  expect(
    historyCommandExitCode(
      new HistoryConfigurationRefusal("retained contradiction"),
    ),
  ).toBe(78);
  expect(
    historyCommandExitCode(
      new Error("retained history checkpoint is missing or unproven"),
    ),
  ).toBe(70);
  expect(
    historyCommandExitCode(new DOMException("cancelled", "AbortError")),
  ).toBe(70);
  const server = createServer();
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", resolve);
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw Error("owned transport fixture absent");
  await new Promise<void>((resolve) => server.close(() => resolve()));
  const failure = await new Promise<Error>((resolve, reject) => {
    const socket = connect({ host: "127.0.0.1", port: address.port });
    const timer = setTimeout(() => {
      socket.destroy();
      reject(Error("owned transport fixture exceeded deadline"));
    }, 1000);
    socket.once("error", (error) => {
      clearTimeout(timer);
      socket.destroy();
      resolve(error);
    });
    socket.once("connect", () => {
      clearTimeout(timer);
      socket.destroy();
      reject(Error("unexpected successor used synthetic transport port"));
    });
  });
  expect(failure).toMatchObject({ code: "ECONNREFUSED" });
  expect(historyCommandExitCode(failure)).toBe(70);
});
