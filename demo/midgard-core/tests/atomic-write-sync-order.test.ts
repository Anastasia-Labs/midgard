import { beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

const state = vi.hoisted(() => ({
  calls: [] as string[],
  syncedDirectories: [] as string[],
  descriptors: new Map<number, string>(),
  firstCreated: undefined as string | undefined,
  failDirectorySync: false,
  failTempWrite: false,
}));

vi.mock("node:fs", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs")>();
  let next = 100;
  return {
    ...actual,
    mkdirSync: vi.fn(() => {
      state.calls.push("mkdir");
      return state.firstCreated;
    }),
    openSync: vi.fn((path: string, flags: string) => {
      const fd = next++;
      state.descriptors.set(fd, flags === "wx" ? "file" : `directory:${path}`);
      return fd;
    }),
    writeFileSync: vi.fn(() => {
      state.calls.push("write:file");
      if (state.failTempWrite) throw new Error("injected write failure");
    }),
    chmodSync: vi.fn(() => {
      state.calls.push("chmod");
    }),
    fsyncSync: vi.fn((fd: number) => {
      const kind = state.descriptors.get(fd)!;
      if (kind === "file") {
        state.calls.push("sync:file");
        return;
      }
      state.calls.push("sync:directory");
      state.syncedDirectories.push(kind.slice("directory:".length));
      if (state.failDirectorySync)
        throw new Error("injected directory sync failure");
    }),
    closeSync: vi.fn(),
    renameSync: vi.fn(() => {
      state.calls.push("rename");
    }),
    rmSync: vi.fn(() => {
      state.calls.push("rm");
    }),
  };
});

let writeTextFileAtomicSync: typeof import("../src/atomic-write.js").writeTextFileAtomicSync;

beforeAll(async () => {
  ({ writeTextFileAtomicSync } = await import("../src/atomic-write.js"));
});

/**
 * The synchronous write's durability ordering, observed at the syscall
 * boundary as `atomic-write-order.test.ts` observes the asynchronous one; the
 * real-filesystem claims are in `atomic-write.test.ts`.
 */
const relativeOrder = (call: string): number => state.calls.indexOf(call);

describe("synchronous durable atomic write ordering", () => {
  beforeEach(() => {
    state.calls.length = 0;
    state.syncedDirectories.length = 0;
    state.descriptors.clear();
    state.firstCreated = undefined;
    state.failDirectorySync = false;
    state.failTempWrite = false;
  });

  it("syncs the file before rename and the parent directory after rename", () => {
    writeTextFileAtomicSync("/tmp/state.json", "content", { mode: 0o600 });

    expect(relativeOrder("write:file")).toBeGreaterThanOrEqual(0);
    expect(relativeOrder("sync:directory")).toBeGreaterThanOrEqual(0);
    expect(relativeOrder("write:file")).toBeLessThan(
      relativeOrder("sync:file"),
    );
    expect(relativeOrder("sync:file")).toBeLessThan(relativeOrder("rename"));
    expect(relativeOrder("rename")).toBeLessThan(
      relativeOrder("sync:directory"),
    );
  });

  it("syncs the parent of every directory it created, not only the file's parent", () => {
    state.firstCreated = "/tmp/run";
    writeTextFileAtomicSync("/tmp/run/services/state.json", "content");

    expect(state.syncedDirectories).toEqual([
      "/tmp/run/services",
      "/tmp/run",
      "/tmp",
    ]);
  });

  it("still removes the temp file when the parent-directory sync fails after rename", () => {
    state.failDirectorySync = true;

    expect(() => writeTextFileAtomicSync("/tmp/state.json", "content")).toThrow(
      "injected directory sync failure",
    );

    expect(relativeOrder("rename")).toBeGreaterThanOrEqual(0);
    expect(relativeOrder("rm")).toBeGreaterThan(relativeOrder("rename"));
  });

  it("never renames a temp file whose write failed", () => {
    state.failTempWrite = true;

    expect(() => writeTextFileAtomicSync("/tmp/state.json", "content")).toThrow(
      "injected write failure",
    );

    expect(state.calls).not.toContain("rename");
    expect(state.calls).not.toContain("sync:file");
    expect(relativeOrder("rm")).toBeGreaterThanOrEqual(0);
  });
});
