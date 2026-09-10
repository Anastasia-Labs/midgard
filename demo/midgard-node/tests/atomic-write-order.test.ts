import type { FileHandle } from "node:fs/promises";

import { beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

const state = vi.hoisted(() => ({
  calls: [] as string[],
  failDirectorySync: false,
  failTempWrite: false,
}));

const openMock = vi.hoisted(() =>
  vi.fn(async (_path: string, flags: string, _mode?: number) => {
    const kind = flags === "wx" ? "file" : "directory";
    return {
      writeFile: async () => {
        state.calls.push("write:file");
        if (state.failTempWrite) throw new Error("injected write failure");
      },
      sync: async () => {
        state.calls.push(`sync:${kind}`);
        if (kind === "directory" && state.failDirectorySync) {
          throw new Error("injected directory sync failure");
        }
      },
      close: async () => {
        state.calls.push(`close:${kind}`);
      },
    } as unknown as FileHandle;
  }),
);

vi.mock("node:fs/promises", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs/promises")>();
  return {
    ...actual,
    mkdir: vi.fn(async () => {
      state.calls.push("mkdir");
    }),
    open: openMock,
    chmod: vi.fn(async () => {
      state.calls.push("chmod");
    }),
    rename: vi.fn(async () => {
      state.calls.push("rename");
    }),
    rm: vi.fn(async () => {
      state.calls.push("rm");
    }),
  };
});

let writeTextFileAtomic: typeof import("../src/files/atomic-write.js").writeTextFileAtomic;

beforeAll(async () => {
  ({ writeTextFileAtomic } = await import("../src/files/atomic-write.js"));
});

/**
 * A real filesystem cannot show that the data reached the platter before the
 * rename, and no crash-injection harness runs in this suite, so the durability
 * ordering is observed through the syscall boundary instead. Only the two
 * ordering relations the contract states are asserted; the transcript itself
 * (how many opens, where chmod sits, whether writeFile or fs.writeFile is
 * used) is implementation strategy. The content, permission, replacement, and
 * cleanup claims are asserted against a real filesystem in
 * `tests/atomic-write.test.ts`.
 */
const relativeOrder = (call: string): number => state.calls.indexOf(call);

describe("durable atomic write ordering", () => {
  beforeEach(() => {
    state.calls.length = 0;
    state.failDirectorySync = false;
    state.failTempWrite = false;
  });

  it("syncs the file before rename and the parent directory after rename", async () => {
    await writeTextFileAtomic("/tmp/state.json", "content", { mode: 0o600 });

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

  it("still removes the temp file when the parent-directory sync fails after rename", async () => {
    state.failDirectorySync = true;

    await expect(
      writeTextFileAtomic("/tmp/state.json", "content"),
    ).rejects.toThrow("injected directory sync failure");

    expect(relativeOrder("rename")).toBeGreaterThanOrEqual(0);
    expect(relativeOrder("rm")).toBeGreaterThan(relativeOrder("rename"));
  });

  it("never renames a temp file whose write failed", async () => {
    state.failTempWrite = true;

    await expect(
      writeTextFileAtomic("/tmp/state.json", "content"),
    ).rejects.toThrow("injected write failure");

    expect(state.calls).not.toContain("rename");
    expect(state.calls).not.toContain("sync:file");
    expect(relativeOrder("rm")).toBeGreaterThanOrEqual(0);
  });
});
