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

let writeTextFileAtomic: typeof import("@/files/atomic-write.js").writeTextFileAtomic;

beforeAll(async () => {
  ({ writeTextFileAtomic } = await import("@/files/atomic-write.js"));
});

describe("durable atomic write ordering and failures", () => {
  beforeEach(() => {
    state.calls.length = 0;
    state.failDirectorySync = false;
    state.failTempWrite = false;
  });

  it("syncs the file before rename and the parent directory after rename", async () => {
    await writeTextFileAtomic("/tmp/state.json", "content", { mode: 0o600 });

    expect(state.calls).toEqual([
      "mkdir",
      "write:file",
      "chmod",
      "sync:file",
      "close:file",
      "rename",
      "sync:directory",
      "close:directory",
    ]);
  });

  it("surfaces a parent-directory sync failure and cleans up", async () => {
    state.failDirectorySync = true;

    await expect(
      writeTextFileAtomic("/tmp/state.json", "content"),
    ).rejects.toThrow("injected directory sync failure");
    expect(state.calls).toEqual([
      "mkdir",
      "write:file",
      "sync:file",
      "close:file",
      "rename",
      "sync:directory",
      "close:directory",
      "rm",
    ]);
  });

  it("cleans up and does not rename when file sync preparation fails", async () => {
    state.failTempWrite = true;

    await expect(
      writeTextFileAtomic("/tmp/state.json", "content"),
    ).rejects.toThrow("injected write failure");
    expect(state.calls).toEqual(["mkdir", "write:file", "close:file", "rm"]);
  });
});
