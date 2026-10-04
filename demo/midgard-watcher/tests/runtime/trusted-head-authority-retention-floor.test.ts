import { mkdtemp, readdir, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import type { TrustedHeadAuthorityRecord } from "../../src/runtime/trusted-head-authority.exact-record.js";
import { compactTrustedHeadRecords } from "../../src/runtime/trusted-head-authority.retention-floor.js";

const disk = vi.hoisted(() => ({ full: false }));
vi.mock("node:fs/promises", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs/promises")>();
  return {
    ...actual,
    open: async (...args: Parameters<typeof actual.open>) => {
      const handle = await actual.open(...args);
      if (!disk.full || !String(args[0]).endsWith(".tmp")) return handle;
      return Object.assign(Object.create(handle) as typeof handle, {
        writeFile: async () => {
          throw Object.assign(new Error("ENOSPC: no space left on device"), {
            code: "ENOSPC",
          });
        },
        close: () => handle.close(),
      });
    },
  };
});

describe("trusted-head retention floor publication", () => {
  it("removes its staging file when the floor write fails, and rethrows", async () => {
    const directory = await mkdtemp(join(tmpdir(), "trusted-head-floor-"));
    const names = ["00000000000000000000.json", "00000000000000000001.json"];
    for (const [revision, name] of names.entries())
      await writeFile(
        join(directory, name),
        JSON.stringify({ head: { revision: revision.toString() } }),
      );
    disk.full = true;
    try {
      await expect(
        compactTrustedHeadRecords(
          directory,
          names,
          [],
          1,
          new Uint8Array(32),
          (value) => value as TrustedHeadAuthorityRecord,
        ),
      ).rejects.toThrow("ENOSPC");
    } finally {
      disk.full = false;
    }
    expect((await readdir(directory)).sort()).toEqual(names);
  });
});
