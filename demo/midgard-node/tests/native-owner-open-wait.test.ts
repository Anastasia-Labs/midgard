import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Level } from "level";
import { afterEach, describe, expect, it } from "vitest";

import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/index.js";
import { nativeOwnerOpenWait } from "../src/services/state-queue-correction-rewind.prepare-state-queue-correction-rewind.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  nativeOwnerBinarySha256,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";

/**
 * A retained native owner that cannot open yet keeps a history recovery's
 * gate closed for a re-evaluation instead of stopping the node: its LevelDB
 * lock still held (a predecessor owner or process not yet gone) or the host
 * briefly out of a resource. Every other open failure (a pinned binary that
 * does not match, a corrupt store) stays a failure.
 */

const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) warnNativeOwnerBinaryAbsent("native-owner-open-wait");

const temporaryPaths: string[] = [];
afterEach(async () => {
  await Promise.all(
    temporaryPaths
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const errno = (code: string) =>
  Object.assign(new Error(`${code}: injected`), { code });

it("waits on a resource-exhaustion cause anywhere on the cause chain, and on nothing else", () => {
  for (const code of ["EAGAIN", "EBUSY", "EMFILE", "ENFILE", "ENOMEM"])
    expect(
      nativeOwnerOpenWait(
        new Error("Native owner child failed", { cause: errno(code) }),
      ),
    ).toContain(code);
  for (const cause of [
    errno("ENOENT"),
    errno("EACCES"),
    new Error("binarySha256 does not match the pinned owner binary"),
    "LEVEL_LOCKED",
    undefined,
  ])
    expect(nativeOwnerOpenWait(cause)).toBeUndefined();
});

describe.skipIf(!binaryPresent)("retained native owner open", () => {
  it("waits while another holder keeps the store's lock, opens once it is released, and never waits on a binary mismatch", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-open-wait-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const options = {
      levelPath,
      binaryPath: nativeOwnerBinaryPath,
      binarySha256: nativeOwnerBinarySha256(),
    };
    const holder = new Level<string, unknown>(levelPath, {
      valueEncoding: "json",
    });
    await holder.open();
    await holder.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
    const locked = await ProductionNativeMpfOwnerService.create(options).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(locked).toBeDefined();
    expect(nativeOwnerOpenWait(locked)).toContain("LEVEL_LOCKED");
    await holder.close();
    const opened = await ProductionNativeMpfOwnerService.create(options);
    try {
      expect((await opened.diagnostics()).durableRoot).toBe(
        SDK.EMPTY_MERKLE_TREE_ROOT,
      );
    } finally {
      await opened.close();
    }
    const mismatch = await ProductionNativeMpfOwnerService.create({
      ...options,
      binarySha256: "00".repeat(32),
    }).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(mismatch).toBeDefined();
    expect(nativeOwnerOpenWait(mismatch)).toBeUndefined();
  }, 120_000);
});
