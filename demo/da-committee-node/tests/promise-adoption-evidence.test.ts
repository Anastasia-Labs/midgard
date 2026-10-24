import { createHash } from "node:crypto";
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  committeeLoadedDistRoot,
  committeePromiseBundleDigest,
  loadTrustedPromiseEvidence,
} from "../src/availability/promise-adoption-evidence.js";
import { committeePromiseAdoptionConfig } from "../src/config.promise-admission.js";
import { tempDir } from "./helpers.js";

describe("explicit runtime adoption evidence", () => {
  it("derives the same actual dist root for bundled and split runtime entries", () => {
    expect(
      committeeLoadedDistRoot(
        "/workspace/demo/da-committee-node/dist/index.js",
      ),
    ).toBe("/workspace/demo/da-committee-node/dist");
    expect(
      committeeLoadedDistRoot(
        "/workspace/demo/da-committee-node/dist/availability/index.js",
      ),
    ).toBe("/workspace/demo/da-committee-node/dist");
    expect(() =>
      committeeLoadedDistRoot("/workspace/demo/src/index.js"),
    ).toThrow("outside a built dist");
  });
  it("keeps absence unavailable and refuses partial owner adoption", () => {
    expect(committeePromiseAdoptionConfig({})).toBeUndefined();
    expect(() =>
      committeePromiseAdoptionConfig({
        DA_PROMISE_POLICY_ARTIFACT_PATH: "/fixture",
      }),
    ).toThrow("requires valid");
  });
  it("loads only exact pinned bounded bytes", async () => {
    const path = join(await tempDir(), "evidence.json");
    const bytes = JSON.stringify({ measured: true });
    await writeFile(path, bytes);
    const digest = createHash("sha256").update(bytes).digest("hex");
    expect(
      await loadTrustedPromiseEvidence(path, digest, bytes.length),
    ).toEqual({ measured: true });
    await expect(
      loadTrustedPromiseEvidence(path, "00".repeat(32)),
    ).rejects.toThrow("digest mismatch");
    await expect(
      loadTrustedPromiseEvidence(path, digest, bytes.length - 1),
    ).rejects.toThrow("byte limit");
  });
  it("derives live bundle identity from implementation and dependency bytes", async () => {
    const directory = await tempDir();
    const root = join(directory, "dist");
    await mkdir(root);
    const module = join(root, "index.js");
    const lock = join(directory, "pnpm-lock.yaml");
    await writeFile(module, "export const value = 1;\n");
    await writeFile(lock, "lockfileVersion: '9.0'\n");
    const input = { packageRoots: { fixture: root }, lockfilePath: lock };
    const original = await committeePromiseBundleDigest(input);
    await writeFile(module, "export const value = 2;\n");
    const changed = await committeePromiseBundleDigest(input);
    expect(changed).not.toBe(original);
    await writeFile(lock, "lockfileVersion: '9.1'\n");
    expect(await committeePromiseBundleDigest(input)).not.toBe(changed);
  });
});
