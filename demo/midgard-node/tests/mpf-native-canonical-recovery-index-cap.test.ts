import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  encodeNativeMpfEventLog,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import {
  NativeMpfFullIndexCapExceeded,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import { FULL_INDEX_HEADER_BYTES } from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { nativeOwnerBinaryPath } from "./helpers/native-owner-binary.js";

const DEFAULT_RECORDS = 2_000_000;
const DEFAULT_BYTES = 512 * 1024 * 1024;

// The full-index caps the TypeScript owner reads at each use, so a test can
// lower one after the owner has opened and built its trie at the default.
const caps = vi.hoisted(() => ({
  records: 2_000_000,
  bytes: 512 * 1024 * 1024,
}));

vi.mock(
  "../src/services/mpf-native-owner/service.normalize-owner-options.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/services/mpf-native-owner/service.normalize-owner-options.js")
      >();
    return {
      ...actual,
      get FULL_INDEX_MAX_RECORDS() {
        return caps.records;
      },
      get FULL_INDEX_MAX_BYTES() {
        return caps.bytes;
      },
    };
  },
);

// A restore whose target closure is in the store in full but whose full index
// is over a cap is refused by that cap, with the store left as it was.
describe("native canonical root recovery over a full-index cap", () => {
  const paths: string[] = [];
  const services = new Set<ProductionNativeMpfOwnerService>();
  let binarySha256: string;

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(nativeOwnerBinaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    caps.records = DEFAULT_RECORDS;
    caps.bytes = DEFAULT_BYTES;
    for (const service of services) await service.close();
    services.clear();
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  const inspectStore = async (levelPath: string, recoveryId: string) => {
    const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
    await db.open();
    try {
      return {
        root: await db.get("__root__"),
        receipt: await db.get(`__canonical_recovery__:${recoveryId}`),
      };
    } finally {
      await db.close();
    }
  };

  /** An owner whose durable root is a descendant of the retained `target`. */
  const fixture = async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-native-cap-"));
    paths.push(directory);
    const options = {
      levelPath: join(directory, "ledger"),
      sidecarPath: join(directory, "ledger.sidecar"),
      binaryPath: nativeOwnerBinaryPath,
      binarySha256,
    };
    const seed = new Level<string, unknown>(options.levelPath, {
      valueEncoding: "json",
    });
    await seed.open();
    await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
    await seed.close();
    const service = await ProductionNativeMpfOwnerService.create(options);
    services.add(service);
    const insert = async (base: string, fill: number) => {
      const generation = await service.fork(base);
      const applied = await service.applyEvents(
        generation,
        encodeNativeMpfEventLog(base, [
          [
            {
              type: "insert",
              key: Buffer.alloc(32, fill),
              value: Buffer.alloc(64, fill + 10),
            },
          ],
        ]),
      );
      await service.promote(generation);
      return applied.candidateRoot;
    };
    const target = await insert(SDK.EMPTY_MERKLE_TREE_ROOT, 41);
    const descendant = await insert(target, 42);
    const plan = {
      recoveryId: Buffer.alloc(32, 61).toString("hex"),
      expectedRoot: descendant,
      targetRoot: target,
    };
    return { service, options, plan };
  };

  it.each([
    {
      cap: "FULL_INDEX_MAX_RECORDS" as const,
      lower: (): number => {
        caps.records = 0;
        return 0;
      },
      text: "over the full-index record cap FULL_INDEX_MAX_RECORDS = 0",
    },
    {
      cap: "FULL_INDEX_MAX_BYTES" as const,
      lower: (): number => {
        caps.bytes = FULL_INDEX_HEADER_BYTES;
        return FULL_INDEX_HEADER_BYTES;
      },
      text: `over the full-index byte cap FULL_INDEX_MAX_BYTES = ${FULL_INDEX_HEADER_BYTES.toString()}`,
    },
  ])(
    "refuses a retained target over $cap by that cap, changes nothing, and restores under a cap that covers it",
    async ({ cap, lower, text }) => {
      const { service, options, plan } = await fixture();
      const limit = lower();
      const refusal = await service.restoreCanonicalRoot(plan).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(refusal).toBeInstanceOf(NativeMpfFullIndexCapExceeded);
      expect(refusal).not.toBeInstanceOf(NativeMpfRootNotRetained);
      expect(refusal).not.toBeInstanceOf(NativeMpfRestoreReadFailed);
      const exceeded = refusal as NativeMpfFullIndexCapExceeded;
      expect(exceeded.targetRoot).toBe(plan.targetRoot);
      expect(exceeded.cap).toBe(cap);
      expect(exceeded.limit).toBe(limit);
      expect(exceeded.observed).toBeGreaterThan(limit);
      expect(exceeded.message).toContain(plan.targetRoot);
      expect(exceeded.message).toContain(text);
      expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);

      caps.records = DEFAULT_RECORDS;
      caps.bytes = DEFAULT_BYTES;
      await service.restoreCanonicalRoot(plan);
      expect((await service.diagnostics()).durableRoot).toBe(plan.targetRoot);
      await service.close();
      services.delete(service);
      expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
        root: plan.targetRoot,
        receipt: JSON.stringify(plan),
      });
    },
  );

  it("leaves the store unchanged when it refuses over a cap", async () => {
    const { service, options, plan } = await fixture();
    caps.records = 0;
    await expect(service.restoreCanonicalRoot(plan)).rejects.toBeInstanceOf(
      NativeMpfFullIndexCapExceeded,
    );
    caps.records = DEFAULT_RECORDS;
    await service.close();
    services.delete(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.expectedRoot,
      receipt: undefined,
    });
  });
});
