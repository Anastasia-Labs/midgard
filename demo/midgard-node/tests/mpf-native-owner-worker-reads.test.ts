/**
 * A worker reads a ledger through its owner port: trie nodes by hash
 * (`readNodes`) from the owner's ledger store, outputs from its own reader.
 * What it reads, the proofs it builds and the roots its applies give are
 * those of the owner's own ledger at the same root. A request that does not
 * name node hashes is refused.
 */
import { createHash } from "node:crypto";
import { once } from "node:events";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { EMPTY_LEDGER_ROOT } from "@al-ft/midgard-fault-proofs/ledger-store";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { NativeMpfWorkerPortClient } from "../src/services/mpf-native-owner/client.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import type { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { workerLedger } from "../src/services/mpf-native-owner/worker-ledger.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";
import {
  createOwnerService,
  seedOwnerLedger,
} from "./helpers/native-owner-ledger.js";

const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent)
  warnNativeOwnerBinaryAbsent("mpf-native-owner-worker-reads");

const keys = [11, 22, 33].map((byte) => Buffer.alloc(32, byte));
const absent = Buffer.alloc(32, 44);
const outputOf = (key: Buffer) => Buffer.concat([Buffer.from("out:"), key]);
const outputs = (outRefs: readonly string[]) =>
  Promise.resolve(
    outRefs.map((outRef) => outputOf(Buffer.from(outRef, "hex"))),
  );

describe.skipIf(!binaryPresent)("worker reads through the owner port", () => {
  let directory = "";
  let service: ProductionNativeMpfOwnerService;
  let root = "";

  beforeAll(async () => {
    await prepareEventFlatDigest();
    directory = await mkdtemp(join(tmpdir(), "midgard-worker-reads-"));
    const levelPath = join(directory, "ledger");
    await seedOwnerLedger(levelPath, EMPTY_LEDGER_ROOT);
    service = await createOwnerService(
      levelPath,
      {
        binaryPath: nativeOwnerBinaryPath,
        binarySha256: createHash("sha256")
          .update(await readFile(nativeOwnerBinaryPath))
          .digest("hex"),
        sidecarPath: join(directory, "ledger.sidecar"),
      },
      outputs,
    );
    const handle = await service.fork(EMPTY_LEDGER_ROOT);
    root = (
      await service.applyEvents(
        handle,
        encodeNativeMpfEventLog(EMPTY_LEDGER_ROOT, [
          keys.map((key) => ({
            type: "insert" as const,
            key,
            value: outputOf(key),
          })),
        ]),
      )
    ).candidateRoot;
    await service.promote(handle);
  }, 60_000);

  afterAll(async () => {
    await service?.close();
    await rm(directory, { recursive: true, force: true });
  });

  it("reads outputs and proofs, and applies to the roots, the owner's ledger gives", async () => {
    const client = new NativeMpfWorkerPortClient(service.createWorkerPort());
    try {
      const worker = workerLedger(client, root, outputs);
      const owner = await service.ledger(root);
      const asked = [keys[1]!, absent];
      const [fromWorker, fromOwner] = await Promise.all(
        [worker, owner].map((ledger) => ledger.read(asked)),
      );
      for (const key of asked) {
        const outRef = key.toString("hex");
        const read = fromWorker!.get(outRef)!;
        expect(read.output).toEqual(fromOwner!.get(outRef)!.output);
        expect(await read.proof()).toEqual(
          await fromOwner!.get(outRef)!.proof(),
        );
      }
      expect(fromWorker!.get(keys[1]!.toString("hex"))!.output).toEqual(
        outputOf(keys[1]!),
      );
      expect(fromWorker!.get(absent.toString("hex"))!.output).toBeUndefined();
      const deleted = [{ type: "delete" as const, outRef: keys[0]! }];
      expect((await worker.apply(deleted)).root).toBe(
        (await owner.apply(deleted)).root,
      );
    } finally {
      client.close();
    }
  });

  it("answers a hash the store does not hold with nothing", async () => {
    const client = new NativeMpfWorkerPortClient(service.createWorkerPort());
    try {
      const [rootNode, missing] = await client.readNodes([
        root,
        "ab".repeat(32),
      ]);
      expect(rootNode?.__kind).toBe("Branch");
      expect(missing).toBeUndefined();
    } finally {
      client.close();
    }
  });

  it.each([
    ["a string, not an array", "ab".repeat(32)],
    ["the root marker key", ["__root__"]],
    ["upper-case hex", ["AB".repeat(32)]],
    ["a short hash", ["ab".repeat(31)]],
    ["a number", [7]],
  ])("refuses a readNodes request naming %s", async (_, hashes) => {
    const port = service.createWorkerPort();
    try {
      port.postMessage({ requestId: 1, method: "readNodes", hashes });
      const [response] = (await once(port, "message")) as [
        Record<string, unknown>,
      ];
      expect(response).toMatchObject({ requestId: 1, ok: false });
    } finally {
      port.close();
    }
  });
});
