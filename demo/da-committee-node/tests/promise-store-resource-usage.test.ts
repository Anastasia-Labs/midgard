import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { JsonFileCommitteeStore } from "../src/store.js";
import { tempDir } from "./helpers.js";

describe("total JSON retained store input domain", () => {
  it("counts unrelated durable maps and exact serialized bytes without modifying records", async () => {
    const directory = await tempDir();
    const path = join(directory, "committee.json");
    const bytes = JSON.stringify({
      stateQueueHeaders: { historic: { headerHash: "historic" } },
      daPayloads: {},
      daSignatures: {},
      daConflictEvidence: {},
      daAttestationCandidates: {},
      l1Submissions: {},
      peerBroadcasts: {},
      peerHealth: { unrelated: { peerId: "unrelated" } },
      peerNonces: {},
      decisionOutbox: {},
    });
    await writeFile(path, bytes);
    const store = await JsonFileCommitteeStore.open(directory);
    try {
      const before = await readFile(path);
      expect(await store.promiseStoreResourceUsage()).toEqual({
        storeRecords: 2,
        storeEncodedBytes: Buffer.byteLength(bytes),
      });
      expect(await store.listDaSignatures()).toEqual([]);
      expect(await readFile(path)).toEqual(before);
    } finally {
      await store.close();
    }
  });
});
