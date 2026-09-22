import {
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  admitWatcherLocalBackfillFinality,
  type WatcherLocalBackfillFinalityReceipt,
} from "../../src/l1/finality-engine.js";
import { admitWatcherLocalBackfillObservation } from "../../src/l1/l1-adapter.js";
import {
  openWatcherLocalHistoricalCapture,
  readWatcherLocalHistoricalCapture,
} from "../../src/l1/local-historical-capture.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  readWatcherNativeChainSyncEventReceipt,
  startWatcherNativeChainSync,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  watcherNativeChainSyncEventReceipt,
  type WatcherNativeChainSyncRuntime,
} from "../../src/l1/native-chain-sync.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { readWatcherUserEventScriptBinding } from "../../src/runtime/deployment-identity.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
} from "../support/user-event-origin-fixture.js";

describe("synthetic native stream and controlled tip", () => {
  it("exposes the default native tip before acquisition and preserves query-driven W12 growth", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipBaseDepth: 100,
    });
    try {
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig: parseWatcherConfig(fixture.watcherConfig),
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({ source });
      expect(boundary.kupoCheckpoint).toEqual(
        fixture.emptySuccessorBlock.point,
      );
      expect(await fixture.readNativeQueries()).toEqual([]);
      const finalized = await fixture.openFinalizedBlock(
        fixture.activationBlock,
      );
      await finalized.close();
      const queries = await fixture.readNativeQueries();
      expect(queries).toHaveLength(4);
      expect(new Set(queries.map((query) => query.tip.blockHash)).size).toBe(4);
      expect(queries.map((query) => query.tip.blockNo)).toEqual(
        [101, 102, 103, 104].map((depth) =>
          (
            BigInt(fixture.activationBlock.point.blockNo) + BigInt(depth)
          ).toString(),
        ),
      );
    } finally {
      await fixture.close();
    }
  });
  it("keeps 64 captures at one actual tip and advances W12 only after explicit native growth", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
    });
    const streams: WatcherNativeChainSyncRuntime[] = [];
    try {
      const blocks: SyntheticUserEventBlock[] = [];
      for (let index = 0; index < 64; index++)
        blocks.push(await fixture.makeBlock({ transactions: [] }));
      const last = blocks.at(-1)!;
      const initialTip = {
        blockHash: "ed".repeat(32),
        blockNo: (BigInt(last.point.blockNo) + 100n).toString(),
        slot: (BigInt(last.point.slot) + 600n).toString(),
      };
      await fixture.setNativeTip(initialTip);
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig: parseWatcherConfig(fixture.watcherConfig),
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({ source });
      const scripts = readWatcherUserEventScriptBinding({
        binding: fixture.scriptBinding,
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const unitHistory = await readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source,
        unit: scripts.hub.policyId + scripts.hub.assetName,
        point: boundary.kupoCheckpoint,
      });
      expect(unitHistory.transactions).toHaveLength(1);
      const creation = unitHistory.transactions[0]!;
      expect(creation.inclusionPoint).toEqual(fixture.activationBlock.point);
      const activation = await readAdmittedLocalKupmiosRawBlockAtPoint({
        source,
        point: creation.inclusionPoint,
      });
      expect(
        activation.transactions.find(
          (transaction) => transaction.txHash === creation.txHash,
        )?.transactionCbor,
      ).toBe(fixture.activationTransactionCbor);
      const enumerated: WatcherNativeChainSyncEvent[] = [];
      const enumeration = await startWatcherNativeChainSync({
        binaryPath: fixture.nativeChainSyncBinaryPath,
        watcherConfig: parseWatcherConfig(fixture.watcherConfig),
        intersection: {
          kind: "point",
          blockHash: fixture.emptySuccessorBlock.point.blockHash,
          slot: fixture.emptySuccessorBlock.point.slot,
        },
        startupTimeoutMs: 10_000,
        onEvent: async (event) => {
          enumerated.push(event);
        },
      });
      streams.push(enumeration);
      await expect.poll(() => enumerated.length, { timeout: 10_000 }).toBe(64);
      for (const [index, event] of enumerated.entries()) {
        expect(event.kind).toBe("roll_forward");
        if (event.kind !== "roll_forward")
          throw new Error("Expected native block");
        expect(admitWatcherNativeRollForwardBlock(event)).toEqual(
          blocks[index]!.nativeBlock,
        );
        expect(event.tip).toEqual({ kind: "point", ...initialTip });
      }
      await enumeration.close();
      const monitored: WatcherNativeChainSyncEvent[] = [];
      const monitor = await startWatcherNativeChainSync({
        binaryPath: fixture.nativeChainSyncBinaryPath,
        watcherConfig: parseWatcherConfig(fixture.watcherConfig),
        intersection: {
          kind: "point",
          blockHash: initialTip.blockHash,
          slot: initialTip.slot,
        },
        startupTimeoutMs: 10_000,
        onEvent: async (event) => {
          monitored.push(event);
        },
      });
      streams.push(monitor);
      expect(
        watcherNativeChainSyncAuthorityDetails(monitor.authority),
      ).toMatchObject({
        operation: { kind: "stream" },
        selectedIntersection: {
          kind: "point",
          blockHash: initialTip.blockHash,
          slot: initialTip.slot,
        },
        currentTip: { kind: "point", ...initialTip },
      });
      const pending: WatcherLocalBackfillFinalityReceipt[] = [];
      const capture = async (
        block: SyntheticUserEventBlock,
        previous: WatcherLocalBackfillFinalityReceipt | null,
      ) => {
        const args = {
          watcherConfig: parseWatcherConfig(fixture.watcherConfig),
          deploymentIdentity: fixture.deploymentIdentity,
          nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
          point: block.point,
          limits: { timeoutMs: 10_000 },
        };
        const owner = await openWatcherLocalHistoricalCapture(args);
        try {
          const tip = readWatcherLocalHistoricalCapture(
            owner.receipt,
          ).observedNativeTip;
          const observation = admitWatcherLocalBackfillObservation(
            owner.receipt,
          );
          return {
            tip,
            finality: admitWatcherLocalBackfillFinality({
              ...args,
              observation,
              previous,
            }),
          };
        } finally {
          await owner.close();
        }
      };
      for (const block of blocks) {
        const result = await capture(block, null);
        expect(result.tip).toEqual(initialTip);
        expect(result.finality.result.action).toBe("observe_pending");
        if (result.finality.admitted === null)
          throw new Error("Expected pending W12 receipt");
        pending.push(result.finality.admitted);
      }
      const firstQueries = await fixture.readNativeQueries();
      expect(firstQueries).toHaveLength(128);
      expect(firstQueries.map((query) => query.tip)).toEqual(
        blocks.flatMap(() => [initialTip, initialTip]),
      );
      expect(
        new Set(firstQueries.map((query) => query.target.blockHash)).size,
      ).toBe(64);
      expect(monitored).toEqual([]);
      const unchanged = await capture(blocks[0]!, pending[0]!);
      expect(unchanged.finality.result.action).toBe("duplicate");
      expect(unchanged.tip).toEqual(initialTip);
      const grown = await fixture.growNativeTip();
      await expect.poll(() => monitored.length, { timeout: 10_000 }).toBe(1);
      const advancement = monitored[0]!;
      if (advancement.kind !== "roll_forward")
        throw new Error("Expected tip advancement");
      expect(admitWatcherNativeRollForwardBlock(advancement).blockHash).toBe(
        grown.blockHash,
      );
      expect(advancement.prevHash).toBe(initialTip.blockHash);
      expect(advancement.tip).toEqual({ kind: "point", ...grown });
      for (const [index, block] of blocks.entries()) {
        const result = await capture(block, pending[index]!);
        expect(result.tip).toEqual(grown);
        expect(result.finality.result.action).toBe("finalize");
      }
      const allQueries = await fixture.readNativeQueries();
      expect(allQueries).toHaveLength(258);
      expect(allQueries.slice(130).map((query) => query.tip)).toEqual(
        blocks.flatMap(() => [grown, grown]),
      );
      const nativeReceipt = watcherNativeChainSyncEventReceipt(advancement);
      if (nativeReceipt === null)
        throw new Error("Expected live native event receipt");
      expect(readWatcherNativeChainSyncEventReceipt(nativeReceipt).event).toBe(
        advancement,
      );
      await fixture.rollbackNativeStream(initialTip);
      await expect
        .poll(() => monitored.some((event) => event.kind === "roll_backward"), {
          timeout: 10_000,
        })
        .toBe(true);
      expect(monitored.find((event) => event.kind === "roll_backward")).toEqual(
        {
          schemaVersion: advancement.schemaVersion,
          kind: "roll_backward",
          point: {
            kind: "point",
            blockHash: initialTip.blockHash,
            slot: initialTip.slot,
          },
          tip: { kind: "point", ...grown },
        },
      );
      expect(() =>
        readWatcherNativeChainSyncEventReceipt(nativeReceipt),
      ).toThrow();
      const exited = expect(monitor.done).rejects.toThrow();
      await fixture.exitNativeStream(7);
      await exited;
      expect(
        watcherNativeChainSyncAuthorityDetails(monitor.authority),
      ).toBeNull();
    } finally {
      await Promise.allSettled(streams.map((stream) => stream.close()));
      await fixture.close();
    }
  }, 120_000);
});
