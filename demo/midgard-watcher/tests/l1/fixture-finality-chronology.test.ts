import { readAdmittedLocalKupmiosBoundary } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  admitWatcherLocalBackfillFinality,
  type WatcherLocalBackfillFinalityReceipt,
} from "../../src/l1/finality-engine.js";
import { admitWatcherLocalBackfillObservation } from "../../src/l1/l1-adapter.js";
import { openWatcherLocalHistoricalCapture } from "../../src/l1/local-historical-capture.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

describe("local watcher fixture finality chronology", () => {
  it("uses block depth with forty one-slot descendants", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
      blockSlotInterval: 1,
    });
    try {
      await fixture.setNativeTip(fixture.emptySuccessorBlock.point);
      const tip = await fixture.growNativeTip(40);
      expect(
        BigInt(tip.slot) - BigInt(fixture.emptySuccessorBlock.point.slot),
      ).toBe(40n);
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig: fixture.watcherConfig,
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({ source });
      expect(
        BigInt(tip.blockNo) - BigInt(boundary.kupoCheckpoint.blockNo) + 1n,
      ).toBe(30n);
      expect(BigInt(boundary.kupoCheckpoint.slot)).toBeGreaterThan(
        BigInt(fixture.activationBlock.point.slot),
      );
    } finally {
      await fixture.close();
    }
  });

  it("provides the strict boundary with twenty-slot blocks and requires a newer tip for finalization", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
      blockSlotInterval: 20,
    });
    try {
      await fixture.setNativeTip(fixture.emptySuccessorBlock.point);
      const initialTip = await fixture.growNativeTip(40);
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig: fixture.watcherConfig,
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({ source });
      expect(boundary.ogmiosTip).toMatchObject(initialTip);
      expect(BigInt(boundary.kupoCheckpoint.slot)).toBeGreaterThan(
        BigInt(fixture.activationBlock.point.slot),
      );
      expect(
        BigInt(initialTip.slot) - BigInt(boundary.kupoCheckpoint.slot),
      ).toBe(580n);
      expect(
        BigInt(initialTip.blockNo) -
          BigInt(boundary.kupoCheckpoint.blockNo) +
          1n,
      ).toBe(30n);

      const capture = async (
        previous: WatcherLocalBackfillFinalityReceipt | null,
      ) => {
        const owner = await openWatcherLocalHistoricalCapture({
          watcherConfig: fixture.watcherConfig,
          deploymentIdentity: fixture.deploymentIdentity,
          nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
          point: fixture.activationBlock.point,
          limits: { timeoutMs: 10_000 },
        });
        try {
          return admitWatcherLocalBackfillFinality({
            watcherConfig: fixture.watcherConfig,
            deploymentIdentity: fixture.deploymentIdentity,
            observation: admitWatcherLocalBackfillObservation(owner.receipt),
            previous,
          });
        } finally {
          await owner.close();
        }
      };
      const first = await capture(null);
      expect(first.result.action).toBe("observe_pending");
      expect(first.admitted).not.toBeNull();
      const repeated = await capture(first.admitted);
      expect(repeated.result.action).toBe("duplicate");
      const advancedTip = await fixture.growNativeTip();
      const finalized = await capture(first.admitted);
      expect(finalized.result.action).toBe("finalize");
      expect(finalized.result.protocolDecision).toBe("finality_granted");
      expect(finalized.result.state?.finalized?.visibilityCount).toBe("2");
      expect((await fixture.readNativeQueries()).map(({ tip }) => tip)).toEqual(
        [
          initialTip,
          initialTip,
          initialTip,
          initialTip,
          advancedTip,
          advancedTip,
        ],
      );
    } finally {
      await fixture.close();
    }
  }, 30_000);
});
