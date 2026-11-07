import { h28 } from "@al-ft/midgard-test-support/hex";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type BuildSchedulerRefreshTxConfig,
  buildUnsignedSchedulerRefreshTxProgram,
  encodeSchedulerDatumForChain,
  SCHEDULER_ASSET_NAME,
} from "../src/index.js";
import {
  makeCallbackProbeLucid,
  probeContext,
} from "./scheduler-refresh.scheduler-refresh-sdk-builder-on-the-lucid-emulator.js";
import { alwaysSucceedsSchedulerValidator } from "./scheduler-refresh.setup-scheduler-scene.js";

describe("scheduler refresh redeemer-callback consistency", () => {
  const scheduler = alwaysSucceedsSchedulerValidator();
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const utxo = (byte: string, outputIndex: number): UTxO =>
    ({
      txHash: byte.repeat(32),
      outputIndex,
      address: scheduler.spendingScriptAddress,
      assets: { lovelace: 5_000_000n },
      datum: null,
    }) as UTxO;
  const schedulerInput = utxo("10", 0);
  const activeTail = utxo("30", 0);
  const refreshedDatum = {
    ActiveOperator: { operator: h28(0x99), start_time: 42n },
  };
  const refreshedDatumCbor = encodeSchedulerDatumForChain(refreshedDatum);
  const config = (lucid: LucidEvolution) =>
    ({
      lucid,
      scheduler,
      operatorKeyHash: h28(0x99),
      schedulerInput,
      refreshedDatum,
      validFrom: 1_000n,
      validTo: 2_000n,
      selection: { kind: "Advance", activeNode: { utxo: activeTail } },
    }) satisfies BuildSchedulerRefreshTxConfig;

  it("publishes the redeemer when every callback resolution agrees", async () => {
    const context = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      1n,
      [activeTail],
    );
    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram(
        config(makeCallbackProbeLucid([context, context, context])),
      ),
    );
    expect(result.layout).toEqual({
      kind: "Advance",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeNodeRefInputIndex: 0n,
    });
  });

  it("refuses to publish a redeemer when two callback resolutions disagree", async () => {
    const first = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      1n,
      [activeTail],
    );
    const second = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      2n,
      [activeTail],
    );
    await expect(
      Effect.runPromise(
        buildUnsignedSchedulerRefreshTxProgram(
          config(makeCallbackProbeLucid([first, second])),
        ),
      ),
    ).rejects.toThrow(
      /resolved inconsistent scheduler refresh redeemers|Failed to build scheduler refresh tx/,
    );
  });
});
