/**
 * A fresh node's start on an actual deployment on the emulator reads
 * `confirmed_ledger` in full zero times (`confirmed_ledger_full_scan_total`):
 * the startup preparation's clean-queue check, the native owner's genesis
 * admission and the first landed-block run, which anchors the frontier at
 * genesis, at the root the store admitted.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Metric, Ref } from "effect";
import { expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";
import { provideDatabaseLayers } from "./utils.js";

const fullScans = () =>
  Effect.runPromise(
    Metric.value(fullScanCounter).pipe(Effect.map(({ count }) => count)),
  );

it("starts a fresh node and anchors its confirmed ledger at genesis, reading confirmed_ledger in full zero times", async () => {
  const scans = await fullScans();
  const h = await openProductionLifecycle();
  try {
    await h.synchronize();
    expect((await fullScans()) - scans).toBe(0);
    const owner = await Effect.runPromise(Ref.get(h.globals.NATIVE_MPF_OWNER));
    const { durableRoot } = await owner!.diagnostics();
    expect(
      await Effect.runPromise(provideDatabaseLayers(Frontier.retrieve)),
    ).toEqual({ headerHash: SDK.GENESIS_HEADER_HASH, utxosRoot: durableRoot });
  } finally {
    await h.close();
  }
});
