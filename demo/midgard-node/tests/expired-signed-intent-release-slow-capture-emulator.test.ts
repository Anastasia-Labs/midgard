import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Cause, Chunk, Runtime } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  expectReplaced,
  moveToExactSlot,
  resetSharedRows,
  signedTtl,
} from "./helpers/signed-intent-replacement.js";
import { makeSlowLedgerScanTransport } from "./helpers/slow-ledger-scan-transport.js";

/**
 * The exact-point state-queue capture of the expired-intent release walks the
 * whole UTxO set on Ogmios (it has no address index), which on preprod took
 * 36 s with three clients of one node scanning at once, past the 30 s
 * per-request source deadline. The capture runs under the ledger-scan
 * deadline instead: an Ogmios answer slower than the transport's request
 * deadline still completes the release. Actual deployed validators, the
 * production history owner and Architecture G; only the transport is
 * synthetic.
 */

const C = Pending.Columns;

/** Why the owner stopped: each failure's own cause chain. */
const ownerFailure = (cause: unknown) =>
  (Runtime.isFiberFailure(cause)
    ? Chunk.toReadonlyArray(
        Cause.failures(cause[Runtime.FiberFailureCauseId]),
      ).map((failure) => (failure as { cause?: unknown }).cause ?? failure)
    : [cause]
  )
    .map((inner) => formatUnknownError(inner, { includeCause: true }))
    .join("; ");
const SOURCE_TIMEOUT_MS = 5_000;
const SCAN_MS = 8_000;

it("completes the expired-intent release when its exact-point ledger scan answers after the source request deadline", async () => {
  const slow = makeSlowLedgerScanTransport({
    timeoutMs: SOURCE_TIMEOUT_MS,
    delayMs: SCAN_MS,
  });
  const h = await openHistoryProductionOwnerLifecycle({
    transportFactory: slow.transportFactory,
  });
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const lost = await submitUnlandedBlock(h, inclusion);
    const journal = await readJournal(lost.submittedHeaderHash);
    moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
    // Only the release's capture scans the ledger from here on.
    slow.arm();
    // With the source deadline in force the capture times out: the owner
    // fails, or retries past this bound, and never becomes ready.
    const synchronized = h.synchronize().then(
      () => "ready" as const,
      (cause: unknown) => `unavailable: ${ownerFailure(cause)}`,
    );
    const outcome = await Promise.race([
      synchronized,
      new Promise<"stuck">((resolve) =>
        setTimeout(() => resolve("stuck"), 6 * SCAN_MS),
      ),
    ]);
    expect(outcome).toBe("ready");
    expect(slow.delays.length).toBeGreaterThan(0);
    for (const held of slow.delays)
      expect(held).toBeGreaterThan(SOURCE_TIMEOUT_MS);
    await expectReplaced(journal, { handle: h });
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
