import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
} from "./support/script-integrity-hash-mismatch-emulator.js";

const RELIABILITY_RESERVE_BYTES = 15_872;

describe("script-integrity-hash-mismatch publication fit", () => {
  it("publishes every applied role validator below the reliability reserve", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    expect([...references.keys()]).toStrictEqual([
      ...SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER,
    ]);
    const published = new Set<string>();
    for (const reference of references.values()) {
      if (published.has(reference.scriptHash)) continue;
      published.add(reference.scriptHash);
      const bytes = reference.publication.completeSignedBytes;
      console.info(
        `[script-integrity-hash-mismatch-publication] ${JSON.stringify({
          role: reference.role,
          scriptHash: reference.scriptHash,
          bytes,
          reserveMargin: RELIABILITY_RESERVE_BYTES - bytes,
        })}`,
      );
      expect(bytes, reference.role).toBeLessThanOrEqual(
        RELIABILITY_RESERVE_BYTES,
      );
      expect(reference.publication.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);
});
