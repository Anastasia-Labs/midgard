import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  makeExecutionSourceScriptDecodingHarness,
  publishExecutionSourceScriptDecodingRoles,
} from "./support/execution-source-script-decoding-emulator.js";

const RELIABILITY_RESERVE_BYTES = 15_872;

describe("execution-source-script-decoding publication fit", () => {
  it("publishes every applied role validator below the reliability reserve", async () => {
    const harness = await makeExecutionSourceScriptDecodingHarness();
    const references = await publishExecutionSourceScriptDecodingRoles(harness);
    expect([...references.keys()]).toStrictEqual([
      ...SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER,
    ]);
    const published = new Set<string>();
    for (const reference of references.values()) {
      if (published.has(reference.scriptHash)) continue;
      published.add(reference.scriptHash);
      const bytes = reference.publication.completeSignedBytes;
      console.info(
        `[execution-source-script-decoding-publication] ${JSON.stringify({
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
