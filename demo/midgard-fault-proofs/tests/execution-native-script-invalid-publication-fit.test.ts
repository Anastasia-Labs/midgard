import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
} from "./support/execution-native-script-invalid-emulator.js";

const RELIABILITY_RESERVE_BYTES = 15_872;

describe("execution-native-script-invalid publication fit", () => {
  it("publishes every applied role validator below the reliability reserve", async () => {
    const harness = await makeExecutionNativeScriptInvalidHarness();
    const references = await publishExecutionNativeScriptInvalidRoles(harness);
    expect([...references.keys()]).toStrictEqual([
      ...SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER,
    ]);
    const published = new Set<string>();
    for (const reference of references.values()) {
      if (published.has(reference.scriptHash)) continue;
      published.add(reference.scriptHash);
      const bytes = reference.publication.completeSignedBytes;
      console.info(
        `[execution-native-script-invalid-publication] ${JSON.stringify({
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
