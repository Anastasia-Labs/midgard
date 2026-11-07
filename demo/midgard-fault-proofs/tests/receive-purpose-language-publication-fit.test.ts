/**
 * `receivePurposeLanguage` signed publication fit: every one of the 21 role
 * scripts, fully applied, publishes as a reference script within the
 * 15,872-byte signed publication bound.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
} from "./support/receive-purpose-language-emulator.js";

const SIGNED_PUBLICATION_BOUND = 15_872;

describe("receivePurposeLanguage signed publication fit", () => {
  it("publishes every fully applied role within the signed publication bound", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const published = await publishReceivePurposeLanguageRoles(harness);
    expect([...published.keys()]).toStrictEqual([
      ...SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER,
    ]);
    for (const role of SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER) {
      const reference = published.get(role)!;
      const bytes = reference.publication.completeSignedBytes;
      console.info(
        `[receive-purpose-language-publication] ${JSON.stringify({ role, scriptHash: reference.scriptHash, bytes })}`,
      );
      expect(bytes, role).toBeLessThanOrEqual(SIGNED_PUBLICATION_BOUND);
    }
  }, 600_000);
});
