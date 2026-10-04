import { decodeSingleCbor } from "@al-ft/midgard-core";
import { expect, it } from "vitest";

import { buildInstalledCanonicalFixture } from "./support/installed-canonical-fixture.js";

it("keeps the late oversized output ordinal ahead of an invalid signature", async () => {
  const fixture = await buildInstalledCanonicalFixture({
    operatorVkey: "11".repeat(28),
    now: 1750000000000,
    outputSizes: [100, 16385],
    invalidSignature: true,
  });
  const trace = fixture.challengerTrace;
  const rejectionStep = trace.witnesses.at(-2)!;
  expect(rejectionStep.phase).toBe("canonicalDecode");
  const control = decodeSingleCbor(rejectionStep.cbor) as unknown[];
  expect(control.slice(4, 6)).toEqual([2, 1]);
  expect(
    trace.witnesses.some((witness) => witness.phase === "signatures"),
  ).toBe(false);
}, 120_000);
