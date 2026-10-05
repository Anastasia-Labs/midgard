import {
  decodeSingleCbor,
  hashMidgardValidationRejectionCode,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { forcedVerdictForRejection } from "../src/workflow/forced-rejection-reason.js";
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
  if (!("code" in fixture.phaseA))
    throw new Error("Oversized output did not reach the forced verdict writer");
  // The actual Phase A rejection must be writable under the frozen machine
  // code. OutputNonCanonical maps to E_INVALID_OUTPUT, so using that arm for
  // this E_INVALID_FIELD_TYPE rejection stopped forced classification.
  const verdict = forcedVerdictForRejection(fixture.phaseA);
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: {
        FieldItemWidthIllegal: { field_index: 2n, item_index: 1n },
      },
    },
  });
  if (verdict === "ForcedTxValid")
    throw new Error("Oversized output acquired an accepted verdict");
  expect(
    hashMidgardValidationRejectionCode(
      Buffer.from(
        SDK.rejectionCodeOf(verdict.ForcedTxInvalid.reason),
        "hex",
      ).toString("ascii"),
    ),
  ).toEqual(trace.tree.descriptor.rejectionCodeHash);
}, 120_000);
