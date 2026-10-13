import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { reconstructDaPayload } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-reconstruct";
import * as SDK from "@al-ft/midgard-sdk";
import {
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { describe, expect, it } from "vitest";

import { fixture } from "./validation-trace-field-limit.fixture.js";

describe(
  "forced events at supported field limits under placeholder descriptors",
  { timeout: 120_000 },
  () => {
    it.each([
      [1, 20_000, 0],
      [318, 200, 0],
      [1, 200, 1090],
    ])(
      "admits and reconstructs %i signatures with %i output bytes and %i observers",
      async (signatureCount, outputBytes, observerCount) => {
        const built = await fixture(signatureCount, outputBytes, observerCount);
        expect(built.member.value.step_count).toBe(1n);
        const bytes = SDK.encodeDaPayload(built.payload);
        expect(bytes.length).toBeLessThan(
          MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
        );
        // Compare the complete canonical wire bytes without enumerating every
        // byte as an object property in the generic deep-equality matcher.
        expect(
          SDK.encodeDaPayload(decodeDaPayloadStrict(bytes)).equals(bytes),
        ).toBe(true);
        const envelope = await wrapDaPayload(bytes, { mode: "identity" });
        const verified = await verifyDaPayloadAgainstHeader(
          envelope,
          built.headerHash,
          built.header,
          {
            payloadSchemaVersion: 1,
            stateQueueOutRef: `${"00".repeat(32)}#0`,
            preBlockUtxos: [[built.spent.toString("hex"), built.output]],
          },
        );
        expect(verified.counts.validationTraceCount).toBe(1n);
        const reconstruction = await reconstructDaPayload({
          payloadEnvelopeCbor: envelope,
          committedHeader: built.header,
        });
        expect(
          reconstruction.forcedTransactions[0]!.fullTransactionCbor,
        ).toEqual(built.forcedBytes);
      },
    );
  },
);
