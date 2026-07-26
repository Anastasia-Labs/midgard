import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadV1Roots,
  decodeDaPayloadV1Strict,
  verifyDaPayloadV1AgainstHeader,
} from "../src/da/payload.js";
import { makePayloadFixture } from "./helpers.js";

describe("canonical V1 DA payload verification", () => {
  it("decodes the canonical inner payload and derives every committed root", async () => {
    const fixture = await makePayloadFixture();

    expect(decodeDaPayloadV1Strict(fixture.innerPayloadCbor)).toEqual(
      fixture.payload,
    );
    await expect(computeDaPayloadV1Roots(fixture.payload)).resolves.toMatchObject({
      utxosRoot: fixture.header.utxosRoot,
      transactionsRoot: fixture.header.transactionsRoot,
      transitionTraceRoot: fixture.header.transitionTraceRoot,
      eventToStepRoot: fixture.header.eventToStepRoot,
      validationTracesRoot: fixture.header.validationTracesRoot,
    });
  });

  it("verifies mandatory envelope, header binding, roots, counts, and trace coverage", async () => {
    const fixture = await makePayloadFixture();

    await expect(
      verifyDaPayloadV1AgainstHeader(
        fixture.payloadCbor,
        fixture.headerHash,
        fixture.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: "state-queue#0",
        },
      ),
    ).resolves.toMatchObject({
      payloadSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      counts: {
        l2TransactionCount: 3n,
        validationTraceCount: 3n,
      },
      validation: {
        payloadVersion: 1,
        rootsMatch: true,
        headerHash: fixture.headerHash,
      },
    });
  });

  it("fails closed when the mandatory DA envelope is unavailable", async () => {
    const fixture = await makePayloadFixture();

    await expect(
      verifyDaPayloadV1AgainstHeader(
        fixture.innerPayloadCbor,
        fixture.headerHash,
        fixture.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: "state-queue#0",
        },
      ),
    ).rejects.toMatchObject({
      code: "malformed_da",
    });
  });

  it("rejects transaction preimage coverage gaps before attestation", () => {
    return makePayloadFixture().then((fixture) => {
      const malformed = SDK.encodeDaPayloadV1({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          transaction_preimages:
            fixture.payload.block_body.transaction_preimages.slice(1),
        },
      });

      expect(() => decodeDaPayloadV1Strict(malformed)).toThrow(
        /exactly one canonical transaction preimage/u,
      );
    });
  });

  it("rejects well-formed payload members whose derived roots differ from the header", async () => {
    const fixture = await makePayloadFixture();
    const inner = SDK.encodeDaPayloadV1({
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [
          [
            `825820${"01".repeat(32)}00`,
            "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
          ],
        ],
      },
    });
    const stored = await wrapDaPayloadV1(inner, { mode: "identity" });

    await expect(
      verifyDaPayloadV1AgainstHeader(
        stored,
        fixture.headerHash,
        fixture.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: "state-queue#0",
        },
      ),
    ).rejects.toMatchObject({
      code: "root_mismatch",
    });
  });

  it("rejects missing transition and validation trace evidence", () => {
    return makePayloadFixture().then((fixture) => {
      const missingTransition = SDK.encodeDaPayloadV1({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          transition_trace: fixture.payload.block_body.transition_trace.slice(1),
        },
      });
      const missingValidation = SDK.encodeDaPayloadV1({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          validation_traces:
            fixture.payload.block_body.validation_traces.slice(1),
        },
      });

      expect(() => decodeDaPayloadV1Strict(missingTransition)).toThrow(
        /payload counts do not match payload member arrays/u,
      );
      expect(() => decodeDaPayloadV1Strict(missingValidation)).toThrow(
        /validation_traces member count/u,
      );
    });
  });
});
