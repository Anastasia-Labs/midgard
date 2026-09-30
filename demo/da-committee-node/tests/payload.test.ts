import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/cek-semantic";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/da/payload.js";
import "./helpers.js";
import "./payload.payload-with-program-material.js";
import "./payload.make-da-bytes-constant-material.js";

import {
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadRoots,
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "../src/da/payload.js";
import { makePayloadFixture } from "./helpers.js";
import { makeDaBytesConstantMaterial } from "./payload.make-da-bytes-constant-material.js";
import {
  dummyRetainedWitnessEntry,
  payloadWithDuplicateProgramEnvelopes,
  payloadWithProgramMaterial,
  sortedEntries,
} from "./payload.payload-with-program-material.js";

describe("canonical V1 DA payload verification", () => {
  it("rejects duplicate and orphan retained validation witness coordinates", async () => {
    const fixture = await makePayloadFixture();
    const existingEvent = LucidData.from(
      fixture.payload.block_body.validation_traces[0]![0],
      SDK.EventKeySchema as never,
    ) as SDK.EventKey;
    const duplicate = dummyRetainedWitnessEntry(existingEvent);
    expect(() =>
      decodeDaPayloadStrict(
        SDK.encodeDaPayload({
          ...fixture.payload,
          block_body: {
            ...fixture.payload.block_body,
            validation_trace_witnesses: [duplicate, duplicate],
          },
        }),
      ),
    ).toThrow(/duplicate/u);

    const orphan = dummyRetainedWitnessEntry({
      L2TransactionEventKey: { tx_id: "ff".repeat(32) },
    });
    expect(() =>
      decodeDaPayloadStrict(
        SDK.encodeDaPayload({
          ...fixture.payload,
          block_body: {
            ...fixture.payload.block_body,
            validation_trace_witnesses: [orphan],
          },
        }),
      ),
    ).toThrow(/orphaned/u);
  });

  it.each([
    // The fixture descriptors have step_count 0: state 0 is -1, the initial
    // endpoint -2 and the terminal endpoint -3.
    [-4n, /outside its descriptor's retained domain/u],
    [0n, /reserved for NativeScripts execution aliases/u],
    [-1n, /does not open the committed descriptor/u],
    [-2n, /does not open the committed descriptor/u],
    [-3n, /does not open the committed descriptor/u],
  ] as const)(
    "rejects a retained witness at coordinate %s that does not open its descriptor",
    async (executionIndex, message) => {
      const fixture = await makePayloadFixture();
      const eventKey = LucidData.from(
        fixture.payload.block_body.validation_traces[0]![0],
        SDK.EventKeySchema as never,
      ) as SDK.EventKey;
      expect(() =>
        decodeDaPayloadStrict(
          SDK.encodeDaPayload({
            ...fixture.payload,
            block_body: {
              ...fixture.payload.block_body,
              validation_trace_witnesses: [
                dummyRetainedWitnessEntry(eventKey, executionIndex),
              ],
            },
          }),
        ),
      ).toThrow(message);
    },
  );

  it("names the decoder's cause when a descriptor is not canonical Plutus Data", async () => {
    const fixture = await makePayloadFixture();
    expect(() =>
      decodeDaPayloadStrict(
        SDK.encodeDaPayload({
          ...fixture.payload,
          block_body: {
            ...fixture.payload.block_body,
            // The core codec's plain CBOR array is not the committed leaf.
            validation_traces: fixture.payload.block_body.validation_traces.map(
              ([key, value], index) => [
                key,
                index === 0 ? "8801015820" : value,
              ],
            ),
          },
        }),
      ),
    ).toThrow(/failed to decode validation_traces\[0\]\.value: .+/u);
  });

  it("decodes the canonical inner payload and derives every committed root", async () => {
    const fixture = await makePayloadFixture();

    expect(decodeDaPayloadStrict(fixture.innerPayloadCbor)).toEqual(
      fixture.payload,
    );
    await expect(computeDaPayloadRoots(fixture.payload)).resolves.toMatchObject(
      {
        utxosRoot: fixture.header.utxosRoot,
        transactionsRoot: fixture.header.transactionsRoot,
        transitionTraceRoot: fixture.header.transitionTraceRoot,
        eventToStepRoot: fixture.header.eventToStepRoot,
        validationTracesRoot: fixture.header.validationTracesRoot,
      },
    );
  });

  it("verifies mandatory envelope, header binding, roots, counts, and trace coverage", async () => {
    const fixture = await makePayloadFixture();

    const verified = await verifyDaPayloadAgainstHeader(
      fixture.payloadCbor,
      fixture.headerHash,
      fixture.header,
      {
        payloadSchemaVersion: 1,
        stateQueueOutRef: "state-queue#0",
      },
    );

    expect(Object.keys(verified).sort()).toEqual([
      "counts",
      "innerPayloadCbor",
      "payload",
      "payloadSha256",
      "roots",
      "storedPayloadCbor",
      "validation",
    ]);
    expect(verified).toMatchObject({
      payload: fixture.payload,
      storedPayloadCbor: fixture.payloadCbor,
      innerPayloadCbor: fixture.innerPayloadCbor,
      payloadSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      roots: {
        utxosRoot: fixture.header.utxosRoot,
        withdrawalsRoot: fixture.header.withdrawalsRoot,
        forcedTransactionsRoot: fixture.header.forcedTransactionsRoot,
        transactionsRoot: fixture.header.transactionsRoot,
        depositsRoot: fixture.header.depositsRoot,
        transitionTraceRoot: fixture.header.transitionTraceRoot,
        eventToStepRoot: fixture.header.eventToStepRoot,
        validationTracesRoot: fixture.header.validationTracesRoot,
      },
      counts: {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 3n,
        depositCount: 0n,
        totalEventCount: 3n,
        transitionStepCount: 3n,
        validationTraceCount: 3n,
      },
      validation: {
        payloadVersion: 1,
        rootsMatch: true,
        headerHash: fixture.headerHash,
      },
    });
    expect(verified.payloadSha256).toBe(
      SDK.daPayloadHashHex(fixture.payloadCbor),
    );
  });

  it("fails closed when the mandatory DA envelope is unavailable", async () => {
    const fixture = await makePayloadFixture();

    await expect(
      verifyDaPayloadAgainstHeader(
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

  it("rejects adjacent runtime payload schema versions before verification", async () => {
    const fixture = await makePayloadFixture();

    for (const payloadSchemaVersion of [0, 2]) {
      await expect(
        verifyDaPayloadAgainstHeader(
          fixture.payloadCbor,
          fixture.headerHash,
          fixture.header,
          {
            payloadSchemaVersion: payloadSchemaVersion as 1,
            stateQueueOutRef: "state-queue#0",
          },
        ),
      ).rejects.toMatchObject({
        code: "wrong_version",
      });
    }
  });

  it("rejects transaction preimage coverage gaps before attestation", () => {
    return makePayloadFixture().then((fixture) => {
      const malformed = SDK.encodeDaPayload({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          transaction_preimages:
            fixture.payload.block_body.transaction_preimages.slice(1),
        },
      });

      expect(() => decodeDaPayloadStrict(malformed)).toThrow(
        /exactly one canonical transaction preimage/u,
      );
    });
  });

  it("rejects well-formed payload members whose derived roots differ from the header", async () => {
    const fixture = await makePayloadFixture();
    const inner = SDK.encodeDaPayload({
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [
          [
            // The ledger key must stay *well formed* — the point of this case is
            // a root mismatch, not a malformed member — so it is built by the
            // §5.3 encoder rather than hand-written. CML's minimal-index form
            // (`825820…00`, 36 bytes) is not an admissible out-ref spelling and
            // would fail closed as `malformed_da` before the root comparison.
            encodeMidgardSpendInputItem({
              txId: Buffer.alloc(32, 0x01),
              outputIndex: 0,
            }).toString("hex"),
            "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
          ],
        ],
      },
    });
    const stored = await wrapDaPayload(inner, { mode: "identity" });

    await expect(
      verifyDaPayloadAgainstHeader(stored, fixture.headerHash, fixture.header, {
        payloadSchemaVersion: 1,
        stateQueueOutRef: "state-queue#0",
      }),
    ).rejects.toMatchObject({
      code: "root_mismatch",
    });
  });

  it("rejects duplicate transaction keys before committee attestation", async () => {
    const fixture = await makePayloadFixture();
    const firstTransaction = fixture.payload.block_body.transactions[0];
    expect(firstTransaction).toBeDefined();
    if (firstTransaction === undefined) {
      throw new Error("canonical fixture must contain a transaction");
    }
    const inner = SDK.encodeDaPayload({
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        transactions: [
          firstTransaction,
          firstTransaction,
          ...fixture.payload.block_body.transactions.slice(1),
        ],
      },
    });
    const stored = await wrapDaPayload(inner, { mode: "identity" });

    await expect(
      verifyDaPayloadAgainstHeader(stored, fixture.headerHash, fixture.header, {
        payloadSchemaVersion: 1,
        stateQueueOutRef: "state-queue#0",
      }),
    ).rejects.toMatchObject({
      code: "duplicate_key",
    });
  });

  it("rejects missing transition and validation trace evidence", () => {
    return makePayloadFixture().then((fixture) => {
      const missingTransition = SDK.encodeDaPayload({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          transition_trace:
            fixture.payload.block_body.transition_trace.slice(1),
        },
      });
      const missingValidation = SDK.encodeDaPayload({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          validation_traces:
            fixture.payload.block_body.validation_traces.slice(1),
        },
      });

      expect(() => decodeDaPayloadStrict(missingTransition)).toThrow(
        /payload counts do not match payload member arrays/u,
      );
      expect(() => decodeDaPayloadStrict(missingValidation)).toThrow(
        /validation_traces member count/u,
      );
    });
  });

  it("deduplicates repeated retained program envelopes without weakening exact material coverage", async () => {
    const fixture = await payloadWithDuplicateProgramEnvelopes();
    expect(() =>
      decodeDaPayloadStrict(SDK.encodeDaPayload(fixture.payload)),
    ).not.toThrow();

    const missing = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [],
      },
    };
    expect(() => decodeDaPayloadStrict(SDK.encodeDaPayload(missing))).toThrow(
      /exactly cover every inline and newly referenced V1 program/u,
    );

    const extraNode = { kind: "builtin", tag: 0n } as const;
    const extraPreimage = encodeMidgardCekTermNode(extraNode);
    const extraRoot = hashMidgardCekTermNode(extraNode);
    const extraEntry = [
      extraRoot.toString("hex"),
      encodeMidgardCekProgramMaterialDaValue({
        kind: "term",
        preimage: extraPreimage,
      }).toString("hex"),
    ] satisfies SDK.DaPayloadEntry;
    const extra = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: sortedEntries([
          fixture.materialEntry,
          extraEntry,
        ]),
      },
    };
    expect(() => decodeDaPayloadStrict(SDK.encodeDaPayload(extra))).toThrow(
      /exactly cover every inline and newly referenced V1 program/u,
    );
  });

  it("accepts distinct envelopes sharing a near-cap constant through strict coverage-only verification", async () => {
    const shared = makeDaBytesConstantMaterial(8_900, 3);
    expect(shared.payloadCborLength).toBeLessThanOrEqual(9_215);
    const payload = await payloadWithProgramMaterial(
      shared.envelopes,
      shared.material,
    );

    expect(() =>
      decodeDaPayloadStrict(SDK.encodeDaPayload(payload)),
    ).not.toThrow();
  });

  it("rejects an authenticated oversized semantic constant through the strict DA decoder", async () => {
    const oversized = makeDaBytesConstantMaterial(9_000, 0);
    expect(oversized.payloadCborLength).toBeGreaterThan(9_215);
    const payload = await payloadWithProgramMaterial(
      oversized.envelopes,
      oversized.material,
    );
    let rejection: unknown;

    try {
      decodeDaPayloadStrict(SDK.encodeDaPayload(payload));
    } catch (cause) {
      rejection = cause;
    }
    expect(rejection).toMatchObject({ code: "coverage_mismatch" });
    expect(
      (rejection as Error & { readonly cause?: unknown }).cause,
    ).toBeInstanceOf(Error);
    expect(
      (rejection as Error & { readonly cause: Error }).cause.message,
    ).toMatch(/source constant payload exceeds the 9215-byte/u);
  });
});
