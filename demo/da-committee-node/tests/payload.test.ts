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

import {
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadRoots,
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "../src/da/payload.js";
import { headerHashOf } from "../src/l1/follower/queue-derivation.js";
import {
  makePayloadFixture,
  withBuiltValidationTracePair,
  withHeaderFields,
} from "./helpers.js";
import { payloadWithProgramMaterial } from "./payload.payload-with-program-material.js";

describe("canonical V1 DA payload verification", () => {
  it("attests a block of script transactions whose retained-witness section is empty", async () => {
    const terminal = { kind: "error" } as const;
    const preimage = encodeMidgardCekTermNode(terminal);
    const root = hashMidgardCekTermNode(terminal);
    const unhashed = await payloadWithProgramMaterial(
      [
        {
          uplcVersion: [1n, 1n, 0n] as const,
          termRoot: root,
          nodeCount: 1n,
          materialByteLength: BigInt(preimage.length),
        },
      ],
      [{ kind: "term", root, preimage }],
    );
    expect(unhashed.block_body.cek_program_material).toHaveLength(1);
    expect(unhashed.block_body.validation_trace_witnesses).toEqual([]);
    const roots = await computeDaPayloadRoots(unhashed);
    const header = {
      ...unhashed.block_body.header,
      ...roots,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTraceCount: 0n,
    };
    const headerHash = headerHashOf(header);
    const payload: SDK.DaPayload = {
      ...unhashed,
      block_body: {
        ...unhashed.block_body,
        header,
        header_hash: headerHash,
      },
    };
    const verified = await verifyDaPayloadAgainstHeader(
      await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" }),
      headerHash,
      header,
      {
        payloadSchemaVersion: 1,
        stateQueueOutRef: "state-queue#0",
      },
    );
    expect(verified.validation.rootsMatch).toBe(true);
    expect(verified.counts.validationTraceCount).toBe(
      BigInt(payload.block_body.transactions.length),
    );
  });

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
        validationTracesRoot: (await computeDaPayloadRoots(fixture.payload))
          .validationTracesRoot,
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

  it("attests an honest payload with transactions under a header that pins the validation-trace pair to the empty tree", async () => {
    const fixture = await makePayloadFixture();
    expect(fixture.header.l2TransactionCount).toBe(3n);
    expect(fixture.header.validationTracesRoot).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    expect(fixture.header.validationTraceCount).toBe(0n);
    const verified = await verifyDaPayloadAgainstHeader(
      fixture.payloadCbor,
      fixture.headerHash,
      fixture.header,
      { payloadSchemaVersion: 1, stateQueueOutRef: "state-queue#0" },
    );
    expect(verified.validation.rootsMatch).toBe(true);
    // The verified roots and counts are the body's: the built pair.
    expect(verified.roots.validationTracesRoot).not.toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    expect(verified.counts.validationTraceCount).toBe(3n);
  });

  it.each([
    [
      "root",
      { validationTracesRoot: true, validationTraceCount: false },
      "root_mismatch",
      /: validation_traces_root$/u,
    ],
    [
      "count",
      { validationTracesRoot: false, validationTraceCount: true },
      "count_mismatch",
      /: validation_trace_count$/u,
    ],
    [
      "pair",
      { validationTracesRoot: true, validationTraceCount: true },
      "root_mismatch",
      /: validation_traces_root$/u,
    ],
  ] as const)(
    "refuses a header that commits the built validation-trace %s",
    async (_, carried, code, message) => {
      const fixture = await withBuiltValidationTracePair(
        await makePayloadFixture(),
        carried,
      );
      await expect(
        verifyDaPayloadAgainstHeader(
          fixture.payloadCbor,
          fixture.headerHash,
          fixture.header,
          { payloadSchemaVersion: 1, stateQueueOutRef: "state-queue#0" },
        ),
      ).rejects.toMatchObject({
        code,
        message: expect.stringMatching(message),
      });
    },
  );

  it.each([
    ["l2_transaction_count", "l2TransactionCount"],
    ["transition_step_count", "transitionStepCount"],
  ] as const)(
    "still refuses a pinned header whose %s differs from the payload",
    async (field, key) => {
      const honest = await makePayloadFixture();
      expect(honest.header.validationTraceCount).toBe(0n);
      const fixture = await withHeaderFields(honest, {
        [key]: honest.header[key] + 1n,
      });
      await expect(
        verifyDaPayloadAgainstHeader(
          fixture.payloadCbor,
          fixture.headerHash,
          fixture.header,
          { payloadSchemaVersion: 1, stateQueueOutRef: "state-queue#0" },
        ),
      ).rejects.toMatchObject({
        code: "count_mismatch",
        message: expect.stringMatching(new RegExp(`: ${field}$`, "u")),
      });
    },
  );

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
    // A header (and payload) that commits a transactions root other than
    // the one the payload's transactions derive.
    const header = {
      ...fixture.header,
      transactionsRoot: "ab".repeat(32),
    };
    const headerHash = headerHashOf(header);
    const inner = SDK.encodeDaPayload({
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: headerHash,
        header,
      },
    });
    const stored = await wrapDaPayload(inner, { mode: "identity" });

    await expect(
      verifyDaPayloadAgainstHeader(stored, headerHash, header, {
        payloadSchemaVersion: 1,
        stateQueueOutRef: "state-queue#0",
      }),
    ).rejects.toMatchObject({
      code: "root_mismatch",
      message: expect.stringMatching(/: transactions_root$/u),
    });
  });

  it("takes the post-state root from the header: the payload carries no UTxO set", async () => {
    // A block deep in the chain: its parent payload is nowhere, and its
    // pre- and post-state roots are not the empty set's.
    const fixture = await makePayloadFixture(3, {
      prevHeaderHash: "11".repeat(28),
      prevUtxosRoot: "cd".repeat(32),
      utxosRoot: "ef".repeat(32),
    });
    const verified = await verifyDaPayloadAgainstHeader(
      fixture.payloadCbor,
      fixture.headerHash,
      fixture.header,
      { payloadSchemaVersion: 1, stateQueueOutRef: "state-queue#0" },
    );
    expect(verified.roots.utxosRoot).toBe("ef".repeat(32));
    expect(verified.validation.rootsMatch).toBe(true);
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
});
