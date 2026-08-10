import {
  commitMidgardCekBlobV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardCekTermNodeV1,
  encodeMidgardCekValueNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  type MidgardCekProgramEnvelopeV1,
  type MidgardCekProgramMaterialEntryV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekDataNodeV1,
  hashMidgardCekDataNodeV1,
  midgardCekDataBytesCborLengthV1,
} from "@al-ft/midgard-core/cek-semantic";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonicalV1,
} from "@al-ft/midgard-core/codec";
import type { Hash32 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadV1Roots,
  decodeDaPayloadV1Strict,
  verifyDaPayloadV1AgainstHeader,
} from "../src/da/payload.js";
import { makePayloadFixture } from "./helpers.js";

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const rewriteL2EventTxId = (
  event: SDK.EventKey,
  txIds: ReadonlyMap<string, string>,
): SDK.EventKey => {
  if (!("L2TransactionEventKey" in event)) return event;
  const current = event.L2TransactionEventKey.tx_id;
  return {
    L2TransactionEventKey: {
      tx_id: txIds.get(current) ?? current,
    },
  };
};

const payloadWithProgramMaterial = async (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
  material: readonly MidgardCekProgramMaterialEntryV1[],
): Promise<SDK.DaPayloadV1> => {
  const fixture = await makePayloadFixture();
  const scriptWitnesses = encodeMidgardVersionedScriptListPreimage(
    envelopes.map((envelope) => ({
      language: "MidgardV1" as const,
      scriptBytes: encodeMidgardCekProgramEnvelopeV1(envelope),
    })),
  );
  const rewrittenTransactions = fixture.payload.block_body.transaction_preimages
    .map(([oldTxId, txCborHex]) => {
      const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.from(txCborHex, "hex"),
      );
      const tx = materializeMidgardNativeTxFromCanonicalV1({
        ...decoded,
        witnessSet: {
          ...decoded.witnessSet,
          scriptTxWitsPreimageCbor: scriptWitnesses,
        },
      });
      const txId = computeMidgardNativeTxIdV1(tx).toString("hex");
      const source = deriveMidgardNativeTxProofSourceV1(tx);
      const committedSource: SDK.L2TransactionSourceV1 = {
        tx_id: txId,
        source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
      };
      return {
        oldTxId,
        txId,
        txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
        committedSource,
      };
    })
    .sort((left, right) => left.txId.localeCompare(right.txId));
  const rewrittenTxIds = new Map(
    rewrittenTransactions.map(({ oldTxId, txId }) => [oldTxId, txId]),
  );
  const rewriteEventCbor = (eventCbor: string): string =>
    LucidData.to(
      rewriteL2EventTxId(
        LucidData.from(eventCbor, SDK.EventKeySchema as never) as SDK.EventKey,
        rewrittenTxIds,
      ) as never,
      SDK.EventKeySchema as never,
    );
  const transitionTrace = fixture.payload.block_body.transition_trace.map(
    ([key, value]) => {
      const step = LucidData.from(
        value,
        SDK.TransitionStepSchema as never,
      ) as SDK.TransitionStep;
      return [
        key,
        LucidData.to(
          {
            ...step,
            event_key: rewriteL2EventTxId(step.event_key, rewrittenTxIds),
          } satisfies SDK.TransitionStep as never,
          SDK.TransitionStepSchema as never,
        ),
      ] satisfies SDK.DaPayloadEntry;
    },
  );
  return {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      transactions: rewrittenTransactions.map(({ txId, committedSource }) => [
        txId,
        LucidData.to(
          committedSource as never,
          SDK.L2TransactionSourceV1Schema as never,
        ),
      ]),
      transaction_preimages: rewrittenTransactions.map(({ txId, txCbor }) => [
        txId,
        txCbor,
      ]),
      cek_program_material: sortedEntries(
        material.map(
          (entry) =>
            [
              Buffer.from(entry.root).toString("hex"),
              encodeMidgardCekProgramMaterialDaValueV1(entry).toString("hex"),
            ] satisfies SDK.DaPayloadEntry,
        ),
      ),
      transition_trace: transitionTrace,
      event_to_step: sortedEntries(
        fixture.payload.block_body.event_to_step.map(([key, value]) => [
          rewriteEventCbor(key),
          value,
        ]),
      ),
      validation_traces: sortedEntries(
        fixture.payload.block_body.validation_traces.map(([key, value]) => [
          rewriteEventCbor(key),
          value,
        ]),
      ),
    },
  };
};

const payloadWithDuplicateProgramEnvelopes = async (): Promise<{
  readonly payload: SDK.DaPayloadV1;
  readonly materialEntry: SDK.DaPayloadEntry;
}> => {
  const terminal = { kind: "error" } as const;
  const preimage = encodeMidgardCekTermNodeV1(terminal);
  const root = hashMidgardCekTermNodeV1(terminal);
  const envelope = {
    uplcVersion: [1n, 1n, 0n] as const,
    termRoot: root,
    nodeCount: 1n,
    materialByteLength: BigInt(preimage.length),
  };
  const material = [{ kind: "term", root, preimage }] as const;
  const materialEntry = [
    root.toString("hex"),
    encodeMidgardCekProgramMaterialDaValueV1(material[0]).toString("hex"),
  ] satisfies SDK.DaPayloadEntry;
  return {
    payload: await payloadWithProgramMaterial(
      [envelope, { ...envelope, termRoot: Buffer.from(root) }],
      material,
    ),
    materialEntry,
  };
};

const makeDaBytesConstantMaterial = (
  rawByteLength: number,
  wrapperCount: number,
): {
  readonly envelopes: readonly MidgardCekProgramEnvelopeV1[];
  readonly material: readonly MidgardCekProgramMaterialEntryV1[];
  readonly payloadCborLength: number;
} => {
  const rawBytes = Buffer.alloc(rawByteLength, 0x5a);
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < rawBytes.length; offset += 64) {
    const chunk = rawBytes.subarray(offset, offset + 64);
    chunks.push(
      chunk.length < 24
        ? Buffer.from([0x40 + chunk.length])
        : Buffer.from([0x58, chunk.length]),
      chunk,
    );
  }
  chunks.push(Buffer.from([0xff]));
  const payloadCbor = Buffer.concat(chunks);
  const typeBlob = commitMidgardCekBlobV1(Buffer.from("9f01ff", "hex"));
  const rawBlob = commitMidgardCekBlobV1(rawBytes);
  const semanticNode = {
    kind: "bytes",
    bytesRoot: rawBlob.root,
    bytesLength: BigInt(rawBytes.length),
    cborLength: midgardCekDataBytesCborLengthV1(BigInt(rawBytes.length)),
    memory: 4n + BigInt(rawBytes.length),
  } as const;
  const semanticRoot = hashMidgardCekDataNodeV1(semanticNode);
  const valueNode = {
    kind: "constant",
    typeRoot: typeBlob.root,
    payloadRoot: semanticRoot,
    payloadLength: BigInt(payloadCbor.length),
    semanticRoot,
    memory: BigInt(rawBytes.length),
  } as const;
  const valueRoot = hashMidgardCekValueNodeV1(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  let termRoot = hashMidgardCekTermNodeV1(termNode);
  const material: MidgardCekProgramMaterialEntryV1[] = [
    {
      kind: "term",
      root: termRoot,
      preimage: encodeMidgardCekTermNodeV1(termNode),
    },
    {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNodeV1(valueNode),
    },
    {
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNodeV1(semanticNode),
    },
    ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
      ([rootHex, node]): MidgardCekProgramMaterialEntryV1 => ({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      }),
    ),
  ];
  let reachableNodeCount = BigInt(material.length);
  let reachableByteLength = material.reduce(
    (total, entry) => total + BigInt(entry.preimage.length),
    0n,
  );
  const envelopes: MidgardCekProgramEnvelopeV1[] = [];
  if (wrapperCount === 0) {
    envelopes.push({
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: reachableNodeCount,
      materialByteLength: reachableByteLength,
    });
  }
  const sharedConstantTermRoot = termRoot;
  for (let index = 0; index < wrapperCount; index += 1) {
    const wrapper = {
      kind: "application",
      function: termRoot,
      argument: sharedConstantTermRoot,
    } as const;
    const preimage = encodeMidgardCekTermNodeV1(wrapper);
    termRoot = hashMidgardCekTermNodeV1(wrapper);
    material.push({ kind: "term", root: termRoot, preimage });
    reachableNodeCount += 1n;
    reachableByteLength += BigInt(preimage.length);
    envelopes.push({
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: reachableNodeCount,
      materialByteLength: reachableByteLength,
    });
  }
  return { envelopes, material, payloadCborLength: payloadCbor.length };
};

describe("canonical V1 DA payload verification", () => {
  it("decodes the canonical inner payload and derives every committed root", async () => {
    const fixture = await makePayloadFixture();

    expect(decodeDaPayloadV1Strict(fixture.innerPayloadCbor)).toEqual(
      fixture.payload,
    );
    await expect(
      computeDaPayloadV1Roots(fixture.payload),
    ).resolves.toMatchObject({
      utxosRoot: fixture.header.utxosRoot,
      transactionsRoot: fixture.header.transactionsRoot,
      transitionTraceRoot: fixture.header.transitionTraceRoot,
      eventToStepRoot: fixture.header.eventToStepRoot,
      validationTracesRoot: fixture.header.validationTracesRoot,
    });
  });

  it("verifies mandatory envelope, header binding, roots, counts, and trace coverage", async () => {
    const fixture = await makePayloadFixture();

    const verified = await verifyDaPayloadV1AgainstHeader(
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

  it("rejects adjacent runtime payload schema versions before verification", async () => {
    const fixture = await makePayloadFixture();

    for (const payloadSchemaVersion of [0, 2]) {
      await expect(
        verifyDaPayloadV1AgainstHeader(
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

  it("rejects duplicate transaction keys before committee attestation", async () => {
    const fixture = await makePayloadFixture();
    const firstTransaction = fixture.payload.block_body.transactions[0];
    expect(firstTransaction).toBeDefined();
    if (firstTransaction === undefined) {
      throw new Error("canonical fixture must contain a transaction");
    }
    const inner = SDK.encodeDaPayloadV1({
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
      code: "duplicate_key",
    });
  });

  it("rejects missing transition and validation trace evidence", () => {
    return makePayloadFixture().then((fixture) => {
      const missingTransition = SDK.encodeDaPayloadV1({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          transition_trace:
            fixture.payload.block_body.transition_trace.slice(1),
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

  it("deduplicates repeated retained program envelopes without weakening exact material coverage", async () => {
    const fixture = await payloadWithDuplicateProgramEnvelopes();
    expect(() =>
      decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(fixture.payload)),
    ).not.toThrow();

    const missing = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [],
      },
    };
    expect(() =>
      decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(missing)),
    ).toThrow(/exactly cover every inline and newly referenced V1 program/u);

    const extraNode = { kind: "builtin", tag: 0n } as const;
    const extraPreimage = encodeMidgardCekTermNodeV1(extraNode);
    const extraRoot = hashMidgardCekTermNodeV1(extraNode);
    const extraEntry = [
      extraRoot.toString("hex"),
      encodeMidgardCekProgramMaterialDaValueV1({
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
    expect(() => decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(extra))).toThrow(
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
      decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(payload)),
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
      decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(payload));
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
