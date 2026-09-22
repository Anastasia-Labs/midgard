import {
  commitMidgardCekBlob,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekDataNode,
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
} from "@al-ft/midgard-core/cek-semantic";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import type { Hash32 } from "@al-ft/midgard-core/codec/hash";
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

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const dummyRetainedWitnessEntry = (
  eventKey: SDK.EventKey,
  executionIndex = 0n,
): SDK.DaPayloadEntry => {
  const key = SDK.encodeRetainedValidationWitnessKey({
    event_key: eventKey,
    execution_index: executionIndex,
  });
  const value = SDK.encodeRetainedValidationWitness({
    machine_state: {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Normal",
      prior_ledger_root: "05".repeat(32),
      phase: "NativeScripts",
      program_counter: 9n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "07".repeat(32),
      ledger_delta_root: "08".repeat(32),
    },
    trace_proof: {
      state_index: 9n,
      state_hash: "09".repeat(32),
      siblings: [],
    },
    phase: 9n,
    program_counter: 9n,
    witness_cbor: "80",
    auxiliary: "NoAuxiliaryWitness",
  });
  return [key.toString("hex"), value.toString("hex")];
};

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
  envelopes: readonly MidgardCekProgramEnvelope[],
  material: readonly MidgardCekProgramMaterialEntry[],
): Promise<SDK.DaPayload> => {
  const fixture = await makePayloadFixture();
  const scriptWitnesses = encodeMidgardVersionedScriptListPreimage(
    envelopes.map((envelope) => ({
      language: "MidgardV1" as const,
      scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
    })),
  );
  const rewrittenTransactions = fixture.payload.block_body.transaction_preimages
    .map(([oldTxId, txCborHex]) => {
      const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(txCborHex, "hex"),
      );
      const tx = materializeMidgardNativeTxFromCanonical({
        ...decoded,
        witnessSet: {
          ...decoded.witnessSet,
          scriptTxWitsPreimageCbor: scriptWitnesses,
        },
      });
      const txId = computeMidgardNativeTxId(tx).toString("hex");
      const source = deriveMidgardNativeTxProofSource(tx);
      const committedSource: SDK.L2TransactionSource = {
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
        txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
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
          SDK.L2TransactionSourceSchema as never,
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
              encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
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
  readonly payload: SDK.DaPayload;
  readonly materialEntry: SDK.DaPayloadEntry;
}> => {
  const terminal = { kind: "error" } as const;
  const preimage = encodeMidgardCekTermNode(terminal);
  const root = hashMidgardCekTermNode(terminal);
  const envelope = {
    uplcVersion: [1n, 1n, 0n] as const,
    termRoot: root,
    nodeCount: 1n,
    materialByteLength: BigInt(preimage.length),
  };
  const material = [{ kind: "term", root, preimage }] as const;
  const materialEntry = [
    root.toString("hex"),
    encodeMidgardCekProgramMaterialDaValue(material[0]).toString("hex"),
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
  readonly envelopes: readonly MidgardCekProgramEnvelope[];
  readonly material: readonly MidgardCekProgramMaterialEntry[];
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
  const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
  const rawBlob = commitMidgardCekBlob(rawBytes);
  const semanticNode = {
    kind: "bytes",
    bytesRoot: rawBlob.root,
    bytesLength: BigInt(rawBytes.length),
    cborLength: midgardCekDataBytesCborLength(BigInt(rawBytes.length)),
    memory: 4n + BigInt(rawBytes.length),
  } as const;
  const semanticRoot = hashMidgardCekDataNode(semanticNode);
  const valueNode = {
    kind: "constant",
    typeRoot: typeBlob.root,
    payloadRoot: semanticRoot,
    payloadLength: BigInt(payloadCbor.length),
    semanticRoot,
    memory: BigInt(rawBytes.length),
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  let termRoot = hashMidgardCekTermNode(termNode);
  const material: MidgardCekProgramMaterialEntry[] = [
    {
      kind: "term",
      root: termRoot,
      preimage: encodeMidgardCekTermNode(termNode),
    },
    {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(valueNode),
    },
    {
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNode(semanticNode),
    },
    ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
      ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
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
  const envelopes: MidgardCekProgramEnvelope[] = [];
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
    const preimage = encodeMidgardCekTermNode(wrapper);
    termRoot = hashMidgardCekTermNode(wrapper);
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

  it("rejects a retained no-aux witness outside admitted phases", async () => {
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
              dummyRetainedWitnessEntry(eventKey, -1n),
            ],
          },
        }),
      ),
    ).toThrow(/not an allowed reconstruction witness/u);
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
