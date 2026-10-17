import "./header-root-reconstruction.w22-adjacent-boundaries.js";

import { createHash } from "node:crypto";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import { WATCHER_HEADER_COUNT_FIELDS } from "../../src/verification/header-root-reconstruction.js";
import {
  buildFixture,
  clonePayload,
  commitMutatedHeader,
  evaluateFixture,
  reencode,
} from "./header-root-reconstruction.build-fixture.js";
import { corpusTransaction } from "./header-root-reconstruction.watcher-header-record.js";

describe("W22 per-count mismatch determinism", () => {
  const countMutations: readonly [
    (typeof WATCHER_HEADER_COUNT_FIELDS)[number],
    keyof SDK.Header,
  ][] = [
    ["withdrawal_count", "withdrawalCount"],
    ["forced_transaction_count", "forcedTransactionCount"],
    ["l2_transaction_count", "l2TransactionCount"],
    ["deposit_count", "depositCount"],
    ["total_event_count", "totalEventCount"],
    ["transition_step_count", "transitionStepCount"],
    ["validation_trace_count", "validationTraceCount"],
  ];

  it.each(countMutations)(
    "reports exactly %s when that count diverges",
    async (field, headerField) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
        depositBytes: [11],
        withdrawalBytes: [21],
      });
      // N2 authenticates validation traces with the committed count label.
      // Rebind that label to isolate the independent declared-count check.
      const validationRoot =
        headerField === "validationTraceCount"
          ? await buildCountedRoot(
              SDK.ROOT_DOMAINS.validationTraces,
              fixture.payload.block_body.validation_traces.map(
                ([key, value]) => ({
                  key: Buffer.from(key, "hex"),
                  value: Buffer.from(value, "hex"),
                }),
              ),
            )
          : null;
      const validationTracesRoot =
        validationRoot === null
          ? fixture.header.validationTracesRoot
          : await commitCountedRoot({
              domain: SDK.ROOT_DOMAINS.validationTraces,
              phasRoot: validationRoot.phasRoot,
              count: fixture.header.validationTraceCount + 7n,
            });
      const mutated = await commitMutatedHeader(fixture, (header) => ({
        ...header,
        validationTracesRoot,
        [headerField]: (header[headerField] as bigint) + 7n,
      }));
      const result = await evaluateFixture(mutated);
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual(["count_mismatch"]);
      expect(result.countMismatches).toStrictEqual([field]);
      expect(result.rootMismatches).toStrictEqual([]);
      expect(result.reconstructedCounts).toBeNull();
    },
  );

  it("refuses a changed validation count before count classification when its root is not rebound", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      validationTraceCount: header.validationTraceCount + 7n,
    }));
    const result = await evaluateFixture(mutated);
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toEqual(["root_mismatch"]);
    expect(result.rootMismatches).toEqual(["validation_traces_root"]);
    expect(result.countMismatches).toEqual([]);
  });

  it("covers every declared count field exactly once", () => {
    expect(countMutations.map(([field]) => field)).toStrictEqual([
      ...WATCHER_HEADER_COUNT_FIELDS,
    ]);
  });

  it("reports the declared-versus-member count divergence separately", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
      depositBytes: [11],
    });
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        counts: {
          ...payload.block_body.counts,
          depositCount: payload.block_body.counts.depositCount + 1n,
        },
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual([
      "count_mismatch",
      "declared_counts_member_mismatch",
    ]);
    expect(result.countMismatches).toStrictEqual(["deposit_count"]);
  });
});

describe("W22 payload-entry mutations", () => {
  it("rejects reordered payload entries", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
    });
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        transactions: [...payload.block_body.transactions].reverse(),
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["invalid_payload_entries"]);
  });

  it("rejects a duplicated source event key", async () => {
    const fixture = await buildFixture({ depositBytes: [11, 12] });
    const payload = clonePayload(fixture.payload);
    const first = payload.block_body.deposits[0]!;
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        deposits: [first, first],
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["invalid_payload_entries"]);
  });

  it("rejects two swapped transactions", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
    });
    const payload = clonePayload(fixture.payload);
    const [a, b] = payload.block_body.transactions;
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: {
        ...payload.block_body,
        transactions: [
          [a![0], b![1]],
          [b![0], a![1]],
        ],
      },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["root_mismatch"]);
    expect(result.rootMismatches).toStrictEqual(["transactions_root"]);
  });
});

describe("W22 malformed payload bytes", () => {
  it("rejects an undecodable envelope", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture, {
      envelope: Buffer.from("deadbeef", "hex"),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
    expect(result.payloadSha256).toBeNull();
  });

  it("rejects an unknown content encoding", async () => {
    const fixture = await buildFixture();
    const inner = SDK.encodeDaPayload(fixture.payload);
    // The canonical encoder refuses to emit an unknown encoding, so the wire
    // bytes are assembled directly: [version, content_encoding, inner_bytes,
    // inner_sha256, body] with an encoding the decoder must not accept.
    const envelope = encodeCbor([
      1n,
      7n,
      BigInt(inner.length),
      createHash("sha256").update(inner).digest(),
      inner,
    ]);
    const result = await evaluateFixture(fixture, { envelope });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
    expect(result.payloadSha256).toBeNull();
  });

  it("rejects an oversize payload", async () => {
    const fixture = await buildFixture();
    const result = await evaluateFixture(fixture, {
      envelope: Buffer.alloc(DA_TRANSPORT_LIMITS.maxPayloadBytes + 1),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects truncated CBOR", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture, {
      envelope: fixture.envelope.subarray(0, fixture.envelope.length - 8),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects a wrong DA payload version", async () => {
    const fixture = await buildFixture();
    // `encodeDaPayload` refuses to serialise a non-V1 version, so the version
    // integer is patched on the wire. Its offset is fixed by the encoding:
    // constructor tag `d8799f` then the version integer.
    const inner = Buffer.from(SDK.encodeDaPayload(fixture.payload));
    expect(inner.subarray(0, 4).toString("hex")).toBe("d8799f01");
    inner[3] = 0x02;
    const result = await evaluateFixture(fixture, {
      envelope: await wrapDaPayload(inner, { mode: "identity" }),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["malformed_payload"]);
  });

  it("rejects an embedded header_hash that is not the embedded header's hash", async () => {
    const fixture = await buildFixture();
    const payload = clonePayload(fixture.payload);
    const mutated: SDK.DaPayload = {
      ...payload,
      block_body: { ...payload.block_body, header_hash: h28(0x4d) },
    };
    const result = await evaluateFixture(fixture, {
      envelope: await reencode(mutated),
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["payload_header_mismatch"]);
  });
});
