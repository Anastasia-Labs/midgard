import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { fixture, rebind, verdict } from "./foreign-block-import.fixture.js";

describe("complete foreign block import", () => {
  it("honestly imports a nonempty rejected forced event with unchanged ledger root", async () => {
    const payload = await fixture();
    expect(payload.block_body.header.forcedTransactionsRoot).not.toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    const result = await verdict(payload);
    expect(result._tag).toBe("Right");
    if (result._tag === "Right")
      expect(result.right).toEqual({
        headerHash: payload.block_body.header_hash,
        root: SDK.EMPTY_MERKLE_TREE_ROOT,
        entries: [],
      });
  });
  it("imports mixed event programs using an exact sidecar at each event", async () => {
    const first = await fixture();
    const second = await fixture({ withProgram: true, orderByte: "5b" });
    const body = second.block_body;
    const step = Data.from(body.transition_trace[0]![1], SDK.TransitionStep);
    const merged = await rebind({
      ...first,
      block_body: {
        ...first.block_body,
        header: {
          ...first.block_body.header,
          forcedTransactionCount: 2n,
          totalEventCount: 2n,
          transitionStepCount: 2n,
          validationTraceCount: 2n,
        },
        counts: {
          ...first.block_body.counts,
          forcedTransactionCount: 2n,
          totalEventCount: 2n,
          transitionStepCount: 2n,
          validationTraceCount: 2n,
        },
        forced_transactions: [
          ...first.block_body.forced_transactions,
          ...body.forced_transactions,
        ],
        forced_transaction_preimages: [
          ...first.block_body.forced_transaction_preimages,
          ...body.forced_transaction_preimages,
        ],
        cek_program_material: body.cek_program_material,
        transition_trace: [
          ...first.block_body.transition_trace,
          [
            encodeTransitionIntegerCbor(1n).toString("hex"),
            encodeTransitionStepCbor({ ...step, step_index: 1n }).toString(
              "hex",
            ),
          ],
        ],
        event_to_step: [
          ...first.block_body.event_to_step,
          [
            body.event_to_step[0]![0],
            encodeEventToStepValueCbor({
              step_index: 1n,
              phase: "ForcedTransaction",
            }).toString("hex"),
          ],
        ],
        validation_traces: [
          ...first.block_body.validation_traces,
          ...body.validation_traces,
        ],
        validation_trace_witnesses: [
          ...first.block_body.validation_trace_witnesses,
          ...body.validation_trace_witnesses,
        ],
      },
    });
    const result = await verdict(merged);
    expect(result._tag).toBe("Right");
    if (result._tag === "Left") throw result.left;
  });
  it.each(["transition", "mapping", "validation"] as const)(
    "refuses a self-consistent %s forgery with the same ledger root",
    async (field) => {
      const payload = await fixture();
      const body = payload.block_body;
      if (field === "transition") {
        const step = Data.from(
          body.transition_trace[0]![1],
          SDK.TransitionStep,
        );
        body.transition_trace[0] = [
          body.transition_trace[0]![0],
          encodeTransitionStepCbor({
            ...step,
            pre_utxos_root: "ab".repeat(32),
          }).toString("hex"),
        ];
      } else if (field === "mapping")
        body.event_to_step[0] = [
          body.event_to_step[0]![0],
          encodeEventToStepValueCbor({
            step_index: 1n,
            phase: "ForcedTransaction",
          }).toString("hex"),
        ];
      else {
        const descriptor = Data.from(
          body.validation_traces[0]![1],
          SDK.ValidationTraceDescriptor,
        );
        body.validation_traces[0] = [
          body.validation_traces[0]![0],
          Data.to(
            { ...descriptor, terminal_state_hash: "ab".repeat(32) },
            SDK.ValidationTraceDescriptor,
          ),
        ];
      }
      const forged = await rebind(payload);
      expect(forged.block_body.header.utxosRoot).toBe(
        payload.block_body.header.utxosRoot,
      );
      const result = await verdict(forged);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left")
        expect(result.left).toMatchObject({
          reason: "invalid",
          detail: expect.stringMatching(
            /foreign (transition|event-to-step|validation descriptor)/,
          ),
        });
    },
  );
  it("waits for missing reachable CEK material", async () => {
    const payload = await fixture({ withProgram: true });
    payload.block_body.cek_program_material = [];
    const result = await verdict(payload);
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "missing",
        detail: expect.stringContaining("material"),
      });
  });

  it.each([
    "utxosRoot",
    "withdrawalsRoot",
    "forcedTransactionsRoot",
    "transactionsRoot",
    "depositsRoot",
    "transitionTraceRoot",
    "eventToStepRoot",
    "validationTracesRoot",
  ] as const)("refuses changed header root %s", async (field) => {
    const payload = await fixture();
    payload.block_body.header[field] = "ab".repeat(32);
    payload.block_body.header_hash = await Effect.runPromise(
      SDK.hashBlockHeader(payload.block_body.header),
    );
    expect((await verdict(payload))._tag).toBe("Left");
  });
  it.each([
    "withdrawalCount",
    "forcedTransactionCount",
    "l2TransactionCount",
    "depositCount",
    "totalEventCount",
    "transitionStepCount",
    "validationTraceCount",
  ] as const)("refuses changed header count %s", async (field) => {
    const payload = await fixture();
    payload.block_body.header[field] += 1n;
    payload.block_body.header_hash = await Effect.runPromise(
      SDK.hashBlockHeader(payload.block_body.header),
    );
    expect((await verdict(payload))._tag).toBe("Left");
  });
  it("waits for missing DA rather than declaring a matching ledger ready", async () => {
    const payload = await fixture();
    const result = await verdict(payload, { payload: undefined });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({ reason: "missing" });
  });
  it("refuses a stale parent binding after rollback even with unchanged ledger roots", async () => {
    const result = await verdict(await fixture(), {
      parentHeaderHash: "bb".repeat(28),
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "invalid",
        detail: expect.stringContaining("verified parent"),
      });
  });
});

describe("foreign block import failure attribution", () => {
  it("passes a caller callback's verdict through unchanged", async () => {
    const verdictOfCaller = new Error("the caller's own verdict");
    const result = await verdict(await fixture(), {
      verifyForcedSource: () => Effect.fail(verdictOfCaller),
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") expect(result.left).toBe(verdictOfCaller);
  });

  it("calls a transaction preimage that does not decode invalid (the program sidecar decodes it first)", async () => {
    const payload = await fixture();
    const [key] = payload.block_body.forced_transaction_preimages[0]!;
    payload.block_body.forced_transaction_preimages[0] = [key, "00"];
    const result = await verdict(await rebind(payload));
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({ reason: "invalid" });
  });

  it("calls DA entries whose roots cannot be formed invalid", async () => {
    const payload = await fixture();
    payload.block_body.utxos = [["00", "00"]];
    const result = await verdict(payload);
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({ reason: "invalid" });
  });

  it("calls a failure it cannot pin on the block incomplete, never invalid", async () => {
    // A parent ledger the local MPF cannot hash: a local computation fails.
    const result = await verdict(await fixture(), {
      parentEntries: [{ outref: Buffer.alloc(0), output: Buffer.alloc(0) }],
    });
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "incomplete",
        detail: expect.stringContaining("MpfError"),
      });
  });
});
