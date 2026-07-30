import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  castStateQueueNodeV1ToData,
  decodeHeaderV1Cbor,
  decodeStateQueueNodeV1Cbor,
  decodeTransitionStepV1Cbor,
  EMPTY_MERKLE_TREE_ROOT,
  encodeHeaderV1Cbor,
  encodeStateQueueNodeV1Cbor,
  encodeTransitionStepV1Cbor,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  hashTransitionStepV1,
  HeaderTransitionCommitmentsV1,
  type HeaderV1,
  makeHeaderTransitionCommitmentsV1Program,
  TransitionStepV1,
  validateHeaderTransitionCommitmentsV1Program,
} from "../src/index.js";

const h32 = (byte: number): string => Buffer.alloc(32, byte).toString("hex");
const h28 = (byte: number): string => Buffer.alloc(28, byte).toString("hex");

const header = (): HeaderV1 => ({
  prevUtxosRoot: h32(1),
  utxosRoot: h32(2),
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: h32(3),
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: h32(4),
  eventToStepRoot: h32(5),
  validationTracesRoot: h32(8),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28(6),
  operatorVkey: h28(7),
  protocolVersion: 1n,
});

const HEADER_V1_CBOR =
  "d8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ff";
const HEADER_V1_HASH =
  "68e507eaad2278934d696204c01ffa64ca7381e989823e5aed19afbc";
const HEADER_TRANSITION_COMMITMENTS_V1_CBOR =
  "d8799f58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101ff";
const STATE_QUEUE_NODE_V1_CBOR =
  "d8799fd8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ff41aaff";
const TRANSITION_STEP_V1_CBOR =
  "d8799f0102d87b9f58203333333333333333333333333333333333333333333333333333333333333333ffd87b805820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ff";
const TRANSITION_STEP_V1_HASH =
  "c7931b31b050e3d59769ee6df1556585c608f1af0c74decc08026ecafbc234c4";

describe("HeaderV1", () => {
  it("has the exact L01 TypeScript CBOR and blake2b-224 vector", async () => {
    const value = header();

    expect(encodeHeaderV1Cbor(value).toString("hex")).toBe(HEADER_V1_CBOR);
    expect(decodeHeaderV1Cbor(Buffer.from(HEADER_V1_CBOR, "hex"))).toEqual(
      value,
    );
    await expect(Effect.runPromise(hashBlockHeaderV1(value))).resolves.toBe(
      HEADER_V1_HASH,
    );
    expect(() => encodeHeaderV1Cbor({ ...value, protocolVersion: 2n })).toThrow(
      /protocol version/u,
    );
  });

  it("has the complete exact L02 transition-commitments vector and semantics", async () => {
    const value = header();
    const commitments = {
      forcedTransactionsRoot: value.forcedTransactionsRoot,
      transitionTraceRoot: value.transitionTraceRoot,
      eventToStepRoot: value.eventToStepRoot,
      validationTracesRoot: value.validationTracesRoot,
      withdrawalCount: value.withdrawalCount,
      forcedTransactionCount: value.forcedTransactionCount,
      l2TransactionCount: value.l2TransactionCount,
      depositCount: value.depositCount,
      totalEventCount: value.totalEventCount,
      transitionStepCount: value.transitionStepCount,
      validationTraceCount: value.validationTraceCount,
    };

    expect(Data.to(commitments, HeaderTransitionCommitmentsV1)).toBe(
      HEADER_TRANSITION_COMMITMENTS_V1_CBOR,
    );
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsV1Program(commitments),
      ),
    ).resolves.toEqual(commitments);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsV1Program({
          ...commitments,
          forcedTransactionCount: 1n,
        }),
      ),
    ).rejects.toThrow(/source root/u);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsV1Program({
          ...commitments,
          transitionStepCount: 0n,
        }),
      ),
    ).rejects.toThrow(/transition_step_count/u);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsV1Program({
          ...commitments,
          withdrawalCount: 10_001n,
        }),
      ),
    ).rejects.toThrow(/compiled consensus bound/u);
  });

  it("has the exact L03 StateQueueNodeV1 topology and datum vector", () => {
    const node = { header: header(), da_attestation: "aa" };

    expect(encodeStateQueueNodeV1Cbor(node).toString("hex")).toBe(
      STATE_QUEUE_NODE_V1_CBOR,
    );
    expect(
      decodeStateQueueNodeV1Cbor(Buffer.from(STATE_QUEUE_NODE_V1_CBOR, "hex")),
    ).toEqual(node);
    expect(() =>
      encodeStateQueueNodeV1Cbor({
        ...node,
        header: { ...node.header, protocolVersion: 2n },
      }),
    ).toThrow(/protocol version/u);
  });

  it("has the exact L06 TransitionStepV1 CBOR/hash and rejects schema drift", async () => {
    const step: TransitionStepV1 = {
      schema_version: 1n,
      step_index: 2n,
      event_key: {
        L2TransactionEventKey: { tx_id: h32(0x33) },
      },
      phase: "L2Transaction",
      pre_utxos_root: h32(0x44),
      post_utxos_root: h32(0x55),
    };

    expect(encodeTransitionStepV1Cbor(step).toString("hex")).toBe(
      TRANSITION_STEP_V1_CBOR,
    );
    expect(
      decodeTransitionStepV1Cbor(Buffer.from(TRANSITION_STEP_V1_CBOR, "hex")),
    ).toEqual(step);
    await expect(Effect.runPromise(hashTransitionStepV1(step))).resolves.toBe(
      TRANSITION_STEP_V1_HASH,
    );
    expect(() =>
      encodeTransitionStepV1Cbor({ ...step, schema_version: 0n }),
    ).toThrow(/schema_version/u);
    expect(() =>
      decodeTransitionStepV1Cbor(
        Buffer.from(
          Data.to({ ...step, schema_version: 2n }, TransitionStepV1),
          "hex",
        ),
      ),
    ).toThrow(/schema_version/u);
    expect(() =>
      decodeTransitionStepV1Cbor(
        Buffer.concat([
          Buffer.from(TRANSITION_STEP_V1_CBOR, "hex"),
          Buffer.of(0),
        ]),
      ),
    ).toThrow();
  });

  it("requires one validation descriptor for every normal or forced transaction", async () => {
    const commitments = await Effect.runPromise(
      makeHeaderTransitionCommitmentsV1Program({
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: h32(3),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: h32(4),
        eventToStepRoot: h32(5),
        validationTracesRoot: h32(8),
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 1n,
        depositCount: 0n,
        validationTraceCount: 1n,
      }),
    );

    expect(commitments.validationTraceCount).toBe(1n);
    await expect(
      Effect.runPromise(
        makeHeaderTransitionCommitmentsV1Program({
          ...commitments,
          withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: h32(3),
          depositsRoot: EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).rejects.toThrow(/must equal/u);
  });

  it("round-trips only protocol version 1 state-queue nodes", async () => {
    const value = header();
    const datum = {
      data: castStateQueueNodeV1ToData({
        header: value,
        da_attestation: "",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderV1FromStateQueueDatum(datum)),
    ).resolves.toEqual(value);

    const wrongVersionDatum = {
      data: castStateQueueNodeV1ToData({
        header: { ...value, protocolVersion: 2n },
        da_attestation: "",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderV1FromStateQueueDatum(wrongVersionDatum)),
    ).rejects.toThrow(/StateQueueNodeV1/u);
  });
});
