import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  castStateQueueNodeToData,
  decodeHeaderCbor,
  decodeStateQueueNodeCbor,
  decodeTransitionStepCbor,
  EMPTY_MERKLE_TREE_ROOT,
  encodeHeaderCbor,
  encodeStateQueueNodeCbor,
  encodeTransitionStepCbor,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  hashTransitionStep,
  type Header,
  HeaderTransitionCommitments,
  makeHeaderTransitionCommitmentsProgram,
  TransitionStepV1,
  validateHeaderTransitionCommitmentsProgram,
} from "../src/index.js";

const h32 = (byte: number): string => Buffer.alloc(32, byte).toString("hex");
const h28 = (byte: number): string => Buffer.alloc(28, byte).toString("hex");

const header = (): Header => ({
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

const HEADER_CBOR =
  "d8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ff";
const HEADER_HASH = "68e507eaad2278934d696204c01ffa64ca7381e989823e5aed19afbc";
const HEADER_TRANSITION_COMMITMENTS_CBOR =
  "d8799f58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101ff";
const STATE_QUEUE_NODE_CBOR =
  "d8799fd8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ffd87a9f5820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaffff";
const TRANSITION_STEP_CBOR =
  "d8799f0102d87b9f58203333333333333333333333333333333333333333333333333333333333333333ffd87b805820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ff";
const TRANSITION_STEP_HASH =
  "c7931b31b050e3d59769ee6df1556585c608f1af0c74decc08026ecafbc234c4";

describe("HeaderV1", () => {
  it("has the exact L01 TypeScript CBOR and blake2b-224 vector", async () => {
    const value = header();

    expect(encodeHeaderCbor(value).toString("hex")).toBe(HEADER_CBOR);
    expect(decodeHeaderCbor(Buffer.from(HEADER_CBOR, "hex"))).toEqual(value);
    await expect(Effect.runPromise(hashBlockHeader(value))).resolves.toBe(
      HEADER_HASH,
    );
    expect(() => encodeHeaderCbor({ ...value, protocolVersion: 2n })).toThrow(
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
    const commitmentInput = {
      ...commitments,
      withdrawalsRoot: value.withdrawalsRoot,
      transactionsRoot: value.transactionsRoot,
      depositsRoot: value.depositsRoot,
    };

    expect(Data.to(commitments, HeaderTransitionCommitments)).toBe(
      HEADER_TRANSITION_COMMITMENTS_CBOR,
    );
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsProgram(commitmentInput),
      ),
    ).resolves.toEqual(commitments);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsProgram({
          ...commitmentInput,
          forcedTransactionCount: 1n,
        }),
      ),
    ).rejects.toThrow(/source root/u);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsProgram({
          ...commitmentInput,
          transitionStepCount: 0n,
        }),
      ),
    ).rejects.toThrow(/transition_step_count/u);
    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsProgram({
          ...commitmentInput,
          withdrawalCount: 10_001n,
        }),
      ),
    ).rejects.toThrow(/compiled consensus bound/u);

    const nonEmptyCommitments = {
      ...commitments,
      withdrawalCount: 1n,
      l2TransactionCount: 1n,
      depositCount: 1n,
      totalEventCount: 3n,
      transitionStepCount: 3n,
    };
    const nonEmptyCommitmentInput = {
      ...nonEmptyCommitments,
      withdrawalsRoot: h32(9),
      transactionsRoot: h32(3),
      depositsRoot: h32(10),
    };

    await expect(
      Effect.runPromise(
        validateHeaderTransitionCommitmentsProgram(nonEmptyCommitmentInput),
      ),
    ).resolves.toEqual(nonEmptyCommitments);

    for (const [label, rootField] of [
      ["withdrawals", "withdrawalsRoot"],
      ["transactions", "transactionsRoot"],
      ["deposits", "depositsRoot"],
    ] as const) {
      const invalid = await Effect.runPromise(
        Effect.either(
          validateHeaderTransitionCommitmentsProgram({
            ...nonEmptyCommitmentInput,
            [rootField]: EMPTY_MERKLE_TREE_ROOT,
          }),
        ),
      );
      expect(invalid._tag).toBe("Left");
      if (invalid._tag === "Left") {
        expect(String(invalid.left.cause)).toContain(`${label}_root`);
      }
    }
  });

  it("has the exact L03 StateQueueNodeV1 topology and datum vector", () => {
    const node = {
      header: header(),
      da_attestation: {
        Attested: { da_bond_asset_name: "aa".repeat(32) },
      },
    } as const;

    expect(encodeStateQueueNodeCbor(node).toString("hex")).toBe(
      STATE_QUEUE_NODE_CBOR,
    );
    expect(
      decodeStateQueueNodeCbor(Buffer.from(STATE_QUEUE_NODE_CBOR, "hex")),
    ).toEqual(node);
    expect(() =>
      encodeStateQueueNodeCbor({
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

    expect(encodeTransitionStepCbor(step).toString("hex")).toBe(
      TRANSITION_STEP_CBOR,
    );
    expect(
      decodeTransitionStepCbor(Buffer.from(TRANSITION_STEP_CBOR, "hex")),
    ).toEqual(step);
    await expect(Effect.runPromise(hashTransitionStep(step))).resolves.toBe(
      TRANSITION_STEP_HASH,
    );
    expect(() =>
      encodeTransitionStepCbor({ ...step, schema_version: 0n }),
    ).toThrow(/schema_version/u);
    expect(() =>
      decodeTransitionStepCbor(
        Buffer.from(
          Data.to({ ...step, schema_version: 2n }, TransitionStepV1),
          "hex",
        ),
      ),
    ).toThrow(/schema_version/u);
    expect(() =>
      decodeTransitionStepCbor(
        Buffer.concat([Buffer.from(TRANSITION_STEP_CBOR, "hex"), Buffer.of(0)]),
      ),
    ).toThrow();
  });

  it("requires one validation descriptor for every normal or forced transaction", async () => {
    const commitments = await Effect.runPromise(
      makeHeaderTransitionCommitmentsProgram({
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
        makeHeaderTransitionCommitmentsProgram({
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
      data: castStateQueueNodeToData({
        header: value,
        da_attestation: "Unattested",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderFromStateQueueDatum(datum)),
    ).resolves.toEqual(value);

    const wrongVersionDatum = {
      data: castStateQueueNodeToData({
        header: { ...value, protocolVersion: 2n },
        da_attestation: "Unattested",
      }) as Parameters<typeof Data.castFrom>[0],
    };
    await expect(
      Effect.runPromise(getHeaderFromStateQueueDatum(wrongVersionDatum)),
    ).rejects.toThrow(/StateQueueNodeV1/u);
  });
});
