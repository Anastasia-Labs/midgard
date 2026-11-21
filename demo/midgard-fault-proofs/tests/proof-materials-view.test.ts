/**
 * The process's one view of the proof-materials address: what it finds by
 * content, what it keeps from a sweep, and when it lets one sweep at all.
 */
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
  PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING,
  type ProofMaterialsHolder,
  type ProofMaterialsReader,
  proofMaterialsViewFor,
} from "../src/workflow/proof-materials-view.js";

const ADDRESS =
  "addr_test1wz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz";
const OTHER_ADDRESS =
  "addr_test1vz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerspjrlsz";
const CHUNK = "5820" + "a1".repeat(32);
const FIELD = "5820" + "b2".repeat(32);
const ABSENT = "5820" + "c3".repeat(32);
const POLICY = "cc".repeat(28);

const output = (
  txHashByte: string,
  outputIndex: number,
  datum: string | undefined,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n },
  datum,
  datumHash: undefined,
  scriptRef: undefined,
  ...extra,
});

const label = (utxo: UTxO | undefined) =>
  utxo === undefined
    ? undefined
    : `${utxo.txHash.slice(0, 2)}#${utxo.outputIndex}`;

/** A Lucid whose only read is the address's outputs, counted. */
const lucidOver = (utxos: () => readonly UTxO[]) => {
  const reads = { count: 0 };
  const lucid = {
    utxosAt: async (address: string) => {
      reads.count += 1;
      return utxos().filter((utxo) => utxo.address === address);
    },
  } as unknown as LucidEvolution;
  return { lucid, reads };
};

/** The planted copies of CHUNK no lookup and no sweep may take. */
const PLANTED = [
  output("01", 0, CHUNK, {
    assets: { lovelace: 2_000_000n, [`${POLICY}01`]: 1n },
  }),
  output("02", 0, CHUNK, {
    scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051200120011" },
  }),
  output("03", 0, undefined, { datumHash: "a1".repeat(32) }),
];

const OBJECTIVE = { category: "proof_chunk", headerHash: "aa".repeat(32) };

const holder = (
  workflowId: string,
  objective = OBJECTIVE,
): ProofMaterialsHolder => ({ workflowId, objective });

/** A workflow that only looks: its needs are released after each find. */
const READER = holder("reader");

const viewOver = (
  utxos: () => readonly UTxO[],
  reader?: (base: ProofMaterialsReader) => ProofMaterialsReader,
  liveObjectives?: () =>
    | readonly { category: string; headerHash: string }[]
    | undefined,
) => {
  const { lucid, reads } = lucidOver(utxos);
  const base = lucidProofMaterialsReader(lucid, ADDRESS);
  return {
    view: createProofMaterialsView({
      address: ADDRESS,
      reader: reader === undefined ? base : reader(base),
      ...(liveObjectives === undefined ? {} : { liveObjectives }),
    }),
    reads,
  };
};

describe("proof-materials view", () => {
  it("finds only exact, plainly shaped copies, in out-ref order and one per occurrence", async () => {
    const chain = [
      ...PLANTED,
      output("09", 1, CHUNK),
      output("05", 2, CHUNK),
      output("07", 0, FIELD),
      // The same value in another encoding is other bytes.
      output("04", 0, "9f" + CHUNK + "ff"),
      output("06", 0, CHUNK, { address: OTHER_ADDRESS }),
    ];
    const { view } = viewOver(() => chain);
    expect(
      (
        await view.find(
          [CHUNK, FIELD, CHUNK.toUpperCase(), CHUNK, ABSENT],
          READER,
        )
      ).map(label),
    ).toEqual(["05#2", "07#0", "09#1", undefined, undefined]);
    expect(await view.find([], READER)).toEqual([]);
  });

  it("sweeps only plain outputs no live need or pin names, and a shared datum only once its last workflow releases it", async () => {
    const chain = [
      ...PLANTED,
      output("05", 0, CHUNK),
      output("06", 0, FIELD),
      output("07", 0, ABSENT),
      // Plain ada a third party sent: nobody needs it.
      output("08", 0, undefined),
    ];
    const { view } = viewOver(() => chain);
    view.need(holder("first"), [CHUNK, FIELD]);
    view.need(holder("second"), [CHUNK], [`${"07".repeat(32)}#0`]);
    expect((await view.sweepable(100)).map(label)).toEqual(["08#0"]);
    view.release("first");
    expect((await view.sweepable(100)).map(label)).toEqual(["06#0", "08#0"]);
    view.release("second");
    expect((await view.sweepable(100)).map(label)).toEqual([
      "05#0",
      "06#0",
      "07#0",
      "08#0",
    ]);
    expect((await view.sweepable(2)).map(label)).toEqual(["05#0", "06#0"]);
  });

  it("keeps what a workflow found out of a sweep until that workflow is released", async () => {
    const chain = [output("05", 0, CHUNK), output("06", 0, FIELD)];
    const { view } = viewOver(() => chain);
    const finder = holder("finder");
    expect((await view.find([CHUNK], finder)).map(label)).toEqual(["05#0"]);
    expect((await view.sweepable(100)).map(label)).toEqual(["06#0"]);
    // A datum found absent is still needed: a copy that lands later is kept.
    await view.find([ABSENT], finder);
    chain.push(output("07", 0, ABSENT));
    expect((await view.sweepable(100)).map(label)).toEqual(["06#0"]);
    view.release(finder.workflowId);
    expect((await view.sweepable(100)).map(label)).toEqual([
      "05#0",
      "06#0",
      "07#0",
    ]);
  });

  it("releases every workflow of an objective that left, and only those", async () => {
    const chain = [output("05", 0, CHUNK), output("06", 0, FIELD)];
    const { view } = viewOver(() => chain);
    const other = { category: "field_carriage", headerHash: "bb".repeat(32) };
    view.need(holder("step"), [CHUNK]);
    view.need(holder("publish"), [], [`${"06".repeat(32)}#0`]);
    view.need(holder("elsewhere", other), [FIELD]);
    expect(await view.sweepable(100)).toEqual([]);
    view.releaseObjective({ ...OBJECTIVE, headerHash: "AA".repeat(32) });
    expect((await view.sweepable(100)).map(label)).toEqual(["05#0"]);
    view.releaseObjective(other);
    expect((await view.sweepable(100)).map(label)).toEqual(["05#0", "06#0"]);
  });

  it("refuses a need without a workflow, or of one workflow for two objectives", async () => {
    const { view } = viewOver(() => []);
    expect(() => view.need(holder(""), [CHUNK])).toThrow(/names its workflow/u);
    await expect(view.find([CHUNK], holder(""))).rejects.toThrow(
      /names its workflow/u,
    );
    view.need(holder("step"), [CHUNK]);
    expect(() =>
      view.need(
        holder("step", {
          category: "proof_chunk",
          headerHash: "dd".repeat(32),
        }),
        [FIELD],
      ),
    ).toThrow(/another objective/u);
  });

  it("sweeps nothing until every live objective has observed once", async () => {
    let live: { category: string; headerHash: string }[] | undefined;
    const { view } = viewOver(
      () => [output("05", 0, CHUNK)],
      undefined,
      () => live,
    );
    expect(await view.sweepable(100)).toEqual([]);
    expect(view.readiness()).toEqual({
      open: false,
      reason: PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING,
    });
    live = [
      { category: "proof_chunk", headerHash: "aa".repeat(32) },
      { category: "field_carriage", headerHash: "bb".repeat(32) },
    ];
    view.observed({ category: "proof_chunk", headerHash: "AA".repeat(32) });
    expect(await view.sweepable(100)).toEqual([]);
    expect(view.readiness()).toEqual({
      open: false,
      reason: `field_carriage/${"bb".repeat(32)} has not observed since this process started`,
    });
    view.observed({ category: "field_carriage", headerHash: "bb".repeat(32) });
    expect(view.readiness()).toEqual({ open: true });
    expect((await view.sweepable(100)).map(label)).toEqual(["05#0"]);
    // A new objective holds sweeps again until it has observed.
    live = [...live, { category: "proof_chunk", headerHash: "cc".repeat(32) }];
    expect(await view.sweepable(100)).toEqual([]);
    expect(view.readiness()).toEqual({
      open: false,
      reason: `proof_chunk/${"cc".repeat(32)} has not observed since this process started`,
    });
  });

  it("reads once per follower point and request", async () => {
    let point = 1;
    const chain = [output("05", 0, CHUNK)];
    const { view, reads } = viewOver(
      () => chain,
      (base) => ({
        ...base,
        point: async () => point.toString(),
        byDatum: async (requests) => ({
          ...(await base.byDatum(requests)),
          point: point.toString(),
        }),
      }),
    );
    await view.find([CHUNK], READER);
    await view.find([CHUNK], READER);
    expect(reads.count).toBe(1);
    await view.find([CHUNK, CHUNK], READER);
    expect(reads.count).toBe(2);
    point = 2;
    await view.find([CHUNK], READER);
    expect(reads.count).toBe(3);
  });

  it("without a point reads every time", async () => {
    const { view, reads } = viewOver(() => [output("05", 0, CHUNK)]);
    await view.find([CHUNK], READER);
    await view.find([CHUNK], READER);
    expect(reads.count).toBe(2);
  });

  it("shares a default view per Lucid and address, and refuses a view of another address", async () => {
    const { lucid } = lucidOver(() => []);
    const first = proofMaterialsViewFor({
      view: undefined,
      lucid,
      address: ADDRESS,
    });
    expect(
      proofMaterialsViewFor({ view: undefined, lucid, address: ADDRESS }),
    ).toBe(first);
    expect(
      proofMaterialsViewFor({
        view: undefined,
        lucid: lucidOver(() => []).lucid,
        address: ADDRESS,
      }),
    ).not.toBe(first);
    expect(() =>
      proofMaterialsViewFor({ view: first, lucid, address: OTHER_ADDRESS }),
    ).toThrow(/reads/u);
  });
});
