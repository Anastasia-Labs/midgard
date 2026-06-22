import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyUTxOStatePatch,
  LedgerColumns,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import type { PhaseBResultWithPatch } from "../src/phase-b.js";
import type { RejectCode, RejectedTx } from "../src/types.js";
import {
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

const phaseBConfig = {
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
};

const runPhaseB = (
  candidates: Parameters<typeof runPhaseBValidationWithPatch>[0],
  preState: Parameters<typeof runPhaseBValidationWithPatch>[1],
  config = phaseBConfig,
) =>
  Effect.runPromise(runPhaseBValidationWithPatch(candidates, preState, config));

const txIds = (txs: PhaseBResultWithPatch["accepted"]) =>
  txs.map((tx) => tx.ledgerTx.txId.toString("hex"));

const preState = (
  entries: readonly (readonly [outRef: Buffer, output: Buffer])[],
) =>
  new Map(entries.map(([outRef, output]) => [outRef.toString("hex"), output]));

const expectSinglePhaseBRejection = (
  result: PhaseBResultWithPatch,
  expectedCode: RejectCode,
): RejectedTx => {
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0].code).toBe(expectedCode);
  return result.rejected[0];
};

describe("phase B validation", () => {
  it("accepts a balanced candidate and returns a deterministic UTxO patch", async () => {
    const spent = outRefFromByte(0x21);
    const inputOutput = makeOutput(10n);
    const candidate = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: 10n,
    });
    const state = preState([[spent, inputOutput]]);

    const result = await runPhaseB([candidate], state);

    expect(result.rejected).toHaveLength(0);
    expect(txIds(result.accepted)).toEqual([
      candidate.ledgerTx.txId.toString("hex"),
    ]);
    expect(result.statePatch.deletedOutRefs).toEqual([spent.toString("hex")]);
    expect(result.statePatch.upsertedOutRefs).toHaveLength(1);

    applyUTxOStatePatch(state, result.statePatch);
    const producedOutRef =
      candidate.graph.produced[0][LedgerColumns.OUTREF].toString("hex");
    expect(state.has(spent.toString("hex"))).toBe(false);
    expect(state.get(producedOutRef)?.equals(makeOutput(10n))).toBe(true);
  });

  it("rejects missing spend inputs before value accounting", async () => {
    const candidate = makePhaseBCandidate({
      spent: [outRefFromByte(0x22)],
      outputLovelace: 10n,
    });

    const result = await runPhaseB([candidate], new Map());

    expectSinglePhaseBRejection(result, RejectCodes.InputNotFound);
  });

  it("rejects missing pubkey witnesses for resolved spend inputs", async () => {
    const spent = outRefFromByte(0x23);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      omitVkeyWitness: true,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.MissingRequiredWitness);
  });

  it("rejects malformed pre-state outputs that cannot be decoded", async () => {
    const spent = outRefFromByte(0x24);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, Buffer.from("ff", "hex")]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.InvalidOutput);
  });

  it("rejects transactions that do not preserve value", async () => {
    const spent = outRefFromByte(0x25);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 9n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.ValueNotPreserved);
  });

  it("rejects candidates outside the current Cardano slot interval", async () => {
    const spent = outRefFromByte(0x26);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      validityIntervalStart: 101n,
      validityIntervalEnd: 110n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.ValidityIntervalMismatch,
    );
    expect(rejection.detail).toBe("100 < 101");
  });

  it("rejects later candidates that double-spend an accepted input", async () => {
    const spent = outRefFromByte(0x27);
    const reference = outRefFromByte(0x28);
    const first = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: 10n,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [spent],
      referenceInputs: [reference],
      outputLovelace: 10n,
    });

    const result = await runPhaseB(
      [first, second],
      preState([
        [spent, makeOutput(10n)],
        [reference, makeOutput(1n)],
      ]),
    );

    expect(txIds(result.accepted)).toEqual([
      first.ledgerTx.txId.toString("hex"),
    ]);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].txId.equals(second.ledgerTx.txId)).toBe(true);
    expect(result.rejected[0].code).toBe(RejectCodes.DoubleSpend);
  });

  it("cascade-rejects descendants when an ancestor fails validation", async () => {
    const parentInput = outRefFromByte(0x29);
    const parent = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [parentInput],
      outputLovelace: 9n,
    });
    const parentOutputRef = parent.graph.produced[0][LedgerColumns.OUTREF];
    const child = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [parentOutputRef],
      outputLovelace: 9n,
    });

    const result = await runPhaseB(
      [parent, child],
      preState([[parentInput, makeOutput(10n)]]),
    );

    expect(result.rejected.map((rejection) => rejection.code)).toEqual([
      RejectCodes.ValueNotPreserved,
      RejectCodes.DependsOnRejectedTx,
    ]);
    expect(result.accepted).toHaveLength(0);
    expect(result.rejected[1].detail).toContain(
      parent.ledgerTx.txId.toString("hex"),
    );
  });

  it("rejects defensive dependency cycles in the Phase A candidate graph", async () => {
    const first = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [outRefFromByte(0x2c)],
      outputLovelace: 10n,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [first.graph.produced[0][LedgerColumns.OUTREF]],
      outputLovelace: 10n,
    });
    const firstDependsOnSecond = {
      ...first,
      graph: {
        ...first.graph,
        spentOutRefHexes: [
          second.graph.produced[0][LedgerColumns.OUTREF].toString("hex"),
        ],
      },
    };

    const result = await runPhaseB([firstDependsOnSecond, second], new Map());

    expect(result.rejected.map((rejection) => rejection.code)).toEqual([
      RejectCodes.DependencyCycle,
      RejectCodes.DependencyCycle,
    ]);
    expect(result.accepted).toHaveLength(0);
  });

  it("rejects extraneous redeemers with no matching script purpose", async () => {
    const spent = outRefFromByte(0x2a);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      scriptLanguages: ["PlutusV3"],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Mint, index: 0n },
      ]),
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.detail).toContain("extraneous redeemer");
  });

  it("rejects PlutusV3 receiving scripts because receive requires MidgardV1 context", async () => {
    const spent = outRefFromByte(0x2b);
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputs: [makeProtectedScriptOutput(scriptHash, 10n)],
      outputLovelace: 10n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Receiving, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.PlutusScriptInvalid,
    );
    expect(rejection.detail).toContain(
      "ReceivingScript requires MidgardV1 context",
    );
  });
});
