import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  LedgerColumns,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import type { PhaseBResultWithPatch } from "../src/phase-b.js";
import type { PhaseBConfig } from "../src/types.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromByte,
} from "./validation-fixtures.js";

const phaseBConfig: PhaseBConfig = {
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
};

const txIdHex = (tx: PhaseBResultWithPatch["accepted"][number]) =>
  tx.ledgerTx.txId.toString("hex");

/**
 * Applies `accepted` one transaction at a time from `preState`, as a block
 * that commits it in order is replayed, and returns every spent or
 * reference input that is absent at its own transaction's position.
 */
const inputsAbsentOnSequentialReplay = (
  accepted: PhaseBResultWithPatch["accepted"],
  preState: ReadonlyMap<string, Buffer>,
): readonly string[] => {
  const state = new Map(preState);
  const absent: string[] = [];
  for (const tx of accepted) {
    for (const outRefHex of [
      ...tx.graph.spentOutRefHexes,
      ...tx.graph.referenceOutRefHexes,
    ]) {
      if (!state.has(outRefHex)) absent.push(`${txIdHex(tx)}:${outRefHex}`);
    }
    for (const outRefHex of tx.graph.spentOutRefHexes) state.delete(outRefHex);
    for (const produced of tx.graph.produced) {
      state.set(
        produced[LedgerColumns.OUTREF].toString("hex"),
        Buffer.from(produced[LedgerColumns.OUTPUT]),
      );
    }
  }
  return absent;
};

describe("phase B application order", () => {
  it("validates a round's ready candidates in candidate order, so its accepted order replays", async () => {
    // P1 spends a and P2 spends b. S (index 2) spends P2's output and X; R
    // (index 3) spends P1's output and references X. Both become ready in
    // the same round, P1's child before P2's. In candidate order S spends X
    // first, so R's reference is absent at R's position.
    const a = outRefFromByte(0x70);
    const b = outRefFromByte(0x71);
    const x = outRefFromByte(0x72);
    const p1 = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [a],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const p2 = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [b],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const s = makePhaseBCandidate({
      arrivalSeq: 2n,
      spent: [p2.graph.produced[0][LedgerColumns.OUTREF], x],
      outputLovelace: FUNDED_OUTPUT_LOVELACE * 2n,
    });
    const r = makePhaseBCandidate({
      arrivalSeq: 3n,
      spent: [p1.graph.produced[0][LedgerColumns.OUTREF]],
      referenceInputs: [x],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const preState = new Map(
      [a, b, x].map((outRef) => [
        outRef.toString("hex"),
        makeOutput(FUNDED_OUTPUT_LOVELACE),
      ]),
    );

    const result = await Effect.runPromise(
      runPhaseBValidationWithPatch([p1, p2, s, r], preState, phaseBConfig),
    );

    expect(result.accepted.map(txIdHex)).toStrictEqual(
      [p1, p2, s].map(txIdHex),
    );
    expect(
      result.rejected.map((rejection) => [
        rejection.txId.toString("hex"),
        rejection.code,
      ]),
    ).toStrictEqual([[txIdHex(r), RejectCodes.InputNotFound]]);
    expect(
      inputsAbsentOnSequentialReplay(result.accepted, preState),
    ).toStrictEqual([]);
  });
});
