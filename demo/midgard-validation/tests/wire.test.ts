import { describe, expect, it } from "vitest";

import {
  deserializePhaseACandidate,
  serializePhaseACandidate,
} from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  makePhaseBCandidate,
  makeRedeemersCbor,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

describe("Phase A worker wire codec", () => {
  it("round-trips every Phase B field across structured clone", () => {
    const candidate = makePhaseBCandidate({
      scriptWitnesses: [plutusV3ScriptWitness(Buffer.from("010203", "hex"))],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Mint, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });

    const clonedWire = structuredClone(serializePhaseACandidate(candidate));
    const restored = deserializePhaseACandidate(clonedWire);
    const expected = {
      ...candidate,
      ledgerTx: { ...candidate.ledgerTx, vkeyWitnesses: [] },
    };

    expect(restored).toStrictEqual(expected);
    expect(Buffer.isBuffer(restored.ledgerTx.txId)).toBe(true);
    expect(Buffer.isBuffer(restored.ledgerTx.outputs[0].address)).toBe(true);
    expect(Buffer.isBuffer(restored.graph.produced[0].outref)).toBe(true);
    expect(Buffer.isBuffer(restored.ledgerTx.redeemers[0].dataCbor)).toBe(true);
    expect(restored.ledgerTx.txId.equals(candidate.ledgerTx.txId)).toBe(true);
    expect(restored.ledgerTx.txId.toString("hex")).toBe(
      candidate.ledgerTx.txId.toString("hex"),
    );
  });

  it("carries redeemer Data bytes exactly, map order and duplicates included", () => {
    // {h'ff': 1, h'00': 2, h'ff': 3}: out of key order, with a duplicate key.
    const data = Buffer.from("a341ff014100 0241ff03".replace(/ /g, ""), "hex");
    const candidate = makePhaseBCandidate({
      scriptWitnesses: [plutusV3ScriptWitness(Buffer.from("010203", "hex"))],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Mint, index: 0n, data },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    expect(candidate.ledgerTx.redeemers[0].dataCbor).toStrictEqual(data);

    const restored = deserializePhaseACandidate(
      structuredClone(serializePhaseACandidate(candidate)),
    );

    expect(restored.ledgerTx.redeemers[0].dataCbor).toStrictEqual(data);
  });

  it("never exposes oversized Buffer backing stores to structured clone", () => {
    const candidate = makePhaseBCandidate();
    const pooledBacking = Buffer.allocUnsafe(8_192);
    const slicedTxId = pooledBacking.subarray(97, 129);
    const wire = serializePhaseACandidate({
      ...candidate,
      ledgerTx: { ...candidate.ledgerTx, txId: slicedTxId },
    });

    const binaryViews: Uint8Array[] = [];
    const visit = (value: unknown): void => {
      if (value instanceof Uint8Array) {
        binaryViews.push(value);
        return;
      }
      if (Array.isArray(value)) {
        value.forEach(visit);
        return;
      }
      if (value instanceof Map) {
        for (const [key, item] of value) {
          visit(key);
          visit(item);
        }
        return;
      }
      if (value !== null && typeof value === "object") {
        Object.values(value).forEach(visit);
      }
    };
    visit(wire);

    expect(binaryViews.length).toBeGreaterThan(0);
    for (const bytes of binaryViews) {
      expect(bytes.byteOffset).toBe(0);
      expect(bytes.buffer.byteLength).toBe(bytes.byteLength);
    }
    expect(wire.ledgerTx.txId.buffer.byteLength).toBe(32);
  });
});
