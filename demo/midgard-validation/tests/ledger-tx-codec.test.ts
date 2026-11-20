import {
  computeHash32,
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardLedgerTxFromCanonicalCbor,
  encodeMidgardLedgerTxToCanonicalCbor,
} from "../src/ledger-tx.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import { plutusDataToCborHex } from "../src/plutus-data.js";
import {
  encodeRecomputedNativeTx,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

const hexes = (values: readonly Buffer[]): string[] =>
  values.map((value) => value.toString("hex"));

describe("Midgard ledger transaction codec", () => {
  it("round-trips canonical native transaction CBOR through the semantic ledger type", () => {
    const spent = outRefFromByte(0x31, 2n);
    const referenceInput = outRefFromByte(0x32, 3n);
    const output = makeOutput(7n);
    const fixture = makeNativeTx({
      spendInputs: [spent],
      referenceInputs: [referenceInput],
      outputs: [output],
      fee: 2n,
      validityIntervalStart: 10n,
      validityIntervalEnd: 20n,
      networkId: 0n,
    });

    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);
    const encoded = encodeMidgardLedgerTxToCanonicalCbor(ledgerTx);

    expect(encoded.equals(fixture.txCbor)).toBe(true);
    expect(ledgerTx.txId.equals(fixture.txId)).toBe(true);
    expect(ledgerTx.spendInputs).toEqual([
      { txId: Buffer.alloc(32, 0x31), index: 2n },
    ]);
    expect(ledgerTx.referenceInputs).toEqual([
      { txId: Buffer.alloc(32, 0x32), index: 3n },
    ]);
    expect(ledgerTx.outputs).toHaveLength(1);
    expect(ledgerTx.outputs[0].value.lovelace).toBe(7n);
    expect(ledgerTx.fee).toBe(2n);
    expect(ledgerTx.validityIntervalStart).toBe(10n);
    expect(ledgerTx.validityIntervalEnd).toBe(20n);
    expect(ledgerTx.networkId).toBe(0n);
  });

  it("maps native sentinel values to absent semantic optional fields", () => {
    const fixture = makeNativeTx();

    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    expect(ledgerTx.networkId).toBeUndefined();
    expect(ledgerTx.validityIntervalStart).toBeUndefined();
    expect(ledgerTx.validityIntervalEnd).toBeUndefined();
    expect(ledgerTx.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)).toBe(true);
    expect(
      encodeMidgardLedgerTxToCanonicalCbor(ledgerTx).equals(fixture.txCbor),
    ).toBe(true);
  });

  it("round-trips vkey witnesses without storing witness CBOR in the semantic type", () => {
    const fixture = makeNativeTx({ invalidVkeyWitness: true });

    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    expect(ledgerTx.vkeyWitnesses).toHaveLength(1);
    expect(ledgerTx.witnessKeyHashes).toEqual([
      ledgerTx.vkeyWitnesses[0].keyHash,
    ]);
    expect(
      encodeMidgardLedgerTxToCanonicalCbor(ledgerTx).equals(fixture.txCbor),
    ).toBe(true);
  });

  it("decodes and canonicalizes signed mint entries", () => {
    const policyId = Buffer.alloc(28, 0xa1);
    const mintedAsset = Buffer.from("alpha");
    const burnedAsset = Buffer.from("beta");
    const mintPreimageCbor = encodeCbor(
      new Map([
        [
          policyId,
          new Map<Buffer, bigint>([
            [mintedAsset, 5n],
            [burnedAsset, -2n],
          ]),
        ],
      ]),
    );
    const base = makeNativeTx();
    const fixture = encodeRecomputedNativeTx({
      ...base.tx,
      body: {
        ...base.tx.body,
        mintPreimageCbor,
      },
    });

    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    const decodedAssets = ledgerTx.mint.assets.map((asset) => ({
      policyId: asset.policyId.toString("hex"),
      assetName: asset.assetName.toString("hex"),
      quantity: asset.quantity,
    }));
    expect(decodedAssets).toHaveLength(2);
    expect(decodedAssets).toEqual(
      expect.arrayContaining([
        {
          policyId: policyId.toString("hex"),
          assetName: mintedAsset.toString("hex"),
          quantity: 5n,
        },
        {
          policyId: policyId.toString("hex"),
          assetName: burnedAsset.toString("hex"),
          quantity: -2n,
        },
      ]),
    );
    expect(
      encodeMidgardLedgerTxToCanonicalCbor(ledgerTx).equals(fixture.txCbor),
    ).toBe(true);
  });

  it("round-trips script witnesses and decoded redeemers", () => {
    const script = plutusV3ScriptWitness(Buffer.from("4d010000332222", "hex"));
    const redeemerData = Buffer.from(
      plutusDataToCborHex(new Constr(0, [1n]), { canonical: true }),
      "hex",
    );
    const redeemerTxWitsPreimageCbor = makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Mint,
        index: 0n,
        data: redeemerData,
        exUnits: [7n, 11n],
      },
    ]);
    const fixture = makeNativeTx({
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor,
      scriptLanguages: ["PlutusV3"],
    });

    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    expect(hexes(ledgerTx.plutusScriptHashes)).toEqual([
      hashScriptWitness(script),
    ]);
    expect(ledgerTx.redeemers).toHaveLength(1);
    expect(ledgerTx.redeemers[0].tag).toBe(MidgardRedeemerTag.Mint);
    expect(ledgerTx.redeemers[0].index).toBe(0n);
    expect(ledgerTx.redeemers[0].exUnits).toEqual({
      memory: 7n,
      steps: 11n,
    });
    expect(ledgerTx.requiresPlutusEvaluation).toBe(true);
    expect(
      encodeMidgardLedgerTxToCanonicalCbor(ledgerTx).equals(fixture.txCbor),
    ).toBe(true);
  });

  it("rejects semantic transactions whose tx id no longer matches the body", () => {
    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(
      makeNativeTx().txCbor,
    );

    expect(() =>
      encodeMidgardLedgerTxToCanonicalCbor({
        ...ledgerTx,
        txId: Buffer.alloc(32, 0xff),
      }),
    ).toThrow(/txId mismatch/);
  });

  it("rejects stale script witness hash metadata", () => {
    const script = plutusV3ScriptWitness(Buffer.from("4d010000332222", "hex"));
    const fixture = makeNativeTx({
      scriptWitnesses: [script],
      scriptLanguages: ["PlutusV3"],
    });
    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    expect(() =>
      encodeMidgardLedgerTxToCanonicalCbor({
        ...ledgerTx,
        scriptWitnesses: [
          {
            ...ledgerTx.scriptWitnesses[0],
            hash: Buffer.alloc(28, 0xee),
          },
        ],
      }),
    ).toThrow(/scriptWitnesses\.hash mismatch/);
  });

  it("rejects duplicate redeemer pointers during serialization", () => {
    const script = plutusV3ScriptWitness(Buffer.from("4d010000332222", "hex"));
    const redeemerTxWitsPreimageCbor = makeRedeemersCbor([
      { tag: MidgardRedeemerTag.Mint, index: 0n },
    ]);
    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(
      makeNativeTx({
        scriptWitnesses: [script],
        redeemerTxWitsPreimageCbor,
        scriptLanguages: ["PlutusV3"],
      }).txCbor,
    );

    expect(() =>
      encodeMidgardLedgerTxToCanonicalCbor({
        ...ledgerTx,
        redeemers: [ledgerTx.redeemers[0], ledgerTx.redeemers[0]],
      }),
    ).toThrow(/duplicate redeemer pointer/);
  });

  it("rejects inconsistent requiresPlutusEvaluation metadata", () => {
    const redeemerTxWitsPreimageCbor = makeRedeemersCbor([
      { tag: MidgardRedeemerTag.Mint, index: 0n },
    ]);
    const scriptIntegrityHash = computeScriptIntegrityHashForLanguages(
      computeHash32(redeemerTxWitsPreimageCbor),
      ["PlutusV3"],
    );
    const base = makeNativeTx();
    const fixture = encodeRecomputedNativeTx({
      ...base.tx,
      body: {
        ...base.tx.body,
        scriptIntegrityHash,
      },
      witnessSet: {
        ...base.tx.witnessSet,
        redeemerTxWitsPreimageCbor,
      },
    });
    const ledgerTx = decodeMidgardLedgerTxFromCanonicalCbor(fixture.txCbor);

    expect(() =>
      encodeMidgardLedgerTxToCanonicalCbor({
        ...ledgerTx,
        requiresPlutusEvaluation: false,
      }),
    ).toThrow(/requiresPlutusEvaluation mismatch/);
  });
});
