import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "../src/codec/index.js";
import {
  advanceMidgardLedgerOutputScanV1,
  buildMidgardLedgerOutputAssetFrontierV1,
  buildMidgardLedgerOutputScanTraceV1,
  commitMidgardValidationMerkleFrontierV1,
  encodeMidgardLedgerOutputScanControlV1,
  initialMidgardLedgerOutputScanControlV1,
  MidgardLedgerOutputScanStagesV1,
} from "../src/index.js";

const outputFixture = (): {
  readonly output: MidgardTxOutput;
  readonly cbor: Buffer;
} => {
  const output: MidgardTxOutput = {
    address: Buffer.concat([
      Buffer.from([0x78]),
      Buffer.alloc(28, 0x11),
    ]),
    value: {
      lovelace: 8_000_000n,
      assets: new Map([
        [
          "55".repeat(28),
          new Map([
            ["0000", 42n],
            ["ff", 7n],
          ]),
        ],
      ]),
    },
    datum: decodeMidgardDatum(
      Buffer.from(Data.to("ab".repeat(5_000)), "hex"),
    ),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(6_000, 0x6b),
    },
  };
  return { output, cbor: encodeMidgardTxOutput(output) };
};

describe("ledger output scan V1", () => {
  it("streams a complete Cardano-sized output into bounded facts", () => {
    const fixture = outputFixture();
    const trace = buildMidgardLedgerOutputScanTraceV1(fixture.cbor);
    const terminal = trace.terminal;
    const expectedAssets = buildMidgardLedgerOutputAssetFrontierV1([
      {
        policyId: Buffer.alloc(28, 0x55),
        assetName: Buffer.from("ff", "hex"),
        quantity: 7n,
      },
      {
        policyId: Buffer.alloc(28, 0x55),
        assetName: Buffer.from("0000", "hex"),
        quantity: 42n,
      },
    ]);

    expect(fixture.cbor.length).toBeGreaterThan(8_192);
    expect(fixture.cbor.length).toBeLessThan(16_384);
    expect(terminal.stage).toBe(
      MidgardLedgerOutputScanStagesV1.Terminal,
    );
    expect(terminal.cursor).toBe(fixture.cbor.length);
    expect(terminal.address).toStrictEqual(fixture.output.address);
    expect(terminal.lovelace).toBe(8_000_000n);
    expect(terminal.assetFrontier.count).toBe(2);
    expect(
      commitMidgardValidationMerkleFrontierV1(
        terminal.assetFrontier,
      ),
    ).toStrictEqual(expectedAssets.commitment);
    expect(terminal.datumLength).toBe(fixture.output.datum!.cbor.length);
    expect(terminal.referenceScriptLanguage).toBe(3);
    expect(terminal.referenceScriptLength).toBe(6_000);
    expect(
      encodeMidgardLedgerOutputScanControlV1(terminal).toString("hex"),
    ).toBe(
      "950107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe18541914270003191481191770",
    );
    expect(
      trace.steps
        .filter(
          ({ control }) =>
            control.stage ===
              MidgardLedgerOutputScanStagesV1.DatumPayload ||
            control.stage ===
              MidgardLedgerOutputScanStagesV1.ReferenceScriptPayload,
        )
        .some(
          ({ next }) =>
            next.cursor > 0 && next.cursor % 4_095 === 0,
        ),
    ).toBe(true);
  });

  it("fails closed when otherwise-canonical bytes have a trailing suffix", () => {
    const fixture = outputFixture();
    expect(() =>
      buildMidgardLedgerOutputScanTraceV1(
        Buffer.concat([fixture.cbor, Buffer.from([0x00])]),
      ),
    ).toThrow(/failed closed|did not terminate/);
  });

  it("fails closed for non-minimal CBOR and malformed control state", () => {
    const fixture = outputFixture();
    expect(() =>
      buildMidgardLedgerOutputScanTraceV1(
        Buffer.concat([Buffer.from([0xb8, 0x04]), fixture.cbor.subarray(1)]),
      ),
    ).toThrow(/failed closed/);

    const malformed = {
      ...initialMidgardLedgerOutputScanControlV1(),
      assetFrontier: { count: 1, peaks: [] },
    };
    expect(
      advanceMidgardLedgerOutputScanV1({
        control: malformed,
        totalLength: fixture.cbor.length,
        window: fixture.cbor,
        windowOffset: 0,
      }),
    ).toBeNull();
  });
});
