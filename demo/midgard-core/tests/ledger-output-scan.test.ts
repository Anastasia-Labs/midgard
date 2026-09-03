import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  type MidgardTxOutput,
  midgardValueToCmlValue,
} from "../src/codec/index.js";
import {
  advanceMidgardLedgerOutputScan,
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardLedgerOutputScanTrace,
  commitMidgardValidationMerkleFrontier,
  encodeMidgardLedgerOutputScanControl,
  initialMidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
} from "../src/index.js";

const outputFixture = (): {
  readonly output: MidgardTxOutput;
  readonly cbor: Buffer;
} => {
  const output: MidgardTxOutput = {
    address: Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]),
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
    datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(5_000)), "hex")),
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
    const trace = buildMidgardLedgerOutputScanTrace(fixture.cbor);
    const terminal = trace.terminal;
    const expectedAssets = buildMidgardLedgerOutputAssetFrontier([
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
    expect(terminal.stage).toBe(MidgardLedgerOutputScanStages.Terminal);
    expect(terminal.cursor).toBe(fixture.cbor.length);
    expect(terminal.address).toStrictEqual(fixture.output.address);
    expect(terminal.lovelace).toBe(8_000_000n);
    expect(terminal.cardanoValueSize).toBe(
      midgardValueToCmlValue(fixture.output.value).to_cbor_bytes().length,
    );
    expect(terminal.assetFrontier.count).toBe(2);
    expect(
      commitMidgardValidationMerkleFrontier(terminal.assetFrontier),
    ).toStrictEqual(expectedAssets.commitment);
    expect(terminal.datumLength).toBe(fixture.output.datum!.cbor.length);
    expect(terminal.referenceScriptLanguage).toBe(3);
    expect(terminal.referenceScriptLength).toBe(6_000);
    expect(terminal.cursor - terminal.referenceScriptItemOffset).toBe(
      encodeMidgardVersionedScript(fixture.output.script_ref!).length,
    );
    expect(encodeMidgardLedgerOutputScanControl(terminal).toString("hex")).toBe(
      "970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770",
    );
    expect(
      trace.steps
        .filter(
          ({ control }) =>
            control.stage === MidgardLedgerOutputScanStages.DatumPayload ||
            control.stage ===
              MidgardLedgerOutputScanStages.ReferenceScriptPayload,
        )
        .some(({ next }) => next.cursor > 0 && next.cursor % 4_095 === 0),
    ).toBe(true);
  });

  it("fails closed when otherwise-canonical bytes have a trailing suffix", () => {
    const fixture = outputFixture();
    expect(() =>
      buildMidgardLedgerOutputScanTrace(
        Buffer.concat([fixture.cbor, Buffer.from([0x00])]),
      ),
    ).toThrow(/failed closed|did not terminate/);
  });

  it("fails closed for non-minimal CBOR and malformed control state", () => {
    const fixture = outputFixture();
    expect(() =>
      buildMidgardLedgerOutputScanTrace(
        Buffer.concat([Buffer.from([0xb8, 0x04]), fixture.cbor.subarray(1)]),
      ),
    ).toThrow(/failed closed/);

    const malformed = {
      ...initialMidgardLedgerOutputScanControl(),
      assetFrontier: { count: 1, peaks: [] },
    };
    expect(
      advanceMidgardLedgerOutputScan({
        control: malformed,
        totalLength: fixture.cbor.length,
        window: fixture.cbor,
        windowOffset: 0,
      }),
    ).toBeNull();
  });

  it("fails closed when an inline datum has no Plutus Data bytes", () => {
    const emptyInlineDatum = Buffer.from(
      `a300581d60${"11".repeat(28)}01821a004c4b40a00240`,
      "hex",
    );

    expect(() => buildMidgardLedgerOutputScanTrace(emptyInlineDatum)).toThrow(
      /canonical Plutus Data|failed closed/,
    );
  });
});
