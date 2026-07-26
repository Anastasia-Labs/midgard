import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardLedgerOutputProofV1,
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputProofTraceV1,
  commitMidgardLedgerOutputReferenceScriptItemV1,
  decodeMidgardDatum,
  digestMidgardLedgerOutputReferenceScriptV1,
  encodeMidgardLedgerOutputProofControlV1,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
  MidgardLedgerOutputProofResultKindsV1,
  MidgardLedgerOutputProofStagesV1,
  type MidgardNativeScript,
  type MidgardTxOutput,
  summarizeMidgardLedgerOutputCardanoSpendDatumV1,
} from "../src/index.js";

const baseOutput = (
  scriptRef: MidgardTxOutput["script_ref"],
): MidgardTxOutput => ({
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
          ["ff", 7n],
          ["0000", 42n],
        ]),
      ],
    ]),
  },
  datum: decodeMidgardDatum(
    Buffer.from(Data.to("ab".repeat(5_000)), "hex"),
  ),
  ...(scriptRef === undefined ? {} : { script_ref: scriptRef }),
});

describe("bounded ledger output proof V1", () => {
  it("authenticates and hashes a real multi-chunk reference script", () => {
    const script = {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(6_000, 0x6b),
    } as const;
    const outputCbor = encodeMidgardTxOutput(baseOutput(script));
    const trace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor,
    });

    expect(outputCbor.length).toBeGreaterThan(8_192);
    expect(trace.terminal.stage).toBe(
      MidgardLedgerOutputProofStagesV1.Terminal,
    );
    expect(
      digestMidgardLedgerOutputReferenceScriptV1(
        trace.terminal,
      )!.toString("hex"),
    ).toBe(hashMidgardVersionedScript(script));
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(
        trace.terminal,
      )!.root,
    ).toHaveLength(32);
    expect(trace.terminal.datum!.stage).toBe(7);
    expect(
      commitMidgardLedgerOutputReferenceScriptItemV1(
        trace.terminal,
      ),
    ).toStrictEqual(
      buildMidgardBoundedItemV1({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX_V1,
        itemIndex: 0,
        bytes: encodeMidgardVersionedScript(script),
      }).commitment,
    );
    expect(trace.terminal.referenceScriptFrontier.count).toBe(2);
    expect(
      trace.steps.filter(
        ({ control }) =>
          control.stage ===
          MidgardLedgerOutputProofStagesV1.ScriptHash,
      ).length,
    ).toBeGreaterThan(600);
    expect(
      trace.steps.some(
        ({ witness }) =>
          witness?.kind === "chunks" &&
          witness.nextChunkProof !== null,
      ),
    ).toBe(true);
  });

  it("authenticates native syntax without evaluating the reference script", () => {
    const nativeScript: MidgardNativeScript = {
      type: "all",
      scripts: Array.from({ length: 40 }, (_, index) => ({
        type: "sig",
        keyHash: Buffer.alloc(28, index),
      })),
    };
    const script = {
      language: "NativeCardano",
      scriptBytes: encodeMidgardNativeScript(nativeScript),
      nativeScript,
    } as const;
    const trace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 7,
      outputCbor: encodeMidgardTxOutput(baseOutput(script)),
    });

    expect(trace.terminal.nativeScript!.nodeCount).toBe(41);
    expect(
      digestMidgardLedgerOutputReferenceScriptV1(
        trace.terminal,
      )!.toString("hex"),
    ).toBe(hashMidgardVersionedScript(script));
  });

  it("authenticates the datum and terminates without a reference script", () => {
    const output = baseOutput(undefined);
    const trace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor: encodeMidgardTxOutput(output),
    });
    expect(trace.terminal.scriptHash).toBeNull();
    expect(trace.terminal.nativeScript).toBeNull();
    expect(
      digestMidgardLedgerOutputReferenceScriptV1(trace.terminal),
    ).toBeNull();
    expect(
      commitMidgardLedgerOutputReferenceScriptItemV1(
        trace.terminal,
      ),
    ).toBeNull();
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(
        trace.terminal,
      )!.root,
    ).toHaveLength(32);
  });

  it("terminates directly when datum and reference script are absent", () => {
    const { datum: _datum, ...output } = baseOutput(undefined);
    const trace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor: encodeMidgardTxOutput(output),
    });

    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(
        trace.terminal,
      )!.root,
    ).toHaveLength(32);
    expect(
      commitMidgardLedgerOutputReferenceScriptItemV1(
        trace.terminal,
      ),
    ).toBeNull();
  });

  it("fails closed for substituted chunks and malformed native syntax", () => {
    const validNative = {
      language: "NativeCardano",
      scriptBytes: encodeMidgardNativeScript({
        type: "sig",
        keyHash: Buffer.alloc(28, 0x44),
      }),
      nativeScript: {
        type: "sig",
        keyHash: Buffer.alloc(28, 0x44),
      },
    } as const;
    const malformedOutput = Buffer.from(
      encodeMidgardTxOutput(baseOutput(validNative)),
    );
    const scriptOffset = malformedOutput.indexOf(validNative.scriptBytes);
    if (scriptOffset < 0) throw new Error("native script not found");
    malformedOutput[scriptOffset] = 0x81;
    expect(() =>
      buildMidgardLedgerOutputProofTraceV1({
        outputIndex: 0,
        outputCbor: malformedOutput,
      }),
    ).toThrow(/invalidReferenceScript/u);

    const validTrace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor: encodeMidgardTxOutput(
        baseOutput({
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(200, 0x6b),
        }),
      ),
    });
    const chunkStep = validTrace.steps.find(
      ({ witness }) => witness?.kind === "chunks",
    )!;
    const witness = chunkStep.witness!;
    if (witness.kind !== "chunks") throw new Error("missing chunk witness");
    expect(
      advanceMidgardLedgerOutputProofV1({
        control: chunkStep.control,
        witness: {
          ...witness,
          chunkProof: {
            ...witness.chunkProof,
            chunk: Buffer.alloc(witness.chunkProof.chunk.length),
          },
        },
      }),
    ).toBeNull();

    const datumCommitmentStep = validTrace.steps.find(
      ({ control, witness: stepWitness }) =>
        control.stage ===
          MidgardLedgerOutputProofStagesV1.DatumTraversal &&
        stepWitness?.kind === "datum" &&
        stepWitness.chunkProof !== null,
    )!;
    const datumWitness = datumCommitmentStep.witness!;
    if (
      datumWitness.kind !== "datum" ||
      datumWitness.chunkProof === null
    ) {
      throw new Error("missing datum commitment chunk witness");
    }
    expect(
      advanceMidgardLedgerOutputProofV1({
        control: datumCommitmentStep.control,
        witness: {
          ...datumWitness,
          chunkProof: {
            ...datumWitness.chunkProof,
            chunk: Buffer.alloc(datumWitness.chunkProof.chunk.length),
          },
        },
      }),
    ).toBeNull();

    const referenceCommitmentStep = validTrace.steps.find(
      ({ control, witness: stepWitness }) =>
        control.stage ===
          MidgardLedgerOutputProofStagesV1.ReferenceScriptCommitment &&
        stepWitness?.kind === "chunks",
    )!;
    const referenceWitness = referenceCommitmentStep.witness!;
    if (referenceWitness.kind !== "chunks") {
      throw new Error("missing reference commitment chunk witness");
    }
    expect(
      advanceMidgardLedgerOutputProofV1({
        control: referenceCommitmentStep.control,
        witness: {
          ...referenceWitness,
          chunkProof: {
            ...referenceWitness.chunkProof,
            chunk: Buffer.alloc(referenceWitness.chunkProof.chunk.length),
          },
        },
      }),
    ).toBeNull();
  });

  it("encodes the long terminal control canonically for Aiken", () => {
    const trace = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor: encodeMidgardTxOutput(
        baseOutput({
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(6_000, 0x6b),
        }),
      ),
    });
    const terminalCbor =
      encodeMidgardLedgerOutputProofControlV1(trace.terminal);
    expect(terminalCbor.toString("hex")).toBe(
      "8b010500192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770d8799f8a0107185419142719142740d87a80d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80",
    );
    expect(terminalCbor.length).toBeLessThan(512);
    expect(
      advanceMidgardLedgerOutputProofV1({
        control: trace.terminal,
        witness: null,
      }),
    ).toBeNull();
    expect(
      MidgardLedgerOutputProofResultKindsV1.Advanced,
    ).toBe("advanced");
  });
});
