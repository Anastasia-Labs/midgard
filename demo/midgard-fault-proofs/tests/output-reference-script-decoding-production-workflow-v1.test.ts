import {
  computeMidgardNativeTxIdV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { forcedVerdictSubjectV1 } from "@al-ft/midgard-sdk";
import {
  Data,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlanV1,
  NativeScriptDecodingPlanRoutesV1,
} from "../src/native-script-decoding/scan-plan-v1.js";
import {
  bindOutputReferenceScriptDecodingReferenceScriptsV1,
  createOutputReferenceScriptDecodingProductionWorkflowRunnerSurfaceV1,
  OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS_V1,
  OutputReferenceOutputControlV1Schema,
  outputReferenceScriptControlDataV1,
  type OutputReferenceScriptDecodingDeploymentBindingV1,
  outputReferenceScriptDecodingNextOutputScanStageV1,
  outputReferenceScriptDecodingNextStructuralStageV1,
  type OutputReferenceScriptDecodingProductionReferenceScriptsV1,
  outputReferenceScriptDecodingStageFromL1,
  OutputReferenceScriptResultClassesV1,
  prepareOutputReferenceScriptDecodingEvidenceV1,
  runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflowV1,
} from "../src/output-reference-script-decoding/index.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});
const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0resolvedoutput",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});
const references =
  (): OutputReferenceScriptDecodingProductionReferenceScriptsV1 => ({
    step01: utxo("1", 0),
    step02: utxo("2", 1),
    step03: utxo("3", 2),
    step04: utxo("4", 3),
    step05: utxo("5", 4),
    step06: utxo("6", 5),
    fieldPreimageCertificateMint: utxo("7", 6),
    witnesses: {
      computationThreadMint: utxo("8", 7),
      fraudProofMint: utxo("9", 8),
      phasMembershipWithdraw: utxo("a", 9),
    },
  });

describe("outputReferenceScriptDecoding production workflow", () => {
  it("exposes the standard callback-free runner and complete manifest roles", () => {
    const runner =
      createOutputReferenceScriptDecodingProductionWorkflowRunnerSurfaceV1({
        loadRuntimeConfig: async () => {
          throw new Error("not reached");
        },
      });
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
    expect(
      Object.values(OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS_V1),
    ).toEqual([
      "fraudProofOutputReferenceScriptDecoding",
      "fraudProofOutputReferenceScriptDecodingStep02",
      "fraudProofOutputReferenceScriptDecodingStep03",
      "fraudProofOutputReferenceScriptDecodingStep04",
      "fraudProofOutputReferenceScriptDecodingStep05",
      "fraudProofOutputReferenceScriptDecodingStep06",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "fieldPreimageCertificateMint",
    ]);
  });

  it("authenticates every reference out-ref and refuses substitution", () => {
    const supplied = references();
    const names = Object.values(
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS_V1,
    );
    const values = [
      supplied.step01,
      supplied.step02,
      supplied.step03,
      supplied.step04,
      supplied.step05,
      supplied.step06,
      supplied.witnesses.computationThreadMint,
      supplied.witnesses.fraudProofMint,
      supplied.witnesses.phasMembershipWithdraw,
      supplied.fieldPreimageCertificateMint,
    ];
    const binding = {
      referenceScriptsByContract: Object.fromEntries(
        names.map((name, index) => [
          name,
          {
            outRef: `${values[index]!.txHash}#${values[index]!.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(values[index]!.scriptRef!),
          },
        ]),
      ),
    } as unknown as OutputReferenceScriptDecodingDeploymentBindingV1;
    expect(
      bindOutputReferenceScriptDecodingReferenceScriptsV1({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
    expect(() =>
      bindOutputReferenceScriptDecodingReferenceScriptsV1({
        binding,
        referenceScripts: {
          ...supplied,
          step06: { ...supplied.step06, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
  });

  it("maps all six authenticated raw-L1 physical steps", () => {
    expect(
      [1, 2, 3, 4, 5, 6].map((step) =>
        outputReferenceScriptDecodingStageFromL1({
          kind: "step",
          step,
        } as never),
      ),
    ).toEqual([
      "step01",
      "step02",
      "outputScan",
      "referenceBind",
      "scan",
      "step06",
    ]);
    expect(() =>
      outputReferenceScriptDecodingStageFromL1({
        kind: "step",
        step: 7,
      } as never),
    ).toThrow(/exceeds six-step topology/u);
  });

  it("derives both descriptor and native self-loop exits from authenticated controls", () => {
    const nativeScript = {
      type: "all" as const,
      scripts: Array.from({ length: 128 }, (_unused, index) => ({
        type: "sig" as const,
        keyHash: Buffer.alloc(28, index),
      })),
    };
    const transaction = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbor: encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
        value: { lovelace: 2_000_000n, assets: new Map() },
        script_ref: {
          language: "NativeCardano",
          scriptBytes: Buffer.alloc(0),
          nativeScript,
        },
      }),
    });
    const transactionId =
      computeMidgardNativeTxIdV1(transaction).toString("hex");
    const evidence = prepareOutputReferenceScriptDecodingEvidenceV1({
      subject: forcedVerdictSubjectV1({
        transactionId,
        sourceKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
        rejectionReason: {
          OutputReferenceScriptMalformed: { output_index: 0n },
        },
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(transaction),
    });
    const controlCbor = (index: number) =>
      Data.to(
        outputReferenceScriptControlDataV1(
          evidence.outputScanControls[index]!,
        ) as never,
        OutputReferenceOutputControlV1Schema as never,
      );
    expect(evidence.outputScanControls.length).toBeGreaterThan(2);
    expect(
      outputReferenceScriptDecodingNextOutputScanStageV1({
        evidence,
        controlCbor: controlCbor(0),
      }),
    ).toBe("outputScan");
    expect(
      outputReferenceScriptDecodingNextOutputScanStageV1({
        evidence,
        controlCbor: controlCbor(evidence.outputScanControls.length - 2),
      }),
    ).toBe("referenceBind");
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: Buffer.from(evidence.referenceScriptItemHex, "hex"),
      direction: 1,
    });
    expect(plan.route).toBe(NativeScriptDecodingPlanRoutesV1.Machine);
    if (plan.route !== NativeScriptDecodingPlanRoutesV1.Machine)
      throw new Error("native fixture did not enter structural machine");
    expect(plan.segments.length).toBeGreaterThan(1);
    expect(
      outputReferenceScriptDecodingNextStructuralStageV1({
        evidence,
        controlCbor: plan.segments[0]!.controlBefore.cborHex,
        resultClass: BigInt(OutputReferenceScriptResultClassesV1.Pending),
      }),
    ).toBe("scan");
    expect(
      outputReferenceScriptDecodingNextStructuralStageV1({
        evidence,
        controlCbor: plan.segments.at(-1)!.controlBefore.cborHex,
        resultClass: BigInt(OutputReferenceScriptResultClassesV1.Pending),
      }),
    ).toBe("step06");
  });

  it("rejects a caller-authored evidence surface before any transport access", async () => {
    await expect(
      runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflowV1({
        workflow: {},
        sources: [],
        journal: {},
        evidence: {},
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence inputs/u);
  });
});
