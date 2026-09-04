import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import {
  Data,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
} from "../src/native-script-decoding/scan-plan.js";
import {
  bindOutputReferenceScriptDecodingReferenceScripts,
  createOutputReferenceScriptDecodingWorkflowRunnerSurface,
  OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
  OutputReferenceOutputControlSchema,
  outputReferenceScriptControlData,
  type OutputReferenceScriptDecodingDeploymentBinding,
  outputReferenceScriptDecodingNextOutputScanStage,
  outputReferenceScriptDecodingNextStructuralStage,
  type OutputReferenceScriptDecodingReferenceScripts,
  outputReferenceScriptDecodingStageFromL1,
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
  runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflow,
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
const references = (): OutputReferenceScriptDecodingReferenceScripts => ({
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
    const runner = createOutputReferenceScriptDecodingWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("not reached");
      },
    });
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
    expect(
      Object.values(OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS),
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
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
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
    } as unknown as OutputReferenceScriptDecodingDeploymentBinding;
    expect(
      bindOutputReferenceScriptDecodingReferenceScripts({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
    expect(() =>
      bindOutputReferenceScriptDecodingReferenceScripts({
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
    const transactionId = computeMidgardNativeTxId(transaction).toString("hex");
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: forcedVerdictSubject({
        transactionId,
        sourceKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
        rejectionReason: {
          OutputReferenceScriptMalformed: { output_index: 0n },
        },
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(transaction),
    });
    const controlCbor = (index: number) =>
      Data.to(
        outputReferenceScriptControlData(
          evidence.outputScanControls[index]!,
        ) as never,
        OutputReferenceOutputControlSchema as never,
      );
    expect(evidence.outputScanControls.length).toBeGreaterThan(2);
    expect(
      outputReferenceScriptDecodingNextOutputScanStage({
        evidence,
        controlCbor: controlCbor(0),
      }),
    ).toBe("outputScan");
    expect(
      outputReferenceScriptDecodingNextOutputScanStage({
        evidence,
        controlCbor: controlCbor(evidence.outputScanControls.length - 2),
      }),
    ).toBe("referenceBind");
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: Buffer.from(evidence.referenceScriptItemHex, "hex"),
      direction: 1,
    });
    expect(plan.route).toBe(NativeScriptDecodingPlanRoutes.Machine);
    if (plan.route !== NativeScriptDecodingPlanRoutes.Machine)
      throw new Error("native fixture did not enter structural machine");
    expect(plan.segments.length).toBeGreaterThan(1);
    expect(
      outputReferenceScriptDecodingNextStructuralStage({
        evidence,
        controlCbor: plan.segments[0]!.controlBefore.cborHex,
        resultClass: BigInt(OutputReferenceScriptResultClasses.Pending),
      }),
    ).toBe("scan");
    expect(
      outputReferenceScriptDecodingNextStructuralStage({
        evidence,
        controlCbor: plan.segments.at(-1)!.controlBefore.cborHex,
        resultClass: BigInt(OutputReferenceScriptResultClasses.Pending),
      }),
    ).toBe("step06");
  });

  it("rejects a caller-authored evidence surface before any transport access", async () => {
    await expect(
      runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflow({
        workflow: {},
        sources: [],
        journal: {},
        evidence: {},
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence inputs/u);
  });
});
