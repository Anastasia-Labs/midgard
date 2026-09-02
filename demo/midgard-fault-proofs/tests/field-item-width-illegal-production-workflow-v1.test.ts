import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  decodeMidgardFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubjectV1 } from "@al-ft/midgard-sdk";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  fieldItemWidthEvidenceIdentityV1,
  prepareFieldItemWidthEvidenceV1,
} from "../src/field-item-width-illegal/field-item-width-illegal-v1.js";
import {
  bindFieldItemWidthIllegalReferenceScriptsV1,
  createFieldItemWidthIllegalProductionWorkflowRunnerSurfaceV1,
  createFieldItemWidthIllegalRawL1StageResolverV1,
  createManifestBoundFieldItemWidthIllegalProductionRuntimeV1,
  deriveFieldItemWidthIllegalAuthenticatedSourceV1,
  deriveFieldItemWidthIllegalEvidenceFromCanonicalBlockV1,
  detectFieldItemWidthIllegalCompleteReplayV1,
  FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS_V1,
  FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1,
  type FieldItemWidthIllegalProductionReferenceScriptsV1,
  type ManifestBoundFieldItemWidthIllegalConfigV1,
  runOrResumeManifestBoundFieldItemWidthIllegalWorkflowV1,
} from "../src/field-item-width-illegal/production-workflow-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  transactionSourceTrieItemV1,
} from "../src/prepare-double-spend.js";
import type { FraudProofWorkflowDeploymentBindingV1 } from "../src/workflow/deployment-manifest-binding-v1.js";
import { committedFieldShapeScenarioMaterialV1 } from "./support/committed-field-shape-emulator-v1.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});

const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0productionbinding",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});

const references = (): FieldItemWidthIllegalProductionReferenceScriptsV1 => ({
  step01: utxo("1", 0),
  step02: utxo("2", 1),
  step03: utxo("3", 2),
  fieldPreimageCertificateMint: utxo("4", 3),
  witnesses: {
    computationThreadMint: utxo("5", 4),
    fraudProofMint: utxo("6", 5),
    phasMembershipWithdraw: utxo("7", 6),
  },
});

const bindingFor = (
  value: FieldItemWidthIllegalProductionReferenceScriptsV1,
): FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal"> => {
  const names = FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS_V1;
  const entries = [
    [names.step01, value.step01],
    [names.step02, value.step02],
    [names.step03, value.step03],
    [names.fieldPreimageCertificateMint, value.fieldPreimageCertificateMint],
    [names.computationThreadMint, value.witnesses.computationThreadMint],
    [names.fraudProofMint, value.witnesses.fraudProofMint],
    [names.phasMembershipWithdraw, value.witnesses.phasMembershipWithdraw],
  ] as const;
  return {
    referenceScriptsByContract: Object.fromEntries(
      entries.map(([name, reference]) => [
        name,
        {
          outRef: `${reference.txHash}#${reference.outputIndex.toString()}`,
          scriptHash: validatorToScriptHash(reference.scriptRef!),
        },
      ]),
    ),
  } as unknown as FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal">;
};

const evidence = (() => {
  const preimage = encodeMidgardFieldPreimageV1([Buffer.alloc(16_385, 7)]);
  return prepareFieldItemWidthEvidenceV1({
    finding: {
      subject: acceptedVerdictSubjectV1("8".repeat(64)),
      fieldIndex: 2,
      itemIndex: 0,
    },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitmentV1(preimage).toString("hex"),
  });
})();

describe("fieldItemWidthIllegal manifest-bound production workflow", () => {
  it("exposes the standard manifest/public-DA runner surface", () => {
    const runner = createFieldItemWidthIllegalProductionWorkflowRunnerSurfaceV1(
      {
        loadRuntimeConfig: async () => {
          throw new Error("not reached");
        },
      },
    );
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
  });

  it("names and binds every family/shared reference role", () => {
    expect(
      Object.values(FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS_V1),
    ).toEqual([
      "fraudProofFieldItemWidthIllegal",
      "fraudProofFieldItemWidthIllegalStep02",
      "fraudProofFieldItemWidthIllegalStep03",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "fieldPreimageCertificateMint",
    ]);
    const supplied = references();
    expect(
      bindFieldItemWidthIllegalReferenceScriptsV1({
        binding: bindingFor(supplied),
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
  });

  it("rejects deployment out-ref and script substitutions", () => {
    const supplied = references();
    const binding = bindingFor(supplied);
    expect(() =>
      bindFieldItemWidthIllegalReferenceScriptsV1({
        binding,
        referenceScripts: {
          ...supplied,
          step02: { ...supplied.step02, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
    expect(() =>
      bindFieldItemWidthIllegalReferenceScriptsV1({
        binding,
        referenceScripts: {
          ...supplied,
          step03: { ...supplied.step03, scriptRef: script("a") },
        },
      }),
    ).toThrow(/script differs from finalized manifest identity/u);
  });

  it("constructs the finding only from canonical retained-DA transaction bytes", () => {
    const material = committedFieldShapeScenarioMaterialV1(
      "field-item-width-illegal",
    );
    if (material.fullTx === null) throw new Error("missing canonical fixture");
    const derived = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlockV1({
      transactions: [
        {
          txCbor: encodeMidgardNativeTxCanonicalV1(material.fullTx).toString(
            "hex",
          ),
        },
      ],
      reconstruction: { forcedTransactions: [] },
    } as never);
    expect(derived.subject.source_kind).toBe(0n);
    expect(derived.fieldIndex).toBe(2);
    expect(derived.decisiveFaultHolds).toBe(true);
  });

  it("derives accepted inclusion and compact source only from authenticated retained DA", async () => {
    const fixture = committedFieldShapeScenarioMaterialV1(
      "field-item-width-illegal",
    );
    if (fixture.fullTx === null) throw new Error("missing canonical fixture");
    const txCbor = encodeMidgardNativeTxCanonicalV1(fixture.fullTx).toString(
      "hex",
    );
    const nodeTxId = computeMidgardNativeTxIdV1(fixture.fullTx).toString("hex");
    const transaction = await decodeTransactionMaterial({ nodeTxId, txCbor });
    const trie = await buildTrieView([
      transactionSourceTrieItemV1(transaction),
    ]);
    const exactEvidence = prepareFieldItemWidthEvidenceV1({
      finding: {
        subject: acceptedVerdictSubjectV1(nodeTxId),
        fieldIndex: 2,
        itemIndex: 0,
      },
      fieldPreimage: fixture.fullTx.body.outputsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitmentV1(
        fixture.fullTx.body.outputsPreimageCbor,
      ).toString("hex"),
    });
    const block = {
      transactions: [
        {
          nodeTxId,
          txCbor,
          l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        },
      ],
      reconstruction: {
        rootData: { transactions: { phasRoot: trie.root } },
      },
      inclusionRootAuthentication: { sourceValuePhasRoot: trie.root },
    };
    const source = await deriveFieldItemWidthIllegalAuthenticatedSourceV1({
      block: block as never,
      evidence: exactEvidence,
    });
    expect(source.acceptedInclusion?.nativeTxId).toBe(nodeTxId);
    expect(source.nativeTxCompactCbor).toBe(transaction.nativeCompactCbor);
    await expect(
      deriveFieldItemWidthIllegalAuthenticatedSourceV1({
        block: {
          ...block,
          inclusionRootAuthentication: {
            sourceValuePhasRoot: "d".repeat(64),
          },
        } as never,
        evidence: exactEvidence,
      }),
    ).rejects.toThrow(/differs from authenticated reconstruction/u);
  });

  it("resolves the step out-ref from raw L1 without an installation callback", async () => {
    let stage: unknown = {
      kind: "step",
      step: 2,
      threadOutRef: `${"e".repeat(64)}#1`,
      stateQueueBlockOutRef: `${"f".repeat(64)}#0`,
    };
    const resolver = createFieldItemWidthIllegalRawL1StageResolverV1({
      config: {
        binding: { definition: { headerHash: "1".repeat(56) } },
      } as never,
      l1: { observe: async () => ({ stage }) } as never,
      source: {
        nativeTxCompactCbor: "aa",
        witnessSetCompactCbor: "bb",
      },
    });
    await expect(
      resolver({ action: "submitStep02", evidence }),
    ).resolves.toEqual(
      expect.objectContaining({
        threadOutRef: `${"e".repeat(64)}#1`,
        nativeTxCompactCbor: "aa",
        witnessSetCompactCbor: "bb",
      }),
    );
    stage = { ...(stage as object), step: 3 };
    await expect(
      resolver({ action: "submitStep02", evidence }),
    ).rejects.toThrow(/differs from authenticated raw-L1 stage/u);
  });

  it("complete replay scans every output and mint coordinate", () => {
    const material = committedFieldShapeScenarioMaterialV1(
      "field-item-width-illegal",
    );
    if (material.fullTx === null) throw new Error("missing canonical fixture");
    const output = decodeMidgardFieldPreimageV1(material.committedPreimage)[0]!;
    const fullTx = materializeMidgardNativeTxFromCanonicalV1({
      ...material.fullTx,
      body: {
        ...material.fullTx.body,
        outputsPreimageCbor: encodeMidgardFieldPreimageV1([output, output]),
        mintPreimageCbor: encodeMidgardFieldPreimageV1([Buffer.alloc(0)]),
      },
    });
    const detections = detectFieldItemWidthIllegalCompleteReplayV1({
      headerHash: "a".repeat(56),
      transactions: [
        {
          nodeTxId: computeMidgardNativeTxIdV1(fullTx).toString("hex"),
          txCbor: encodeMidgardNativeTxCanonicalV1(fullTx).toString("hex"),
        },
      ],
      reconstruction: { forcedTransactions: [] },
    } as never);
    expect(detections.map(({ detectionId }) => detectionId)).toEqual([
      expect.stringContaining(":2:0:"),
      expect.stringContaining(":2:1:"),
      expect.stringContaining(":5:0:0"),
    ]);
  });

  it("complete replay detects only false authenticated forced width reasons", async () => {
    const forced = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: "b".repeat(56),
      now: 1_900_000_000_000,
      fieldItemWidthIllegalCoordinate: { fieldIndex: 2, itemIndex: 0 },
    });
    const wrongful = detectFieldItemWidthIllegalCompleteReplayV1({
      headerHash: forced.reconstruction.headerHash,
      transactions: [],
      reconstruction: forced.reconstruction,
    } as never);
    expect(wrongful).toHaveLength(1);
    expect(wrongful[0]?.detectionId).toContain(":forced:0:");

    const illegal = committedFieldShapeScenarioMaterialV1(
      "field-item-width-illegal",
    ).fullTx!;
    const illegalCbor = encodeMidgardNativeTxCanonicalV1(illegal);
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      encodeMidgardNativeTxCanonicalV1(
        adjudicateMidgardNativeTxFullV1Validity(illegal, "TxIsInvalid"),
      ),
    );
    const original = forced.reconstruction.forcedTransactions[0]!;
    const honestEntry = {
      ...original,
      fullTransactionCbor: illegalCbor,
      value: {
        ...original.value,
        tx_id: material.transactionId.toString("hex"),
        source: {
          compact_cbor: material.proofSource.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            material.proofSource.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
        },
      },
    };
    const honestBlock = {
      headerHash: forced.reconstruction.headerHash,
      transactions: [],
      reconstruction: {
        ...forced.reconstruction,
        forcedTransactions: [honestEntry],
      },
    };
    expect(
      detectFieldItemWidthIllegalCompleteReplayV1(honestBlock as never),
    ).toEqual([]);
    expect(() =>
      detectFieldItemWidthIllegalCompleteReplayV1({
        ...honestBlock,
        reconstruction: {
          ...honestBlock.reconstruction,
          forcedTransactions: [
            {
              ...honestEntry,
              value: {
                ...honestEntry.value,
                source: {
                  ...honestEntry.value.source,
                  compact_cbor: `00${honestEntry.value.source.compact_cbor.slice(2)}`,
                },
              },
            },
          ],
        },
      } as never),
    ).toThrow(/differs from its authenticated leaf/u);
  });

  it("rejects a caller-authored evidence substitution before reading sources", async () => {
    await expect(
      runOrResumeManifestBoundFieldItemWidthIllegalWorkflowV1({
        workflow: {} as never,
        sources: [],
        journal: {} as never,
        evidence,
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence/u);
  });

  it("resumes from authenticated terminal removal without replaying a builder", async () => {
    const identity = fieldItemWidthEvidenceIdentityV1(evidence);
    const resolveStage = vi.fn();
    const append = vi.fn();
    const runtime = createManifestBoundFieldItemWidthIllegalProductionRuntimeV1(
      {
        config: {} as ManifestBoundFieldItemWidthIllegalConfigV1,
        journal: {
          load: async () => [
            {
              sequence: 0,
              identity,
              stage: "step01",
              txHash: "9".repeat(64),
              outputReference: `${"9".repeat(64)}#0`,
            },
          ],
          append,
        },
        observe: async () => "removed",
        resolveStage,
      },
    );
    await expect(runtime.runOrResume(evidence)).resolves.toBe("removed");
    expect(runtime.runtimeVersion).toBe(
      FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1,
    );
    expect(resolveStage).not.toHaveBeenCalled();
    expect(append).not.toHaveBeenCalled();
  });
});
