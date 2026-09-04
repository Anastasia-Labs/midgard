import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  fieldItemWidthEvidenceIdentity,
  prepareFieldItemWidthEvidence,
} from "../src/field-item-width-illegal/field-item-width-illegal.js";
import {
  bindFieldItemWidthIllegalReferenceScripts,
  createFieldItemWidthIllegalRawL1StageResolver,
  createFieldItemWidthIllegalWorkflowRunnerSurface,
  createManifestBoundFieldItemWidthIllegalRuntime,
  deriveFieldItemWidthIllegalAuthenticatedSource,
  deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock,
  detectFieldItemWidthIllegalCompleteReplay,
  FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS,
  FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
  type FieldItemWidthIllegalReferenceScripts,
  type ManifestBoundFieldItemWidthIllegalConfig,
  runOrResumeManifestBoundFieldItemWidthIllegalWorkflow,
} from "../src/field-item-width-illegal/workflow.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  transactionSourceTrieItem,
} from "../src/prepare-double-spend.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";
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

const references = (): FieldItemWidthIllegalReferenceScripts => ({
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
  value: FieldItemWidthIllegalReferenceScripts,
): FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal"> => {
  const names = FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS;
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
  } as unknown as FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
};

const evidence = (() => {
  const preimage = encodeMidgardFieldPreimage([Buffer.alloc(16_385, 7)]);
  return prepareFieldItemWidthEvidence({
    finding: {
      subject: acceptedVerdictSubject("8".repeat(64)),
      fieldIndex: 2,
      itemIndex: 0,
    },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
  });
})();

describe("fieldItemWidthIllegal manifest-bound production workflow", () => {
  it("exposes the standard manifest/public-DA runner surface", () => {
    const runner = createFieldItemWidthIllegalWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("not reached");
      },
    });
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
  });

  it("names and binds every family/shared reference role", () => {
    expect(Object.values(FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS)).toEqual([
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
      bindFieldItemWidthIllegalReferenceScripts({
        binding: bindingFor(supplied),
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
  });

  it("rejects deployment out-ref and script substitutions", () => {
    const supplied = references();
    const binding = bindingFor(supplied);
    expect(() =>
      bindFieldItemWidthIllegalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step02: { ...supplied.step02, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
    expect(() =>
      bindFieldItemWidthIllegalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step03: { ...supplied.step03, scriptRef: script("a") },
        },
      }),
    ).toThrow(/script differs from finalized manifest identity/u);
  });

  it("constructs the finding only from canonical retained-DA transaction bytes", () => {
    const material = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    );
    if (material.fullTx === null) throw new Error("missing canonical fixture");
    const derived = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock({
      transactions: [
        {
          txCbor: encodeMidgardNativeTxCanonical(material.fullTx).toString(
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
    const fixture = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    );
    if (fixture.fullTx === null) throw new Error("missing canonical fixture");
    const txCbor = encodeMidgardNativeTxCanonical(fixture.fullTx).toString(
      "hex",
    );
    const nodeTxId = computeMidgardNativeTxId(fixture.fullTx).toString("hex");
    const transaction = await decodeTransactionMaterial({ nodeTxId, txCbor });
    const trie = await buildTrieView([transactionSourceTrieItem(transaction)]);
    const exactEvidence = prepareFieldItemWidthEvidence({
      finding: {
        subject: acceptedVerdictSubject(nodeTxId),
        fieldIndex: 2,
        itemIndex: 0,
      },
      fieldPreimage: fixture.fullTx.body.outputsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitment(
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
    const source = await deriveFieldItemWidthIllegalAuthenticatedSource({
      block: block as never,
      evidence: exactEvidence,
    });
    expect(source.acceptedInclusion?.nativeTxId).toBe(nodeTxId);
    expect(source.nativeTxCompactCbor).toBe(transaction.nativeCompactCbor);
    await expect(
      deriveFieldItemWidthIllegalAuthenticatedSource({
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
    const resolver = createFieldItemWidthIllegalRawL1StageResolver({
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
    const material = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    );
    if (material.fullTx === null) throw new Error("missing canonical fixture");
    const output = decodeMidgardFieldPreimage(material.committedPreimage)[0]!;
    const fullTx = materializeMidgardNativeTxFromCanonical({
      ...material.fullTx,
      body: {
        ...material.fullTx.body,
        outputsPreimageCbor: encodeMidgardFieldPreimage([output, output]),
        mintPreimageCbor: encodeMidgardFieldPreimage([Buffer.alloc(0)]),
      },
    });
    const detections = detectFieldItemWidthIllegalCompleteReplay({
      headerHash: "a".repeat(56),
      transactions: [
        {
          nodeTxId: computeMidgardNativeTxId(fullTx).toString("hex"),
          txCbor: encodeMidgardNativeTxCanonical(fullTx).toString("hex"),
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
    const wrongful = detectFieldItemWidthIllegalCompleteReplay({
      headerHash: forced.reconstruction.headerHash,
      transactions: [],
      reconstruction: forced.reconstruction,
    } as never);
    expect(wrongful).toHaveLength(1);
    expect(wrongful[0]?.detectionId).toContain(":forced:0:");

    const illegal = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    ).fullTx!;
    const illegalCbor = encodeMidgardNativeTxCanonical(illegal);
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeMidgardNativeTxCanonical(
        adjudicateMidgardNativeTxFullValidity(illegal, "TxIsInvalid"),
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
      detectFieldItemWidthIllegalCompleteReplay(honestBlock as never),
    ).toEqual([]);
    expect(() =>
      detectFieldItemWidthIllegalCompleteReplay({
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
      runOrResumeManifestBoundFieldItemWidthIllegalWorkflow({
        workflow: {} as never,
        sources: [],
        journal: {} as never,
        evidence,
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence/u);
  });

  it("resumes from authenticated terminal removal without replaying a builder", async () => {
    const identity = fieldItemWidthEvidenceIdentity(evidence);
    const resolveStage = vi.fn();
    const append = vi.fn();
    const runtime = createManifestBoundFieldItemWidthIllegalRuntime({
      config: {} as ManifestBoundFieldItemWidthIllegalConfig,
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
    });
    await expect(runtime.runOrResume(evidence)).resolves.toBe("removed");
    expect(runtime.runtimeVersion).toBe(FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW);
    expect(resolveStage).not.toHaveBeenCalled();
    expect(append).not.toHaveBeenCalled();
  });
});
