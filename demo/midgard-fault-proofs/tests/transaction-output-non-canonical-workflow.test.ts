import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  bindTransactionOutputNonCanonicalReferenceScripts,
  createTransactionOutputNonCanonicalRawL1StageResolver,
  createTransactionOutputNonCanonicalWorkflowRunnerSurface,
  deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock,
  detectTransactionOutputNonCanonicalCompleteReplay,
  TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
  type TransactionOutputNonCanonicalReferenceScripts,
} from "../src/transaction-output-non-canonical/index.js";
import type { FraudProofWorkflowDeploymentBinding } from "../src/workflow/deployment-manifest-binding.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});
const utxo = (byte: string, outputIndex: number): UTxO => ({
  txHash: byte.repeat(64),
  outputIndex,
  address: "addr_test1vr0outputnoncanonical",
  assets: { lovelace: 2_000_000n },
  scriptRef: script(byte),
});
const references = (): TransactionOutputNonCanonicalReferenceScripts => ({
  step01: utxo("1", 0),
  step02: utxo("2", 1),
  step03: utxo("3", 2),
  step04: utxo("4", 3),
  fieldPreimageCertificateMint: utxo("5", 4),
  witnesses: {
    computationThreadMint: utxo("6", 5),
    fraudProofMint: utxo("7", 6),
    phasMembershipWithdraw: utxo("8", 7),
  },
});

describe("transactionOutputNonCanonical production workflow", () => {
  it("exposes the standard callback-free execution surface and all references", () => {
    const runner = createTransactionOutputNonCanonicalWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("not reached");
      },
    });
    expect(Object.keys(runner).sort()).toEqual([
      "runOrResume",
      "runnerVersion",
    ]);
    expect(
      Object.values(TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS),
    ).toEqual([
      "fraudProofTransactionOutputNonCanonical",
      "fraudProofTransactionOutputNonCanonicalStep02",
      "fraudProofTransactionOutputNonCanonicalStep03",
      "fraudProofTransactionOutputNonCanonicalStep04",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "fieldPreimageCertificateMint",
    ]);
  });

  it("binds reference out-refs and script identities against the manifest", () => {
    const supplied = references();
    const names = Object.values(
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
    );
    const values = [
      supplied.step01,
      supplied.step02,
      supplied.step03,
      supplied.step04,
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
    } as unknown as FraudProofWorkflowDeploymentBinding<never>;
    expect(
      bindTransactionOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: supplied,
      }),
    ).toStrictEqual(supplied);
    expect(() =>
      bindTransactionOutputNonCanonicalReferenceScripts({
        binding,
        referenceScripts: {
          ...supplied,
          step04: { ...supplied.step04, outputIndex: 99 },
        },
      }),
    ).toThrow(/differs from finalized manifest identity/u);
  });

  it("scans every retained field-2 output and derives the sole exact violation", () => {
    const material = committedFieldShapeScenarioMaterial("honest");
    if (material.fullTx === null) throw new Error("missing full transaction");
    const canonical = Buffer.from(
      "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
      "hex",
    );
    const malformed = Buffer.from(
      `b802${canonical.subarray(1).toString("hex")}`,
      "hex",
    );
    const fullTx = materializeMidgardNativeTxFromCanonical({
      ...material.fullTx,
      body: {
        ...material.fullTx.body,
        outputsPreimageCbor: encodeMidgardFieldPreimage([canonical, malformed]),
      },
    });
    const nodeTxId = computeMidgardNativeTxId(fullTx).toString("hex");
    const block = {
      headerHash: "a".repeat(56),
      transactions: [
        {
          nodeTxId,
          txCbor: encodeMidgardNativeTxCanonical(fullTx).toString("hex"),
        },
      ],
      reconstruction: { forcedTransactions: [] },
    };
    const detections = detectTransactionOutputNonCanonicalCompleteReplay(
      block as never,
    );
    expect(detections).toHaveLength(1);
    expect(detections[0]?.detectionId).toContain(":2:1:");
    expect(
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(
        block as never,
      ).itemIndex,
    ).toBe(1);
  });

  it("resolves the fourth physical checkpoint from raw L1", async () => {
    const resolver = createTransactionOutputNonCanonicalRawL1StageResolver({
      config: {
        binding: { definition: { headerHash: "a".repeat(56) } },
      } as never,
      l1: {
        observe: async () => ({
          stage: {
            kind: "step",
            step: 4,
            threadOutRef: `${"b".repeat(64)}#0`,
            stateQueueBlockOutRef: `${"c".repeat(64)}#0`,
          },
        }),
      } as never,
      source: { nativeTxCompactCbor: "aa", witnessSetCompactCbor: "bb" },
    });
    await expect(
      resolver({ action: "submitStep04", evidence: {} as never }),
    ).resolves.toEqual(
      expect.objectContaining({ threadOutRef: `${"b".repeat(64)}#0` }),
    );
  });
});
