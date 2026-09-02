import {
  buildMidgardValidationTraceTree,
  hashMidgardMintAssetLeafV1,
  hashMidgardValidationEventKeyV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationWorkWitnessV1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineStateV1,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { makeNativeTx } from "../../midgard-validation/tests/validation-fixtures.js";
import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID_V1,
  prepareDistinctAssetAccumulationEvidenceV1,
} from "../src/distinct-asset-accumulation-limit/family-v1.js";
import { detectDistinctAssetAccumulationCanonicalViolationsV1 } from "../src/distinct-asset-accumulation-limit/production-replay-v1.js";
import { buildDistinctAssetAuthenticationFromRetainedDaV1 } from "../src/distinct-asset-accumulation-limit/retained-value-and-mint-v1.js";
import { submitDistinctAssetAccumulationCancelV1 } from "../src/distinct-asset-accumulation-limit/submit-cancel-v1.js";
import { submitDistinctAssetAccumulationFoldV1 } from "../src/distinct-asset-accumulation-limit/submit-fold-v1.js";
import { submitDistinctAssetAccumulationStep01AcceptedV1 } from "../src/distinct-asset-accumulation-limit/submit-step-01-v1.js";
import { submitDistinctAssetAccumulationStep02V1 } from "../src/distinct-asset-accumulation-limit/submit-step-02-v1.js";
import { submitDistinctAssetAccumulationStep06V1 } from "../src/distinct-asset-accumulation-limit/submit-step-06-v1.js";
import { requireLinearFaultThreadUtxoV1 } from "../src/linear-fault-family-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixtureV1 } from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

describe("distinctAssetAccumulationLimit concrete Lucid lifecycle", () => {
  it("runs Init through permanent mint, restart, six cancellations, and leased removal", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: {
        realDistinctAssetAccumulationLimit: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = harness.contracts.distinctAssetAccumulationLimit;
    if (contracts === undefined)
      throw new Error("real distinct-asset contracts absent");
    const catalogue = await buildCatalogueDeploymentInfo({
      ...harness.contracts.fraudProofs,
      distinctAssetAccumulationLimit: {
        ...contracts.steps[0],
        spendingScriptCBOR: contracts.steps[0].spendingScript.script,
      },
    });
    const category = catalogue.categories.distinctAssetAccumulationLimit!;
    expect(category.categoryId).toBe(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID_V1,
    );

    const nativeTx = makeNativeTx().tx;
    const block = await buildDecodingBlockFixtureV1({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      subject: { kind: "normal", nativeTx },
    });
    if (block.txInclusion === null)
      throw new Error("accepted transaction inclusion absent");
    const transactionId = block.txInclusion.nativeTxId;
    const eventKey = {
      L2TransactionEventKey: { tx_id: transactionId },
    } as const;
    const eventKeyCbor = Buffer.from(
      Data.to(eventKey as never, SDK.EventKeySchema as never),
      "hex",
    );
    const policyId = Buffer.alloc(28, 0x5a);
    const assetName = Buffer.from("first-crossing", "ascii");
    const quantity = 1n;
    const mintLeaf = hashMidgardMintAssetLeafV1({
      policyId,
      assetName,
      quantity,
    });
    const bytes32 = () => Buffer.alloc(32);
    const nativeControlCbor = encodeCbor([
      Buffer.alloc(0),
      Buffer.alloc(0),
      Buffer.alloc(0),
      Buffer.alloc(0),
      0n,
      bytes32(),
      0n,
      [],
      0n,
      bytes32(),
      0n,
      [],
      0n,
      [],
      0n,
      [],
      0n,
      [],
      [],
      1n,
      [[0n, mintLeaf]],
      0n,
      [],
      0n,
      0n,
      bytes32(),
    ]);
    const accumulatorCbor = encodeCbor([
      0n,
      Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
      16_384n,
      0n,
    ]);
    const witnessCbor = encodeCbor([
      nativeControlCbor,
      4n,
      bytes32(),
      0n,
      0n,
      bytes32(),
      bytes32(),
      bytes32(),
      0n,
      0n,
      0n,
      accumulatorCbor,
    ]);
    const state: MidgardValidationMachineStateV1 = {
      machineVersion: 1,
      eventKeyHash: hashMidgardValidationEventKeyV1(eventKeyCbor),
      transactionId: Buffer.from(transactionId, "hex"),
      transactionCommitment: Buffer.alloc(32, 0x11),
      validationContextHash: Buffer.alloc(32, 0x22),
      sourceKind: "normal",
      priorLedgerRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
      phase: "valueAndMint",
      programCounter: 0,
      workRoot: hashMidgardValidationWorkWitnessV1({
        phase: "valueAndMint",
        programCounter: 0,
        witnessCbor,
      }),
      executionCpu: 0n,
      executionMemory: 0n,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      ledgerDeltaRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
    };
    const trace = buildMidgardValidationTraceTree(
      [hashMidgardValidationMachineStateV1(state)],
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    );
    const descriptor: SDK.ValidationTraceDescriptorV1 = {
      schema_version: BigInt(trace.descriptor.schemaVersion),
      machine_version: BigInt(trace.descriptor.machineVersion),
      trace_root: trace.descriptor.traceRoot.toString("hex"),
      step_count: BigInt(trace.descriptor.stepCount),
      initial_state_hash: trace.descriptor.initialStateHash.toString("hex"),
      terminal_state_hash: trace.descriptor.terminalStateHash.toString("hex"),
      verdict: "Accepted",
      rejection_code_hash: trace.descriptor.rejectionCodeHash.toString("hex"),
    };
    const descriptorCbor = Buffer.from(
      Data.to(
        descriptor as never,
        SDK.ValidationTraceDescriptorV1Schema as never,
      ),
      "hex",
    );
    const traceRoot = await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      [{ key: eventKeyCbor, value: descriptorCbor }],
    );
    const auxiliary: SDK.ValidationAuxiliaryWitnessV1 = {
      ValueMintAssetWitness: {
        mint_index: 0n,
        policy_id: policyId.toString("hex"),
        asset_name: assetName.toString("hex"),
        quantity,
        siblings: [],
        mutation: {
          delta_was_present: false,
          old_delta: 0n,
          delta_proof: [],
        },
      },
    };
    const retained: SDK.RetainedValidationWitnessV1 = {
      machine_state: SDK.validationMachineStateDataFromCore(state),
      trace_proof: SDK.validationTraceProofDataFromCore(trace.proofs[0]!),
      phase: 12n,
      program_counter: 0n,
      witness_cbor: witnessCbor.toString("hex"),
      auxiliary,
    };
    const retainedKey: SDK.RetainedValidationWitnessKeyV1 = {
      event_key: eventKey,
      execution_index: -1n,
    };
    const finding = {
      subject: SDK.acceptedVerdictSubjectV1(transactionId),
      coordinate: { kind: "mint" as const, mintIndex: 0 },
    };
    const retainedAuth = await buildDistinctAssetAuthenticationFromRetainedDaV1(
      {
        eventKey,
        finding,
        authenticatedValidationTraceEntries: [
          { key: eventKeyCbor, value: descriptorCbor },
        ],
        retainedValidationWitnessEntries: [
          {
            key: SDK.encodeRetainedValidationWitnessKeyV1(retainedKey),
            value: SDK.encodeRetainedValidationWitnessV1(retained),
          },
        ],
        expectedValidationTracesRoot: traceRoot.root,
      },
    );
    const evidence = prepareDistinctAssetAccumulationEvidenceV1({
      finding,
      traceStateHashHex: retained.trace_proof.state_hash,
      workRootHex: retained.machine_state.work_root,
      pre: {
        assetRootHex: SDK.EMPTY_MERKLE_TREE_ROOT,
        seenAssetCount: 16_384,
        nonzeroAssetCount: 0,
        cursor: 0,
      },
      post: null,
      mutationWasPresent: false,
    });
    const header = {
      ...block.header,
      validationTracesRoot: traceRoot.root,
      validationTraceCount: traceRoot.count,
    };
    const replayBlock = {
      headerHash: block.headerHash,
      header,
      reconstruction: {
        ...block.reconstruction,
        payload: {
          ...block.reconstruction.payload,
          block_body: {
            ...block.reconstruction.payload.block_body,
            validation_traces: [
              [eventKeyCbor.toString("hex"), descriptorCbor.toString("hex")],
            ],
            validation_trace_witnesses: [
              [
                Buffer.from(
                  SDK.encodeRetainedValidationWitnessKeyV1(retainedKey),
                ).toString("hex"),
                Buffer.from(
                  SDK.encodeRetainedValidationWitnessV1(retained),
                ).toString("hex"),
              ],
            ],
          },
        },
      },
      transactions: block.reconstruction.transactions.map((entry) => ({
        nodeTxId: Buffer.from(entry.keyBytes).toString("hex"),
        txCbor: entry.fullTransactionCbor.toString("hex"),
        l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
      })),
    } as never;
    expect(
      await detectDistinctAssetAccumulationCanonicalViolationsV1(replayBlock),
    ).toHaveLength(1);
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header,
    });
    const references: UTxO[] = [];
    for (const [index, step] of contracts.steps.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `distinct-asset-lifecycle-${index.toString()}`,
          })
        ).utxo,
      );

    const ledger: Array<{
      label: string;
      bytes: number;
      memory: string;
      cpu: string;
      margin: number;
    }> = [];
    const measured = async <T>(label: string, operation: () => Promise<T>) => {
      const capture = await captureEmulatorSubmission(
        harness.emulator,
        operation,
      );
      ledger.push({
        label,
        bytes: capture.measurement.completeSignedBytes,
        memory: capture.measurement.executionMemory.toString(),
        cpu: capture.measurement.executionSteps.toString(),
        margin: capture.measurement.l1ByteMargin,
      });
      return capture.result;
    };
    const init = () =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category: category as never,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const step01 = async (threadOutRef: string) => {
      const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        family: "distinct-asset-accumulation-limit",
        stepIndex: 0,
        threadOutRef,
      });
      return await submitDistinctAssetAccumulationStep01AcceptedV1({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding,
        threadUtxo,
        threadToken,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion!,
        validationTracesRoot: traceRoot.root,
        validationTraceCount: traceRoot.count,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    };
    const step02 = (threadOutRef: string) =>
      submitDistinctAssetAccumulationStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        authentication: retainedAuth.authentication,
        referenceScriptUtxo: references[1]!,
      });
    const fold = (threadOutRef: string, stepIndex: 2 | 3 | 4) =>
      submitDistinctAssetAccumulationFoldV1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        stepIndex,
        action: retainedAuth.folds[stepIndex - 2]!,
        referenceScriptUtxo: references[stepIndex]!,
      });
    const finalize = (threadOutRef: string) =>
      submitDistinctAssetAccumulationStep06V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const advance = async (count: number) => {
      let outRef = (await init()).nextThreadOutRef;
      if (count > 0) outRef = (await step01(outRef)).nextThreadOutRef;
      if (count > 1) outRef = (await step02(outRef)).nextThreadOutRef;
      if (count > 2) outRef = (await fold(outRef, 2)).nextThreadOutRef;
      if (count > 3) outRef = (await fold(outRef, 3)).nextThreadOutRef;
      if (count > 4) outRef = (await fold(outRef, 4)).nextThreadOutRef;
      return outRef;
    };
    for (let stepIndex = 0; stepIndex < 6; stepIndex += 1) {
      const outRef = await advance(stepIndex);
      await measured(`cancel-step-${(stepIndex + 1).toString()}`, () =>
        submitDistinctAssetAccumulationCancelV1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRef,
          referenceScriptUtxo: references[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
    }

    const initialized = await measured("init", init);
    const one = await measured("step01", () =>
      step01(initialized.nextThreadOutRef),
    );
    const two = await measured("step02", () => step02(one.nextThreadOutRef));
    const three = await measured("step03-input-skip", () =>
      fold(two.nextThreadOutRef, 2),
    );
    const four = await measured("step04-output-skip", () =>
      fold(three.nextThreadOutRef, 3),
    );
    const five = await measured("step05-mint-crossing", () =>
      fold(four.nextThreadOutRef, 4),
    );
    const six = await measured("step06-permanent-mint", () =>
      finalize(five.nextThreadOutRef),
    );
    const proof = (
      await harness.proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        six.fraudProofUnit,
      )
    )[0];
    if (proof === undefined) throw new Error("permanent proof mint absent");

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const names = [
      "fraudProofDistinctAssetAccumulationLimit",
      "fraudProofDistinctAssetAccumulationLimitStep02",
      "fraudProofDistinctAssetAccumulationLimitStep03",
      "fraudProofDistinctAssetAccumulationLimitStep04",
      "fraudProofDistinctAssetAccumulationLimitStep05",
      "fraudProofDistinctAssetAccumulationLimitStep06",
    ];
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        ...Object.fromEntries(
          contracts.steps.map((step, index) => [
            names[index]!,
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    const now = BigInt(harness.emulator.now());
    await measured("leased-removal", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "distinctAssetAccumulationLimit" as never,
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "distinct-asset-accumulation-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    for (const row of ledger) {
      expect(row.margin, row.label).toBeGreaterThan(0);
      expect(BigInt(row.memory), row.label).toBeGreaterThan(0n);
      expect(BigInt(row.cpu), row.label).toBeGreaterThan(0n);
    }
  }, 600_000);
});
