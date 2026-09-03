import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  MidgardRedeemerTag,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  applyScriptIntegrityHashMismatchScripts,
  type ScriptIntegrityHashMismatchContracts,
} from "../src/script-integrity-hash-mismatch/contracts.js";
import {
  prepareScriptIntegrityHashMismatchEvidence,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID,
} from "../src/script-integrity-hash-mismatch/family.js";
import { buildScriptIntegrityStageThreeAuthenticationFromRetainedDa } from "../src/script-integrity-hash-mismatch/retained-stage-three.js";
import {
  submitScriptIntegrityHashMismatchCancel,
  submitScriptIntegrityHashMismatchStep01Accepted,
  submitScriptIntegrityHashMismatchStep02,
  submitScriptIntegrityHashMismatchStep03,
  submitScriptIntegrityHashMismatchStep04,
  submitScriptIntegrityHashMismatchStep05,
} from "../src/script-integrity-hash-mismatch/submit.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

describe("scriptIntegrityHashMismatch concrete Lucid lifecycle", () => {
  it("restarts by out-ref, cancels every nonterminal step, mints and records signed ExUnits", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = Data.from(
      Data.to(
        await Effect.runPromise(
          SDK.addressDataFromBech32(
            harness.contracts.fraudProof.spendingScriptAddress,
          ),
        ),
        SDK.AddressData,
      ),
    );
    const applied = applyScriptIntegrityHashMismatchScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.policyId,
    });
    const contracts: ScriptIntegrityHashMismatchContracts = {
      steps: applied.map((step, index) => ({
        ...step,
        blueprintTitle: applied[index]!.blueprintTitle,
        referenceOutRef: `${"00".repeat(32)}#${index}`,
      })) as unknown as ScriptIntegrityHashMismatchContracts["steps"],
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    };
    const catalogue = await buildCatalogueDeploymentInfo({
      ...harness.contracts.fraudProofs,
      scriptIntegrityHashMismatch: {
        ...contracts.steps[0],
        spendingScriptCBOR: contracts.steps[0].spendingScript.script,
      },
    });
    const category = catalogue.categories.scriptIntegrityHashMismatch!;
    expect(category.categoryId).toBe(
      SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID,
    );

    const spent = outRefFromByte(0x61);
    const privateKey = CML.PrivateKey.generate_ed25519();
    const script = plutusV3ScriptWitness(
      Buffer.from(
        "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
        "hex",
      ),
    );
    const spentOutput = makeProtectedScriptOutput(
      hashScriptWitness(script),
      FUNDED_OUTPUT_LOVELACE,
    );
    const producedOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const correct = makeNativeTx({
      spendInputs: [spent],
      outputs: [producedOutput],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
      privateKey,
    });
    const malformedBody = {
      ...correct.tx.body,
      scriptIntegrityHash: Buffer.alloc(32, 0xff),
    };
    const signedBodyHash = computeMidgardNativeTxId({
      version: correct.tx.version,
      transactionBody: deriveMidgardNativeTxBodyCompact(malformedBody),
      transactionWitnessSetHash: Buffer.alloc(32),
      validity: correct.tx.validity,
    });
    const malformed = encodeRecomputedNativeTx({
      ...correct.tx,
      body: malformedBody,
      witnessSet: {
        ...correct.tx.witnessSet,
        addrTxWitsPreimageCbor: encodeByteList([
          Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              privateKey,
            ).to_cbor_bytes(),
          ),
        ]),
      },
    });
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(malformed.txId),
          outputCbor: producedOutput,
        }),
      ],
    });
    const eventKey = {
      L2TransactionEventKey: { tx_id: malformed.txId.toString("hex") },
    } as const;
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: Buffer.from(
          Data.to(eventKey as never, SDK.EventKeySchema as never),
          "hex",
        ),
        sourceKind: "normal",
        blockEndTimeMs: 1_800_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: malformed.txId,
        canonicalTransactionCbor: malformed.txCbor,
        programMaterialSidecarCbor: Buffer.from(
          "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
          "hex",
        ),
        priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
        postUtxosRoot: mutations[0]!.preRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: "E_INVALID_FIELD_TYPE",
      }),
    );
    const integrityIndexes = trace.witnesses.flatMap(({ phase }, index) =>
      phase === "scriptIntegrity" ? [index] : [],
    );
    const stateIndex = integrityIndexes[3] ?? -1;
    if (stateIndex < 0)
      throw new Error(
        `fixture stages ${JSON.stringify(trace.witnesses.map(({ phase, programCounter }) => [phase, programCounter]))}`,
      );
    expect(stateIndex).toBeGreaterThanOrEqual(0);
    const witness = trace.witnesses[stateIndex]!;
    const claimedTree = buildMidgardValidationTraceTree(
      trace.states.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    );
    const descriptor: SDK.ValidationTraceDescriptor = {
      schema_version: BigInt(claimedTree.descriptor.schemaVersion),
      machine_version: BigInt(claimedTree.descriptor.machineVersion),
      trace_root: claimedTree.descriptor.traceRoot.toString("hex"),
      step_count: BigInt(claimedTree.descriptor.stepCount),
      initial_state_hash:
        claimedTree.descriptor.initialStateHash.toString("hex"),
      terminal_state_hash:
        claimedTree.descriptor.terminalStateHash.toString("hex"),
      verdict: "Accepted",
      rejection_code_hash:
        claimedTree.descriptor.rejectionCodeHash.toString("hex"),
    };
    const eventKeyCbor = Buffer.from(
      Data.to(eventKey as never, SDK.EventKeySchema as never),
      "hex",
    );
    const descriptorCbor = Buffer.from(
      Data.to(
        descriptor as never,
        SDK.ValidationTraceDescriptorSchema as never,
      ),
      "hex",
    );
    const traceRoot = await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      [{ key: eventKeyCbor, value: descriptorCbor }],
    );
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      SDK.ValidationAuxiliaryWitnessSchema,
    ) as unknown as SDK.ValidationAuxiliaryWitness;
    const retained: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: SDK.validationTraceProofDataFromCore(
        claimedTree.proofs[stateIndex]!,
      ),
      phase: 10n,
      program_counter: 3n,
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    };
    const retainedKey: SDK.RetainedValidationWitnessKey = {
      event_key: eventKey,
      execution_index: -1n,
    };
    const authentication =
      await buildScriptIntegrityStageThreeAuthenticationFromRetainedDa({
        eventKey,
        authenticatedValidationTraceEntries: [
          { key: eventKeyCbor, value: descriptorCbor },
        ],
        retainedValidationWitnessEntries: [
          {
            key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
            value: SDK.encodeRetainedValidationWitness(retained),
          },
        ],
        expectedValidationTracesRoot: traceRoot.root,
      });
    const evidence = prepareScriptIntegrityHashMismatchEvidence({
      finding: {
        subject: SDK.acceptedVerdictSubject(malformed.txId.toString("hex")),
      },
      scriptIntegrityHash: authentication.scriptIntegrityHash,
      redeemerWitnessHash: authentication.redeemerWitnessHash,
      selectedLanguageBitmap: Number(authentication.control.language_bitmap) as
        | 0
        | 1
        | 2
        | 3,
      executionCount: authentication.control.execution_count,
    });

    const block = await buildDecodingBlockFixture({
      operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
      startTime: BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      ),
      priorLedgerRoot: mutations[0]!.preRoot.toString("hex"),
      subject: { kind: "normal", nativeTx: malformed.tx },
    });
    const header = {
      ...block.header,
      validationTracesRoot: traceRoot.root,
      validationTraceCount: traceRoot.count,
    };
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header,
    });
    if (block.txInclusion === null)
      throw new Error("accepted inclusion absent");
    const references: UTxO[] = [];
    for (const [index, step] of contracts.steps.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `script-integrity-mismatch-${index}`,
          })
        ).utxo,
      );

    const ledger: {
      label: string;
      bytes: number;
      memory: string;
      cpu: string;
      margin: number;
    }[] = [];
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
        category,
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
    const step01 = (threadOutRef: string) =>
      submitScriptIntegrityHashMismatchStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion!,
        header,
        evidence,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const step02 = (threadOutRef: string) =>
      submitScriptIntegrityHashMismatchStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        authentication,
        referenceScriptUtxo: references[1]!,
      });
    const step03 = (threadOutRef: string) =>
      submitScriptIntegrityHashMismatchStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[2]!,
      });
    const step04 = (threadOutRef: string) =>
      submitScriptIntegrityHashMismatchStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
      });
    const cancel = (threadOutRef: string, index: number) =>
      submitScriptIntegrityHashMismatchCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[index]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });

    const advance = async (to: number) => {
      const initial = await init();
      let outRef = initial.nextThreadOutRef;
      if (to > 0) outRef = (await step01(outRef)).nextThreadOutRef;
      if (to > 1) outRef = (await step02(outRef)).nextThreadOutRef;
      if (to > 2) outRef = (await step03(outRef)).nextThreadOutRef;
      if (to > 3) outRef = (await step04(outRef)).nextThreadOutRef;
      if (to > 3) outRef = (await step04(outRef)).nextThreadOutRef;
      return outRef;
    };
    for (let index = 0; index < 5; index += 1) {
      const outRef = await advance(index);
      await expect(
        harness.proverLucid.utxosByOutRef([
          {
            txHash: outRef.slice(0, 64),
            outputIndex: Number(outRef.slice(65)),
          },
        ]),
      ).resolves.toHaveLength(1);
      await measured(`cancel-step0${index + 1}`, () => cancel(outRef, index));
    }

    const initialized = await measured("init", init);
    const one = await measured("step01", () =>
      step01(initialized.nextThreadOutRef),
    );
    const two = await measured("step02", () => step02(one.nextThreadOutRef));
    const three = await measured("step03", () => step03(two.nextThreadOutRef));
    const fourA = await measured("step04-0", () =>
      step04(three.nextThreadOutRef),
    );
    expect(fourA.terminal).toBe(false);
    const fourB = await measured("step04-1", () =>
      step04(fourA.nextThreadOutRef),
    );
    expect(fourB.terminal).toBe(true);
    const five = await measured("step05", () =>
      submitScriptIntegrityHashMismatchStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: fourB.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(five.fraudProofUnit).toBeTruthy();
    const proofUtxos = await harness.proverLucid.utxosAtWithUnit(
      contracts.fraudProof.spendingScriptAddress,
      five.fraudProofUnit,
    );
    const proof = proofUtxos[0];
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
    const firstStepDeploymentEntry =
      "fraudProofScriptIntegrityHashMismatch" as const;
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        [firstStepDeploymentEntry]: {
          scriptHash: contracts.steps[0].spendingScriptHash,
          contract: {
            type: contracts.steps[0].spendingScript.type,
            cborHex: contracts.steps[0].spendingScript.script,
          },
        },
        ...Object.fromEntries(
          contracts.steps.slice(1).map((step, offset) => [
            `fraudProofScriptIntegrityHashMismatchStep0${offset + 2}`,
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
    await measured("removal", () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "scriptIntegrityHashMismatch",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "script-integrity-hash-mismatch-emulator",
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
    ledger.forEach((row) => {
      expect(row.margin, row.label).toBeGreaterThan(0);
      expect(BigInt(row.memory), row.label).toBeGreaterThan(0n);
      expect(BigInt(row.cpu), row.label).toBeGreaterThan(0n);
    });
    console.info(
      `[script-integrity-hash-mismatch-lifecycle-ledger] ${JSON.stringify(ledger)}`,
    );
  }, 600_000);
});
