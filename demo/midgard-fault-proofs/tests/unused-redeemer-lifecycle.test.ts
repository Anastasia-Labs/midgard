import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
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
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  applyUnusedRedeemerScripts,
  type UnusedRedeemerContracts,
} from "../src/unused-redeemer/contracts-v1.js";
import { UNUSED_REDEEMER_CATEGORY_ID } from "../src/unused-redeemer/family-v1.js";
import { buildUnusedRedeemerMaterialFromRetainedDa } from "../src/unused-redeemer/production-replay-v1.js";
import { submitUnusedRedeemerCancel } from "../src/unused-redeemer/submit-cancel-v1.js";
import {
  submitUnusedRedeemerStep01Accepted,
  submitUnusedRedeemerStep01Forced,
} from "../src/unused-redeemer/submit-step-01-v1.js";
import { submitUnusedRedeemerStep02 } from "../src/unused-redeemer/submit-step-02-v1.js";
import { submitUnusedRedeemerStep02a } from "../src/unused-redeemer/submit-step-02a-v1.js";
import { submitUnusedRedeemerStep02b } from "../src/unused-redeemer/submit-step-02b-v1.js";
import { submitUnusedRedeemerStep02c } from "../src/unused-redeemer/submit-step-02c-v1.js";
import { submitUnusedRedeemerStep03 } from "../src/unused-redeemer/submit-step-03-v1.js";
import { submitUnusedRedeemerStep04 } from "../src/unused-redeemer/submit-step-04-v1.js";
import { submitUnusedRedeemerStep05 } from "../src/unused-redeemer/submit-step-05-v1.js";
import { submitUnusedRedeemerStep06 } from "../src/unused-redeemer/submit-step-06-v1.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

const buildMaterial = async (
  direction: "accepted" | "forced",
  mutateProofIndex = false,
  redeemerIndexOverride?: number,
  omitAuditHeader = false,
) => {
  const spent = outRefFromByte(0x71);
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
  const source = makeNativeTx({
    spendInputs: [spent],
    outputs: [producedOutput],
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      { tag: MidgardRedeemerTag.Spend, index: 0n },
      { tag: MidgardRedeemerTag.Mint, index: 0n },
    ]),
    scriptLanguages: ["PlutusV3"],
    privateKey,
  });
  const bodyHash = computeMidgardNativeTxId({
    version: source.tx.version,
    transactionBody: deriveMidgardNativeTxBodyCompact(source.tx.body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: source.tx.validity,
  });
  const transaction = encodeRecomputedNativeTx({
    ...source.tx,
    witnessSet: {
      ...source.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList([
        Buffer.from(
          CML.make_vkey_witness(
            CML.TransactionHash.from_raw_bytes(bodyHash),
            privateKey,
          ).to_cbor_bytes(),
        ),
      ]),
    },
  });
  const sourceKey = { transactionId: "f7".repeat(32), outputIndex: 0n };
  const eventKey =
    direction === "accepted"
      ? ({
          L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
        } as const)
      : ({ ForcedTransactionEventKey: { tx_order_id: sourceKey } } as const);
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey as never, SDK.EventKeySchema as never),
        "hex",
      ),
      sourceKind: direction === "accepted" ? "normal" : "forced",
      blockEndTimeMs: 1_800_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      programMaterialSidecarCbor: Buffer.from(
        "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
        "hex",
      ),
      priorUtxosRoot: "00".repeat(32),
      postUtxosRoot: "00".repeat(32),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_INVALID_FIELD_TYPE",
    }),
  );
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    direction === "accepted" ? "accepted" : "rejected",
    direction === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_INVALID_FIELD_TYPE"),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(claimedTree.descriptor.schemaVersion),
    machine_version: BigInt(claimedTree.descriptor.machineVersion),
    trace_root: claimedTree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(claimedTree.descriptor.stepCount),
    initial_state_hash: claimedTree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash:
      claimedTree.descriptor.terminalStateHash.toString("hex"),
    verdict: direction === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash:
      claimedTree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema as never),
    "hex",
  );
  const descriptorCbor = Buffer.from(
    Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema as never),
    "hex",
  );
  const traceRoot = await buildCountedRoot(SDK.ROOT_DOMAINS.validationTraces, [
    { key: eventKeyCbor, value: descriptorCbor },
  ]);
  const retainedWitnesses = trace.witnesses.flatMap((witness, index) => {
    if (witness.phase !== "scriptSources" && witness.phase !== "nativeScripts")
      return [];
    const defaultTarget = direction === "accepted" ? 1 : 0;
    const auditHeaderPc = defaultTarget === 1 ? 88 : 85;
    if (omitAuditHeader && witness.programCounter === auditHeaderPc) return [];
    const retained: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[index]!,
      ),
      trace_proof: {
        ...SDK.validationTraceProofDataFromCore(claimedTree.proofs[index]!),
        state_index: BigInt(index + (mutateProofIndex ? 1 : 0)),
      },
      phase: witness.phase === "scriptSources" ? 8n : 9n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary: Data.from(
        Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
        SDK.ValidationAuxiliaryWitnessSchema,
      ) as unknown as SDK.ValidationAuxiliaryWitness,
    };
    return [
      [
        SDK.encodeRetainedValidationWitnessKey({
          event_key: eventKey,
          execution_index: -BigInt(index + 1),
        }),
        SDK.encodeRetainedValidationWitness(retained),
      ] as const,
    ];
  });
  const block = {
    header: { validationTracesRoot: traceRoot.root },
    reconstruction: {
      payload: {
        block_body: {
          validation_traces: [
            [eventKeyCbor.toString("hex"), descriptorCbor.toString("hex")],
          ],
          validation_trace_witnesses: retainedWitnesses.map(([key, value]) => [
            key.toString("hex"),
            value.toString("hex"),
          ]),
        },
      },
    },
  } as unknown as CanonicalBlockEvidence;
  const redeemerIndex =
    redeemerIndexOverride ?? (direction === "accepted" ? 1 : 0);
  const subject =
    direction === "accepted"
      ? SDK.acceptedVerdictSubject(transaction.txId.toString("hex"))
      : SDK.forcedVerdictSubject({
          transactionId: transaction.txId.toString("hex"),
          sourceKey,
          rejectionReason: { UnusedRedeemer: { redeemer_index: 0n } },
        });
  const material = await buildUnusedRedeemerMaterialFromRetainedDa({
    block,
    eventKey,
    subject,
    redeemerIndex,
    txCbor: transaction.txCbor,
  });
  return { material, transaction, traceRoot, trace, subject, eventKey };
};

describe("unusedRedeemer concrete retained lifecycle material", () => {
  it("derives exact stage-12 item and execution evidence", async () => {
    const { material } = await buildMaterial("accepted");
    expect(material.evidence.unused).toBe(true);
    expect(material.authentication.control.stage).toBe(12n);
    expect(material.authentication.itemControl.item_index).toBe(1n);
    expect(material.authentication.controlState.program_counter).toBe(88n);
  });

  it("derives the exact stage-12 selected redeemer frontier", async () => {
    const { material } = await buildMaterial("forced");
    expect(material.evidence.unused).toBe(false);
    expect(material.authentication.control.stage).toBe(12n);
    expect(material.authentication.itemControl.item_index).toBe(0n);
    expect(material.authentication.controlState.program_counter).toBe(85n);
  });

  it("refuses a wrong global program-counter substitution", async () => {
    await expect(buildMaterial("accepted", true)).rejects.toThrow(
      "retained state/proof/work witness is invalid",
    );
  });

  it("refuses a cross-cursor substitution", async () => {
    await expect(buildMaterial("accepted", false, 0)).rejects.toThrow(
      "terminal ScriptSources frontier is incomplete",
    );
  });

  it("refuses substituting the post-selection stage-11 frontier", async () => {
    await expect(
      buildMaterial("accepted", false, undefined, true),
    ).rejects.toThrow("exact direction-specific ScriptSources state is absent");
  });

  it.each(["accepted", "forced"] as const)(
    "runs %s Init through all nine real reference scripts",
    async (direction) => {
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
      const applied = applyUnusedRedeemerScripts({
        blueprint: harness.realBlueprint,
        network,
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: addressData,
        hubOracleScriptHash: harness.contracts.hubOracle.policyId,
      });
      const contracts: UnusedRedeemerContracts = {
        steps: applied.map((step, index) => ({
          ...step,
          referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
        })) as unknown as UnusedRedeemerContracts["steps"],
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      };
      const catalogue = await buildCatalogueDeploymentInfo({
        ...harness.contracts.fraudProofs,
        unusedRedeemer: {
          ...contracts.steps[0],
          spendingScriptCBOR: contracts.steps[0].spendingScript.script,
        },
      });
      const category = catalogue.categories.unusedRedeemer!;
      expect(category.categoryId).toBe(UNUSED_REDEEMER_CATEGORY_ID);
      const { material, transaction, traceRoot, eventKey } =
        await buildMaterial(direction);
      const forcedEvent = eventKey.ForcedTransactionEventKey;
      const forcedOrderKey = forcedEvent?.tx_order_id ?? null;
      if (direction === "forced" && forcedOrderKey === null)
        throw new Error("forced event key absent");
      const block = await buildDecodingBlockFixture({
        operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
        startTime: BigInt(
          alignUnixTimeToEmulatorSlotBoundary(
            harness.funderLucid,
            harness.emulator.now() + 120_000,
          ) - 1,
        ),
        priorLedgerRoot: "00".repeat(32),
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: transaction.tx }
            : {
                kind: "forced",
                nativeTx: transaction.tx,
                orderKey: forcedOrderKey!,
                verdict: {
                  ForcedTxInvalid: {
                    reason: { UnusedRedeemer: { redeemer_index: 0n } },
                  },
                },
              },
      });
      if (direction === "accepted" && block.txInclusion === null)
        throw new Error("accepted inclusion absent");
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
      const references: UTxO[] = [];
      for (const [index, step] of contracts.steps.entries())
        references.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `unused-redeemer-${index.toString()}`,
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
      const measured = async <T>(
        label: string,
        operation: () => Promise<T>,
      ) => {
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
      const initialize = () =>
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
      const init = await measured(`${direction}-init`, initialize);
      let outRef = init.nextThreadOutRef;
      const restart = async () => {
        const [utxo] = await harness.proverLucid.utxosByOutRef([
          {
            txHash: outRef.slice(0, 64),
            outputIndex: Number(outRef.slice(65)),
          },
        ]);
        expect(utxo).toBeDefined();
      };
      await restart();
      outRef = (
        await measured(`${direction}-step01`, async () => {
          if (direction === "accepted")
            return await submitUnusedRedeemerStep01Accepted({
              lucid: harness.proverLucid,
              blueprint: harness.realBlueprint,
              network,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef: outRef,
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
              txInclusion: block.txInclusion!,
              header,
              redeemerIndex: 1n,
              referenceScriptUtxo: references[0]!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            });
          const membership = await buildForcedTransactionLeafMembershipProof({
            reconstruction: block.reconstruction,
            eventKey,
          });
          return await submitUnusedRedeemerStep01Forced({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: outRef,
            header,
            membership,
            redeemerIndex: 0n,
            referenceScriptUtxo: references[0]!,
          });
        })
      ).nextThreadOutRef;
      await restart();
      const linear = [
        submitUnusedRedeemerStep02,
        submitUnusedRedeemerStep02a,
        submitUnusedRedeemerStep02b,
        submitUnusedRedeemerStep02c,
        submitUnusedRedeemerStep03,
        submitUnusedRedeemerStep04,
      ] as const;
      for (const [offset, submit] of linear.entries()) {
        const result = await measured(`${direction}-step-${offset + 2}`, () =>
          submit({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: outRef,
            authentication: material.authentication,
            evidence: material.evidence,
            referenceScriptUtxo: references[offset + 1]!,
          } as never),
        );
        outRef = result.nextThreadOutRef;
        await restart();
      }
      const scan = await measured(`${direction}-step05`, () =>
        submitUnusedRedeemerStep05({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRef,
          evidence: material.evidence,
          referenceScriptUtxo: references[7]!,
        }),
      );
      expect(scan.complete).toBe(true);
      outRef = scan.nextThreadOutRef;
      await restart();
      const finalized = await measured(`${direction}-step06`, () =>
        submitUnusedRedeemerStep06({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRef,
          evidence: material.evidence,
          referenceScriptUtxo: references[8]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expect(finalized.fraudProofUnit).toBeTruthy();
      if (direction === "accepted") {
        const advanceForCancel = async (target: number) => {
          let current = (await initialize()).nextThreadOutRef;
          if (target === 0) return current;
          current = (
            await submitUnusedRedeemerStep01Accepted({
              lucid: harness.proverLucid,
              blueprint: harness.realBlueprint,
              network,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef: current,
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
              txInclusion: block.txInclusion!,
              header,
              redeemerIndex: 1n,
              referenceScriptUtxo: references[0]!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            })
          ).nextThreadOutRef;
          for (let step = 1; step < target && step <= 6; step += 1) {
            const submit = linear[step - 1]!;
            current = (
              await submit({
                lucid: harness.proverLucid,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: current,
                authentication: material.authentication,
                evidence: material.evidence,
                referenceScriptUtxo: references[step]!,
              } as never)
            ).nextThreadOutRef;
          }
          if (target > 7)
            current = (
              await submitUnusedRedeemerStep05({
                lucid: harness.proverLucid,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: current,
                evidence: material.evidence,
                referenceScriptUtxo: references[7]!,
              })
            ).nextThreadOutRef;
          return current;
        };
        for (let target = 0; target < 9; target += 1) {
          const cancelOutRef = await advanceForCancel(target);
          await measured(`cancel-step-${target.toString()}`, () =>
            submitUnusedRedeemerCancel({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef: cancelOutRef,
              referenceScriptUtxo: references[target]!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          );
        }
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
          "fraudProofUnusedRedeemer",
          "fraudProofUnusedRedeemerStep02",
          "fraudProofUnusedRedeemerStep02a",
          "fraudProofUnusedRedeemerStep02b",
          "fraudProofUnusedRedeemerStep02c",
          "fraudProofUnusedRedeemerStep03",
          "fraudProofUnusedRedeemerStep04",
          "fraudProofUnusedRedeemerStep05",
          "fraudProofUnusedRedeemerStep06",
        ] as const;
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
        await measured("leased-target-descendant-removal", () =>
          submitRemoveFraudulentBlock({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            deploymentInfo,
            network,
            signer: harness.proverSigner,
            fraudCategory: "unusedRedeemer",
            fraudulentHeaderHash: setup.headerHash,
            awaitConfirmation: true,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: {
              acquire: async () => ({
                token: "unused-redeemer-emulator-lease",
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
      }
      ledger.forEach((row) => {
        expect(row.margin, row.label).toBeGreaterThan(0);
        expect(BigInt(row.memory), row.label).toBeGreaterThan(0n);
        expect(BigInt(row.cpu), row.label).toBeGreaterThan(0n);
      });
      console.info(
        `[unused-redeemer-lifecycle-ledger] ${JSON.stringify(ledger)}`,
      );
    },
    600_000,
  );
});
