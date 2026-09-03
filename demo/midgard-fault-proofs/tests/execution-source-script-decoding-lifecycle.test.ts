import {
  buildMidgardValidationMerkleMembership,
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  EventKeySchema,
  forcedVerdictSubject,
  type RetainedValidationWitness,
  type RetainedValidationWitnessKey,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { prepareExecutionSourceScriptDecodingArtifact } from "../src/execution-source-script-decoding/authenticated-replay.js";
import { applyExecutionSourceScriptDecodingScripts } from "../src/execution-source-script-decoding/contracts.js";
import { prepareExecutionSourceScriptDecodingEvidence } from "../src/execution-source-script-decoding/family.js";
import { buildExecutionSourceMachineAuthentication } from "../src/execution-source-script-decoding/machine-authentication.js";
import { submitExecutionSourceScriptDecodingCancel } from "../src/execution-source-script-decoding/submit-cancel.js";
import { submitExecutionSourceScriptDecodingInit } from "../src/execution-source-script-decoding/submit-init.js";
import {
  submitExecutionSourceScriptDecodingStep01Accepted,
  submitExecutionSourceScriptDecodingStep01Forced,
} from "../src/execution-source-script-decoding/submit-step-01.js";
import { submitExecutionSourceScriptDecodingStep02 } from "../src/execution-source-script-decoding/submit-step-02.js";
import { submitExecutionSourceScriptDecodingStep03 } from "../src/execution-source-script-decoding/submit-step-03.js";
import { submitExecutionSourceScriptDecodingStep04 } from "../src/execution-source-script-decoding/submit-step-04.js";
import { submitExecutionSourceScriptDecodingStep05 } from "../src/execution-source-script-decoding/submit-step-05.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

describe("executionSourceScriptDecoding genuine machine fixture", () => {
  it("reconstructs the authenticated nativeExecutionScan state and proof", async () => {
    const spent = outRefFromByte(0x71);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const script = nativeScriptWitness({ type: "all", scripts: [] });
    const policyId = Buffer.from(hashScriptWitness(script), "hex");
    const assetName = Buffer.from("31", "hex");
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 1n]])],
      ]),
    );
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [script],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, 1n]])]]),
      ),
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: encodeCbor([2n, transaction.txId]),
        sourceKind: "normal",
        blockEndTimeMs: 1_750_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    const stateIndex = trace.witnesses.findIndex(
      ({ auxiliary }) => auxiliary?.kind === "nativeExecutionDescriptor",
    );
    expect(stateIndex).toBeGreaterThanOrEqual(0);
    const witness = trace.witnesses[stateIndex]!;
    expect(witness.phase).toBe("nativeScripts");
    expect(witness.auxiliary).toMatchObject({
      kind: "nativeExecutionDescriptor",
      executionIndex: 0,
      languageTag: 0,
    });
    expect(trace.tree.proofs[stateIndex]?.stateHash).toEqual(
      trace.tree.proofs[stateIndex]?.stateHash,
    );
    const authenticated = await buildExecutionSourceMachineAuthentication({
      trace,
      eventKey: {
        L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
      },
      claimedVerdict: "accepted",
      claimedRejectionCode: null,
    });
    expect(authenticated.validationTraceCount).toBe(1n);
    expect(authenticated.validationTracesRoot).toMatch(/^[0-9a-f]{64}$/u);
    expect(authenticated.authentication.language_tag).toBe(0n);
    expect(authenticated.authentication.trace_proof.state_hash).toBe(
      trace.tree.proofs[stateIndex]?.stateHash.toString("hex"),
    );
  }, 60_000);

  it.each([
    { direction: "forced" as const, cancelAt: null },
    { direction: "accepted" as const, cancelAt: null },
    { direction: "forced" as const, cancelAt: "step01" as const },
    { direction: "forced" as const, cancelAt: "step02" as const },
    { direction: "forced" as const, cancelAt: "step03" as const },
    { direction: "forced" as const, cancelAt: "scan" as const },
  ])(
    "runs $direction lifecycle (cancel=$cancelAt)",
    async ({ direction, cancelAt }) => {
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: { alwaysFraudProofCatalogue: true },
      });
      const addressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(
          Effect.map((address) => Data.from(Data.to(address, AddressData))),
        ),
      );
      const applied = applyExecutionSourceScriptDecodingScripts({
        blueprint: harness.realBlueprint,
        network,
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: addressData,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      });
      const contracts = {
        steps: applied,
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        stateQueuePolicyId: harness.contracts.stateQueue.policyId,
        fieldPreimageCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        fieldPreimageCertificateMintingScript:
          harness.contracts.fieldPreimageCertificate.mintingScript,
      };
      const catalogue = await buildCatalogueDeploymentInfo({
        ...harness.contracts.fraudProofs,
        executionSourceScriptDecoding: {
          ...harness.contracts.fraudProofs.executionSourceScriptDecoding,
          spendingScriptHash: applied[0].spendingScriptHash,
        },
      });
      const category = catalogue.categories.executionSourceScriptDecoding;
      expect(category.categoryId).toBe("00000031");
      expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
      const spent = outRefFromByte(0x72);
      const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
      const script = nativeScriptWitness({ type: "all", scripts: [] });
      const canonicalScriptItem = encodeMidgardVersionedScript(script);
      const malformedScriptItem = Buffer.from("820043820700", "hex");
      const scriptItem =
        direction === "accepted" ? malformedScriptItem : canonicalScriptItem;
      const policyId =
        direction === "accepted"
          ? computeHash28(
              Buffer.concat([Buffer.from([0]), Buffer.from("820700", "hex")]),
            )
          : Buffer.from(hashScriptWitness(script), "hex");
      const assetName = Buffer.from("31", "hex");
      const output = makeOutput(
        FUNDED_OUTPUT_LOVELACE,
        undefined,
        new Map([
          [
            policyId.toString("hex"),
            new Map([[assetName.toString("hex"), 1n]]),
          ],
        ]),
      );
      let transaction = makeNativeTx({
        version: 1n,
        spendInputs: [spent],
        outputs: [output],
        scriptWitnesses: [script],
        mintPreimageCbor: makeMintPreimageCbor(
          new Map([[policyId, new Map([[assetName, 1n]])]]),
        ),
      });
      if (direction === "accepted") {
        const firstRaw = encodeRecomputedNativeTx({
          ...transaction.tx,
          witnessSet: {
            ...transaction.tx.witnessSet,
            scriptTxWitsPreimageCbor: encodeCbor([malformedScriptItem]),
          },
        });
        transaction = encodeRecomputedNativeTx({
          ...firstRaw.tx,
        });
      }
      const nativeTx =
        direction === "forced"
          ? decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor)
          : transaction.tx;
      const allOperations = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ];
      const operations = direction === "forced" ? allOperations : [];
      const mutations = await buildValidationMachineLedgerMutationSteps({
        initialEntries: [{ outRef: spent, output: spentOutput }],
        operations: allOperations,
      });
      const orderKey = { transactionId: "73".repeat(32), outputIndex: 0n };
      const eventKey =
        direction === "forced"
          ? ({ ForcedTransactionEventKey: { tx_order_id: orderKey } } as const)
          : ({
              L2TransactionEventKey: {
                tx_id: transaction.txId.toString("hex"),
              },
            } as const);
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(
            Data.to(eventKey as never, EventKeySchema),
            "hex",
          ),
          sourceKind: direction === "forced" ? "forced" : "normal",
          committedForcedVerdict:
            direction === "forced" ? "rejected" : undefined,
          blockEndTimeMs: 1_750_000_001_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 0n,
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
          postUtxosRoot:
            direction === "forced"
              ? mutations.at(-1)!.postRoot.toString("hex")
              : mutations[0]!.preRoot.toString("hex"),
          ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
          expectedLedgerOps: operations,
          ledgerMutationSteps: direction === "forced" ? mutations : [],
          expectedVerdict: direction === "forced" ? "accepted" : "rejected",
          expectedRejectionCode:
            direction === "forced" ? null : "E_INVALID_FIELD_TYPE",
        }),
      );
      const authentication = await buildExecutionSourceMachineAuthentication({
        trace,
        eventKey,
        claimedVerdict: direction === "forced" ? "rejected" : "accepted",
        claimedRejectionCode:
          direction === "forced" ? "E_INVALID_FIELD_TYPE" : null,
      });
      const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
      const startTime = BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      );
      const reason = {
        ExecutionNativeScriptMalformed: { execution_index: 0n },
      } as const;
      const block = await buildDecodingBlockFixture({
        operatorVkey,
        startTime,
        priorLedgerRoot: mutations[0]!.preRoot.toString("hex"),
        subject:
          direction === "forced"
            ? {
                kind: "forced",
                nativeTx,
                orderKey,
                verdict: { ForcedTxInvalid: { reason } },
              }
            : { kind: "normal", nativeTx },
      });
      const header = {
        ...block.header,
        validationTracesRoot: authentication.validationTracesRoot,
        validationTraceCount: authentication.validationTraceCount,
      };
      if (direction === "forced" && cancelAt === null) {
        const stateIndex = trace.witnesses.findIndex(
          ({ phase, auxiliary }) =>
            phase === "nativeScripts" &&
            auxiliary?.kind === "nativeExecutionDescriptor",
        );
        const retained = trace.witnesses[stateIndex];
        if (
          retained?.auxiliary?.kind !== "nativeExecutionDescriptor" ||
          trace.states[stateIndex] === undefined ||
          trace.tree.proofs[stateIndex] === undefined
        )
          throw new Error("forced retained witness fixture is incomplete");
        const eventKeyCbor = Data.to(
          authentication.authentication.trace_membership.key as never,
          EventKeySchema,
        );
        const validationTraceEntry = [
          eventKeyCbor,
          Data.to(
            authentication.authentication.trace_membership.value as never,
            ValidationTraceDescriptorSchema,
          ),
        ] as const;
        const retainedKey: RetainedValidationWitnessKey = {
          event_key: eventKey,
          execution_index: 0n,
        };
        const retainedValue: RetainedValidationWitness = {
          machine_state: validationMachineStateDataFromCore(
            trace.states[stateIndex]!,
          ),
          trace_proof: validationTraceProofDataFromCore(
            trace.tree.proofs[stateIndex]!,
          ),
          phase: 9n,
          program_counter: BigInt(retained.programCounter),
          witness_cbor: retained.cbor.toString("hex"),
          auxiliary: Data.from(
            Data.to(
              validationAuxiliaryWitnessData(retained.auxiliary) as never,
            ),
            ValidationAuxiliaryWitnessSchema,
          ) as unknown as RetainedValidationWitness["auxiliary"],
        };
        const artifact = await prepareExecutionSourceScriptDecodingArtifact({
          headerHash: block.headerHash,
          header,
          reconstruction: {
            ...block.reconstruction,
            payload: {
              ...block.reconstruction.payload,
              block_body: {
                ...block.reconstruction.payload.block_body,
                validation_traces: [validationTraceEntry],
                validation_trace_witnesses: [
                  [
                    encodeRetainedValidationWitnessKey(retainedKey).toString(
                      "hex",
                    ),
                    encodeRetainedValidationWitness(retainedValue).toString(
                      "hex",
                    ),
                  ],
                ],
              },
            },
          },
          transactions: [],
        } as unknown as CanonicalBlockEvidence);
        expect(artifact.forcedMembership).toBeDefined();
        expect(artifact.acceptedInclusion).toBeUndefined();
      }
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
      const references: UTxO[] = [];
      for (const [index, step] of applied.entries())
        references.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `execution-source-lifecycle-${index.toString()}`,
            })
          ).utxo,
        );
      const lifecycleRows: Array<{
        label: string;
        measurement: CompleteSignedTransactionMeasurement;
      }> = [];
      const measured = async <T>(
        label: string,
        operation: () => Promise<T>,
      ): Promise<T> => {
        const captured = await captureEmulatorSubmission(
          harness.emulator,
          operation,
        );
        lifecycleRows.push({ label, measurement: captured.measurement });
        return captured.result;
      };
      const subject =
        direction === "forced"
          ? forcedVerdictSubject({
              transactionId: block.nativeTxId,
              sourceKey: orderKey,
              rejectionReason: reason,
            })
          : acceptedVerdictSubject(block.nativeTxId);
      const auxiliary = trace.witnesses.find(
        ({ phase, auxiliary }) =>
          phase === "nativeScripts" &&
          auxiliary?.kind === "nativeExecutionDescriptor",
      )?.auxiliary;
      if (auxiliary?.kind !== "nativeExecutionDescriptor")
        throw new Error("missing native descriptor");
      const purposeLeaf = hashMidgardScriptPurposeLeaf({
        purposeKind: auxiliary.purpose.purposeKind,
        purposeIndex: auxiliary.purpose.purposeIndex,
        scriptHash: auxiliary.purpose.scriptHash,
        subject: auxiliary.purpose.subject,
      });
      const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
        sourceIndex: BigInt(auxiliary.source.sourceIndex),
        scriptLanguageTag: 0,
        scriptHash: auxiliary.purpose.scriptHash,
        scriptTotalLength: auxiliary.source.scriptTotalLength,
        itemCommitment: auxiliary.source.scriptItemCommitment,
      });
      const executionLeaf = hashMidgardScriptExecutionLeaf({
        languageTag: 0,
        purposeLeaf,
        sourceLeaf,
        redeemerLeaf: auxiliary.redeemerLeaf,
      });
      const evidence = prepareExecutionSourceScriptDecodingEvidence({
        finding: { subject, executionIndex: 0 },
        descriptor: {
          sourceIndex: auxiliary.source.sourceIndex,
          originKind: 0,
          sourceKeyHex: auxiliary.source.sourceKey.toString("hex"),
          languageTag: 0,
          scriptHashHex: auxiliary.purpose.scriptHash.toString("hex"),
          scriptItemHex: scriptItem.toString("hex"),
          purposeKind: auxiliary.purpose.purposeKind,
          purposeIndex: Number(auxiliary.purpose.purposeIndex),
          purposeSubjectHex: auxiliary.purpose.subject.toString("hex"),
          redeemerLeafHex: "",
          purposeMembership: buildMidgardValidationMerkleMembership(
            [purposeLeaf],
            0,
          ),
          sourceMembership: buildMidgardValidationMerkleMembership(
            [sourceLeaf],
            0,
          ),
          executionMembership: buildMidgardValidationMerkleMembership(
            [executionLeaf],
            0,
          ),
        },
      });
      const init = await measured(
        "init",
        async () =>
          await submitExecutionSourceScriptDecodingInit({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            category,
            catalogue: {
              policyId: harness.contracts.fraudProofCatalogue.policyId,
              spendingScriptAddress:
                harness.contracts.fraudProofCatalogue.spendingScriptAddress,
              root: catalogue.root,
            },
            signer: harness.proverSigner,
            fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            fraudulentHeaderHash: setup.headerHash,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
      );
      const cancel = async (threadOutRef: string, stepIndex: number) => {
        const result = await submitExecutionSourceScriptDecodingCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: references[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        expect(result.txHash).toMatch(/^[0-9a-f]{64}$/u);
      };
      if (cancelAt === "step01") {
        await cancel(init.nextThreadOutRef, 0);
        return;
      }
      const step01 = await measured(`step01-${direction}`, async () => {
        if (direction === "accepted") {
          if (block.txInclusion === null)
            throw new Error("accepted malformed fixture omitted inclusion");
          return await submitExecutionSourceScriptDecodingStep01Accepted({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: init.nextThreadOutRef,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: block.txInclusion,
            header,
            executionIndex: 0n,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          });
        }
        const membership = await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey,
        });
        return await submitExecutionSourceScriptDecodingStep01Forced({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: init.nextThreadOutRef,
          header,
          membership,
          executionIndex: 0n,
          referenceScriptUtxo: references[0]!,
        });
      });
      if (cancelAt === "step02") {
        await cancel(step01.nextThreadOutRef, 1);
        return;
      }
      await expect(
        submitExecutionSourceScriptDecodingStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          evidence,
          authentication: {
            ...authentication.authentication,
            script_hash: "ff".repeat(28),
          },
          referenceScriptUtxo: references[1]!,
        }),
      ).rejects.toThrow();
      const step02 = await measured(
        "step02-authenticate",
        async () =>
          await submitExecutionSourceScriptDecodingStep02({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step01.nextThreadOutRef,
            evidence,
            authentication: authentication.authentication,
            referenceScriptUtxo: references[1]!,
          }),
      );
      if (cancelAt === "step03") {
        await cancel(step02.nextThreadOutRef, 2);
        return;
      }
      expect(step02.nextThreadOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
      const step03 = await measured(
        "step03-open-item",
        async () =>
          await submitExecutionSourceScriptDecodingStep03({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step02.nextThreadOutRef,
            evidence,
            referenceScriptUtxo: references[2]!,
          }),
      );
      if (cancelAt === "scan") {
        await cancel(step03.nextThreadOutRef, 3);
        return;
      }
      let threadOutRef = step03.nextThreadOutRef;
      let scanCount = 0;
      for (;;) {
        const scan = await measured(
          `step04-scan-${scanCount.toString()}`,
          async () =>
            await submitExecutionSourceScriptDecodingStep04({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              evidence,
              referenceScriptUtxo: references[3]!,
            }),
        );
        scanCount += 1;
        threadOutRef = scan.nextThreadOutRef;
        if (scan.closed) break;
        if (scanCount > 16)
          throw new Error("execution source scan did not close");
      }
      expect(scanCount).toBeGreaterThan(0);
      const final = await measured(
        "step05-mint",
        async () =>
          await submitExecutionSourceScriptDecodingStep05({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef,
            evidence,
            referenceScriptUtxo: references[4]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
      );
      expect(final.txHash).toMatch(/^[0-9a-f]{64}$/u);
      const proof = (
        await harness.proverLucid.utxosAt(
          harness.contracts.fraudProof.spendingScriptAddress,
        )
      ).find(({ txHash }) => txHash === final.txHash);
      expect(proof).toBeDefined();
      if (proof === undefined)
        throw new Error("execution source proof output absent");
      const removalReferences = await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      });
      const baseDeployment = buildRemovalDeploymentInfo(
        harness.contracts,
        catalogue,
        { removalReferenceScripts: removalReferences.published },
      );
      const deploymentInfo = {
        ...baseDeployment,
        contracts: {
          ...baseDeployment.contracts,
          fraudProofExecutionSourceScriptDecoding: {
            scriptHash: applied[0].spendingScriptHash,
            contract: {
              type: applied[0].spendingScript.type,
              cborHex: applied[0].spendingScript.script,
            },
          },
          ...Object.fromEntries(
            applied.slice(1).map((step, index) => [
              `fraudProofExecutionSourceScriptDecodingStep0${(
                index + 2
              ).toString()}`,
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
      vi.setSystemTime(harness.emulator.now());
      const removal = await measured(
        "remove",
        async () =>
          await submitRemoveFraudulentBlock({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            deploymentInfo,
            network,
            signer: harness.proverSigner,
            fraudCategory: "executionSourceScriptDecoding",
            fraudulentHeaderHash: setup.headerHash,
            awaitConfirmation: true,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: {
              acquire: async () => ({
                token: "execution-source-lifecycle",
                source: "emulator",
                renew: async () => undefined,
                release: async () => undefined,
                fail: async () => undefined,
              }),
            },
            validFrom:
              BigInt(harness.emulator.now()) > 120_000n
                ? BigInt(harness.emulator.now()) - 120_000n
                : 0n,
            validTo: BigInt(harness.emulator.now()) + 300_000n,
          }),
      );
      expect(removal.txHash).toMatch(/^[0-9a-f]{64}$/u);
      expect(lifecycleRows.map(({ label }) => label)).toEqual([
        "init",
        `step01-${direction}`,
        "step02-authenticate",
        "step03-open-item",
        "step04-scan-0",
        "step05-mint",
        "remove",
      ]);
      for (const { measurement } of lifecycleRows) {
        expect(measurement.l1ByteMargin).toBeGreaterThan(0);
        expect(measurement.executionMemory).toBeGreaterThan(0n);
        expect(measurement.executionSteps).toBeGreaterThan(0n);
      }
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          JSON.stringify(lifecycleRows, (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          ),
        );
    },
    120_000,
  );
});
