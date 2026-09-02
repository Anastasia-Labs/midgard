import {
  buildMidgardValidationMerkleMembershipV1,
  computeHash28,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeCbor,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  AddressData,
  addressDataFromBech32,
  encodeRetainedValidationWitnessKeyV1,
  encodeRetainedValidationWitnessV1,
  EventKeySchema,
  forcedVerdictSubjectV1,
  type RetainedValidationWitnessKeyV1,
  type RetainedValidationWitnessV1,
  ValidationAuxiliaryWitnessV1Schema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  validationAuxiliaryWitnessDataV1,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE_V1,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidenceV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { applyExecutionSourceScriptDecodingScriptsV1 } from "../src/execution-source-script-decoding/contracts-v1.js";
import { prepareExecutionSourceScriptDecodingEvidenceV1 } from "../src/execution-source-script-decoding/family-v1.js";
import { buildExecutionSourceMachineAuthenticationV1 } from "../src/execution-source-script-decoding/machine-authentication-v1.js";
import { prepareProductionExecutionSourceScriptDecodingArtifactV1 } from "../src/execution-source-script-decoding/production-replay-v1.js";
import { submitExecutionSourceScriptDecodingCancelV1 } from "../src/execution-source-script-decoding/submit-cancel-v1.js";
import { submitExecutionSourceScriptDecodingInitV1 } from "../src/execution-source-script-decoding/submit-init-v1.js";
import {
  submitExecutionSourceScriptDecodingStep01AcceptedV1,
  submitExecutionSourceScriptDecodingStep01ForcedV1,
} from "../src/execution-source-script-decoding/submit-step-01-v1.js";
import { submitExecutionSourceScriptDecodingStep02V1 } from "../src/execution-source-script-decoding/submit-step-02-v1.js";
import { submitExecutionSourceScriptDecodingStep03V1 } from "../src/execution-source-script-decoding/submit-step-03-v1.js";
import { submitExecutionSourceScriptDecodingStep04V1 } from "../src/execution-source-script-decoding/submit-step-04-v1.js";
import { submitExecutionSourceScriptDecodingStep05V1 } from "../src/execution-source-script-decoding/submit-step-05-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { buildDecodingBlockFixtureV1 } from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

describe("executionSourceScriptDecoding genuine machine fixture", () => {
  it("reconstructs the authenticated nativeExecutionScan state and proof", async () => {
    const spent = outRefFromByte(0x71);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
    const script = nativeScriptWitness({ type: "all", scripts: [] });
    const policyId = Buffer.from(hashScriptWitness(script), "hex");
    const assetName = Buffer.from("31", "hex");
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE_V1,
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
      buildValidationMachineLedgerInsertOpV1({
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
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
    const authenticated = await buildExecutionSourceMachineAuthenticationV1({
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
      const harness = await makeFaultProofEmulatorHarnessV1({
        contractOptions: { alwaysFraudProofCatalogue: true },
      });
      const addressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(
          Effect.map((address) => Data.from(Data.to(address, AddressData))),
        ),
      );
      const applied = applyExecutionSourceScriptDecodingScriptsV1({
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
      const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
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
        FUNDED_OUTPUT_LOVELACE_V1,
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
          ? decodeMidgardNativeTxFullV1FromCanonicalCbor(transaction.txCbor)
          : transaction.tx;
      const allOperations = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOpV1({
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
          consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
      const authentication = await buildExecutionSourceMachineAuthenticationV1({
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
      const block = await buildDecodingBlockFixtureV1({
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
            ValidationTraceDescriptorV1Schema,
          ),
        ] as const;
        const retainedKey: RetainedValidationWitnessKeyV1 = {
          event_key: eventKey,
          execution_index: 0n,
        };
        const retainedValue: RetainedValidationWitnessV1 = {
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
              validationAuxiliaryWitnessDataV1(retained.auxiliary) as never,
            ),
            ValidationAuxiliaryWitnessV1Schema,
          ) as unknown as RetainedValidationWitnessV1["auxiliary"],
        };
        const productionArtifact =
          await prepareProductionExecutionSourceScriptDecodingArtifactV1({
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
                      encodeRetainedValidationWitnessKeyV1(
                        retainedKey,
                      ).toString("hex"),
                      encodeRetainedValidationWitnessV1(retainedValue).toString(
                        "hex",
                      ),
                    ],
                  ],
                },
              },
            },
            transactions: [],
          } as unknown as CanonicalBlockEvidenceV1);
        expect(productionArtifact.forcedMembership).toBeDefined();
        expect(productionArtifact.acceptedInclusion).toBeUndefined();
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
          ? forcedVerdictSubjectV1({
              transactionId: block.nativeTxId,
              sourceKey: orderKey,
              rejectionReason: reason,
            })
          : acceptedVerdictSubjectV1(block.nativeTxId);
      const auxiliary = trace.witnesses.find(
        ({ phase, auxiliary }) =>
          phase === "nativeScripts" &&
          auxiliary?.kind === "nativeExecutionDescriptor",
      )?.auxiliary;
      if (auxiliary?.kind !== "nativeExecutionDescriptor")
        throw new Error("missing native descriptor");
      const purposeLeaf = hashMidgardScriptPurposeLeafV1({
        purposeKind: auxiliary.purpose.purposeKind,
        purposeIndex: auxiliary.purpose.purposeIndex,
        scriptHash: auxiliary.purpose.scriptHash,
        subject: auxiliary.purpose.subject,
      });
      const sourceLeaf = hashMidgardInlineScriptSourceLeafV1({
        sourceIndex: BigInt(auxiliary.source.sourceIndex),
        scriptLanguageTag: 0,
        scriptHash: auxiliary.purpose.scriptHash,
        scriptTotalLength: auxiliary.source.scriptTotalLength,
        itemCommitment: auxiliary.source.scriptItemCommitment,
      });
      const executionLeaf = hashMidgardScriptExecutionLeafV1({
        languageTag: 0,
        purposeLeaf,
        sourceLeaf,
        redeemerLeaf: auxiliary.redeemerLeaf,
      });
      const evidence = prepareExecutionSourceScriptDecodingEvidenceV1({
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
          purposeMembership: buildMidgardValidationMerkleMembershipV1(
            [purposeLeaf],
            0,
          ),
          sourceMembership: buildMidgardValidationMerkleMembershipV1(
            [sourceLeaf],
            0,
          ),
          executionMembership: buildMidgardValidationMerkleMembershipV1(
            [executionLeaf],
            0,
          ),
        },
      });
      const init = await measured(
        "init",
        async () =>
          await submitExecutionSourceScriptDecodingInitV1({
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
        const result = await submitExecutionSourceScriptDecodingCancelV1({
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
          return await submitExecutionSourceScriptDecodingStep01AcceptedV1({
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
        return await submitExecutionSourceScriptDecodingStep01ForcedV1({
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
        submitExecutionSourceScriptDecodingStep02V1({
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
          await submitExecutionSourceScriptDecodingStep02V1({
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
          await submitExecutionSourceScriptDecodingStep03V1({
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
            await submitExecutionSourceScriptDecodingStep04V1({
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
          await submitExecutionSourceScriptDecodingStep05V1({
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
