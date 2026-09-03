import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardAddressWitnessItem,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  MIDGARD_CONSENSUS_PROFILE,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  EventKeySchema,
  forcedVerdictSubject,
  missingSignatureVkeyHash,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "@al-ft/midgard-validation";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
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
import { reconstructExecutionNativeScriptPurposes } from "../src/execution-native-script-invalid/canonical-reconstruction-v1.js";
import { applyExecutionNativeScriptInvalidScripts } from "../src/execution-native-script-invalid/contracts-v1.js";
import { prepareExecutionNativeScriptInvalidEvidence } from "../src/execution-native-script-invalid/family-v1.js";
import {
  submitExecutionNativeScriptInvalidAcceptedFinishInline,
  submitExecutionNativeScriptInvalidAcceptedFinishPurpose,
  submitExecutionNativeScriptInvalidAcceptedFinishReceivePass,
  submitExecutionNativeScriptInvalidAcceptedFinishSpends,
  submitExecutionNativeScriptInvalidAcceptedInit,
  submitExecutionNativeScriptInvalidAcceptedInlineSource,
  submitExecutionNativeScriptInvalidAcceptedMint,
  submitExecutionNativeScriptInvalidAcceptedObserver,
  submitExecutionNativeScriptInvalidAcceptedReceive,
  submitExecutionNativeScriptInvalidAcceptedReferenceSource,
  submitExecutionNativeScriptInvalidAcceptedSpend,
} from "../src/execution-native-script-invalid/submit-accepted-reconstruction-v1.js";
import {
  submitExecutionNativeScriptInvalidAcceptedCancel,
  submitExecutionNativeScriptInvalidCancel,
} from "../src/execution-native-script-invalid/submit-cancel-v1.js";
import { submitExecutionNativeScriptInvalidInit } from "../src/execution-native-script-invalid/submit-init-v1.js";
import {
  submitExecutionNativeScriptInvalidStep01Accepted,
  submitExecutionNativeScriptInvalidStep01Forced,
} from "../src/execution-native-script-invalid/submit-step-01-v1.js";
import { submitExecutionNativeScriptInvalidStep02 } from "../src/execution-native-script-invalid/submit-step-02-v1.js";
import { submitExecutionNativeScriptInvalidStep03 } from "../src/execution-native-script-invalid/submit-step-03-v1.js";
import { submitExecutionNativeScriptInvalidStep04StartSignerScan } from "../src/execution-native-script-invalid/submit-step-04-v1.js";
import { submitExecutionNativeScriptInvalidStep05 } from "../src/execution-native-script-invalid/submit-step-05-v1.js";
import { submitExecutionNativeScriptInvalidStep06 } from "../src/execution-native-script-invalid/submit-step-06-v1.js";
import { buildExecutionSourceMachineAuthentication } from "../src/execution-source-script-decoding/machine-authentication-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

describe("executionNativeScriptInvalid genuine machine fixture", () => {
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
    {
      direction: "accepted" as "forced" | "accepted",
      sourceOrigin: "inline" as "inline" | "reference",
      acceptedPurpose: "mint" as const,
      cancelAt: null,
    },
    {
      direction: "forced" as "forced" | "accepted",
      sourceOrigin: "inline" as "inline" | "reference",
      cancelAt: null,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "reference" as const,
      cancelAt: null,
    },
    {
      direction: "accepted" as const,
      sourceOrigin: "reference" as const,
      acceptedPurpose: "mint" as const,
      cancelAt: null,
    },
    ...(["spend", "observer", "receive"] as const).map((acceptedPurpose) => ({
      direction: "accepted" as const,
      sourceOrigin: "inline" as const,
      acceptedPurpose,
      cancelAt: null,
    })),
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "step01" as const,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "step02" as const,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "step03" as const,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "scan" as const,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "step05" as const,
    },
    {
      direction: "forced" as const,
      sourceOrigin: "inline" as const,
      cancelAt: "step06" as const,
    },
    ...[
      "acceptedDispatch",
      "acceptedInit",
      "acceptedSpend",
      "acceptedMint",
      "acceptedObserver",
      "acceptedReceive",
    ].map((cancelAt) => ({
      direction: "accepted" as const,
      sourceOrigin: "inline" as const,
      acceptedPurpose: "receive" as const,
      cancelAt,
    })),
    {
      direction: "accepted" as const,
      sourceOrigin: "reference" as const,
      acceptedPurpose: "mint" as const,
      cancelAt: "acceptedInline" as const,
    },
  ])(
    "runs $direction/$sourceOrigin/$acceptedPurpose lifecycle (cancel=$cancelAt)",
    async ({ direction, sourceOrigin, acceptedPurpose = "mint", cancelAt }) => {
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
      const applied = applyExecutionNativeScriptInvalidScripts({
        blueprint: harness.realBlueprint,
        network,
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: addressData,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
        fieldPreimageCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
      });
      const contracts = {
        steps: applied,
        acceptedPrelude: applied.acceptedPrelude,
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
        executionNativeScriptInvalid: {
          ...harness.contracts.fraudProofs.executionNativeScriptInvalid,
          spendingScriptHash: applied[0].spendingScriptHash,
        },
      });
      const category = catalogue.categories.executionNativeScriptInvalid;
      expect(category.categoryId).toBe("00000032");
      expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
      const spent = outRefFromByte(0x72);
      const witnessKeys = Array.from({ length: 32 }, () =>
        CML.PrivateKey.generate_ed25519(),
      );
      const script =
        direction === "accepted"
          ? nativeScriptWitness({
              type: "all",
              scripts: Array.from({ length: 31 }, (_, index) => ({
                type: "sig" as const,
                keyHash: Buffer.alloc(28, 0x80 + index),
              })),
            })
          : nativeScriptWitness({ type: "all", scripts: [] });
      const scriptAddress = Buffer.concat([
        Buffer.from([0x70]),
        Buffer.from(hashScriptWitness(script), "hex"),
      ]);
      const spentOutput = makeOutput(
        FUNDED_OUTPUT_LOVELACE,
        direction === "accepted" && acceptedPurpose === "spend"
          ? scriptAddress
          : undefined,
      );
      const canonicalScriptItem = encodeMidgardVersionedScript(script);
      const scriptItem = canonicalScriptItem;
      const reference = outRefFromByte(0x74, 7n);
      const referenceOutput = encodeMidgardTxOutput({
        ...decodeMidgardTxOutput(spentOutput),
        script_ref: script,
      });
      const policyId = Buffer.from(hashScriptWitness(script), "hex");
      const assetName = Buffer.from("31", "hex");
      const output = makeOutput(
        FUNDED_OUTPUT_LOVELACE,
        direction === "accepted" && acceptedPurpose === "receive"
          ? protectMidgardAddress(scriptAddress)
          : undefined,
        acceptedPurpose === "mint" || direction === "forced"
          ? new Map([
              [
                policyId.toString("hex"),
                new Map([[assetName.toString("hex"), 1n]]),
              ],
            ])
          : undefined,
      );
      let transaction = makeNativeTx({
        version: 1n,
        spendInputs:
          direction === "forced" || acceptedPurpose === "spend" ? [spent] : [],
        referenceInputs: sourceOrigin === "reference" ? [reference] : [],
        outputs: [output],
        requiredObserverItems:
          direction === "accepted" && acceptedPurpose === "observer"
            ? [policyId]
            : [],
        scriptWitnesses: sourceOrigin === "inline" ? [script] : [],
        mintPreimageCbor:
          acceptedPurpose === "mint" || direction === "forced"
            ? makeMintPreimageCbor(
                new Map([[policyId, new Map([[assetName, 1n]])]]),
              )
            : undefined,
      });
      const signedBodyHash = computeMidgardNativeTxId({
        version: transaction.tx.version,
        transactionBody: transaction.tx.compact.transactionBody,
        transactionWitnessSetHash: Buffer.alloc(32),
        validity: transaction.tx.validity,
      });
      const initialAddressWitnessItems = decodeMidgardFieldPreimage(
        transaction.tx.witnessSet.addrTxWitsPreimageCbor,
      );
      const addressWitnessItems = [
        ...initialAddressWitnessItems.map((item) => ({
          signerHash: Buffer.from(
            missingSignatureVkeyHash(
              Buffer.from(
                decodeMidgardAddressWitnessItem(item).verificationKey,
              ).toString("hex"),
            ),
            "hex",
          ),
          item,
        })),
        ...witnessKeys.map((key) => ({
          signerHash: Buffer.from(
            missingSignatureVkeyHash(
              Buffer.from(key.to_public().to_raw_bytes()).toString("hex"),
            ),
            "hex",
          ),
          item: Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              key,
            ).to_cbor_bytes(),
          ),
        })),
      ]
        .sort((left, right) =>
          Buffer.compare(left.signerHash, right.signerHash),
        )
        .map(({ item }) => item);
      transaction = encodeRecomputedNativeTx({
        ...transaction.tx,
        witnessSet: {
          ...transaction.tx.witnessSet,
          addrTxWitsPreimageCbor: encodeCbor(addressWitnessItems),
        },
      });
      const compactWitnessSet = deriveMidgardNativeTxWitnessSetCompact(
        transaction.tx.witnessSet,
      );
      const witnessSet = {
        addr_tx_wits_hash: Buffer.from(
          compactWitnessSet.addrTxWitsHash,
        ).toString("hex"),
        script_tx_wits_hash: Buffer.from(
          compactWitnessSet.scriptTxWitsHash,
        ).toString("hex"),
        redeemer_tx_wits_hash: Buffer.from(
          compactWitnessSet.redeemerTxWitsHash,
        ).toString("hex"),
      };
      const nativeTx =
        direction === "forced"
          ? decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor)
          : transaction.tx;
      const nativeTxCompactCbor = encodeMidgardNativeTxCompact(
        nativeTx.compact,
      ).toString("hex");
      const allOperations = [
        ...(direction === "forced" || acceptedPurpose === "spend"
          ? [{ type: "delete" as const, key: spent }]
          : []),
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ];
      const operations = direction === "forced" ? allOperations : [];
      const mutations = await buildValidationMachineLedgerMutationSteps({
        initialEntries: [
          ...(direction === "forced" || acceptedPurpose === "spend"
            ? [{ outRef: spent, output: spentOutput }]
            : []),
          ...(sourceOrigin === "reference"
            ? [{ outRef: reference, output: referenceOutput }]
            : []),
        ],
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
      const trace =
        direction === "forced"
          ? await Effect.runPromise(
              buildDeterministicValidationMachineTrace({
                consensusProfile: MIDGARD_CONSENSUS_PROFILE,
                eventKeyCbor: Buffer.from(
                  Data.to(eventKey as never, EventKeySchema),
                  "hex",
                ),
                sourceKind: "forced",
                committedForcedVerdict: "rejected",
                blockEndTimeMs: 1_750_000_001_000,
                expectedNetworkId: 0n,
                minFeeA: 0n,
                minFeeB: 0n,
                blockSlot: 0n,
                transactionId: transaction.txId,
                canonicalTransactionCbor: transaction.txCbor,
                priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
                postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
                ledgerWitnessEntries: [
                  { outRef: spent, output: spentOutput },
                  ...(sourceOrigin === "reference"
                    ? [{ outRef: reference, output: referenceOutput }]
                    : []),
                ],
                expectedLedgerOps: operations,
                ledgerMutationSteps: mutations,
                expectedVerdict: "accepted",
                expectedRejectionCode: null,
              }),
            )
          : undefined;
      const authentication =
        trace !== undefined
          ? await buildExecutionSourceMachineAuthentication({
              trace,
              eventKey,
              claimedVerdict: "rejected",
              claimedRejectionCode: "E_NATIVE_SCRIPT_INVALID",
            })
          : undefined;
      const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
      const startTime = BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      );
      const reason = {
        ExecutionNativeScriptFalse: { execution_index: 0n },
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
      const needsPriorLedger =
        direction === "forced" ||
        sourceOrigin === "reference" ||
        acceptedPurpose === "spend";
      const canonicalLedgerHeader = needsPriorLedger
        ? {
            ...block.header,
            prevUtxosRoot: mutations[0]!.preRoot.toString("hex"),
            utxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
          }
        : block.header;
      let header =
        authentication === undefined
          ? canonicalLedgerHeader
          : {
              ...canonicalLedgerHeader,
              validationTracesRoot: authentication.validationTracesRoot,
              validationTraceCount: authentication.validationTraceCount,
            };
      const setup = needsPriorLedger
        ? await (async () => {
            const firstHeader = {
              ...block.header,
              utxosRoot: mutations[0]!.preRoot.toString("hex"),
            };
            const first = await submitSetupTx({
              lucid: harness.funderLucid,
              contracts: harness.contracts,
              nonceUtxo: harness.nonceUtxo,
              catalogue,
              header: firstHeader,
            });
            header = {
              ...header,
              prevHeaderHash: first.headerHash,
              startTime: firstHeader.endTime,
              endTime: firstHeader.endTime + 120_000n,
            };
            const second = await submitSecondHeaderTx({
              lucid: harness.funderLucid,
              contracts: harness.contracts,
              header,
            });
            return {
              ...first,
              fraudulentBlockOutRef: second.blockOutRef,
              headerHash: second.headerHash,
            };
          })()
        : await submitSetupTx({
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
              label: `execution-native-lifecycle-${index.toString()}`,
            })
          ).utxo,
        );
      const acceptedReferences: UTxO[] = [];
      for (const [index, step] of applied.acceptedPrelude.entries())
        acceptedReferences.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `execution-native-accepted-${index.toString()}`,
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
        ).catch((cause: unknown) => {
          throw new Error(
            `executionNativeScriptInvalid ${label} failed: ${String(cause)}`,
          );
        });
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
      const forcedAuxiliary = trace?.witnesses.find(
        ({ phase, auxiliary }) =>
          phase === "nativeScripts" &&
          auxiliary?.kind === "nativeExecutionDescriptor",
      )?.auxiliary;
      const canonicalPurpose =
        direction === "accepted"
          ? reconstructExecutionNativeScriptPurposes({
              canonicalTransactionCbor: transaction.txCbor,
              resolvedOutputsByOutRef: new Map([
                ...(acceptedPurpose === "spend"
                  ? ([[spent.toString("hex"), spentOutput]] as const)
                  : []),
                ...(sourceOrigin === "reference"
                  ? ([[reference.toString("hex"), referenceOutput]] as const)
                  : []),
              ]),
            }).purposes[0]
          : undefined;
      if (
        direction === "forced" &&
        forcedAuxiliary?.kind !== "nativeExecutionDescriptor"
      )
        throw new Error("missing forced native descriptor");
      if (direction === "accepted" && canonicalPurpose === undefined)
        throw new Error("missing accepted canonical purpose");
      const sourceLeaf = (() => {
        if (canonicalPurpose !== undefined) {
          const source = canonicalPurpose.source;
          return source.originKind === 0
            ? hashMidgardInlineScriptSourceLeaf({
                sourceIndex: BigInt(source.sourceIndex),
                scriptLanguageTag: 0,
                scriptHash: Buffer.from(source.scriptHash, "hex"),
                scriptTotalLength: source.totalLength,
                itemCommitment: Buffer.from(source.itemCommitment, "hex"),
              })
            : hashMidgardReferenceScriptSourceLeaf({
                sourceKey: Buffer.from(source.sourceKey, "hex"),
                scriptLanguageTag: 0,
                scriptHash: Buffer.from(source.scriptHash, "hex"),
                scriptTotalLength: source.totalLength,
                itemCommitment: Buffer.from(source.itemCommitment, "hex"),
              });
        }
        if (forcedAuxiliary?.kind !== "nativeExecutionDescriptor")
          throw new Error("missing forced native descriptor");
        return forcedAuxiliary.source.originKind === "inline"
          ? hashMidgardInlineScriptSourceLeaf({
              sourceIndex: BigInt(forcedAuxiliary.source.sourceIndex),
              scriptLanguageTag: 0,
              scriptHash: forcedAuxiliary.purpose.scriptHash,
              scriptTotalLength: forcedAuxiliary.source.scriptTotalLength,
              itemCommitment: forcedAuxiliary.source.scriptItemCommitment,
            })
          : hashMidgardReferenceScriptSourceLeaf({
              sourceKey: forcedAuxiliary.source.sourceKey,
              scriptLanguageTag: 0,
              scriptHash: forcedAuxiliary.purpose.scriptHash,
              scriptTotalLength: forcedAuxiliary.source.scriptTotalLength,
              itemCommitment: forcedAuxiliary.source.scriptItemCommitment,
            });
      })();
      const evidence = prepareExecutionNativeScriptInvalidEvidence({
        finding: { subject, executionIndex: 0 },
        transactionIdHex: block.nativeTxId,
        sourceDescriptorHashHex: sourceLeaf.toString("hex"),
        scriptItemHashHex: computeHash32(
          decodeMidgardVersionedScript(scriptItem).scriptBytes,
        ).toString("hex"),
        scriptBytes: decodeMidgardVersionedScript(scriptItem).scriptBytes,
        addressWitnessItems,
        validityIntervalStart: transaction.tx.body.validityIntervalStart,
        validityIntervalEnd: transaction.tx.body.validityIntervalEnd,
      });
      const init = await measured(
        "init",
        async () =>
          await submitExecutionNativeScriptInvalidInit({
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
        const result = await submitExecutionNativeScriptInvalidCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: references[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        expect(result.txHash).toMatch(/^[0-9a-f]{64}$/u);
        const restarted = await submitExecutionNativeScriptInvalidInit({
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
        });
        expect(restarted.nextThreadOutRef).not.toBe(threadOutRef);
      };
      const cancelAccepted = async (
        threadOutRef: string,
        stepIndex: number,
      ) => {
        const result = await submitExecutionNativeScriptInvalidAcceptedCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: acceptedReferences[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        expect(result.txHash).toMatch(/^[0-9a-f]{64}$/u);
        const restarted = await submitExecutionNativeScriptInvalidInit({
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
        });
        expect(restarted.nextThreadOutRef).not.toBe(threadOutRef);
      };
      if (cancelAt === "step01") {
        await cancel(init.nextThreadOutRef, 0);
        return;
      }
      const step01 = await measured(`step01-${direction}`, async () => {
        if (direction === "accepted") {
          if (block.txInclusion === null)
            throw new Error("accepted malformed fixture omitted inclusion");
          return await submitExecutionNativeScriptInvalidStep01Accepted({
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
        return await submitExecutionNativeScriptInvalidStep01Forced({
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
      let step02: { txHash: string; nextThreadOutRef: string };
      if (direction === "accepted") {
        if (cancelAt === "acceptedDispatch") {
          await cancelAccepted(step01.nextThreadOutRef, 0);
          return;
        }
        const acceptedInit = await measured(
          "accepted-reconstruction-init",
          async () =>
            await submitExecutionNativeScriptInvalidAcceptedInit({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef: step01.nextThreadOutRef,
              referenceScriptUtxo: acceptedReferences[0]!,
            }),
        );
        if (cancelAt === "acceptedInit") {
          await cancelAccepted(acceptedInit.nextThreadOutRef, 1);
          return;
        }
        let acceptedThreadOutRef = acceptedInit.nextThreadOutRef;
        if (acceptedPurpose === "spend") {
          const material = buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: decodeMidgardSpendInputItem(spent).outputIndex,
            outputCbor: spentOutput,
          });
          const store = new Store(undefined);
          await store.ready();
          const trie = new Trie(store);
          await trie.insert(spent, material.descriptorCbor);
          if (sourceOrigin === "reference") {
            const referenceMaterial = buildCanonicalMidgardLedgerOutputMaterial(
              {
                outputIndex: decodeMidgardSpendInputItem(reference).outputIndex,
                outputCbor: referenceOutput,
              },
            );
            await trie.insert(reference, referenceMaterial.descriptorCbor);
          }
          const proof = await trie.prove(spent);
          const selected = await measured(
            "accepted-spend-prefix",
            async () =>
              await submitExecutionNativeScriptInvalidAcceptedSpend({
                lucid: harness.proverLucid,
                network,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: acceptedThreadOutRef,
                nativeTxCompactCbor,
                spendInputsPreimageCbor:
                  transaction.tx.body.spendInputsPreimageCbor.toString("hex"),
                descriptorCbor: material.descriptorCbor.toString("hex"),
                membershipProof: Data.from(
                  proof.toCBOR().toString("hex"),
                  Proof,
                ),
                membershipProofCbor: proof.toCBOR().toString("hex"),
                membershipReferenceScriptUtxo:
                  harness.witnessReferenceScripts.phasMembershipWithdraw!,
                referenceScriptUtxo: acceptedReferences[1]!,
              }),
          );
          expect(selected.selected).toBe(true);
          acceptedThreadOutRef = selected.nextThreadOutRef;
        } else {
          const acceptedSpend = await measured(
            "accepted-spend-finish",
            async () =>
              await submitExecutionNativeScriptInvalidAcceptedFinishSpends({
                lucid: harness.proverLucid,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: acceptedThreadOutRef,
                nativeTxCompactCbor,
                spendInputsPreimageCbor:
                  transaction.tx.body.spendInputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: acceptedReferences[1]!,
              }),
          );
          acceptedThreadOutRef = acceptedSpend.nextThreadOutRef;
          if (cancelAt === "acceptedSpend") {
            await cancelAccepted(acceptedThreadOutRef, 2);
            return;
          }
          if (acceptedPurpose === "mint") {
            const acceptedMint = await measured(
              "accepted-mint-prefix",
              async () =>
                await submitExecutionNativeScriptInvalidAcceptedMint({
                  lucid: harness.proverLucid,
                  contracts,
                  categoryId: category.categoryId,
                  signer: harness.proverSigner,
                  threadOutRef: acceptedThreadOutRef,
                  nativeTxCompactCbor,
                  mintPreimageCbor:
                    transaction.tx.body.mintPreimageCbor.toString("hex"),
                  referenceScriptUtxo: acceptedReferences[2]!,
                }),
            );
            expect(acceptedMint.selected).toBe(true);
            acceptedThreadOutRef = acceptedMint.nextThreadOutRef;
          } else {
            const finishMint = await measured(
              "accepted-mint-finish",
              async () =>
                await submitExecutionNativeScriptInvalidAcceptedFinishPurpose({
                  lucid: harness.proverLucid,
                  contracts,
                  categoryId: category.categoryId,
                  signer: harness.proverSigner,
                  threadOutRef: acceptedThreadOutRef,
                  phase: "mint",
                  nativeTxCompactCbor,
                  fieldPreimageCbor:
                    transaction.tx.body.mintPreimageCbor.toString("hex"),
                  referenceScriptUtxo: acceptedReferences[2]!,
                }),
            );
            acceptedThreadOutRef = finishMint.nextThreadOutRef;
            if (cancelAt === "acceptedObserver") {
              await cancelAccepted(acceptedThreadOutRef, 3);
              return;
            }
            if (acceptedPurpose === "observer") {
              const observer = await measured(
                "accepted-observer-prefix",
                async () =>
                  await submitExecutionNativeScriptInvalidAcceptedObserver({
                    lucid: harness.proverLucid,
                    contracts,
                    categoryId: category.categoryId,
                    signer: harness.proverSigner,
                    threadOutRef: acceptedThreadOutRef,
                    nativeTxCompactCbor,
                    observersPreimageCbor:
                      transaction.tx.body.requiredObserversPreimageCbor.toString(
                        "hex",
                      ),
                    referenceScriptUtxo: acceptedReferences[3]!,
                  }),
              );
              expect(observer.selected).toBe(true);
              acceptedThreadOutRef = observer.nextThreadOutRef;
            } else {
              const finishObserver = await measured(
                "accepted-observer-finish",
                async () =>
                  await submitExecutionNativeScriptInvalidAcceptedFinishPurpose(
                    {
                      lucid: harness.proverLucid,
                      contracts,
                      categoryId: category.categoryId,
                      signer: harness.proverSigner,
                      threadOutRef: acceptedThreadOutRef,
                      phase: "observer",
                      nativeTxCompactCbor,
                      fieldPreimageCbor:
                        transaction.tx.body.requiredObserversPreimageCbor.toString(
                          "hex",
                        ),
                      referenceScriptUtxo: acceptedReferences[3]!,
                    },
                  ),
              );
              acceptedThreadOutRef = finishObserver.nextThreadOutRef;
              if (cancelAt === "acceptedReceive") {
                await cancelAccepted(acceptedThreadOutRef, 4);
                return;
              }
              const receiveScan = await measured(
                "accepted-receive-scan",
                async () =>
                  await submitExecutionNativeScriptInvalidAcceptedReceive({
                    lucid: harness.proverLucid,
                    contracts,
                    categoryId: category.categoryId,
                    signer: harness.proverSigner,
                    threadOutRef: acceptedThreadOutRef,
                    nativeTxCompactCbor,
                    outputsPreimageCbor:
                      transaction.tx.body.outputsPreimageCbor.toString("hex"),
                    referenceScriptUtxo: acceptedReferences[4]!,
                  }),
              );
              const receive = await measured(
                "accepted-receive-finish",
                async () =>
                  await submitExecutionNativeScriptInvalidAcceptedFinishReceivePass(
                    {
                      lucid: harness.proverLucid,
                      contracts,
                      categoryId: category.categoryId,
                      signer: harness.proverSigner,
                      threadOutRef: receiveScan.nextThreadOutRef,
                      nativeTxCompactCbor,
                      outputsPreimageCbor:
                        transaction.tx.body.outputsPreimageCbor.toString("hex"),
                      referenceScriptUtxo: acceptedReferences[4]!,
                    },
                  ),
              );
              expect(receive.selected).toBe(true);
              acceptedThreadOutRef = receive.nextThreadOutRef;
            }
          }
        }
        if (cancelAt === "acceptedMint") {
          await cancelAccepted(acceptedThreadOutRef, 5);
          return;
        }
        const acceptedSource = await measured(
          sourceOrigin === "inline"
            ? "accepted-inline-source"
            : "accepted-reference-source",
          async () => {
            if (sourceOrigin === "inline")
              return await submitExecutionNativeScriptInvalidAcceptedInlineSource(
                {
                  lucid: harness.proverLucid,
                  contracts,
                  categoryId: category.categoryId,
                  signer: harness.proverSigner,
                  threadOutRef: acceptedThreadOutRef,
                  nativeTxCompactCbor,
                  witnessSet,
                  scriptsPreimageCbor:
                    transaction.tx.witnessSet.scriptTxWitsPreimageCbor.toString(
                      "hex",
                    ),
                  referenceScriptUtxo: acceptedReferences[5]!,
                },
              );
            const finished =
              await submitExecutionNativeScriptInvalidAcceptedFinishInline({
                lucid: harness.proverLucid,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: acceptedThreadOutRef,
                nativeTxCompactCbor,
                witnessSet,
                scriptsPreimageCbor:
                  transaction.tx.witnessSet.scriptTxWitsPreimageCbor.toString(
                    "hex",
                  ),
                referenceScriptUtxo: acceptedReferences[5]!,
              });
            if (cancelAt === "acceptedInline") {
              await cancelAccepted(finished.nextThreadOutRef, 6);
              return { ...finished, selected: false };
            }
            const material = buildCanonicalMidgardLedgerOutputMaterial({
              outputIndex: decodeMidgardSpendInputItem(reference).outputIndex,
              outputCbor: referenceOutput,
            });
            const store = new Store(undefined);
            await store.ready();
            const trie = new Trie(store);
            if (acceptedPurpose === "spend") {
              const spentMaterial = buildCanonicalMidgardLedgerOutputMaterial({
                outputIndex: decodeMidgardSpendInputItem(spent).outputIndex,
                outputCbor: spentOutput,
              });
              await trie.insert(spent, spentMaterial.descriptorCbor);
            }
            await trie.insert(reference, material.descriptorCbor);
            const proof = await trie.prove(reference);
            return await submitExecutionNativeScriptInvalidAcceptedReferenceSource(
              {
                lucid: harness.proverLucid,
                network,
                contracts,
                categoryId: category.categoryId,
                signer: harness.proverSigner,
                threadOutRef: finished.nextThreadOutRef,
                nativeTxCompactCbor,
                referenceInputsPreimageCbor:
                  transaction.tx.body.referenceInputsPreimageCbor.toString(
                    "hex",
                  ),
                descriptorCbor: material.descriptorCbor.toString("hex"),
                membershipProof: Data.from(
                  proof.toCBOR().toString("hex"),
                  Proof,
                ),
                membershipProofCbor: proof.toCBOR().toString("hex"),
                membershipReferenceScriptUtxo:
                  harness.witnessReferenceScripts.phasMembershipWithdraw!,
                referenceScriptUtxo: acceptedReferences[6]!,
              },
            );
          },
        );
        if (cancelAt === "acceptedInline") return;
        expect(acceptedSource.selected).toBe(true);
        step02 = acceptedSource;
      } else {
        if (authentication === undefined)
          throw new Error("forced fixture omitted machine authentication");
        await expect(
          submitExecutionNativeScriptInvalidStep02({
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
        await expect(
          submitExecutionNativeScriptInvalidStep02({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step01.nextThreadOutRef,
            evidence,
            authentication: {
              ...authentication.authentication,
              purpose_index: authentication.authentication.purpose_index + 1n,
            },
            referenceScriptUtxo: references[1]!,
          }),
        ).rejects.toThrow();
        await expect(
          submitExecutionNativeScriptInvalidStep02({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step01.nextThreadOutRef,
            evidence,
            authentication: {
              ...authentication.authentication,
              source_key: "01",
            },
            referenceScriptUtxo: references[1]!,
          }),
        ).rejects.toThrow();
        step02 = await measured(
          "step02-authenticate",
          async () =>
            await submitExecutionNativeScriptInvalidStep02({
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
      }
      if (cancelAt === "step03") {
        await cancel(step02.nextThreadOutRef, 2);
        return;
      }
      expect(step02.nextThreadOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
      const step03 = await measured(
        "step03-open-item",
        async () =>
          await submitExecutionNativeScriptInvalidStep03({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step02.nextThreadOutRef,
            scriptItemCbor: scriptItem,
            referenceScriptUtxo: references[2]!,
          }),
      );
      if (cancelAt === "scan") {
        await cancel(step03.nextThreadOutRef, 3);
        return;
      }
      const start = await measured(
        "step04-signer-start",
        async () =>
          await submitExecutionNativeScriptInvalidStep04StartSignerScan({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step03.nextThreadOutRef,
            nativeTxCompactCbor,
            witnessSet,
            scriptItemCbor: scriptItem,
            addressWitnessItems,
            referenceScriptUtxo: references[3]!,
          }),
      );
      if (cancelAt === "step05") {
        await cancel(start.nextThreadOutRef, 4);
        return;
      }
      let threadOutRef = start.nextThreadOutRef;
      let signerAction = "";
      do {
        const scan = await measured(
          "step05-signer-scan",
          async () =>
            await submitExecutionNativeScriptInvalidStep05({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              nativeTxCompactCbor,
              witnessSet,
              addressWitnessItems,
              referenceScriptUtxo: references[4]!,
            }),
        );
        signerAction = scan.action;
        threadOutRef = scan.nextThreadOutRef;
      } while (signerAction !== "FinalizeSignerScan");
      if (cancelAt === "step06") {
        await cancel(threadOutRef, 5);
        return;
      }
      let final: Awaited<
        ReturnType<typeof submitExecutionNativeScriptInvalidStep06>
      >;
      for (;;) {
        final = await measured(
          "step06-evaluate",
          async () =>
            await submitExecutionNativeScriptInvalidStep06({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              scriptItemCbor: scriptItem,
              addressWitnessItems,
              referenceScriptUtxo: references[5]!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
        );
        if ("fraudProofUnit" in final) break;
        threadOutRef = final.nextThreadOutRef;
      }
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
          fraudProofExecutionNativeScriptInvalid: {
            scriptHash: applied[0].spendingScriptHash,
            contract: {
              type: applied[0].spendingScript.type,
              cborHex: applied[0].spendingScript.script,
            },
          },
          ...Object.fromEntries(
            applied.slice(1).map((step, index) => [
              `fraudProofExecutionNativeScriptInvalidStep0${(
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
          ...Object.fromEntries(
            applied.acceptedPrelude.map((step, index) => [
              [
                "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
                "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
                "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
                "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
                "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
                "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
                "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
              ][index]!,
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
            fraudCategory: "executionNativeScriptInvalid",
            fraudulentHeaderHash: setup.headerHash,
            awaitConfirmation: true,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: {
              acquire: async () => ({
                token: "execution-native-lifecycle",
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
      expect(lifecycleRows.map(({ label }) => label)).toEqual(
        expect.arrayContaining([
          "init",
          `step01-${direction}`,
          ...(direction === "forced"
            ? ["step02-authenticate"]
            : [
                "accepted-reconstruction-init",
                ...(acceptedPurpose === "spend"
                  ? ["accepted-spend-prefix"]
                  : ["accepted-spend-finish"]),
                ...(acceptedPurpose === "mint"
                  ? ["accepted-mint-prefix"]
                  : acceptedPurpose === "observer"
                    ? ["accepted-mint-finish", "accepted-observer-prefix"]
                    : acceptedPurpose === "receive"
                      ? [
                          "accepted-mint-finish",
                          "accepted-observer-finish",
                          "accepted-receive-scan",
                          "accepted-receive-finish",
                        ]
                      : []),
                sourceOrigin === "inline"
                  ? "accepted-inline-source"
                  : "accepted-reference-source",
              ]),
          "step03-open-item",
          "step04-signer-start",
          "step05-signer-scan",
          "step06-evaluate",
          "remove",
        ]),
      );
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
    300_000,
  );
});
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
