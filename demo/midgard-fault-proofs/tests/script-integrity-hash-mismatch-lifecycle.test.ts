import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
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
import { afterAll, describe, expect, it } from "vitest";

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
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  type PublishedProofChunk,
  publishProofChunks,
} from "../src/publish-proof-chunks.js";
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
import { IntegrityStep05RedeemerSchema } from "../src/script-integrity-hash-mismatch/schemas.js";
import {
  submitScriptIntegrityHashMismatchCancel,
  submitScriptIntegrityHashMismatchStep01Accepted,
  submitScriptIntegrityHashMismatchStep01Forced,
  submitScriptIntegrityHashMismatchStep02,
  submitScriptIntegrityHashMismatchStep03,
  submitScriptIntegrityHashMismatchStep04,
  submitScriptIntegrityHashMismatchStep05,
} from "../src/script-integrity-hash-mismatch/submit.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
  registerChunkedVerifyRewardAccount,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

const network = "Custom" as const;
const fitMeasurements: VanRossemFitMeasurement[] = [];
let completedCases = 0;
afterAll(async () => {
  const blueprintBytes = readFileSync(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "scriptIntegrityHashMismatch",
    blueprintSha256: createHash("sha256").update(blueprintBytes).digest("hex"),
    compilerVersion: JSON.parse(blueprintBytes.toString()).preamble.compiler
      .version,
    measurements: fitMeasurements,
  });
  if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
    expect(completedCases).toBe(16);
    await writeVanRossemFitLedger(
      fileURLToPath(
        new URL(
          "../../../docs/fault-proofs/size-plans/script-integrity-hash-mismatch-v1-fit-ledger.json",
          import.meta.url,
        ),
      ),
      ledger,
    );
  }
});

describe("scriptIntegrityHashMismatch concrete Lucid lifecycle", () => {
  it.each(
    ([0, 1, 2, 3] as const).flatMap((bitmap) =>
      (["accepted", "forced"] as const).flatMap((direction) =>
        [false, true].map((honest) => ({ bitmap, direction, honest })),
      ),
    ),
  )(
    "$direction bitmap $bitmap honest=$honest: authenticated lifecycle and terminal polarity",
    async ({ direction, bitmap, honest }) => {
      const integrityMismatch = direction === "accepted" ? !honest : honest;
      const harness = await makeFaultProofEmulatorHarness({
        registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
        contractOptions: {
          alwaysFraudProofCatalogue: true,
          realScriptIntegrityHashMismatch: true,
        },
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
      const registered =
        harness.contracts.fraudProofContracts.scriptIntegrityHashMismatch;
      const catalogue = harness.catalogue;
      const category = catalogue.categories.scriptIntegrityHashMismatch!;
      expectRegisteredChainParity({ registered, applied, category });
      const contracts: ScriptIntegrityHashMismatchContracts = {
        steps: familyStepsFromRegisteredChain(
          registered.steps,
          applied.map((step) => step.blueprintTitle),
        ),
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      };
      expect(category.categoryId).toBe(
        SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID,
      );

      const spent = outRefFromByte(0x61);
      const privateKey = CML.PrivateKey.from_normal_bytes(
        new Uint8Array(32).fill(7),
      );
      const script = plutusV3ScriptWitness(
        Buffer.from(
          "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
          "hex",
        ),
      );
      const midgardScript = {
        language: "MidgardV1" as const,
        scriptBytes: script.scriptBytes,
      };
      const scripts =
        bitmap === 0
          ? []
          : bitmap === 1
            ? [script]
            : bitmap === 2
              ? [midgardScript]
              : [script, midgardScript];
      const spentOutput =
        bitmap === 0
          ? makeOutput(
              FUNDED_OUTPUT_LOVELACE,
              Buffer.from(
                CML.EnterpriseAddress.new(
                  0,
                  CML.Credential.new_pub_key(privateKey.to_public().hash()),
                )
                  .to_address()
                  .to_raw_bytes(),
              ),
            )
          : makeProtectedScriptOutput(
              hashScriptWitness(scripts[0]!),
              FUNDED_OUTPUT_LOVELACE,
            );
      const producedOutput =
        bitmap === 3
          ? makeProtectedScriptOutput(
              hashScriptWitness(midgardScript),
              FUNDED_OUTPUT_LOVELACE,
            )
          : makeOutput(FUNDED_OUTPUT_LOVELACE);
      const correct = makeNativeTx({
        spendInputs: [spent],
        outputs: [producedOutput],
        scriptWitnesses: scripts,
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          ...(bitmap === 0
            ? []
            : [{ tag: MidgardRedeemerTag.Spend, index: 0n }]),
          ...(bitmap === 3
            ? [{ tag: MidgardRedeemerTag.Receiving, index: 0n }]
            : []),
        ]),
        scriptLanguages: [
          ...(bitmap & 1 ? ["PlutusV3" as const] : []),
          ...(bitmap & 2 ? ["MidgardV1" as const] : []),
        ],
        privateKey,
      });
      const malformedBody = {
        ...correct.tx.body,
        scriptIntegrityHash: integrityMismatch
          ? Buffer.alloc(32, 0xff)
          : correct.tx.body.scriptIntegrityHash,
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
          { type: "delete", key: spent },
          buildValidationMachineLedgerInsertOp({
            key: outRefFromTxId(malformed.txId),
            outputCbor: producedOutput,
          }),
        ],
      });
      const orderKey = { transactionId: "cd".repeat(32), outputIndex: 0n };
      const eventKey =
        direction === "accepted"
          ? ({
              L2TransactionEventKey: { tx_id: malformed.txId.toString("hex") },
            } as const)
          : ({ ForcedTransactionEventKey: { tx_order_id: orderKey } } as const);
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(
            Data.to(eventKey as never, SDK.EventKeySchema as never),
            "hex",
          ),
          sourceKind: direction === "accepted" ? "normal" : "forced",
          ...(direction === "forced" ? {} : {}),
          blockEndTimeMs: 1_800_000_000_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
          transactionId: malformed.txId,
          canonicalTransactionCbor:
            direction === "forced"
              ? encodeMidgardForcedTxCanonical(
                  decodeMidgardNativeTxFullFromCanonicalCbor(malformed.txCbor),
                )
              : malformed.txCbor,
          programMaterialSidecarCbor:
            bitmap === 0
              ? undefined
              : Buffer.from(
                  "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
                  "hex",
                ),
          priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
          postUtxosRoot: (integrityMismatch
            ? mutations[0]!.preRoot
            : mutations.at(-1)!.postRoot
          ).toString("hex"),
          ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
          expectedLedgerOps: integrityMismatch
            ? []
            : [
                { type: "delete", key: spent },
                buildValidationMachineLedgerInsertOp({
                  key: outRefFromTxId(malformed.txId),
                  outputCbor: producedOutput,
                }),
              ],
          ledgerMutationSteps: integrityMismatch ? [] : mutations,
          expectedVerdict: integrityMismatch ? "rejected" : "accepted",
          expectedRejectionCode: integrityMismatch
            ? "E_INVALID_FIELD_TYPE"
            : null,
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
        direction === "accepted" ? "accepted" : "rejected",
        direction === "accepted"
          ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
          : hashMidgardValidationRejectionCode("E_INVALID_FIELD_TYPE"),
      );
      const maximum = bitmap === 3 && !honest;
      let selectedProof = claimedTree.proofs[stateIndex]!;
      let selectedDescriptor = claimedTree.descriptor;
      if (maximum) {
        const siblings = Array.from({ length: 32 }, (_, index) =>
          computeHash32(Buffer.from(`integrity maximum sibling ${index}`)),
        );
        let root = computeHash32(
          Buffer.concat([
            Buffer.from("MidgardValidationTraceLeafV1"),
            selectedProof.stateHash,
          ]),
        );
        let index = selectedProof.stateIndex;
        for (const sibling of siblings) {
          root = computeHash32(
            Buffer.concat([
              Buffer.from("MidgardValidationTraceBranchV1"),
              ...(index % 2 === 0 ? [root, sibling] : [sibling, root]),
            ]),
          );
          index = Math.floor(index / 2);
        }
        selectedProof = { ...selectedProof, siblings };
        selectedDescriptor = {
          ...selectedDescriptor,
          traceRoot: root,
          stepCount: MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
        };
        expect(
          verifyMidgardValidationTraceProof({
            descriptor: selectedDescriptor,
            proof: selectedProof,
          }),
        ).toBe(true);
      }
      const descriptor: SDK.ValidationTraceDescriptor = {
        schema_version: BigInt(selectedDescriptor.schemaVersion),
        machine_version: BigInt(selectedDescriptor.machineVersion),
        trace_root: selectedDescriptor.traceRoot.toString("hex"),
        step_count: BigInt(selectedDescriptor.stepCount),
        initial_state_hash: selectedDescriptor.initialStateHash.toString("hex"),
        terminal_state_hash:
          selectedDescriptor.terminalStateHash.toString("hex"),
        verdict: direction === "accepted" ? "Accepted" : "Rejected",
        rejection_code_hash:
          selectedDescriptor.rejectionCodeHash.toString("hex"),
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
        trace_proof: SDK.validationTraceProofDataFromCore(selectedProof),
        phase: 10n,
        program_counter: BigInt(witness.programCounter),
        witness_cbor: witness.cbor.toString("hex"),
        auxiliary,
      };
      const retainedKey: SDK.RetainedValidationWitnessKey = {
        event_key: eventKey,
        execution_index: -1n,
      };
      let authentication =
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
      expect(retained.program_counter).not.toBe(3n);
      await expect(
        buildScriptIntegrityStageThreeAuthenticationFromRetainedDa({
          eventKey,
          authenticatedValidationTraceEntries: [
            { key: eventKeyCbor, value: descriptorCbor },
          ],
          retainedValidationWitnessEntries: [
            {
              key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
              value: SDK.encodeRetainedValidationWitness({
                ...retained,
                program_counter: 3n,
              }),
            },
          ],
          expectedValidationTracesRoot: traceRoot.root,
        }),
      ).rejects.toThrow(/stage\/auxiliary changed/u);
      if (maximum) {
        const deep = syntheticDeepMembershipProof({
          key: eventKeyCbor,
          value: descriptorCbor,
          branchLevels: 64,
        });
        const count = traceRoot.count;
        const root = await commitCountedRoot({
          domain: SDK.ROOT_DOMAINS.validationTraces,
          phasRoot: deep.transactionsPhasRoot,
          count,
        });
        authentication = {
          ...authentication,
          validationTracesRoot: root,
          validationTraceCount: count,
          traceMembership: {
            ...authentication.traceMembership,
            root,
            phas_root: deep.transactionsPhasRoot,
            count,
            proof: Data.from(deep.proofCbor, SDK.Proof),
          },
        };
      }
      const evidence = prepareScriptIntegrityHashMismatchEvidence({
        finding: {
          subject:
            direction === "accepted"
              ? SDK.acceptedVerdictSubject(malformed.txId.toString("hex"))
              : SDK.forcedVerdictSubject({
                  transactionId: malformed.txId.toString("hex"),
                  sourceKey: orderKey,
                  rejectionReason: "ScriptIntegrityHashMismatch",
                }),
        },
        scriptIntegrityHash: authentication.scriptIntegrityHash,
        redeemerWitnessHash: authentication.redeemerWitnessHash,
        selectedLanguageBitmap: Number(
          authentication.control.language_bitmap,
        ) as 0 | 1 | 2 | 3,
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
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: malformed.tx }
            : {
                kind: "forced",
                nativeTx: malformed.tx,
                orderKey,
                verdict: {
                  ForcedTxInvalid: { reason: "ScriptIntegrityHashMismatch" },
                },
              },
      });
      const header = {
        ...block.header,
        validationTracesRoot: authentication.validationTracesRoot,
        validationTraceCount: authentication.validationTraceCount,
      };
      let forcedMembership =
        direction === "forced"
          ? await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey: {
                ForcedTransactionEventKey: { tx_order_id: orderKey },
              },
            })
          : undefined;
      let txInclusion = block.txInclusion;
      if (maximum && txInclusion !== null) {
        const deep = syntheticDeepMembershipProof({
          key: Buffer.from(txInclusion.nativeTxId, "hex"),
          value: Buffer.from(txInclusion.l2TransactionSourceCbor, "hex"),
          branchLevels: 64,
        });
        txInclusion = {
          ...txInclusion,
          transactionsPhasRoot: deep.transactionsPhasRoot,
          txMembershipProof: Data.from(deep.proofCbor, SDK.Proof),
          txMembershipProofCbor: deep.proofCbor,
        };
        header.transactionsRoot = await commitCountedRoot({
          domain: SDK.ROOT_DOMAINS.transactionsV1,
          phasRoot: deep.transactionsPhasRoot,
          count: header.l2TransactionCount,
        });
      }
      if (maximum && forcedMembership !== undefined) {
        const deep = syntheticDeepMembershipProof({
          key: Buffer.from(
            Data.to<SDK.OutputReference>(
              forcedMembership.key,
              SDK.OutputReferenceSchema as never,
            ),
            "hex",
          ),
          value: Buffer.from(
            Data.to<SDK.ForcedInclusionTxV1>(
              forcedMembership.value,
              SDK.ForcedInclusionTxV1Schema as never,
            ),
            "hex",
          ),
          branchLevels: 64,
        });
        const root = await commitCountedRoot({
          domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
          phasRoot: deep.transactionsPhasRoot,
          count: forcedMembership.count,
        });
        forcedMembership = {
          ...forcedMembership,
          root,
          phas_root: deep.transactionsPhasRoot,
          proof: Data.from(deep.proofCbor, SDK.Proof),
        };
        header.forcedTransactionsRoot = root;
      }
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
      if (direction === "accepted" && txInclusion === null)
        throw new Error("accepted inclusion absent");
      const prefix = `${direction}-bitmap-${bitmap}-honest-${honest}`;
      const shape = maximum
        ? "64-branch descriptor MPF and 32-level validation trace; dual language set"
        : `language bitmap ${bitmap}; ${honest ? "honest terminal refusal" : "contradiction"}`;
      const record = (
        label: string,
        kind: VanRossemFitMeasurement["kind"],
        measurement: {
          completeSignedBytes: number;
          executionMemory: bigint;
          executionSteps: bigint;
        },
      ) => {
        fitMeasurements.push({
          name: `${prefix}-${label}`,
          kind,
          maximumShape: shape,
          signedBytes: measurement.completeSignedBytes,
          memoryUnits: measurement.executionMemory,
          cpuUnits: measurement.executionSteps,
        });
      };
      const references: UTxO[] = [];
      for (const [index, step] of contracts.steps.entries()) {
        const publication = await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `script-integrity-mismatch-${index}`,
        });
        references.push(publication.utxo);
        record(
          `reference-${index}`,
          "publication",
          publication.publicationMeasurement,
        );
      }

      const ledger: {
        label: string;
        bytes: number;
        memory: string;
        cpu: string;
        margin: number;
      }[] = [];
      const measured = async <T>(
        label: string,
        operation: () => Promise<T>,
      ) => {
        const capture = await captureEmulatorSubmission(
          harness.emulator,
          operation,
        );
        record(label, "lifecycle", capture.measurement);
        ledger.push({
          label,
          bytes: capture.measurement.completeSignedBytes,
          memory: capture.measurement.executionMemory.toString(),
          cpu: capture.measurement.executionSteps.toString(),
          margin: capture.measurement.l1ByteMargin,
        });
        return capture.result;
      };
      let publishedProofChunks: readonly PublishedProofChunk[] = [];
      if (maximum && txInclusion !== null) {
        const publication = await captureEmulatorSubmission(
          harness.emulator,
          () =>
            publishProofChunks({
              lucid: harness.proverLucid,
              network,
              signer: harness.proverSigner,
              proofCbor: txInclusion!.txMembershipProofCbor,
            }),
        );
        publishedProofChunks = publication.result.chunks;
        record("source-proof-chunks", "publication", publication.measurement);
      }
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
        direction === "forced"
          ? submitScriptIntegrityHashMismatchStep01Forced({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              header,
              membership: forcedMembership!,
              evidence,
              referenceScriptUtxo: references[0]!,
            })
          : submitScriptIntegrityHashMismatchStep01Accepted({
              lucid: harness.proverLucid,
              blueprint: harness.realBlueprint,
              network,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
              txInclusion: txInclusion!,
              publishedProofChunks,
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
      for (let index = 0; index < (honest ? 0 : 5); index += 1) {
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
      if (maximum) {
        const corrupted = [
          {
            ...authentication,
            machineState: {
              ...authentication.machineState,
              transaction_id: "ee".repeat(32),
            },
          },
          {
            ...authentication,
            machineState: {
              ...authentication.machineState,
              event_key_hash: "ee".repeat(32),
            },
          },
          {
            ...authentication,
            machineState: {
              ...authentication.machineState,
              work_root: "ee".repeat(32),
            },
          },
          {
            ...authentication,
            traceProof: {
              ...authentication.traceProof,
              siblings: authentication.traceProof.siblings.map((hash, index) =>
                index === 0 ? "ee".repeat(32) : hash,
              ),
            },
          },
          {
            ...authentication,
            traceMembership: {
              ...authentication.traceMembership,
              key: { L2TransactionEventKey: { tx_id: "ee".repeat(32) } },
            },
          },
        ];
        for (const altered of corrupted) {
          await expect(
            submitScriptIntegrityHashMismatchStep02({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef: one.nextThreadOutRef,
              evidence,
              authentication: altered,
              referenceScriptUtxo: references[1]!,
            }),
          ).rejects.toThrow();
        }
      }
      const two = await measured("step02", () => step02(one.nextThreadOutRef));
      const three = await measured("step03", () =>
        step03(two.nextThreadOutRef),
      );
      const fourA = await measured("step04-0", () =>
        step04(three.nextThreadOutRef),
      );
      expect(fourA.terminal).toBe(false);
      const fourB = await measured("step04-1", () =>
        step04(fourA.nextThreadOutRef),
      );
      expect(fourB.terminal).toBe(true);
      if (honest) {
        const terminalArgs = {
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: fourB.nextThreadOutRef,
          evidence,
          referenceScriptUtxo: references[4]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        };
        await expect(
          submitScriptIntegrityHashMismatchStep05(terminalArgs),
        ).rejects.toThrow(/not the retained contradiction/u);
        const thread = await requireLinearFaultThreadUtxo({
          ...terminalArgs,
          family: "script-integrity-hash-mismatch",
          stepIndex: 4,
        });
        // Bypass the SDK predicate: the actual validator must refuse an honest claim.
        await expect(
          submitLinearFaultFinalize({
            ...terminalArgs,
            ...thread,
            family: "script-integrity-hash-mismatch",
            stepIndex: 4,
            step: contracts.steps[4],
            computationThread: contracts.computationThread,
            fraudProof: contracts.fraudProof,
            spendRedeemerSchema: IntegrityStep05RedeemerSchema,
            buildFamilyArgs: (layout) => ({
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            }),
            awaitConfirmation: true,
          }),
        ).rejects.toThrow();
        await measured("cancel-honest-terminal", () =>
          cancel(fourB.nextThreadOutRef, 4),
        );
        completedCases += 1;
        return;
      }
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
      const deploymentInfo = baseDeployment;
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
        expect(BigInt(row.memory), row.label).toBeLessThan(16_500_000n);
        expect(BigInt(row.cpu), row.label).toBeGreaterThan(0n);
        expect(BigInt(row.cpu), row.label).toBeLessThan(10_000_000_000n);
      });
      completedCases += 1;
      console.info(
        `[script-integrity-hash-mismatch-lifecycle-ledger] ${JSON.stringify(ledger)}`,
      );
    },
    600_000,
  );
});
