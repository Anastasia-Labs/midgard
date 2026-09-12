import { createHash } from "node:crypto";
import { mkdtempSync } from "node:fs";
import { rm } from "node:fs/promises";
import { readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationMerkleMembership,
  buildMidgardValidationTraceTree,
  commitMidgardValidationMerkleFrontier,
  computeHash32,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardLedgerOutputCommitment,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardLedgerOutputAssetLeaf,
  hashMidgardMintAssetLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  hashMidgardValidationWorkWitness,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  prependMidgardInputResolutionSchedule,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  createDistinctAssetAccumulationActuator,
  type DistinctAssetAccumulationActuatorAction,
} from "../src/distinct-asset-accumulation-limit/actuator.js";
import { detectDistinctAssetAccumulationCanonicalViolations } from "../src/distinct-asset-accumulation-limit/authenticated-replay.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
  prepareDistinctAssetAccumulationEvidence,
} from "../src/distinct-asset-accumulation-limit/family.js";
import {
  captureDistinctAssetActionWithProofCarriage,
  createDistinctAssetProofPrerequisite,
  DISTINCT_ASSET_PROOF_PUBLICATION,
  distinctAssetPreparedJournalArtifact,
  requireDistinctAssetPreparedArtifact,
} from "../src/distinct-asset-accumulation-limit/proof-carriage.js";
import { buildDistinctAssetAuthenticationFromRetainedDa } from "../src/distinct-asset-accumulation-limit/retained-value-and-mint.js";
import { submitDistinctAssetAccumulationCancel } from "../src/distinct-asset-accumulation-limit/submit-cancel.js";
import { submitDistinctAssetAccumulationFold } from "../src/distinct-asset-accumulation-limit/submit-fold.js";
import {
  submitDistinctAssetAccumulationStep01Accepted,
  submitDistinctAssetAccumulationStep01Forced,
} from "../src/distinct-asset-accumulation-limit/submit-step-01.js";
import { submitDistinctAssetAccumulationStep02 } from "../src/distinct-asset-accumulation-limit/submit-step-02.js";
import { submitDistinctAssetAccumulationStep06 } from "../src/distinct-asset-accumulation-limit/submit-step-06.js";
import {
  admitDistinctAssetWorkflowArtifact,
  distinctAssetWorkflowArtifact,
} from "../src/distinct-asset-accumulation-limit/v1.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  type JournalJsonObject,
} from "../src/workflow/journal.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { expectProofFit } from "./support/emulator/proof-fit.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildDecodingBlockFixture,
  decodingSubjectTransaction,
} from "./support/native-script-decoding-emulator.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  publishRemovalReferenceScripts,
  registerChunkedVerifyRewardAccount,
} from "./support/submit-init-emulator-shared.js";

const journalDirectory = mkdtempSync(
  join(tmpdir(), "distinct-assets-journal-"),
);
const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  await rm(journalDirectory, { recursive: true, force: true });
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  const ledger = buildVanRossemFitLedger({
    category: "distinctAssetAccumulationLimit:00000035:testnet",
    blueprintSha256: createHash("sha256")
      .update(await readFile(realBlueprintPath))
      .digest("hex"),
    compilerVersion: "aiken v1.1.23+5adf783",
    measurements,
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/distinct-asset-accumulation-limit-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});
const network = "Custom" as const;

describe("distinctAssetAccumulationLimit concrete Lucid lifecycle", () => {
  it.each(
    ["input", "output", "mint"].flatMap((kind) =>
      [false, true].flatMap((forced) => [
        { kind, forced, maximum: false, honest: false },
        { kind, forced, maximum: true, honest: false },
        { kind, forced, maximum: false, honest: true },
      ]),
    ),
  )(
    "proves $kind forced=$forced maximum=$maximum honest=$honest crossing/boundary",
    async ({ kind, forced, maximum, honest }) => {
      const harness = await makeFaultProofEmulatorHarness({
        registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
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
        DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
      );

      const baseNativeTx = decodingSubjectTransaction({});
      const nativeTx = maximum
        ? materializeMidgardNativeTxFromCanonical({
            ...baseNativeTx,
            body: {
              ...baseNativeTx.body,
              fee: 0xffffffffffffffffn,
              validityIntervalStart: 0x7fffffffffffffffn,
              validityIntervalEnd: 0x7fffffffffffffffn,
            },
          })
        : baseNativeTx;
      const orderKey = { transactionId: "71".repeat(32), outputIndex: 0n };
      const selectedIndex = maximum ? 16_383 : 0;
      const crossing = forced === honest;
      const reason: SDK.RejectionReason =
        kind === "input"
          ? {
              InputAssetAccumulationLimit: {
                input_index: 0n,
                asset_index: BigInt(selectedIndex),
              },
            }
          : kind === "output"
            ? {
                OutputAssetAccumulationLimit: {
                  output_index: 0n,
                  asset_index: BigInt(selectedIndex),
                },
              }
            : {
                MintAssetAccumulationLimit: {
                  mint_index: BigInt(selectedIndex),
                },
              };
      const coordinate =
        kind === "input"
          ? { kind: "input" as const, inputIndex: 0, assetIndex: selectedIndex }
          : kind === "output"
            ? {
                kind: "output" as const,
                outputIndex: 0,
                assetIndex: selectedIndex,
              }
            : { kind: "mint" as const, mintIndex: selectedIndex };
      const block = await buildDecodingBlockFixture({
        operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
        startTime: BigInt(
          alignUnixTimeToEmulatorSlotBoundary(
            harness.funderLucid,
            harness.emulator.now() + 120_000,
          ) - 1,
        ),
        priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        decoyTransactionCount: maximum && !forced ? 128 : 0,
        subject: forced
          ? {
              kind: "forced",
              nativeTx,
              orderKey,
              verdict: { ForcedTxInvalid: { reason } },
            }
          : { kind: "normal", nativeTx },
      });
      const transactionId = block.nativeTxId;
      const eventKey = forced
        ? { ForcedTransactionEventKey: { tx_order_id: orderKey } }
        : { L2TransactionEventKey: { tx_id: transactionId } };
      const eventKeyCbor = Buffer.from(
        Data.to(eventKey as never, SDK.EventKeySchema as never),
        "hex",
      );
      const policyId = Buffer.alloc(28, 0x5a);
      const assetName = Buffer.alloc(32, 0xa5);
      const quantity = maximum ? 0x7fffffffffffffffn : 1n;
      const mintLeaf = hashMidgardMintAssetLeaf({
        policyId,
        assetName,
        quantity,
      });
      const bytes32 = () => Buffer.alloc(32);
      const assetLeaf = hashMidgardLedgerOutputAssetLeaf({
        policyId,
        assetName,
        quantity,
      });
      const assetProof = buildMidgardValidationMerkleMembership(
        Array.from({ length: selectedIndex + 1 }, () => assetLeaf),
        selectedIndex,
      );
      const mintProof = buildMidgardValidationMerkleMembership(
        Array.from({ length: selectedIndex + 1 }, () => mintLeaf),
        selectedIndex,
      );
      const outputCbor = encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x99)]),
        value: { lovelace: 5_000_000n, assets: new Map() },
      });
      const baseDescriptor = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: 0,
        outputCbor,
      }).descriptor;
      const descriptorCbor = encodeMidgardLedgerOutputCommitment({
        ...baseDescriptor,
        ...(maximum
          ? {
              address: Buffer.concat([
                Buffer.from([0x00]),
                Buffer.alloc(56, 0x99),
              ]),
              lovelace: 0xffffffffffffffffn,
              totalLength: 32768,
              cardanoValueSize: 5000,
              referenceScriptLanguage: 128 as const,
              referenceScriptHash: Buffer.alloc(28, 0x77),
              referenceScriptTotalLength: 32768,
              referenceScriptItemCommitment: Buffer.alloc(32, 0x88),
            }
          : {}),
        assetCount: assetProof.frontier.count,
        assetFrontierCommitment: commitMidgardValidationMerkleFrontier(
          assetProof.frontier,
        ),
      });
      const replayKey = Buffer.alloc(38, 0x14);
      const schedule = prependMidgardInputResolutionSchedule({
        sourceKind: "spend",
        key: replayKey,
        nextHash: bytes32(),
      });
      const densePeaks = maximum
        ? Array.from({ length: 15 }, (_, height) => [
            BigInt(height),
            Buffer.alloc(32, height + 1),
          ])
        : [];
      const sourceFields = forced
        ? deriveMidgardForcedTxProofSource(nativeTx)
        : deriveMidgardNativeTxProofSourceFromCanonicalCbor(
            encodeMidgardNativeTxCanonical(nativeTx),
          );
      // Conservative control envelope: all nonselected frontiers have every
      // height below the 32,768-byte field ceiling, simultaneously. Hidden
      // subtrees are operator commitments; only the selected fold is opened.
      const nativeControlCbor = encodeCbor([
        maximum ? sourceFields.compactCbor : Buffer.alloc(0),
        maximum ? sourceFields.witnessSetCompactCbor : Buffer.alloc(0),
        maximum
          ? encodeCbor(Array.from({ length: 9 }, () => 32768n))
          : Buffer.alloc(0),
        maximum
          ? encodeCbor([
              1n,
              Buffer.alloc(64, 0x61),
              ...Array.from({ length: 5 }, () => 0xffffffffffffffffn),
            ])
          : Buffer.alloc(0),
        1n,
        bytes32(),
        1n,
        densePeaks,
        maximum ? 32767n : 0n,
        bytes32(),
        maximum ? 32767n : 0n,
        densePeaks,
        maximum ? 32767n : 0n,
        densePeaks,
        maximum ? 32767n : 0n,
        densePeaks,
        1n,
        densePeaks,
        densePeaks,
        BigInt(mintProof.frontier.count),
        mintProof.frontier.peaks.map((p) => [BigInt(p.height), p.hash]),
        maximum ? 32767n : 0n,
        densePeaks,
        0n,
        0n,
        bytes32(),
      ]);
      const deltaProof = MpfProof.fromJSON(
        Buffer.concat([policyId, assetName]),
        encodeCbor(kind === "output" ? -quantity : quantity),
        Array.from({ length: maximum ? 16 : 0 }, (_, index) => ({
          type: "branch",
          skip: 0,
          neighbors: Buffer.alloc(128, index + 1).toString("hex"),
        })),
      );
      const assetRoot =
        deltaProof.verify(false)?.toString("hex") ?? SDK.EMPTY_MERKLE_TREE_ROOT;
      const accumulatorCbor = encodeCbor([
        0n,
        Buffer.from(assetRoot, "hex"),
        crossing ? 16_384n : 16_383n,
        0n,
      ]);
      const witnessCbor = encodeCbor([
        nativeControlCbor,
        kind === "input" ? 2n : kind === "output" ? 3n : 4n,
        bytes32(),
        0n,
        BigInt(selectedIndex + 1),
        computeHash32(descriptorCbor),
        bytes32(),
        schedule,
        0n,
        BigInt(selectedIndex + 1),
        BigInt(selectedIndex),
        accumulatorCbor,
      ]);
      const rejectionHash = forced
        ? hashMidgardValidationRejectionCode("E_ASSET_COUNT")
        : MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH;
      const state: MidgardValidationMachineState = {
        machineVersion: 1,
        eventKeyHash: hashMidgardValidationEventKey(eventKeyCbor),
        transactionId: Buffer.from(transactionId, "hex"),
        transactionCommitment: Buffer.alloc(32, 0x11),
        validationContextHash: Buffer.alloc(32, 0x22),
        sourceKind: forced ? "forced" : "normal",
        priorLedgerRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
        phase: "valueAndMint",
        programCounter: 0,
        workRoot: hashMidgardValidationWorkWitness({
          phase: "valueAndMint",
          programCounter: 0,
          witnessCbor,
        }),
        executionCpu: 0n,
        executionMemory: 0n,
        verdict: forced ? "rejected" : "accepted",
        rejectionCodeHash: rejectionHash,
        ledgerDeltaRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
      };
      let trace = buildMidgardValidationTraceTree(
        [hashMidgardValidationMachineState(state)],
        forced ? "rejected" : "accepted",
        rejectionHash,
      );
      if (maximum) {
        let node = computeHash32(
          Buffer.concat([
            Buffer.from("MidgardValidationTraceLeafV1"),
            trace.proofs[0]!.stateHash,
          ]),
        );
        const siblings: Buffer[] = [];
        for (let depth = 0; depth < 32; depth++) {
          siblings.push(node);
          node = computeHash32(
            Buffer.concat([
              Buffer.from("MidgardValidationTraceBranchV1"),
              node,
              node,
            ]),
          );
        }
        trace = {
          ...trace,
          descriptor: {
            ...trace.descriptor,
            traceRoot: node,
            stepCount: 0xffff_ffff,
          },
          proofs: [{ ...trace.proofs[0]!, siblings }],
        };
      }
      const descriptor: SDK.ValidationTraceDescriptor = {
        schema_version: BigInt(trace.descriptor.schemaVersion),
        machine_version: BigInt(trace.descriptor.machineVersion),
        trace_root: trace.descriptor.traceRoot.toString("hex"),
        step_count: BigInt(trace.descriptor.stepCount),
        initial_state_hash: trace.descriptor.initialStateHash.toString("hex"),
        terminal_state_hash: trace.descriptor.terminalStateHash.toString("hex"),
        verdict: forced ? "Rejected" : "Accepted",
        rejection_code_hash: trace.descriptor.rejectionCodeHash.toString("hex"),
      };
      const traceDescriptorCbor = Buffer.from(
        Data.to(
          descriptor as never,
          SDK.ValidationTraceDescriptorSchema as never,
        ),
        "hex",
      );
      const validationEntries = [
        { key: eventKeyCbor, value: traceDescriptorCbor },
        ...block.reconstruction.payload.block_body.validation_traces
          .filter(([key]) => key !== eventKeyCbor.toString("hex"))
          .map(([key, value]) => ({
            key: Buffer.from(key, "hex"),
            value: Buffer.from(value, "hex"),
          })),
      ];
      const traceRoot = await buildCountedRoot(
        SDK.ROOT_DOMAINS.validationTraces,
        validationEntries,
      );
      const mutation = {
        delta_was_present: false,
        old_delta: 0n,
        delta_proof: Data.from(
          Buffer.from(deltaProof.toCBOR()).toString("hex"),
          SDK.Proof,
        ),
      };
      const assetWitness = {
        descriptor_cbor: descriptorCbor.toString("hex"),
        asset_index: BigInt(selectedIndex),
        policy_id: policyId.toString("hex"),
        asset_name: assetName.toString("hex"),
        quantity,
        asset_peaks: assetProof.frontier.peaks.map((p) => ({
          height: BigInt(p.height),
          hash: p.hash.toString("hex"),
        })),
        asset_siblings: assetProof.siblings.map((h) => h.toString("hex")),
        mutation,
      };
      const auxiliary: SDK.ValidationAuxiliaryWitness =
        kind === "input"
          ? {
              ValueInputAssetWitness: {
                ...assetWitness,
                source_kind: 0n,
                key: replayKey.toString("hex"),
                next_schedule_hash: bytes32().toString("hex"),
              },
            }
          : kind === "output"
            ? { ValueOutputAssetWitness: { ...assetWitness, output_index: 0n } }
            : {
                ValueMintAssetWitness: {
                  mint_index: BigInt(selectedIndex),
                  policy_id: policyId.toString("hex"),
                  asset_name: assetName.toString("hex"),
                  quantity,
                  siblings: mintProof.siblings.map((h) => h.toString("hex")),
                  mutation,
                },
              };
      const retained: SDK.RetainedValidationWitness = {
        machine_state: SDK.validationMachineStateDataFromCore(state),
        trace_proof: SDK.validationTraceProofDataFromCore(trace.proofs[0]!),
        phase: 12n,
        program_counter: 0n,
        witness_cbor: witnessCbor.toString("hex"),
        auxiliary,
      };
      const retainedKey: SDK.RetainedValidationWitnessKey = {
        event_key: eventKey,
        execution_index: -1n,
      };
      const finding = {
        subject: forced
          ? SDK.forcedVerdictSubject({
              transactionId,
              sourceKey: orderKey,
              rejectionReason: reason,
            })
          : SDK.acceptedVerdictSubject(transactionId),
        coordinate,
      };
      const retainedAuth = await buildDistinctAssetAuthenticationFromRetainedDa(
        {
          eventKey,
          finding,
          authenticatedValidationTraceEntries: validationEntries,
          retainedValidationWitnessEntries: [
            {
              key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
              value: SDK.encodeRetainedValidationWitness(retained),
            },
          ],
          expectedValidationTracesRoot: traceRoot.root,
        },
      );
      const evidence = prepareDistinctAssetAccumulationEvidence({
        finding,
        traceStateHashHex: retained.trace_proof.state_hash,
        workRootHex: retained.machine_state.work_root,
        pre: {
          assetRootHex: assetRoot,
          seenAssetCount: crossing ? 16_384 : 16_383,
          nonzeroAssetCount: 0,
          cursor: 0,
        },
        post: crossing
          ? null
          : {
              assetRootHex: deltaProof.verify(true)!.toString("hex"),
              seenAssetCount: 16_384,
              nonzeroAssetCount: 1,
              cursor: selectedIndex + 1,
            },
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
              validation_traces: validationEntries.map(({ key, value }) => [
                key.toString("hex"),
                value.toString("hex"),
              ]),
              validation_trace_witnesses: [
                [
                  Buffer.from(
                    SDK.encodeRetainedValidationWitnessKey(retainedKey),
                  ).toString("hex"),
                  Buffer.from(
                    SDK.encodeRetainedValidationWitness(retained),
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
        await detectDistinctAssetAccumulationCanonicalViolations(replayBlock),
      ).toHaveLength(honest ? 0 : 1);
      let txInclusion = block.txInclusion;
      let sourceMembership = forced
        ? await buildForcedTransactionLeafMembershipProof({
            reconstruction: block.reconstruction,
            eventKey,
          })
        : undefined;
      if (maximum) {
        // An operator can commit any subtree roots. Reconstruct the largest
        // hash-path proof against the target leaf without allocating hidden trees.
        const sourceKey = forced
          ? Buffer.from(Data.to(orderKey, SDK.OutputReference), "hex")
          : Buffer.from(transactionId, "hex");
        const sourceValue = forced
          ? Buffer.from(
              Data.to(sourceMembership!.value, SDK.ForcedInclusionTxV1),
              "hex",
            )
          : Buffer.from(txInclusion!.l2TransactionSourceCbor, "hex");
        const sourceProof = MpfProof.fromJSON(
          sourceKey,
          sourceValue,
          Array.from({ length: 64 }, (_, index) => ({
            type: "branch",
            skip: 0,
            neighbors: Buffer.alloc(128, index + 1).toString("hex"),
          })),
        );
        const phasRoot = sourceProof.verify(true)!.toString("hex");
        const proof = Data.from(
          Buffer.from(sourceProof.toCBOR()).toString("hex"),
          SDK.Proof,
        );
        if (forced) {
          const root = await commitCountedRoot({
            domain: sourceMembership!.domain,
            phasRoot,
            count: sourceMembership!.count,
          });
          sourceMembership = {
            ...sourceMembership!,
            root,
            phas_root: phasRoot,
            proof,
          };
          header.forcedTransactionsRoot = root;
        } else {
          header.transactionsRoot = await commitCountedRoot({
            domain: SDK.ROOT_DOMAINS.transactionsV1,
            phasRoot,
            count: header.l2TransactionCount,
          });
          txInclusion = {
            ...txInclusion!,
            transactionsPhasRoot: phasRoot,
            txMembershipProof: proof,
            txMembershipProofCbor: canonicalPlutusDataCbor(
              Data.to(proof, SDK.Proof),
            ),
          };
        }
      }
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
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
        for (const measurement of capture.measurements) {
          expectProofFit({
            stage: label,
            measurement,
            maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
            maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
          });
          measurements.push({
            name: `${expect.getState().currentTestName}:${label}:${measurements.length}`,
            kind:
              measurement.executionMemory === 0n ? "publication" : "lifecycle",
            maximumShape: expect.getState().currentTestName!,
            signedBytes: measurement.completeSignedBytes,
            memoryUnits: measurement.executionMemory,
            cpuUnits: measurement.executionSteps,
          });
        }
        ledger.push({
          label,
          bytes: capture.measurement.completeSignedBytes,
          memory: capture.measurement.executionMemory.toString(),
          cpu: capture.measurement.executionSteps.toString(),
          margin: capture.measurement.l1ByteMargin,
        });
        return capture.result;
      };
      const references: UTxO[] = [];
      for (const [index, step] of contracts.steps.entries())
        references.push(
          (
            await measured(`publish-step-${index + 1}`, () =>
              publishPlainReferenceScriptUtxo({
                lucid: harness.funderLucid,
                script: step.spendingScript,
                label: `distinct-asset-lifecycle-${index.toString()}`,
              }),
            )
          ).utxo,
        );

      const forcedSource = forced
        ? { header, membership: sourceMembership!, direction: 1n }
        : undefined;
      const artifact = {
        headerHash: setup.headerHash,
        finding,
        evidence,
        authentication: retainedAuth.authentication,
        folds: retainedAuth.folds,
        ...(forcedSource === undefined
          ? {
              accepted: {
                txInclusion: txInclusion!,
                validationTracesRoot: traceRoot.root,
                validationTraceCount: traceRoot.count,
              },
            }
          : { forcedSource }),
      };
      const serializedArtifact = distinctAssetWorkflowArtifact(artifact);
      expect(
        distinctAssetWorkflowArtifact(
          admitDistinctAssetWorkflowArtifact(serializedArtifact),
        ),
      ).toEqual(serializedArtifact);
      const installed = async (
        action: DistinctAssetAccumulationActuatorAction,
        nextStep: number,
      ) => {
        // The installed runner rebuilds from retained DA on each journal turn.
        const recovered = await buildDistinctAssetAuthenticationFromRetainedDa({
          eventKey,
          finding,
          authenticatedValidationTraceEntries: validationEntries,
          retainedValidationWitnessEntries: [
            {
              key: SDK.encodeRetainedValidationWitnessKey(retainedKey),
              value: SDK.encodeRetainedValidationWitness(retained),
            },
          ],
          expectedValidationTracesRoot: traceRoot.root,
        });
        const actuator = createDistinctAssetAccumulationActuator({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo: {},
          network,
          signer: harness.proverSigner,
          categoryId: category.categoryId,
          contracts,
          references: {
            steps: contracts.steps.map(
              (_, index) => references[index]!,
            ) as never,
            witnesses: {
              computationThreadMint:
                harness.witnessReferenceScripts.computationThreadMint!,
              fraudProofMint: harness.witnessReferenceScripts.fraudProofMint!,
              phasMembershipWithdraw:
                harness.witnessReferenceScripts.phasMembershipWithdraw!,
              chunkedVerifyWithdraw:
                harness.witnessReferenceScripts.chunkedVerifyWithdraw!,
              pexcludesWithdraw:
                harness.witnessReferenceScripts.pexcludesWithdraw!,
            },
          },
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => {
              throw new Error("unexpected removal lease");
            },
          },
          fraudProofSpendingScriptHash:
            harness.contracts.fraudProof.spendingScriptHash,
          fraudProverRewardLovelace: 0n,
        });
        const identity: FraudProofWorkflowIdentity = {
          schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
          deploymentFingerprint: "aa".repeat(32),
          category: "distinctAssetAccumulationLimit",
          target: { kind: "state_queue_header", headerHash: setup.headerHash },
        };
        const workflowId = computeFraudProofWorkflowId(identity);
        const append = async (event: FraudProofWorkflowJournalEvent) => {
          const store = new DirectoryFraudProofWorkflowJournalStore(
            journalDirectory,
          );
          const entries = await store.load(workflowId);
          await store.append(
            {
              schemaVersion: "midgard-fraud-proof-workflow-journal-entry-v1",
              workflowId,
              identity,
              sequence: entries.length,
              recordedAt: "2026-09-05T00:00:00.000Z",
              event,
            },
            entries.length,
          );
        };
        if (
          (
            await new DirectoryFraudProofWorkflowJournalStore(
              journalDirectory,
            ).load(workflowId)
          ).length === 0
        ) {
          await append({ kind: "started" });
          const prepared = distinctAssetPreparedJournalArtifact(artifact);
          await append({
            kind: "prepared",
            artifact: prepared,
            artifactDigest: journalJsonDigest(prepared),
          });
        }
        const publications = {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (input: {
            address: string;
            expectedOutRef: string;
            expectedDatumCbor: string;
          }) => {
            const found = (
              await harness.proverLucid.utxosAt(input.address)
            ).find(
              (u) =>
                `${u.txHash}#${u.outputIndex}` === input.expectedOutRef &&
                u.datum === input.expectedDatumCbor,
            );
            return found === undefined
              ? { kind: "not_found" as const }
              : { kind: "confirmed" as const, outRef: input.expectedOutRef };
          },
        };
        let txHash = "";
        for (let attempt = 0; attempt < 2; attempt++) {
          const prerequisite = createDistinctAssetProofPrerequisite({
            maximumTransactionBytes:
              harness.emulator.protocolParameters.maxTxSize,
            lucid: harness.proverLucid,
            network,
            signer: harness.proverSigner,
            publications,
            artifact: admitDistinctAssetWorkflowArtifact(
              distinctAssetWorkflowArtifact({ ...artifact, ...recovered }),
            ),
            transactionConfirmed: async () => true,
          });
          const entries = await new DirectoryFraudProofWorkflowJournalStore(
            journalDirectory,
          ).load(workflowId);
          requireDistinctAssetPreparedArtifact(entries, {
            ...artifact,
            ...recovered,
          });
          expect(() =>
            requireDistinctAssetPreparedArtifact(entries, {
              ...artifact,
              headerHash: "ff".repeat(28),
            }),
          ).toThrow("retained artifact changed");
          const captured = await captureDistinctAssetActionWithProofCarriage({
            actuator,
            action,
            artifact: admitDistinctAssetWorkflowArtifact(
              distinctAssetWorkflowArtifact({ ...artifact, ...recovered }),
            ),
            entries,
            prerequisite,
            lucid: harness.proverLucid,
            signer: harness.proverSigner,
          });
          await append({
            kind: "preflight_passed",
            actionId: captured.actionId,
            txHash: captured.transaction.txHash,
            localEvaluator: "lucid-evolution-local-uplc-v1",
            referenceScripts: captured.transaction.referenceScripts,
          });
          await append({
            kind: "submission_intent",
            actionId: captured.actionId,
            actionInput: captured.actionInput,
            attempt: 1,
            txHash: captured.transaction.txHash,
            ...(captured.durableRecovery === undefined
              ? {}
              : { durableRecovery: captured.durableRecovery }),
          });
          const reloaded = await new DirectoryFraudProofWorkflowJournalStore(
            journalDirectory,
          ).load(workflowId);
          expect(reloaded.at(-1)!.event).toMatchObject({
            kind: "submission_intent",
            txHash: captured.transaction.txHash,
          });
          txHash = await captured.transaction.signed.submit();
          harness.emulator.awaitBlock(1);
          if (
            captured.durableRecovery?.schemaVersion ===
            DISTINCT_ASSET_PROOF_PUBLICATION
          ) {
            const publication = captured.durableRecovery
              .publication as JournalJsonObject;
            expect(
              (
                await prerequisite.reconcile({
                  headerHash: setup.headerHash,
                  action: {
                    actionId: captured.actionId,
                    input: captured.actionInput,
                  },
                  artifact: {},
                  txHash,
                  durableRecovery: publication,
                })
              ).kind,
            ).toBe("confirmed");
            expect(
              (
                await prerequisite.reconcile({
                  headerHash: setup.headerHash,
                  action: {
                    actionId: captured.actionId,
                    input: captured.actionInput,
                  },
                  artifact: {},
                  txHash: "ff".repeat(32),
                  durableRecovery: publication,
                })
              ).kind,
            ).toBe("conflict");
          }
          await append({
            kind: "reconciled",
            actionId: captured.actionId,
            txHash,
            outcome: "confirmed",
          });
          await append({
            kind: "confirmed",
            actionId: captured.actionId,
            txHash,
          });
          if (captured.durableRecovery === undefined) break;
        }
        if (nextStep === 6)
          return {
            nextThreadOutRef: "",
            fraudProofUnit:
              contracts.fraudProof.policyId +
              category.categoryId +
              setup.headerHash,
          };
        const next = (
          await harness.proverLucid.utxosAt(
            contracts.steps[nextStep]!.spendingScriptAddress,
          )
        ).find((u) => u.txHash === txHash);
        if (next === undefined) throw new Error("installed successor absent");
        return {
          nextThreadOutRef: `${next.txHash}#${next.outputIndex}`,
          fraudProofUnit: "",
        };
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
        if (maximum)
          return await installed(
            {
              stage: "step01",
              threadOutRef,
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            },
            1,
          );
        if (forced)
          return await submitDistinctAssetAccumulationStep01Forced({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef,
            finding,
            forcedSource: forcedSource!,
            referenceScriptUtxo: references[0]!,
          });
        const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          family: "distinct-asset-accumulation-limit",
          stepIndex: 0,
          threadOutRef,
        });
        return await submitDistinctAssetAccumulationStep01Accepted({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          signer: harness.proverSigner,
          finding,
          threadUtxo,
          threadToken,
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: txInclusion!,
          validationTracesRoot: traceRoot.root,
          validationTraceCount: traceRoot.count,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
      };
      const step02 = async (threadOutRef: string) =>
        maximum
          ? installed({ stage: "step02", threadOutRef }, 2)
          : submitDistinctAssetAccumulationStep02({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              authentication: retainedAuth.authentication,
              referenceScriptUtxo: references[1]!,
            });
      const fold = async (threadOutRef: string, stepIndex: 2 | 3 | 4) => {
        const selected = retainedAuth.folds[stepIndex - 2]!;
        if (!maximum && !honest && selected.kind === "authenticate")
          await expect(
            submitDistinctAssetAccumulationFold({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              stepIndex,
              action: {
                ...selected,
                evidence: {
                  ...selected.evidence,
                  mutation: {
                    ...selected.evidence.mutation,
                    delta_was_present:
                      !selected.evidence.mutation.delta_was_present,
                  },
                },
              },
              referenceScriptUtxo: references[stepIndex]!,
            }),
          ).rejects.toThrow();
        return maximum
          ? installed({ stage: "fold", threadOutRef, stepIndex }, stepIndex + 1)
          : submitDistinctAssetAccumulationFold({
              lucid: harness.proverLucid,
              contracts,
              categoryId: category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              stepIndex,
              action: retainedAuth.folds[stepIndex - 2]!,
              referenceScriptUtxo: references[stepIndex]!,
            });
      };
      const finalize = async (threadOutRef: string) =>
        maximum
          ? installed({ stage: "step06", threadOutRef }, 6)
          : submitDistinctAssetAccumulationStep06({
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
      for (
        let stepIndex = 0;
        kind === "mint" && !maximum && !honest && stepIndex < 6;
        stepIndex += 1
      ) {
        const outRef = await advance(stepIndex);
        await measured(`cancel-step-${(stepIndex + 1).toString()}`, () =>
          submitDistinctAssetAccumulationCancel({
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
      if (!maximum && !honest)
        await expect(
          submitDistinctAssetAccumulationStep02({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: one.nextThreadOutRef,
            authentication: {
              ...retainedAuth.authentication,
              pre: {
                ...retainedAuth.authentication.pre,
                work_root: "ee".repeat(32),
              },
            },
            referenceScriptUtxo: references[1]!,
          }),
        ).rejects.toThrow();
      const two = await measured("step02", () => step02(one.nextThreadOutRef));
      const three = await measured(
        `step03-input-${kind === "input" ? "decisive" : "skip"}`,
        () => fold(two.nextThreadOutRef, 2),
      );
      const four = await measured(
        `step04-output-${kind === "output" ? "decisive" : "skip"}`,
        () => fold(three.nextThreadOutRef, 3),
      );
      const five = await measured(
        `step05-mint-${kind === "mint" ? "decisive" : "skip"}`,
        () => fold(four.nextThreadOutRef, 4),
      );
      if (honest) {
        await expect(finalize(five.nextThreadOutRef)).rejects.toThrow(
          "terminal evidence is honest",
        );
        const forgedEvidence = {
          ...evidence,
          pre: { ...evidence.pre, seenAssetCount: forced ? 16_383 : 16_384 },
          post: forced
            ? {
                assetRootHex: assetRoot,
                seenAssetCount: 16_384,
                nonzeroAssetCount: 1,
                cursor: selectedIndex + 1,
              }
            : null,
        };
        await expect(
          submitDistinctAssetAccumulationStep06({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: five.nextThreadOutRef,
            evidence: forgedEvidence,
            referenceScriptUtxo: references[5]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        ).rejects.toThrow();
        return;
      }
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
        expect(BigInt(row.memory), row.label).toBeGreaterThanOrEqual(0n);
        expect(BigInt(row.cpu), row.label).toBeGreaterThanOrEqual(0n);
      }
    },
    600_000,
  );
});
