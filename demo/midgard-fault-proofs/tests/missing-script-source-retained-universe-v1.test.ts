import {
  buildMidgardValidationTraceTree,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitnessV1,
  encodeRetainedValidationWitnessKeyV1,
  encodeRetainedValidationWitnessV1,
  type EventKey,
  EventKeySchema,
  ROOT_DOMAINS,
  ValidationAuxiliaryWitnessV1Schema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  validationAuxiliaryWitnessDataV1,
} from "@al-ft/midgard-validation";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE_V1,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { applyMissingScriptSourceScriptsV1 } from "../src/missing-script-source/contracts-v1.js";
import { missingScriptSourceEvidenceFromUniverseV1 } from "../src/missing-script-source/production-replay-v1.js";
import {
  buildRetainedMissingScriptSourceUniverseV1,
  discoverRetainedMissingScriptSourceCoordinatesV1,
} from "../src/missing-script-source/retained-script-universe-v1.js";
import { submitMissingScriptSourceCancelV1 } from "../src/missing-script-source/submit-cancel-v1.js";
import { submitMissingScriptSourceInitV1 } from "../src/missing-script-source/submit-init-v1.js";
import {
  submitMissingScriptSourceStep01AcceptedV1,
  submitMissingScriptSourceStep01ForcedV1,
} from "../src/missing-script-source/submit-step-01-v1.js";
import { submitMissingScriptSourceStep02V1 } from "../src/missing-script-source/submit-step-02-v1.js";
import { submitMissingScriptSourceStep03V1 } from "../src/missing-script-source/submit-step-03-v1.js";
import { submitMissingScriptSourceStep04V1 } from "../src/missing-script-source/submit-step-04-v1.js";
import { submitMissingScriptSourceStep05V1 } from "../src/missing-script-source/submit-step-05-v1.js";
import { submitMissingScriptSourceStep06V1 } from "../src/missing-script-source/submit-step-06-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { buildDecodingBlockFixtureV1 } from "./support/native-script-decoding-emulator-v1.js";
import {
  emulatorSuccessorHeaderStartV1,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
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

const AddressDataLocal = Data.Object({
  paymentCredential: Data.Enum([
    Data.Object({ PublicKeyCredential: Data.Tuple([Data.Bytes()]) }),
    Data.Object({ ScriptCredential: Data.Tuple([Data.Bytes()]) }),
  ]),
  stakeCredential: Data.Nullable(Data.Any()),
});

const retainedFixture = async (
  presentAt: "inline" | "reference" | null = null,
) => {
  const spent = outRefFromByte(0x31);
  const reference = outRefFromByte(0x32);
  const required =
    presentAt === null
      ? nativeScriptWitness({
          type: "sig",
          keyHash: Buffer.alloc(28, 0x44),
        })
      : nativeScriptWitness({ type: "all", scripts: [] });
  const inline =
    presentAt === null
      ? nativeScriptWitness({ type: "all", scripts: [] })
      : nativeScriptWitness({
          type: "atLeast",
          required: 0n,
          scripts: [],
        });
  const referenced = nativeScriptWitness({ type: "before", slot: 500n });
  const acceptedInlineSources = [
    inline,
    ...Array.from({ length: 22 }, (_, index) =>
      nativeScriptWitness({
        type: "all",
        scripts: Array.from({ length: index + 1 }, () => ({
          type: "all" as const,
          scripts: [],
        })),
      }),
    ),
  ];
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(required),
    FUNDED_OUTPUT_LOVELACE_V1,
  );
  const referenceOutput = encodeMidgardTxOutput({
    address: Buffer.alloc(29, 0x61),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE_V1, assets: new Map() },
    script_ref: presentAt === "reference" ? required : referenced,
  });
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    referenceInputs: [reference],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE_V1)],
    scriptWitnesses:
      presentAt === null
        ? acceptedInlineSources
        : presentAt === "inline"
          ? [required, inline]
          : [inline],
  });
  const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
  const eventKey = (
    presentAt === null
      ? { L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") } }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } }
  ) as EventKey;
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor,
      sourceKind: presentAt === null ? "normal" : "forced",
      committedForcedVerdict: presentAt === null ? undefined : "rejected",
      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: "33".repeat(32),
      postUtxosRoot: "33".repeat(32),
      ledgerWitnessEntries: [
        { outRef: spent, output: spentOutput },
        { outRef: reference, output: referenceOutput },
      ],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode:
        presentAt === null
          ? "E_MISSING_REQUIRED_WITNESS"
          : "E_INVALID_FIELD_TYPE",
    }),
  );
  const committedRejectionHash =
    presentAt === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCodeV1("E_MISSING_REQUIRED_WITNESS");
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineStateV1),
    presentAt === null ? "accepted" : "rejected",
    committedRejectionHash,
  );
  const descriptorData = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict: presentAt === null ? ("Accepted" as const) : ("Rejected" as const),
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: eventKeyCbor,
      value: Buffer.from(
        Data.to(
          descriptorData as never,
          ValidationTraceDescriptorV1Schema as never,
        ),
        "hex",
      ),
    },
  ];
  const retainedEntries = trace.witnesses.flatMap((witness, stateIndex) => {
    if (
      witness.phase !== "scriptSources" ||
      (witness.auxiliary !== null &&
        witness.auxiliary.kind !== "scriptPurposeScan" &&
        witness.auxiliary.kind !== "scriptSourceScan")
    )
      return [];
    const key = encodeRetainedValidationWitnessKeyV1({
      event_key: eventKey,
      execution_index: BigInt(stateIndex) - BigInt(trace.witnesses.length),
    });
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessDataV1(witness.auxiliary) as never),
      ValidationAuxiliaryWitnessV1Schema,
    );
    const value = encodeRetainedValidationWitnessV1({
      machine_state: validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: validationTraceProofDataFromCore(tree.proofs[stateIndex]!),
      phase: 8n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    } as never);
    return [{ key, value }];
  });
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  return {
    eventKey,
    descriptorEntries,
    retainedEntries,
    expectedRoot: root.root,
    transaction,
    orderKey,
    trace,
  };
};

describe("missingScriptSource retained ScriptSources universe", () => {
  it("reconstructs the production purpose/source/no-auxiliary sequence in canonical location order", async () => {
    const fixture = await retainedFixture();
    const coordinates = discoverRetainedMissingScriptSourceCoordinatesV1({
      eventKey: fixture.eventKey,
      retainedValidationWitnessEntries: fixture.retainedEntries,
    });
    expect(coordinates).toHaveLength(1);
    const universe = await buildRetainedMissingScriptSourceUniverseV1({
      eventKey: fixture.eventKey,
      ...coordinates[0]!,
      authenticatedValidationTraceEntries: fixture.descriptorEntries,
      retainedValidationWitnessEntries: fixture.retainedEntries,
      expectedValidationTracesRoot: fixture.expectedRoot,
    });
    expect(universe.purpose.purposeKind).toBe(0);
    expect(universe.sources.map(({ originKind }) => originKind)).toEqual([
      ...Array.from({ length: 23 }, () => 0),
      1,
    ]);
    expect(universe.transactionSourceCount).toBe(23);
    expect(
      universe.sources.some(
        ({ scriptHashHex }) =>
          scriptHashHex === universe.purpose.requiredScriptHashHex,
      ),
    ).toBe(false);
  }, 60_000);

  it("rejects omitted openings and normalizes retained transport order", async () => {
    const fixture = await retainedFixture();
    const coordinates = discoverRetainedMissingScriptSourceCoordinatesV1({
      eventKey: fixture.eventKey,
      retainedValidationWitnessEntries: fixture.retainedEntries,
    });
    const sourceEntryIndex = fixture.retainedEntries.findIndex(({ value }) => {
      const auxiliary = decodeRetainedValidationWitnessV1(value).auxiliary;
      return (
        typeof auxiliary === "object" && "ScriptSourceScanWitness" in auxiliary
      );
    });
    expect(sourceEntryIndex).toBeGreaterThanOrEqual(0);
    await expect(
      buildRetainedMissingScriptSourceUniverseV1({
        eventKey: fixture.eventKey,
        ...coordinates[0]!,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: fixture.retainedEntries.filter(
          (_entry, index) => index !== sourceEntryIndex,
        ),
        expectedValidationTracesRoot: fixture.expectedRoot,
      }),
    ).rejects.toThrow(/absent|incomplete/u);
    await expect(
      buildRetainedMissingScriptSourceUniverseV1({
        eventKey: fixture.eventKey,
        ...coordinates[0]!,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: [
          ...fixture.retainedEntries,
        ].reverse(),
        expectedValidationTracesRoot: fixture.expectedRoot,
      }),
    ).resolves.toMatchObject({ transactionSourceCount: 23 });
  }, 60_000);

  it.each(["inline", "reference"] as const)(
    "reconstructs an authenticated production prefix when the matching source is %s",
    async (presentAt) => {
      const fixture = await retainedFixture(presentAt);
      const universe = await buildRetainedMissingScriptSourceUniverseV1({
        eventKey: fixture.eventKey,
        purposeKind: 0,
        purposeIndex: 0,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.expectedRoot,
        expectedPresence: true,
      });
      expect(universe.sources.map(({ originKind }) => originKind)).toEqual(
        presentAt === "inline" ? [0] : [0, 1],
      );
      expect(
        universe.sources.findIndex(
          ({ scriptHashHex }) =>
            scriptHashHex === universe.purpose.requiredScriptHashHex,
        ),
      ).toBe(presentAt === "inline" ? 0 : 1);
    },
    60_000,
  );

  it.each([
    { presentAt: null, cancel: false },
    { presentAt: "reference" as const, cancel: false },
    { presentAt: null, cancel: true },
  ])(
    "executes the real Lucid $presentAt contradiction (cancel=$cancel)",
    async ({ presentAt, cancel }) => {
      const fixture = await retainedFixture(presentAt);
      const universe = await buildRetainedMissingScriptSourceUniverseV1({
        eventKey: fixture.eventKey,
        purposeKind: 0,
        purposeIndex: 0,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.expectedRoot,
        expectedPresence: presentAt !== null,
      });
      const harness = await makeFaultProofEmulatorHarnessV1({
        contractOptions: { alwaysFraudProofCatalogue: true },
        lucidOptions: { evaluator: createScalusEvaluator() },
      });
      const paymentCredential = getAddressDetails(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).paymentCredential!;
      const addressData = Data.from(
        Data.to(
          {
            paymentCredential:
              paymentCredential.type === "Key"
                ? { PublicKeyCredential: [paymentCredential.hash] }
                : { ScriptCredential: [paymentCredential.hash] },
            stakeCredential: null,
          } as never,
          AddressDataLocal as never,
        ),
      );
      const steps = applyMissingScriptSourceScriptsV1({
        blueprint: harness.realBlueprint,
        network,
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: addressData,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      });
      const contracts = {
        steps,
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
        missingScriptSource: {
          ...harness.contracts.fraudProofs.missingScriptSource,
          spendingScriptHash: steps[0].spendingScriptHash,
        },
      });
      const category = catalogue.categories.missingScriptSource;
      const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
      const startTime = BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      );
      const reason = {
        ScriptSourceMissing: { purpose_kind: 0n, purpose_index: 0n },
      } as const;
      const block = await buildDecodingBlockFixtureV1({
        operatorVkey,
        startTime,
        priorLedgerRoot: "33".repeat(32),
        subject:
          presentAt === null
            ? { kind: "normal", nativeTx: fixture.transaction.tx }
            : {
                kind: "forced",
                nativeTx: fixture.transaction.tx,
                orderKey: fixture.orderKey,
                verdict: { ForcedTxInvalid: { reason } },
              },
      });
      const header = {
        ...block.header,
        endTime: block.header.endTime + 60_000n,
        validationTracesRoot: fixture.expectedRoot,
        validationTraceCount: 1n,
      };
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
      const successorStart = emulatorSuccessorHeaderStartV1({
        predecessorEndTime: header.endTime,
        emulator: harness.emulator,
      });
      const successor = await submitSuccessorBlockTx({
        lucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        anchorBlockUnit: setup.stateQueueBlockUnit,
        header: {
          ...header,
          startTime: BigInt(successorStart),
          endTime: BigInt(successorStart + 60_000),
          prevHeaderHash: setup.headerHash,
        },
        hubOracle: setup.hubOracle,
        scheduler: setup.scheduler,
        activeOperatorNode: setup.activeOperatorNode,
        activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
      });
      const references: UTxO[] = [];
      for (const [index, step] of steps.entries())
        references.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `missing-source-${index.toString()}`,
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
        presentAt === null
          ? ({
              version: 1n,
              direction: 0n,
              source_kind: 0n,
              transaction_id: block.nativeTxId,
              source_key: "",
              rejection_reason: null,
            } as const)
          : ({
              version: 1n,
              direction: 1n,
              source_kind: 1n,
              transaction_id: block.nativeTxId,
              source_key: Data.to(
                fixture.orderKey as never,
                Data.Object({
                  transactionId: Data.Bytes(),
                  outputIndex: Data.Integer(),
                }),
              ),
              rejection_reason: reason,
            } as const);
      const evidence = missingScriptSourceEvidenceFromUniverseV1({
        subject,
        universe,
      });
      const init = await measured("init", () =>
        submitMissingScriptSourceInitV1({
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
          fraudulentBlockOutRef: successor.continuedAnchorOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      const common = {
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        header,
        executionIndex: BigInt(universe.purpose.absoluteIndex),
        purposeKind: 0 as const,
        purposeIndex: 0n,
        referenceScriptUtxo: references[0]!,
      };
      const step01 =
        presentAt === null
          ? await measured("step01-accepted", () =>
              submitMissingScriptSourceStep01AcceptedV1({
                ...common,
                blueprint: harness.realBlueprint,
                network,
                stateQueueBlockOutRef: successor.continuedAnchorOutRef,
                txInclusion: block.txInclusion!,
                witnessReferenceScripts: harness.witnessReferenceScripts,
              }),
            )
          : await measured("step01-forced", async () =>
              submitMissingScriptSourceStep01ForcedV1({
                ...common,
                membership: await buildForcedTransactionLeafMembershipProof({
                  reconstruction: block.reconstruction,
                  eventKey: fixture.eventKey,
                }),
              }),
            );
      const step02 = await measured("step02-authenticate", () =>
        submitMissingScriptSourceStep02V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          evidence,
          authentication: universe.authentication,
          referenceScriptUtxo: references[1]!,
        }),
      );
      if (cancel) {
        const cancelled = await measured("cancel", () =>
          submitMissingScriptSourceCancelV1({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step02.nextThreadOutRef,
            referenceScriptUtxo: references[2]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        );
        expect(cancelled.txHash).toMatch(/^[0-9a-f]{64}$/u);
        for (const { measurement } of lifecycleRows) {
          expect(measurement.l1ByteMargin).toBeGreaterThan(0);
          expect(measurement.executionMemory).toBeGreaterThan(0n);
          expect(measurement.executionSteps).toBeGreaterThan(0n);
        }
        if (process.env.MIDGARD_PRINT_FIT === "1")
          console.info(
            `[missing-script-source-fit] ${JSON.stringify(lifecycleRows, (_key, value) => (typeof value === "bigint" ? value.toString() : value))}`,
          );
        return;
      }
      // Model a process restart: discard the submitter result and recover the
      // continuation solely from the live step-03 script output.
      const resumedThread = (
        await harness.proverLucid.utxosAt(steps[2]!.spendingScriptAddress)
      ).find(({ txHash }) => txHash === step02.txHash);
      expect(resumedThread).toBeDefined();
      if (resumedThread === undefined)
        throw new Error("missing ScriptSource step-03 output after restart");
      const resumedThreadOutRef = `${resumedThread.txHash}#${resumedThread.outputIndex.toString()}`;
      expect(resumedThreadOutRef).toBe(step02.nextThreadOutRef);
      const step03 = await measured("step03-frontiers", () =>
        submitMissingScriptSourceStep03V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: resumedThreadOutRef,
          evidence,
          authentication: universe.authentication,
          referenceScriptUtxo: references[2]!,
        }),
      );
      const step04 = await measured("step04-open", () =>
        submitMissingScriptSourceStep04V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step03.nextThreadOutRef,
          evidence,
          referenceScriptUtxo: references[3]!,
        }),
      );
      const step05 = await measured("step05-scan", () =>
        submitMissingScriptSourceStep05V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step04.nextThreadOutRef,
          evidence,
          referenceScriptUtxo: references[4]!,
        }),
      );
      expect(step05.closed).toBe(true);
      const final = await measured("step06-mint", () =>
        submitMissingScriptSourceStep06V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step05.nextThreadOutRef,
          evidence,
          referenceScriptUtxo: references[5]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expect(final.txHash).toMatch(/^[0-9a-f]{64}$/u);
      const [permanentProof] = await harness.proverLucid.utxosAtWithUnit(
        harness.contracts.fraudProof.spendingScriptAddress,
        final.fraudProofUnit,
      );
      expect(permanentProof?.txHash).toBe(final.txHash);
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
          fraudProofMissingScriptSource: {
            scriptHash: steps[0]!.spendingScriptHash,
            contract: {
              type: steps[0]!.spendingScript.type,
              cborHex: steps[0]!.spendingScript.script,
            },
          },
          ...Object.fromEntries(
            steps.slice(1).map((step, index) => [
              `fraudProofMissingScriptSourceStep0${(index + 2).toString()}`,
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
      let leaseReleased = false;
      const now = BigInt(harness.emulator.now());
      const removal = await measured("remove-leased", () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "missingScriptSource",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "missing-source-lifecycle",
              source: "emulator",
              renew: async () => undefined,
              release: async () => {
                leaseReleased = true;
              },
              fail: async () => undefined,
            }),
          },
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
      );
      expect(removal.fraudulentHeaderHash).toBe(setup.headerHash);
      expect(leaseReleased).toBe(true);
      for (const { measurement } of lifecycleRows) {
        expect(measurement.l1ByteMargin).toBeGreaterThan(0);
        expect(measurement.executionMemory).toBeGreaterThan(0n);
        expect(measurement.executionSteps).toBeGreaterThan(0n);
      }
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          `[missing-script-source-fit] ${JSON.stringify(lifecycleRows, (_key, value) => (typeof value === "bigint" ? value.toString() : value))}`,
        );
    },
    600_000,
  );
});
