/**
 * Shared fixtures and stage drivers for the missingScriptSource emulator
 * suites: a canonical retained-DA fixture over any purpose kind and source
 * location, the committed block the thread disputes, the retained universe
 * a prover reconstructs from it, and measured drivers for every physical
 * step of the applied chain.
 */
import {
  buildMidgardValidationTraceTree,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import {
  AddressData,
  addressDataFromBech32,
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  forcedVerdictSubject,
  type RejectionReason,
  ROOT_DOMAINS,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import { missingScriptSourceEvidenceFromUniverse } from "../../src/missing-script-source/authenticated-replay.js";
import {
  applyMissingScriptSourceScripts,
  type MissingScriptSourceContracts,
} from "../../src/missing-script-source/contracts.js";
import type { MissingScriptSourceEvidence } from "../../src/missing-script-source/family.js";
import {
  buildRetainedMissingScriptSourceUniverse,
  parseRetainedScriptSourcesStageNineControl,
  type RetainedMissingScriptSourceUniverse,
} from "../../src/missing-script-source/retained-script-universe.js";
import { submitMissingScriptSourceCancel } from "../../src/missing-script-source/submit-cancel.js";
import { submitMissingScriptSourceInit } from "../../src/missing-script-source/submit-init.js";
import {
  submitMissingScriptSourceStep01Accepted,
  submitMissingScriptSourceStep01Forced,
} from "../../src/missing-script-source/submit-step-01.js";
import {
  type ExecutionSourceAuthenticationData,
  submitMissingScriptSourceStep02,
} from "../../src/missing-script-source/submit-step-02.js";
import { submitMissingScriptSourceStep03 } from "../../src/missing-script-source/submit-step-03.js";
import { submitMissingScriptSourceStep04 } from "../../src/missing-script-source/submit-step-04.js";
import { submitMissingScriptSourceStep05 } from "../../src/missing-script-source/submit-step-05.js";
import { submitMissingScriptSourceStep06 } from "../../src/missing-script-source/submit-step-06.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { expectProofFit } from "./emulator/proof-fit.js";
import { decoyMissingScriptSourceScript } from "./missing-script-source-shapes.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export type MissingScriptSourcePurposeKind = 0 | 1 | 2 | 3;
export type MissingScriptSourceLocation = "inline" | "reference";

export type MissingScriptSourceFixtureShape = Readonly<{
  /** Consensus purpose kind: spend=0, mint=1, observe=2, receive=3. */
  purposeKind: MissingScriptSourcePurposeKind;
  /** Where the required script is committed; `absent` for no source at all. */
  presentAt: MissingScriptSourceLocation | "absent";
  /** Position of the present source among its location's sources. */
  presentPosition?: "first" | "last";
  /** Decoy inline script witnesses (field 6) beside the required one. */
  inlineDecoys: number;
  /** Decoy reference inputs (field 1), each resolving to a reference script. */
  referenceDecoys: number;
  /**
   * `accepted`: the block commits the transaction as an accepted L2 event.
   * `forced`: the block commits a forced rejection under
   * `ScriptSourceMissing { purposeKind, 0 }`.
   */
  direction: "accepted" | "forced";
  /**
   * The transaction is genuinely valid (source present, no decoys) and the
   * machine accepts it: the honest accepted block a prover cannot convict.
   */
  honest?: boolean;
}>;

const FAKE_LEDGER_ROOT = "33".repeat(32);
const REFERENCE_TX_ID = Buffer.alloc(32, 0x90);
const REFERENCE_OUTPUT_ADDRESS = Buffer.alloc(29, 0x61);
const FORCED_ORDER_KEY = { transactionId: "52".repeat(32), outputIndex: 0n };

export const missingScriptSourceReason = (
  purposeKind: MissingScriptSourcePurposeKind,
  purposeIndex = 0n,
): RejectionReason => ({
  ScriptSourceMissing: {
    purpose_kind: BigInt(purposeKind),
    purpose_index: purposeIndex,
  },
});

const referenceOutRef = (index: number) =>
  encodeMidgardSpendInputItem({ txId: REFERENCE_TX_ID, outputIndex: index });

const referenceOutput = (script: MidgardVersionedScript) =>
  encodeMidgardTxOutput({
    address: REFERENCE_OUTPUT_ADDRESS,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: script,
  });

/**
 * A canonical transaction whose purpose of the given kind requires one script
 * hash, the retained ScriptSources trace the validation machine produces for
 * it, and the DA payload rows (descriptor and retained witnesses) a block
 * commits. Nothing here is fabricated mid-thread: every proof-thread input
 * derives from these rows exactly as the production replay reads them.
 */
export const buildMissingScriptSourceFixture = async (
  shape: MissingScriptSourceFixtureShape,
) => {
  const {
    purposeKind,
    presentAt,
    presentPosition = "first",
    inlineDecoys,
    referenceDecoys,
    direction,
    honest = false,
  } = shape;
  if (honest && (presentAt === "absent" || inlineDecoys + referenceDecoys > 0))
    throw new Error("an honest fixture commits exactly the required source");
  if (!honest && presentAt !== "absent" && inlineDecoys === 0)
    throw new Error(
      "a present-source fixture needs an unused inline decoy so the machine rejects after discovery",
    );
  const required =
    presentAt === "absent"
      ? nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x44) })
      : nativeScriptWitness({ type: "all", scripts: [] });
  const requiredHashHex = hashScriptWitness(required);
  const requiredHash = Buffer.from(requiredHashHex, "hex");
  const inlineDecoyScripts = Array.from({ length: inlineDecoys }, (_v, i) =>
    decoyMissingScriptSourceScript(i),
  );
  const referenceDecoyScripts = Array.from(
    { length: referenceDecoys },
    (_v, i) => decoyMissingScriptSourceScript(100_000 + i),
  );
  const place = (
    location: MissingScriptSourceLocation,
    decoys: readonly MidgardVersionedScript[],
  ) =>
    presentAt !== location
      ? decoys
      : presentPosition === "last"
        ? [...decoys, required]
        : [required, ...decoys];
  const inlineScripts = place("inline", inlineDecoyScripts);
  const referenceScripts = place("reference", referenceDecoyScripts);
  const references = referenceScripts.map((script, index) => ({
    outRef: referenceOutRef(index),
    output: referenceOutput(script),
  }));
  const spent = outRefFromByte(0x31);
  const spentOutput =
    purposeKind === 0
      ? makeProtectedScriptOutput(requiredHashHex, FUNDED_OUTPUT_LOVELACE)
      : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const mintAssetName = Buffer.from("31", "hex");
  const output =
    purposeKind === 3
      ? makeProtectedScriptOutput(requiredHashHex, FUNDED_OUTPUT_LOVELACE)
      : purposeKind === 1
        ? makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            undefined,
            new Map([
              [requiredHashHex, new Map([[mintAssetName.toString("hex"), 1n]])],
            ]),
          )
        : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    referenceInputs: references.map(({ outRef }) => outRef),
    outputs: [output],
    scriptWitnesses: inlineScripts,
    ...(purposeKind === 1
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([[requiredHash, new Map([[mintAssetName, 1n]])]]),
          ),
        }
      : {}),
    ...(purposeKind === 2 ? { requiredObserverItems: [requiredHash] } : {}),
  });
  const orderKey = FORCED_ORDER_KEY;
  const eventKey = (
    direction === "accepted"
      ? { L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") } }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } }
  ) as EventKey;
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const ledgerWitnessEntries = [
    { outRef: spent, output: spentOutput },
    ...references,
  ];
  const honestOperations = honest
    ? [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ]
    : [];
  const mutations = honest
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: ledgerWitnessEntries,
        operations: honestOperations,
      })
    : [];
  const priorLedgerRoot = honest
    ? mutations[0]!.preRoot.toString("hex")
    : FAKE_LEDGER_ROOT;
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: direction === "accepted" ? "normal" : "forced",

      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        direction === "forced"
          ? forcedTraceBytes(forcedTraceView(transaction.tx))
          : transaction.txCbor,
      priorUtxosRoot: priorLedgerRoot,
      postUtxosRoot: honest
        ? mutations.at(-1)!.postRoot.toString("hex")
        : FAKE_LEDGER_ROOT,
      ledgerWitnessEntries,
      expectedLedgerOps: honestOperations,
      ledgerMutationSteps: mutations,
      expectedVerdict: honest ? "accepted" : "rejected",
      expectedRejectionCode: honest
        ? null
        : presentAt === "absent"
          ? "E_MISSING_REQUIRED_WITNESS"
          : "E_INVALID_FIELD_TYPE",
    }),
  );
  // The block commits the operator's verdict: an accepted event carries no
  // rejection code; a forced rejection carries the family's exact code.
  const committedRejectionHash =
    direction === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_MISSING_REQUIRED_WITNESS");
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    direction === "accepted" ? "accepted" : "rejected",
    committedRejectionHash,
  );
  const descriptorData = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict:
      direction === "accepted" ? ("Accepted" as const) : ("Rejected" as const),
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: eventKeyCbor,
      value: Buffer.from(
        Data.to(
          descriptorData as never,
          ValidationTraceDescriptorSchema as never,
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
    const key = encodeRetainedValidationWitnessKey({
      event_key: eventKey,
      execution_index: BigInt(stateIndex) - BigInt(trace.witnesses.length),
    });
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      ValidationAuxiliaryWitnessSchema,
    );
    const value = encodeRetainedValidationWitness({
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
  const sourceCount = inlineScripts.length + referenceScripts.length;
  const presentSourceIndex =
    presentAt === "absent"
      ? null
      : presentAt === "inline"
        ? presentPosition === "last"
          ? inlineScripts.length - 1
          : 0
        : inlineScripts.length +
          (presentPosition === "last" ? referenceScripts.length - 1 : 0);
  return {
    shape,
    transaction,
    eventKey,
    orderKey,
    trace,
    descriptorEntries,
    retainedEntries,
    expectedRoot: root.root,
    priorLedgerRoot,
    requiredHashHex,
    sourceCount,
    transactionSourceCount: inlineScripts.length,
    presentSourceIndex,
    reason: missingScriptSourceReason(purposeKind),
  };
};
export type MissingScriptSourceFixture = Awaited<
  ReturnType<typeof buildMissingScriptSourceFixture>
>;

/** The exact prover reconstruction from the fixture's public retained DA. */
export const buildMissingScriptSourceUniverse = (
  fixture: MissingScriptSourceFixture,
  expectedPresence = fixture.shape.presentAt !== "absent",
) =>
  buildRetainedMissingScriptSourceUniverse({
    eventKey: fixture.eventKey,
    purposeKind: fixture.shape.purposeKind,
    purposeIndex: 0,
    authenticatedValidationTraceEntries: fixture.descriptorEntries,
    retainedValidationWitnessEntries: fixture.retainedEntries,
    expectedValidationTracesRoot: fixture.expectedRoot,
    expectedPresence,
  });

/**
 * The universe a lying prover claims: the retained stage-9 witness at
 * `sourceCursor` as the terminal, and the authenticated source prefix up to
 * it. Every row is still the operator's own retained DA; only the choice of
 * terminal is dishonest, which is exactly what the chain must refuse.
 */
export const claimMissingScriptSourcePrefix = ({
  fixture,
  universe,
  sourceCursor,
}: {
  readonly fixture: MissingScriptSourceFixture;
  readonly universe: RetainedMissingScriptSourceUniverse;
  readonly sourceCursor: number;
}): RetainedMissingScriptSourceUniverse => {
  const entries = fixture.retainedEntries.map((entry) => ({
    key: decodeRetainedValidationWitnessKey(entry.key),
    witness: decodeRetainedValidationWitness(entry.value),
  }));
  const claimed = entries.find(({ witness }) => {
    const auxiliary = witness.auxiliary;
    if (
      witness.phase !== 8n ||
      typeof auxiliary !== "object" ||
      !("ScriptSourceScanWitness" in auxiliary)
    )
      return false;
    try {
      const control = parseRetainedScriptSourcesStageNineControl(
        witness.witness_cbor,
      );
      return (
        control.discovery.sourceCursor === BigInt(sourceCursor) &&
        control.discovery.purposeKind ===
          BigInt(universe.purpose.purposeKind) &&
        control.discovery.purposeIndex ===
          BigInt(universe.purpose.purposeIndex) &&
        control.discovery.matchedSourceIndex === -1n
      );
    } catch {
      return false;
    }
  });
  if (claimed === undefined)
    throw new Error("no retained source-scan witness at the claimed cursor");
  const control = parseRetainedScriptSourcesStageNineControl(
    claimed.witness.witness_cbor,
  );
  return Object.freeze({
    ...universe,
    authentication: {
      ...universe.authentication,
      machine_state: claimed.witness.machine_state,
      trace_proof: claimed.witness.trace_proof,
      control: control.control,
      control_data: control.controlData,
    },
    sources: universe.sources.slice(0, sourceCursor + 1),
    transactionSourceCount: Math.min(
      universe.transactionSourceCount,
      sourceCursor + 1,
    ),
  });
};

export const missingScriptSourceSubject = (
  fixture: MissingScriptSourceFixture,
  nativeTxId: string,
  reason: RejectionReason = fixture.reason,
): VerdictSubject =>
  fixture.shape.direction === "accepted"
    ? {
        version: 1n,
        direction: 0n,
        source_kind: 0n,
        transaction_id: nativeTxId,
        source_key: "",
        rejection_reason: null,
      }
    : forcedVerdictSubject({
        transactionId: nativeTxId,
        sourceKey: fixture.orderKey,
        rejectionReason: reason,
      });

export const missingScriptSourceEvidence = ({
  fixture,
  universe,
  nativeTxId,
  reason,
}: {
  readonly fixture: MissingScriptSourceFixture;
  readonly universe: RetainedMissingScriptSourceUniverse;
  readonly nativeTxId: string;
  readonly reason?: RejectionReason;
}): MissingScriptSourceEvidence =>
  missingScriptSourceEvidenceFromUniverse({
    subject: missingScriptSourceSubject(fixture, nativeTxId, reason),
    universe,
  });

export const makeMissingScriptSourceHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const steps = applyMissingScriptSourceScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
  });
  // The generic Init/removal submitters read the state-queue and certificate
  // identities beside the family's own steps.
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
  } satisfies MissingScriptSourceContracts & Record<string, unknown>;
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    missingScriptSource: {
      ...harness.contracts.fraudProofs.missingScriptSource,
      spendingScriptHash: steps[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.missingScriptSource;
  expect(category.categoryId).toBe("0000002d");
  expect(category.scriptHash).toBe(steps[0].spendingScriptHash);
  const references: UTxO[] = [];
  const publications: {
    name: string;
    scriptHash: string;
    measurement: CompleteSignedTransactionMeasurement;
  }[] = [];
  for (const [index, step] of steps.entries()) {
    // Published by the prover: the funder's first UTxO is the nonce the
    // disputed block's setup transaction must still be able to spend.
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.proverLucid,
      script: step.spendingScript as Script,
      label: `missing-script-source-step-0${(index + 1).toString()}`,
    });
    references.push(published.utxo);
    publications.push({
      name: `reference-step-0${(index + 1).toString()}`,
      scriptHash: step.spendingScriptHash,
      measurement: published.publicationMeasurement,
    });
    expectProofFit({
      stage: `publication:step-0${(index + 1).toString()}`,
      measurement: published.publicationMeasurement,
      maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
      maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
    });
  }
  return { harness, contracts, catalogue, category, references, publications };
};
export type MissingScriptSourceHarness = Awaited<
  ReturnType<typeof makeMissingScriptSourceHarness>
>;

/**
 * Commits the fixture's transaction in a disputed block (plus one successor,
 * so removal exercises target-and-descendant deletion) and returns the block
 * evidence the thread binds to. `committedReason` lets a suite commit the
 * forced leaf under a different typed reason than the prover claims.
 */
export const commitMissingScriptSourceBlock = async ({
  harness,
  catalogue,
  fixture,
  committedReason = fixture.reason,
}: {
  readonly harness: MissingScriptSourceHarness["harness"];
  readonly catalogue: MissingScriptSourceHarness["catalogue"];
  readonly fixture: MissingScriptSourceFixture;
  readonly committedReason?: RejectionReason;
}) => {
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: fixture.priorLedgerRoot,
    subject:
      fixture.shape.direction === "accepted"
        ? { kind: "normal", nativeTx: fixture.transaction.tx }
        : {
            kind: "forced",
            nativeTx: fixture.transaction.tx,
            orderKey: fixture.orderKey,
            verdict: { ForcedTxInvalid: { reason: committedReason } },
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
  const successorStart = emulatorSuccessorHeaderStart({
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
  const forcedMembership =
    fixture.shape.direction === "forced"
      ? await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey: fixture.eventKey,
        })
      : null;
  return {
    block,
    header,
    setup,
    successor,
    forcedMembership,
    nativeTxId: block.nativeTxId,
    /** The state-queue block out-ref every thread on this block binds to. */
    disputedBlockOutRef: successor.continuedAnchorOutRef,
  };
};
export type MissingScriptSourceBlock = Awaited<
  ReturnType<typeof commitMissingScriptSourceBlock>
>;

export type MissingScriptSourceStageRow = {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
};

/** Measured drivers for every physical step over one harness and block. */
export const makeMissingScriptSourceStages = (
  {
    harness,
    contracts,
    catalogue,
    category,
    references,
  }: MissingScriptSourceHarness,
  block: MissingScriptSourceBlock,
) => {
  const measurements: MissingScriptSourceStageRow[] = [];
  const record = <T>(
    stage: string,
    captured: Awaited<ReturnType<typeof captureEmulatorSubmission<T>>>,
  ) => {
    measurements.push({ stage, measurement: captured.measurement });
    return captured.result;
  };
  const measured = async <T>(stage: string, operation: () => Promise<T>) => {
    console.info(`[missing-script-source-stage] ${stage}`);
    return record(
      stage,
      await captureEmulatorSubmission(harness.emulator, operation),
    );
  };
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  } as const;
  const init = async () =>
    (
      await measured("init", () =>
        submitMissingScriptSourceInit({
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
          fraudulentBlockOutRef: block.disputedBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      )
    ).nextThreadOutRef;
  const step01 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    options: {
      readonly purposeKind?: MissingScriptSourcePurposeKind;
      readonly purposeIndex?: bigint;
      readonly referenceScriptUtxo?: UTxO;
    } = {},
  ) => {
    const purposeKind = options.purposeKind ?? evidence.finding.purposeKind;
    const purposeIndex =
      options.purposeIndex ?? BigInt(evidence.finding.purposeIndex);
    const shared = {
      ...common,
      threadOutRef,
      header: block.header,
      executionIndex: BigInt(evidence.finding.executionIndex),
      purposeKind,
      purposeIndex,
      referenceScriptUtxo: options.referenceScriptUtxo ?? references[0]!,
    };
    return (
      await measured(
        `step01-${evidence.finding.subject.direction === 0n ? "accepted" : "forced"}`,
        () =>
          evidence.finding.subject.direction === 0n
            ? submitMissingScriptSourceStep01Accepted({
                ...shared,
                blueprint: harness.realBlueprint,
                network,
                stateQueueBlockOutRef: block.disputedBlockOutRef,
                txInclusion: block.block.txInclusion!,
                witnessReferenceScripts: harness.witnessReferenceScripts,
              })
            : submitMissingScriptSourceStep01Forced({
                ...shared,
                membership: block.forcedMembership!,
              }),
      )
    ).nextThreadOutRef;
  };
  const step02 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    authentication: ExecutionSourceAuthenticationData,
    referenceScriptUtxo: UTxO = references[1]!,
  ) =>
    (
      await measured("step02-trace", () =>
        submitMissingScriptSourceStep02({
          ...common,
          threadOutRef,
          evidence,
          authentication,
          referenceScriptUtxo,
        }),
      )
    ).nextThreadOutRef;
  const step03 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    authentication: ExecutionSourceAuthenticationData,
  ) =>
    (
      await measured("step03-frontiers", () =>
        submitMissingScriptSourceStep03({
          ...common,
          threadOutRef,
          evidence,
          authentication,
          referenceScriptUtxo: references[2]!,
        }),
      )
    ).nextThreadOutRef;
  const step04 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) =>
    (
      await measured("step04-open-scan", () =>
        submitMissingScriptSourceStep04({
          ...common,
          threadOutRef,
          evidence,
          referenceScriptUtxo: references[3]!,
        }),
      )
    ).nextThreadOutRef;
  const step05 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
    options: { readonly itemBudget?: number; readonly label?: string } = {},
  ) =>
    await measured(options.label ?? "step05-scan", () =>
      submitMissingScriptSourceStep05({
        ...common,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        ...(options.itemBudget === undefined
          ? {}
          : { itemBudget: options.itemBudget }),
      }),
    );
  /** Every scan batch in order; returns the step-06 thread and batch count. */
  const scan = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) => {
    let cursor = threadOutRef;
    let batches = 0;
    for (;;) {
      const result = await step05(cursor, evidence, {
        label: `step05-scan-${batches.toString()}`,
      });
      batches += 1;
      cursor = result.nextThreadOutRef;
      if (result.closed) break;
      if (batches > 1_000)
        throw new Error("missing-script-source scan did not close");
    }
    return { threadOutRef: cursor, batches };
  };
  const step06 = async (
    threadOutRef: string,
    evidence: MissingScriptSourceEvidence,
  ) =>
    await measured("step06-mint", () =>
      submitMissingScriptSourceStep06({
        ...common,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const cancel = async (threadOutRef: string, stepIndex: number) => {
    const result = await measured(
      `cancel-step-0${(stepIndex + 1).toString()}`,
      () =>
        submitMissingScriptSourceCancel({
          ...common,
          threadOutRef,
          referenceScriptUtxo: references[stepIndex]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(result.cancelledStepIndex).toBe(stepIndex);
    return result;
  };
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const stepEntry = (
      step: MissingScriptSourceContracts["steps"][number],
    ) => ({
      scriptHash: step.spendingScriptHash,
      contract: {
        type: step.spendingScript.type,
        cborHex: step.spendingScript.script,
      },
    });
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        fraudProofMissingScriptSource: stepEntry(contracts.steps[0]),
        ...Object.fromEntries(
          contracts.steps
            .slice(1)
            .map((step, index) => [
              `fraudProofMissingScriptSourceStep0${(index + 2).toString()}`,
              stepEntry(step),
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
        fraudulentHeaderHash: block.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "missing-script-source-lifecycle",
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
    expect(removal.fraudulentHeaderHash).toBe(block.setup.headerHash);
    expect(leaseReleased).toBe(true);
    return removal;
  };
  const assertFit = (label: string) => {
    for (const { stage, measurement } of measurements)
      expectProofFit({
        stage: `${label}:${stage}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-script-source-fit:${label}] ${JSON.stringify(
          measurements.map(({ stage, measurement }) => ({
            stage,
            bytes: measurement.completeSignedBytes,
            margin: measurement.l1ByteMargin,
            memory: measurement.executionMemory.toString(),
            cpu: measurement.executionSteps.toString(),
          })),
        )}`,
      );
  };
  return {
    init,
    step01,
    step02,
    step03,
    step04,
    step05,
    scan,
    step06,
    cancel,
    remove,
    assertFit,
    measurements,
  };
};
export type MissingScriptSourceStages = ReturnType<
  typeof makeMissingScriptSourceStages
>;

/** Init through the permanent mint, returning the proof unit. */
export const runMissingScriptSourceThread = async (
  stages: MissingScriptSourceStages,
  evidence: MissingScriptSourceEvidence,
  authentication: ExecutionSourceAuthenticationData,
) => {
  const thread = await stages.step04(
    await stages.step03(
      await stages.step02(
        await stages.step01(await stages.init(), evidence),
        evidence,
        authentication,
      ),
      evidence,
      authentication,
    ),
    evidence,
  );
  const scanned = await stages.scan(thread, evidence);
  const final = await stages.step06(scanned.threadOutRef, evidence);
  expect(final.txHash).toMatch(/^[0-9a-f]{64}$/u);
  return { final, batches: scanned.batches };
};
