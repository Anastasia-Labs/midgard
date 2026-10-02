import {
  AddressData,
  addressDataFromBech32,
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  forcedVerdictSubject,
  type RejectionReason,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data, type Script, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

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
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import { buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { type CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";
import { expectProofFit } from "./emulator/proof-fit.js";
import { type MissingScriptSourceFixture } from "./missing-script-source-emulator.build-missing-script-source-fixture.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  emulatorSuccessorHeaderStart,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

/** The exact prover reconstruction from the fixture's public retained DA. */
export const buildMissingScriptSourceUniverse = (
  fixture: MissingScriptSourceFixture,
  expectedPresence = fixture.shape.presentAt !== "absent",
  purposeIndex = 0,
) =>
  buildRetainedMissingScriptSourceUniverse({
    eventKey: fixture.eventKey,
    purposeKind: fixture.shape.purposeKind,
    purposeIndex,
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
