/** The validation-trace Open and VerifySource scene: a deposit block seeds
 * three blocks committed from DA fixtures, each with one event that spends a
 * PlutusV3-locked seed (a normal transaction whose script fails, one whose
 * script passes, and a forced transaction whose script fails), and the
 * builders that Init, Open and VerifySource a thread over one of them. The
 * claim opened on the failing block is built from the detection the
 * complete DA replay of that committed block reports. */
import {
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildChallengerDescriptorV1,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  openValidationTraceV1,
} from "@al-ft/midgard-validation";
import {
  generateEmulatorAccount,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import {
  buildValidationClaimWitness,
  type ValidationTraceClaimMaterial,
} from "../../src/validation-trace-dispute/claim-material.js";
import { detectedValidationTraceClaim } from "../../src/validation-trace-dispute/detected-claim.js";
import { plutusFailureReplayInput } from "../../src/validation-trace-dispute/replay.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../../src/workflow/complete-replay.js";
import {
  plutusExecutionBlockFixture,
  type PlutusExecutionScenario,
  plutusExecutionSeedDeposit,
} from "../helpers/plutus-execution-block-fixture.js";
import { awaitHeaderCommitWindow } from "./emulator/family-history.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";
import { submitSuccessorBlockTx } from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  type CompleteSignedTransactionMeasurement,
  type FaultProofEmulatorHarness,
  fixtureBlockSlot,
  fundedProverEmulatorAccount,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./transition-trace-retained.deposit-events-retained-block.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./validation-trace-claim-material.js";

/** L_d: the longest validity range VerifySource admits. */
const MAX_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;
/** The dispute bond `buildMinimalFaultProofContracts` deploys source-v1 with. */
export const BOND_LOVELACE = 10_000_000_000n;

export type Block = Awaited<ReturnType<typeof plutusExecutionBlockFixture>>;

export type Scene = {
  readonly h: FaultProofEmulatorHarness;
  readonly chain: FaultProofEmulatorHarness["contracts"]["fraudProofContracts"]["validationTraceDispute"];
  readonly hubOracle: UTxO;
  readonly caseConfig: UTxO;
  readonly references: {
    readonly opener: UTxO;
    readonly source: UTxO;
    readonly case: UTxO;
  };
  readonly failing: Block;
  readonly passing: Block;
  readonly forced: Block;
};

/** The signed transaction's size and execution units, each within the
 * emulator's transaction limits. */
export const measureWithinTransactionLimits = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const measured = measureCompleteSignedTransaction(transactionCbor);
  expect(measured.completeSignedBytes).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
  );
  expect(measured.executionMemory).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
  );
  expect(measured.executionSteps).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
  );
  return measured;
};

const publishedReference = (h: FaultProofEmulatorHarness, hash: string) => {
  const entry = Object.values(h.faultProofReferenceScripts).find(
    (candidate) => candidate.scriptHash === hash,
  );
  if (entry === undefined) throw new Error(`no published script ${hash}`);
  return entry.utxo;
};

export const setUpScene = async (): Promise<Scene> => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
    accounts: {
      funder: generateEmulatorAccount({ lovelace: 40_000_000_000n }),
      prover: fundedProverEmulatorAccount(60_000_000_000n),
      publisher: generateEmulatorAccount({ lovelace: 40_000_000_000n }),
    },
  });
  const chain = h.contracts.fraudProofContracts.validationTraceDispute;
  const caseReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.proverLucid,
      script: chain.case.spendingScript,
      label: "validation case",
    })
  ).utxo;
  const caseConfigReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.proverLucid,
      script: chain.caseConfig.mintingScript,
      label: "validation case configuration",
    })
  ).utxo;

  const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
  const now = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      h.funderLucid,
      h.emulator.now() + 120_000,
    ) - 1,
  );
  const seeds = {
    failing: 0x61,
    passing: 0x64,
    forced_failing: 0x65,
  } as const;
  const deposits = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: now,
    endTime: now + 1_000n,
    blockSlot: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: Object.entries(seeds).map(([scenario, byte]) =>
      plutusExecutionSeedDeposit(scenario as PlutusExecutionScenario, byte),
    ),
  });
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: {
      ...h.contracts,
      validationCaseConfig: {
        contracts: chain,
        referenceScript: caseConfigReference,
      },
    },
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: deposits.header,
    beforeHeaderCommit: async () =>
      awaitHeaderCommitWindow(h.emulator, deposits.header),
  });
  expect(setup.headerHash).toBe(deposits.headerHash);

  let activeOperatorNode = setup.activeOperatorNode;
  let previous = {
    headerHash: deposits.headerHash,
    endTime: deposits.header.endTime,
    utxosRoot: deposits.header.utxosRoot,
    entries: deposits.postLedgerEntries,
  };
  const commit = async (scenario: PlutusExecutionScenario) => {
    const endTime = previous.endTime + 60_000n;
    const block = await plutusExecutionBlockFixture(scenario, {
      seedByte: seeds[scenario as keyof typeof seeds],
      predecessorEntries: previous.entries,
      header: {
        operatorVkey,
        startTime: previous.endTime,
        endTime,
        blockSlot: fixtureBlockSlot(endTime),
        prevHeaderHash: previous.headerHash,
      },
    });
    expect(block.evidence.header.prevUtxosRoot).toBe(previous.utxosRoot);
    const committed = await submitSuccessorBlockTx({
      lucid: h.funderLucid,
      emulator: h.emulator,
      contracts: h.contracts,
      anchorBlockUnit: toUnit(
        h.contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + previous.headerHash,
      ),
      header: block.evidence.header,
      hubOracle: setup.hubOracle,
      scheduler: setup.scheduler,
      activeOperatorNode,
      activeOperatorNodeUnit: setup.activeOperatorNodeUnit,
    });
    expect(committed.successorHeaderHash).toBe(block.evidence.headerHash);
    activeOperatorNode = committed.activeOperatorNode;
    previous = {
      headerHash: block.evidence.headerHash,
      endTime,
      utxosRoot: block.evidence.header.utxosRoot,
      entries: block.postEntries,
    };
    return block;
  };
  const failing = await commit("failing");
  const passing = await commit("passing");
  const forced = await commit("forced_failing");

  const [caseConfig] = await h.proverLucid.utxosAtWithUnit(
    SDK.validationCaseConfigAddress(network, chain.caseConfig),
    toUnit(chain.caseConfig.policyId, SDK.VALIDATION_CASE_CONFIG_ASSET_NAME),
  );
  if (caseConfig === undefined)
    throw new Error("the case configuration was not initialized");
  return {
    h,
    chain,
    hubOracle: setup.hubOracle,
    caseConfig,
    references: {
      opener: publishedReference(h, chain.opener.spendingScriptHash),
      source: publishedReference(h, chain.source.spendingScriptHash),
      case: caseReference,
    },
    failing,
    passing,
    forced,
  };
};

const replayer =
  createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);

/** The validation-trace detections of the complete DA replay of `block`. */
export const detectOn = async (block: Block) => {
  const decision = await replayer.replay(block.evidence, block.context);
  return requireCompleteCanonicalReplayDecision({
    evidence: block.evidence,
    replayer,
    decision,
    context: block.context,
  });
};

/** The claim material for the failing block's one detection. */
export const failingMaterial = async (
  scene: Scene,
): Promise<ValidationTraceClaimMaterial> => {
  const detections = await detectOn(scene.failing);
  if (detections.length !== 1)
    throw new Error("the failing block has no single detection");
  return (
    await detectedValidationTraceClaim({
      evidence: scene.failing.evidence,
      context: scene.failing.context,
      detection: detections[0]!,
      l1SlotConfig,
    })
  ).material;
};

/** The honest trace of the passing transaction: Terminal Accepted. */
export const passingTrace = async (scene: Scene) => {
  const block = scene.passing;
  const { event, ledger } = await eventAndLedger(block);
  const expectedLedgerOps = [
    { type: "delete" as const, key: block.seed.outRef },
    buildValidationMachineLedgerInsertOp({
      key: block.created.outRef,
      outputCbor: block.created.output,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: ledger.entries.map(({ outRef, output }) => ({
      outRef,
      output,
    })),
    operations: expectedLedgerOps,
  });
  const leaves = await Effect.runPromise(
    openValidationTraceV1(
      {
        ...plutusFailureReplayInput({
          header: block.evidence.header,
          event,
          ledger,
          l1SlotConfig,
        }),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      },
      { revealStates: [0] },
    ),
  );
  const terminal = leaves.states.get(leaves.stepCount)!;
  expect(terminal).toMatchObject({ phase: "terminal", verdict: "accepted" });
  const trace = buildChallengerDescriptorV1(leaves, leaves.stepCount);
  return {
    claim: {
      claimWitness: await buildValidationClaimWitness({
        reconstruction: block.evidence.reconstruction,
        eventKey: event.eventKey,
      }),
      challengerRoot: trace.descriptor.traceRoot.toString("hex"),
      stepCount: BigInt(leaves.stepCount),
    },
    terminal,
    terminalProof: trace.proof(leaves.stepCount),
    initialState: leaves.states.get(0)!,
  };
};

const liveBlock = async (scene: Scene, block: Block): Promise<UTxO> => {
  const { h } = scene;
  const [node] = await h.proverLucid.utxosAtWithUnit(
    h.contracts.stateQueue.spendingScriptAddress,
    toUnit(
      h.contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.evidence.headerHash,
    ),
  );
  if (node === undefined) throw new Error("the challenged block is absent");
  return node;
};

/** Init a validation-trace thread on `block`, then Open it with `claim`
 * within the transaction limits; returns the pending claim at source-v1. */
export const initAndOpen = async (
  scene: Scene,
  block: Block,
  claim: SDK.ValidationDisputeClaim,
): Promise<{ readonly opener: UTxO }> => {
  const { h, chain } = scene;
  const category = h.catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  const blockUtxo = await liveBlock(scene, block);
  const init = await submitResolvedInit({
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    network,
    label: "validation-trace open",
    contracts: {
      steps: [h.contracts.fraudProofs.validationTraceDispute],
      computationThread: h.contracts.computationThread,
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
      stateQueuePolicyId: h.contracts.stateQueue.policyId,
    },
    category,
    catalogue: {
      policyId: h.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        h.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: h.catalogue.root,
    },
    signer: h.proverSigner,
    fraudulentBlockOutRef: `${blockUtxo.txHash}#${blockUtxo.outputIndex.toString()}`,
    witnessReferenceScripts: h.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const thread = (
    await h.proverLucid.utxosAtWithUnit(
      init.firstStepAddress,
      init.computationThreadUnit,
    )
  ).find((utxo) => utxo.txHash === init.txHash);
  if (thread === undefined) throw new Error("the Init thread is absent");
  const challenged = await Effect.runPromise(
    SDK.validationDisputeChallengedBlock(
      blockUtxo,
      h.contracts.stateQueue.policyId,
    ),
  );
  const built = await SDK.appendValidationDisputeOpen(h.proverLucid.newTx(), {
    contracts: { opener: chain.opener, source: chain.source },
    computationThreadPolicyId: h.contracts.computationThread.policyId,
    openerReferenceScript: scene.references.opener,
    thread,
    fraudProver: init.fraudProver,
    hubOracle: scene.hubOracle,
    challenged,
    claim,
  }).complete({ localUPLCEval: true });
  const signed = await built.sign.withWallet().complete();
  measureWithinTransactionLimits(signed.toCBOR());
  const txHash = await signed.submit();
  await h.proverLucid.awaitTx(txHash);
  const opener = (
    await h.proverLucid.utxosAtWithUnit(
      chain.source.spendingScriptAddress,
      init.computationThreadUnit,
    )
  ).find((utxo) => utxo.txHash === txHash);
  if (opener === undefined) throw new Error("the pending claim is absent");
  return { opener };
};

/** VerifySource over `opener`, valid for L_d from the emulator's now (plus
 * `offsetMs`). */
export const verifySource = (
  scene: Scene,
  opener: UTxO,
  presented: {
    readonly terminal: MidgardValidationMachineState;
    readonly terminalProof: MidgardValidationTraceProof;
    readonly initialState: MidgardValidationMachineState;
    readonly bondLovelace?: bigint;
    /** Shifts both bounds off the emulator's slot boundary. */
    readonly offsetMs?: number;
  },
) => {
  const { h, chain } = scene;
  const validFrom = h.emulator.now() + (presented.offsetMs ?? 0);
  const validTo = validFrom + MAX_RANGE_MS;
  const { tx, opening } = SDK.appendValidationSourceVerification(
    h.proverLucid.newTx(),
    {
      contracts: {
        source: chain.source,
        case: chain.case,
        disputeBond: chain.disputeBond,
      },
      computationThreadPolicyId: h.contracts.computationThread.policyId,
      sourceReferenceScript: scene.references.source,
      caseReferenceScript: scene.references.case,
      caseConfig: scene.caseConfig,
      opener,
      challengerTerminal: presented.terminal,
      challengerTerminalProof: presented.terminalProof,
      agreedInitialState: presented.initialState,
      bondLovelace: presented.bondLovelace ?? BOND_LOVELACE,
      validFrom,
      validTo,
      slots: h.proverLucid,
    },
  );
  return {
    opening,
    validTo,
    build: () => tx.complete({ localUPLCEval: true }),
  };
};

export const failingClaim = (material: ValidationTraceClaimMaterial) => ({
  claimWitness: material.claimWitness,
  challengerRoot: material.challengerRoot,
  stepCount: material.stepCount,
});

export const honestPresentation = (material: ValidationTraceClaimMaterial) => ({
  terminal: material.terminal,
  terminalProof: material.terminalLeafProof,
  initialState: material.initialState,
});
