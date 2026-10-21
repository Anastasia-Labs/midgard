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
  type EmulatorAccount,
  generateEmulatorAccount,
  type LucidEvolution,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import { detectedValidationTraceClaim } from "../../src/validation-trace-dispute/detected-claim.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../../src/workflow/complete-replay.js";
import {
  plutusExecutionBlockFixture,
  type PlutusExecutionScenario,
  plutusExecutionSeedDeposit,
  referenceSeedDeposits,
} from "../helpers/plutus-execution-block-fixture.js";
import { recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { awaitHeaderCommitWindow } from "./emulator/family-history.js";
import { submitSuccessorBlockTx } from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  type FaultProofEmulatorHarness,
  fixtureBlockSlot,
  fundedProverEmulatorAccount,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./transition-trace-retained.deposit-events-retained-block.js";
import { l1SlotConfig } from "./validation-trace-claim-material.js";
import { measureWithinTransactionLimits } from "./validation-trace-dispute-transaction-limits.js";

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
    readonly bond: UTxO;
    readonly game: UTxO;
    readonly timeout: UTxO;
    readonly award: UTxO;
    /** The active-operators spending script, which a Defend runs. */
    readonly activeOperators: UTxO;
  };
  readonly failing: Block;
  readonly passing: Block;
  readonly forced: Block;
  /** The operator that committed the blocks, and its active-operator node. */
  readonly operator: { readonly vkey: string; readonly nodeUnit: string };
  /** The operator's account: the funder wallet, which signed the blocks. */
  readonly funder: EmulatorAccount;
  /** The raw L1 view of every transaction after the scene's setup. */
  readonly l1: ReturnType<typeof recordCrossBlockRawEmulator>;
};

/** The scene's harness and dispute contracts, before its blocks exist. */
export type SceneContracts = Pick<Scene, "h" | "chain">;

const publishedReference = (h: FaultProofEmulatorHarness, hash: string) => {
  const entry = Object.values(h.faultProofReferenceScripts).find(
    (candidate) => candidate.scriptHash === hash,
  );
  if (entry === undefined) throw new Error(`no published script ${hash}`);
  return entry.utxo;
};

/**
 * Sets up the scene; `beforeBlocks` runs once its contracts exist and before
 * its blocks are committed, so what it does takes none of the time a
 * dispute move has from the failing block's end. The failing transaction's
 * redeemer is `failingRedeemerData` when given, and it reads
 * `failingReferenceInputs` outputs the deposits create as reference inputs.
 */
export const setUpScene = async (
  beforeBlocks?: (scene: SceneContracts) => Promise<void>,
  {
    failingRedeemerData,
    failingReferenceInputs = 0,
  }: {
    readonly failingRedeemerData?: Uint8Array;
    readonly failingReferenceInputs?: number;
  } = {},
): Promise<Scene> => {
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
    accounts: {
      funder,
      prover: fundedProverEmulatorAccount(60_000_000_000n),
      publisher: generateEmulatorAccount({ lovelace: 40_000_000_000n }),
    },
  });
  const chain = h.contracts.fraudProofContracts.validationTraceDispute;
  const publish = async (script: Script, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: h.proverLucid,
        script,
        label,
      })
    ).utxo;
  const caseReference = await publish(chain.case.spendingScript, "case");
  const bondReference = await publish(chain.disputeBond.spendingScript, "bond");
  const caseConfigReference = await publish(
    chain.caseConfig.mintingScript,
    "validation case configuration",
  );
  const gameReference = await publish(chain.game.spendingScript, "game");
  const timeoutReference = await publish(
    chain.timeout.spendingScript,
    "timeout",
  );
  const awardReference = await publish(chain.award.spendingScript, "award");
  const activeOperatorsReference = await publish(
    h.contracts.activeOperators.spendingScript,
    "active operators",
  );
  await beforeBlocks?.({ h, chain });

  const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
  const now = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      h.funderLucid,
      h.emulator.now() + 120_000,
    ) - 1,
  );
  const seeds = { failing: 0x61, passing: 0x64, forced_failing: 0x65 } as const;
  const references = referenceSeedDeposits(failingReferenceInputs);
  const deposits = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: now,
    endTime: now + 1_000n,
    blockSlot: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [
      ...Object.entries(seeds).map(([scenario, byte]) =>
        plutusExecutionSeedDeposit(scenario as PlutusExecutionScenario, byte),
      ),
      ...references.deposits,
    ],
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
      ...(scenario === "failing" && failingRedeemerData !== undefined
        ? { redeemerData: failingRedeemerData }
        : {}),
      ...(scenario === "failing"
        ? { referenceInputs: references.outRefs }
        : {}),
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
    l1: recordCrossBlockRawEmulator({ emulator: h.emulator }),
    h,
    chain,
    hubOracle: setup.hubOracle,
    caseConfig,
    references: {
      opener: publishedReference(h, chain.opener.spendingScriptHash),
      source: publishedReference(h, chain.source.spendingScriptHash),
      case: caseReference,
      bond: bondReference,
      game: gameReference,
      timeout: timeoutReference,
      award: awardReference,
      activeOperators: activeOperatorsReference,
    },
    failing,
    passing,
    forced,
    operator: { vkey: operatorVkey, nodeUnit: setup.activeOperatorNodeUnit },
    funder,
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

/** VerifySource over `opener`, valid for L_d from now (plus `offsetMs`). */
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
    /** Who submits it; by default the prover, whose claim it verifies. */
    readonly lucid?: LucidEvolution;
  },
) => {
  const { h, chain } = scene;
  const validFrom = h.emulator.now() + (presented.offsetMs ?? 0);
  const validTo = validFrom + MAX_RANGE_MS;
  const { tx, opening } = SDK.appendValidationSourceVerification(
    (presented.lucid ?? h.proverLucid).newTx(),
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
