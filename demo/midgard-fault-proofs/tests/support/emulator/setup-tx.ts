import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  buildActivateOperatorTx,
  buildRegisterOperatorTx,
  ConfirmedState,
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  getProtocolParameters,
  hashBlockHeaderV1,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  makeHubOracleDatum,
  type MidgardValidators,
  type NodeWithDatum,
  REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorDatum,
  RegisteredOperatorMintRedeemer,
  REGISTRATION_DURATION_MS,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
  scriptRewardAddress,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  Lucid,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { network } from "./blueprints.js";
import { ledgerOrderedIndex } from "./catalogue.js";
import {
  firstWalletUtxo,
  largestWalletUtxo,
  requireUtxoWithUnit,
  runEmulatorLifecycleStage,
} from "./emulator-context.js";
import {
  SCHEDULER_APPOINTMENT_OUTPUT_INDEX,
  SETUP_OUTPUT_INDEX,
} from "./header-fixtures.js";
import {
  type MinAdaYieldReferenceScriptsV1,
  publishMinAdaYieldReferenceScriptsV1,
  publishStateQueueYieldReferenceScriptV1,
} from "./reference-scripts.js";
import { type OperatorLifecycleReferenceScriptsV1 } from "./reference-scripts.js";

type SetupLucid = Awaited<ReturnType<typeof Lucid>>;
type SetupContracts = MidgardValidators & {
  readonly operatorLifecycleReferenceScripts?: OperatorLifecycleReferenceScriptsV1;
  readonly minAdaYieldReferenceScripts?: MinAdaYieldReferenceScriptsV1;
  readonly minAda?: {
    readonly yields: MidgardValidators["fraudProofContracts"]["minAda"]["yields"];
  };
};

/** Every asset unit the four setup transactions mint or track. */
const setupUnits = (
  contracts: MidgardValidators,
  header: HeaderV1,
  headerHash: string,
) => ({
  hubOracle: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
  correctionLock: toUnit(
    contracts.hubOracle.policyId,
    CORRECTION_LOCK_ASSET_NAME,
  ),
  fraudProofCatalogue: toUnit(
    contracts.fraudProofCatalogue.policyId,
    FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  ),
  stateQueueBlock: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  ),
  stateQueueRoot: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  ),
  scheduler: toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  activeOperatorsRoot: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ),
  retiredOperatorsRoot: toUnit(
    contracts.retiredOperators.policyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  ),
  activeOperatorNode: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  ),
  registeredOperatorsRoot: toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  ),
});

type SetupUnits = ReturnType<typeof setupUnits>;

/**
 * Lovelace the correction lock is created with.
 *
 * `correction-lock.ak` requires every correction spend to preserve the lock's
 * value exactly (`lock_output.value == own_input.output.value`), so the UTxO
 * can never be topped up after it is minted: it has to be funded once, at
 * creation, for the largest datum it will ever carry. Lucid's automatic
 * min-Ada top-up sizes it for the 3-byte `Idle` datum instead (1,146,460
 * lovelace), which is 70 bytes -- 301,700 lovelace at 4,310 lovelace per byte
 * -- short of the 73-byte `Locked { target_header_hash: bytes(28),
 * correction_identity: FraudProof { bytes(32) } }` datum that the non-terminal
 * removal leg has to write. Under-funding makes every multi-transaction fraud
 * removal unbuildable: the lock output is silently bumped to its own min-Ada,
 * and the exact fraud-slash fee -- the operator bond minus the prover reward,
 * to the lovelace -- has no slack to absorb the difference, so the first
 * transaction fails balancing by exactly 301,700 lovelace. The round figure
 * here clears the 1,448,160-lovelace worst case with margin for datum drift.
 */
const CORRECTION_LOCK_LOVELACE_V1 = 2_000_000n;

const registerStateQueueYieldRewardAccountsV1 = async (
  lucid: SetupLucid,
  contracts: MidgardValidators,
): Promise<void> => {
  const missing: string[] = [];
  for (const { withdrawalScript } of Object.values(
    contracts.stateQueue.yields,
  )) {
    const rewardAddress = scriptRewardAddress(network, withdrawalScript);
    if (!(await lucid.rewardAccountAt(rewardAddress)).registered) {
      missing.push(rewardAddress);
    }
  }
  if (missing.length === 0) return;
  let registration = lucid.newTx();
  for (const rewardAddress of missing) {
    registration = registration.register.Stake(rewardAddress);
  }
  const signed = await (
    await registration.complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await signed.submit());
};

/**
 * Transaction 1: mint the hub oracle, scheduler, state-queue root, operator
 * roots, and fraud-proof catalogue in one authored output order.
 */
const submitInitialMintTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
  units,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
}): Promise<void> => {
  const initialReferences =
    contracts.operatorLifecycleReferenceScripts?.initial;
  if (initialReferences === undefined || initialReferences.length !== 7) {
    throw new Error(
      "initial setup reference scripts must be published before genesis minting",
    );
  }
  const hubOracleDatum = await Effect.runPromise(makeHubOracleDatum(contracts));
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: header.startTime,
    endTime: header.startTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  let builder = lucid
    .newTx()
    .validFrom(Number(header.startTime - 120_000n))
    .validTo(Number(header.startTime + 1n))
    .collectFrom([nonceUtxo])
    // `hub_oracle.mint` requires the exact hub-policy set — hub oracle and
    // correction lock — at equal quantities.
    .mintAssets(
      {
        [units.hubOracle]: 1n,
        [units.correctionLock]: 1n,
      },
      Data.void(),
    )
    .readFrom(initialReferences.map(({ utxo }) => utxo))
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      {
        kind: "inline",
        value: Data.to(hubOracleDatum, HubOracleDatum),
      },
      { [units.hubOracle]: 1n },
    )
    .pay.ToContract(
      contracts.correctionLock.spendingScriptAddress,
      { kind: "inline", value: Data.to("Idle", CorrectionLockDatum) },
      {
        lovelace: CORRECTION_LOCK_LOVELACE_V1,
        [units.correctionLock]: 1n,
      },
    )
    .mintAssets(
      { [units.scheduler]: 1n },
      Data.to("Init", SchedulerMintRedeemer),
    )
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      { [units.scheduler]: 1n },
    )
    // Fixed by the authored setup output order: hub oracle, correction lock, scheduler,
    // state-queue root, active-operators root, retired-operators root, then
    // registered-operators root.
    .mintAssets(
      { [units.stateQueueRoot]: 1n },
      Data.to(
        { InitV1: { output_index: SETUP_OUTPUT_INDEX.stateQueueRoot } },
        StateQueueRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: Data.castTo(confirmedState, ConfirmedState),
        }),
      },
      { [units.stateQueueRoot]: 1n },
    )
    .mintAssets(
      { [units.activeOperatorsRoot]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.activeOperatorsRoot } },
        ActiveOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.activeOperatorsRoot]: 1n },
    )
    .mintAssets(
      { [units.retiredOperatorsRoot]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.retiredOperatorsRoot } },
        RetiredOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.retiredOperatorsRoot]: 1n },
    )
    .mintAssets(
      { [units.registeredOperatorsRoot]: 1n },
      Data.to(
        {
          Init: {
            output_index: SETUP_OUTPUT_INDEX.registeredOperatorsRoot,
          },
        },
        RegisteredOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.registeredOperatorsRoot]: 1n },
    )
    .mintAssets({ [units.fraudProofCatalogue]: 1n }, Data.void())
    .pay.ToAddressWithData(
      contracts.fraudProofCatalogue.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(catalogue.root, FraudProofCatalogueDatum),
      },
      { [units.fraudProofCatalogue]: 1n },
    );
  if (contracts.minAda !== undefined) {
    for (const { withdrawalScript } of Object.values(contracts.minAda.yields)) {
      builder = builder.register.Stake(
        scriptRewardAddress(network, withdrawalScript),
      );
    }
  }
  const mintPolicyOrder = [
    ["hub-oracle", contracts.hubOracle.policyId],
    ["fraud-proof-catalogue", contracts.fraudProofCatalogue.policyId],
    ["scheduler", contracts.scheduler.policyId],
    ["state-queue", contracts.stateQueue.policyId],
    ["active-operators", contracts.activeOperators.policyId],
    ["retired-operators", contracts.retiredOperators.policyId],
    ["registered-operators", contracts.registeredOperators.policyId],
  ]
    .sort((left, right) => left[1]!.localeCompare(right[1]!))
    .map(([label]) => label)
    .join(",");
  const unsigned = await runEmulatorLifecycleStage(
    `setup.initial.complete mint-policy-order=[${mintPolicyOrder}]`,
    () => builder.complete({ localUPLCEval: true }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.initial", async () =>
    lucid.awaitTx(await signed.submit()),
  );
};

const nodeWithDatum = async ({
  utxo,
  policyId,
  label,
}: {
  readonly utxo: UTxO;
  readonly policyId: string;
  readonly label: string;
}): Promise<NodeWithDatum> => {
  const units = Object.entries(utxo.assets).filter(
    ([unit, quantity]) =>
      unit !== "lovelace" && unit.startsWith(policyId) && quantity === 1n,
  );
  if (units.length !== 1) {
    throw new Error(`${label} must carry exactly one linked-list NFT`);
  }
  return {
    utxo,
    datum: await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo)),
    assetName: units[0]![0].slice(56),
  };
};

/**
 * Transactions 2 and 3: genuinely register the header's operator, then move
 * that authenticated node into the active-operators set.
 */
const submitOperatorActivationTx = async ({
  lucid,
  contracts,
  header,
  units,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
}): Promise<void> => {
  const [hubOracleUtxo, activeRootUtxo, retiredRootUtxo, registeredRootUtxo] =
    await Promise.all([
      requireUtxoWithUnit(
        lucid,
        credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOracle.policyId),
        ),
        units.hubOracle,
        "hub oracle after the setup mint",
      ),
      requireUtxoWithUnit(
        lucid,
        contracts.activeOperators.spendingScriptAddress,
        units.activeOperatorsRoot,
        "active-operators root after the setup mint",
      ),
      requireUtxoWithUnit(
        lucid,
        contracts.retiredOperators.spendingScriptAddress,
        units.retiredOperatorsRoot,
        "retired-operators root after the setup mint",
      ),
      requireUtxoWithUnit(
        lucid,
        contracts.registeredOperators.spendingScriptAddress,
        units.registeredOperatorsRoot,
        "registered-operators root after the setup mint",
      ),
    ]);
  const [activeRoot, retiredRoot, registeredRoot] = await Promise.all([
    nodeWithDatum({
      utxo: activeRootUtxo,
      policyId: contracts.activeOperators.policyId,
      label: "active-operators root",
    }),
    nodeWithDatum({
      utxo: retiredRootUtxo,
      policyId: contracts.retiredOperators.policyId,
      label: "retired-operators root",
    }),
    nodeWithDatum({
      utxo: registeredRootUtxo,
      policyId: contracts.registeredOperators.policyId,
      label: "registered-operators root",
    }),
  ]);
  const lifecycleReferences = contracts.operatorLifecycleReferenceScripts;
  if (lifecycleReferences === undefined) {
    throw new Error(
      "operator lifecycle reference scripts must be published before the header clock is sampled",
    );
  }
  const registerValidTo = BigInt(
    lucid.slotToUnixTime(lucid.currentSlot() + 120),
  );
  const activationTime = registerValidTo - 1n + REGISTRATION_DURATION_MS;
  const activationTimeHex = activationTime.toString(16);
  const registrationNodeKey =
    activationTimeHex.length % 2 === 0
      ? activationTimeHex
      : `0${activationTimeHex}`;
  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registrationNodeKey,
  );
  const prependedNodeDatum = {
    key: { Key: { key: registrationNodeKey } },
    next: registeredRoot.datum.next,
    data: Data.castTo(
      { operator: header.operatorVkey },
      RegisteredOperatorDatum,
    ),
  } as const;
  const updatedRegisteredRootDatum = {
    ...registeredRoot.datum,
    next: { Key: { key: registrationNodeKey } },
  } as const;
  const registrationFunding = [
    await largestWalletUtxo(lucid, "operator registration funding"),
  ];
  let registerLayout: Parameters<typeof buildRegisterOperatorTx>[0]["layout"];
  const registerTx = (layout = registerLayout) =>
    buildRegisterOperatorTx({
      lucid,
      contracts,
      operatorKeyHash: header.operatorVkey,
      registeredOperatorScriptRefs: lifecycleReferences.registered,
      hubOracleRefInput: hubOracleUtxo,
      activeNotMemberWitness: activeRoot,
      retiredNotMemberWitness: retiredRoot,
      registeredRootNode: registeredRoot,
      registerFundingInputs: registrationFunding,
      registerMintAssets: { [registeredNodeUnit]: 1n },
      prependedNodeDatum,
      prependedNodeAssets: {
        lovelace: getProtocolParameters(network).required_bond,
        [registeredNodeUnit]: 1n,
      },
      updatedRegisteredRootDatum,
      registerValidTo,
      ...(layout === undefined ? {} : { layout }),
      onLayout: (resolved) => {
        registerLayout = resolved;
      },
    });
  await runEmulatorLifecycleStage(
    `setup.operator-registration.preflight policy=${contracts.registeredOperators.policyId}`,
    () =>
      registerTx().complete({
        localUPLCEval: true,
        presetWalletInputs: [...registrationFunding],
      }),
  );
  if (registerLayout === undefined) {
    throw new Error("operator registration layout was not resolved");
  }
  const registrationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-registration.complete",
    () =>
      registerTx(registerLayout).complete({
        localUPLCEval: true,
        presetWalletInputs: [...registrationFunding],
      }),
  );
  const registrationSigned = await registrationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-registration", async () =>
    lucid.awaitTx(await registrationSigned.submit()),
  );

  const [registeredNodeUtxo, continuedRegisteredRootUtxo] = await Promise.all([
    requireUtxoWithUnit(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
      "registered operator node",
    ),
    requireUtxoWithUnit(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      units.registeredOperatorsRoot,
      "registered-operators root after registration",
    ),
  ]);
  const [registeredNode, continuedRegisteredRoot] = await Promise.all([
    nodeWithDatum({
      utxo: registeredNodeUtxo,
      policyId: contracts.registeredOperators.policyId,
      label: "registered operator node",
    }),
    nodeWithDatum({
      utxo: continuedRegisteredRootUtxo,
      policyId: contracts.registeredOperators.policyId,
      label: "continued registered-operators root",
    }),
  ]);
  const activationFunding = [
    await largestWalletUtxo(lucid, "operator activation funding"),
  ];
  const transferredOperatorAssets = {
    ...registeredNode.utxo.assets,
    [units.activeOperatorNode]: 1n,
  };
  delete transferredOperatorAssets[registeredNodeUnit];
  let activateLayout: Parameters<typeof buildActivateOperatorTx>[0]["layout"];
  const activateTx = (layout = activateLayout) =>
    buildActivateOperatorTx({
      lucid,
      contracts,
      operatorKeyHash: header.operatorVkey,
      registeredOperatorScriptRefs: lifecycleReferences.registered,
      activeOperatorScriptRefs: lifecycleReferences.active,
      hubOracleRefInput: hubOracleUtxo,
      retiredNotMemberWitness: retiredRoot,
      registeredNode,
      registeredAnchor: continuedRegisteredRoot,
      activeAppendAnchor: activeRoot,
      activationFundingInputs: activationFunding,
      validFrom: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
      registeredNodeUnit,
      activeNodeUnit: units.activeOperatorNode,
      transferredOperatorAssets,
      updatedRegisteredAnchorDatum: {
        ...continuedRegisteredRoot.datum,
        next: registeredNode.datum.next,
      },
      ...(layout === undefined ? {} : { layout }),
      onLayout: (resolved) => {
        activateLayout = resolved;
      },
    });
  const activationMintPolicyOrder = [
    ["active-operators", contracts.activeOperators.policyId],
    ["registered-operators", contracts.registeredOperators.policyId],
  ]
    .sort((left, right) => left[1]!.localeCompare(right[1]!))
    .map(([label]) => label)
    .join(",");
  await runEmulatorLifecycleStage(
    `setup.operator-activation.preflight mint-policy-order=[${activationMintPolicyOrder}]`,
    () =>
      activateTx().complete({
        localUPLCEval: true,
        presetWalletInputs: [...activationFunding],
      }),
  );
  if (activateLayout === undefined) {
    throw new Error("operator activation layout was not resolved");
  }
  const activationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-activation.complete",
    () =>
      activateTx(activateLayout).complete({
        localUPLCEval: true,
        presetWalletInputs: [...activationFunding],
      }),
  );
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-activation", async () =>
    lucid.awaitTx(await activationSigned.submit()),
  );
};

/**
 * Transaction 3: appoint the activated operator as the scheduler's first
 * shift, returning the appointed scheduler UTxO.
 */
const submitSchedulerAppointmentTx = async ({
  lucid,
  contracts,
  header,
  units,
  schedulerUtxo,
  activeOperatorNode,
  registeredOperatorsRoot,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
  readonly schedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly registeredOperatorsRoot: UTxO;
}): Promise<UTxO> => {
  const schedulerAppointmentFeeInput = await firstWalletUtxo(
    lucid,
    "scheduler appointment fee input",
  );
  const appointmentInputs = [schedulerAppointmentFeeInput, schedulerUtxo];
  const appointmentRefs = [activeOperatorNode, registeredOperatorsRoot];
  const schedulerAppointmentRedeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: ledgerOrderedIndex(
      appointmentInputs,
      schedulerUtxo,
      "scheduler appointment input",
    ),
    scheduler_output_index: SCHEDULER_APPOINTMENT_OUTPUT_INDEX.scheduler,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          activeOperatorNode,
          "active-operator node appointment reference input",
        ),
        registered_element_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          registeredOperatorsRoot,
          "registered-operators root appointment reference input",
        ),
      },
    },
  };
  const appointmentUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-appointment.complete",
    () =>
      lucid
        .newTx()
        .collectFrom([schedulerAppointmentFeeInput])
        .collectFrom(
          [schedulerUtxo],
          Data.to(schedulerAppointmentRedeemer, SchedulerSpendRedeemer),
        )
        .readFrom(appointmentRefs)
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: Data.to(
              {
                ActiveOperator: {
                  operator: header.operatorVkey,
                  start_time: header.startTime,
                },
              },
              SchedulerDatum,
            ),
          },
          schedulerUtxo.assets,
        )
        .attach.Script(contracts.scheduler.spendingScript)
        .validFrom(Number(header.startTime - 120_000n))
        .validTo(Number(header.startTime + 1n))
        .complete({ localUPLCEval: true }),
  );
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-appointment", async () =>
    lucid.awaitTx(await appointmentSigned.submit()),
  );

  const appointedSchedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after the appointment transaction",
  );
  expect(Data.from(appointedSchedulerUtxo.datum!, SchedulerDatum)).toEqual({
    ActiveOperator: {
      operator: header.operatorVkey,
      start_time: header.startTime,
    },
  });
  return appointedSchedulerUtxo;
};

/**
 * Transaction 4: commit the (fraudulent) header onto the state queue behind
 * the root, holding the operator's bond.
 */
const submitHeaderCommitTx = async ({
  lucid,
  contracts,
  header,
  headerHash,
  units,
  hubOracleUtxo,
  correctionLockUtxo,
  stateQueueRootUtxo,
  appointedSchedulerUtxo,
  activeOperatorNode,
  confirmedStateRefInput,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
  readonly headerHash: string;
  readonly units: SetupUnits;
  readonly hubOracleUtxo: UTxO;
  readonly correctionLockUtxo: UTxO;
  readonly stateQueueRootUtxo: UTxO;
  readonly appointedSchedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly confirmedStateRefInput?: UTxO;
}): Promise<{
  readonly fraudulentBlockUtxo: UTxO;
  readonly continuedActiveOperatorNode: UTxO;
}> => {
  await registerStateQueueYieldRewardAccountsV1(lucid, contracts);
  const commitYieldPublication = await publishStateQueueYieldReferenceScriptV1({
    lucid,
    contracts,
    arm: "commit",
  });
  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueue.policyId),
  );
  const commitFeeInput = await firstWalletUtxo(lucid, "commit fee input");
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[units.activeOperatorNode] ?? 0n) === 1n,
            "commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: appointedSchedulerUtxo,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        ...(confirmedStateRefInput === undefined
          ? {}
          : { confirmedStateRefInput }),
        additionalRefInputs: [hubOracleUtxo],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: commitYieldPublication.utxo,
          script: contracts.stateQueue.yields.commit.withdrawalScript,
        },
      },
    ),
  );
  const commitUnsigned = await runEmulatorLifecycleStage(
    "setup.header-commit.complete",
    () => commitTx.complete({ localUPLCEval: true }),
  );
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.header-commit", async () =>
    lucid.awaitTx(await commitSigned.submit()),
  );

  const fraudulentBlockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueBlock,
    "committed block after the header commit",
  );
  const continuedRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after the header commit",
  );
  const continuedActiveOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after the header commit",
  );
  const committedBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(fraudulentBlockUtxo, contracts.stateQueue.policyId),
  );
  const committedHeader = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(committedBlock.datum),
  );
  expect(committedHeader.transactionsRoot).toBe(header.transactionsRoot);
  const continuedRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
  );
  expect(continuedRoot.datum.next).toEqual({
    Key: {
      key:
        confirmedStateRefInput === undefined
          ? headerHash
          : header.prevHeaderHash,
    },
  });
  return { fraudulentBlockUtxo, continuedActiveOperatorNode };
};

/**
 * The four-transaction setup journey every emulator suite starts from:
 * initial mint, operator activation, scheduler appointment, then the
 * fraudulent header commit.
 */
export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
}): Promise<{
  readonly fraudulentBlockOutRef: string;
  readonly headerHash: string;
  readonly stateQueueBlockUnit: string;
  readonly stateQueueRootUnit: string;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRoot: UTxO;
  readonly retiredOperatorsRootUnit: string;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  readonly registeredOperatorsRoot: UTxO;
  readonly minAdaYieldReferenceScripts?: MinAdaYieldReferenceScriptsV1;
}> => {
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const units = setupUnits(contracts, header, headerHash);

  await submitInitialMintTx({
    lucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
    units,
  });
  const minAdaYieldReferenceScripts =
    contracts.minAda === undefined
      ? undefined
      : (contracts.minAdaYieldReferenceScripts ??
        (await publishMinAdaYieldReferenceScriptsV1({ lucid, contracts })));
  await submitOperatorActivationTx({ lucid, contracts, header, units });

  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    units.hubOracle,
    "hub oracle after setup",
  );
  const stateQueueRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after setup",
  );
  const correctionLockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
    "correction lock after setup",
  );
  const schedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after setup",
  );
  const activeOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after activation",
  );
  const activeOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
    "active-operators root after activation",
  );
  const retiredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.retiredOperators.spendingScriptAddress,
    units.retiredOperatorsRoot,
    "retired-operators root after setup",
  );
  const registeredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.registeredOperators.spendingScriptAddress,
    units.registeredOperatorsRoot,
    "registered-operators root after setup",
  );

  const appointedSchedulerUtxo = await submitSchedulerAppointmentTx({
    lucid,
    contracts,
    header,
    units,
    schedulerUtxo,
    activeOperatorNode,
    registeredOperatorsRoot,
  });
  const { fraudulentBlockUtxo, continuedActiveOperatorNode } =
    await submitHeaderCommitTx({
      lucid,
      contracts,
      header,
      headerHash,
      units,
      hubOracleUtxo,
      correctionLockUtxo,
      stateQueueRootUtxo,
      appointedSchedulerUtxo,
      activeOperatorNode,
    });

  return {
    fraudulentBlockOutRef: `${fraudulentBlockUtxo.txHash}#${fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
    stateQueueBlockUnit: units.stateQueueBlock,
    stateQueueRootUnit: units.stateQueueRoot,
    hubOracle: hubOracleUtxo,
    scheduler: appointedSchedulerUtxo,
    activeOperatorsRoot,
    activeOperatorsRootUnit: units.activeOperatorsRoot,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit: units.retiredOperatorsRoot,
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit: units.activeOperatorNode,
    registeredOperatorsRoot,
    ...(minAdaYieldReferenceScripts === undefined
      ? {}
      : { minAdaYieldReferenceScripts }),
  };
};

/**
 * Commit one additional authenticated header after `submitSetupTx` has
 * established the operator, scheduler, and state-queue root. Existing setup
 * callers retain the original single-header behavior; this helper only
 * advances when explicitly invoked by a multi-header lifecycle fixture.
 */
export const submitSecondHeaderTxV1 = async ({
  lucid,
  contracts,
  header,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
}): Promise<{
  readonly blockOutRef: string;
  readonly headerHash: string;
}> => {
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const units = setupUnits(contracts, header, headerHash);
  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    units.hubOracle,
    "hub oracle before second header",
  );
  const correctionLockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
    "correction lock before second header",
  );
  const stateQueueRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root before second header",
  );
  const previousBlockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header.prevHeaderHash,
    ),
    "previous block before second header",
  );
  const appointedSchedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler before second header",
  );
  const activeOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active operator before second header",
  );
  const committed = await submitHeaderCommitTx({
    lucid,
    contracts,
    header,
    headerHash,
    units,
    hubOracleUtxo,
    correctionLockUtxo,
    stateQueueRootUtxo: previousBlockUtxo,
    confirmedStateRefInput: stateQueueRootUtxo,
    appointedSchedulerUtxo,
    activeOperatorNode,
  });
  return {
    blockOutRef: `${committed.fraudulentBlockUtxo.txHash}#${committed.fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
  };
};
