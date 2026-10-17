import {
  ActiveOperatorMintRedeemer,
  ConfirmedState,
  CorrectionLockDatum,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getLinkedListNodeViewFromUTxO,
  Header,
  HubOracleDatum,
  makeHubOracleDatum,
  type NodeWithDatum,
  RegisteredOperatorMintRedeemer,
  RetiredOperatorMintRedeemer,
  SchedulerDatum,
  SchedulerMintRedeemer,
  scriptRewardAddress,
  STATE_QUEUE_NODE_MIN_LOVELACE,
  StateQueueRedeemer,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { network } from "./blueprints.js";
import { runEmulatorLifecycleStage } from "./emulator-context.js";
import { SETUP_OUTPUT_INDEX } from "./header-fixtures.js";
import {
  CORRECTION_LOCK_LOVELACE,
  type SetupContracts,
  type SetupLucid,
  type SetupUnits,
} from "./setup-tx.setup-units.js";

/**
 * Transaction 1: mint the hub oracle, scheduler, state-queue root, operator
 * roots, and fraud-proof catalogue in one authored output order.
 */
export const submitInitialMintTx = async ({
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
  readonly header: Header;
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
    .validFrom(Math.max(0, lucid.slotToUnixTime(lucid.currentSlot()) - 60_000))
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
        lovelace: CORRECTION_LOCK_LOVELACE,
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
      // Covers the linked root, so the first commit need not top the root up.
      { [units.stateQueueRoot]: 1n, lovelace: STATE_QUEUE_NODE_MIN_LOVELACE },
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
  if (contracts.validationTraceDispute !== undefined) {
    const rewardAddresses = new Set(
      Object.values(contracts.validationTraceDispute.yields).map(
        ({ withdrawalScript }) =>
          scriptRewardAddress(network, withdrawalScript),
      ),
    );
    for (const rewardAddress of rewardAddresses) {
      builder = builder.register.Stake(rewardAddress);
    }
  }
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

export const nodeWithDatum = async ({
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

/** The node whose key precedes `key` and whose link skips past it. */
export const orderedInsertionAnchor = (
  nodes: readonly NodeWithDatum[],
  key: string,
  label: string,
): NodeWithDatum => {
  const anchor = nodes.find(
    ({ datum }) =>
      (datum.key === "Empty" || datum.key.Key.key < key) &&
      (datum.next === "Empty" || datum.next.Key.key > key),
  );
  if (anchor === undefined)
    throw new Error(`${label} has no insertion anchor for ${key}`);
  return anchor;
};

export const directoryNodes = async (
  lucid: SetupLucid,
  address: string,
  policyId: string,
  label: string,
): Promise<NodeWithDatum[]> =>
  Promise.all(
    (await lucid.utxosAt(address))
      .filter((utxo) =>
        Object.entries(utxo.assets).some(
          ([unit, quantity]) =>
            unit !== "lovelace" && unit.startsWith(policyId) && quantity === 1n,
        ),
      )
      .map((utxo) => nodeWithDatum({ utxo, policyId, label })),
  );
