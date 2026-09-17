import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";

import {
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventAuthority,
} from "../../src/indexers/user-event-indexer.js";
import {
  type EvaluateWatcherBlockReplayInput,
  watcherBlockReplayPriorState,
} from "../../src/verification/block-replay.js";
import { evaluateWatcherHeaderRootReconstruction } from "../../src/verification/header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";

/** Existing ordinary deposit bytes through actual local publication + W22/W24.
 * The structural SDK header observation here is a W25 unit boundary; genuine
 * queue-source acquisition is exercised by the separate source fixture.
 */
export const makeLocalDepositReplayFixture = async (
  localUserEvent: WatcherLocalUserEventAuthority,
  programCommitments: Readonly<Record<string, string>>,
): Promise<EvaluateWatcherBlockReplayInput> => {
  const local = await readWatcherLocalUserEventAuthority(localUserEvent);
  if (local.event.kind !== "deposit")
    throw new Error("local replay fixture requires a deposit");
  const event = local.event;
  const decoded = Data.from(event.eventCborHex, SDK.DepositEvent);
  const value = CML.TransactionOutput.from_cbor_hex(
    event.outputCborHex,
  ).amount();
  const l1Assets: Record<string, bigint> = { lovelace: value.coin() };
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policies = multiasset.keys();
    for (let i = 0; i < policies.len(); i += 1) {
      const policy = policies.get(i);
      const assets = multiasset.get_assets(policy)!;
      const names = assets.keys();
      for (let j = 0; j < names.len(); j += 1) {
        const name = names.get(j);
        l1Assets[`${policy.to_hex()}${name.to_hex()}`] = assets.get(name)!;
      }
    }
  }
  const transitionEffect = deriveCanonicalDepositTransitionEffect({
    configuredNetwork: local.network,
    eventId: decoded.id,
    l2NetworkId: decoded.info.l2_network_id,
    l2Address: decoded.info.l2_address,
    l2DatumCbor:
      decoded.info.l2_datum === null
        ? null
        : Buffer.from(Data.to(decoded.info.l2_datum), "hex"),
    l1Assets,
    depositPolicyId: event.policyId,
    depositAssetNameHex: event.assetNameHex,
  });
  const postState = transitionEffect.operations.map((operation) => {
    if (operation.type !== "insert")
      throw new Error("deposit fixture effect must insert");
    return {
      outRef: operation.outRefCbor.toString("hex"),
      outputCbor: operation.outputCbor.toString("hex"),
    };
  });
  const prior = await watcherBlockReplayPriorState([]);
  const post = await watcherBlockReplayPriorState(postState);
  const eventKey: SDK.EventKey = {
    DepositEventKey: { deposit_id: decoded.id },
  };
  const deposits: SDK.DaPayloadEntry[] = [
    [event.eventId, Data.to(decoded.info, SDK.DepositInfo)],
  ];
  const transition: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Deposit",
          pre_utxos_root: prior.root,
          post_utxos_root: post.root,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const eventToStep: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase: "Deposit" }, SDK.EventToStepValue),
    ],
  ];
  const root = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[] = [],
  ): Promise<string> =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: local.deploymentManifestId,
      blueprintHash: local.blueprintHash,
      network: local.network,
      programCommitments,
    },
    targetParameterSnapshot: { finalityDepth: 30 },
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(ruleBundle);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
  };
  const header: SDK.Header = {
    prevUtxosRoot: prior.root,
    utxosRoot: post.root,
    withdrawalsRoot: await root(SDK.ROOT_DOMAINS.withdrawals),
    forcedTransactionsRoot: await root(SDK.ROOT_DOMAINS.forcedTransactionsV1),
    transactionsRoot: await root(SDK.ROOT_DOMAINS.transactionsV1),
    depositsRoot: await root(SDK.ROOT_DOMAINS.deposits, deposits),
    transitionTraceRoot: await root(
      SDK.ROOT_DOMAINS.transitionTrace,
      transition,
    ),
    eventToStepRoot: await root(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
    validationTracesRoot: await root(SDK.ROOT_DOMAINS.validationTraces),
    ...counts,
    startTime: BigInt(event.inclusionTime) - 1n,
    endTime: BigInt(event.inclusionTime),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "51".repeat(28),
    operatorVkey: "52".repeat(28),
    protocolVersion: BigInt(ruleBundle.protocolVersion),
  };
  const headerHash = computeHash28(
    Buffer.from(Data.to(header, SDK.Header), "hex"),
  ).toString("hex");
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: postState.map((entry) => [entry.outRef, entry.outputCbor]),
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      deposits,
      transition_trace: transition,
      event_to_step: eventToStep,
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDK.encodeDaPayload(payload),
    { mode: "identity" },
  );
  const observation = await SDK.admitAuthenticatedStateQueueHeaderObservation({
    observation: {
      schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
      sourceMode: "local_node",
      provenance: {
        trustClass: "authenticated_cardano_l1",
        sourceId: "local-event-w25-unit",
        grade: "security",
      },
      chainPoint: {
        slot: BigInt(event.originSlot),
        blockHash: event.originBlockHash,
      },
      confirmationDepth: 30,
      headerHash,
      header,
    },
  });
  const daProvenance: SDK.EvidenceProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "local-event-w25-unit-da",
    grade: "security",
  };
  const reconstruction = await evaluateWatcherHeaderRootReconstruction({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
  });
  const phaseA = await evaluateWatcherPhaseABlock({
    observation,
    reconstruction,
    payloadEnvelopeCbor,
    daProvenance,
    ruleBundle,
    ruleBundleCommitment,
  });
  if (reconstruction.action !== "accept" || phaseA.action !== "accept")
    throw new Error("local event W22/W24 fixture did not accept");
  return {
    observation,
    reconstruction,
    phaseA,
    payloadEnvelopeCbor,
    daProvenance,
    priorState: [],
    ruleBundle,
    ruleBundleCommitment,
    eventAuthorities: [
      { eventKey, phase: "Deposit", localUserEvent, transitionEffect },
    ],
  };
};
