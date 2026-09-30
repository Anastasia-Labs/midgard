import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventAuthority,
} from "../../src/indexers/user-event-indexer.js";
import {
  type WatcherBlockReplayEventAuthority,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayPriorUtxo,
} from "../../src/verification/block-replay.js";
import {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherAuthenticatedHeaderObservation,
} from "../../src/verification/header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  bufferEntries,
  CHAIN_POINT,
  DA_PROVENANCE,
  dataHex,
  headerHashOf,
  L1_PROVENANCE,
  type PublicFixtureEvent,
  type PublicReplayFixture,
  RULE_BUNDLE,
  sortEntries,
  watcherHeaderRecord,
} from "./block-replay-public-fixture.committed-steps-for-effects.js";

export const buildPublicReplayFixture = async (input: {
  readonly txCbors?: readonly Buffer[];
  readonly events?: readonly PublicFixtureEvent[];
  readonly steps: readonly SDK.TransitionStep[];
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly postState: readonly WatcherBlockReplayPriorUtxo[];
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthority[];
  readonly eventToStep?: readonly {
    readonly key: SDK.EventKey;
    readonly value: SDK.EventToStepValue;
  }[];
  readonly requireAcceptedBindings?: boolean;
  readonly minFeeB?: bigint;
  readonly eventWindow?: Readonly<{ start: bigint; end: bigint }>;
  /** Defaults to the fixed test rule bundle; local event authority needs its own. */
  readonly ruleBundle?: WatcherRuleBundle;
}): Promise<PublicReplayFixture> => {
  const ruleBundle = input.ruleBundle ?? RULE_BUNDLE;
  const txCbors = input.txCbors ?? [];
  const events = input.events ?? [];
  const transactions = txCbors.map((canonicalCbor) => {
    const full = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
    const proof =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const source: SDK.L2TransactionSource = {
      tx_id: computeMidgardNativeTxId(full).toString("hex"),
      source: {
        compact_cbor: proof.compactCbor.toString("hex"),
        witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          proof.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    return { canonicalCbor, source };
  });
  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map(
    ({ source }) => [
      source.tx_id,
      dataHex(source, SDK.L2TransactionSourceSchema),
    ],
  );
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map(
    ({ canonicalCbor, source }) => [
      source.tx_id,
      canonicalCbor.toString("hex"),
    ],
  );
  const withdrawalEntries = events
    .filter(({ domain }) => domain === "withdrawals")
    .map(({ entry }) => entry);
  const forcedEntries = events
    .filter(({ domain }) => domain === "forced_transactions")
    .map(({ entry }) => entry);
  const depositEntries = events
    .filter(({ domain }) => domain === "deposits")
    .map(({ entry }) => entry);
  const forcedPreimages = events.flatMap(({ forcedPreimage }) =>
    forcedPreimage === undefined ? [] : [forcedPreimage],
  );
  const transitionEntries: SDK.DaPayloadEntry[] = input.steps.map((step) => [
    dataHex(step.step_index, Data.Integer()),
    dataHex(step, SDK.TransitionStepSchema),
  ]);
  const eventToStep =
    input.eventToStep ??
    input.steps.map((step) => ({
      key: step.event_key,
      value: { step_index: step.step_index, phase: step.phase },
    }));
  const eventToStepEntries: SDK.DaPayloadEntry[] = eventToStep.map(
    ({ key, value }) => [
      dataHex(key, SDK.EventKeySchema),
      dataHex(value, SDK.EventToStepValueSchema),
    ],
  );
  const validationKeys = [
    ...transactions.map(
      ({ source }) =>
        ({
          L2TransactionEventKey: { tx_id: source.tx_id },
        }) satisfies SDK.EventKey,
    ),
    ...events
      .filter(({ domain }) => domain === "forced_transactions")
      .map(({ eventKey }) => eventKey),
  ];
  const validationTraceEntries: SDK.DaPayloadEntry[] = validationKeys.map(
    (eventKey, index) => [
      dataHex(eventKey, SDK.EventKeySchema),
      dataHex(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptor,
        SDK.ValidationTraceDescriptorSchema,
      ),
    ],
  );
  const utxoEntries: SDK.DaPayloadEntry[] = input.postState.map((entry) => [
    entry.outRef,
    entry.outputCbor,
  ]);
  const countedRoot = async (
    domain: SDK.RootDomain,
    values: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(values))).root;
  const priorRoot = await watcherBlockReplayPriorState(input.priorState);
  const postRoot = await watcherBlockReplayPriorState(input.postState);
  const counts = {
    withdrawalCount: BigInt(withdrawalEntries.length),
    forcedTransactionCount: BigInt(forcedEntries.length),
    l2TransactionCount: BigInt(transactionEntries.length),
    depositCount: BigInt(depositEntries.length),
    totalEventCount: BigInt(
      withdrawalEntries.length +
        forcedEntries.length +
        transactionEntries.length +
        depositEntries.length,
    ),
    transitionStepCount: BigInt(transitionEntries.length),
    validationTraceCount: BigInt(validationTraceEntries.length),
  };
  const header: SDK.Header = {
    prevUtxosRoot: priorRoot.root,
    utxosRoot: postRoot.root,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEntries,
    ),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedEntries,
    ),
    transactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactionEntries,
    ),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionEntries,
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepEntries,
    ),
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraceEntries,
    ),
    ...counts,
    startTime: input.eventWindow?.start ?? 10n,
    endTime: input.eventWindow?.end ?? 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: input.minFeeB ?? 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: BigInt(ruleBundle.protocolVersion),
  };
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sortEntries(utxoEntries),
      withdrawals: sortEntries(withdrawalEntries),
      forced_transactions: sortEntries(forcedEntries),
      transactions: sortEntries(transactionEntries),
      deposits: sortEntries(depositEntries),
      transition_trace: sortEntries(transitionEntries),
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: sortEntries(preimageEntries),
      forced_transaction_preimages: sortEntries(forcedPreimages),
      cek_program_material: [],
      validation_traces: sortEntries(validationTraceEntries),
      validation_trace_witnesses: [],
      counts,
    },
  };
  const envelope = await wrapDaPayload(SDK.encodeDaPayload(payload), {
    mode: "identity",
  });
  const observation = await makeWatcherAuthenticatedHeaderObservation({
    header: watcherHeaderRecord(header, headerHash),
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstruction({
    observation,
    payloadEnvelopeCbor: envelope,
    daProvenance: DA_PROVENANCE,
  });
  const phaseA = await evaluateWatcherPhaseABlock({
    observation,
    reconstruction,
    payloadEnvelopeCbor: envelope,
    daProvenance: DA_PROVENANCE,
    ruleBundle,
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
  });
  if (input.requireAcceptedBindings !== false) {
    expect(reconstruction.action).toBe("accept");
    expect(phaseA.action).toBe("accept");
  }
  return {
    observation,
    reconstruction,
    phaseA,
    envelope,
    priorState: input.priorState,
    eventAuthorities: input.eventAuthorities ?? [],
    header,
    ruleBundle,
  };
};

type LocalUserEvent = Awaited<
  ReturnType<typeof readWatcherLocalUserEventAuthority>
>["event"];

export type LocalReplayEvent = Readonly<{
  localUserEvent: WatcherLocalUserEventAuthority;
  event: LocalUserEvent;
  network: WatcherRuleBundle["network"];
}>;

export const readLocalReplayEvent = async (
  localUserEvent: WatcherLocalUserEventAuthority,
): Promise<LocalReplayEvent> => {
  const read = await readWatcherLocalUserEventAuthority(localUserEvent);
  return Object.freeze({
    localUserEvent,
    event: read.event,
    network: read.network,
  });
};
