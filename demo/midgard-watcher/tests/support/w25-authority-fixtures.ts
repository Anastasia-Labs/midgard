import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  type CanonicalTransitionEffectV1,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { blake2b } from "../../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { wrapDaPayloadV1 } from "../../../midgard-core/src/da-payload-envelope.js";
import { makeQueued } from "../../../midgard-validation/tests/validation-fixtures.js";
import {
  type EvaluateWatcherBlockReplayInputV1,
  type WatcherBlockReplayEventAuthorityV1,
  watcherBlockReplayPriorStateV1,
  type WatcherBlockReplayPriorUtxoV1,
} from "../../src/block-replay.js";
import {
  evaluateWatcherHeaderRootReconstructionV1,
  makeWatcherAuthenticatedHeaderObservationV1,
} from "../../src/header-root-reconstruction.js";
import {
  evaluateWatcherPhaseABlockV1,
  type WatcherPhaseAVerificationResultV1,
} from "../../src/phase-a-verifier.js";
import {
  computeWatcherRuleBundleV1Commitment,
  makeWatcherCanonicalRuleBundleV1,
} from "../../src/rule-bundle-v1.js";
import type { WatcherStateQueueHeaderV1 } from "../../src/state-queue-indexer.js";
import {
  replayGenuineDepositAuthorityScenarioV1,
  replayGenuineForcedTerminalAuthorityScenarioV1,
  replayGenuineWithdrawalAuthorityScenarioV1,
  type W15AcceptedAuthorityScenarioV1,
  type W15AuthorityScenarioInputV1,
  w15ForcedOperatorVerdictForClassificationV1,
} from "./w15-authority-scenarios.js";
import {
  type GenuineW16SettlementAuthorityV1,
  replayAcceptedW16AuthorityScenarioV1,
  replayGenuineAbsorbToReserveAuthorityScenarioV1,
  replayGenuineRefundWithdrawalAuthorityScenarioV1,
  replayGenuineSpawnSettlementAuthorityScenarioV1,
  type W16AcceptedAuthorityScenarioV1,
  type W16AuthorityScenarioInputV1,
} from "./w16-authority-scenarios.js";

/**
 * W25 deliberately has no record factory.  Each facade call replays the
 * caller's authenticated W15/W16 context through the production parser and
 * returns the original opaque context plus digest evidence.
 */
export type W25UserEventAuthorityFixtureInputV1 = W15AuthorityScenarioInputV1;
export type W25SettlementAuthorityFixtureInputV1 = W16AuthorityScenarioInputV1;

export type AcceptedW25UserEventAuthorityFixtureV1 =
  W15AcceptedAuthorityScenarioV1 & Readonly<{ rawResultDigest: string }>;

export type AcceptedW25SettlementAuthorityFixtureV1 =
  W16AcceptedAuthorityScenarioV1 & Readonly<{ rawResultDigest: string }>;

const userEventFacade = (
  scenario: W15AcceptedAuthorityScenarioV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

const settlementFacade = (
  scenario: W16AcceptedAuthorityScenarioV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

export const makeAcceptedW25DepositAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineDepositAuthorityScenarioV1(input));

export const makeAcceptedW25WithdrawalAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineWithdrawalAuthorityScenarioV1(input));

export const makeAcceptedW25ForcedAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineForcedTerminalAuthorityScenarioV1(input));

export const makeAcceptedW25SpawnSettlementAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineSpawnSettlementAuthorityScenarioV1(input));

export const makeAcceptedW25AbsorbToReserveAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineAbsorbToReserveAuthorityScenarioV1(input));

export const makeAcceptedW25InitializePayoutAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(
    replayAcceptedW16AuthorityScenarioV1(input, "initialize_payout"),
  );

export const makeAcceptedW25RefundWithdrawalAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineRefundWithdrawalAuthorityScenarioV1(input));

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

const L1_PROVENANCE: SDK.EvidenceProvenanceV1 = Object.freeze({
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
});
export const GENUINE_W25_DA_PROVENANCE_V1: SDK.EvidenceProvenanceV1 =
  Object.freeze({
    trustClass: "public_or_permissionless_da",
    sourceId: "watcher-da-peer-1",
    grade: "security",
  });
const CHAIN_POINT = Object.freeze({ slot: 4242n, blockHash: h32(7) });
const RULE_BUNDLE = makeWatcherCanonicalRuleBundleV1({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    releaseEvidenceDigest: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});
const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleV1Commitment(RULE_BUNDLE);

const dataHex = <A>(value: A, schema: Parameters<typeof Data.to>[1]): string =>
  encodeData(value, schema as never).toString("hex");
const sortEntries = (
  values: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...values].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );
const bufferEntries = (values: readonly SDK.DaPayloadEntry[]) =>
  values.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
const headerHashOf = (value: SDK.HeaderV1): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(value, SDK.HeaderV1), "hex"), { dkLen: 28 }),
  ).toString("hex");

type GenuineW25PublicEventV1 = Readonly<{
  eventKey: SDK.EventKey;
  phase: "Withdrawal" | "ForcedTransaction";
  domain: "withdrawals" | "forced_transactions";
  entry: SDK.DaPayloadEntry;
  forcedPreimage?: SDK.DaPayloadEntry;
}>;

const publicEventFromAuthority = (
  authority: W15AcceptedAuthorityScenarioV1,
  canonicalNativeTxCbor: Buffer | null,
): GenuineW25PublicEventV1 => {
  const event = authority.event;
  if (event.kind === "withdrawal") {
    const decoded = Data.from(event.eventCborHex, SDK.WithdrawalEvent) as {
      readonly info: SDK.WithdrawalInfo;
    };
    return Object.freeze({
      eventKey: {
        WithdrawalEventKey: {
          withdrawal_id: Data.from(
            event.eventId,
            SDK.OutputReference as never,
          ) as SDK.OutputReference,
        },
      },
      phase: "Withdrawal",
      domain: "withdrawals",
      entry: [
        event.eventId,
        dataHex(decoded.info, SDK.WithdrawalInfoSchema),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (
    event.kind !== "forced_order" ||
    canonicalNativeTxCbor === null ||
    !("terminalClassification" in event) ||
    event.terminalClassification === undefined
  ) {
    throw new Error("genuine W25 fixture requires an authenticated event");
  }
  const decoded = Data.from(event.eventCborHex, SDK.TxOrderEventV1) as {
    readonly tx: {
      readonly tx_id: string;
      readonly transaction_commitment: string;
      readonly source: SDK.L2TransactionSourceV1["source"];
    };
  };
  return Object.freeze({
    eventKey: {
      ForcedTransactionEventKey: {
        tx_order_id: Data.from(
          event.eventId,
          SDK.OutputReference as never,
        ) as SDK.OutputReference,
      },
    },
    phase: "ForcedTransaction",
    domain: "forced_transactions",
    entry: [
      event.eventId,
      dataHex(
        {
          tx_id: decoded.tx.tx_id,
          source: decoded.tx.source,
          verdict: w15ForcedOperatorVerdictForClassificationV1(
            event.terminalClassification.operatorValidity,
          ),
        },
        SDK.ForcedInclusionTxV1Schema,
      ),
    ] as SDK.DaPayloadEntry,
    forcedPreimage: [
      event.eventId,
      canonicalNativeTxCbor.toString("hex"),
    ] as SDK.DaPayloadEntry,
  });
};

const watcherHeaderRecord = (
  value: SDK.HeaderV1,
  headerHash: string,
): WatcherStateQueueHeaderV1 => ({
  headerHash,
  headerCborHex: Data.to(value, SDK.HeaderV1),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: value.prevUtxosRoot,
  utxosRoot: value.utxosRoot,
  withdrawalsRoot: value.withdrawalsRoot,
  forcedTransactionsRoot: value.forcedTransactionsRoot,
  transactionsRoot: value.transactionsRoot,
  depositsRoot: value.depositsRoot,
  transitionTraceRoot: value.transitionTraceRoot,
  eventToStepRoot: value.eventToStepRoot,
  validationTracesRoot: value.validationTracesRoot,
  withdrawalCount: value.withdrawalCount.toString(),
  forcedTransactionCount: value.forcedTransactionCount.toString(),
  l2TransactionCount: value.l2TransactionCount.toString(),
  depositCount: value.depositCount.toString(),
  totalEventCount: value.totalEventCount.toString(),
  transitionStepCount: value.transitionStepCount.toString(),
  validationTraceCount: value.validationTraceCount.toString(),
  startTime: value.startTime.toString(),
  endTime: value.endTime.toString(),
  blockSlot: value.blockSlot.toString(),
  expectedNetworkId: value.expectedNetworkId.toString(),
  minFeeA: value.minFeeA.toString(),
  minFeeB: value.minFeeB.toString(),
  prevHeaderHash: value.prevHeaderHash,
  operatorVkey: value.operatorVkey,
  protocolVersion: value.protocolVersion.toString(),
  daAttestationPolicyId: null,
});

const settlementAuthority = (
  authority: GenuineW16SettlementAuthorityV1,
): NonNullable<WatcherBlockReplayEventAuthorityV1["settlement"]> => ({
  result: authority.result,
  context: authority.context,
  observationDigest: authority.observation.observationDigest,
});

const eventAuthority = (input: {
  readonly publicEvent: GenuineW25PublicEventV1;
  readonly userEvent: W15AcceptedAuthorityScenarioV1;
  readonly settlement: GenuineW16SettlementAuthorityV1 | null;
  readonly effect: CanonicalTransitionEffectV1;
  readonly canonicalNativeTxCbor: Buffer | null;
}): WatcherBlockReplayEventAuthorityV1 => ({
  eventKey: input.publicEvent.eventKey,
  phase: input.publicEvent.phase,
  userEvent: {
    result: input.userEvent.result,
    context: input.userEvent.context,
  },
  ...(input.settlement === null
    ? {}
    : { settlement: settlementAuthority(input.settlement) }),
  transitionEffect: input.effect,
  ...(input.canonicalNativeTxCbor === null
    ? {}
    : {
        canonicalNativeTxCbor: input.canonicalNativeTxCbor,
        programMaterialSidecarCbor: makeQueued(
          Buffer.alloc(32),
          input.canonicalNativeTxCbor,
        ).programMaterialSidecarCbor,
      }),
});

export type GenuineW25PublicReplayFixtureV1 = Readonly<{
  replayInput: EvaluateWatcherBlockReplayInputV1;
  header: WatcherStateQueueHeaderV1;
  phaseA: WatcherPhaseAVerificationResultV1;
}>;

/**
 * Builds real W21/W22/W23/W24 inputs for one authenticated W15/W16 event.
 * It intentionally does not fabricate or evaluate a W25 receipt; the caller
 * must invoke the public `evaluateWatcherBlockReplayV1` entry point.
 */
export const makeGenuineW25PublicReplayFixtureV1 = async (input: {
  readonly userEvent: W15AcceptedAuthorityScenarioV1;
  readonly settlement?: GenuineW16SettlementAuthorityV1 | null;
  readonly canonicalNativeTxCbor?: Buffer | null;
  readonly transitionEffect: CanonicalTransitionEffectV1;
  readonly priorState: readonly WatcherBlockReplayPriorUtxoV1[];
  readonly postState: readonly WatcherBlockReplayPriorUtxoV1[];
  readonly minFeeB?: bigint;
}): Promise<GenuineW25PublicReplayFixtureV1> => {
  const canonicalNativeTxCbor = input.canonicalNativeTxCbor ?? null;
  const publicEvent = publicEventFromAuthority(
    input.userEvent,
    canonicalNativeTxCbor,
  );
  const prior = await watcherBlockReplayPriorStateV1(input.priorState);
  const operations: ValidationMachineLedgerOp[] =
    input.transitionEffect.operations.map((operation) =>
      operation.type === "delete"
        ? { type: "delete", key: operation.outRefCbor }
        : buildValidationMachineLedgerInsertOpV1({
            key: operation.outRefCbor,
            outputCbor: operation.outputCbor,
          }),
    );
  const mutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: input.priorState.map((entry) => ({
      outRef: Buffer.from(entry.outRef, "hex"),
      output: Buffer.from(entry.outputCbor, "hex"),
    })),
    operations,
  });
  const machinePostRoot = mutationSteps.at(-1)?.postRoot.toString("hex");
  const postRoot =
    machinePostRoot === undefined
      ? prior.root
      : machinePostRoot === "00".repeat(32)
        ? SDK.EMPTY_MERKLE_TREE_ROOT
        : machinePostRoot;
  const step: SDK.TransitionStep = Object.freeze({
    schema_version: 1n,
    step_index: 0n,
    event_key: publicEvent.eventKey,
    phase: publicEvent.phase,
    pre_utxos_root: prior.root,
    post_utxos_root: postRoot,
  });
  const transitionEntries: SDK.DaPayloadEntry[] = [
    [
      dataHex(step.step_index, Data.Integer()),
      dataHex(step, SDK.TransitionStepSchema),
    ],
  ];
  const eventToStepEntries: SDK.DaPayloadEntry[] = [
    [
      dataHex(publicEvent.eventKey, SDK.EventKeySchema),
      dataHex(
        { step_index: 0n, phase: publicEvent.phase },
        SDK.EventToStepValueSchema,
      ),
    ],
  ];
  const withdrawalEntries =
    publicEvent.domain === "withdrawals" ? [publicEvent.entry] : [];
  const forcedEntries =
    publicEvent.domain === "forced_transactions" ? [publicEvent.entry] : [];
  const forcedPreimages =
    publicEvent.forcedPreimage === undefined
      ? []
      : [publicEvent.forcedPreimage];
  const validationTraceEntries: SDK.DaPayloadEntry[] =
    publicEvent.phase === "ForcedTransaction"
      ? [
          [
            dataHex(publicEvent.eventKey, SDK.EventKeySchema),
            dataHex(
              {
                schema_version: 1n,
                machine_version: 1n,
                trace_root: h32(140),
                step_count: 1n,
                initial_state_hash: h32(150),
                terminal_state_hash: h32(160),
                verdict: "Accepted",
                rejection_code_hash: h32(170),
              } satisfies SDK.ValidationTraceDescriptorV1,
              SDK.ValidationTraceDescriptorV1Schema,
            ),
          ],
        ]
      : [];
  const utxoEntries: SDK.DaPayloadEntry[] = input.postState.map((entry) => [
    entry.outRef,
    entry.outputCbor,
  ]);
  const countedRoot = async (
    domain: SDK.RootDomain,
    values: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(values))).root;
  const committedPost = await watcherBlockReplayPriorStateV1(input.postState);
  const inclusionTime = BigInt(input.userEvent.event.inclusionTime);
  if (inclusionTime === 0n)
    throw new Error("genuine event inclusion time is not classifiable");
  const counts = {
    withdrawalCount: BigInt(withdrawalEntries.length),
    forcedTransactionCount: BigInt(forcedEntries.length),
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: BigInt(validationTraceEntries.length),
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot: prior.root,
    utxosRoot: committedPost.root,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEntries,
    ),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedEntries,
    ),
    transactionsRoot: await countedRoot(SDK.ROOT_DOMAINS.transactionsV1, []),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, []),
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
    startTime: inclusionTime - 1n,
    endTime: inclusionTime,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: input.minFeeB ?? 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: BigInt(RULE_BUNDLE.protocolVersion),
  };
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sortEntries(utxoEntries),
      withdrawals: sortEntries(withdrawalEntries),
      forced_transactions: sortEntries(forcedEntries),
      transactions: [],
      deposits: [],
      transition_trace: sortEntries(transitionEntries),
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: [],
      forced_transaction_preimages: sortEntries(forcedPreimages),
      cek_program_material: [],
      validation_traces: sortEntries(validationTraceEntries),
      counts,
    },
  };
  const envelope = await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
    mode: "identity",
  });
  const stateQueueHeader = watcherHeaderRecord(header, headerHash);
  const observation = await makeWatcherAuthenticatedHeaderObservationV1({
    header: stateQueueHeader,
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstructionV1({
    observation,
    payloadEnvelopeCbor: envelope,
    daProvenance: GENUINE_W25_DA_PROVENANCE_V1,
  });
  const phaseA = await evaluateWatcherPhaseABlockV1({
    observation,
    reconstruction,
    payloadEnvelopeCbor: envelope,
    daProvenance: GENUINE_W25_DA_PROVENANCE_V1,
    ruleBundle: RULE_BUNDLE,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
  });
  if (reconstruction.action !== "accept" || phaseA.action !== "accept") {
    throw new Error("genuine W21-W24 fixture did not verify");
  }
  return Object.freeze({
    replayInput: Object.freeze({
      observation,
      reconstruction,
      phaseA,
      payloadEnvelopeCbor: envelope,
      daProvenance: GENUINE_W25_DA_PROVENANCE_V1,
      priorState: input.priorState,
      ruleBundle: RULE_BUNDLE,
      ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
      eventAuthorities: Object.freeze([
        eventAuthority({
          publicEvent,
          userEvent: input.userEvent,
          settlement: input.settlement ?? null,
          effect: input.transitionEffect,
          canonicalNativeTxCbor,
        }),
      ]),
    }),
    header: stateQueueHeader,
    phaseA,
  });
};
