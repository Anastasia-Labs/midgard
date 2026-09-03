import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec/native";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type CanonicalTransitionEffect,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { blake2b } from "../../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { makeQueued } from "../../../midgard-validation/tests/validation-fixtures.js";
import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-indexer.js";
import {
  type EvaluateWatcherBlockReplayInput,
  type WatcherBlockReplayEventAuthority,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayPriorUtxo,
} from "../../src/verification/block-replay.js";
import {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherAuthenticatedHeaderObservation,
} from "../../src/verification/header-root-reconstruction.js";
import {
  evaluateWatcherPhaseABlock,
  type WatcherPhaseAVerificationResult,
} from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  type GenuineSettlementAuthority,
  replayAcceptedSettlementAuthorityScenario,
  replayGenuineAbsorbToReserveAuthorityScenario,
  replayGenuineRefundWithdrawalAuthorityScenario,
  replayGenuineSpawnSettlementAuthorityScenario,
  type SettlementAcceptedAuthorityScenario,
  type SettlementAuthorityScenarioInput,
} from "./settlement-authority-scenarios.js";
import {
  replayGenuineDepositAuthorityScenario,
  replayGenuineForcedTerminalAuthorityScenario,
  replayGenuineWithdrawalAuthorityScenario,
  type UserEventAcceptedAuthorityScenario,
  type UserEventAuthorityScenarioInput,
  userEventForcedOperatorVerdictForClassification,
} from "./user-event-authority-scenarios.js";

/**
 * W25 deliberately has no record factory.  Each facade call replays the
 * caller's authenticated W15/W16 context through the production parser and
 * returns the original opaque context plus digest evidence.
 */
export type ReplayUserEventAuthorityFixtureInput =
  UserEventAuthorityScenarioInput;
export type ReplaySettlementAuthorityFixtureInput =
  SettlementAuthorityScenarioInput;

export type AcceptedReplayUserEventAuthorityFixture =
  UserEventAcceptedAuthorityScenario & Readonly<{ rawResultDigest: string }>;

export type AcceptedReplaySettlementAuthorityFixture =
  SettlementAcceptedAuthorityScenario & Readonly<{ rawResultDigest: string }>;

const userEventFacade = (
  scenario: UserEventAcceptedAuthorityScenario,
): AcceptedReplayUserEventAuthorityFixture =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

const settlementFacade = (
  scenario: SettlementAcceptedAuthorityScenario,
): AcceptedReplaySettlementAuthorityFixture =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

export const makeAcceptedReplayDepositAuthorityFixture = (
  input: ReplayUserEventAuthorityFixtureInput,
): AcceptedReplayUserEventAuthorityFixture =>
  userEventFacade(replayGenuineDepositAuthorityScenario(input));

export const makeAcceptedReplayWithdrawalAuthorityFixture = (
  input: ReplayUserEventAuthorityFixtureInput,
): AcceptedReplayUserEventAuthorityFixture =>
  userEventFacade(replayGenuineWithdrawalAuthorityScenario(input));

export const makeAcceptedReplayForcedAuthorityFixture = (
  input: ReplayUserEventAuthorityFixtureInput,
): AcceptedReplayUserEventAuthorityFixture =>
  userEventFacade(replayGenuineForcedTerminalAuthorityScenario(input));

export const makeAcceptedReplaySpawnSettlementAuthorityFixture = (
  input: ReplaySettlementAuthorityFixtureInput,
): AcceptedReplaySettlementAuthorityFixture =>
  settlementFacade(replayGenuineSpawnSettlementAuthorityScenario(input));

export const makeAcceptedReplayAbsorbToReserveAuthorityFixture = (
  input: ReplaySettlementAuthorityFixtureInput,
): AcceptedReplaySettlementAuthorityFixture =>
  settlementFacade(replayGenuineAbsorbToReserveAuthorityScenario(input));

export const makeAcceptedReplayInitializePayoutAuthorityFixture = (
  input: ReplaySettlementAuthorityFixtureInput,
): AcceptedReplaySettlementAuthorityFixture =>
  settlementFacade(
    replayAcceptedSettlementAuthorityScenario(input, "initialize_payout"),
  );

export const makeAcceptedReplayRefundWithdrawalAuthorityFixture = (
  input: ReplaySettlementAuthorityFixtureInput,
): AcceptedReplaySettlementAuthorityFixture =>
  settlementFacade(replayGenuineRefundWithdrawalAuthorityScenario(input));

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

const L1_PROVENANCE: SDK.EvidenceProvenance = Object.freeze({
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
});
export const GENUINE_W25_DA_PROVENANCE: SDK.EvidenceProvenance = Object.freeze({
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
});
const CHAIN_POINT = Object.freeze({ slot: 4242n, blockHash: h32(7) });
const RULE_BUNDLE = makeWatcherCanonicalRuleBundle({
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
const RULE_BUNDLE_COMMITMENT = computeWatcherRuleBundleCommitment(RULE_BUNDLE);

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
const headerHashOf = (value: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(value, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

type GenuineReplayPublicEvent = Readonly<{
  eventKey: SDK.EventKey;
  phase: "Withdrawal" | "ForcedTransaction";
  domain: "withdrawals" | "forced_transactions";
  entry: SDK.DaPayloadEntry;
  forcedPreimage?: SDK.DaPayloadEntry;
}>;

const publicEventFromAuthority = (
  authority: UserEventAcceptedAuthorityScenario,
  canonicalNativeTxCbor: Buffer | null,
): GenuineReplayPublicEvent => {
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
  const decoded = Data.from(event.eventCborHex, SDK.TxOrderEvent) as {
    readonly tx: {
      readonly tx_id: string;
      readonly transaction_commitment: string;
      readonly source: SDK.L2TransactionSource["source"];
    };
  };
  const verdict = userEventForcedOperatorVerdictForClassification(
    event.terminalClassification.operatorValidity,
  );
  // The ORDER event binds the SUBMITTED source, but the committed DA leaf
  // carries the operator-ADJUDICATED one (§2.4.3(e)) — the payload
  // reconstruction authenticates exactly that. Re-derive through the single
  // stamping helper by the leaf's verdict rather than copying the event's
  // submitted triple.
  const adjudicatedSource = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(
      decodeMidgardNativeTxFullFromCanonicalCbor(canonicalNativeTxCbor),
      verdict === "ForcedTxValid" ? "TxIsValid" : "TxIsInvalid",
    ),
  );
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
          source: {
            compact_cbor: adjudicatedSource.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              adjudicatedSource.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              adjudicatedSource.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict,
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
  value: SDK.Header,
  headerHash: string,
): WatcherStateQueueHeader => ({
  headerHash,
  headerCborHex: Data.to(value, SDK.Header),
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
  authority: GenuineSettlementAuthority,
): NonNullable<WatcherBlockReplayEventAuthority["settlement"]> => ({
  result: authority.result,
  context: authority.context,
  observationDigest: authority.observation.observationDigest,
});

const eventAuthority = (input: {
  readonly publicEvent: GenuineReplayPublicEvent;
  readonly userEvent: UserEventAcceptedAuthorityScenario;
  readonly settlement: GenuineSettlementAuthority | null;
  readonly effect: CanonicalTransitionEffect;
  readonly canonicalNativeTxCbor: Buffer | null;
}): WatcherBlockReplayEventAuthority => ({
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

export type GenuineReplayPublicReplayFixture = Readonly<{
  replayInput: EvaluateWatcherBlockReplayInput;
  header: WatcherStateQueueHeader;
  phaseA: WatcherPhaseAVerificationResult;
}>;

/**
 * Builds real W21/W22/W23/W24 inputs for one authenticated W15/W16 event.
 * It intentionally does not fabricate or evaluate a W25 receipt; the caller
 * must invoke the public `evaluateWatcherBlockReplayV1` entry point.
 */
export const makeGenuineReplayPublicReplayFixture = async (input: {
  readonly userEvent: UserEventAcceptedAuthorityScenario;
  readonly settlement?: GenuineSettlementAuthority | null;
  readonly canonicalNativeTxCbor?: Buffer | null;
  readonly transitionEffect: CanonicalTransitionEffect;
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly postState: readonly WatcherBlockReplayPriorUtxo[];
  readonly minFeeB?: bigint;
}): Promise<GenuineReplayPublicReplayFixture> => {
  const canonicalNativeTxCbor = input.canonicalNativeTxCbor ?? null;
  const publicEvent = publicEventFromAuthority(
    input.userEvent,
    canonicalNativeTxCbor,
  );
  const prior = await watcherBlockReplayPriorState(input.priorState);
  const operations: ValidationMachineLedgerOp[] =
    input.transitionEffect.operations.map((operation) =>
      operation.type === "delete"
        ? { type: "delete", key: operation.outRefCbor }
        : buildValidationMachineLedgerInsertOp({
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
              } satisfies SDK.ValidationTraceDescriptor,
              SDK.ValidationTraceDescriptorSchema,
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
  const committedPost = await watcherBlockReplayPriorState(input.postState);
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
  const header: SDK.Header = {
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
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
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
      validation_trace_witnesses: [],
      counts,
    },
  };
  const envelope = await wrapDaPayload(SDK.encodeDaPayload(payload), {
    mode: "identity",
  });
  const stateQueueHeader = watcherHeaderRecord(header, headerHash);
  const observation = await makeWatcherAuthenticatedHeaderObservation({
    header: stateQueueHeader,
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstruction({
    observation,
    payloadEnvelopeCbor: envelope,
    daProvenance: GENUINE_W25_DA_PROVENANCE,
  });
  const phaseA = await evaluateWatcherPhaseABlock({
    observation,
    reconstruction,
    payloadEnvelopeCbor: envelope,
    daProvenance: GENUINE_W25_DA_PROVENANCE,
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
      daProvenance: GENUINE_W25_DA_PROVENANCE,
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
