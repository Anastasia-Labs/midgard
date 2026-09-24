/**
 * Public W21/W22/W23/W24-bound block-replay input builder shared by the W25
 * suites, plus the originating-event adapters for private local user-event
 * publication (the only W25 event authority).
 */
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  buildCanonicalTransitionEffect,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  canonicalCommittedWithdrawalTransitionEffect,
  type CanonicalTransitionEffect,
  deriveCanonicalOriginalDepositTransitionEffect,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import {
  makeNativeTx,
  makeQueued,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect } from "vitest";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-snapshot.js";
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
  type WatcherHeaderRootReconstructionResult,
} from "../../src/verification/header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";
import type { makeForcedTxFixture } from "./forced-submission-fixture.js";

export const entries = (
  values: readonly (readonly [Buffer, Buffer])[],
): readonly WatcherBlockReplayPriorUtxo[] =>
  values.map(([outRef, output]) => ({
    outRef: outRef.toString("hex"),
    outputCbor: output.toString("hex"),
  }));

export const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

export const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

export const RULE_BUNDLE: WatcherRuleBundle = makeWatcherCanonicalRuleBundle({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    blueprintHash: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});

export const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleCommitment(RULE_BUNDLE);

export const headerHashOf = (value: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(value, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

export const sortEntries = (
  values: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...values].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const bufferEntries = (values: readonly SDK.DaPayloadEntry[]) =>
  values.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export const dataHex = <A>(
  value: A,
  schema: Parameters<typeof Data.to>[1],
): string => encodeData(value, schema as never).toString("hex");

export const cardanoOutputAssets = (
  outputCborHex: string,
): Readonly<Record<string, bigint>> => {
  const value = CML.TransactionOutput.from_cbor_hex(outputCborHex).amount();
  const assets: Record<string, bigint> = { lovelace: value.coin() };
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policies = multiasset.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const policyAssets = multiasset.get_assets(policy);
      if (policyAssets === undefined) continue;
      const names = policyAssets.keys();
      for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
        const name = names.get(nameIndex);
        const quantity = policyAssets.get(name);
        if (quantity !== undefined) {
          assets[`${policy.to_hex()}${name.to_hex()}`] = quantity;
        }
      }
    }
  }
  return Object.freeze(assets);
};

export const nativeEffect = (input: {
  readonly spent: readonly Buffer[];
  readonly native: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">;
  readonly outputs: readonly Buffer[];
}): CanonicalTransitionEffect =>
  buildCanonicalTransitionEffect([
    ...input.spent.map((outRefCbor) => ({
      type: "delete" as const,
      outRefCbor,
    })),
    ...input.outputs.map((outputCbor, outputIndex) => ({
      type: "insert" as const,
      outRefCbor: outRefFromTxId(input.native.txId, BigInt(outputIndex)),
      outputCbor,
    })),
  ]);

export type CommittedEffectGroup = Readonly<{
  eventKey: SDK.EventKey;
  phase: SDK.TransitionPhase;
  effect: CanonicalTransitionEffect;
}>;

export const committedStepsForEffects = async (
  priorState: readonly WatcherBlockReplayPriorUtxo[],
  groups: readonly CommittedEffectGroup[],
): Promise<readonly SDK.TransitionStep[]> => {
  const prior = await watcherBlockReplayPriorState(priorState);
  const operations: ValidationMachineLedgerOp[] = groups.flatMap(({ effect }) =>
    effect.operations.map((operation) =>
      operation.type === "delete"
        ? { type: "delete" as const, key: operation.outRefCbor }
        : buildValidationMachineLedgerInsertOp({
            key: operation.outRefCbor,
            outputCbor: operation.outputCbor,
          }),
    ),
  );
  const mutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: priorState.map((entry) => ({
      outRef: Buffer.from(entry.outRef, "hex"),
      output: Buffer.from(entry.outputCbor, "hex"),
    })),
    operations,
  });
  let root = prior.root;
  let cursor = 0;
  return Object.freeze(
    groups.map((group, stepIndex) => {
      const preRoot = root;
      cursor += group.effect.operations.length;
      if (group.effect.operations.length > 0) {
        const machineRoot = mutationSteps[cursor - 1]!.postRoot.toString("hex");
        root =
          machineRoot === "00".repeat(32)
            ? SDK.EMPTY_MERKLE_TREE_ROOT
            : machineRoot;
      }
      return Object.freeze({
        schema_version: 1n,
        step_index: BigInt(stepIndex),
        event_key: group.eventKey,
        phase: group.phase,
        pre_utxos_root: preRoot,
        post_utxos_root: root,
      });
    }),
  );
};

export const watcherHeaderRecord = (
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

export type PublicFixtureEvent = Readonly<{
  eventKey: SDK.EventKey;
  phase: Exclude<SDK.TransitionPhase, "L2Transaction">;
  domain: "withdrawals" | "forced_transactions" | "deposits";
  entry: SDK.DaPayloadEntry;
  forcedPreimage?: SDK.DaPayloadEntry;
}>;

export type PublicReplayFixture = Readonly<{
  observation: SDK.AuthenticatedStateQueueHeaderObservation;
  reconstruction: WatcherHeaderRootReconstructionResult;
  phaseA: Awaited<ReturnType<typeof evaluateWatcherPhaseABlock>>;
  envelope: Buffer;
  priorState: readonly WatcherBlockReplayPriorUtxo[];
  eventAuthorities: readonly WatcherBlockReplayEventAuthority[];
  header: SDK.Header;
  ruleBundle: WatcherRuleBundle;
}>;

export const publicInput = (fixture: PublicReplayFixture) => ({
  observation: fixture.observation,
  reconstruction: fixture.reconstruction,
  phaseA: fixture.phaseA,
  payloadEnvelopeCbor: fixture.envelope,
  daProvenance: DA_PROVENANCE,
  priorState: fixture.priorState,
  eventAuthorities: fixture.eventAuthorities,
  ruleBundle: fixture.ruleBundle,
  ruleBundleCommitment: computeWatcherRuleBundleCommitment(fixture.ruleBundle),
});

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

/** The DA-committed claim for a locally published originating event. */
export const publicEventFromLocal = (
  local: LocalReplayEvent,
  options: Readonly<{
    forcedNative?: ReturnType<typeof makeForcedTxFixture>;
    withdrawalValidity?: SDK.WithdrawalValidity;
    forcedVerdict?: SDK.OperatorVerdict;
  }> = {},
): PublicFixtureEvent => {
  const event = local.event;
  const outputReference = Data.from(
    event.eventId,
    SDK.OutputReference as never,
  ) as SDK.OutputReference;
  if (event.kind === "deposit") {
    const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
      readonly info: SDK.DepositInfo;
    };
    return Object.freeze({
      eventKey: { DepositEventKey: { deposit_id: outputReference } },
      phase: "Deposit" as const,
      domain: "deposits" as const,
      entry: [
        event.eventId,
        dataHex(decoded.info, SDK.DepositInfoSchema),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (event.kind === "withdrawal") {
    const decoded = Data.from(event.eventCborHex, SDK.WithdrawalEvent) as {
      readonly info: SDK.WithdrawalInfo;
    };
    return Object.freeze({
      eventKey: { WithdrawalEventKey: { withdrawal_id: outputReference } },
      phase: "Withdrawal" as const,
      domain: "withdrawals" as const,
      entry: [
        event.eventId,
        SDK.committedWithdrawalValueBytes({
          ...decoded.info,
          validity: options.withdrawalValidity ?? decoded.info.validity,
        }),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (options.forcedNative === undefined) {
    throw new Error("forced public event requires canonical native bytes");
  }
  const decoded = Data.from(event.eventCborHex, SDK.TxOrderEvent) as {
    readonly tx: {
      readonly tx_id: string;
      readonly submitted_source: SDK.ForcedTxProofSource;
    };
  };
  return Object.freeze({
    eventKey: { ForcedTransactionEventKey: { tx_order_id: outputReference } },
    phase: "ForcedTransaction" as const,
    domain: "forced_transactions" as const,
    entry: [
      event.eventId,
      dataHex(
        {
          tx_id: decoded.tx.tx_id,
          submitted_source: {
            compact_cbor: decoded.tx.submitted_source.compact_cbor,
            witness_set_compact_cbor:
              decoded.tx.submitted_source.witness_set_compact_cbor,
            field_preimage_lengths_cbor:
              decoded.tx.submitted_source.field_preimage_lengths_cbor,
          },
          verdict: options.forcedVerdict ?? ("ForcedTxValid" as const),
        },
        SDK.ForcedInclusionTxV1Schema,
      ),
    ] as SDK.DaPayloadEntry,
    forcedPreimage: [
      event.eventId,
      options.forcedNative.txCbor.toString("hex"),
    ] as SDK.DaPayloadEntry,
  });
};

/** Header window containing the local event's inclusion time. */
export const localEventWindow = (
  local: LocalReplayEvent,
): Readonly<{ start: bigint; end: bigint }> => {
  const inclusion = BigInt(local.event.inclusionTime);
  return Object.freeze({ start: inclusion - 1n, end: inclusion });
};

export const depositEffectFromLocal = (
  local: LocalReplayEvent,
): CanonicalTransitionEffect => {
  const event = local.event;
  if (event.kind !== "deposit") {
    throw new Error("local authority is not a deposit");
  }
  const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
    readonly id: SDK.OutputReference;
    readonly info: SDK.DepositInfo;
  };
  return deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: local.network,
    eventId: decoded.id,
    l2NetworkId: decoded.info.l2_network_id,
    l2Address: decoded.info.l2_address,
    l2DatumCbor:
      decoded.info.l2_datum === null
        ? null
        : Buffer.from(
            plutusConstrFieldCbor(event.eventCborHex, [1, 2, 0]),
            "hex",
          ),
    // Structural list funds remain on L1 and must not enter the expected L2
    // transition. Derive the fixture independently from the authenticated Order.
    originalAssets: SDK.eventHistoryOriginalAssets(
      Data.from(event.datumCborHex, SDK.EventHistoryNode),
      cardanoOutputAssets(event.outputCborHex),
      event.policyId,
    ),
  });
};

export const withdrawalEffectFromLocal = (
  local: LocalReplayEvent,
  committedValid: boolean,
): CanonicalTransitionEffect => {
  if (local.event.kind !== "withdrawal") {
    throw new Error("local authority is not a withdrawal");
  }
  const decoded = Data.from(local.event.eventCborHex, SDK.WithdrawalEvent) as {
    readonly info: SDK.WithdrawalInfo;
  };
  const outRef = decoded.info.body.l2_outref;
  return canonicalCommittedWithdrawalTransitionEffect({
    committedValid,
    // The Plutus-Data `OutputReference` in the event datum is a *different*
    // encoding from the ledger out-ref; going from one to the other means
    // re-encoding through §5.3's fixed-index field-0/1 item, never CML's
    // minimal-index `TransactionInput` CBOR.
    outRefCbor: outRefFromTxId(
      Buffer.from(outRef.transactionId, "hex"),
      outRef.outputIndex,
    ),
  });
};

export const localEventAuthority = (input: {
  readonly event: PublicFixtureEvent;
  readonly local: LocalReplayEvent;
  readonly effect: CanonicalTransitionEffect;
  readonly forcedNative?: ReturnType<typeof makeForcedTxFixture>;
}): WatcherBlockReplayEventAuthority => {
  const common = {
    eventKey: input.event.eventKey,
    localUserEvent: input.local.localUserEvent,
  };
  if (input.event.phase === "ForcedTransaction") {
    if (input.forcedNative === undefined) {
      throw new Error("forced replay fixture requires canonical native bytes");
    }
    return {
      ...common,
      phase: input.event.phase,
      canonicalNativeTxCbor: input.forcedNative.txCbor,
      programMaterialSidecarCbor: makeQueued(
        input.forcedNative.txId,
        input.forcedNative.txCbor,
      ).programMaterialSidecarCbor,
    };
  }
  return {
    ...common,
    phase: input.event.phase,
    transitionEffect: input.effect,
  };
};
