import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";

import {
  buildMidgardValidationTraceTree,
  computeMidgardForcedTxProofCommitment,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  midgardFieldCommitment,
  MidgardValidationPhase,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asDataType, asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import type { AuthenticatedStateQueueHeaderObservation } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type DaPayload,
  type DaPayloadEntry,
  encodeDaPayload,
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  GENESIS_HEADER_HASH,
  hashBlockHeader,
  rejectionCodeOf,
  type RejectionReason,
  retainedValidationEndpointCoordinate,
  retainedValidationStateCoordinate,
  ROOT_DOMAINS,
  TransitionStep,
  type ValidationAuxiliaryWitness,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  type ValidationTraceDescriptor,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildMidgardCanonicalCekProgram,
  type DeterministicValidationMachineTrace,
  replayValidationMachineEvent,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  CML,
  Constr,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { RetainedDaPayloadSource } from "../../src/transition-trace/fetch.js";
import { captureTransitionTraceL1Events } from "../../src/transition-trace/l1-events.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "../../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import {
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
} from "../../src/workflow/historical-native-script-corpus.js";
import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
} from "../../src/workflow/raw-l1-snapshot.js";
import type { FraudProofReleaseFinalityAuthority } from "../../src/workflow/release-finality-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "../helpers/canonical-block-evidence-fixture.js";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "../helpers/transition-history-fixture.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

/** Retains existing machine states under the operator's exact claimed verdict. */
export const retainValidationTrace = ({
  trace,
  eventKey,
  claim,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly eventKey: EventKey;
  readonly claim:
    | { readonly verdict: "accepted" }
    | { readonly verdict: "rejected"; readonly reason: RejectionReason };
}) => {
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    claim.verdict,
    claim.verdict === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode(
          Buffer.from(rejectionCodeOf(claim.reason), "hex").toString("ascii"),
        ),
  );
  const stepCount = BigInt(tree.descriptor.stepCount);
  const descriptor: ValidationTraceDescriptor = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: stepCount,
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict: claim.verdict === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: Buffer.from(Data.to(eventKey, asLucidSchema(EventKeySchema)), "hex"),
      value: Buffer.from(
        Data.to(descriptor, asLucidSchema(ValidationTraceDescriptorSchema)),
        "hex",
      ),
    },
  ];
  const entry = (
    index: number,
    coordinate: bigint,
    endpoint?: "initial" | "terminal",
  ) => {
    const witness = trace.witnesses[index]!;
    return {
      key: encodeRetainedValidationWitnessKey({
        event_key: eventKey,
        execution_index: coordinate,
      }),
      value: encodeRetainedValidationWitness({
        machine_state: validationMachineStateDataFromCore(trace.states[index]!),
        trace_proof: validationTraceProofDataFromCore(tree.proofs[index]!),
        phase:
          endpoint === "initial"
            ? -1n
            : BigInt(MidgardValidationPhase[witness.phase]),
        program_counter: BigInt(witness.programCounter),
        witness_cbor:
          endpoint === "initial"
            ? trace.validationContextCbor.toString("hex")
            : witness.cbor.toString("hex"),
        auxiliary:
          endpoint === undefined
            ? Data.from(
                Data.to<unknown>(
                  validationAuxiliaryWitnessData(witness.auxiliary),
                ),
                asDataType<ValidationAuxiliaryWitness>(
                  ValidationAuxiliaryWitnessSchema,
                ),
              )
            : "NoAuxiliaryWitness",
      }),
    };
  };
  const retainedEntries = trace.witnesses.map((witness, index) =>
    entry(
      index,
      witness.auxiliary?.kind === "nativeExecutionDescriptor"
        ? BigInt(witness.auxiliary.executionIndex)
        : retainedValidationStateCoordinate(stepCount, BigInt(index)),
    ),
  );
  retainedEntries.push(
    entry(
      0,
      retainedValidationEndpointCoordinate(stepCount, "initial"),
      "initial",
    ),
  );
  retainedEntries.push(
    entry(
      trace.states.length - 1,
      retainedValidationEndpointCoordinate(stepCount, "terminal"),
      "terminal",
    ),
  );
  return { descriptorEntries, retainedEntries };
};

/** Retains ordinary machine states under the operator's exact claimed rejection. */
export const retainRejectedValidationTrace = ({
  trace,
  eventKey,
  reason,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly eventKey: EventKey;
  readonly reason: RejectionReason;
}) =>
  retainValidationTrace({
    trace,
    eventKey,
    claim: { verdict: "rejected", reason },
  });

/** Commits existing small validation fixtures into the same DA/header boundary as classification. */
export const buildRetainedValidationBlockFixture = async ({
  subject,
  priorLedgerRoot,
  descriptorEntries,
  retainedEntries,
  blockEndTimeMs,
  blockStartTimeMs = blockEndTimeMs - 60_000,
  blockSlot = 100n,
  minFeeA = 0n,
  minFeeB = 0n,
  operatorVkey = "b1".repeat(28),
  prevHeaderHash,
  programMaterialEntries,
  postLedgerEntries,
}: {
  readonly subject: Parameters<typeof buildDecodingBlockFixture>[0]["subject"];
  readonly priorLedgerRoot: string;
  readonly descriptorEntries?: readonly { key: Buffer; value: Buffer }[];
  readonly retainedEntries?: readonly { key: Buffer; value: Buffer }[];
  readonly blockEndTimeMs: number;
  readonly blockStartTimeMs?: number;
  readonly blockSlot?: bigint;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
  readonly operatorVkey?: string;
  readonly prevHeaderHash?: string;
  readonly programMaterialEntries?: readonly DaPayloadEntry[];
  readonly postLedgerEntries?: readonly { outRef: Buffer; output: Buffer }[];
}) => {
  const base = await buildDecodingBlockFixture({
    subject,
    priorLedgerRoot,
    operatorVkey,
    startTime: BigInt(blockStartTimeMs),
  });
  const descriptors =
    descriptorEntries ??
    base.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptors,
  );
  const postLedger =
    postLedgerEntries === undefined
      ? undefined
      : await keyValuePhasRootWithCount(
          postLedgerEntries.map(({ outRef, output }) => ({
            key: outRef,
            value: buildCanonicalMidgardLedgerEntryOutputMaterial({
              outRef,
              outputCbor: output,
            }).descriptorCbor,
          })),
        );
  const transitionEntries = base.reconstruction.transitionTrace.map(
    (entry) => ({
      key: entry.keyBytes,
      value: Buffer.from(
        Data.to(
          {
            ...entry.value,
            ...(postLedger === undefined
              ? {}
              : { post_utxos_root: postLedger.root }),
          },
          TransitionStep,
        ),
        "hex",
      ),
    }),
  );
  const transitionRoot = await buildCountedRoot(
    ROOT_DOMAINS.transitionTrace,
    transitionEntries,
  );
  const header = {
    ...base.header,
    endTime: BigInt(blockEndTimeMs),
    blockSlot,
    minFeeA,
    minFeeB,
    prevUtxosRoot: priorLedgerRoot,
    ...(prevHeaderHash === undefined ? {} : { prevHeaderHash }),
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
    transitionTraceRoot: transitionRoot.root,
    ...(postLedger === undefined ? {} : { utxosRoot: postLedger.root }),
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const entries = (
    values: readonly { key: Buffer; value: Buffer }[],
  ): DaPayloadEntry[] =>
    [...values]
      .sort((a, b) => Buffer.compare(a.key, b.key))
      .map(({ key, value }) => [key.toString("hex"), value.toString("hex")]);
  const payload: DaPayload = {
    ...base.reconstruction.payload,
    block_body: {
      ...base.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      validation_traces: entries(descriptors),
      validation_trace_witnesses: entries(retainedEntries ?? []),
      transition_trace: entries(transitionEntries),
      ...(programMaterialEntries === undefined
        ? {}
        : {
            cek_program_material: [...programMaterialEntries].sort(
              ([left], [right]) => left.localeCompare(right),
            ),
          }),
      ...(postLedgerEntries === undefined
        ? {}
        : {
            utxos: entries(
              postLedgerEntries.map(({ outRef, output }) => ({
                key: outRef,
                value: output,
              })),
            ),
          }),
      counts: {
        ...base.reconstruction.payload.block_body.counts,
        validationTraceCount: root.count,
      },
    },
  };
  return {
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

export type RetainedPlutusFixtureOptions = Readonly<{
  sourceKind?: "normal" | "forced";
  /** Reuse the same identity program against an actually deposited ledger entry. */
  ledgerInput?: Readonly<{ outRef: Buffer; output: Buffer }>;
  predecessor?: Pick<
    Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
    "header" | "headerHash" | "payloadEnvelopeCbor"
  >;
  orderKey?: SDK.OutputReference;
  operatorVkey?: string;
  blockStartTimeMs?: number;
  blockEndTimeMs?: number;
  blockSlot?: bigint;
}>;

const buildRetainedPlutusFixture = async (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
  flatProgramHex: string,
  options: RetainedPlutusFixtureOptions = {},
) => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(flatProgramHex, "hex"),
  );
  const script = {
    language: "PlutusV3" as const,
    scriptBytes: program.envelopeCbor,
  };
  const spent = options.ledgerInput?.outRef ?? outRefCbor(0x1d, 0n);
  const value = {
    lovelace: 10_000_000n,
    assets: new Map<string, Map<string, bigint>>(),
  };
  const inputOutput =
    options.ledgerInput?.output ??
    encodeMidgardTxOutput({
      address: protectMidgardAddress(
        Buffer.concat([
          Buffer.from([0x70]),
          Buffer.from(hashMidgardVersionedScript(script), "hex"),
        ]),
      ),
      value,
    });
  const output = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
    value,
  });
  const redeemer = encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: 0n,
    redeemerCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
    executionUnits: { memory: 1_000_000_000n, steps: 1_000_000_000n },
  });
  const transaction = buildFixtureTransaction({
    spendInputs: [spent],
    outputs: [output],
    fee: 0n,
    networkId: 0n,
    scriptWitnesses: [encodeMidgardVersionedScript(script)],
    redeemerWitnesses: [redeemer],
    scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
      midgardFieldCommitment(encodeMidgardFieldPreimage([redeemer])),
      ["PlutusV3"],
    ),
  });
  const predecessor =
    options.predecessor ??
    (await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key: spent, value: inputOutput }],
      prevHeaderHash: GENESIS_HEADER_HASH,
    }));
  const sourceKind = options.sourceKind ?? "normal";
  const orderKey = options.orderKey ?? {
    transactionId: "52".repeat(32),
    outputIndex: 0n,
  };
  const eventKey: EventKey =
    sourceKind === "normal"
      ? {
          L2TransactionEventKey: { tx_id: transaction.txId },
        }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } };
  const blockEndTimeMs = options.blockEndTimeMs ?? 1_750_000_000_000;
  const blockSlot = options.blockSlot ?? 100n;
  const material = [...program.material.values()];
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey, asLucidSchema(EventKeySchema)),
        "hex",
      ),
      canonicalTransactionCbor:
        sourceKind === "forced"
          ? forcedTraceBytes(
              forcedTraceView(
                decodeMidgardNativeTxFullFromCanonicalCbor(
                  transaction.canonicalCbor,
                ),
              ),
            )
          : transaction.canonicalCbor,
      programMaterialSidecarCbor:
        encodeMidgardCekProgramMaterialSidecar(material),
      ...(sourceKind === "normal"
        ? { sourceKind: "normal" as const }
        : {
            sourceKind: "forced" as const,
          }),
      ledgerWitnessEntries: [{ outRef: spent, output: inputOutput }],
      priorUtxosRoot: predecessor.header.utxosRoot,
      blockEndTimeMs,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot,
    }),
  );
  const block = await buildRetainedValidationBlockFixture({
    subject:
      sourceKind === "normal"
        ? {
            kind: "normal",
            nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
          }
        : {
            kind: "forced",
            nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
            orderKey,
            verdict:
              claim.verdict === "accepted"
                ? "ForcedTxValid"
                : { ForcedTxInvalid: { reason: claim.reason } },
          },
    priorLedgerRoot: predecessor.header.utxosRoot,
    prevHeaderHash: predecessor.headerHash,
    blockEndTimeMs,
    blockSlot,
    ...(options.operatorVkey === undefined
      ? {}
      : { operatorVkey: options.operatorVkey }),
    ...(options.blockStartTimeMs === undefined
      ? {}
      : { blockStartTimeMs: options.blockStartTimeMs }),
    ...retainValidationTrace({ trace: replay.trace, eventKey, claim }),
    programMaterialEntries: material.map((entry) => [
      Buffer.from(entry.root).toString("hex"),
      encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
    ]),
    postLedgerEntries:
      (sourceKind === "forced" && claim.verdict === "rejected") ||
      replay.trace.verdict === "rejected"
        ? [{ outRef: spent, output: inputOutput }]
        : replay.statePatch.upsertedOutRefs.map(([outRef, output]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output,
          })),
  });
  return { block, predecessor, replay, orderKey, transaction };
};

/** Existing UPLC 1.1.0 lambda(var0) from validation-machine-event-replay.test.ts. */
export const buildRetainedPlutusIdentityFixture = (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
  options: RetainedPlutusFixtureOptions = {},
) => buildRetainedPlutusFixture(claim, "010100200101", options);

/** Exact existing bounded CEK unbound-variable refusal from cek-executor.test.ts. */
export const buildRetainedPlutusUnboundVariableFixture = (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
) => buildRetainedPlutusFixture(claim, "0101000011");

/** Unit-only raw transaction fixture, admitted through the real snapshot owner. */
export const captureRetainedPlutusIdentityOrigins = async (
  fixture: Pick<
    Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>,
    "block" | "transaction" | "orderKey"
  >,
  options: Readonly<{
    omitEvent?: boolean;
    advanceCapture?: boolean;
    inclusionTime?: bigint;
  }> = {},
) => {
  const hubPolicy = "61".repeat(28);
  const depositPolicy = "62".repeat(28);
  const withdrawalPolicy = "63".repeat(28);
  const orderPolicy = "64".repeat(28);
  const dummy = "65".repeat(28);
  const addressData = (hash: string) => ({
    paymentCredential: { ScriptCredential: [hash] as [string] },
    stakeCredential: null,
  });
  const address = (hash: string) =>
    credentialToAddress("Preprod", scriptHashToCredential(hash));
  const hub: SDK.HubOracleDatum = {
    registered_operators: dummy,
    active_operators: dummy,
    retired_operators: dummy,
    scheduler: dummy,
    state_queue: dummy,
    fraud_proof_catalogue: dummy,
    fraud_proof: dummy,
    deposit: depositPolicy,
    withdrawal: withdrawalPolicy,
    tx_order: orderPolicy,
    settlement: dummy,
    payout: dummy,
    registered_operators_addr: addressData(dummy),
    active_operators_addr: addressData(dummy),
    retired_operators_addr: addressData(dummy),
    scheduler_addr: addressData(dummy),
    state_queue_addr: addressData(dummy),
    fraud_proof_catalogue_addr: addressData(dummy),
    fraud_proof_addr: addressData(dummy),
    deposit_addr: addressData(depositPolicy),
    withdrawal_addr: addressData(withdrawalPolicy),
    tx_order_addr: addressData(orderPolicy),
    settlement_addr: addressData(dummy),
    reserve_addr: addressData(dummy),
    payout_addr: addressData(dummy),
    reserve_observer: dummy,
  };
  const submitted = deriveMidgardForcedTxProofSource(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      fixture.transaction.canonicalCbor,
    ),
  );
  const order: SDK.TxOrderDatum = {
    event: {
      id: fixture.orderKey,
      tx: {
        tx_id: fixture.transaction.txId,
        transaction_commitment:
          computeMidgardForcedTxProofCommitment(submitted).toString("hex"),
        submitted_source: {
          compact_cbor: submitted.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            submitted.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            submitted.fieldPreimageLengthsCbor.toString("hex"),
        },
      },
    },
    inclusion_time: options.inclusionTime ?? 1_749_999_999_000n,
    witness: dummy,
    refund_address: addressData(dummy),
    refund_datum: "NoDatum",
  };
  const hubUnit = hubPolicy + SDK.HUB_ORACLE_ASSET_NAME;
  const orderUnit = orderPolicy + "01";
  const entries = [
    {
      address: address(hubPolicy),
      unit: hubUnit,
      datumCbor: Data.to(hub, SDK.HubOracleDatum),
    },
    ...(options.omitEvent
      ? []
      : [
          {
            address: address(orderPolicy),
            unit: orderUnit,
            datumCbor: Data.to(order, SDK.TxOrderDatum),
          },
        ]),
  ];
  const outputs = CML.TransactionOutputList.new();
  const mint = CML.Mint.new();
  for (const entry of entries) {
    const assets = CML.MultiAsset.new();
    const policy = CML.ScriptHash.from_hex(entry.unit.slice(0, 56));
    const name = CML.AssetName.from_hex(entry.unit.slice(56));
    assets.set(policy, name, 1n);
    mint.set(policy, name, 1n);
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(entry.address),
      CML.Value.new(3_000_000n, assets),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(entry.datumCbor)),
    );
    outputs.add(output);
  }
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("31".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_mint(mint);
  const txHash = CML.hash_transaction(body).to_hex();
  const spent = CML.TransactionOutput.new(
    CML.Address.from_bech32(address(dummy)),
    CML.Value.from_coin(10_000_000n),
  ).to_canonical_cbor_hex();
  const point = (slot: string, blockNo: string, byte: string) => {
    const value = { slot, blockNo, blockHash: byte.repeat(32) };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const included = point("1070", "70", "41");
  const cursor = options.advanceCapture
    ? point("1072", "72", "44")
    : point("1071", "71", "42");
  const tip = options.advanceCapture
    ? point("1101", "101", "45")
    : point("1100", "100", "43");
  const policy = {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  } as const;
  const finality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: "d1".repeat(32),
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) =>
      ({
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "retained-origin-unit-fixture",
          grade: "security",
          sourceMode: "local_kupo_ogmios",
          kupoCheckpoint: cursor,
          ogmiosTip: tip,
        },
        cursor: {
          point: cursor,
          tip,
          confirmationDepth: 30,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId: "retained-origin-unit-fixture",
            pointId: cursor.pointId,
          }),
        },
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos: entries.flatMap((entry, index) =>
            entry.address === scope.address
              ? [
                  {
                    outRef: txHash + "#" + index.toString(),
                    outputCbor: outputs.get(index).to_canonical_cbor_hex(),
                    datumCbor: CML.PlutusData.from_cbor_hex(
                      entry.datumCbor,
                    ).to_canonical_cbor_hex(),
                    referenceScriptCbor: null,
                  },
                ]
              : [],
          ),
        })),
        historyUnits: request.historyUnits,
        history: request.historyUnits.map((unit) => ({
          unit,
          fromGenesis: true,
          completeThroughPointId: cursor.pointId,
          transactionHashes: [txHash],
        })),
        transactions: [
          {
            txHash,
            bodyCbor: body.to_cbor_hex(),
            witnessSetCbor:
              CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
            redeemersCbor: null,
            isValid: true,
            inclusionPoint: included,
            confirmationDepth: options.advanceCapture ? 32 : 31,
            resolvedInputs: [
              {
                outRef: "31".repeat(32) + "#0",
                outputCbor: spent,
                datumCbor: null,
                referenceScriptCbor: null,
              },
            ],
            resolvedReferenceInputs: [],
          },
        ],
      }) satisfies FraudProofRawL1Snapshot,
  };
  // Only fields consumed by raw event capture are supplied; this does not claim
  // that a deployment manifest or an actual Cardano transaction was admitted.
  const binding = {
    network: "Preprod",
    releaseFinality: finality,
    resolvedContracts: {
      hubOraclePolicyId: hubPolicy,
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
    },
    definition: { headerHash: fixture.block.headerHash },
  } as Parameters<typeof captureTransitionTraceL1Events>[0]["binding"];
  return captureTransitionTraceL1Events({ binding, authority });
};

/** Runs the production classifier against retained bytes and an L1 observation. */
export const classifyRetainedReasonFixture = async ({
  observation,
  payloadEnvelopeCbor,
  deploymentFingerprint,
  releaseFinalityAuthority,
  replayer,
  predecessor,
  history = [],
  replayContext,
  transitionTraceEventAuthority,
  settlementAuthority,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Buffer;
  readonly deploymentFingerprint: string;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly replayer: CompleteCanonicalReplay;
  readonly history?: readonly {
    readonly headerHash: string;
    readonly payloadEnvelopeCbor: Buffer;
  }[];
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly transitionTraceEventAuthority?: Parameters<
    typeof createHeaderClassifier
  >[0]["transitionTraceEventAuthority"];
  readonly settlementAuthority?: Parameters<
    typeof createHeaderClassifier
  >[0]["settlementAuthority"];
  readonly predecessor?: {
    readonly observation: AuthenticatedStateQueueHeaderObservation;
    readonly payloadEnvelopeCbor: Buffer;
  };
}) => {
  const sources: readonly RetainedDaPayloadSource[] = [
    {
      sourceId: "retained-fixture",
      fetchPayloadByHeaderHash: async (headerHash) => {
        const bytes =
          headerHash === observation.headerHash
            ? payloadEnvelopeCbor
            : headerHash === predecessor?.observation.headerHash
              ? predecessor.payloadEnvelopeCbor
              : history.find((block) => block.headerHash === headerHash)
                  ?.payloadEnvelopeCbor;
        if (bytes === undefined)
          throw new Error("Retained fixture requested another header");
        return {
          ok: true,
          sourceId: "retained-fixture",
          sourcePeerId: "emulator",
          attempts: [],
          payloadEnvelopeCbor: bytes,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "retained-fixture/emulator",
            grade: "security",
          },
        };
      },
    },
  ];
  const requiresHistory = replayer.launchScope.some((category) =>
    [
      "resolvedOutputNonCanonical",
      "spendInputSignerMissing",
      "executionNativeScriptInvalid",
      "missingNativeScriptUtxo",
      "transitionTrace",
    ].includes(category),
  );
  const checkpointDirectory = requiresHistory
    ? mkdtempSync("/var/tmp/midgard-retained-reason-")
    : undefined;
  try {
    const historicalReplayAuthority =
      checkpointDirectory === undefined
        ? undefined
        : {
            checkpointStore: createSqliteHistoricalNativeScriptCheckpointStore({
              path: join(checkpointDirectory, "checkpoint.sqlite"),
              rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
            }),
            historySource: createHistoricalNativeScriptHistorySource({
              providerRoster: createHistoricalNativeScriptProviderRoster({
                deploymentFingerprint,
                // All fixture history is present in the retained source above;
                // its unknown-header failure prevents external archive fallback.
                providers: [
                  {
                    sourceId: "archive-a",
                    authorityEndpoint: "https://archive-a.example.test",
                    operatorIdentitySha256: "aa".repeat(32),
                  },
                  {
                    sourceId: "archive-b",
                    authorityEndpoint: "https://archive-b.example.test",
                    operatorIdentitySha256: "bb".repeat(32),
                  },
                ],
              }),
            }),
          };
    const classifier = await createHeaderClassifier({
      deploymentFingerprint,
      replayer,
      releaseFinalityAuthority,
      historicalReplayAuthority,
      transitionTraceEventAuthority,
      settlementAuthority,
    });
    const policy = await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint,
    });
    const decision = await classifyHeader({
      classifier,
      observation,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation,
          minimumConfirmationDepth: policy.policy.confirmationDepth,
        }),
      sources,
      ...(replayContext === undefined ? {} : { replayContext }),
      ...(predecessor === undefined
        ? {}
        : { predecessorObservation: predecessor.observation }),
    });
    return { decision, sources };
  } finally {
    if (checkpointDirectory !== undefined)
      rmSync(checkpointDirectory, { recursive: true, force: true });
  }
};
