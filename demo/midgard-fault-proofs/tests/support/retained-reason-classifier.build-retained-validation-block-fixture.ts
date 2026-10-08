import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  MidgardValidationPhase,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asDataType, asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type DaPayload,
  type DaPayloadEntry,
  encodeDaPayload,
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  hashBlockHeader,
  rejectionCodeOf,
  type RejectionReason,
  type RetainedValidationAuxiliaryWitness,
  RetainedValidationAuxiliaryWitnessSchema,
  retainedValidationEndpointCoordinate,
  retainedValidationStateCoordinate,
  ROOT_DOMAINS,
  TransitionStep,
  validationMachineStateDataFromCore,
  type ValidationTraceDescriptor,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  type DeterministicValidationMachineTrace,
  retainedValidationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { buildCanonicalBlockFixture } from "../helpers/canonical-block-evidence-fixture.js";
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
                  retainedValidationAuxiliaryWitnessData(witness.auxiliary),
                ),
                asDataType<RetainedValidationAuxiliaryWitness>(
                  RetainedValidationAuxiliaryWitnessSchema,
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
  /**
   * The default predecessor's header operator and times, so it can be
   * committed as an ordinary first block on an emulator state queue.
   */
  predecessorFrame?: Readonly<{
    operatorVkey: string;
    startTime: bigint;
    endTime: bigint;
  }>;
  /** The trace the operator commits, in place of the replayed one. */
  committedTrace?: (
    replayed: DeterministicValidationMachineTrace,
  ) => DeterministicValidationMachineTrace;
}>;
