import {
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  Proof,
  ROOT_DOMAINS,
  validationTraceDescriptorCoreFromData,
  ValidationTraceDescriptorSchema,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import type { DistinctAssetAccumulationFinding } from "./family.js";
import type { DistinctAssetAccumulationCoordinate } from "./family.js";
import type { DistinctAssetFoldAction } from "./submit-fold.js";
import type { DistinctAssetAccumulatorAuthentication } from "./submit-step-02.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;
type Retained = ReturnType<typeof decodeRetainedValidationWitness>;
const exactNumber = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`distinctAssetAccumulationLimit retained ${label} changed`);
  return result;
};
const bytes = (value: unknown, label: string): string => {
  if (!(value instanceof Uint8Array))
    throw new Error(
      `distinctAssetAccumulationLimit retained ${label} is not bytes`,
    );
  return Buffer.from(value).toString("hex");
};
const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "number" && typeof value !== "bigint")
    throw new Error(
      `distinctAssetAccumulationLimit retained ${label} is not integer`,
    );
  return BigInt(value);
};
const frontier = (value: unknown) => {
  if (!Array.isArray(value))
    throw new Error("distinctAssetAccumulationLimit retained frontier changed");
  return value.map((peak) => {
    if (!Array.isArray(peak) || peak.length !== 2)
      throw new Error(
        "distinctAssetAccumulationLimit retained frontier peak changed",
      );
    return {
      height: integer(peak[0], "frontier height"),
      hash: bytes(peak[1], "frontier hash"),
    };
  });
};
const nativeControl = (value: unknown) => {
  const decoded = value instanceof Uint8Array ? decodeSingleCbor(value) : value;
  if (!Array.isArray(decoded) || decoded.length !== 26)
    throw new Error(
      "distinctAssetAccumulationLimit retained native control changed",
    );
  return {
    compact_cbor: bytes(decoded[0], "compact cbor"),
    witness_set_compact_cbor: bytes(decoded[1], "witness cbor"),
    field_preimage_lengths_cbor: bytes(decoded[2], "lengths cbor"),
    context_cbor: bytes(decoded[3], "context cbor"),
    resolved_input_count: integer(decoded[4], "resolved input count"),
    resolved_inputs_accumulator: bytes(decoded[5], "resolved accumulator"),
    spend_input_count: integer(decoded[6], "spend input count"),
    resolved_item_peaks: frontier(decoded[7]),
    signer_count: integer(decoded[8], "signer count"),
    signer_frontier_commitment: bytes(decoded[9], "signer frontier"),
    source_count: integer(decoded[10], "source count"),
    source_peaks: frontier(decoded[11]),
    redeemer_count: integer(decoded[12], "redeemer count"),
    redeemer_peaks: frontier(decoded[13]),
    purpose_count: integer(decoded[14], "purpose count"),
    purpose_peaks: frontier(decoded[15]),
    output_count: integer(decoded[16], "output count"),
    output_peaks: frontier(decoded[17]),
    output_descriptor_peaks: frontier(decoded[18]),
    mint_count: integer(decoded[19], "mint count"),
    mint_peaks: frontier(decoded[20]),
    execution_count: integer(decoded[21], "execution count"),
    execution_peaks: frontier(decoded[22]),
    execution_cursor: integer(decoded[23], "execution cursor"),
    language_bitmap: integer(decoded[24], "language bitmap"),
    resolution_schedule_hash: bytes(decoded[25], "resolution schedule"),
  };
};
export const decodeDistinctAssetValueAndMintControl = (witnessCbor: string) => {
  const decoded = decodeSingleCbor(Buffer.from(witnessCbor, "hex"));
  if (!Array.isArray(decoded) || decoded.length !== 12)
    throw new Error(
      "distinctAssetAccumulationLimit retained ValueAndMint control changed",
    );
  const accumulator =
    decoded[11] instanceof Uint8Array
      ? decodeSingleCbor(decoded[11])
      : decoded[11];
  if (!Array.isArray(accumulator) || accumulator.length !== 4)
    throw new Error(
      "distinctAssetAccumulationLimit retained accumulator changed",
    );
  return {
    native_control: nativeControl(decoded[0]),
    stage: integer(decoded[1], "stage"),
    replay_schedule_hash: bytes(decoded[2], "replay schedule"),
    replay_cursor: integer(decoded[3], "replay cursor"),
    replay_asset_cursor: integer(decoded[4], "replay asset cursor"),
    replay_value_hash: bytes(decoded[5], "replay value hash"),
    replay_accumulator: bytes(decoded[6], "replay accumulator"),
    replay_remaining_schedule_hash: bytes(decoded[7], "remaining schedule"),
    output_cursor: integer(decoded[8], "output cursor"),
    output_asset_cursor: integer(decoded[9], "output asset cursor"),
    mint_cursor: integer(decoded[10], "mint cursor"),
    value_accumulator: {
      lovelace_delta: integer(accumulator[0], "lovelace delta"),
      asset_root: bytes(accumulator[1], "asset root"),
      seen_asset_count: integer(accumulator[2], "seen asset count"),
      nonzero_asset_count: integer(accumulator[3], "nonzero asset count"),
    },
  };
};
const eventKeyCbor = (key: EventKey) =>
  Buffer.from(Data.to(key as never, EventKeySchema), "hex");
const stateFromData = (
  state: Retained["machine_state"],
): MidgardValidationMachineState => {
  if (state.machine_version !== 1n)
    throw new Error(
      "distinctAssetAccumulationLimit retained machine version changed",
    );
  return {
    machineVersion: 1,
    eventKeyHash: Buffer.from(state.event_key_hash, "hex"),
    transactionId: Buffer.from(state.transaction_id, "hex"),
    transactionCommitment: Buffer.from(state.transaction_commitment, "hex"),
    validationContextHash: Buffer.from(state.validation_context_hash, "hex"),
    sourceKind: state.source_kind === "Normal" ? "normal" : "forced",
    priorLedgerRoot: Buffer.from(state.prior_ledger_root, "hex"),
    phase: "valueAndMint",
    programCounter: exactNumber(state.program_counter, "program counter"),
    workRoot: Buffer.from(state.work_root, "hex"),
    executionCpu: state.execution_cpu,
    executionMemory: state.execution_memory,
    verdict:
      state.verdict === "Pending"
        ? "pending"
        : state.verdict === "Accepted"
          ? "accepted"
          : "rejected",
    rejectionCodeHash: Buffer.from(state.rejection_code_hash, "hex"),
    ledgerDeltaRoot: Buffer.from(state.ledger_delta_root, "hex"),
  };
};

const selectedAuxiliary = (
  retained: Retained,
  finding: DistinctAssetAccumulationFinding,
): DistinctAssetFoldAction | null => {
  const auxiliary = retained.auxiliary;
  const coordinate = finding.coordinate;
  if (
    coordinate.kind === "input" &&
    typeof auxiliary === "object" &&
    "ValueInputAssetWitness" in auxiliary &&
    auxiliary.ValueInputAssetWitness.asset_index ===
      BigInt(coordinate.assetIndex)
  )
    return { kind: "authenticate", evidence: auxiliary.ValueInputAssetWitness };
  if (
    coordinate.kind === "output" &&
    typeof auxiliary === "object" &&
    "ValueOutputAssetWitness" in auxiliary &&
    auxiliary.ValueOutputAssetWitness.output_index ===
      BigInt(coordinate.outputIndex) &&
    auxiliary.ValueOutputAssetWitness.asset_index ===
      BigInt(coordinate.assetIndex)
  )
    return {
      kind: "authenticate",
      evidence: auxiliary.ValueOutputAssetWitness,
    };
  if (
    coordinate.kind === "mint" &&
    typeof auxiliary === "object" &&
    "ValueMintAssetWitness" in auxiliary &&
    auxiliary.ValueMintAssetWitness.mint_index === BigInt(coordinate.mintIndex)
  )
    return { kind: "authenticate", evidence: auxiliary.ValueMintAssetWitness };
  return null;
};

export type DistinctAssetRetainedAuthentication = Readonly<{
  authentication: DistinctAssetAccumulatorAuthentication;
  folds: readonly [
    DistinctAssetFoldAction,
    DistinctAssetFoldAction,
    DistinctAssetFoldAction,
  ];
}>;

export type DistinctAssetRetainedMutationCandidate = Readonly<{
  eventKey: EventKey;
  transactionId: string;
  coordinate: DistinctAssetAccumulationCoordinate;
  traceStateHashHex: string;
  workRootHex: string;
  control: ReturnType<typeof decodeDistinctAssetValueAndMintControl>;
  action: Extract<DistinctAssetFoldAction, { kind: "authenticate" }>;
}>;

/**
 * Enumerates only exact stage-2/3/4 retained ValueAndMint asset mutations.
 * Authentication of the selected candidate remains mandatory below; this
 * discovery function merely derives deterministic candidate coordinates.
 */
export const discoverDistinctAssetRetainedMutationCandidates = (
  retainedValidationWitnessEntries: readonly EncodedEntry[],
): readonly DistinctAssetRetainedMutationCandidate[] =>
  Object.freeze(
    retainedValidationWitnessEntries.flatMap(({ key, value }) => {
      const retainedKey = decodeRetainedValidationWitnessKey(key);
      const retained = decodeRetainedValidationWitness(value);
      if (
        retained.phase !== 12n ||
        retained.machine_state.phase !== "ValueAndMint" ||
        retained.program_counter !== retained.machine_state.program_counter
      )
        return [];
      const control = decodeDistinctAssetValueAndMintControl(
        retained.witness_cbor,
      );
      const auxiliary = retained.auxiliary;
      let coordinate: DistinctAssetAccumulationCoordinate;
      let evidence: Extract<
        DistinctAssetFoldAction,
        { kind: "authenticate" }
      >["evidence"];
      if (
        control.stage === 2n &&
        typeof auxiliary === "object" &&
        "ValueInputAssetWitness" in auxiliary
      ) {
        coordinate = {
          kind: "input",
          inputIndex: exactNumber(control.replay_cursor, "input cursor"),
          assetIndex: exactNumber(
            auxiliary.ValueInputAssetWitness.asset_index,
            "input asset index",
          ),
        };
        evidence = auxiliary.ValueInputAssetWitness;
      } else if (
        control.stage === 3n &&
        typeof auxiliary === "object" &&
        "ValueOutputAssetWitness" in auxiliary
      ) {
        coordinate = {
          kind: "output",
          outputIndex: exactNumber(
            auxiliary.ValueOutputAssetWitness.output_index,
            "output index",
          ),
          assetIndex: exactNumber(
            auxiliary.ValueOutputAssetWitness.asset_index,
            "output asset index",
          ),
        };
        evidence = auxiliary.ValueOutputAssetWitness;
      } else if (
        control.stage === 4n &&
        typeof auxiliary === "object" &&
        "ValueMintAssetWitness" in auxiliary
      ) {
        coordinate = {
          kind: "mint",
          mintIndex: exactNumber(
            auxiliary.ValueMintAssetWitness.mint_index,
            "mint index",
          ),
        };
        evidence = auxiliary.ValueMintAssetWitness;
      } else return [];
      return [
        Object.freeze({
          eventKey: retainedKey.event_key,
          transactionId: retained.machine_state.transaction_id,
          coordinate,
          traceStateHashHex: retained.trace_proof.state_hash,
          workRootHex: retained.machine_state.work_root,
          control,
          action: Object.freeze({ kind: "authenticate" as const, evidence }),
        }),
      ];
    }),
  );

/** Reconstructs the exact selected ValueAndMint asset mutation from public DA. */
export const buildDistinctAssetAuthenticationFromRetainedDa = async ({
  eventKey,
  finding,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
}: {
  readonly eventKey: EventKey;
  readonly finding: DistinctAssetAccumulationFinding;
  readonly authenticatedValidationTraceEntries: readonly EncodedEntry[];
  readonly retainedValidationWitnessEntries: readonly EncodedEntry[];
  readonly expectedValidationTracesRoot: string;
}): Promise<DistinctAssetRetainedAuthentication> => {
  const keyBytes = eventKeyCbor(eventKey);
  const descriptors = authenticatedValidationTraceEntries.map(
    ({ key, value }) => ({ key: Buffer.from(key), value: Buffer.from(value) }),
  );
  const descriptorMatches = descriptors.filter(({ key }) =>
    key.equals(keyBytes),
  );
  if (descriptorMatches.length !== 1)
    throw new Error(
      "distinctAssetAccumulationLimit validation descriptor is absent or duplicated",
    );
  const descriptorData = Data.from(
    descriptorMatches[0]!.value.toString("hex"),
    ValidationTraceDescriptorSchema,
  ) as never;
  const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
  const expectedStage =
    finding.coordinate.kind === "input"
      ? 2n
      : finding.coordinate.kind === "output"
        ? 3n
        : 4n;
  const candidates = retainedValidationWitnessEntries.flatMap((entry) => {
    const retainedKey = decodeRetainedValidationWitnessKey(entry.key);
    if (!eventKeyCbor(retainedKey.event_key).equals(keyBytes)) return [];
    const retained = decodeRetainedValidationWitness(entry.value);
    if (
      retained.phase !== 12n ||
      retained.machine_state.phase !== "ValueAndMint" ||
      retained.program_counter !== retained.machine_state.program_counter ||
      retained.machine_state.transaction_id !== finding.subject.transaction_id
    )
      return [];
    const control = decodeDistinctAssetValueAndMintControl(
      retained.witness_cbor,
    );
    const action = selectedAuxiliary(retained, finding);
    const coordinate = finding.coordinate;
    const controlMatches =
      coordinate.kind === "input"
        ? control.replay_cursor === BigInt(coordinate.inputIndex) &&
          control.replay_asset_cursor === BigInt(coordinate.assetIndex + 1)
        : coordinate.kind === "output"
          ? control.output_cursor === BigInt(coordinate.outputIndex) &&
            control.output_asset_cursor === BigInt(coordinate.assetIndex + 1)
          : control.mint_cursor === BigInt(coordinate.mintIndex);
    return control.stage === expectedStage && action !== null && controlMatches
      ? [{ retained, control, action }]
      : [];
  });
  if (candidates.length !== 1)
    throw new Error(
      "distinctAssetAccumulationLimit exact retained mutation is absent or duplicated",
    );
  const { retained, control, action } = candidates[0]!;
  const state = stateFromData(retained.machine_state);
  const proof = validationTraceProofCoreFromData(retained.trace_proof);
  if (
    !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
    !verifyMidgardValidationTraceProof({ descriptor, proof }) ||
    !state.eventKeyHash.equals(hashMidgardValidationEventKey(keyBytes)) ||
    !state.workRoot.equals(
      hashMidgardValidationWorkWitness({
        phase: "valueAndMint",
        programCounter: state.programCounter,
        witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
      }),
    )
  )
    throw new Error(
      "distinctAssetAccumulationLimit retained state/proof/work authentication failed",
    );
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptors,
  );
  if (root.root !== expectedValidationTracesRoot)
    throw new Error("distinctAssetAccumulationLimit validation root changed");
  const membership = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    keyBytes,
    descriptorMatches[0]!.value,
  );
  const targetFold = Number(expectedStage - 2n);
  const folds: DistinctAssetRetainedAuthentication["folds"] = [
    targetFold === 0 ? action : { kind: "skip" },
    targetFold === 1 ? action : { kind: "skip" },
    targetFold === 2 ? action : { kind: "skip" },
  ];
  return Object.freeze({
    authentication: {
      trace_membership: {
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key: eventKey,
        value: descriptorData,
        proof: Data.from(Data.to(membership, Proof), Proof),
      },
      pre: retained.machine_state,
      trace_proof: retained.trace_proof,
      control,
    },
    folds: Object.freeze(folds),
  });
};
