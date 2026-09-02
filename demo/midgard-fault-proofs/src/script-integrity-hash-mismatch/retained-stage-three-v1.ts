import {
  hashMidgardValidationEventKeyV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationWorkWitnessV1,
  type MidgardValidationMachineStateV1,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  decodeRetainedValidationWitnessKeyV1,
  decodeRetainedValidationWitnessV1,
  type EventKey,
  EventKeySchema,
  Proof,
  ROOT_DOMAINS,
  validationTraceDescriptorCoreFromData,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { executionSourceNativeControlFromCborV1 } from "../execution-source-script-decoding/machine-authentication-v1.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;
const exactNumber = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`scriptIntegrityHashMismatch retained ${label} changed`);
  return result;
};
const bytes = (value: unknown, label: string): Buffer => {
  if (!(value instanceof Uint8Array))
    throw new Error(
      `scriptIntegrityHashMismatch retained ${label} is not bytes`,
    );
  return Buffer.from(value);
};
const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "number" && typeof value !== "bigint")
    throw new Error(
      `scriptIntegrityHashMismatch retained ${label} is not integer`,
    );
  return BigInt(value);
};
const eventKeyCbor = (key: EventKey) =>
  Buffer.from(Data.to(key as never, EventKeySchema), "hex");
const stateFromData = (
  state: ReturnType<typeof decodeRetainedValidationWitnessV1>["machine_state"],
): MidgardValidationMachineStateV1 => {
  if (state.phase !== "ScriptIntegrity")
    throw new Error("scriptIntegrityHashMismatch retained phase changed");
  if (state.machine_version !== 1n)
    throw new Error(
      "scriptIntegrityHashMismatch retained machine version changed",
    );
  return {
    machineVersion: 1,
    eventKeyHash: Buffer.from(state.event_key_hash, "hex"),
    transactionId: Buffer.from(state.transaction_id, "hex"),
    transactionCommitment: Buffer.from(state.transaction_commitment, "hex"),
    validationContextHash: Buffer.from(state.validation_context_hash, "hex"),
    sourceKind: state.source_kind === "Normal" ? "normal" : "forced",
    priorLedgerRoot: Buffer.from(state.prior_ledger_root, "hex"),
    phase: "scriptIntegrity",
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

export type ScriptIntegrityStageThreeAuthenticationV1 = Readonly<{
  validationTracesRoot: string;
  validationTraceCount: bigint;
  traceMembership: Readonly<Record<string, unknown>>;
  machineState: ReturnType<
    typeof decodeRetainedValidationWitnessV1
  >["machine_state"];
  traceProof: ReturnType<
    typeof decodeRetainedValidationWitnessV1
  >["trace_proof"];
  control: ReturnType<typeof executionSourceNativeControlFromCborV1>;
  scriptIntegrityHash: string;
  redeemerWitnessHash: string;
}>;

/** Strict reconstruction of the sole canonical ScriptIntegrity stage-3 witness. */
export const buildScriptIntegrityStageThreeAuthenticationFromRetainedDaV1 =
  async ({
    eventKey,
    authenticatedValidationTraceEntries,
    retainedValidationWitnessEntries,
    expectedValidationTracesRoot,
  }: {
    eventKey: EventKey;
    authenticatedValidationTraceEntries: readonly EncodedEntry[];
    retainedValidationWitnessEntries: readonly EncodedEntry[];
    expectedValidationTracesRoot: string;
  }): Promise<ScriptIntegrityStageThreeAuthenticationV1> => {
    const keyBytes = eventKeyCbor(eventKey);
    const descriptorEntries = authenticatedValidationTraceEntries.map(
      ({ key, value }) => ({
        key: Buffer.from(key),
        value: Buffer.from(value),
      }),
    );
    const descriptorMatches = descriptorEntries.filter(({ key }) =>
      key.equals(keyBytes),
    );
    if (descriptorMatches.length !== 1)
      throw new Error(
        "scriptIntegrityHashMismatch validation descriptor is absent or duplicated",
      );
    const descriptorData = Data.from(
      descriptorMatches[0]!.value.toString("hex"),
      ValidationTraceDescriptorV1Schema,
    ) as never;
    const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
    const seen = new Set<string>();
    const candidates = retainedValidationWitnessEntries.flatMap((entry) => {
      const retainedKey = decodeRetainedValidationWitnessKeyV1(entry.key);
      const retainedKeyBytes = eventKeyCbor(retainedKey.event_key);
      const coordinate = `${retainedKeyBytes.toString("hex")}:${retainedKey.execution_index.toString()}`;
      if (seen.has(coordinate))
        throw new Error(
          "scriptIntegrityHashMismatch duplicate retained coordinate",
        );
      seen.add(coordinate);
      if (!retainedKeyBytes.equals(keyBytes)) return [];
      const witness = decodeRetainedValidationWitnessV1(entry.value);
      return witness.phase === 10n && witness.program_counter === 3n
        ? [witness]
        : [];
    });
    if (candidates.length !== 1)
      throw new Error(
        "scriptIntegrityHashMismatch exact retained stage is absent or duplicated",
      );
    const retained = candidates[0]!;
    if (
      retained.machine_state.phase !== "ScriptIntegrity" ||
      retained.auxiliary !== "NoAuxiliaryWitness"
    )
      throw new Error(
        "scriptIntegrityHashMismatch retained stage/auxiliary changed",
      );
    const outer = decodeSingleCbor(Buffer.from(retained.witness_cbor, "hex"));
    if (
      !Array.isArray(outer) ||
      outer.length !== 4 ||
      integer(outer[1], "stage") !== 3n
    )
      throw new Error(
        "scriptIntegrityHashMismatch work witness is not stage 3",
      );
    const nativeControlCbor = bytes(outer[0], "native control");
    const scriptIntegrityHash = bytes(
      outer[2],
      "script integrity hash",
    ).toString("hex");
    const redeemerWitnessHash = bytes(
      outer[3],
      "redeemer witness hash",
    ).toString("hex");
    if (scriptIntegrityHash.length !== 64 || redeemerWitnessHash.length !== 64)
      throw new Error(
        "scriptIntegrityHashMismatch retained hash width changed",
      );
    const control = executionSourceNativeControlFromCborV1(nativeControlCbor);
    if (
      control.execution_cursor !== control.execution_count ||
      control.execution_count !== control.purpose_count ||
      control.language_bitmap < 0n ||
      control.language_bitmap > 3n
    )
      throw new Error(
        "scriptIntegrityHashMismatch retained language frontier is incomplete",
      );
    const state = stateFromData(retained.machine_state);
    const proof = validationTraceProofCoreFromData(retained.trace_proof);
    if (
      !hashMidgardValidationMachineStateV1(state).equals(proof.stateHash) ||
      !verifyMidgardValidationTraceProofV1({ descriptor, proof }) ||
      !state.eventKeyHash.equals(hashMidgardValidationEventKeyV1(keyBytes)) ||
      !state.workRoot.equals(
        hashMidgardValidationWorkWitnessV1({
          phase: "scriptIntegrity",
          programCounter: state.programCounter,
          witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
        }),
      )
    )
      throw new Error(
        "scriptIntegrityHashMismatch retained state/proof/work authentication failed",
      );
    const root = await buildCountedRoot(
      ROOT_DOMAINS.validationTraces,
      descriptorEntries,
    );
    if (root.root !== expectedValidationTracesRoot)
      throw new Error("scriptIntegrityHashMismatch validation root changed");
    const membership = await keyValuePhasProof(
      { root: root.phasRoot, count: root.count, entries: root.entries },
      keyBytes,
      descriptorMatches[0]!.value,
    );
    return Object.freeze({
      validationTracesRoot: root.root,
      validationTraceCount: root.count,
      traceMembership: {
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key: eventKey,
        value: descriptorData,
        proof: Data.from(Data.to(membership, Proof), Proof),
      },
      machineState: retained.machine_state,
      traceProof: retained.trace_proof,
      control,
      scriptIntegrityHash,
      redeemerWitnessHash,
    });
  };
