import {
  commitMidgardValidationMerkleFrontierV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardValidationEventKeyV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationWorkWitnessV1,
  type MidgardValidationMachineStateV1,
  type MidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
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

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import {
  type ExecutionSourceMachineAuthenticationV1,
  executionSourceNativeControlFromCborV1,
} from "./machine-authentication-v1.js";
import { NativeScriptsControlV1Schema } from "./schemas-v1.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;

const exactNumber = (value: bigint, label: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0)
    throw new Error(`${label} is not a non-negative safe integer`);
  return number;
};

const stateFromData = (
  state: ReturnType<typeof decodeRetainedValidationWitnessV1>["machine_state"],
): MidgardValidationMachineStateV1 => {
  if (state.phase !== "NativeScripts")
    throw new Error("retained execution state is not NativeScripts");
  const machineVersion = exactNumber(state.machine_version, "machine version");
  if (machineVersion !== 1)
    throw new Error("retained execution machine version is unsupported");
  return {
    machineVersion,
    eventKeyHash: Buffer.from(state.event_key_hash, "hex"),
    transactionId: Buffer.from(state.transaction_id, "hex"),
    transactionCommitment: Buffer.from(state.transaction_commitment, "hex"),
    validationContextHash: Buffer.from(state.validation_context_hash, "hex"),
    sourceKind: state.source_kind === "Normal" ? "normal" : "forced",
    priorLedgerRoot: Buffer.from(state.prior_ledger_root, "hex"),
    phase: "nativeScripts",
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

const merkleFrontier = (
  count: bigint,
  peaks: readonly { height: bigint; hash: string }[],
): MidgardValidationMerkleFrontierV1 => ({
  count: exactNumber(count, "frontier count"),
  peaks: peaks.map(({ height, hash }) => ({
    height: exactNumber(height, "frontier height"),
    hash: Buffer.from(hash, "hex"),
  })),
});

const requireMembership = (
  frontier: MidgardValidationMerkleFrontierV1,
  leafIndex: bigint,
  leafHash: Buffer,
  siblings: readonly string[],
  label: string,
): void => {
  if (
    !verifyMidgardValidationMerkleMembershipV1({
      frontier,
      leafIndex: exactNumber(leafIndex, `${label} index`),
      leafHash,
      siblings: siblings.map((sibling) => Buffer.from(sibling, "hex")),
    })
  )
    throw new Error(`retained execution ${label} membership is invalid`);
};

/** Strict public-DA reconstruction seam for ID31's source-authentication step. */
export const buildExecutionSourceMachineAuthenticationFromRetainedDaV1 =
  async ({
    eventKey,
    executionIndex,
    authenticatedValidationTraceEntries,
    retainedValidationWitnessEntries,
    expectedValidationTracesRoot,
    expectedLanguageTag = 0,
    expectedPurposeKind,
  }: {
    readonly eventKey: EventKey;
    readonly executionIndex: number;
    readonly authenticatedValidationTraceEntries: readonly EncodedEntry[];
    readonly retainedValidationWitnessEntries: readonly EncodedEntry[];
    readonly expectedValidationTracesRoot: string;
    readonly expectedLanguageTag?: 0 | 3 | 128;
    readonly expectedPurposeKind?: 0 | 1 | 2 | 3;
  }): Promise<ExecutionSourceMachineAuthenticationV1> => {
    if (!Number.isSafeInteger(executionIndex) || executionIndex < 0)
      throw new Error("execution index is not a non-negative safe integer");
    const eventKeyCbor = Buffer.from(
      Data.to(eventKey as never, EventKeySchema),
      "hex",
    );
    const descriptorEntries = authenticatedValidationTraceEntries.map(
      (entry) => ({
        key: Buffer.from(entry.key),
        value: Buffer.from(entry.value),
      }),
    );
    const descriptorMatches = descriptorEntries.filter(({ key }) =>
      key.equals(eventKeyCbor),
    );
    if (descriptorMatches.length !== 1)
      throw new Error("retained validation descriptor is absent or duplicated");
    const descriptorData = Data.from(
      descriptorMatches[0]!.value.toString("hex"),
      ValidationTraceDescriptorV1Schema,
    ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptorV1;
    const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
    const descriptorKeys = new Set(
      descriptorEntries.map(({ key }) => key.toString("hex")),
    );
    const seen = new Set<string>();
    const decoded = retainedValidationWitnessEntries.map((entry) => {
      const key = decodeRetainedValidationWitnessKeyV1(entry.key);
      const keyBytes = Buffer.from(
        Data.to(key.event_key as never, EventKeySchema),
        "hex",
      );
      if (!descriptorKeys.has(keyBytes.toString("hex")))
        throw new Error("orphan retained validation witness");
      const coordinate = `${keyBytes.toString("hex")}:${key.execution_index.toString()}`;
      if (seen.has(coordinate))
        throw new Error("duplicate retained validation witness");
      seen.add(coordinate);
      return {
        key,
        keyBytes,
        value: decodeRetainedValidationWitnessV1(entry.value),
      };
    });
    const matches = decoded.filter(
      ({ key, keyBytes }) =>
        keyBytes.equals(eventKeyCbor) &&
        key.execution_index === BigInt(executionIndex),
    );
    if (matches.length !== 1)
      throw new Error(
        "retained validation witness coordinate is absent or duplicated",
      );
    const retained = matches[0]!.value;
    if (
      retained.phase !== 9n ||
      retained.program_counter !== retained.machine_state.program_counter
    )
      throw new Error(
        "retained validation witness phase/program counter changed",
      );
    const auxiliary = retained.auxiliary;
    if (
      !(
        typeof auxiliary === "object" &&
        "NativeExecutionDescriptorWitness" in auxiliary
      )
    )
      throw new Error(
        "retained validation witness has the wrong auxiliary kind",
      );
    const native = auxiliary.NativeExecutionDescriptorWitness;
    if (
      native.execution_index !== BigInt(executionIndex) ||
      native.language_tag !== BigInt(expectedLanguageTag) ||
      (native.origin_kind !== 0n && native.origin_kind !== 1n)
    )
      throw new Error(
        "retained validation witness selected a different execution source",
      );
    if (
      native.origin_kind === 0n &&
      !Buffer.from(native.source_key, "hex").equals(
        encodeCbor(native.source_index),
      )
    )
      throw new Error("retained inline source key is not canonical");
    const state = stateFromData(retained.machine_state);
    const proof = validationTraceProofCoreFromData(retained.trace_proof);
    if (
      !hashMidgardValidationMachineStateV1(state).equals(proof.stateHash) ||
      !verifyMidgardValidationTraceProofV1({ descriptor, proof }) ||
      !state.eventKeyHash.equals(
        hashMidgardValidationEventKeyV1(eventKeyCbor),
      ) ||
      !state.workRoot.equals(
        hashMidgardValidationWorkWitnessV1({
          phase: "nativeScripts",
          programCounter: state.programCounter,
          witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
        }),
      )
    )
      throw new Error(
        "retained validation state/proof/work witness is invalid",
      );
    const control = executionSourceNativeControlFromCborV1(
      Buffer.from(retained.witness_cbor, "hex"),
    );
    const purposeKind = exactNumber(native.purpose_kind, "purpose kind");
    if (purposeKind > 3)
      throw new Error("retained execution purpose kind is unsupported");
    if (
      expectedPurposeKind !== undefined &&
      purposeKind !== expectedPurposeKind
    )
      throw new Error("retained execution purpose kind changed");
    const purposeLeaf = hashMidgardScriptPurposeLeafV1({
      purposeKind: purposeKind as 0 | 1 | 2 | 3,
      purposeIndex: native.purpose_index,
      scriptHash: Buffer.from(native.script_hash, "hex"),
      subject: Buffer.from(native.subject, "hex"),
    });
    const sourceLeaf =
      native.origin_kind === 0n
        ? hashMidgardInlineScriptSourceLeafV1({
            sourceIndex: native.source_index,
            scriptLanguageTag: expectedLanguageTag,
            scriptHash: Buffer.from(native.script_hash, "hex"),
            scriptTotalLength: exactNumber(
              native.script_total_length,
              "script length",
            ),
            itemCommitment: Buffer.from(native.script_item_commitment, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeafV1({
            sourceKey: Buffer.from(native.source_key, "hex"),
            scriptLanguageTag: expectedLanguageTag,
            scriptHash: Buffer.from(native.script_hash, "hex"),
            scriptTotalLength: exactNumber(
              native.script_total_length,
              "script length",
            ),
            itemCommitment: Buffer.from(native.script_item_commitment, "hex"),
          });
    const executionLeaf = hashMidgardScriptExecutionLeafV1({
      languageTag: expectedLanguageTag,
      purposeLeaf,
      sourceLeaf,
      redeemerLeaf: Buffer.from(native.redeemer_leaf, "hex"),
    });
    requireMembership(
      merkleFrontier(control.purpose_count, control.purpose_peaks),
      native.purpose_index,
      purposeLeaf,
      native.purpose_siblings,
      "purpose",
    );
    requireMembership(
      merkleFrontier(control.source_count, control.source_peaks),
      native.source_index,
      sourceLeaf,
      native.source_siblings,
      "source",
    );
    requireMembership(
      merkleFrontier(control.execution_count, control.execution_peaks),
      native.execution_index,
      executionLeaf,
      native.execution_siblings,
      "execution",
    );
    let signerFrontierValid = false;
    try {
      signerFrontierValid = commitMidgardValidationMerkleFrontierV1(
        merkleFrontier(control.signer_count, native.signer_peaks),
      ).equals(Buffer.from(control.signer_frontier_commitment, "hex"));
    } catch {
      signerFrontierValid = false;
    }
    // Native decoding subsequently consumes signer material; scalar
    // purpose/language predicates do not, and non-native descriptors omit the
    // native-only signer peaks by construction.
    if (expectedLanguageTag === 0 && !signerFrontierValid)
      throw new Error("retained validation signer frontier is invalid");
    const root = await buildCountedRoot(
      ROOT_DOMAINS.validationTraces,
      descriptorEntries,
    );
    if (root.root !== expectedValidationTracesRoot)
      throw new Error("retained validation descriptor root changed");
    const descriptorProof = await keyValuePhasProof(
      { root: root.phasRoot, count: root.count, entries: root.entries },
      eventKeyCbor,
      descriptorMatches[0]!.value,
    );
    return {
      validationTracesRoot: root.root,
      validationTraceCount: root.count,
      authentication: {
        trace_membership: {
          domain: root.domain,
          root: root.root,
          phas_root: root.phasRoot,
          count: root.count,
          key: eventKey,
          value: descriptorData,
          proof: Data.from(Data.to(descriptorProof, Proof), Proof),
        },
        machine_state: retained.machine_state,
        trace_proof: retained.trace_proof,
        control: control as never,
        control_data: Data.from(
          Data.to(control as never, NativeScriptsControlV1Schema as never),
        ),
        absolute_purpose_index: native.purpose_index,
        required_script_hash: native.script_hash,
        purpose_kind: native.purpose_kind,
        purpose_index: native.purpose_index,
        script_hash: native.script_hash,
        purpose_subject: native.subject,
        purpose_siblings: native.purpose_siblings,
        source_index: native.source_index,
        origin_kind: native.origin_kind,
        source_key: native.source_key,
        language_tag: native.language_tag,
        total_length: native.script_total_length,
        item_commitment: native.script_item_commitment,
        source_siblings: native.source_siblings,
        redeemer_leaf: native.redeemer_leaf,
        execution_siblings: native.execution_siblings,
      },
    };
  };
