import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  verifyMidgardValidationMerkleMembership,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
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
import {
  coreFrontier,
  decodeMissingRedeemerStageTenControl,
  type EncodedEntry,
  exactNumber,
  machineState,
  type MissingRedeemerStageTenAuthentication,
} from "./retained-stage-ten.decode-missing-redeemer-stage-ten-control.js";

/** Strict callback-free reconstruction from public retained validation DA. */
export const buildMissingRedeemerStageTenAuthenticationFromRetainedDa = async ({
  eventKey,
  transactionId,
  purposeKind,
  purposeIndex,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
}: {
  eventKey: EventKey;
  transactionId: string;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  authenticatedValidationTraceEntries: readonly EncodedEntry[];
  retainedValidationWitnessEntries: readonly EncodedEntry[];
  expectedValidationTracesRoot: string;
}): Promise<MissingRedeemerStageTenAuthentication> => {
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const descriptorEntries = authenticatedValidationTraceEntries.map(
    ({ key, value }) => ({
      key: Buffer.from(key),
      value: Buffer.from(value),
    }),
  );
  const descriptorMatches = descriptorEntries.filter(({ key }) =>
    key.equals(eventKeyCbor),
  );
  if (descriptorMatches.length !== 1)
    throw new Error(
      "missingRedeemer validation descriptor is absent or duplicated",
    );
  const descriptorData = Data.from(
    descriptorMatches[0]!.value.toString("hex"),
    ValidationTraceDescriptorSchema,
  ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptor;
  const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
  const eventEntries = retainedValidationWitnessEntries.flatMap((entry) => {
    const key = decodeRetainedValidationWitnessKey(entry.key);
    const keyCbor = Buffer.from(
      Data.to(key.event_key as never, EventKeySchema),
      "hex",
    );
    return keyCbor.equals(eventKeyCbor)
      ? [{ key, retained: decodeRetainedValidationWitness(entry.value) }]
      : [];
  });
  const validated = eventEntries
    .filter(
      ({ retained }) =>
        retained.phase === 8n &&
        retained.machine_state.phase === "ScriptSources",
    )
    .map(({ key, retained }) => {
      const state = machineState(retained.machine_state);
      const proof = validationTraceProofCoreFromData(retained.trace_proof);
      if (
        retained.phase !== 8n ||
        retained.program_counter !== retained.machine_state.program_counter ||
        state.transactionId.toString("hex") !== transactionId ||
        !state.eventKeyHash.equals(
          hashMidgardValidationEventKey(eventKeyCbor),
        ) ||
        !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
        !verifyMidgardValidationTraceProof({ descriptor, proof }) ||
        !state.workRoot.equals(
          hashMidgardValidationWorkWitness({
            phase: "scriptSources",
            programCounter: state.programCounter,
            witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
          }),
        )
      )
        throw new Error(
          "missingRedeemer retained state/proof/work witness is invalid",
        );
      return { key, retained };
    });
  const acceptedEvent = "L2TransactionEventKey" in eventKey;
  // The producer's earliest stage-10 state whose discovery selects the exact
  // purpose. Every such state carries the same purpose and matched-source
  // frontiers, and the family's own committed field-8 scan, not the
  // producer's auxiliary, decides presence; the earliest state is the one a
  // producer whose trace stops at this purpose (an honest missing-redeemer
  // rejection, or its wrongful claim) is guaranteed to have committed.
  const selections = validated.flatMap(({ retained }) => {
    try {
      const control = decodeMissingRedeemerStageTenControl(
        Buffer.from(retained.witness_cbor, "hex"),
      );
      return control.discovery.current_purpose_kind === BigInt(purposeKind) &&
        control.discovery.current_purpose_index === BigInt(purposeIndex)
        ? [{ retained, control }]
        : [];
    } catch {
      return [];
    }
  });
  const terminals = [...selections]
    .sort((left, right) =>
      left.retained.program_counter < right.retained.program_counter
        ? -1
        : left.retained.program_counter > right.retained.program_counter
          ? 1
          : 0,
    )
    .slice(0, 1);
  if (terminals.length !== 1)
    throw new Error("missingRedeemer stage-10 purpose selection is absent");
  const { retained, control } = terminals[0]!;
  if (
    retained.machine_state.source_kind !==
      (acceptedEvent ? "Normal" : "Forced") ||
    descriptor.verdict !== (acceptedEvent ? "accepted" : "rejected") ||
    retained.machine_state.verdict !== "Pending"
  )
    throw new Error("missingRedeemer retained direction/verdict changed");
  const discovery = control.discovery;
  if (
    discovery.matched_language_tag !== 3n &&
    discovery.matched_language_tag !== 128n
  )
    throw new Error("missingRedeemer selected source is not Plutus");
  const purposeCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(
        typeof auxiliary === "object" && "ScriptPurposeScanWitness" in auxiliary
      )
    )
      return [];
    const purpose = auxiliary.ScriptPurposeScanWitness;
    return purpose.purpose_kind === BigInt(purposeKind) &&
      purpose.purpose_index === BigInt(purposeIndex) &&
      purpose.script_hash === discovery.current_script_hash &&
      purpose.subject === discovery.current_subject
      ? [purpose]
      : [];
  });
  const purposeUnique = new Map(
    purposeCandidates.map((value) => [
      [
        value.purpose_kind.toString(),
        value.purpose_index.toString(),
        value.script_hash,
        value.subject,
        value.siblings.join(":"),
      ].join("/"),
      value,
    ]),
  );
  if (purposeUnique.size !== 1)
    throw new Error(
      "missingRedeemer purpose membership witness is absent or ambiguous",
    );
  const purpose = [...purposeUnique.values()][0]!;
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex: BigInt(purposeIndex),
    scriptHash: Buffer.from(purpose.script_hash, "hex"),
    subject: Buffer.from(purpose.subject, "hex"),
  });
  if (
    !verifyMidgardValidationMerkleMembership({
      frontier: coreFrontier(control.purpose_count, control.purpose_peaks),
      leafIndex: exactNumber(
        discovery.purpose_cursor,
        "absolute purpose index",
      ),
      leafHash: purposeLeaf,
      siblings: purpose.siblings.map((value) => Buffer.from(value, "hex")),
    })
  )
    throw new Error("missingRedeemer purpose membership is invalid");
  const sourceCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(typeof auxiliary === "object" && "ScriptSourceScanWitness" in auxiliary)
    )
      return [];
    const source = auxiliary.ScriptSourceScanWitness;
    return source.source_index === discovery.matched_source_index &&
      source.script_language_tag === discovery.matched_language_tag &&
      source.script_hash === discovery.current_script_hash
      ? [source]
      : [];
  });
  const sourceUnique = new Map(
    sourceCandidates.map((value) => [
      [
        value.source_index.toString(),
        value.origin_kind.toString(),
        value.source_key,
        value.script_language_tag.toString(),
        value.script_hash,
        value.script_total_length.toString(),
        value.script_item_commitment,
        value.siblings.join(":"),
      ].join("/"),
      value,
    ]),
  );
  if (sourceUnique.size !== 1)
    throw new Error(
      "missingRedeemer source membership witness is absent or ambiguous",
    );
  const source = [...sourceUnique.values()][0]!;
  if (source.origin_kind !== 0n && source.origin_kind !== 1n)
    throw new Error("missingRedeemer source origin changed");
  if (
    source.origin_kind === 0n &&
    source.source_key !== encodeCbor(source.source_index).toString("hex")
  )
    throw new Error("missingRedeemer inline source key/index changed");
  const sourceLeaf =
    source.origin_kind === 0n
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: source.source_index,
          scriptLanguageTag: Number(source.script_language_tag) as 3 | 128,
          scriptHash: Buffer.from(source.script_hash, "hex"),
          scriptTotalLength: exactNumber(
            source.script_total_length,
            "source length",
          ),
          itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(source.source_key, "hex"),
          scriptLanguageTag: Number(source.script_language_tag) as 3 | 128,
          scriptHash: Buffer.from(source.script_hash, "hex"),
          scriptTotalLength: exactNumber(
            source.script_total_length,
            "source length",
          ),
          itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
        });
  if (
    sourceLeaf.toString("hex") !== discovery.matched_source_leaf ||
    !verifyMidgardValidationMerkleMembership({
      frontier: coreFrontier(control.source_count, control.source_peaks),
      leafIndex: exactNumber(source.source_index, "source index"),
      leafHash: sourceLeaf,
      siblings: source.siblings.map((value) => Buffer.from(value, "hex")),
    })
  )
    throw new Error("missingRedeemer source membership is invalid");
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  if (root.root !== expectedValidationTracesRoot)
    throw new Error("missingRedeemer retained validation root changed");
  const membership = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    eventKeyCbor,
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
    absolutePurposeIndex: discovery.purpose_cursor,
    purposeSiblings: purpose.siblings,
    sourceOriginKind: source.origin_kind,
    sourceKey: source.source_key,
    sourceLanguageTag: discovery.matched_language_tag,
    sourceScriptHash: source.script_hash,
    sourceTotalLength: source.script_total_length,
    sourceItemCommitment: source.script_item_commitment,
    sourceSiblings: source.siblings,
  });
};
