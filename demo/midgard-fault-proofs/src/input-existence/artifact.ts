/** Durable input-existence material is admitted before any fee reservation.
 * All roots, source bytes and the event's ledger proof are independently replayed.
 */
import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  computeHash32,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/native";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { eventKeyPhase } from "../transition-trace/reconstruct.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
} from "../workflow/native-index-artifact.js";
import type {
  PreparedInputExistence,
  PreparedInputExistenceLedgerEvidence,
} from "./prepare.js";
import { deriveInputExistenceSelection } from "./selection.js";
import type { OpenedProofThreadSource } from "./source.js";

export const INPUT_EXISTENCE_ARTIFACT = "midgard-input-existence-artifact-v1";

const proofJson = (proof: SDK.Proof) =>
  proof.map((step) => {
    if ("Branch" in step)
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    if ("Fork" in step)
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const verifyMpf = ({
  root,
  key,
  value,
  proof,
}: {
  readonly root: string;
  readonly key: string;
  readonly value?: string;
  readonly proof: SDK.Proof;
}): void => {
  const steps = proofJson(proof);
  // The common parser checks the same cursor/neighbor grammar as the onchain door.
  parseMidgardMpfProofJson(steps);
  const opened = MpfProof.fromJSON(
    Buffer.from(key, "hex"),
    value === undefined ? undefined : Buffer.from(value, "hex"),
    steps,
  ).verify(value !== undefined);
  const actual =
    opened === null
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : Buffer.from(opened).toString("hex");
  if (actual !== root)
    throw new Error("input-existence artifact: MPF root changed");
};

const verifyCounted = <K, V>(
  witness: SDK.RootMembershipProof<K, V>,
  domain: SDK.RootDomain,
  root: string,
  count: bigint,
  key: string,
  value: string,
): void => {
  if (
    count <= 0n ||
    witness.domain !== domain ||
    witness.root !== root ||
    witness.count !== count ||
    computeHash32(
      Buffer.from(
        SDK.countedRootPreimageHex({
          domain,
          phasRoot: witness.phas_root,
          count,
        }),
        "hex",
      ),
    ).toString("hex") !== root
  ) {
    throw new Error("input-existence artifact: counted root changed");
  }
  verifyMpf({ root: witness.phas_root, key, value, proof: witness.proof });
};

const canonicalCbor = <T>(value: unknown, schema: T, label: string): T => {
  const cbor = canonicalHex(value, EVEN_HEX, label);
  const decoded = Data.from(cbor, schema);
  if (Data.to(decoded as never, schema as never) !== cbor)
    throw new Error(`${label} is noncanonical`);
  return decoded;
};

export const inputExistenceArtifact = (
  prepared: PreparedInputExistence,
): JournalJsonObject =>
  normalizeJournalJson({
    schemaVersion: INPUT_EXISTENCE_ARTIFACT,
    field: prepared.field,
    headerHash: prepared.source.headerHash,
    header: Data.to(prepared.source.header, SDK.Header),
    source:
      prepared.source.kind === "accepted"
        ? { kind: "accepted", inclusion: prepared.source.inclusion }
        : {
            kind: "forced",
            membership: Data.to(
              prepared.source.membership,
              SDK.ForcedTransactionSourceMembershipProof,
            ),
          },
    transaction: prepared.source.fullTransactionCbor,
    inputIndex: prepared.inputIndex.toString(),
    event: Data.to(prepared.eventMembership, SDK.EventToStepMembershipProof),
    transition: Data.to(prepared.transitionMembership, SDK.IndexedTraceProof),
    ledger:
      prepared.ledgerEvidence === "OutOfDomain"
        ? "OutOfDomain"
        : "Present" in prepared.ledgerEvidence
          ? {
              kind: "present",
              value: prepared.ledgerEvidence.Present.value,
              proof: Data.to(prepared.ledgerEvidence.Present.proof, SDK.Proof),
            }
          : {
              kind: "absent",
              proof: Data.to(prepared.ledgerEvidence.Absent.proof, SDK.Proof),
            },
  }) as JournalJsonObject;

export const admitInputExistenceArtifact = (
  value: unknown,
  owner: string,
): PreparedInputExistence => {
  const artifact = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "field",
      "headerHash",
      "header",
      "source",
      "transaction",
      "inputIndex",
      "event",
      "transition",
      "ledger",
    ],
    "input-existence artifact",
  );
  if (
    artifact.schemaVersion !== INPUT_EXISTENCE_ARTIFACT ||
    (artifact.field !== 0 && artifact.field !== 1)
  ) {
    throw new Error("input-existence artifact: identity changed");
  }
  const field = artifact.field;
  const header = canonicalCbor(
    artifact.header,
    SDK.Header,
    "input-existence header",
  );
  const headerHash = canonicalHex(
    artifact.headerHash,
    HEX_28,
    "input-existence header hash",
  );
  if (
    header.protocolVersion !== 1n ||
    computeHash28(SDK.encodeHeaderCbor(header)).toString("hex") !== headerHash
  ) {
    throw new Error("input-existence artifact: header changed");
  }
  const transaction = canonicalHex(
    artifact.transaction,
    EVEN_HEX,
    "input-existence transaction",
  );
  const sourceRecord = artifact.source;
  if (
    typeof sourceRecord !== "object" ||
    sourceRecord === null ||
    Array.isArray(sourceRecord)
  ) {
    throw new Error("input-existence artifact: source is absent");
  }
  const accepted = "kind" in sourceRecord && sourceRecord.kind === "accepted";
  const record = exactJournalRecord(
    sourceRecord,
    accepted ? ["kind", "inclusion"] : ["kind", "membership"],
    "input-existence source",
  );
  let source: OpenedProofThreadSource;
  if (accepted) {
    const inclusion = admitNativeInclusionArtifact(
      record.inclusion,
      "input-existence accepted source",
    ).artifact;
    const committed = Data.from(
      inclusion.l2TransactionSourceCbor,
      SDK.L2TransactionSource,
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction, "hex"),
    );
    if (material.canonical.validity !== "TxIsValid")
      throw new Error(
        "input-existence artifact: normal verdict is not acceptance",
      );
    const countRoot = computeHash32(
      Buffer.from(
        SDK.countedRootPreimageHex({
          domain: SDK.ROOT_DOMAINS.transactionsV1,
          phasRoot: inclusion.transactionsPhasRoot,
          count: header.l2TransactionCount,
        }),
        "hex",
      ),
    ).toString("hex");
    if (
      header.l2TransactionCount <= 0n ||
      countRoot !== header.transactionsRoot ||
      material.transactionId.toString("hex") !== inclusion.nativeTxId ||
      committed.tx_id !== inclusion.nativeTxId ||
      material.proofSource.compactCbor.toString("hex") !==
        inclusion.nativeTxCompactCbor ||
      committed.source.compact_cbor !== inclusion.nativeTxCompactCbor ||
      committed.source.witness_set_compact_cbor !==
        material.proofSource.witnessSetCompactCbor.toString("hex") ||
      committed.source.field_preimage_lengths_cbor !==
        material.proofSource.fieldPreimageLengthsCbor.toString("hex")
    ) {
      throw new Error("input-existence artifact: normal source changed");
    }
    source = {
      kind: "accepted",
      header,
      headerHash,
      inclusion,
      eventKey: { L2TransactionEventKey: { tx_id: inclusion.nativeTxId } },
      subject: SDK.acceptedVerdictSubject(inclusion.nativeTxId),
      compactCbor: inclusion.nativeTxCompactCbor,
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      fieldPreimageLengthsCbor:
        material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      fullTransactionCbor: transaction,
      fieldPreimages: material.fieldPreimages,
    };
  } else {
    if (record.kind !== "forced")
      throw new Error("input-existence artifact: unknown source kind");
    const membership = canonicalCbor(
      record.membership,
      SDK.ForcedTransactionSourceMembershipProof,
      "input-existence forced membership",
    );
    const leaf = membership.value;
    verifyCounted(
      membership,
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      header.forcedTransactionsRoot,
      header.forcedTransactionCount,
      Data.to(membership.key, SDK.OutputReference),
      Data.to(leaf, SDK.ForcedInclusionTxV1),
    );
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      Buffer.from(transaction, "hex"),
    );
    if (
      material.transactionId.toString("hex") !== leaf.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        leaf.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        leaf.submitted_source.witness_set_compact_cbor ||
      material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
        leaf.submitted_source.field_preimage_lengths_cbor
    ) {
      throw new Error("input-existence artifact: forced source changed");
    }
    source = {
      kind: "forced",
      header,
      headerHash,
      membership,
      eventKey: { ForcedTransactionEventKey: { tx_order_id: membership.key } },
      subject: SDK.forcedVerdictSubject({
        transactionId: leaf.tx_id,
        sourceKey: membership.key,
        rejectionReason:
          leaf.verdict === "ForcedTxValid"
            ? null
            : leaf.verdict.ForcedTxInvalid.reason,
      }),
      compactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      fieldPreimageLengthsCbor:
        material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      fullTransactionCbor: transaction,
      fieldPreimages: material.fieldPreimages,
    };
  }
  if (
    typeof artifact.inputIndex !== "string" ||
    !/^(?:0|-?[1-9][0-9]*)$/u.test(artifact.inputIndex)
  )
    throw new Error("input-existence artifact: noncanonical input index");
  const selection = deriveInputExistenceSelection({
    source,
    field,
    inputIndex: BigInt(artifact.inputIndex),
    owner,
  });
  const { inputIndex, inputItems, selectedInput } = selection;
  const selected =
    selectedInput === null ? undefined : inputItems[Number(inputIndex)];
  const eventMembership = canonicalCbor(
    artifact.event,
    SDK.EventToStepMembershipProof,
    "input-existence event",
  );
  const transitionMembership = canonicalCbor(
    artifact.transition,
    SDK.IndexedTraceProof,
    "input-existence transition",
  );
  verifyCounted(
    eventMembership,
    SDK.ROOT_DOMAINS.eventToStep,
    header.eventToStepRoot,
    header.totalEventCount,
    Data.to(eventMembership.key, SDK.EventKey),
    Data.to(eventMembership.value, SDK.EventToStepValue),
  );
  verifyCounted(
    transitionMembership,
    SDK.ROOT_DOMAINS.transitionTrace,
    header.transitionTraceRoot,
    header.transitionStepCount,
    Data.to(transitionMembership.key),
    Data.to(transitionMembership.value, SDK.TransitionStep),
  );
  const step = transitionMembership.value;
  if (
    Data.to(eventMembership.key, SDK.EventKey) !==
      Data.to(source.eventKey, SDK.EventKey) ||
    Data.to(step.event_key, SDK.EventKey) !==
      Data.to(source.eventKey, SDK.EventKey) ||
    eventMembership.value.phase !== eventKeyPhase(source.eventKey) ||
    step.phase !== eventKeyPhase(source.eventKey) ||
    step.schema_version !== 1n ||
    step.step_index !== eventMembership.value.step_index ||
    step.step_index !== transitionMembership.key ||
    step.step_index < 0n ||
    step.step_index >= header.transitionStepCount
  ) {
    throw new Error("input-existence artifact: event/transition changed");
  }
  let ledgerEvidence: PreparedInputExistenceLedgerEvidence;
  if (selected === undefined) {
    if (artifact.ledger !== "OutOfDomain")
      throw new Error(
        "input-existence artifact: impossible coordinate carries a witness",
      );
    ledgerEvidence = "OutOfDomain";
  } else {
    const raw = artifact.ledger;
    if (typeof raw !== "object" || raw === null || Array.isArray(raw))
      throw new Error("input-existence artifact: ledger witness absent");
    const present = "kind" in raw && raw.kind === "present";
    const ledger = exactJournalRecord(
      raw,
      present ? ["kind", "value", "proof"] : ["kind", "proof"],
      "input-existence ledger",
    );
    const proof = canonicalCbor(
      ledger.proof,
      SDK.Proof,
      "input-existence ledger proof",
    );
    if (present) {
      const entryValue = canonicalHex(
        ledger.value,
        EVEN_HEX,
        "input-existence ledger value",
      );
      verifyMpf({
        root: step.pre_utxos_root,
        key: selected.toString("hex"),
        value: entryValue,
        proof,
      });
      ledgerEvidence = {
        Present: {
          value: entryValue,
          value_hash: computeHash32(Buffer.from(entryValue, "hex")).toString(
            "hex",
          ),
          proof,
        },
      };
    } else {
      if (ledger.kind !== "absent")
        throw new Error("input-existence artifact: unknown ledger evidence");
      verifyMpf({
        root: step.pre_utxos_root,
        key: selected.toString("hex"),
        proof,
      });
      ledgerEvidence = { Absent: { proof } };
    }
  }
  return {
    field,
    source,
    ...selection,
    eventMembership,
    transitionMembership,
    ledgerEvidence,
  };
};
