import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type CountedRoot,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "./phas.js";
import {
  encodeData,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import { buildSourceMembershipProof } from "./witnesses.build-source-membership-proof.js";

const phasView = (root: CountedRoot) => ({
  root: root.phasRoot,
  count: root.count,
  entries: root.entries,
});
const valueHash = (value: Buffer): string =>
  computeHash32(value).toString("hex");

export const validationRunBytesAreWellFormed = (bytes: Buffer): boolean => {
  try {
    const raw = bytes.toString("hex");
    const d = Data.from(raw, SDK.ValidationTraceDescriptor);
    return (
      Data.to(d, SDK.ValidationTraceDescriptor) === raw &&
      d.schema_version === 1n &&
      d.machine_version === 1n &&
      d.step_count > 0n &&
      d.step_count <= 4294967295n &&
      d.verdict !== "Pending" &&
      (d.verdict === "Rejected"
        ? d.rejection_code_hash !== "00".repeat(32)
        : d.rejection_code_hash === "00".repeat(32))
    );
  } catch {
    return false;
  }
};

export const buildMissingValidationRunFault = async (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
): Promise<SDK.TransitionFault> => {
  const run = reconstruction.rootData.validationTraces;
  return SDK.sourceMembershipMismatchFault({
    SourceEventMissingValidationRun: {
      source: await buildSourceMembershipProof({ reconstruction, eventKey }),
      run_phas_root: run.phasRoot,
      run_absence_proof: await keyValuePhasNonMembershipProof(
        phasView(run),
        encodeData(eventKey, SDK.EventKeySchema),
      ),
    },
  });
};

export const buildForeignValidationRunFault = async (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey | Buffer,
  value: Buffer,
): Promise<SDK.TransitionFault> => {
  const run = reconstruction.rootData.validationTraces;
  const rawKey = Buffer.isBuffer(eventKey)
    ? eventKey
    : encodeData(eventKey, SDK.EventKeySchema);
  let canonicalKey: SDK.EventKey | undefined;
  try {
    const decoded = Data.from(rawKey.toString("hex"), SDK.EventKey);
    if (encodeData(decoded, SDK.EventKeySchema).equals(rawKey))
      canonicalKey = decoded;
  } catch {
    /* An invalid raw key is itself foreign. */
  }
  let source: CountedRoot | undefined;
  let key: Buffer | undefined;
  if (
    canonicalKey !== undefined &&
    "ForcedTransactionEventKey" in canonicalKey
  ) {
    source = reconstruction.rootData.forcedTransactions;
    key = encodeData(
      canonicalKey.ForcedTransactionEventKey.tx_order_id,
      SDK.OutputReferenceSchema,
    );
  }
  if (canonicalKey !== undefined && "L2TransactionEventKey" in canonicalKey) {
    source = reconstruction.rootData.transactions;
    key = Buffer.from(canonicalKey.L2TransactionEventKey.tx_id, "hex");
  }
  return SDK.sourceMembershipMismatchFault({
    ForeignValidationRun: {
      event_key: rawKey.toString("hex"),
      value_hash: valueHash(value),
      run_phas_root: run.phasRoot,
      run_proof: await keyValuePhasProof(phasView(run), rawKey, value),
      source_phas_root: source?.phasRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
      source_absence_proof:
        source !== undefined && key !== undefined
          ? await keyValuePhasNonMembershipProof(phasView(source), key)
          : [],
    },
  });
};

export const buildMalformedValidationRunFault = async (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
  value: Buffer,
): Promise<SDK.TransitionFault> => {
  const run = reconstruction.rootData.validationTraces;
  return SDK.sourceMembershipMismatchFault({
    MalformedValidationRun: {
      event_key: eventKey,
      value: value.toString("hex"),
      run_phas_root: run.phasRoot,
      run_proof: await keyValuePhasProof(
        phasView(run),
        encodeData(eventKey, SDK.EventKeySchema),
        value,
      ),
    },
  });
};
