import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { Proof, ProofChunkDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  type DirectCapacityFailure,
  exact,
  OUT_REF,
  PROOF_CARRIAGE_RECOVERY,
  PROOF_CHUNK_PREREQUISITE,
  PROOF_CHUNK_PUBLICATION_RECOVERY,
  type ProofCarriageRecovery,
  type ProofChunkPublicationRecovery,
  type ProofChunkRequirement,
  record,
  requirementFor,
  sameJson,
  sha256,
  TX_HASH,
} from "./proof-chunk-prerequisite.route-action-identity.js";

export const proofCarriageRecovery = ({
  route,
  baseAction,
  requirement,
  directCapacityFailure,
  baseDurableRecovery,
  publicationDurableRecovery,
}: {
  readonly route: ProofCarriageRecovery["route"];
  readonly baseAction: FraudProofWorkflowAction;
  readonly requirement: ProofChunkRequirement;
  readonly directCapacityFailure?: DirectCapacityFailure;
  readonly baseDurableRecovery?: JournalJsonObject;
  readonly publicationDurableRecovery?: JournalJsonObject;
}): JournalJsonObject =>
  Object.freeze({
    proofCarriage: Object.freeze({
      schemaVersion: PROOF_CARRIAGE_RECOVERY,
      route,
      baseAction: Object.freeze({
        actionId: baseAction.actionId,
        input: Object.freeze({ ...baseAction.input }),
      }),
      proofCborSha256: requirement.proofCborSha256,
      ...(directCapacityFailure === undefined ? {} : { directCapacityFailure }),
      ...(baseDurableRecovery === undefined ? {} : { baseDurableRecovery }),
      ...(publicationDurableRecovery === undefined
        ? {}
        : { publicationDurableRecovery }),
    }),
  });

export const parseProofCarriageRecovery = ({
  value,
  requirement,
}: {
  readonly value: JournalJsonObject | undefined;
  readonly requirement: ProofChunkRequirement;
}): ProofCarriageRecovery => {
  const outer = exact(value, ["proofCarriage"], "proof-carriage recovery");
  const raw = record(outer.proofCarriage, "proof-carriage recovery payload");
  const route = raw.route;
  if (route !== "direct" && route !== "publication") {
    throw new Error("proof-carriage recovery has an unknown route");
  }
  const expectedKeys = [
    "schemaVersion",
    "route",
    "baseAction",
    "proofCborSha256",
    ...(route === "direct" && raw.baseDurableRecovery !== undefined
      ? ["baseDurableRecovery"]
      : []),
    ...(route === "publication"
      ? ["directCapacityFailure", "publicationDurableRecovery"]
      : []),
  ];
  exact(raw, expectedKeys, "proof-carriage recovery payload");
  if (
    raw.schemaVersion !== PROOF_CARRIAGE_RECOVERY ||
    raw.proofCborSha256 !== requirement.proofCborSha256
  ) {
    throw new Error("proof-carriage recovery changed its proof identity");
  }
  const rawAction = exact(
    raw.baseAction,
    ["actionId", "input"],
    "proof-carriage base action",
  );
  if (typeof rawAction.actionId !== "string") {
    throw new Error("proof-carriage recovery has an invalid base action");
  }
  const baseAction: FraudProofWorkflowAction = Object.freeze({
    actionId: rawAction.actionId,
    input: record(
      rawAction.input,
      "proof-carriage recovery base action input",
    ) as JournalJsonObject,
  });
  if (route === "direct") {
    return Object.freeze({
      schemaVersion: PROOF_CARRIAGE_RECOVERY,
      route,
      baseAction,
      proofCborSha256: requirement.proofCborSha256,
      ...(raw.baseDurableRecovery === undefined
        ? {}
        : {
            baseDurableRecovery: record(
              raw.baseDurableRecovery,
              "direct proof-carriage base recovery",
            ) as JournalJsonObject,
          }),
    });
  }
  const failure = exact(
    raw.directCapacityFailure,
    [
      "kind",
      "maximumTransactionBytes",
      "actualTransactionBytes",
      "errorSha256",
    ],
    "proof-carriage direct capacity failure",
  );
  if (
    failure.kind !== "max_tx_size" ||
    !Number.isSafeInteger(failure.maximumTransactionBytes) ||
    (failure.maximumTransactionBytes as number) <= 0 ||
    !Number.isSafeInteger(failure.actualTransactionBytes) ||
    (failure.actualTransactionBytes as number) <=
      (failure.maximumTransactionBytes as number) ||
    typeof failure.errorSha256 !== "string" ||
    !TX_HASH.test(failure.errorSha256)
  ) {
    throw new Error("proof-carriage direct capacity failure is invalid");
  }
  return Object.freeze({
    schemaVersion: PROOF_CARRIAGE_RECOVERY,
    route,
    baseAction,
    proofCborSha256: requirement.proofCborSha256,
    directCapacityFailure: Object.freeze({
      kind: "max_tx_size",
      maximumTransactionBytes: failure.maximumTransactionBytes as number,
      actualTransactionBytes: failure.actualTransactionBytes as number,
      errorSha256: failure.errorSha256,
    }),
    publicationDurableRecovery: record(
      raw.publicationDurableRecovery,
      "proof-carriage publication recovery",
    ) as JournalJsonObject,
  });
};

export const parseRecovery = ({
  value,
  requirement,
  txHash,
}: {
  readonly value: JournalJsonObject | undefined;
  readonly requirement: ProofChunkRequirement;
  readonly txHash?: string;
}): ProofChunkPublicationRecovery => {
  const outer = exact(
    value,
    ["proofChunkPublication"],
    "proof-chunk durable recovery",
  );
  const parsed = exact(
    outer.proofChunkPublication,
    ["schemaVersion", "proofCborSha256", "outputs"],
    "proof-chunk durable recovery payload",
  );
  if (
    parsed.schemaVersion !== PROOF_CHUNK_PUBLICATION_RECOVERY ||
    parsed.proofCborSha256 !== requirement.proofCborSha256 ||
    !Array.isArray(parsed.outputs) ||
    parsed.outputs.length !== requirement.chunkDatums.length
  ) {
    throw new Error("proof-chunk durable recovery changed its proof identity");
  }
  const seen = new Set<string>();
  const outputs = parsed.outputs.map((value, index) => {
    const output = exact(
      value,
      ["outRef", "datumCbor"],
      `proof-chunk durable recovery outputs[${index.toString()}]`,
    );
    if (
      typeof output.outRef !== "string" ||
      !OUT_REF.test(output.outRef) ||
      seen.has(output.outRef) ||
      (txHash !== undefined && !output.outRef.startsWith(`${txHash}#`)) ||
      output.datumCbor !== requirement.chunkDatums[index]
    ) {
      throw new Error("proof-chunk durable recovery changed an exact output");
    }
    seen.add(output.outRef);
    return Object.freeze({
      outRef: output.outRef,
      datumCbor: output.datumCbor,
    });
  });
  return Object.freeze({
    schemaVersion: PROOF_CHUNK_PUBLICATION_RECOVERY,
    proofCborSha256: requirement.proofCborSha256,
    outputs: Object.freeze(outputs),
  });
};

/**
 * Reconstructs a previously captured proof from its journaled chunk outputs.
 * Reconciliation must not ask a live proof source for today's registry root:
 * a concurrent mutation can make a correctly submitted old publication stale
 * without making that publication disappear from L1 history.
 */
export const requirementFromJournaledAction = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  action,
  durableRecovery,
}: {
  readonly category: Category;
  readonly action: FraudProofWorkflowAction;
  readonly durableRecovery: JournalJsonObject | undefined;
}): ProofChunkRequirement => {
  const input = exact(
    action.input,
    [
      "schemaVersion",
      "category",
      "stage",
      "forAction",
      "proofCborSha256",
      "chunkDatumSha256s",
    ],
    `${category} journaled proof-chunk action`,
  );
  if (
    input.schemaVersion !== PROOF_CHUNK_PREREQUISITE ||
    input.category !== category ||
    input.stage !== "direct_or_publish_proof" ||
    typeof input.proofCborSha256 !== "string" ||
    !Array.isArray(input.chunkDatumSha256s)
  ) {
    throw new Error(
      `${category} journaled proof-chunk action changed identity`,
    );
  }
  const baseAction = exact(
    input.forAction,
    ["actionId", "input"],
    `${category} journaled proof-chunk base action`,
  );
  if (typeof baseAction.actionId !== "string") {
    throw new Error(`${category} journaled proof-chunk base action is invalid`);
  }
  record(
    baseAction.input,
    `${category} journaled proof-chunk base action input`,
  );
  const outer = exact(
    durableRecovery,
    ["proofChunkPublication"],
    "proof-chunk durable recovery",
  );
  const recovery = exact(
    outer.proofChunkPublication,
    ["schemaVersion", "proofCborSha256", "outputs"],
    "proof-chunk durable recovery payload",
  );
  if (
    recovery.schemaVersion !== PROOF_CHUNK_PUBLICATION_RECOVERY ||
    recovery.proofCborSha256 !== input.proofCborSha256 ||
    !Array.isArray(recovery.outputs) ||
    recovery.outputs.length !== input.chunkDatumSha256s.length
  ) {
    throw new Error("proof-chunk durable recovery changed its proof identity");
  }
  const steps: unknown[] = [];
  for (const [index, value] of recovery.outputs.entries()) {
    const output = exact(
      value,
      ["outRef", "datumCbor"],
      `proof-chunk durable recovery outputs[${index.toString()}]`,
    );
    if (
      typeof output.datumCbor !== "string" ||
      sha256(output.datumCbor) !== input.chunkDatumSha256s[index]
    ) {
      throw new Error("proof-chunk durable recovery changed a chunk identity");
    }
    let chunk: { readonly proof_steps: readonly unknown[] };
    try {
      chunk = Data.from(output.datumCbor, ProofChunkDatum) as unknown as {
        readonly proof_steps: readonly unknown[];
      };
    } catch {
      throw new Error("proof-chunk durable recovery contains malformed steps");
    }
    steps.push(...chunk.proof_steps);
  }
  const proofCbor = canonicalPlutusDataCbor(Data.to(steps as never, Proof));
  const requirement = requirementFor({
    proofCbor,
    label: `${category} journaled proof chunks`,
  });
  if (
    requirement.proofCborSha256 !== input.proofCborSha256 ||
    !sameJson(requirement.chunkDatumSha256s, input.chunkDatumSha256s)
  ) {
    throw new Error("proof-chunk durable recovery does not rebuild its proof");
  }
  return requirement;
};
