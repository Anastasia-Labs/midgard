import { createHash } from "node:crypto";

import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { Proof, ProofChunkDatum } from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type Network,
} from "@lucid-evolution/lucid";

import {
  type PublishedProofChunk,
  publishProofChunks,
  resolvePublishedProofChunks,
  splitProofIntoChunkDatums,
} from "../publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "./journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
} from "./orchestrator.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import {
  bindWorkflowPreflightTransaction,
  captureLocallyEvaluatedTransaction,
  copyWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

export const PROOF_CHUNK_PREREQUISITE =
  "midgard-production-proof-chunk-prerequisite-v1" as const;
export const PROOF_CHUNK_PUBLICATION_RECOVERY =
  "midgard-production-proof-chunk-publication-recovery-v1" as const;
export const PROOF_CARRIAGE_RECOVERY =
  "midgard-production-proof-carriage-recovery-v1" as const;

const TX_HASH = /^[0-9a-f]{64}$/u;
const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

type ProofChunkRequirement = Readonly<{
  proofCbor: string;
  proofCborSha256: string;
  chunkDatums: readonly string[];
  chunkDatumSha256s: readonly string[];
}>;

type ProofChunkPublicationRecovery = Readonly<{
  schemaVersion: typeof PROOF_CHUNK_PUBLICATION_RECOVERY;
  proofCborSha256: string;
  outputs: readonly Readonly<{ outRef: string; datumCbor: string }>[];
}>;

type DirectCapacityFailure = Readonly<{
  kind: "max_tx_size";
  maximumTransactionBytes: number;
  actualTransactionBytes: number;
  errorSha256: string;
}>;

type ProofCarriageRecovery = Readonly<{
  schemaVersion: typeof PROOF_CARRIAGE_RECOVERY;
  route: "direct" | "publication";
  baseAction: FraudProofWorkflowAction;
  proofCborSha256: string;
  directCapacityFailure?: DirectCapacityFailure;
  baseDurableRecovery?: JournalJsonObject;
  publicationDurableRecovery?: JournalJsonObject;
}>;

export interface ProofChunkPrerequisitePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof PROOF_CHUNK_PREREQUISITE;
  readonly category: Category;
  classifyDirectCapacityFailure(cause: unknown): DirectCapacityFailure;
  inspect(input: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<
    | { readonly kind: "not_required" | "satisfied" }
    | { readonly kind: "pending"; readonly reason: string }
    | { readonly kind: "required"; readonly action: FraudProofWorkflowAction }
  >;
  capture(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<{
    readonly transaction: LocallyEvaluatedTransaction;
    readonly durableRecovery: JournalJsonObject;
  }>;
  reconcile(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly txHash?: string;
    readonly durableRecovery?: JournalJsonObject;
  }): Promise<FraudProofWorkflowReconcileResult>;
}

const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

type DirectFirstProofCarriageRoute = "direct" | "publication";

const directFirstProofCarriageRouteByAction = new WeakMap<
  object,
  DirectFirstProofCarriageRoute
>();

const withDirectFirstProofCarriageRoute = async <Result>({
  action,
  route,
  run,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly route: DirectFirstProofCarriageRoute;
  readonly run: () => Promise<Result>;
}): Promise<Result> => {
  if (directFirstProofCarriageRouteByAction.has(action)) {
    throw new Error("proof-carriage action already has an active route");
  }
  directFirstProofCarriageRouteByAction.set(action, route);
  try {
    return await run();
  } finally {
    directFirstProofCarriageRouteByAction.delete(action);
  }
};

/**
 * Resolves an already-authorized published route without making publication a
 * prerequisite for the direct fit attempt. `undefined` is deliberately the
 * direct route: the outer adapter will only revisit this after an exact
 * release-bound capacity refusal and a journal-confirmed publication.
 */
export const resolveDirectFirstProofChunks = async ({
  action,
  lucid,
  address,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly proofCbor: string;
}): Promise<readonly PublishedProofChunk[]> => {
  const route = directFirstProofCarriageRouteByAction.get(action);
  if (route === undefined) {
    throw new Error(
      "proof chunks were requested outside an admitted direct-first route",
    );
  }
  if (route === "direct") return [];
  const chunks = await resolvePublishedProofChunks({
    lucid,
    address,
    proofCbor,
  });
  if (chunks === undefined) {
    throw new Error(
      "journal-authorized proof publication has no exact complete output set",
    );
  }
  return chunks;
};

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const requirementFor = ({
  proofCbor,
  label,
}: {
  readonly proofCbor: string;
  readonly label: string;
}): ProofChunkRequirement => {
  if (typeof proofCbor !== "string" || proofCbor.length === 0) {
    throw new Error(`${label} omitted its canonical MPF proof`);
  }
  let canonical: string;
  try {
    canonical = canonicalPlutusDataCbor(
      Data.to(Data.from(proofCbor, Proof), Proof),
    );
  } catch {
    throw new Error(`${label} MPF proof is not canonical PlutusData CBOR`);
  }
  if (canonical !== proofCbor) {
    throw new Error(`${label} MPF proof is not canonical PlutusData CBOR`);
  }
  const chunkDatums = Object.freeze([...splitProofIntoChunkDatums(proofCbor)]);
  return Object.freeze({
    proofCbor,
    proofCborSha256: sha256(proofCbor),
    chunkDatums,
    chunkDatumSha256s: Object.freeze(chunkDatums.map(sha256)),
  });
};

const publicationAction = <Category extends FraudProofCatalogueCategoryName>({
  category,
  baseAction,
  requirement,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowAction;
  readonly requirement: ProofChunkRequirement;
}): FraudProofWorkflowAction => {
  const frozenBaseAction = Object.freeze({
    actionId: baseAction.actionId,
    input: Object.freeze({ ...baseAction.input }),
  });
  return Object.freeze({
    actionId: `publish-proof-chunks:${baseAction.actionId}:${requirement.proofCborSha256}`,
    input: Object.freeze({
      schemaVersion: PROOF_CHUNK_PREREQUISITE,
      category,
      stage: "direct_or_publish_proof",
      forAction: frozenBaseAction,
      proofCborSha256: requirement.proofCborSha256,
      chunkDatumSha256s: requirement.chunkDatumSha256s,
    }),
  });
};

const isPublicationAction = (action: FraudProofWorkflowAction): boolean =>
  action.input.schemaVersion === PROOF_CHUNK_PREREQUISITE &&
  action.input.stage === "direct_or_publish_proof";

const routeActionIdentity = (
  action: FraudProofWorkflowAction,
): Readonly<{
  baseAction: FraudProofWorkflowAction;
  requirement: ProofChunkRequirement;
}> => {
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
    "proof-carriage route action",
  );
  if (
    input.schemaVersion !== PROOF_CHUNK_PREREQUISITE ||
    input.stage !== "direct_or_publish_proof" ||
    typeof input.proofCborSha256 !== "string" ||
    !TX_HASH.test(input.proofCborSha256) ||
    !Array.isArray(input.chunkDatumSha256s) ||
    input.chunkDatumSha256s.some(
      (digest) => typeof digest !== "string" || !TX_HASH.test(digest),
    )
  ) {
    throw new Error("proof-carriage route action changed identity");
  }
  const rawAction = exact(
    input.forAction,
    ["actionId", "input"],
    "proof-carriage route base action",
  );
  if (typeof rawAction.actionId !== "string") {
    throw new Error("proof-carriage route base action is invalid");
  }
  return Object.freeze({
    baseAction: Object.freeze({
      actionId: rawAction.actionId,
      input: record(
        rawAction.input,
        "proof-carriage route base action input",
      ) as JournalJsonObject,
    }),
    requirement: Object.freeze({
      proofCbor: "",
      proofCborSha256: input.proofCborSha256,
      chunkDatums: Object.freeze([]),
      chunkDatumSha256s: Object.freeze(
        input.chunkDatumSha256s as readonly string[],
      ),
    }),
  });
};

const proofCarriageRecovery = ({
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

const parseProofCarriageRecovery = ({
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

const parseRecovery = ({
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
const requirementFromJournaledAction = <
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

const recoveryForTransaction = ({
  transaction,
  requirement,
  address,
}: {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly requirement: ProofChunkRequirement;
  readonly address: string;
}): JournalJsonObject => {
  if (transaction.referenceScripts.length !== 0) {
    throw new Error("proof-chunk publication unexpectedly used a script");
  }
  requireReferenceOnlyScriptWitnesses({
    transaction,
    label: "proof-chunk publication",
  });
  const outputs = transaction.signed.toTransaction().body().outputs();
  const claimed = new Set<number>();
  const recovered = requirement.chunkDatums.map((datumCbor) => {
    let outputIndex = -1;
    for (let index = 0; index < outputs.len(); index += 1) {
      if (claimed.has(index)) continue;
      const output = outputs.get(index);
      const decoded = coreToTxOutput(output);
      if (
        decoded.address === address &&
        output.datum_hash() === undefined &&
        output.datum()?.as_datum()?.to_canonical_cbor_hex() === datumCbor &&
        output.script_ref() === undefined &&
        Object.entries(decoded.assets).every(
          ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
        )
      ) {
        outputIndex = index;
        break;
      }
    }
    if (outputIndex < 0) {
      throw new Error(
        "proof-chunk publication body omitted an exact ADA-only inline-datum output",
      );
    }
    claimed.add(outputIndex);
    return Object.freeze({
      outRef: `${transaction.txHash}#${outputIndex.toString()}`,
      datumCbor,
    });
  });
  return Object.freeze({
    proofChunkPublication: Object.freeze({
      schemaVersion: PROOF_CHUNK_PUBLICATION_RECOVERY,
      proofCborSha256: requirement.proofCborSha256,
      outputs: Object.freeze(recovered),
    }),
  });
};

/**
 * Concrete authenticated publication port. Lucid is used only to build/query
 * candidate UTxOs; release-final admission is always performed by the raw-L1
 * publication observer.
 */
export const createAuthenticatedProofChunkPrerequisitePort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  network,
  signer,
  publications,
  proofCborForAction,
  maximumTransactionBytes,
  transactionConfirmed,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly proofCborForAction: (input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) => string | null | Promise<string | null>;
  /** Exact `cardanoProtocolParameters.snapshot.maxTxSize` from the manifest. */
  readonly maximumTransactionBytes?: string | number;
  readonly transactionConfirmed: (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => Promise<boolean>;
}): ProofChunkPrerequisitePort<Category> => {
  if (
    publications.observerVersion !==
    FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER
  ) {
    throw new Error(`${category} proof chunks require a raw-L1 observer`);
  }
  const releaseMaximumTransactionBytes =
    maximumTransactionBytes === undefined
      ? undefined
      : typeof maximumTransactionBytes === "string"
        ? Number(maximumTransactionBytes)
        : maximumTransactionBytes;
  if (
    releaseMaximumTransactionBytes !== undefined &&
    (!Number.isSafeInteger(releaseMaximumTransactionBytes) ||
      releaseMaximumTransactionBytes <= 0 ||
      String(releaseMaximumTransactionBytes) !==
        String(maximumTransactionBytes))
  ) {
    throw new Error(
      `${category} proof carriage requires canonical release maxTxSize`,
    );
  }
  const requirement = async ({
    headerHash,
    action,
    artifact,
  }: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<ProofChunkRequirement | null> => {
    const proofCbor = await proofCborForAction({
      headerHash,
      action,
      artifact,
    });
    return proofCbor === null
      ? null
      : requirementFor({ proofCbor, label: `${category} ${action.actionId}` });
  };
  const exactAction = async ({
    headerHash,
    action,
    artifact,
  }: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<{ readonly requirement: ProofChunkRequirement }> => {
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
      `${category} proof-chunk publication action`,
    );
    if (
      input.schemaVersion !== PROOF_CHUNK_PREREQUISITE ||
      input.category !== category ||
      input.stage !== "direct_or_publish_proof"
    ) {
      throw new Error(`${category} proof-chunk action changed identity`);
    }
    const parsedBaseAction = exact(
      input.forAction,
      ["actionId", "input"],
      `${category} proof-chunk base action`,
    );
    if (typeof parsedBaseAction.actionId !== "string") {
      throw new Error(`${category} proof-chunk base action changed identity`);
    }
    const baseAction: FraudProofWorkflowAction = {
      actionId: parsedBaseAction.actionId,
      input: record(
        parsedBaseAction.input,
        `${category} proof-chunk base action input`,
      ) as JournalJsonObject,
    };
    const required = await requirement({
      headerHash,
      action: baseAction,
      artifact,
    });
    if (required === null) {
      throw new Error(`${category} proof-chunk action is no longer required`);
    }
    const expected = publicationAction({
      category,
      baseAction,
      requirement: required,
    });
    if (!sameJson(action, expected)) {
      throw new Error(`${category} proof-chunk action changed its proof`);
    }
    return { requirement: required };
  };
  const observeOutputs = async ({
    headerHash,
    outputs,
  }: {
    readonly headerHash: string;
    readonly outputs: readonly Readonly<{
      outRef: string;
      datumCbor: string;
    }>[];
  }): Promise<readonly boolean[]> =>
    await Promise.all(
      outputs.map(
        async (output) =>
          (
            await publications.observeExact({
              headerHash,
              kind: "proof_chunk",
              address: signer.address,
              expectedOutRef: output.outRef,
              expectedDatumCbor: output.datumCbor,
            })
          ).kind === "confirmed",
      ),
    );
  const port: ProofChunkPrerequisitePort<Category> = {
    portVersion: PROOF_CHUNK_PREREQUISITE,
    category,
    classifyDirectCapacityFailure: (cause) => {
      const message = cause instanceof Error ? cause.message : String(cause);
      const matched =
        /Max transaction size of (\d+) exceeded\. Found: (\d+)/u.exec(message);
      if (matched === null) {
        throw cause instanceof Error
          ? cause
          : new Error(
              `${category} direct proof preflight failed for a non-capacity reason: ${message}`,
            );
      }
      const maximum = Number(matched[1]);
      const actual = Number(matched[2]);
      if (
        releaseMaximumTransactionBytes === undefined ||
        maximum !== releaseMaximumTransactionBytes ||
        !Number.isSafeInteger(actual) ||
        actual <= maximum
      ) {
        throw new Error(
          `${category} direct proof capacity failure does not match the release-bound maxTxSize`,
        );
      }
      return Object.freeze({
        kind: "max_tx_size" as const,
        maximumTransactionBytes: maximum,
        actualTransactionBytes: actual,
        errorSha256: sha256(message),
      });
    },
    inspect: async ({ headerHash, baseAction, artifact, entries }) => {
      const required = await requirement({
        headerHash,
        action: baseAction,
        artifact,
      });
      if (required === null || required.chunkDatums.length === 0) {
        return { kind: "not_required" };
      }
      const routeAction = publicationAction({
        category,
        baseAction,
        requirement: required,
      });
      const confirmed = entries.some(
        (entry) =>
          entry.event.kind === "confirmed" &&
          entry.event.actionId === routeAction.actionId,
      );
      const intent = [...entries]
        .reverse()
        .find(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === routeAction.actionId,
        );
      let publicationAuthorized = false;
      if (
        confirmed &&
        intent !== undefined &&
        intent.event.kind === "submission_intent"
      ) {
        const recovery = parseProofCarriageRecovery({
          value: intent.event.durableRecovery,
          requirement: required,
        });
        publicationAuthorized =
          recovery.route === "publication" &&
          sameJson(recovery.baseAction, baseAction);
      }
      if (!publicationAuthorized) {
        return { kind: "required", action: routeAction };
      }
      const chunks = await resolvePublishedProofChunks({
        lucid,
        address: signer.address,
        proofCbor: required.proofCbor,
      });
      if (chunks === undefined) {
        return {
          kind: "pending",
          reason: `${category} journaled proof publication has no exact output set`,
        };
      }
      const outputsConfirmed = await observeOutputs({
        headerHash,
        outputs: chunks.map((chunk) => ({
          outRef: chunk.outRef,
          datumCbor: chunk.datumCbor,
        })),
      });
      return outputsConfirmed.every(Boolean)
        ? { kind: "satisfied" }
        : {
            kind: "pending",
            reason: `${category} exact proof chunks exist but are not release-final`,
          };
    },
    capture: async ({ headerHash, action, artifact }) => {
      const required = (await exactAction({ headerHash, action, artifact }))
        .requirement;
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await publishProofChunks({
            lucid,
            network,
            signer,
            proofCbor: required.proofCbor,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
        },
      );
      return {
        transaction,
        durableRecovery: recoveryForTransaction({
          transaction,
          requirement: required,
          address: signer.address,
        }),
      };
    },
    reconcile: async ({ headerHash, action, txHash, durableRecovery }) => {
      let required: ProofChunkRequirement;
      try {
        required = requirementFromJournaledAction({
          category,
          action,
          durableRecovery,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (txHash === undefined || !TX_HASH.test(txHash)) {
        return {
          kind: "conflict",
          reason: `${category} proof-chunk intent omitted its exact transaction hash`,
        };
      }
      let recovery: ProofChunkPublicationRecovery;
      try {
        recovery = parseRecovery({
          value: durableRecovery,
          requirement: required,
          txHash,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      const confirmed = await observeOutputs({
        headerHash,
        outputs: recovery.outputs,
      });
      if (confirmed.every(Boolean)) {
        return { kind: "confirmed", txHash };
      }
      const included = await transactionConfirmed({ headerHash, txHash });
      if (confirmed.some(Boolean) || included) {
        return {
          kind: "conflict",
          reason: `${category} proof-chunk transaction did not produce its exact complete output set`,
        };
      }
      return { kind: "not_found" };
    },
  };
  return Object.freeze(port);
};

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

/**
 * Adds a direct-first, journal-visible proof-carriage decision in front of an
 * exact family step. The wrapper first captures and locally evaluates the
 * complete direct transaction. It may publish proof chunks only when that
 * exact attempt fails with the release-bound CML max-transaction-size error.
 */
export const withProofChunkPrerequisite = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  prerequisite,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly prerequisite: ProofChunkPrerequisitePort<Category>;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER ||
    base.category !== category ||
    base.safety.evidenceSource !== FRAUD_PROOF_WORKFLOW_SAFETY.evidenceSource ||
    base.safety.scriptCarriage !== FRAUD_PROOF_WORKFLOW_SAFETY.scriptCarriage ||
    base.safety.localEvaluation !==
      FRAUD_PROOF_WORKFLOW_SAFETY.localEvaluation ||
    prerequisite.portVersion !== PROOF_CHUNK_PREREQUISITE ||
    prerequisite.category !== category
  ) {
    throw new Error(
      `${category} proof-chunk prerequisite ports changed identity`,
    );
  }
  type PreparedRoute =
    | Readonly<{
        kind: "direct";
        baseAction: FraudProofWorkflowAction;
        basePreflight: FraudProofWorkflowPreflight;
        durableRecovery: JournalJsonObject;
      }>
    | Readonly<{
        kind: "publication";
        transaction: LocallyEvaluatedTransaction;
        durableRecovery: JournalJsonObject;
      }>;
  const prepared = new Map<string, PreparedRoute>();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async (input) => await base.prepare(input),
    observe: async (context) => {
      const observed = await base.observe(context);
      if (
        observed.kind !== "action_required" ||
        context.identity.target.kind !== "state_queue_header"
      ) {
        return observed;
      }
      const inspection = await prerequisite.inspect({
        headerHash: context.identity.target.headerHash,
        baseAction: observed.action,
        artifact: context.artifact,
        entries: context.entries,
      });
      if (inspection.kind === "required") {
        return { kind: "action_required", action: inspection.action };
      }
      if (inspection.kind === "pending") {
        return { kind: "conflict", reason: inspection.reason };
      }
      return observed;
    },
    preflight: async (context) => {
      if (!isPublicationAction(context.action)) {
        if (context.identity.target.kind !== "state_queue_header") {
          throw new Error(
            `${category} proof prerequisite changed workflow target`,
          );
        }
        const inspection = await prerequisite.inspect({
          headerHash: context.identity.target.headerHash,
          baseAction: context.action,
          artifact: context.artifact,
          entries: context.entries,
        });
        if (inspection.kind === "required" || inspection.kind === "pending") {
          throw new Error(
            `${category} proof step cannot bypass its direct-first carriage decision`,
          );
        }
        return await withDirectFirstProofCarriageRoute({
          action: context.action,
          route: inspection.kind === "satisfied" ? "publication" : "direct",
          run: async () => await base.preflight(context),
        });
      }
      if (context.identity.target.kind !== "state_queue_header") {
        throw new Error(`${category} proof carriage changed workflow target`);
      }
      const observed = await base.observe(context);
      if (observed.kind !== "action_required") {
        throw new Error(`${category} proof carriage has no current base step`);
      }
      const inspection = await prerequisite.inspect({
        headerHash: context.identity.target.headerHash,
        baseAction: observed.action,
        artifact: context.artifact,
        entries: context.entries,
      });
      if (
        inspection.kind !== "required" ||
        !sameJson(inspection.action, context.action)
      ) {
        throw new Error(
          `${category} proof carriage differs from the current requirement`,
        );
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} proof carriage already has an outstanding captured body`,
        );
      }
      const route = routeActionIdentity(context.action);
      if (context.action.input.category !== category) {
        throw new Error(`${category} proof carriage changed category`);
      }
      if (!sameJson(route.baseAction, observed.action)) {
        throw new Error(`${category} proof carriage changed its base action`);
      }
      let direct: FraudProofWorkflowPreflight;
      try {
        direct = await withDirectFirstProofCarriageRoute({
          action: observed.action,
          route: "direct",
          run: async () =>
            await base.preflight({
              ...context,
              action: observed.action,
            }),
        });
      } catch (cause) {
        const directCapacityFailure =
          prerequisite.classifyDirectCapacityFailure(cause);
        const captured = await prerequisite.capture({
          headerHash: context.identity.target.headerHash,
          action: context.action,
          artifact: context.artifact,
        });
        if (
          !TX_HASH.test(captured.transaction.txHash) ||
          captured.transaction.signed.toHash().toLowerCase() !==
            captured.transaction.txHash
        ) {
          throw new Error(`${category} proof publication body hash is invalid`);
        }
        requireReferenceOnlyScriptWitnesses({
          transaction: captured.transaction,
          label: `${category} proof publication`,
        });
        const durableRecovery = proofCarriageRecovery({
          route: "publication",
          baseAction: observed.action,
          requirement: route.requirement,
          directCapacityFailure,
          publicationDurableRecovery: captured.durableRecovery,
        });
        prepared.set(
          key,
          Object.freeze({
            kind: "publication",
            transaction: captured.transaction,
            durableRecovery,
          }),
        );
        return bindWorkflowPreflightTransaction(
          {
            actionId: context.action.actionId,
            txHash: captured.transaction.txHash,
            scriptExecution: "none",
            localUplcEvaluation: {
              status: "passed",
              evaluator: LOCAL_UPLC_EVALUATOR,
            },
            referenceScripts: [],
            durableRecovery,
          } satisfies FraudProofWorkflowPreflight,
          captured.transaction.signed,
        );
      }
      const durableRecovery = proofCarriageRecovery({
        route: "direct",
        baseAction: observed.action,
        requirement: route.requirement,
        ...(direct.durableRecovery === undefined
          ? {}
          : { baseDurableRecovery: direct.durableRecovery }),
      });
      prepared.set(
        key,
        Object.freeze({
          kind: "direct",
          baseAction: observed.action,
          basePreflight: direct,
          durableRecovery,
        }),
      );
      return copyWorkflowPreflightTransaction({
        from: direct,
        to: {
          ...direct,
          actionId: context.action.actionId,
          durableRecovery,
        } satisfies FraudProofWorkflowPreflight,
      });
    },
    submit: async (context) => {
      if (!isPublicationAction(context.action)) {
        return await base.submit(context);
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        !sameJson(captured.durableRecovery, context.preflight.durableRecovery)
      ) {
        throw new Error(
          `${category} proof carriage has no exact locally evaluated body`,
        );
      }
      try {
        if (captured.kind === "direct") {
          if (captured.basePreflight.txHash !== context.preflight.txHash) {
            throw new Error(
              `${category} direct proof carriage changed transaction body`,
            );
          }
          return await base.submit({
            ...context,
            action: captured.baseAction,
            preflight: captured.basePreflight,
          });
        }
        if (captured.transaction.txHash !== context.preflight.txHash) {
          throw new Error(
            `${category} proof publication changed transaction body`,
          );
        }
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      if (!isPublicationAction(context.action)) {
        return await base.reconcile(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        return {
          kind: "conflict",
          reason: `${category} proof publication changed workflow target`,
        };
      }
      let route: ReturnType<typeof routeActionIdentity>;
      let recovery: ProofCarriageRecovery;
      try {
        route = routeActionIdentity(context.action);
        if (context.action.input.category !== category) {
          throw new Error(`${category} proof carriage changed category`);
        }
        recovery = parseProofCarriageRecovery({
          value: context.durableRecovery,
          requirement: route.requirement,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (!sameJson(route.baseAction, recovery.baseAction)) {
        return {
          kind: "conflict",
          reason: `${category} proof carriage recovery changed its base action`,
        };
      }
      if (recovery.route === "direct") {
        return await base.reconcile({
          identity: context.identity,
          workflowId: context.workflowId,
          artifact: context.artifact,
          entries: context.entries,
          action: recovery.baseAction,
          ...(context.txHash === undefined ? {} : { txHash: context.txHash }),
          ...(recovery.baseDurableRecovery === undefined
            ? {}
            : { durableRecovery: recovery.baseDurableRecovery }),
        });
      }
      return await prerequisite.reconcile({
        headerHash: context.identity.target.headerHash,
        action: context.action,
        artifact: context.artifact,
        ...(context.txHash === undefined ? {} : { txHash: context.txHash }),
        durableRecovery: recovery.publicationDurableRecovery,
      });
    },
  };
  return Object.freeze(adapter);
};
