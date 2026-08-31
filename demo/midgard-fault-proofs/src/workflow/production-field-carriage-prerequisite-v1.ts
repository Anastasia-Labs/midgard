import { createHash } from "node:crypto";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxCompactV1,
  midgardFieldCarriagePlansAreInterchangeableV1,
  type MidgardFieldCarriagePlanV1,
  planMidgardFieldCarriageV1,
} from "@al-ft/midgard-core";
import {
  buildUnsignedFieldPreimagePublicationV1Program,
  deriveFieldPreimageCertificationV1,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1,
  fieldPreimagePublicationDatumCborV1,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  type LucidEvolution,
  type MintingPolicy,
  type Network,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  certifyFaultProofFieldCarriageV1,
  type FaultProofFieldOpeningPlanV1,
  fieldPreimageCertificateAddressV1,
} from "../field-opening-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type {
  FraudProofWorkflowJournalEntryV1,
  JournalJsonObjectV1,
} from "./journal-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowPreflightV1,
  type FraudProofWorkflowReconcileResultV1,
} from "./orchestrator-v1.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER_V1,
  type FraudProofAuthenticatedPublicationObserverV1,
} from "./raw-l1-publication-observation-v1.js";
import {
  bindProductionWorkflowPreflightTransactionV1,
  captureLocallyEvaluatedTransactionV1,
  LOCAL_UPLC_EVALUATOR_V1,
  type LocallyEvaluatedTransactionV1,
  requireReferenceOnlyScriptWitnessesV1,
  submitCapturedTransactionV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1 =
  "midgard-production-field-carriage-prerequisite-v1" as const;
export const PRODUCTION_FIELD_CARRIAGE_RECOVERY_V1 =
  "midgard-production-field-carriage-recovery-v1" as const;

const TX_HASH = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type ProductionRawCommittedFieldCarriagePlanV1 = Readonly<{
  kind: "raw_committed_preimage_v1";
  fieldIndex: number;
  nativeTxId: string;
  preimage: Buffer;
  commitment: string;
  plan: MidgardFieldCarriagePlanV1;
}>;

/** Plans raw committed bytes without pretending they decoded into §5.1 items. */
export const createProductionRawCommittedFieldCarriagePlanV1 = ({
  owner,
  nativeTxId,
  fieldIndex,
  preimage,
}: {
  readonly owner: string;
  readonly nativeTxId: string;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): ProductionRawCommittedFieldCarriagePlanV1 => {
  if (!/^[0-9a-f]{56}$/u.test(owner) || !/^[0-9a-f]{64}$/u.test(nativeTxId)) {
    throw new Error("raw committed field carriage identity is malformed");
  }
  const bytes = Buffer.from(preimage);
  const plan = planMidgardFieldCarriageV1({
    owner: Buffer.from(owner, "hex"),
    txId: Buffer.from(nativeTxId, "hex"),
    fieldIndex,
    preimage: bytes,
  });
  return Object.freeze({
    kind: "raw_committed_preimage_v1",
    fieldIndex: plan.fieldIndex,
    nativeTxId: plan.txId.toString("hex"),
    preimage: bytes,
    commitment: plan.commitment.toString("hex"),
    plan,
  });
};

export type ProductionFieldCarriageRequirementV1 = Readonly<{
  planned:
    | FaultProofFieldOpeningPlanV1
    | ProductionRawCommittedFieldCarriagePlanV1;
  /** Exact compact bytes the tier-3 certificate policy welds. */
  compactCbor: string;
  witnessSetCompactCbor?: string;
  certificate: Readonly<{
    policyId: string;
    mintingScript: MintingPolicy;
    referenceScriptUtxo: UTxO;
  }>;
}>;

type RequirementV1 = ProductionFieldCarriageRequirementV1 &
  Readonly<{
    identitySha256: string;
    publicationDatums: readonly string[];
    publicationDigests: readonly string[];
    certificateDatumCbor: string | null;
    certificateUnit: string | null;
  }>;

type RecoveryV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_FIELD_CARRIAGE_RECOVERY_V1;
  kind: "publication" | "certificate";
  requirementSha256: string;
  outRef: string;
  datumCbor: string;
  unit: string | null;
}>;

export interface ProductionFieldCarriagePrerequisitePortV1<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1;
  readonly category: Category;
  resolveAuthenticated(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<
    Readonly<{
      publications: readonly UTxO[];
      certificate?: UTxO;
      requirement: ProductionFieldCarriageRequirementV1 | null;
    }>
  >;
  inspect(input: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
    readonly entries: readonly FraudProofWorkflowJournalEntryV1[];
  }): Promise<
    | { readonly kind: "not_required" | "satisfied" }
    | { readonly kind: "pending"; readonly reason: string }
    | { readonly kind: "required"; readonly action: FraudProofWorkflowActionV1 }
  >;
  capture(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<{
    readonly transaction: LocallyEvaluatedTransactionV1;
    readonly durableRecovery: JournalJsonObjectV1;
  }>;
  reconcile(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
    readonly txHash?: string;
    readonly durableRecovery?: JournalJsonObjectV1;
  }): Promise<FraudProofWorkflowReconcileResultV1>;
}

const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

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

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const requirementIdentity = (
  requirement: ProductionFieldCarriageRequirementV1,
): RequirementV1 => {
  const { planned } = requirement;
  const normalizedCompactCbor = requirement.compactCbor.toLowerCase();
  let decodedCompact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  try {
    decodedCompact = decodeMidgardNativeTxCompactV1(
      Buffer.from(normalizedCompactCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `field carriage compact CBOR does not decode: ${String(cause)}`,
    );
  }
  const planBytes =
    planned.plan.inlinePreimage ??
    Buffer.concat(planned.plan.publications.map(({ bytes }) => bytes));
  const replayedPlan = planMidgardFieldCarriageV1({
    owner: planned.plan.certificate?.owner ?? Buffer.alloc(28),
    txId: planned.plan.txId,
    fieldIndex: planned.plan.fieldIndex,
    preimage: planBytes,
    publish:
      planned.plan.tier === "RawUtxo" && planned.plan.inlinePreimage === null,
  });
  if (
    !/^(?:[0-9a-f]{2})+$/u.test(requirement.compactCbor) ||
    encodeMidgardNativeTxCompactV1(decodedCompact).toString("hex") !==
      normalizedCompactCbor ||
    computeMidgardNativeTxIdV1(decodedCompact).toString("hex") !==
      planned.nativeTxId ||
    planned.plan.txId.toString("hex") !== planned.nativeTxId ||
    planned.plan.fieldIndex !== planned.fieldIndex ||
    planned.plan.totalLength !== planned.preimage.length ||
    !planBytes.equals(planned.preimage) ||
    !midgardFieldCarriagePlansAreInterchangeableV1(
      replayedPlan,
      planned.plan,
    ) ||
    computeHash32(planned.preimage).toString("hex") !== planned.commitment ||
    planned.plan.commitment.toString("hex") !== planned.commitment ||
    !/^[0-9a-f]{56}$/u.test(requirement.certificate.policyId) ||
    requirement.certificate.referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(
      requirement.certificate.referenceScriptUtxo.scriptRef,
    ) !== validatorToScriptHash(requirement.certificate.mintingScript) ||
    ("nativeTxCompactCbor" in planned &&
      requirement.compactCbor !== planned.nativeTxCompactCbor)
  ) {
    throw new Error(
      "field carriage requires the exact manifest-bound certificate policy reference",
    );
  }
  const publicationDatums = Object.freeze(
    planned.plan.publications.map((publication) =>
      fieldPreimagePublicationDatumCborV1(publication.bytes),
    ),
  );
  const publicationDigests = Object.freeze(
    planned.plan.publications.map((publication) =>
      publication.digest.toString("hex"),
    ),
  );
  const certification =
    planned.plan.tier === "Certified"
      ? deriveFieldPreimageCertificationV1(planned.plan)
      : null;
  const certificateUnit =
    certification === null
      ? null
      : `${requirement.certificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`;
  const identitySha256 = sha256(
    JSON.stringify({
      fieldIndex: planned.fieldIndex,
      nativeTxId: planned.nativeTxId,
      nativeTxCompactCbor: requirement.compactCbor,
      planKind: "kind" in planned ? planned.kind : "decoded_field_opening_v1",
      preimage: planned.preimage.toString("hex"),
      itemCount: "itemCount" in planned ? planned.itemCount : null,
      commitment: planned.commitment,
      tier: planned.plan.tier,
      publicationDatums,
      publicationDigests,
      certificateDatumCbor: certification?.datumCbor ?? null,
      certificatePolicyId: requirement.certificate.policyId,
      certificateReferenceOutRef: outRef(
        requirement.certificate.referenceScriptUtxo,
      ),
      certificateReferenceScriptHash: validatorToScriptHash(
        requirement.certificate.mintingScript,
      ),
      compactCbor: requirement.compactCbor,
      witnessSetCompactCbor: requirement.witnessSetCompactCbor ?? null,
    }),
  );
  return Object.freeze({
    ...requirement,
    identitySha256,
    publicationDatums,
    publicationDigests,
    certificateDatumCbor: certification?.datumCbor ?? null,
    certificateUnit,
  });
};

const frozenBaseAction = (
  action: FraudProofWorkflowActionV1,
): FraudProofWorkflowActionV1 =>
  Object.freeze({
    actionId: action.actionId,
    input: Object.freeze({ ...action.input }),
  });

const publicationAction = <Category extends FraudProofCatalogueCategoryName>({
  category,
  baseAction,
  requirement,
  publicationIndex,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowActionV1;
  readonly requirement: RequirementV1;
  readonly publicationIndex: number;
}): FraudProofWorkflowActionV1 =>
  Object.freeze({
    actionId: `publish-field-carriage:${baseAction.actionId}:${requirement.identitySha256}:${publicationIndex.toString()}`,
    input: Object.freeze({
      schemaVersion: PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1,
      category,
      stage: "publish_field_carriage",
      forAction: frozenBaseAction(baseAction),
      requirementSha256: requirement.identitySha256,
      publicationIndex,
      publicationDigest: requirement.publicationDigests[publicationIndex]!,
      datumCborSha256: sha256(requirement.publicationDatums[publicationIndex]!),
    }),
  });

const certificateAction = <Category extends FraudProofCatalogueCategoryName>({
  category,
  baseAction,
  requirement,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowActionV1;
  readonly requirement: RequirementV1;
}): FraudProofWorkflowActionV1 =>
  Object.freeze({
    actionId: `certify-field-carriage:${baseAction.actionId}:${requirement.identitySha256}`,
    input: Object.freeze({
      schemaVersion: PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1,
      category,
      stage: "certify_field_carriage",
      forAction: frozenBaseAction(baseAction),
      requirementSha256: requirement.identitySha256,
      certificateDatumCborSha256: sha256(requirement.certificateDatumCbor!),
      certificateUnit: requirement.certificateUnit!,
    }),
  });

const isPrerequisiteAction = (action: FraudProofWorkflowActionV1): boolean =>
  action.input.schemaVersion === PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1 &&
  (action.input.stage === "publish_field_carriage" ||
    action.input.stage === "certify_field_carriage");

const parseBaseAction = (
  value: unknown,
  label: string,
): FraudProofWorkflowActionV1 => {
  const parsed = exact(value, ["actionId", "input"], label);
  if (typeof parsed.actionId !== "string") {
    throw new Error(`${label} actionId is malformed`);
  }
  return {
    actionId: parsed.actionId,
    input: record(parsed.input, `${label} input`) as JournalJsonObjectV1,
  };
};

const recovery = ({
  kind,
  requirement,
  transaction,
  address,
  datumCbor,
  unit,
}: {
  readonly kind: RecoveryV1["kind"];
  readonly requirement: RequirementV1;
  readonly transaction: LocallyEvaluatedTransactionV1;
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string | null;
}): JournalJsonObjectV1 => {
  const outputs = transaction.signed.toTransaction().body().outputs();
  let found: number | undefined;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const decoded = coreToTxOutput(output);
    if (
      found === undefined &&
      decoded.address === address &&
      output.datum_hash() === undefined &&
      output.datum()?.as_datum()?.to_canonical_cbor_hex() === datumCbor &&
      output.script_ref() === undefined &&
      (unit === null
        ? Object.entries(decoded.assets).every(
            ([asset, quantity]) => asset === "lovelace" || quantity === 0n,
          )
        : decoded.assets[unit] === 1n &&
          Object.entries(decoded.assets).every(
            ([asset, quantity]) =>
              asset === "lovelace" || asset === unit || quantity === 0n,
          ))
    ) {
      found = index;
    }
  }
  if (found === undefined) {
    throw new Error(
      `field carriage ${kind} body omitted its exact authenticated output`,
    );
  }
  return Object.freeze({
    fieldCarriage: Object.freeze({
      schemaVersion: PRODUCTION_FIELD_CARRIAGE_RECOVERY_V1,
      kind,
      requirementSha256: requirement.identitySha256,
      outRef: `${transaction.txHash}#${found.toString()}`,
      datumCbor,
      unit,
    }),
  });
};

const parseRecovery = ({
  value,
  requirement,
  txHash,
  kind,
}: {
  readonly value: JournalJsonObjectV1 | undefined;
  readonly requirement: RequirementV1;
  readonly txHash: string;
  readonly kind: RecoveryV1["kind"];
}): RecoveryV1 => {
  const outer = exact(value, ["fieldCarriage"], "field carriage recovery");
  const parsed = exact(
    outer.fieldCarriage,
    [
      "schemaVersion",
      "kind",
      "requirementSha256",
      "outRef",
      "datumCbor",
      "unit",
    ],
    "field carriage recovery payload",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_FIELD_CARRIAGE_RECOVERY_V1 ||
    parsed.kind !== kind ||
    parsed.requirementSha256 !== requirement.identitySha256 ||
    typeof parsed.outRef !== "string" ||
    !OUT_REF.test(parsed.outRef) ||
    !parsed.outRef.startsWith(`${txHash}#`) ||
    typeof parsed.datumCbor !== "string" ||
    (parsed.unit !== null && typeof parsed.unit !== "string")
  ) {
    throw new Error("field carriage recovery changed identity");
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_FIELD_CARRIAGE_RECOVERY_V1,
    kind,
    requirementSha256: requirement.identitySha256,
    outRef: parsed.outRef,
    datumCbor: parsed.datumCbor,
    unit: parsed.unit,
  });
};

/**
 * Adds one durable action per field chunk and one for the tier-3 certificate.
 * Candidate discovery may use Lucid, but only raw-L1 admission can satisfy an
 * action or reconcile an ambiguous submission.
 */
export const createAuthenticatedFieldCarriagePrerequisitePortV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  network,
  signer,
  publications,
  requirementForAction,
  transactionConfirmed,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserverV1;
  readonly requirementForAction: (input: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }) =>
    | ProductionFieldCarriageRequirementV1
    | null
    | Promise<ProductionFieldCarriageRequirementV1 | null>;
  readonly transactionConfirmed: (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => Promise<boolean>;
}): ProductionFieldCarriagePrerequisitePortV1<Category> => {
  if (
    publications.observerVersion !==
    FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER_V1
  ) {
    throw new Error(`${category} field carriage requires a raw-L1 observer`);
  }
  const requirement = async (input: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<RequirementV1 | null> => {
    const resolved = await requirementForAction(input);
    return resolved === null ? null : requirementIdentity(resolved);
  };
  const candidate = async ({
    headerHash,
    kind,
    address,
    datumCbor,
    unit,
  }: {
    readonly headerHash: string;
    readonly kind: "field_publication" | "field_certificate";
    readonly address: string;
    readonly datumCbor: string;
    readonly unit: string | null;
  }): Promise<{
    readonly kind: "absent" | "pending" | "confirmed";
    readonly utxo?: UTxO;
  }> => {
    const matches = (await lucid.utxosAt(address))
      .filter(
        (utxo) =>
          utxo.datum === datumCbor &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          (unit === null
            ? Object.entries(utxo.assets).every(
                ([asset, quantity]) => asset === "lovelace" || quantity === 0n,
              )
            : utxo.assets[unit] === 1n &&
              Object.entries(utxo.assets).every(
                ([asset, quantity]) =>
                  asset === "lovelace" || asset === unit || quantity === 0n,
              )),
      )
      .sort((left, right) => outRef(left).localeCompare(outRef(right)));
    if (matches.length === 0) return { kind: "absent" };
    for (const utxo of matches) {
      const observed = await publications.observeExact({
        headerHash,
        kind,
        address,
        expectedOutRef: outRef(utxo),
        expectedDatumCbor: datumCbor,
        ...(unit === null ? {} : { expectedUnit: unit }),
      });
      if (observed.kind === "confirmed") {
        return { kind: "confirmed", utxo };
      }
    }
    return { kind: "pending" };
  };
  const inspect = async ({
    headerHash,
    baseAction,
    artifact,
  }: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }) => {
    const required = await requirement({ action: baseAction, artifact });
    if (required === null || required.planned.plan.tier === "Inline") {
      return { kind: "not_required" as const };
    }
    for (const [index, datumCbor] of required.publicationDatums.entries()) {
      const observed = await candidate({
        headerHash,
        kind: "field_publication",
        address: signer.address,
        datumCbor,
        unit: null,
      });
      if (observed.kind === "absent") {
        return {
          kind: "required" as const,
          action: publicationAction({
            category,
            baseAction,
            requirement: required,
            publicationIndex: index,
          }),
        };
      }
      if (observed.kind === "pending") {
        return {
          kind: "pending" as const,
          reason: `${category} field publication ${index.toString()} is not release-final`,
        };
      }
    }
    if (required.planned.plan.tier !== "Certified") {
      return { kind: "satisfied" as const };
    }
    const certificate = await candidate({
      headerHash,
      kind: "field_certificate",
      address: fieldPreimageCertificateAddressV1({
        network,
        certificatePolicyId: required.certificate.policyId,
      }),
      datumCbor: required.certificateDatumCbor!,
      unit: required.certificateUnit!,
    });
    if (certificate.kind === "absent") {
      return {
        kind: "required" as const,
        action: certificateAction({
          category,
          baseAction,
          requirement: required,
        }),
      };
    }
    return certificate.kind === "confirmed"
      ? { kind: "satisfied" as const }
      : {
          kind: "pending" as const,
          reason: `${category} field certificate is not release-final`,
        };
  };
  const exactAction = async ({
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<
    Readonly<{
      kind: RecoveryV1["kind"];
      baseAction: FraudProofWorkflowActionV1;
      requirement: RequirementV1;
      publicationIndex?: number;
    }>
  > => {
    const stage = action.input.stage;
    const keys =
      stage === "publish_field_carriage"
        ? [
            "schemaVersion",
            "category",
            "stage",
            "forAction",
            "requirementSha256",
            "publicationIndex",
            "publicationDigest",
            "datumCborSha256",
          ]
        : [
            "schemaVersion",
            "category",
            "stage",
            "forAction",
            "requirementSha256",
            "certificateDatumCborSha256",
            "certificateUnit",
          ];
    const input = exact(action.input, keys, `${category} field action`);
    if (
      input.schemaVersion !== PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1 ||
      input.category !== category ||
      (input.stage !== "publish_field_carriage" &&
        input.stage !== "certify_field_carriage")
    ) {
      throw new Error(`${category} field action changed identity`);
    }
    const baseAction = parseBaseAction(
      input.forAction,
      `${category} field base action`,
    );
    const required = await requirement({ action: baseAction, artifact });
    if (required === null) {
      throw new Error(`${category} field action is no longer required`);
    }
    if (input.stage === "publish_field_carriage") {
      if (
        typeof input.publicationIndex !== "number" ||
        !Number.isSafeInteger(input.publicationIndex)
      ) {
        throw new Error(`${category} field publication index is malformed`);
      }
      const publicationIndex = input.publicationIndex;
      if (
        publicationIndex < 0 ||
        publicationIndex >= required.publicationDatums.length ||
        !sameJson(
          action,
          publicationAction({
            category,
            baseAction,
            requirement: required,
            publicationIndex,
          }),
        )
      ) {
        throw new Error(`${category} field publication changed identity`);
      }
      return {
        kind: "publication",
        baseAction,
        requirement: required,
        publicationIndex,
      };
    }
    if (
      required.planned.plan.tier !== "Certified" ||
      !sameJson(
        action,
        certificateAction({ category, baseAction, requirement: required }),
      )
    ) {
      throw new Error(`${category} field certificate changed identity`);
    }
    return { kind: "certificate", baseAction, requirement: required };
  };
  const port: ProductionFieldCarriagePrerequisitePortV1<Category> = {
    portVersion: PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1,
    category,
    resolveAuthenticated: async ({ headerHash, action, artifact }) => {
      const required = await requirement({ action, artifact });
      if (required === null || required.planned.plan.tier === "Inline") {
        return Object.freeze({
          publications: Object.freeze([]),
          requirement: required,
        });
      }
      const resolved: UTxO[] = [];
      for (const datumCbor of required.publicationDatums) {
        const observed = await candidate({
          headerHash,
          kind: "field_publication",
          address: signer.address,
          datumCbor,
          unit: null,
        });
        if (observed.kind !== "confirmed" || observed.utxo === undefined) {
          throw new Error(
            `${category} proof step cannot use an unauthenticated field publication`,
          );
        }
        resolved.push(observed.utxo);
      }
      if (required.planned.plan.tier !== "Certified") {
        return Object.freeze({
          publications: Object.freeze(resolved),
          requirement: required,
        });
      }
      const observed = await candidate({
        headerHash,
        kind: "field_certificate",
        address: fieldPreimageCertificateAddressV1({
          network,
          certificatePolicyId: required.certificate.policyId,
        }),
        datumCbor: required.certificateDatumCbor!,
        unit: required.certificateUnit!,
      });
      if (observed.kind !== "confirmed" || observed.utxo === undefined) {
        throw new Error(
          `${category} proof step cannot use an unauthenticated field certificate`,
        );
      }
      return Object.freeze({
        publications: Object.freeze(resolved),
        certificate: observed.utxo,
        requirement: required,
      });
    },
    inspect: async (input) => await inspect(input),
    capture: async ({ headerHash, action, artifact }) => {
      const parsed = await exactAction({ action, artifact });
      signer.selectWallet(lucid);
      if (parsed.kind === "publication") {
        const index = parsed.publicationIndex!;
        const publication =
          parsed.requirement.planned.plan.publications[index]!;
        const datumCbor = parsed.requirement.publicationDatums[index]!;
        const unsigned = await Effect.runPromise(
          buildUnsignedFieldPreimagePublicationV1Program(lucid, {
            publication: {
              chunkIndex: publication.chunkIndex,
              datumCbor,
              byteLength: publication.bytes.length,
              digestHex: publication.digest.toString("hex"),
            },
            publisherAddress: signer.address,
          }),
        );
        const signed = await unsigned.sign.withWallet().complete();
        const transaction: LocallyEvaluatedTransactionV1 = Object.freeze({
          txHash: signed.toHash().toLowerCase(),
          signed,
          referenceScripts: Object.freeze([]),
        });
        return {
          transaction,
          durableRecovery: recovery({
            kind: "publication",
            requirement: parsed.requirement,
            transaction,
            address: signer.address,
            datumCbor,
            unit: null,
          }),
        };
      }
      const chunkUtxos: UTxO[] = [];
      for (const datumCbor of parsed.requirement.publicationDatums) {
        const observed = await candidate({
          headerHash,
          kind: "field_publication",
          address: signer.address,
          datumCbor,
          unit: null,
        });
        if (observed.kind !== "confirmed" || observed.utxo === undefined) {
          throw new Error(
            `${category} field certificate cannot bypass authenticated chunk publication`,
          );
        }
        chunkUtxos.push(observed.utxo);
      }
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await certifyFaultProofFieldCarriageV1({
            lucid,
            network,
            signer,
            planned: parsed.requirement.planned,
            certificatePolicyId: parsed.requirement.certificate.policyId,
            certificateMintingScript:
              parsed.requirement.certificate.mintingScript,
            certificateReferenceScriptUtxo:
              parsed.requirement.certificate.referenceScriptUtxo,
            chunkUtxos,
            compactCbor: parsed.requirement.compactCbor,
            ...(parsed.requirement.witnessSetCompactCbor === undefined
              ? {}
              : {
                  witnessSetCompactCbor:
                    parsed.requirement.witnessSetCompactCbor,
                }),
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      const address = fieldPreimageCertificateAddressV1({
        network,
        certificatePolicyId: parsed.requirement.certificate.policyId,
      });
      return {
        transaction,
        durableRecovery: recovery({
          kind: "certificate",
          requirement: parsed.requirement,
          transaction,
          address,
          datumCbor: parsed.requirement.certificateDatumCbor!,
          unit: parsed.requirement.certificateUnit!,
        }),
      };
    },
    reconcile: async ({
      headerHash,
      action,
      artifact,
      txHash,
      durableRecovery,
    }) => {
      const parsed = await exactAction({ action, artifact });
      if (txHash === undefined || !TX_HASH.test(txHash)) {
        return {
          kind: "conflict",
          reason: `${category} field prerequisite omitted its exact transaction hash`,
        };
      }
      let recovered: RecoveryV1;
      try {
        recovered = parseRecovery({
          value: durableRecovery,
          requirement: parsed.requirement,
          txHash,
          kind: parsed.kind,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      const expectedDatum =
        parsed.kind === "publication"
          ? parsed.requirement.publicationDatums[parsed.publicationIndex!]!
          : parsed.requirement.certificateDatumCbor!;
      const expectedUnit =
        parsed.kind === "publication"
          ? null
          : parsed.requirement.certificateUnit!;
      if (
        recovered.datumCbor !== expectedDatum ||
        recovered.unit !== expectedUnit
      ) {
        return {
          kind: "conflict",
          reason: `${category} field prerequisite recovery changed its exact output`,
        };
      }
      const address =
        parsed.kind === "publication"
          ? signer.address
          : fieldPreimageCertificateAddressV1({
              network,
              certificatePolicyId: parsed.requirement.certificate.policyId,
            });
      const observation = await publications.observeExact({
        headerHash,
        kind:
          parsed.kind === "publication"
            ? "field_publication"
            : "field_certificate",
        address,
        expectedOutRef: recovered.outRef,
        expectedDatumCbor: recovered.datumCbor,
        ...(recovered.unit === null ? {} : { expectedUnit: recovered.unit }),
      });
      if (observation.kind === "confirmed") {
        return { kind: "confirmed", txHash };
      }
      return (await transactionConfirmed({ headerHash, txHash }))
        ? {
            kind: "conflict",
            reason: `${category} field prerequisite transaction omitted its journaled output`,
          }
        : { kind: "not_found" };
    },
  };
  return Object.freeze(port);
};

const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;

/** Adds durable field publication/certification in front of a family adapter. */
export const withProductionFieldCarriagePrerequisiteV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  base,
  prerequisite,
}: {
  readonly category: Category;
  readonly base: FraudProofFamilyWorkflowAdapterV1;
  readonly prerequisite: ProductionFieldCarriagePrerequisitePortV1<Category>;
}): FraudProofFamilyWorkflowAdapterV1 => {
  if (
    base.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER_V1 ||
    base.category !== category ||
    !sameJson(base.safety, FRAUD_PROOF_WORKFLOW_SAFETY_V1) ||
    prerequisite.portVersion !== PRODUCTION_FIELD_CARRIAGE_PREREQUISITE_V1 ||
    prerequisite.category !== category
  ) {
    throw new Error(`${category} field prerequisite ports changed identity`);
  }
  const prepared = new Map<
    string,
    Readonly<{
      transaction: LocallyEvaluatedTransactionV1;
      durableRecovery: JournalJsonObjectV1;
    }>
  >();
  const adapter: FraudProofFamilyWorkflowAdapterV1 = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
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
      return inspection.kind === "pending"
        ? { kind: "conflict", reason: inspection.reason }
        : observed;
    },
    preflight: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        if (context.identity.target.kind !== "state_queue_header") {
          throw new Error(`${category} field prerequisite changed target`);
        }
        const inspection = await prerequisite.inspect({
          headerHash: context.identity.target.headerHash,
          baseAction: context.action,
          artifact: context.artifact,
          entries: context.entries,
        });
        if (inspection.kind === "required" || inspection.kind === "pending") {
          throw new Error(
            `${category} proof step cannot bypass authenticated field carriage`,
          );
        }
        return await base.preflight(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        throw new Error(`${category} field prerequisite changed target`);
      }
      const observed = await base.observe(context);
      if (observed.kind !== "action_required") {
        throw new Error(`${category} field prerequisite has no base action`);
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
          `${category} field prerequisite differs from current requirement`,
        );
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      if (prepared.has(key)) {
        throw new Error(
          `${category} field prerequisite already captured this action`,
        );
      }
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
        throw new Error(`${category} field prerequisite body hash is invalid`);
      }
      requireReferenceOnlyScriptWitnessesV1({
        transaction: captured.transaction,
        label: `${category} field prerequisite`,
      });
      prepared.set(key, captured);
      return bindProductionWorkflowPreflightTransactionV1(
        {
          actionId: context.action.actionId,
          txHash: captured.transaction.txHash,
          scriptExecution:
            captured.transaction.referenceScripts.length === 0
              ? "none"
              : "reference_scripts",
          localUplcEvaluation: {
            status: "passed",
            evaluator: LOCAL_UPLC_EVALUATOR_V1,
          },
          referenceScripts: captured.transaction.referenceScripts,
          durableRecovery: captured.durableRecovery,
        } satisfies FraudProofWorkflowPreflightV1,
        captured.transaction.signed,
      );
    },
    submit: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        return await base.submit(context);
      }
      const key = cacheKey(context.workflowId, context.action.actionId);
      const captured = prepared.get(key);
      if (
        captured === undefined ||
        captured.transaction.txHash !== context.preflight.txHash ||
        !sameJson(captured.durableRecovery, context.preflight.durableRecovery)
      ) {
        throw new Error(
          `${category} field prerequisite has no exact captured body`,
        );
      }
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransactionV1(captured.transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async (context) => {
      if (!isPrerequisiteAction(context.action)) {
        return await base.reconcile(context);
      }
      if (context.identity.target.kind !== "state_queue_header") {
        return {
          kind: "conflict",
          reason: `${category} field prerequisite changed target`,
        };
      }
      return await prerequisite.reconcile({
        headerHash: context.identity.target.headerHash,
        action: context.action,
        artifact: context.artifact,
        ...(context.txHash === undefined ? {} : { txHash: context.txHash }),
        ...(context.durableRecovery === undefined
          ? {}
          : { durableRecovery: context.durableRecovery }),
      });
    },
  };
  return Object.freeze(adapter);
};
