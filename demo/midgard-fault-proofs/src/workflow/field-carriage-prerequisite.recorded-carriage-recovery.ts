import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  BOUND_DATA_PUBLICATION_PREREQUISITE,
  exact,
  FIELD_CARRIAGE_PREREQUISITE,
  FIELD_CARRIAGE_RECOVERY,
  OUT_REF,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
  record,
  type Recovery,
  type Requirement,
  sameJson,
  sha256,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import {
  carriageActionInputKeys,
  parseBaseAction,
  PUBLICATION_ENCODINGS,
  type PublicationEncoding,
  publishedContentDigest,
  requirementIdentity,
} from "./field-carriage-prerequisite.requirement-identity.js";
import type { JournalJsonObject } from "./journal.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import { type LocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const recovery = ({
  kind,
  requirement,
  transaction,
  address,
  datumCbor,
  unit,
}: {
  readonly kind: Recovery["kind"];
  readonly requirement: Requirement;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string | null;
}): JournalJsonObject => {
  const outputs = transaction.signed.toTransaction().body().outputs();
  let found: number | undefined;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const decoded = coreToTxOutput(output);
    if (
      found === undefined &&
      decoded.address === address &&
      output.datum_hash() === undefined &&
      output.datum()?.as_datum()?.to_canonical_cbor_hex() ===
        CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex() &&
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
      schemaVersion: FIELD_CARRIAGE_RECOVERY,
      kind,
      requirementSha256: requirement.identitySha256,
      outRef: `${transaction.txHash}#${found.toString()}`,
      datumCbor,
      unit,
      ...("kind" in requirement && requirement.kind === "bound_data_publication"
        ? { publicationAddress: address }
        : {}),
    }),
  });
};

const parseRecovery = ({
  value,
  requirementSha256,
  txHash,
  kind,
  publicationAddress,
}: {
  readonly publicationAddress?: string;
  readonly value: JournalJsonObject | undefined;
  readonly requirementSha256: string;
  readonly txHash: string;
  readonly kind: Recovery["kind"];
}): Recovery => {
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
      ...(publicationAddress === undefined ? [] : ["publicationAddress"]),
    ],
    "field carriage recovery payload",
  );
  if (
    parsed.schemaVersion !== FIELD_CARRIAGE_RECOVERY ||
    parsed.kind !== kind ||
    parsed.publicationAddress !== publicationAddress ||
    parsed.requirementSha256 !== requirementSha256 ||
    typeof parsed.outRef !== "string" ||
    !OUT_REF.test(parsed.outRef) ||
    !parsed.outRef.startsWith(`${txHash}#`) ||
    typeof parsed.datumCbor !== "string" ||
    (parsed.unit !== null && typeof parsed.unit !== "string")
  ) {
    throw new Error("field carriage recovery changed identity");
  }
  return Object.freeze({
    schemaVersion: FIELD_CARRIAGE_RECOVERY,
    kind,
    requirementSha256: requirementSha256,
    outRef: parsed.outRef,
    datumCbor: parsed.datumCbor,
    unit: parsed.unit,
    ...(publicationAddress === undefined ? {} : { publicationAddress }),
  });
};

/** Recover exact published output identity without replaying a removed target. */
export const recordedCarriageRecovery = ({
  category,
  action,
  txHash,
  value,
}: {
  category: FraudProofCatalogueCategoryName;
  action: FraudProofWorkflowAction;
  txHash: string;
  value: JournalJsonObject | undefined;
}): Recovery => {
  const publication = action.input.stage === "publish_field_carriage";
  const input = exact(
    action.input,
    carriageActionInputKeys(action),
    "recorded field carriage action",
  );
  const bound = input.schemaVersion === BOUND_DATA_PUBLICATION_PREREQUISITE;
  const raw = input.schemaVersion === RAW_DATUM_PREIMAGE_PREREQUISITE;
  if (
    (!bound && !raw && input.schemaVersion !== FIELD_CARRIAGE_PREREQUISITE) ||
    input.category !== category ||
    (!publication &&
      (bound || raw || input.stage !== "certify_field_carriage")) ||
    typeof input.requirementSha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(input.requirementSha256)
  )
    throw new Error("recorded field carriage action changed identity");
  const base = parseBaseAction(
    input.forAction,
    "recorded field carriage base action",
  );
  if (
    base.actionId.trim() !== base.actionId ||
    base.actionId.length === 0 ||
    (base.input.category !== undefined && base.input.category !== category)
  )
    throw new Error("recorded field carriage base action changed identity");
  if (bound) {
    if (typeof input.publicationAddress !== "string")
      throw new Error("bound publication address changed identity");
    CML.Address.from_bech32(input.publicationAddress);
    record(input.sourceIdentity, "bound publication source identity");
  }
  const recovered = parseRecovery({
    value,
    requirementSha256: input.requirementSha256,
    txHash,
    kind: publication ? "publication" : "certificate",
    ...(bound
      ? { publicationAddress: input.publicationAddress as string }
      : {}),
  });
  if (bound) {
    const sourceIdentity = input.sourceIdentity as JournalJsonObject;
    const reconstructed = requirementIdentity({
      kind: "bound_data_publication",
      publicationAddress: input.publicationAddress as string,
      datumCbor: recovered.datumCbor,
      sourceIdentity,
    });
    if (
      reconstructed.identitySha256 !== input.requirementSha256 ||
      !sameJson(sourceIdentity.action, base)
    )
      throw new Error(
        "bound publication changed its source or exact output identity",
      );
  }
  if (publication) {
    if (
      typeof input.publicationIndex !== "number" ||
      !Number.isSafeInteger(input.publicationIndex) ||
      input.publicationIndex < 0 ||
      (bound &&
        (input.publicationIndex !== 0 ||
          input.publicationEncoding !== "structured_data")) ||
      action.actionId !==
        `publish-${bound ? "bound-data" : raw ? "raw-datum-preimage" : "field-carriage"}:${base.actionId}:${input.requirementSha256}:${input.publicationIndex}` ||
      recovered.unit !== null ||
      sha256(recovered.datumCbor) !== input.datumCborSha256 ||
      !PUBLICATION_ENCODINGS.includes(
        input.publicationEncoding as PublicationEncoding,
      ) ||
      publishedContentDigest(
        input.publicationEncoding as PublicationEncoding,
        recovered.datumCbor,
      ) !== input.publicationDigest
    )
      throw new Error("recorded field publication changed its exact output");
  } else {
    if (
      action.actionId !==
        `certify-field-carriage:${base.actionId}:${input.requirementSha256}` ||
      typeof recovered.unit !== "string" ||
      !new RegExp(
        `^[0-9a-f]{56}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}$`,
        "u",
      ).test(recovered.unit) ||
      recovered.unit !== input.certificateUnit ||
      sha256(recovered.datumCbor) !== input.certificateDatumCborSha256
    )
      throw new Error("recorded field certificate changed its exact output");
    CML.PlutusData.from_cbor_hex(recovered.datumCbor);
  }
  return recovered;
};
