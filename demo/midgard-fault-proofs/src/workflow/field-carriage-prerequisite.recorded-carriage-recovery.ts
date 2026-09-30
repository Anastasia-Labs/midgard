import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  exact,
  FIELD_CARRIAGE_PREREQUISITE,
  FIELD_CARRIAGE_RECOVERY,
  OUT_REF,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
  type Recovery,
  type Requirement,
  sha256,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import {
  parseBaseAction,
  PUBLICATION_ENCODINGS,
  type PublicationEncoding,
  publishedContentDigest,
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
    }),
  });
};

const parseRecovery = ({
  value,
  requirementSha256,
  txHash,
  kind,
}: {
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
    ],
    "field carriage recovery payload",
  );
  if (
    parsed.schemaVersion !== FIELD_CARRIAGE_RECOVERY ||
    parsed.kind !== kind ||
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
    publication
      ? [
          "schemaVersion",
          "category",
          "stage",
          "forAction",
          "requirementSha256",
          "publicationIndex",
          "publicationEncoding",
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
        ],
    "recorded field carriage action",
  );
  const raw = input.schemaVersion === RAW_DATUM_PREIMAGE_PREREQUISITE;
  if (
    (!raw && input.schemaVersion !== FIELD_CARRIAGE_PREREQUISITE) ||
    input.category !== category ||
    (!publication && (raw || input.stage !== "certify_field_carriage")) ||
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
  const recovered = parseRecovery({
    value,
    requirementSha256: input.requirementSha256,
    txHash,
    kind: publication ? "publication" : "certificate",
  });
  if (publication) {
    if (
      typeof input.publicationIndex !== "number" ||
      !Number.isSafeInteger(input.publicationIndex) ||
      input.publicationIndex < 0 ||
      action.actionId !==
        `publish-${raw ? "raw-datum-preimage" : "field-carriage"}:${base.actionId}:${input.requirementSha256}:${input.publicationIndex}` ||
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
