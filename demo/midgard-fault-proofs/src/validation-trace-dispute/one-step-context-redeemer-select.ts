/** The redeemer select of a CEK context step our trace routes to the redeemer
 * select authenticate, as our trace plans it: a select of the redeemer the
 * execution at the purpose frontier names, which the authenticate binds
 * (RedeemerSelectionPending), the initialize summarises the purpose of
 * (RedeemerPurposePending) and the hash opens the item scan of
 * (RedeemerInitialPending), before the finish writes the context's
 * successor; or a skip of a native execution, which the authenticate
 * verifies alone. */
import {
  hashMidgardRedeemerItemLeaf,
  hashMidgardRedeemerItemProofControl,
  initialMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  cardanoScriptPurposeData,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
  redeemerTagForPurposeKind,
  summarizeMidgardCekLucidData,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

/** A redeemer select's states past the authenticate, each field as Data
 * CBOR. */
export type ValidationTraceContextRedeemerSelect = Readonly<{
  /** `RedeemerSelectionPending`'s fields past its staged context. */
  selectionFieldsCbor: readonly string[];
  /** `Option<DataSummaryV1>` of the selected execution's purpose. */
  purposeCbor: string;
  /** `hash_control_v1` of the item scan's initial control, as bytes. */
  initialHashCbor: string;
}>;

/** `RedeemerSelectAuxiliary` and `RedeemerSkipAuxiliary`'s constructors. */
const SELECT_AUXILIARY = 17;
const SKIP_AUXILIARY = 36;

/** `CekRedeemerContextControlV1`'s purpose bound. */
const PURPOSE_BOUND = 6;

/** `cek_context_control_from_cbor`'s language tag; `NativeFacts`' redeemer
 * count. */
const LANGUAGE_TAG = 1;
const REDEEMER_COUNT = 8;

/** The purpose `script_purpose_summary_v1` summarises, as a script of
 * `languageTag` sees it; undefined for a descriptor-only receive. */
const purposeData = (
  purposeKind: bigint,
  scriptHash: string,
  subject: string,
  languageTag: bigint,
): Data | undefined => {
  const purpose: MidgardScriptPurpose =
    purposeKind === 0n
      ? { kind: "spend", scriptHash, outRefHex: subject }
      : purposeKind === 1n
        ? { kind: "mint", scriptHash, policyId: scriptHash }
        : purposeKind === 2n
          ? { kind: "observe", scriptHash }
          : { kind: "receive", scriptHash };
  if (languageTag === 128n) return midgardScriptPurposeData(purpose) as Data;
  return purposeKind === 3n
    ? undefined
    : (cardanoScriptPurposeData(purpose) as Data);
};

/**
 * The redeemer select whose evidence is `auxiliaryCbor`, on the context
 * control `context` with native facts `nativeFactsCbor`; undefined for the
 * skip of a native execution, whose verified successor is the context's
 * own.
 */
export const planValidationTraceContextRedeemerSelect = ({
  auxiliaryCbor,
  context,
  nativeFactsCbor,
}: {
  readonly auxiliaryCbor: string;
  readonly context: Constr<Data>;
  readonly nativeFactsCbor: string;
}): ValidationTraceContextRedeemerSelect | undefined => {
  const auxiliary = Data.from(auxiliaryCbor);
  if (auxiliary instanceof Constr && auxiliary.index === SKIP_AUXILIARY)
    return undefined;
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== SELECT_AUXILIARY ||
    auxiliary.fields.length !== 12
  )
    throw new Error(
      "validation-trace CEK context redeemer select's evidence is neither a select nor a skip",
    );
  const [
    control,
    index,
    length,
    commitment,
    purposeKind,
    purposeIndex,
    scriptHash,
    subject,
  ] = auxiliary.fields as [
    Constr<Data>,
    bigint,
    bigint,
    string,
    bigint,
    bigint,
    string,
    string,
  ];
  const itemCount = Data.from<bigint>(
    plutusConstrFieldCbor(nativeFactsCbor, [REDEEMER_COUNT]),
  );
  const leaf = hashMidgardRedeemerItemLeaf({
    redeemerIndex: Number(index),
    itemCommitment: Buffer.from(commitment, "hex"),
  }).toString("hex");
  const frontier = (control.fields[PURPOSE_BOUND] as bigint) - 1n;
  const languageTag = context.fields[LANGUAGE_TAG] as bigint;
  const purpose = purposeData(purposeKind, scriptHash, subject, languageTag);
  const initial = initialMidgardRedeemerItemProofControl({
    mode:
      purpose === undefined
        ? MidgardRedeemerItemProofModes.Descriptor
        : MidgardRedeemerItemProofModes.Data,
    itemIndex: Number(index),
    itemCount: Number(itemCount),
    totalLength: Number(length),
    itemCommitment: Buffer.from(commitment, "hex"),
    expectedPurposeTag: redeemerTagForPurposeKind(Number(purposeKind)) ?? -1,
    expectedPointerIndex: Number(purposeIndex),
  });
  const summary =
    purpose === undefined ? undefined : summarizeMidgardCekLucidData(purpose);
  return Object.freeze({
    selectionFieldsCbor: Object.freeze(
      [
        control,
        index,
        itemCount,
        length,
        commitment,
        leaf,
        frontier,
        purposeKind,
        purposeIndex,
        scriptHash,
        subject,
      ].map((field) => Data.to(field)),
    ),
    // Some(summary), or None.
    purposeCbor: Data.to(
      summary === undefined
        ? new Constr(1, [])
        : new Constr(0, [
            new Constr(0, [
              Buffer.from(summary.root).toString("hex"),
              summary.cborLength,
              summary.memory,
            ]),
          ]),
    ),
    initialHashCbor: Data.to(
      hashMidgardRedeemerItemProofControl(initial).toString("hex"),
    ),
  });
};
