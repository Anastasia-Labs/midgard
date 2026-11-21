/**
 * The actions that record how Cardano decodes a Plutus item's script before
 * a proof step reads the record: stage the script's bytes, Bind a run, Walk
 * it, and Finish it into the record. Each action is keyed by the chain state
 * it consumes, so a chain state requires one action.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { CML } from "@lucid-evolution/lucid";

import {
  exact,
  OUT_REF,
  sha256,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { parseBaseAction } from "./field-carriage-prerequisite.requirement-identity.js";
import type { JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

export const SCRIPT_DECODE_PREREQUISITE =
  "midgard-script-decode-record-prerequisite-v1" as const;

/** Each stage is also the funding action kind of its transactions. */
export const SCRIPT_DECODE_STAGES = [
  "script_decode_stage",
  "script_decode_bind",
  "script_decode_walk",
  "script_decode_finish",
] as const;

export type ScriptDecodeStage = (typeof SCRIPT_DECODE_STAGES)[number];

const HASH_28 = /^[0-9a-f]{56}$/u;

type StageFields = {
  readonly script_decode_stage: {
    readonly chunkIndex: number;
    readonly datumCborSha256: string;
  };
  /** Bind starts a run when no honest one is open on chain. */
  readonly script_decode_bind: Readonly<Record<never, never>>;
  readonly script_decode_walk: { readonly runOutRef: string };
  readonly script_decode_finish: { readonly runOutRef: string };
};

const FIELD_KEYS: { readonly [S in ScriptDecodeStage]: readonly string[] } = {
  script_decode_stage: ["chunkIndex", "datumCborSha256"],
  script_decode_bind: [],
  script_decode_walk: ["runOutRef"],
  script_decode_finish: ["runOutRef"],
};

const actionIdSuffix = (fields: StageFields[ScriptDecodeStage]): string => {
  if ("chunkIndex" in fields) return fields.chunkIndex.toString();
  return "runOutRef" in fields ? fields.runOutRef : "first";
};

export const scriptDecodeActionId = <S extends ScriptDecodeStage>(
  stage: S,
  baseAction: FraudProofWorkflowAction,
  scriptHashHex: string,
  fields: StageFields[S],
): string =>
  `${stage.replaceAll("_", "-")}:${baseAction.actionId}:${scriptHashHex}:${actionIdSuffix(fields)}`;

/** The action of `stage` that records `scriptHashHex` before `baseAction`. */
export const scriptDecodeAction = <S extends ScriptDecodeStage>({
  category,
  baseAction,
  scriptHashHex,
  stage,
  fields,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly baseAction: FraudProofWorkflowAction;
  readonly scriptHashHex: string;
  readonly stage: S;
  readonly fields: StageFields[S];
}): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: scriptDecodeActionId(stage, baseAction, scriptHashHex, fields),
    input: Object.freeze({
      schemaVersion: SCRIPT_DECODE_PREREQUISITE,
      category,
      stage,
      forAction: Object.freeze({
        actionId: baseAction.actionId,
        input: Object.freeze({ ...baseAction.input }),
      }),
      scriptHashHex,
      ...fields,
    }) as JournalJsonObject,
  });

export const isScriptDecodePrerequisiteAction = (
  action: FraudProofWorkflowAction,
): boolean =>
  action.input.schemaVersion === SCRIPT_DECODE_PREREQUISITE &&
  SCRIPT_DECODE_STAGES.includes(action.input.stage as ScriptDecodeStage);

export type ParsedScriptDecodeAction = Readonly<{
  stage: ScriptDecodeStage;
  baseAction: FraudProofWorkflowAction;
  scriptHashHex: string;
  chunkIndex?: number;
  datumCborSha256?: string;
  runOutRef?: string;
}>;

/** The exact fields of a script-decode action of `category`, or throws. */
export const parseScriptDecodeAction = (
  action: FraudProofWorkflowAction,
  category: FraudProofCatalogueCategoryName,
): ParsedScriptDecodeAction => {
  const stage = action.input.stage as ScriptDecodeStage;
  if (!SCRIPT_DECODE_STAGES.includes(stage)) {
    throw new Error(`${category} script decode action has no known stage`);
  }
  const input = exact(
    action.input,
    [
      "schemaVersion",
      "category",
      "stage",
      "forAction",
      "scriptHashHex",
      ...FIELD_KEYS[stage],
    ],
    `${category} script decode action`,
  );
  const baseAction = parseBaseAction(
    input.forAction,
    `${category} script decode base action`,
  );
  const valid =
    input.schemaVersion === SCRIPT_DECODE_PREREQUISITE &&
    input.category === category &&
    typeof input.scriptHashHex === "string" &&
    HASH_28.test(input.scriptHashHex) &&
    (stage !== "script_decode_stage" ||
      (typeof input.chunkIndex === "number" &&
        Number.isSafeInteger(input.chunkIndex) &&
        input.chunkIndex >= 0 &&
        typeof input.datumCborSha256 === "string" &&
        /^[0-9a-f]{64}$/u.test(input.datumCborSha256))) &&
    ((stage !== "script_decode_walk" && stage !== "script_decode_finish") ||
      (typeof input.runOutRef === "string" && OUT_REF.test(input.runOutRef)));
  const fields = Object.fromEntries(
    FIELD_KEYS[stage].map((key) => [key, input[key]]),
  ) as StageFields[typeof stage];
  if (
    !valid ||
    action.actionId !==
      scriptDecodeActionId(
        stage,
        baseAction,
        input.scriptHashHex as string,
        fields,
      )
  ) {
    throw new Error(`${category} script decode action changed identity`);
  }
  return Object.freeze({
    stage,
    baseAction,
    scriptHashHex: input.scriptHashHex as string,
    ...fields,
  });
};

/** The content digest of an inline datum, over its canonical encoding. */
export const scriptDecodeDatumDigest = (datumCbor: string): string =>
  sha256(CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex());
