/**
 * The journaled actions that record how Cardano decodes a Plutus item's
 * script before a proof step reads the record: stage the script's bytes,
 * Bind a run, Walk it, and Finish it into the record. Each action is keyed
 * by the chain state it consumes, so a chain state requires one action, and
 * its durable recovery names the exact output it leaves.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { CML, type TxOutput } from "@lucid-evolution/lucid";

import {
  exact,
  OUT_REF,
  sha256,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { parseBaseAction } from "./field-carriage-prerequisite.requirement-identity.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
} from "./journal.js";
import {
  attemptCount,
  latestSubmissionIntent,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import { claimOwnOutputs, type OwnOutputClaim } from "./own-outputs.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const SCRIPT_DECODE_PREREQUISITE =
  "midgard-script-decode-record-prerequisite-v1" as const;

export const SCRIPT_DECODE_RECOVERY =
  "midgard-script-decode-record-recovery-v1" as const;

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
    /** The confirmed chunk this one replaces, after that one vanished. */
    readonly replacesOutRef: string | null;
  };
  readonly script_decode_bind: {
    /** Our newest run, after it vanished; null for the first Bind. */
    readonly previousRunOutRef: string | null;
  };
  readonly script_decode_walk: { readonly runOutRef: string };
  readonly script_decode_finish: { readonly runOutRef: string };
};

const FIELD_KEYS: { readonly [S in ScriptDecodeStage]: readonly string[] } = {
  script_decode_stage: ["chunkIndex", "datumCborSha256", "replacesOutRef"],
  script_decode_bind: ["previousRunOutRef"],
  script_decode_walk: ["runOutRef"],
  script_decode_finish: ["runOutRef"],
};

const actionIdSuffix = (fields: StageFields[ScriptDecodeStage]): string => {
  if ("chunkIndex" in fields) {
    return `${fields.chunkIndex.toString()}${
      fields.replacesOutRef === null ? "" : `:${fields.replacesOutRef}`
    }`;
  }
  if ("previousRunOutRef" in fields) {
    return fields.previousRunOutRef ?? "first";
  }
  return fields.runOutRef;
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
  replacesOutRef?: string | null;
  previousRunOutRef?: string | null;
  runOutRef?: string;
}>;

const optionalOutRef = (value: unknown): boolean =>
  value === null || (typeof value === "string" && OUT_REF.test(value));

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
        /^[0-9a-f]{64}$/u.test(input.datumCborSha256) &&
        optionalOutRef(input.replacesOutRef))) &&
    (stage !== "script_decode_bind" ||
      optionalOutRef(input.previousRunOutRef)) &&
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

export type ScriptDecodeRecovery = Readonly<{
  schemaVersion: typeof SCRIPT_DECODE_RECOVERY;
  stage: ScriptDecodeStage;
  outRef: string;
  address: string;
  datumCborSha256: string;
  unit: string | null;
}>;

/** The content digest of an inline datum, over its canonical encoding. */
export const scriptDecodeDatumDigest = (datumCbor: string): string =>
  sha256(CML.PlutusData.from_cbor_hex(datumCbor).to_canonical_cbor_hex());

/**
 * The durable recovery of a captured script-decode transaction: the output
 * of our own signed body at `address` that `matches` (inline datum, no
 * reference script), which the next action continues from, by its out-ref.
 */
export const scriptDecodeRecovery = ({
  stage,
  transaction,
  address,
  matches,
  unit,
}: {
  readonly stage: ScriptDecodeStage;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly address: string;
  readonly matches: (output: TxOutput) => boolean;
  readonly unit: string | null;
}): JournalJsonObject => {
  let claim: OwnOutputClaim;
  try {
    [claim] = claimOwnOutputs({
      signed: transaction.signed,
      slots: [
        {
          address,
          matches: (output) =>
            typeof output.datum === "string" &&
            output.scriptRef == null &&
            matches(output),
        },
      ],
    }) as [OwnOutputClaim];
  } catch (cause) {
    throw new Error(`script decode ${stage} body has no exact output`, {
      cause,
    });
  }
  if (!claim.outRef.startsWith(`${transaction.txHash}#`)) {
    throw new Error(`script decode ${stage} body is not the captured one`);
  }
  return Object.freeze({
    scriptDecode: Object.freeze({
      schemaVersion: SCRIPT_DECODE_RECOVERY,
      stage,
      outRef: claim.outRef,
      address,
      datumCborSha256: scriptDecodeDatumDigest(claim.output.datum!),
      unit,
    }),
  });
};

/** The recovery a script-decode intent journaled for `txHash`, or throws. */
export const parseScriptDecodeRecovery = ({
  value,
  stage,
  txHash,
}: {
  readonly value: JournalJsonObject | undefined;
  readonly stage: ScriptDecodeStage;
  readonly txHash: string;
}): ScriptDecodeRecovery => {
  const outer = exact(value, ["scriptDecode"], "script decode recovery");
  const parsed = exact(
    outer.scriptDecode,
    ["schemaVersion", "stage", "outRef", "address", "datumCborSha256", "unit"],
    "script decode recovery payload",
  );
  if (
    !TX_HASH.test(txHash) ||
    parsed.schemaVersion !== SCRIPT_DECODE_RECOVERY ||
    parsed.stage !== stage ||
    typeof parsed.outRef !== "string" ||
    !OUT_REF.test(parsed.outRef) ||
    !parsed.outRef.startsWith(`${txHash}#`) ||
    typeof parsed.address !== "string" ||
    typeof parsed.datumCborSha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(parsed.datumCborSha256) ||
    (parsed.unit !== null &&
      (typeof parsed.unit !== "string" ||
        !/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(parsed.unit)))
  ) {
    throw new Error("script decode recovery changed identity");
  }
  return Object.freeze({
    schemaVersion: SCRIPT_DECODE_RECOVERY,
    stage,
    outRef: parsed.outRef,
    address: parsed.address,
    datumCborSha256: parsed.datumCborSha256,
    unit: parsed.unit,
  });
};

/** One confirmed script-decode transaction of the journal, and its output. */
export type ConfirmedScriptDecodeOutput = Readonly<{
  action: ParsedScriptDecodeAction;
  recovery: ScriptDecodeRecovery;
  /** When the confirmation was journaled (ISO time). */
  confirmedAt: string;
}>;

/**
 * The journal's confirmed script-decode transactions of `scriptHashHex` in
 * `stages`, oldest first. A confirmation a later reobservation undid (a
 * rollback) is left out until it is confirmed again.
 */
export const confirmedScriptDecodeOutputs = ({
  entries,
  category,
  scriptHashHex,
  stages,
}: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly category: FraudProofCatalogueCategoryName;
  readonly scriptHashHex: string;
  readonly stages: readonly ScriptDecodeStage[];
}): readonly ConfirmedScriptDecodeOutput[] => {
  const latest = new Map<string, FraudProofWorkflowJournalEntry>();
  for (const entry of entries) {
    const { event } = entry;
    if (event.kind === "confirmed" || event.kind === "reobserved") {
      latest.delete(event.actionId);
      latest.set(event.actionId, entry);
    }
  }
  const confirmed: ConfirmedScriptDecodeOutput[] = [];
  for (const [actionId, entry] of latest) {
    if (entry.event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, actionId);
    if (
      intent === undefined ||
      intent.txHash !== entry.event.txHash ||
      intent.actionInput.schemaVersion !== SCRIPT_DECODE_PREREQUISITE ||
      intent.actionInput.scriptHashHex !== scriptHashHex ||
      !stages.includes(intent.actionInput.stage as ScriptDecodeStage)
    ) {
      continue;
    }
    const action = parseScriptDecodeAction(
      { actionId, input: intent.actionInput },
      category,
    );
    confirmed.push(
      Object.freeze({
        action,
        recovery: parseScriptDecodeRecovery({
          value: intent.durableRecovery,
          stage: action.stage,
          txHash: intent.txHash,
        }),
        confirmedAt: entry.recordedAt,
      }),
    );
  }
  return Object.freeze(confirmed);
};

/**
 * True when `actionId` was submitted `limit` times and its latest attempt
 * was found impossible: its refits are exhausted.
 */
export const scriptDecodeAttemptsExhausted = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
  limit: number,
): boolean => {
  if (attemptCount(entries, actionId) < limit) return false;
  const reconciled = [...entries]
    .reverse()
    .find(
      ({ event }) => event.kind === "reconciled" && event.actionId === actionId,
    );
  return (
    reconciled?.event.kind === "reconciled" &&
    reconciled.event.outcome === "not_found"
  );
};

/** Attempts of `actionId` the ledger found impossible so far. */
export const scriptDecodeRejectedAttempts = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): number =>
  entries.filter(
    ({ event }) =>
      event.kind === "reconciled" &&
      event.actionId === actionId &&
      event.outcome === "not_found",
  ).length;
