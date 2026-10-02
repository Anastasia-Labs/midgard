import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import {
  collectMidgardAttachedProgramEnvelopes,
  decodeMidgardScriptProgramEnvelope,
} from "@al-ft/midgard-core/script-proof";

import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
} from "./ledger-tx/types.js";
import {
  MidgardRedeemerPointer,
  MidgardScriptPurpose,
} from "./midgard-redeemers.js";
import {
  inputOrdinalOf,
  REJECT_SOURCE_KIND_REFERENCE,
  type RejectSubject,
} from "./reject-subject.js";
import {
  ResolvedScriptSource,
  ScriptSource,
  scriptSourceFromVersionedScript,
} from "./script-source.js";
import { sortTxOutRefHexes } from "./tx-out-ref.js";
import {
  PhaseAValidatedTx,
  PhaseBResult,
  RejectCodes,
  RejectedTx,
} from "./types.js";

type UTxOState = Map<string, Buffer>;

export type PreState = readonly LedgerEntry[] | UTxOState;

export type CandidateNode = {
  readonly index: number;
  readonly candidate: PhaseAValidatedTx;
  readonly spentOutRefs: Set<string>;
  readonly referenceOutRefs: Set<string>;
  readonly parents: Set<number>;
  readonly children: Set<number>;
};

export type CandidateStatus = "pending" | "accepted" | "rejected";

export type CandidateDecision = {
  readonly index: number;
  readonly accepted: boolean;
  readonly rejection?: RejectedTx;
};

export type ResolvedReferenceInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

export type ResolvedReferenceInputs = {
  readonly inputs: readonly ResolvedReferenceInput[];
  readonly scriptSources: readonly ScriptSource[];
  readonly scriptHashes: ReadonlySet<string>;
};

export type UTxOStatePatch = {
  readonly deletedOutRefs: readonly string[];
  readonly upsertedOutRefs: readonly (readonly [string, Buffer])[];
};

export type PhaseBResultWithPatch = PhaseBResult & {
  readonly statePatch: UTxOStatePatch;
};

type MutableStatePatch = {
  readonly deletedOutRefs: Set<string>;
  readonly upsertedOutRefs: Map<string, Buffer>;
};

export const buildState = (entries: PreState): UTxOState => {
  if (entries instanceof Map) {
    return entries;
  }
  const state: UTxOState = new Map();
  for (const entry of entries) {
    state.set(entry[LedgerColumns.OUTREF].toString("hex"), entry.output);
  }
  return state;
};

export const makeEmptyStatePatch = (): MutableStatePatch => ({
  deletedOutRefs: new Set<string>(),
  upsertedOutRefs: new Map<string, Buffer>(),
});

export const getStateValue = (
  baseState: UTxOState,
  patch: MutableStatePatch,
  outRefHex: string,
): Buffer | undefined => {
  const updatedValue = patch.upsertedOutRefs.get(outRefHex);
  if (updatedValue !== undefined) {
    return updatedValue;
  }
  if (patch.deletedOutRefs.has(outRefHex)) {
    return undefined;
  }
  return baseState.get(outRefHex);
};

export const materializeStatePatch = (
  patch: MutableStatePatch,
): UTxOStatePatch => ({
  deletedOutRefs: Array.from(patch.deletedOutRefs),
  upsertedOutRefs: Array.from(patch.upsertedOutRefs.entries()).map(
    ([outRefHex, output]) =>
      [outRefHex, Buffer.from(output)] as readonly [string, Buffer],
  ),
});

export const applyUTxOStatePatch = (
  state: UTxOState,
  patch: UTxOStatePatch,
): void => {
  for (const outRefHex of patch.deletedOutRefs) {
    state.delete(outRefHex);
  }
  for (const [outRefHex, output] of patch.upsertedOutRefs) {
    state.set(outRefHex, Buffer.from(output));
  }
};

export const reject = (
  txId: Buffer,
  code: RejectedTx["code"],
  detail: string | null = null,
  consensusPhase: MidgardValidationPhaseName = "resolveInputs",
  subject?: RejectSubject,
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
  ...(subject === undefined ? {} : { subject }),
});

const referenceInputSubject = (
  node: CandidateNode,
  arm: "InputNotFound" | "InputSpentOutputNonCanonical",
  outRefHex: string,
): RejectSubject => ({
  arm,
  sourceKind: REJECT_SOURCE_KIND_REFERENCE,
  index: inputOrdinalOf(
    node.candidate.ledgerTx,
    REJECT_SOURCE_KIND_REFERENCE,
    outRefHex,
  ),
});

export const resolveReferenceInputs = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
): ResolvedReferenceInputs | RejectedTx => {
  const inputs: ResolvedReferenceInput[] = [];
  const scriptSources: ScriptSource[] = [];
  const scriptHashes = new Set<string>();

  for (const referenceOutRefHex of sortTxOutRefHexes(node.referenceOutRefs)) {
    const referenceOutput = stateValue(referenceOutRefHex);
    if (referenceOutput === undefined) {
      return reject(
        node.candidate.ledgerTx.txId,
        RejectCodes.InputNotFound,
        `reference input not found: ${referenceOutRefHex}`,
        "resolveInputs",
        referenceInputSubject(node, "InputNotFound", referenceOutRefHex),
      );
    }

    let output: MidgardTxOutput;
    try {
      output = decodeMidgardTxOutput(referenceOutput);
    } catch (e) {
      return reject(
        node.candidate.ledgerTx.txId,
        RejectCodes.InvalidOutput,
        `failed to decode reference input output ${referenceOutRefHex}: ${String(e)}`,
        "resolveInputs",
        referenceInputSubject(
          node,
          "InputSpentOutputNonCanonical",
          referenceOutRefHex,
        ),
      );
    }
    inputs.push({ outRefHex: referenceOutRefHex, output });

    const scriptRef = output.script_ref;
    if (scriptRef === undefined) {
      continue;
    }

    const source = scriptSourceFromVersionedScript(
      scriptRef,
      "reference",
      referenceOutRefHex,
    );
    scriptSources.push(source);
    scriptHashes.add(source.scriptHash);
  }

  const sidecar = node.candidate.submission.programMaterialSidecarCbor;
  if (sidecar !== null) {
    try {
      const material = decodeMidgardCekProgramMaterialSidecar(sidecar);
      const canonicalTx = (
        node.candidate.submission.sourceKind === "forced"
          ? decodeMidgardForcedTxFullFromCanonicalCbor
          : decodeMidgardNativeTxFullFromCanonicalCbor
      )(node.candidate.submission.txCbor);
      const envelopes = [
        ...collectMidgardAttachedProgramEnvelopes(canonicalTx),
      ];
      for (const input of inputs) {
        if (input.output.script_ref === undefined) continue;
        const envelope = decodeMidgardScriptProgramEnvelope(
          input.output.script_ref,
        );
        if (envelope !== null) {
          envelopes.push(envelope);
        }
      }
      verifyMidgardCekProgramMaterialBundle(envelopes, material);
    } catch (cause) {
      return reject(
        node.candidate.ledgerTx.txId,
        RejectCodes.CekProgramMaterial,
        `invalid exact attached/reference-script CEK material: ${String(cause)}`,
        "scriptSources",
      );
    }
  }

  return { inputs, scriptSources, scriptHashes };
};

export type RequiredScriptExecution = {
  readonly purpose: MidgardScriptPurpose;
  readonly pointer: MidgardRedeemerPointer;
  readonly resolved: ResolvedScriptSource;
};

export type LocalScriptValidationResult =
  | { readonly kind: "accepted" }
  | {
      readonly kind: "rejected";
      readonly code: RejectedTx["code"];
      readonly detail: string;
      readonly consensusPhase: MidgardValidationPhaseName;
      readonly subject?: RejectSubject;
    };

export const ledgerOutputToTxOutput = (
  output: MidgardLedgerOutput,
): MidgardTxOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.scriptRef === undefined ? {} : { script_ref: output.scriptRef }),
});

export const collectInlineScriptSources = (
  ledgerTx: MidgardLedgerTx,
): readonly ScriptSource[] =>
  ledgerTx.scriptWitnesses.map((witness) =>
    scriptSourceFromVersionedScript(
      witness.script,
      "inline",
      `script_wit:${witness.index}`,
    ),
  );

const scriptLanguageForExecution = (
  execution: RequiredScriptExecution,
): ScriptLanguageName | undefined => {
  switch (execution.resolved.version) {
    case "PlutusV3":
      return "PlutusV3";
    case "MidgardV1":
      return "MidgardV1";
    case "NativeCardano":
      return undefined;
  }
};

export const requiredScriptLanguages = (
  executions: readonly RequiredScriptExecution[],
): readonly ScriptLanguageName[] =>
  Array.from(
    new Set(
      executions.flatMap((execution) => {
        const language = scriptLanguageForExecution(execution);
        return language === undefined ? [] : [language];
      }),
    ),
  ).sort();
