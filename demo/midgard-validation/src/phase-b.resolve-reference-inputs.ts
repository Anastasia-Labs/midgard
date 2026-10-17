import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
  type MidgardValue,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";

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
  REJECT_SOURCE_KIND_SPEND,
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
import { orderMidgardInputResolutionSchedule } from "./validation-machine/input-resolution.js";

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

export type ResolvedCandidateInputs = ResolvedReferenceInputs & {
  /** The value of every spent output, keyed by out-ref. */
  readonly spentValues: ReadonlyMap<string, MidgardValue>;
  /** Whether some spent output is locked by a script. */
  readonly spendsScriptOutput: boolean;
};

/**
 * Resolves a candidate's spend and reference inputs in the machine's
 * ResolveInputs order: one schedule of both kinds, ascending by canonical
 * out-ref (`orderMidgardInputResolutionSchedule`, the order the trace builder
 * replays). Each node is refused at its own position: a spend already spent
 * by an accepted transaction, an absent or undecodable output, then a key
 * spend whose key did not sign. The program-material bundle is checked once
 * every input has resolved. Without `witnessKeyHashes`, spend signers are not
 * checked.
 */
export const resolveCandidateInputs = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  checks: {
    readonly spentByAccepted?: ReadonlySet<string>;
    readonly witnessKeyHashes?: ReadonlySet<string>;
  } = {},
): ResolvedCandidateInputs | RejectedTx => {
  const { ledgerTx } = node.candidate;
  const inputs: ResolvedReferenceInput[] = [];
  const scriptSources: ScriptSource[] = [];
  const scriptHashes = new Set<string>();
  const spentValues = new Map<string, MidgardValue>();
  let spendsScriptOutput = false;
  const fail = (
    code: RejectedTx["code"],
    detail: string,
    subject: RejectSubject,
  ): RejectedTx =>
    reject(ledgerTx.txId, code, detail, "resolveInputs", subject);

  const schedule = orderMidgardInputResolutionSchedule({
    spend: [...node.spentOutRefs],
    reference: [...node.referenceOutRefs],
    keyOf: (outRefHex) => Buffer.from(outRefHex, "hex"),
  });
  for (const { sourceKind, item: outRefHex } of schedule) {
    const spend = sourceKind === "spend";
    const kind = spend
      ? REJECT_SOURCE_KIND_SPEND
      : REJECT_SOURCE_KIND_REFERENCE;
    const index = () => inputOrdinalOf(ledgerTx, kind, outRefHex);
    if (spend && checks.spentByAccepted?.has(outRefHex) === true) {
      return reject(ledgerTx.txId, RejectCodes.DoubleSpend, outRefHex);
    }
    const outputBytes = stateValue(outRefHex);
    if (outputBytes === undefined) {
      return fail(
        RejectCodes.InputNotFound,
        spend ? outRefHex : `reference input not found: ${outRefHex}`,
        { arm: "InputNotFound", sourceKind: kind, index: index() },
      );
    }
    let output: MidgardTxOutput;
    let signer: string | undefined;
    try {
      output = decodeMidgardTxOutput(outputBytes);
      if (spend) {
        const paymentCred = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;
        if (paymentCred.kind === "Script") spendsScriptOutput = true;
        else signer = paymentCred.hash.toString("hex");
      }
    } catch (e) {
      return fail(
        RejectCodes.InvalidOutput,
        spend
          ? `failed to decode input output: ${String(e)}`
          : `failed to decode reference input output ${outRefHex}: ${String(e)}`,
        {
          arm: "InputSpentOutputNonCanonical",
          sourceKind: kind,
          index: index(),
        },
      );
    }
    if (spend) {
      if (
        signer !== undefined &&
        checks.witnessKeyHashes !== undefined &&
        !checks.witnessKeyHashes.has(signer)
      ) {
        return fail(
          RejectCodes.MissingRequiredWitness,
          `missing witness for input signer ${signer} (outref ${outRefHex})`,
          { arm: "SpendInputSignerMissing", index: index() },
        );
      }
      spentValues.set(outRefHex, output.value);
      continue;
    }
    inputs.push({ outRefHex, output });
    if (output.script_ref !== undefined) {
      const source = scriptSourceFromVersionedScript(
        output.script_ref,
        "reference",
        outRefHex,
      );
      scriptSources.push(source);
      scriptHashes.add(source.scriptHash);
    }
  }

  // A forced transaction runs no program and carries no program material.
  const sidecar = node.candidate.submission.programMaterialSidecarCbor;
  if (sidecar !== null && node.candidate.submission.sourceKind !== "forced") {
    try {
      const material = decodeMidgardCekProgramMaterialSidecar(sidecar);
      const canonicalTx = decodeMidgardNativeTxFullFromCanonicalCbor(
        node.candidate.submission.txCbor,
      );
      // Every input is present here, so this is the event's whole program
      // set, the one block builders and DA committee members derive.
      verifyMidgardCekProgramMaterialBundle(
        collectMidgardEventProgramEnvelopes(canonicalTx, stateValue),
        material,
      );
    } catch (cause) {
      return reject(
        ledgerTx.txId,
        RejectCodes.CekProgramMaterial,
        `invalid exact attached/reference-script CEK material: ${String(cause)}`,
        "scriptSources",
      );
    }
  }

  return {
    inputs,
    scriptSources,
    scriptHashes,
    spentValues,
    spendsScriptOutput,
  };
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

const collectInlineScriptSources = (
  ledgerTx: MidgardLedgerTx,
): readonly ScriptSource[] =>
  ledgerTx.scriptWitnesses.map((witness) =>
    scriptSourceFromVersionedScript(
      witness.script,
      "inline",
      `script_wit:${witness.index}`,
    ),
  );

/**
 * Script sources carried by spent outputs, in canonical out-ref order. A spent
 * output that is absent or does not decode contributes nothing here: input
 * resolution refuses the transaction before any source is looked up.
 */
const spentOutputScriptSources = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
): readonly ScriptSource[] =>
  sortTxOutRefHexes(node.spentOutRefs).flatMap((outRefHex) => {
    const outputBytes = stateValue(outRefHex);
    if (outputBytes === undefined) return [];
    let output: MidgardTxOutput;
    try {
      output = decodeMidgardTxOutput(outputBytes);
    } catch {
      return [];
    }
    return output.script_ref === undefined
      ? []
      : [
          scriptSourceFromVersionedScript(
            output.script_ref,
            "reference",
            outRefHex,
          ),
        ];
  });

/**
 * The transaction's script-source universe in lookup order: the reference
 * scripts of every resolved output (spent and reference inputs, in canonical
 * out-ref order) before the inline script witnesses. A purpose resolves to the
 * first source with its hash, so an inline witness whose script a resolved
 * output already carries is never used.
 */
export const collectScriptSources = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  references: ResolvedReferenceInputs,
): readonly ScriptSource[] => {
  const resolved = [
    ...spentOutputScriptSources(node, stateValue),
    ...references.scriptSources,
  ];
  const order = new Map(
    sortTxOutRefHexes(new Set(resolved.map((source) => source.sourceId))).map(
      (outRefHex, position) => [outRefHex, position],
    ),
  );
  return [
    ...resolved.sort(
      (left, right) => order.get(left.sourceId)! - order.get(right.sourceId)!,
    ),
    ...collectInlineScriptSources(node.candidate.ledgerTx),
  ];
};

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
