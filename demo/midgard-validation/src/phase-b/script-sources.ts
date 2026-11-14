import { type MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  type MidgardTxOutput,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";

import {
  type MidgardLedgerOutput,
  type MidgardLedgerTx,
} from "../ledger-tx/types.js";
import {
  MidgardRedeemerPointer,
  MidgardScriptPurpose,
} from "../midgard-redeemers.js";
import { type RejectSubject } from "../reject-subject.js";
import {
  ResolvedScriptSource,
  ScriptSource,
  scriptSourceFromVersionedScript,
} from "../script-source.js";
import { sortTxOutRefHexes } from "../tx-out-ref.js";
import { RejectedTx } from "../types.js";
import { CandidateNode } from "./candidate-graph.js";

export type ResolvedReferenceInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

export type ResolvedReferenceInputs = {
  readonly inputs: readonly ResolvedReferenceInput[];
  readonly scriptSources: readonly ScriptSource[];
  readonly scriptHashes: ReadonlySet<string>;
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
