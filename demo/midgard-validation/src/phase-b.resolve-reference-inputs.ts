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
import {
  outputCborMeetsMinAda,
  outputCborMinAdaLovelace,
} from "./value-accounting.js";

/**
 * `E_MIN_ADA` / MIN-ADA-TX (#618 ruling 1; R8 of decision 0005): the first
 * output of this transaction that does not fund the parameterized minimum-Ada
 * floor, or `null` when every output does.
 *
 * The bytes measured here are `graph.produced[i][LedgerColumns.OUTPUT]` -- the
 * canonical output encoding Phase A already committed to the ledger entry, and
 * therefore exactly the bytes the on-chain output descriptor's `total_length`
 * binds (`buildMidgardLedgerOutputMaterialV1` sets `totalLength` from them).
 * Re-encoding the output here would risk measuring a different serialization
 * than the one the L1 machine convicts on.
 *
 * WHY THIS RUNS IN PHASE B, AND HERE. The on-chain twin rejects on the
 * ValueAndMint stage-3 output-descriptor step, so this rejection must carry the
 * `valueAndMint` consensus phase; and because `orderedPhases` in
 * ./validation-machine.ts places `valueAndMint` after `resolveInputs`,
 * `scriptSources`, `nativeScripts`, `scriptIntegrity` and `cek`, the check has
 * to run after everything those phases decide, and before the stage-5
 * value-preservation conjunct that shares the phase. Hoisting it into Phase A
 * admission -- where the rule would also be computable, since it is stateless
 * -- would invert the phase order and make the operator's claimed terminal
 * unprovable against the machine.
 */
export const minAdaViolation = (
  candidate: PhaseAValidatedTx,
): { readonly index: number; readonly detail: string } | null => {
  const outputs = candidate.ledgerTx.outputs;
  const produced = candidate.graph.produced;
  for (let index = 0; index < produced.length; index += 1) {
    const outputCbor = produced[index]![LedgerColumns.OUTPUT];
    const lovelace = outputs[index]?.value.lovelace;
    if (lovelace === undefined) {
      // Phase A builds one produced entry per output; a mismatch is a
      // construction bug, not a transaction fault, and must not be silently
      // read as "meets the floor".
      throw new Error(
        `phase B min-Ada scan: produced entry ${index.toString()} has no matching ledger output`,
      );
    }
    if (!outputCborMeetsMinAda(outputCbor, lovelace)) {
      return {
        index,
        detail: `output[${index.toString()}] ${lovelace.toString()} < ${outputCborMinAdaLovelace(
          outputCbor,
        ).toString()} for ${outputCbor.length.toString()} serialized bytes`,
      };
    }
  }
  return null;
};

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
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
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
