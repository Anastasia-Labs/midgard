import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";

import {
  inputOrdinalOf,
  REJECT_SOURCE_KIND_REFERENCE,
  REJECT_SOURCE_KIND_SPEND,
  type RejectSubject,
} from "../reject-subject.js";
import {
  ScriptSource,
  scriptSourceFromVersionedScript,
} from "../script-source.js";
import { RejectCodes, RejectedTx } from "../types.js";
import { orderMidgardInputResolutionSchedule } from "../validation-machine/input-resolution.js";
import { CandidateNode } from "./candidate-graph.js";
import { reject } from "./rejection.js";
import {
  ResolvedReferenceInput,
  ResolvedReferenceInputs,
} from "./script-sources.js";

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

  return {
    inputs,
    scriptSources,
    scriptHashes,
    spentValues,
    spendsScriptOutput,
  };
};
