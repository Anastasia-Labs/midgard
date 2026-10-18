import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";

import {
  findRedeemerByPointer,
  MidgardRedeemerPointer,
  midgardRedeemerPointerKey,
  MidgardRedeemerTag,
  MidgardScriptPurpose,
} from "./midgard-redeemers.js";
import {
  type CandidateNode,
  ledgerOutputToTxOutput,
  type LocalScriptValidationResult,
  type RequiredScriptExecution,
  type ResolvedReferenceInput,
} from "./phase-b.resolve-reference-inputs.js";
import { inputOrdinalOf, REJECT_SOURCE_KIND_SPEND } from "./reject-subject.js";
import { ScriptContextView } from "./script-context.js";
import { resolveScriptSource, ScriptSource } from "./script-source.js";
import { sortTxOutRefHexes } from "./tx-out-ref.js";
import { RejectCodes } from "./types.js";
import { mintDeltaToScriptMintValue } from "./value-accounting.js";

/** `purpose_kind` coordinate of a script purpose (0 spend .. 3 receive). */
export const PURPOSE_KIND: Record<MidgardScriptPurpose["kind"], bigint> = {
  spend: 0n,
  mint: 1n,
  observe: 2n,
  receive: 3n,
};

export const discoverLocalScriptExecutions = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  sources: readonly ScriptSource[],
  resolvedReferenceInputs: readonly ResolvedReferenceInput[],
  witnessKeyHashes: ReadonlySet<string>,
  l1SlotConfig: L1SlotConfig,
):
  | Extract<LocalScriptValidationResult, { readonly kind: "rejected" }>
  | {
      readonly kind: "discovered";
      readonly executions: readonly RequiredScriptExecution[];
      /** The unused-redeemer audit's refusal, if any; see below. */
      readonly unusedRedeemer:
        | Extract<LocalScriptValidationResult, { readonly kind: "rejected" }>
        | undefined;
      readonly contextView: ScriptContextView;
    } => {
  const candidate = node.candidate;
  const { ledgerTx } = candidate;
  const redeemers = ledgerTx.redeemers;
  const sortedSpent = sortTxOutRefHexes(node.spentOutRefs);
  const resolvedInputs: {
    readonly outRefHex: string;
    readonly output: MidgardTxOutput;
  }[] = [];
  const executions: RequiredScriptExecution[] = [];
  // Raw field-8 positions of the redeemers selected executions use: the first
  // redeemer at each non-native purpose's pointer, as the machine's stage-10
  // search marks it.
  const usedRedeemerIndices = new Set<number>();

  const addExecution = (
    purpose: MidgardScriptPurpose,
    pointer: MidgardRedeemerPointer,
  ):
    | Extract<LocalScriptValidationResult, { readonly kind: "rejected" }>
    | {
        readonly kind: "added";
        readonly execution: RequiredScriptExecution;
      } => {
    const resolved = resolveScriptSource(purpose.scriptHash, sources);
    if (resolved === undefined) {
      return {
        kind: "rejected",
        code: RejectCodes.MissingRequiredWitness,
        detail: `missing script source for ${purpose.kind} ${purpose.scriptHash}`,
        consensusPhase: "scriptSources",
        subject: {
          arm: "ScriptSourceMissing",
          purposeKind: PURPOSE_KIND[purpose.kind],
          purposeIndex: pointer.index,
        },
      };
    }
    // A native script runs without a redeemer, so a redeemer at its pointer
    // stays unused and is refused by the unused-redeemer audit.
    if (resolved.version !== "NativeCardano") {
      const redeemerIndex = redeemers.findIndex(
        (redeemer) =>
          redeemer.tag === pointer.tag && redeemer.index === pointer.index,
      );
      if (redeemerIndex < 0) {
        return {
          kind: "rejected",
          code: RejectCodes.MissingRequiredWitness,
          detail: `missing redeemer for ${purpose.kind} ${purpose.scriptHash}`,
          consensusPhase: "scriptSources",
          subject: {
            arm: "RedeemerMissing",
            purposeKind: PURPOSE_KIND[purpose.kind],
            purposeIndex: pointer.index,
          },
        };
      }
      usedRedeemerIndices.add(redeemerIndex);
    }
    const execution = { purpose, pointer, resolved };
    executions.push(execution);
    return { kind: "added", execution };
  };

  // The machine's order: replay the spent inputs into spend purposes (stage
  // 3), scan every output (stages 4-5), add the mint, observer and receive
  // purposes (stages 6-7), and only then look each purpose's source and
  // redeemer up (stages 8-10).
  const purposes: {
    readonly purpose: MidgardScriptPurpose;
    readonly pointer: MidgardRedeemerPointer;
  }[] = [];
  for (let index = 0; index < sortedSpent.length; index += 1) {
    const outRefHex = sortedSpent[index];
    const outputBytes = stateValue(outRefHex);
    if (outputBytes === undefined) {
      return {
        kind: "rejected",
        code: RejectCodes.InputNotFound,
        detail: outRefHex,
        consensusPhase: "resolveInputs",
        subject: {
          arm: "InputNotFound",
          sourceKind: REJECT_SOURCE_KIND_SPEND,
          index: inputOrdinalOf(ledgerTx, REJECT_SOURCE_KIND_SPEND, outRefHex),
        },
      };
    }
    const output = decodeMidgardTxOutput(outputBytes);
    resolvedInputs.push({ outRefHex, output });
    const paymentCred = decodeMidgardAddressBytes(
      output.address,
    ).paymentCredential;
    if (paymentCred.kind === "Script") {
      const scriptHash = paymentCred.hash.toString("hex");
      purposes.push({
        purpose: { kind: "spend", scriptHash, outRefHex },
        pointer: { tag: MidgardRedeemerTag.Spend, index: BigInt(index) },
      });
    }
  }

  const outputs = ledgerTx.outputs.map(ledgerOutputToTxOutput);
  const protectedReceivingHashes = new Set<string>();
  for (let index = 0; index < outputs.length; index += 1) {
    const output = outputs[index];
    const outputAddress = decodeMidgardAddressBytes(output.address);
    if (
      BigInt(outputAddress.networkId) !== candidate.derived.expectedNetworkId
    ) {
      return {
        kind: "rejected",
        code: RejectCodes.NetworkIdMismatch,
        detail: `output ${index.toString()} network ${outputAddress.networkId.toString()} != ${candidate.derived.expectedNetworkId.toString()}`,
        consensusPhase: "scriptSources",
      };
    }
    if (!outputAddress.protected) {
      continue;
    }
    const paymentCred = outputAddress.paymentCredential;
    if (paymentCred.kind === "PubKey") {
      const pubKey = paymentCred.hash.toString("hex");
      if (!witnessKeyHashes.has(pubKey)) {
        return {
          kind: "rejected",
          code: RejectCodes.MissingRequiredWitness,
          detail: `missing witness for protected output signer ${pubKey}`,
          consensusPhase: "scriptSources",
          subject: {
            arm: "ProtectedOutputSignerMissing",
            index: BigInt(index),
          },
        };
      }
      continue;
    }
    const scriptHash = paymentCred.hash.toString("hex");
    protectedReceivingHashes.add(scriptHash);
  }
  // A forced transaction's held output follows the outputs above.
  if (candidate.heldOutputFaultIndex !== undefined) {
    return {
      kind: "rejected",
      code: RejectCodes.InvalidOutput,
      detail: `output[${candidate.heldOutputFaultIndex}] is not a canonical ledger output`,
      consensusPhase: "scriptSources",
      subject: {
        arm: "OutputNonCanonical",
        index: BigInt(candidate.heldOutputFaultIndex),
      },
    };
  }

  const mintValue = mintDeltaToScriptMintValue(candidate.derived.mintDelta);
  const mintPolicies = candidate.derived.mintPolicyHashHexes;
  for (let index = 0; index < mintPolicies.length; index += 1) {
    const policyId = mintPolicies[index];
    purposes.push({
      purpose: { kind: "mint", scriptHash: policyId, policyId },
      pointer: { tag: MidgardRedeemerTag.Mint, index: BigInt(index) },
    });
  }

  const observers = [...candidate.derived.requiredObserverHashHexes].sort();
  for (let index = 0; index < observers.length; index += 1) {
    purposes.push({
      purpose: { kind: "observe", scriptHash: observers[index] },
      pointer: { tag: MidgardRedeemerTag.Reward, index: BigInt(index) },
    });
  }

  const receivingHashes = [...protectedReceivingHashes].sort();
  for (let index = 0; index < receivingHashes.length; index += 1) {
    purposes.push({
      purpose: { kind: "receive", scriptHash: receivingHashes[index] },
      pointer: { tag: MidgardRedeemerTag.Receiving, index: BigInt(index) },
    });
  }

  for (const { purpose, pointer } of purposes) {
    const result = addExecution(purpose, pointer);
    if (result.kind === "rejected") {
      return result;
    }
  }

  // The machine audits unused redeemers (stage 12) only after its unused
  // inline witness audit (stage 11), so the caller runs this one second. A
  // redeemer repeating a used pointer, or at a native purpose's pointer, is
  // unused; the first unused one in raw field-8 order is refused.
  const unusedRedeemerIndex = redeemers.findIndex(
    (_redeemer, redeemerIndex) => !usedRedeemerIndices.has(redeemerIndex),
  );
  const unusedKey =
    unusedRedeemerIndex < 0
      ? ""
      : midgardRedeemerPointerKey(redeemers[unusedRedeemerIndex]!);
  const repeatsPointer = redeemers
    .slice(0, Math.max(unusedRedeemerIndex, 0))
    .some((redeemer) => midgardRedeemerPointerKey(redeemer) === unusedKey);
  const unusedRedeemer =
    unusedRedeemerIndex < 0
      ? undefined
      : {
          kind: "rejected" as const,
          code: RejectCodes.InvalidFieldType,
          detail: `${repeatsPointer ? "duplicate" : "extraneous"} redeemer ${unusedKey}`,
          consensusPhase: "scriptSources" as const,
          subject: {
            arm: "UnusedRedeemer" as const,
            index: BigInt(unusedRedeemerIndex),
          },
        };

  return {
    kind: "discovered",
    executions,
    unusedRedeemer,
    contextView: {
      txId: ledgerTx.txId,
      inputs: resolvedInputs,
      referenceInputs: resolvedReferenceInputs,
      outputs,
      fee: ledgerTx.fee,
      validityIntervalStart: ledgerTx.validityIntervalStart,
      validityIntervalEnd: ledgerTx.validityIntervalEnd,
      l1SlotConfig,
      observers,
      signatories: candidate.derived.witnessKeyHashHexes,
      mint: mintValue,
      // Ledger order, (tag, index) ascending: executions are discovered
      // spends, mints, observers, then receives, each by pointer index. A
      // native script has no redeemer and no map entry.
      redeemers: executions.flatMap((execution) => {
        if (execution.resolved.version === "NativeCardano") {
          return [];
        }
        const redeemer = findRedeemerByPointer(redeemers, execution.pointer);
        return redeemer === undefined
          ? []
          : [{ purpose: execution.purpose, redeemer }];
      }),
    },
  };
};
