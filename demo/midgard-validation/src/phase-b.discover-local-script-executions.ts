import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";

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
import { ScriptContextView } from "./script-context.js";
import { resolveScriptSource, ScriptSource } from "./script-source.js";
import { sortTxOutRefHexes } from "./tx-out-ref.js";
import { RejectCodes } from "./types.js";
import { mintDeltaToScriptMintValue } from "./value-accounting.js";

export const discoverLocalScriptExecutions = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  sources: readonly ScriptSource[],
  resolvedReferenceInputs: readonly ResolvedReferenceInput[],
  witnessKeyHashes: ReadonlySet<string>,
):
  | Extract<LocalScriptValidationResult, { readonly kind: "rejected" }>
  | {
      readonly kind: "discovered";
      readonly executions: readonly RequiredScriptExecution[];
      readonly contextView: ScriptContextView;
    } => {
  const candidate = node.candidate;
  const { ledgerTx } = candidate;
  const redeemers = ledgerTx.redeemers;
  const seenRedeemerPointers = new Set<string>();
  for (const redeemer of redeemers) {
    const key = midgardRedeemerPointerKey(redeemer);
    if (seenRedeemerPointers.has(key)) {
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `duplicate redeemer ${key}`,
        consensusPhase: "scriptSources",
      };
    }
    seenRedeemerPointers.add(key);
  }

  const sortedSpent = sortTxOutRefHexes(node.spentOutRefs);
  const resolvedInputs: {
    readonly outRefHex: string;
    readonly output: MidgardTxOutput;
  }[] = [];
  const executions: RequiredScriptExecution[] = [];
  const expectedPointers = new Set<string>();

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
      };
    }
    // A native script runs without a redeemer, so a redeemer at its pointer
    // stays unexpected and is refused below as extraneous.
    if (resolved.version !== "NativeCardano") {
      expectedPointers.add(midgardRedeemerPointerKey(pointer));
      const redeemer = findRedeemerByPointer(redeemers, pointer);
      if (redeemer === undefined) {
        return {
          kind: "rejected",
          code: RejectCodes.MissingRequiredWitness,
          detail: `missing redeemer for ${purpose.kind} ${purpose.scriptHash}`,
          consensusPhase: "scriptSources",
        };
      }
    }
    const execution = { purpose, pointer, resolved };
    executions.push(execution);
    return { kind: "added", execution };
  };

  for (let index = 0; index < sortedSpent.length; index += 1) {
    const outRefHex = sortedSpent[index];
    const outputBytes = stateValue(outRefHex);
    if (outputBytes === undefined) {
      return {
        kind: "rejected",
        code: RejectCodes.InputNotFound,
        detail: outRefHex,
        consensusPhase: "resolveInputs",
      };
    }
    const output = decodeMidgardTxOutput(outputBytes);
    resolvedInputs.push({ outRefHex, output });
    const paymentCred = decodeMidgardAddressBytes(
      output.address,
    ).paymentCredential;
    if (paymentCred.kind === "Script") {
      const scriptHash = paymentCred.hash.toString("hex");
      const result = addExecution(
        { kind: "spend", scriptHash, outRefHex },
        { tag: MidgardRedeemerTag.Spend, index: BigInt(index) },
      );
      if (result.kind === "rejected") {
        return result;
      }
    }
  }

  const mintValue = mintDeltaToScriptMintValue(candidate.derived.mintDelta);
  const mintPolicies = candidate.derived.mintPolicyHashHexes;
  for (let index = 0; index < mintPolicies.length; index += 1) {
    const policyId = mintPolicies[index];
    const result = addExecution(
      { kind: "mint", scriptHash: policyId, policyId },
      { tag: MidgardRedeemerTag.Mint, index: BigInt(index) },
    );
    if (result.kind === "rejected") {
      return result;
    }
  }

  const observers = [...candidate.derived.requiredObserverHashHexes].sort();
  for (let index = 0; index < observers.length; index += 1) {
    const observer = observers[index];
    const result = addExecution(
      { kind: "observe", scriptHash: observer },
      { tag: MidgardRedeemerTag.Reward, index: BigInt(index) },
    );
    if (result.kind === "rejected") {
      return result;
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
        };
      }
      continue;
    }
    const scriptHash = paymentCred.hash.toString("hex");
    protectedReceivingHashes.add(scriptHash);
  }

  const receivingHashes = [...protectedReceivingHashes].sort();
  for (let index = 0; index < receivingHashes.length; index += 1) {
    const scriptHash = receivingHashes[index];
    const result = addExecution(
      { kind: "receive", scriptHash },
      { tag: MidgardRedeemerTag.Receiving, index: BigInt(index) },
    );
    if (result.kind === "rejected") {
      return result;
    }
  }

  for (const redeemer of redeemers) {
    if (!expectedPointers.has(midgardRedeemerPointerKey(redeemer))) {
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `extraneous redeemer ${midgardRedeemerPointerKey(redeemer)}`,
        consensusPhase: "scriptSources",
      };
    }
  }

  const purposeByPointer = new Map(
    executions.map((execution) => [
      midgardRedeemerPointerKey(execution.pointer),
      execution.purpose,
    ]),
  );
  return {
    kind: "discovered",
    executions,
    contextView: {
      txId: ledgerTx.txId,
      inputs: resolvedInputs,
      referenceInputs: resolvedReferenceInputs,
      outputs,
      fee: ledgerTx.fee,
      validityIntervalStart: ledgerTx.validityIntervalStart,
      validityIntervalEnd: ledgerTx.validityIntervalEnd,
      observers,
      signatories: candidate.derived.witnessKeyHashHexes,
      mint: mintValue,
      // Witness-list order, which is the order the fault proof commits the
      // context's redeemer map in.
      redeemers: redeemers.flatMap((redeemer) => {
        const purpose = purposeByPointer.get(
          midgardRedeemerPointerKey(redeemer),
        );
        return purpose === undefined ? [] : [{ purpose, redeemer }];
      }),
    },
  };
};
