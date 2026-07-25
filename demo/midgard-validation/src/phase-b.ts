import type { MidgardValidationPhaseName } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  verifyMidgardCekProgramMaterialV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  type MidgardTxOutput,
  type MidgardValue,
  type ScriptLanguageName,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  decodeMidgardV1ScriptProgramEnvelope,
} from "@al-ft/midgard-core/script-proof";
import { Effect } from "effect";

import {
  buildMidgardCekExecutionGraphV1,
  executeMidgardCekStructuralProgramV1,
} from "./cek-executor.js";
import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
} from "./ledger-tx/types.js";
import {
  encodeScriptContextCbor,
  evaluateScriptWithHarmonic,
  type LocalScriptEvalResult,
} from "./local-script-eval.js";
import {
  findRedeemerByPointer,
  MidgardRedeemerPointer,
  midgardRedeemerPointerKey,
  MidgardRedeemerTag,
  MidgardScriptPurpose,
} from "./midgard-redeemers.js";
import {
  buildMidgardV1ScriptContext,
  buildPlutusV3ScriptContext,
  ScriptContextView,
} from "./script-context.js";
import {
  ResolvedScriptSource,
  resolveScriptSource,
  ScriptSource,
  scriptSourceFromVersionedScript,
} from "./script-source.js";
import { sortTxOutRefHexes } from "./tx-out-ref.js";
import {
  PhaseAValidatedTx,
  PhaseBConfig,
  PhaseBResult,
  RejectCodes,
  RejectedTx,
} from "./types.js";
import {
  describeValueDelta,
  isZeroValueDelta,
  mintDeltaToScriptMintValue,
  sumMidgardValues,
  valuePreservationDelta,
} from "./value-accounting.js";

type UTxOState = Map<string, Buffer>;
type PreState = readonly LedgerEntry[] | UTxOState;

type CandidateNode = {
  readonly index: number;
  readonly candidate: PhaseAValidatedTx;
  readonly spentOutRefs: Set<string>;
  readonly referenceOutRefs: Set<string>;
  readonly parents: Set<number>;
  readonly children: Set<number>;
};

type CandidateStatus = "pending" | "accepted" | "rejected";

type CandidateDecision = {
  readonly index: number;
  readonly accepted: boolean;
  readonly rejection?: RejectedTx;
};

type ResolvedReferenceInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

type ResolvedReferenceInputs = {
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

const buildState = (entries: PreState): UTxOState => {
  if (entries instanceof Map) {
    return entries;
  }
  const state: UTxOState = new Map();
  for (const entry of entries) {
    state.set(entry[LedgerColumns.OUTREF].toString("hex"), entry.output);
  }
  return state;
};

const makeEmptyStatePatch = (): MutableStatePatch => ({
  deletedOutRefs: new Set<string>(),
  upsertedOutRefs: new Map<string, Buffer>(),
});

const getStateValue = (
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

const materializeStatePatch = (patch: MutableStatePatch): UTxOStatePatch => ({
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

const reject = (
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

const resolveReferenceInputs = (
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

  const sidecar =
    node.candidate.submission.programMaterialSidecarCbor;
  if (sidecar !== null) {
    try {
      const material =
        decodeMidgardCekProgramMaterialSidecarV1(sidecar);
      for (const input of inputs) {
        if (input.output.script_ref === undefined) continue;
        const envelope = decodeMidgardV1ScriptProgramEnvelope(
          input.output.script_ref,
        );
        if (envelope !== null) {
          verifyMidgardCekProgramMaterialV1(envelope, material, {
            allowUnreachable: true,
          });
        }
      }
    } catch (cause) {
      return reject(
        node.candidate.ledgerTx.txId,
        RejectCodes.CekProgramMaterial,
        `invalid reference-script CEK material: ${String(cause)}`,
        "scriptSources",
      );
    }
  }

  return { inputs, scriptSources, scriptHashes };
};

type RequiredScriptExecution = {
  readonly purpose: MidgardScriptPurpose;
  readonly pointer: MidgardRedeemerPointer;
  readonly resolved: ResolvedScriptSource;
};

type LocalScriptValidationResult =
  | { readonly kind: "accepted" }
  | {
      readonly kind: "rejected";
      readonly code: RejectedTx["code"];
      readonly detail: string;
      readonly consensusPhase: MidgardValidationPhaseName;
    };

const ledgerOutputToTxOutput = (
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

const requiredScriptLanguages = (
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

const discoverLocalScriptExecutions = (
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
    expectedPointers.add(midgardRedeemerPointerKey(pointer));
    const resolved = resolveScriptSource(purpose.scriptHash, sources);
    if (resolved === undefined) {
      return {
        kind: "rejected",
        code: RejectCodes.MissingRequiredWitness,
        detail: `missing script source for ${purpose.kind} ${purpose.scriptHash}`,
        consensusPhase: "scriptSources",
      };
    }
    if (resolved.version !== "NativeCardano") {
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
      redeemers: executions.flatMap((execution) => {
        const redeemer = findRedeemerByPointer(redeemers, execution.pointer);
        return redeemer === undefined
          ? []
          : [{ purpose: execution.purpose, redeemer }];
      }),
    },
  };
};

const runLocalScriptEvaluation = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  resolvedReferenceInputs: ResolvedReferenceInputs,
  witnessKeyHashes: ReadonlySet<string>,
  config: PhaseBConfig,
): Effect.Effect<LocalScriptValidationResult, Error> =>
  Effect.gen(function* () {
    const candidate = node.candidate;
    const { ledgerTx } = candidate;
    const inlineSources = collectInlineScriptSources(ledgerTx);
    const sources = [
      ...inlineSources,
      ...resolvedReferenceInputs.scriptSources,
    ];
    const discovered = discoverLocalScriptExecutions(
      node,
      stateValue,
      sources,
      resolvedReferenceInputs.inputs,
      witnessKeyHashes,
    );
    if (discovered.kind === "rejected") {
      return discovered;
    }
    let proofProgramMaterial:
      | ReturnType<typeof decodeMidgardCekProgramMaterialSidecarV1>
      | null = null;
    if (candidate.submission.programMaterialSidecarCbor !== null) {
      try {
        proofProgramMaterial =
          decodeMidgardCekProgramMaterialSidecarV1(
            candidate.submission.programMaterialSidecarCbor,
          );
      } catch (cause) {
        return {
          kind: "rejected",
          code: RejectCodes.CekProgramMaterial,
          detail: `invalid V1 CEK material during execution: ${String(cause)}`,
          consensusPhase: "scriptSources",
        };
      }
    }

    const usedInlineSourceIds = new Set(
      discovered.executions
        .filter((execution) => execution.resolved.source.origin === "inline")
        .map((execution) => execution.resolved.source.sourceId),
    );
    for (const source of inlineSources) {
      if (!usedInlineSourceIds.has(source.sourceId)) {
        const kind =
          source.nativeScript === undefined ? "non-native" : "native";
        return {
          kind: "rejected",
          code: RejectCodes.InvalidFieldType,
          detail: `extraneous ${kind} script witness ${source.sourceId}`,
          consensusPhase: "scriptSources",
        };
      }
    }

    for (const execution of discovered.executions) {
      if (execution.resolved.version !== "NativeCardano") {
        continue;
      }
      const nativeScript = execution.resolved.source.nativeScript!;
      if (
        !verifyMidgardNativeScript(nativeScript, {
          validityIntervalStart: ledgerTx.validityIntervalStart,
          validityIntervalEnd: ledgerTx.validityIntervalEnd,
          witnessSigners: witnessKeyHashes,
        })
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.NativeScriptInvalid,
          detail: `native script verification failed for ${execution.purpose.kind} ${execution.purpose.scriptHash}`,
          consensusPhase: "nativeScripts",
        };
      }
    }

    const nonNativeExecutions = discovered.executions.filter(
      (execution) => execution.resolved.version !== "NativeCardano",
    );

    const languages = requiredScriptLanguages(discovered.executions);
    const expectedScriptIntegrityHash = computeScriptIntegrityHashForLanguages(
      candidate.derived.redeemerWitnessHash,
      languages,
    );
    if (!ledgerTx.scriptIntegrityHash.equals(expectedScriptIntegrityHash)) {
      const expectedHex = expectedScriptIntegrityHash.toString("hex");
      const actualHex = ledgerTx.scriptIntegrityHash.toString("hex");
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `script_integrity_hash mismatch: expected ${expectedHex} actual ${actualHex} required_languages=${languages.join(",")}`,
        consensusPhase: "scriptIntegrity",
      };
    }

    for (const execution of nonNativeExecutions) {
      const redeemer = findRedeemerByPointer(
        discovered.contextView.redeemers.map((entry) => entry.redeemer),
        execution.pointer,
      )!;

      if (
        execution.resolved.version === "PlutusV3" &&
        execution.purpose.kind === "receive"
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: "ReceivingScript requires MidgardV1 context",
          consensusPhase: "cek",
        };
      }

      const context =
        execution.resolved.version === "MidgardV1"
          ? buildMidgardV1ScriptContext(
              discovered.contextView,
              execution.purpose,
              redeemer,
            )
          : buildPlutusV3ScriptContext(
              discovered.contextView,
              execution.purpose,
              redeemer,
            );
      const contextCbor = encodeScriptContextCbor(context);
      let result: LocalScriptEvalResult;
      if (proofProgramMaterial !== null) {
        if (config.evaluateProofScript !== undefined) {
          result = yield* config.evaluateProofScript(
            execution.resolved.source.scriptBytes,
            contextCbor,
          );
        } else {
          try {
            const envelope = decodeMidgardCekProgramEnvelopeV1(
              execution.resolved.source.scriptBytes,
            );
            const graph = buildMidgardCekExecutionGraphV1(
              envelope,
              proofProgramMaterial,
              contextCbor,
            );
            const cek = executeMidgardCekStructuralProgramV1({
              root: graph.root,
              material: graph.material.values(),
              constantWitnesses: graph.constantWitnesses,
              maxSteps:
                MIDGARD_CONSENSUS_LIMITS_V1
                  .maxValidationMachineStepCount,
            });
            result =
              cek.terminalState.mode === "haltSuccess"
                ? {
                    kind: "accepted",
                    budget: {
                      cpu: cek.terminalState.cpu,
                      memory: cek.terminalState.memory,
                    },
                  }
                : {
                    kind: "script_invalid",
                    detail: `V1 CEK halted with error ${cek.terminalState.auxiliary.toString(10)}`,
                  };
          } catch (cause) {
            result = {
              kind: "script_invalid",
              detail: `V1 CEK execution failed closed: ${String(cause)}`,
            };
          }
        }
      } else {
        result =
          config.evaluateScript === undefined
            ? evaluateScriptWithHarmonic(
                execution.resolved.source.scriptBytes,
                context,
              )
            : yield* config.evaluateScript(
                execution.resolved.source.scriptBytes,
                contextCbor,
              );
      }
      if (result.kind === "script_invalid") {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: ${result.detail}`,
          consensusPhase: "cek",
        };
      }
      if (
        config.enforceScriptBudget !== false &&
        (result.budget.cpu > redeemer.exUnits.steps ||
          result.budget.memory > redeemer.exUnits.memory)
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: budget exceeded (spent mem=${result.budget.memory} cpu=${result.budget.cpu}, declared mem=${redeemer.exUnits.memory} cpu=${redeemer.exUnits.steps})`,
          consensusPhase: "cek",
        };
      }
    }

    return { kind: "accepted" };
  });

type ConflictNode = {
  readonly spentOutRefs: ReadonlySet<string>;
  readonly referenceOutRefs: ReadonlySet<string>;
};

/**
 * Builds transitive conflict components in near-linear time. Ref/ref overlap is
 * intentionally not a conflict; spend/spend and spend/ref overlap are.
 */
export const buildConflictComponents = <T extends ConflictNode>(
  readyNodes: readonly T[],
): T[][] => {
  const parent = readyNodes.map((_, index) => index);
  const rank = readyNodes.map(() => 0);
  const find = (index: number): number => {
    let root = index;
    while (parent[root] !== root) {
      root = parent[root];
    }
    while (parent[index] !== index) {
      const next = parent[index];
      parent[index] = root;
      index = next;
    }
    return root;
  };
  const union = (left: number, right: number): void => {
    let leftRoot = find(left);
    let rightRoot = find(right);
    if (leftRoot === rightRoot) return;
    if (rank[leftRoot] < rank[rightRoot]) {
      [leftRoot, rightRoot] = [rightRoot, leftRoot];
    }
    parent[rightRoot] = leftRoot;
    if (rank[leftRoot] === rank[rightRoot]) rank[leftRoot] += 1;
  };

  const spenderByOutRef = new Map<string, number>();
  const referencersByOutRef = new Map<string, number[]>();
  readyNodes.forEach((node, nodeIndex) => {
    for (const outRef of node.spentOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      for (const referencer of referencersByOutRef.get(outRef) ?? []) {
        union(nodeIndex, referencer);
      }
      spenderByOutRef.set(outRef, nodeIndex);
    }
    for (const outRef of node.referenceOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      const referencers = referencersByOutRef.get(outRef) ?? [];
      referencers.push(nodeIndex);
      referencersByOutRef.set(outRef, referencers);
    }
  });

  const componentByRoot = new Map<number, T[]>();
  readyNodes.forEach((node, index) => {
    const root = find(index);
    const component = componentByRoot.get(root) ?? [];
    component.push(node);
    componentByRoot.set(root, component);
  });
  return Array.from(componentByRoot.values());
};

const buildNodes = (
  candidates: readonly PhaseAValidatedTx[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();

  for (let i = 0; i < candidates.length; i++) {
    for (const produced of candidates[i].graph.produced) {
      producerByOutRef.set(produced[LedgerColumns.OUTREF].toString("hex"), i);
    }
  }

  const nodes: CandidateNode[] = [];

  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(candidate.graph.spentOutRefHexes);
    const referenceOutRefs = new Set(candidate.graph.referenceOutRefHexes);
    const parents = new Set<number>();
    for (const spentOutRef of spentOutRefs) {
      const parent = producerByOutRef.get(spentOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }
    for (const referenceOutRef of referenceOutRefs) {
      const parent = producerByOutRef.get(referenceOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }

    nodes.push({
      index: i,
      candidate,
      spentOutRefs,
      referenceOutRefs,
      parents,
      children: new Set<number>(),
    });
  }

  for (const node of nodes) {
    for (const parent of node.parents) {
      nodes[parent].children.add(node.index);
    }
  }

  return nodes;
};

const findCycleNodes = (nodes: readonly CandidateNode[]): Set<number> => {
  const indegree = new Map<number, number>(
    nodes.map((node) => [node.index, node.parents.size]),
  );
  const queue: number[] = nodes
    .filter((node) => node.parents.size === 0)
    .map((node) => node.index);
  let visited = 0;

  while (queue.length > 0) {
    const index = queue.shift()!;
    visited += 1;
    for (const child of nodes[index].children) {
      const next = (indegree.get(child) ?? 0) - 1;
      indegree.set(child, next);
      if (next === 0) {
        queue.push(child);
      }
    }
  }

  if (visited === nodes.length) {
    return new Set<number>();
  }

  const cycleNodes = new Set<number>();
  for (const node of nodes) {
    if ((indegree.get(node.index) ?? 0) > 0) {
      cycleNodes.add(node.index);
    }
  }

  return cycleNodes;
};

const validateCandidateAgainstState = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  config: PhaseBConfig,
): Effect.Effect<CandidateDecision, Error> =>
  Effect.gen(function* () {
    const candidate = node.candidate;
    const { ledgerTx } = candidate;
    const fail = (
      code: RejectedTx["code"],
      detail: string | null = null,
      consensusPhase: MidgardValidationPhaseName = "resolveInputs",
    ) => ({
      index: node.index,
      accepted: false as const,
      rejection: reject(ledgerTx.txId, code, detail, consensusPhase),
    });

    if (
      ledgerTx.validityIntervalStart !== undefined &&
      config.nowCardanoSlotNo < ledgerTx.validityIntervalStart
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} < ${ledgerTx.validityIntervalStart}`,
      );
    }

    if (
      ledgerTx.validityIntervalEnd !== undefined &&
      config.nowCardanoSlotNo > ledgerTx.validityIntervalEnd
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} > ${ledgerTx.validityIntervalEnd}`,
      );
    }

    const inputValues: MidgardValue[] = [];
    let sawScriptInput = false;

    const witnessKeyHashes = new Set(candidate.derived.witnessKeyHashHexes);
    const inlineNativeScriptHashes = new Set(
      candidate.derived.nativeScriptHashHexes,
    );
    const inlinePlutusScriptHashes = new Set(
      candidate.derived.plutusScriptHashHexes,
    );
    const resolvedReferenceInputs = resolveReferenceInputs(node, stateValue);
    if ("code" in resolvedReferenceInputs) {
      return {
        index: node.index,
        accepted: false,
        rejection: resolvedReferenceInputs,
      };
    }

    const hasSatisfiedScriptMaterial = (
      scriptHash: string,
      context: string,
    ): CandidateDecision | true => {
      if (inlineNativeScriptHashes.has(scriptHash)) {
        return true;
      }

      if (
        inlinePlutusScriptHashes.has(scriptHash) ||
        resolvedReferenceInputs.scriptHashes.has(scriptHash)
      ) {
        return true;
      }

      return fail(
        RejectCodes.MissingRequiredWitness,
        `missing script witness ${scriptHash} for ${context}`,
        "scriptSources",
      );
    };

    for (const observerHash of candidate.derived.requiredObserverHashHexes) {
      const observerSatisfied = hasSatisfiedScriptMaterial(
        observerHash,
        `required observer ${observerHash}`,
      );
      if (observerSatisfied !== true) {
        return observerSatisfied;
      }
    }

    for (const mintPolicyHash of candidate.derived.mintPolicyHashHexes) {
      const mintSatisfied = hasSatisfiedScriptMaterial(
        mintPolicyHash,
        `mint policy ${mintPolicyHash}`,
      );
      if (mintSatisfied !== true) {
        return mintSatisfied;
      }
    }

    for (const inputOutRefHex of node.spentOutRefs) {
      if (spentByAccepted.has(inputOutRefHex)) {
        return fail(RejectCodes.DoubleSpend, inputOutRefHex);
      }

      const inputOutput = stateValue(inputOutRefHex);
      if (!inputOutput) {
        return fail(RejectCodes.InputNotFound, inputOutRefHex);
      }

      try {
        const output = decodeMidgardTxOutput(inputOutput);
        const paymentCred = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;

        if (paymentCred.kind === "PubKey") {
          const inputSigner = paymentCred.hash.toString("hex");
          if (!witnessKeyHashes.has(inputSigner)) {
            return fail(
              RejectCodes.MissingRequiredWitness,
              `missing witness for input signer ${inputSigner} (outref ${inputOutRefHex})`,
            );
          }
        } else {
          sawScriptInput = true;
          const inputScriptHash = paymentCred.hash.toString("hex");
          const inputScriptSatisfied = hasSatisfiedScriptMaterial(
            inputScriptHash,
            `outref ${inputOutRefHex}`,
          );
          if (inputScriptSatisfied !== true) {
            return inputScriptSatisfied;
          }
        }

        inputValues.push(output.value);
      } catch (e) {
        return fail(
          RejectCodes.InvalidOutput,
          `failed to decode input output: ${String(e)}`,
        );
      }
    }

    if (sawScriptInput || candidate.derived.requiresLocalScriptDiscovery) {
      const localScriptEvaluation = yield* runLocalScriptEvaluation(
        node,
        stateValue,
        resolvedReferenceInputs,
        witnessKeyHashes,
        config,
      );
      if (localScriptEvaluation.kind === "rejected") {
        return fail(
          localScriptEvaluation.code,
          localScriptEvaluation.detail,
          localScriptEvaluation.consensusPhase,
        );
      }
    }

    const delta = valuePreservationDelta(
      sumMidgardValues(inputValues),
      ledgerTx.fee,
      candidate.derived.mintDelta,
      candidate.derived.outputSum,
    );
    if (!isZeroValueDelta(delta)) {
      return fail(
        RejectCodes.ValueNotPreserved,
        `equation mismatch: inputs - fee + mint - outputs = ${describeValueDelta(delta)}`,
        "valueAndMint",
      );
    }

    return {
      index: node.index,
      accepted: true,
    };
  });

const validatePlainCandidateAgainstState = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  config: PhaseBConfig,
): CandidateDecision | undefined => {
  const candidate = node.candidate;
  if (candidate.derived.requiresLocalScriptDiscovery) return undefined;
  const { ledgerTx } = candidate;
  const fail = (
    code: RejectedTx["code"],
    detail: string | null = null,
    consensusPhase: MidgardValidationPhaseName = "resolveInputs",
  ) => ({
    index: node.index,
    accepted: false as const,
    rejection: reject(ledgerTx.txId, code, detail, consensusPhase),
  });

  if (
    ledgerTx.validityIntervalStart !== undefined &&
    config.nowCardanoSlotNo < ledgerTx.validityIntervalStart
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `${config.nowCardanoSlotNo} < ${ledgerTx.validityIntervalStart}`,
    );
  }
  if (
    ledgerTx.validityIntervalEnd !== undefined &&
    config.nowCardanoSlotNo > ledgerTx.validityIntervalEnd
  ) {
    return fail(
      RejectCodes.ValidityIntervalMismatch,
      `${config.nowCardanoSlotNo} > ${ledgerTx.validityIntervalEnd}`,
    );
  }

  const resolvedReferenceInputs = resolveReferenceInputs(node, stateValue);
  if ("code" in resolvedReferenceInputs) {
    return {
      index: node.index,
      accepted: false,
      rejection: resolvedReferenceInputs,
    };
  }

  const witnessKeyHashes = new Set(candidate.derived.witnessKeyHashHexes);
  const inputValues: MidgardValue[] = [];
  for (const inputOutRefHex of node.spentOutRefs) {
    if (spentByAccepted.has(inputOutRefHex)) {
      return fail(RejectCodes.DoubleSpend, inputOutRefHex);
    }
    const inputOutput = stateValue(inputOutRefHex);
    if (inputOutput === undefined) {
      return fail(RejectCodes.InputNotFound, inputOutRefHex);
    }
    try {
      const output = decodeMidgardTxOutput(inputOutput);
      const paymentCred = decodeMidgardAddressBytes(
        output.address,
      ).paymentCredential;
      if (paymentCred.kind === "Script") return undefined;
      const inputSigner = paymentCred.hash.toString("hex");
      if (!witnessKeyHashes.has(inputSigner)) {
        return fail(
          RejectCodes.MissingRequiredWitness,
          `missing witness for input signer ${inputSigner} (outref ${inputOutRefHex})`,
        );
      }
      inputValues.push(output.value);
    } catch (error) {
      return fail(
        RejectCodes.InvalidOutput,
        `failed to decode input output: ${String(error)}`,
      );
    }
  }

  const delta = valuePreservationDelta(
    sumMidgardValues(inputValues),
    ledgerTx.fee,
    candidate.derived.mintDelta,
    candidate.derived.outputSum,
  );
  if (!isZeroValueDelta(delta)) {
    return fail(
      RejectCodes.ValueNotPreserved,
      `equation mismatch: inputs - fee + mint - outputs = ${describeValueDelta(delta)}`,
      "valueAndMint",
    );
  }
  return { index: node.index, accepted: true };
};

const validatePlainComponentAgainstState = (
  component: readonly CandidateNode[],
  stateValue: (outRefHex: string) => Buffer | undefined,
  spentByAccepted: Set<string>,
  statusByIndex: readonly CandidateStatus[],
  config: PhaseBConfig,
): readonly CandidateDecision[] | undefined => {
  const componentSpent = new Set(spentByAccepted);
  const componentStateValue = (outRefHex: string): Buffer | undefined =>
    componentSpent.has(outRefHex) ? undefined : stateValue(outRefHex);
  const decisions: CandidateDecision[] = [];
  for (const node of component) {
    if (statusByIndex[node.index] !== "pending") continue;
    const decision = validatePlainCandidateAgainstState(
      node,
      componentStateValue,
      componentSpent,
      config,
    );
    if (decision === undefined) return undefined;
    decisions.push(decision);
    if (decision.accepted) {
      for (const outRef of node.spentOutRefs) componentSpent.add(outRef);
    }
  }
  return decisions;
};

const cascadeRejectDescendants = (
  nodes: readonly CandidateNode[],
  rejectedRoot: number,
  statusByIndex: CandidateStatus[],
  rejected: RejectedTx[],
): void => {
  const queue = [...nodes[rejectedRoot].children];

  while (queue.length > 0) {
    const child = queue.shift()!;
    if (statusByIndex[child] !== "pending") {
      continue;
    }

    statusByIndex[child] = "rejected";
    rejected.push(
      reject(
        nodes[child].candidate.ledgerTx.txId,
        RejectCodes.DependsOnRejectedTx,
        `depends on rejected tx ${nodes[rejectedRoot].candidate.ledgerTx.txId.toString("hex")}`,
      ),
    );

    queue.push(...nodes[child].children);
  }
};

export const runPhaseBValidationWithPatch = (
  phaseACandidates: readonly PhaseAValidatedTx[],
  preStateEntries: PreState,
  config: PhaseBConfig,
): Effect.Effect<PhaseBResultWithPatch, Error> =>
  Effect.gen(function* () {
    const accepted: PhaseAValidatedTx[] = [];
    const rejected: RejectedTx[] = [];
    const statePatch = makeEmptyStatePatch();

    if (phaseACandidates.length === 0) {
      return {
        accepted,
        rejected,
        statePatch: materializeStatePatch(statePatch),
      };
    }

    const nodes = buildNodes(phaseACandidates);
    const cycleNodes = findCycleNodes(nodes);

    const statusByIndex: CandidateStatus[] = Array.from(
      { length: nodes.length },
      () => "pending",
    );

    for (const cycleNode of cycleNodes) {
      statusByIndex[cycleNode] = "rejected";
      rejected.push(
        reject(
          nodes[cycleNode].candidate.ledgerTx.txId,
          RejectCodes.DependencyCycle,
          "transaction is part of a dependency cycle",
        ),
      );
    }

    const indegree = nodes.map(
      (node) =>
        Array.from(node.parents).filter(
          (parent) => statusByIndex[parent] === "pending",
        ).length,
    );

    const baseState = buildState(preStateEntries);
    const spentByAccepted = new Set<string>();
    const stateValue = (outRefHex: string): Buffer | undefined =>
      getStateValue(baseState, statePatch, outRefHex);

    const readyQueue: number[] = nodes
      .filter(
        (node) =>
          statusByIndex[node.index] === "pending" && indegree[node.index] === 0,
      )
      .map((node) => node.index);

    while (readyQueue.length > 0) {
      const readyIndices = readyQueue.splice(0, readyQueue.length);
      const readyNodes = readyIndices
        .map((index) => nodes[index])
        .filter((node) => statusByIndex[node.index] === "pending");

      if (readyNodes.length === 0) {
        continue;
      }

      const components = buildConflictComponents(readyNodes);
      const nextReady: number[] = [];
      const plainDecisions: CandidateDecision[][] = [];
      const effectfulComponents: CandidateNode[][] = [];
      for (const component of components) {
        const decisions = validatePlainComponentAgainstState(
          component,
          stateValue,
          spentByAccepted,
          statusByIndex,
          config,
        );
        if (decisions === undefined) effectfulComponents.push(component);
        else plainDecisions.push([...decisions]);
      }
      const decisionsByComponent = yield* Effect.forEach(
        effectfulComponents,
        (component) =>
          Effect.gen(function* () {
            const componentSpent = new Set(spentByAccepted);
            const componentStateValue = (
              outRefHex: string,
            ): Buffer | undefined =>
              componentSpent.has(outRefHex) ? undefined : stateValue(outRefHex);
            const decisions: CandidateDecision[] = [];
            for (const node of component) {
              if (statusByIndex[node.index] !== "pending") continue;
              const decision = yield* validateCandidateAgainstState(
                node,
                componentStateValue,
                componentSpent,
                config,
              );
              decisions.push(decision);
              if (decision.accepted) {
                for (const outRef of node.spentOutRefs) {
                  componentSpent.add(outRef);
                }
              }
            }
            return decisions;
          }),
        {
          concurrency:
            config.bucketConcurrency <= 0
              ? "unbounded"
              : config.bucketConcurrency,
        },
      );
      const decisions = [...plainDecisions, ...decisionsByComponent]
        .flat()
        .sort((left, right) => left.index - right.index);

      for (const decision of decisions) {
        const node = nodes[decision.index];
        if (statusByIndex[node.index] !== "pending") {
          continue;
        }

        if (!decision.accepted) {
          statusByIndex[node.index] = "rejected";
          if (decision.rejection !== undefined) {
            rejected.push(decision.rejection);
          }
          cascadeRejectDescendants(nodes, node.index, statusByIndex, rejected);
          continue;
        }

        statusByIndex[node.index] = "accepted";
        accepted.push(node.candidate);

        for (const outRefHex of node.candidate.graph.spentOutRefHexes) {
          spentByAccepted.add(outRefHex);
          statePatch.deletedOutRefs.add(outRefHex);
          statePatch.upsertedOutRefs.delete(outRefHex);
        }
        for (const produced of node.candidate.graph.produced) {
          const outRefHex = produced[LedgerColumns.OUTREF].toString("hex");
          statePatch.upsertedOutRefs.set(
            outRefHex,
            Buffer.from(produced[LedgerColumns.OUTPUT]),
          );
          statePatch.deletedOutRefs.delete(outRefHex);
        }

        for (const child of node.children) {
          if (statusByIndex[child] !== "pending") {
            continue;
          }
          indegree[child] = Math.max(indegree[child] - 1, 0);
          if (indegree[child] === 0) {
            nextReady.push(child);
          }
        }
      }

      if (nextReady.length > 0) {
        readyQueue.push(...nextReady);
      }
    }

    for (const node of nodes) {
      if (statusByIndex[node.index] === "pending") {
        statusByIndex[node.index] = "rejected";
        rejected.push(
          reject(
            node.candidate.ledgerTx.txId,
            RejectCodes.DependsOnRejectedTx,
            "dependency chain unresolved due to rejected ancestor",
          ),
        );
      }
    }

    accepted.sort((left, right) =>
      left.submission.arrivalSeq < right.submission.arrivalSeq
        ? -1
        : left.submission.arrivalSeq > right.submission.arrivalSeq
          ? 1
          : 0,
    );

    return {
      accepted,
      rejected,
      statePatch: materializeStatePatch(statePatch),
    };
  });
