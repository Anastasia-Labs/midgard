import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardAddressBytes,
  decodeMidgardNativeMint,
  decodeMidgardNativeTxFullFromCanonicalBinary,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxWitnessSetCompact,
  type MidgardNativeScript,
  type MidgardNativeTxFull,
  type MidgardTxOutput,
  midgardValueToCmlValue,
  type ScriptLanguageName,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import { evaluateScriptWithHarmonic } from "./local-script-eval.js";
import {
  decodeMidgardRedeemers,
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
  ScriptMintValue,
} from "./script-context.js";
import {
  ResolvedScriptSource,
  resolveScriptSource,
  ScriptSource,
  scriptSourceFromVersionedScript,
} from "./script-source.js";
import { sortTxOutRefHexes } from "./tx-out-ref.js";
import {
  PhaseAAccepted,
  PhaseBConfig,
  PhaseBResult,
  RejectCodes,
  RejectedTx,
} from "./types.js";

type UTxOState = Map<string, Buffer>;
type PreState = readonly LedgerEntry[] | UTxOState;

type CandidateNode = {
  readonly index: number;
  readonly candidate: PhaseAAccepted;
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
): RejectedTx => ({
  txId,
  code,
  detail,
});

const sumValues = (
  values: readonly InstanceType<typeof CML.Value>[],
): InstanceType<typeof CML.Value> => {
  let sum = CML.Value.zero();
  for (const value of values) {
    sum = sum.checked_add(value);
  }
  return sum;
};

const hasIntersection = (left: Set<string>, right: Set<string>): boolean => {
  const smaller = left.size <= right.size ? left : right;
  const larger = left.size <= right.size ? right : left;
  for (const entry of smaller) {
    if (larger.has(entry)) {
      return true;
    }
  }
  return false;
};

const decodeReferenceInputNativeScripts = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
):
  | {
      readonly nativeScripts: Map<string, MidgardNativeScript>;
      readonly nonNativeScriptHashes: Set<string>;
    }
  | RejectedTx => {
  const referenceScripts = new Map<string, MidgardNativeScript>();
  const nonNativeScriptHashes = new Set<string>();

  for (const referenceOutRefHex of node.referenceOutRefs) {
    const referenceOutput = stateValue(referenceOutRefHex);
    if (referenceOutput === undefined) {
      continue;
    }

    let output: MidgardTxOutput;
    try {
      output = decodeMidgardTxOutput(referenceOutput);
    } catch (e) {
      return reject(
        node.candidate.txId,
        RejectCodes.InvalidOutput,
        `failed to decode reference input output ${referenceOutRefHex}: ${String(e)}`,
      );
    }

    const scriptRef = output.script_ref;
    if (scriptRef === undefined) {
      continue;
    }

    const source = scriptSourceFromVersionedScript(
      scriptRef,
      "reference",
      referenceOutRefHex,
    );
    if (source.nativeScript === undefined) {
      nonNativeScriptHashes.add(source.scriptHash);
      continue;
    }

    referenceScripts.set(source.scriptHash, source.nativeScript);
  }

  return { nativeScripts: referenceScripts, nonNativeScriptHashes };
};

const conflict = (left: CandidateNode, right: CandidateNode): boolean =>
  hasIntersection(left.spentOutRefs, right.spentOutRefs) ||
  hasIntersection(left.spentOutRefs, right.referenceOutRefs) ||
  hasIntersection(right.spentOutRefs, left.referenceOutRefs);

const mintValueData = (
  decodedMint: ReturnType<typeof decodeMidgardNativeMint>,
): ScriptMintValue => {
  const result = new Map<string, Map<string, bigint>>();
  if (decodedMint === undefined) {
    return result;
  }

  const policyIds = decodedMint.mint.keys();
  for (let i = 0; i < policyIds.len(); i += 1) {
    const policyId = policyIds.get(i);
    const assetQuantities = decodedMint.mint.get_assets(policyId)!;
    const assets = assetQuantities.keys();
    const assetMap = new Map<string, bigint>();
    for (let j = 0; j < assets.len(); j += 1) {
      const assetName = assets.get(j);
      assetMap.set(
        Buffer.from(assetName.to_raw_bytes()).toString("hex"),
        assetQuantities.get(assetName)!,
      );
    }
    result.set(policyId.to_hex(), assetMap);
  }

  return result;
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
    };

const collectInlineScriptSources = (
  nativeTx: MidgardNativeTxFull,
): readonly ScriptSource[] =>
  nativeTx.witnessSet.scriptTxWits.map((script, index) =>
    scriptSourceFromVersionedScript(script, "inline", `script_wit:${index}`),
  );

const collectReferenceScriptSources = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
): readonly ScriptSource[] => {
  const sources: ScriptSource[] = [];
  for (const referenceOutRefHex of node.referenceOutRefs) {
    const outputBytes = stateValue(referenceOutRefHex);
    if (outputBytes === undefined) {
      continue;
    }
    const output = decodeMidgardTxOutput(outputBytes);
    const scriptRef = output.script_ref;
    if (scriptRef !== undefined) {
      sources.push(
        scriptSourceFromVersionedScript(
          scriptRef,
          "reference",
          referenceOutRefHex,
        ),
      );
    }
  }
  return sources;
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
  nativeTx: MidgardNativeTxFull,
  stateValue: (outRefHex: string) => Buffer | undefined,
  sources: readonly ScriptSource[],
  witnessKeyHashes: ReadonlySet<string>,
):
  | Extract<LocalScriptValidationResult, { readonly kind: "rejected" }>
  | {
      readonly kind: "discovered";
      readonly executions: readonly RequiredScriptExecution[];
      readonly contextView: ScriptContextView;
    } => {
  const candidate = node.candidate;
  const redeemers = decodeMidgardRedeemers(
    nativeTx.witnessSet.redeemerTxWits,
  );
  const seenRedeemerPointers = new Set<string>();
  for (const redeemer of redeemers) {
    const key = midgardRedeemerPointerKey(redeemer);
    if (seenRedeemerPointers.has(key)) {
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `duplicate redeemer ${key}`,
      };
    }
    seenRedeemerPointers.add(key);
  }

  const sortedSpent = sortTxOutRefHexes(node.spentOutRefs);
  const sortedReferenceInputs = sortTxOutRefHexes(node.referenceOutRefs);
  const resolvedInputs: {
    readonly outRefHex: string;
    readonly output: MidgardTxOutput;
  }[] = [];
  const resolvedReferenceInputs: {
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
      };
    }
    if (resolved.version !== "NativeCardano") {
      const redeemer = findRedeemerByPointer(redeemers, pointer);
      if (redeemer === undefined) {
        return {
          kind: "rejected",
          code: RejectCodes.MissingRequiredWitness,
          detail: `missing redeemer for ${purpose.kind} ${purpose.scriptHash}`,
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

  for (const outRefHex of sortedReferenceInputs) {
    const outputBytes = stateValue(outRefHex);
    if (outputBytes !== undefined) {
      const output = decodeMidgardTxOutput(outputBytes);
      resolvedReferenceInputs.push({ outRefHex, output });
    }
  }

  const decodedMint = decodeMidgardNativeMint(nativeTx.body.mint);
  const mintValue = mintValueData(decodedMint);
  const mintPolicies = candidate.mintPolicyHashes;
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

  const observers = [...candidate.requiredObserverHashes].sort();
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

  const outputs = nativeTx.body.outputs;
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
      };
    }
  }

  return {
    kind: "discovered",
    executions,
    contextView: {
      txId: candidate.txId,
      inputs: resolvedInputs,
      referenceInputs: resolvedReferenceInputs,
      outputs,
      fee: candidate.fee,
      validityIntervalStart: candidate.validityIntervalStart,
      validityIntervalEnd: candidate.validityIntervalEnd,
      observers,
      signatories: candidate.witnessKeyHashes,
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
  witnessKeyHashes: ReadonlySet<string>,
  enforceScriptBudget: boolean,
): LocalScriptValidationResult => {
  const candidate = node.candidate;
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalBinary(candidate.txCbor);
  const inlineSources = collectInlineScriptSources(nativeTx);
  const referenceSources = collectReferenceScriptSources(node, stateValue);
  const sources = [...inlineSources, ...referenceSources];
  const discovered = discoverLocalScriptExecutions(
    node,
    nativeTx,
    stateValue,
    sources,
    witnessKeyHashes,
  );
  if (discovered.kind === "rejected") {
    return discovered;
  }

  const usedInlineSourceIds = new Set(
    discovered.executions
      .filter((execution) => execution.resolved.source.origin === "inline")
      .map((execution) => execution.resolved.source.sourceId),
  );
  for (const source of inlineSources) {
    if (!usedInlineSourceIds.has(source.sourceId)) {
      const kind = source.nativeScript === undefined ? "non-native" : "native";
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `extraneous ${kind} script witness ${source.sourceId}`,
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
        validityIntervalStart: candidate.validityIntervalStart,
        validityIntervalEnd: candidate.validityIntervalEnd,
        witnessSigners: witnessKeyHashes,
      })
    ) {
      return {
        kind: "rejected",
        code: RejectCodes.NativeScriptInvalid,
        detail: `native script verification failed for ${execution.purpose.kind} ${execution.purpose.scriptHash}`,
      };
    }
  }

  const nonNativeExecutions = discovered.executions.filter(
    (execution) => execution.resolved.version !== "NativeCardano",
  );

  const languages = requiredScriptLanguages(discovered.executions);
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(
    nativeTx.witnessSet,
  );
  const expectedScriptIntegrityHash = computeScriptIntegrityHashForLanguages(
    witnessCompact.redeemerTxWitsHash,
    languages,
  );
  if (!nativeTx.body.scriptIntegrityHash.equals(expectedScriptIntegrityHash)) {
    const expectedHex = expectedScriptIntegrityHash.toString("hex");
    const actualHex = nativeTx.body.scriptIntegrityHash.toString("hex");
    return {
      kind: "rejected",
      code: RejectCodes.InvalidFieldType,
      detail: `script_integrity_hash mismatch: expected ${expectedHex} actual ${actualHex} required_languages=${languages.join(",")}`,
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
    const result = evaluateScriptWithHarmonic(
      execution.resolved.source.scriptBytes,
      context,
    );
    if (result.kind === "script_invalid") {
      return {
        kind: "rejected",
        code: RejectCodes.PlutusScriptInvalid,
        detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: ${result.detail}`,
      };
    }
    if (
      enforceScriptBudget &&
      (result.budget.cpu > redeemer.exUnits.steps ||
        result.budget.memory > redeemer.exUnits.memory)
    ) {
      return {
        kind: "rejected",
        code: RejectCodes.PlutusScriptInvalid,
        detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: budget exceeded (spent mem=${result.budget.memory} cpu=${result.budget.cpu}, declared mem=${redeemer.exUnits.memory} cpu=${redeemer.exUnits.steps})`,
      };
    }
  }

  return { kind: "accepted" };
};

const buildConflictBuckets = (
  readyNodes: readonly CandidateNode[],
): CandidateNode[][] => {
  const buckets: CandidateNode[][] = [];

  for (const node of readyNodes) {
    const bucket = buckets.find(
      (candidateBucket) =>
        !candidateBucket.some((bucketNode) => conflict(node, bucketNode)),
    );
    if (bucket === undefined) {
      buckets.push([node]);
    } else {
      bucket.push(node);
    }
  }

  return buckets;
};

const buildNodes = (
  candidates: readonly PhaseAAccepted[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();

  for (let i = 0; i < candidates.length; i++) {
    for (const produced of candidates[i].processedTx.produced) {
      producerByOutRef.set(produced[LedgerColumns.OUTREF].toString("hex"), i);
    }
  }

  const nodes: CandidateNode[] = [];

  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(
      candidate.processedTx.spent.map((outRef) => outRef.toString("hex")),
    );
    const referenceOutRefs = new Set(
      candidate.referenceInputs.map((outRef) => outRef.toString("hex")),
    );
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
): Effect.Effect<CandidateDecision> =>
  Effect.gen(function* () {
    const candidate = node.candidate;
    const fail = (code: RejectedTx["code"], detail: string | null = null) => ({
      index: node.index,
      accepted: false as const,
      rejection: reject(candidate.txId, code, detail),
    });

    if (
      candidate.validityIntervalStart !== undefined &&
      config.nowCardanoSlotNo < candidate.validityIntervalStart
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} < ${candidate.validityIntervalStart}`,
      );
    }

    if (
      candidate.validityIntervalEnd !== undefined &&
      config.nowCardanoSlotNo > candidate.validityIntervalEnd
    ) {
      return fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowCardanoSlotNo} > ${candidate.validityIntervalEnd}`,
      );
    }

    const inputValues: InstanceType<typeof CML.Value>[] = [];

    for (const referenceOutRefHex of node.referenceOutRefs) {
      if (stateValue(referenceOutRefHex) === undefined) {
        return fail(
          RejectCodes.InputNotFound,
          `reference input not found: ${referenceOutRefHex}`,
        );
      }
    }

    const witnessKeyHashes = new Set(candidate.witnessKeyHashes);
    const inlineNativeScriptHashes = new Set(candidate.nativeScriptHashes);
    const inlinePlutusScriptHashes = new Set(candidate.plutusScriptHashes);
    const referenceNativeScriptsResult = decodeReferenceInputNativeScripts(
      node,
      stateValue,
    );
    if ("code" in referenceNativeScriptsResult) {
      return {
        index: node.index,
        accepted: false,
        rejection: referenceNativeScriptsResult,
      };
    }
    const { nativeScripts: referenceNativeScripts, nonNativeScriptHashes } =
      referenceNativeScriptsResult;

    const hasSatisfiedScriptMaterial = (
      scriptHash: string,
      context: string,
    ): CandidateDecision | true => {
      if (inlineNativeScriptHashes.has(scriptHash)) {
        return true;
      }

      const referenceNativeScript = referenceNativeScripts.get(scriptHash);
      if (referenceNativeScript !== undefined) {
        if (
          !verifyMidgardNativeScript(referenceNativeScript, {
            validityIntervalStart: candidate.validityIntervalStart,
            validityIntervalEnd: candidate.validityIntervalEnd,
            witnessSigners: witnessKeyHashes,
          })
        ) {
          return fail(
            RejectCodes.NativeScriptInvalid,
            `reference native script verification failed for ${context}: ${scriptHash}`,
          );
        }
        return true;
      }

      if (
        inlinePlutusScriptHashes.has(scriptHash) ||
        nonNativeScriptHashes.has(scriptHash)
      ) {
        return true;
      }

      return fail(
        RejectCodes.MissingRequiredWitness,
        `missing script witness ${scriptHash} for ${context}`,
      );
    };

    for (const observerHash of candidate.requiredObserverHashes) {
      const observerSatisfied = hasSatisfiedScriptMaterial(
        observerHash,
        `required observer ${observerHash}`,
      );
      if (observerSatisfied !== true) {
        return observerSatisfied;
      }
    }

    for (const mintPolicyHash of candidate.mintPolicyHashes) {
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
          const inputScriptHash = paymentCred.hash.toString("hex");
          const inputScriptSatisfied = hasSatisfiedScriptMaterial(
            inputScriptHash,
            `outref ${inputOutRefHex}`,
          );
          if (inputScriptSatisfied !== true) {
            return inputScriptSatisfied;
          }
        }

        inputValues.push(midgardValueToCmlValue(output.value));
      } catch (e) {
        return fail(
          RejectCodes.InvalidOutput,
          `failed to decode input output: ${String(e)}`,
        );
      }
    }

    const localScriptEvaluation = runLocalScriptEvaluation(
      node,
      stateValue,
      witnessKeyHashes,
      config.enforceScriptBudget !== false,
    );
    if (localScriptEvaluation.kind === "rejected") {
      return fail(localScriptEvaluation.code, localScriptEvaluation.detail);
    }

    try {
      const inputSum = sumValues(inputValues);
      const outputSum = candidate.outputSum;
      const lhs = inputSum
        .checked_sub(CML.Value.from_coin(candidate.fee))
        .checked_add(candidate.mintedValue)
        .checked_sub(candidate.burnedValue);
      const delta = lhs.checked_sub(outputSum);

      if (!delta.is_zero()) {
        return fail(
          RejectCodes.ValueNotPreserved,
          `equation mismatch: (inputs - fee + minted - burned) - outputs = { coin: ${delta.coin()}, has_multiassets: ${delta.has_multiassets()} }`,
        );
      }
    } catch (e) {
      return fail(RejectCodes.ValueNotPreserved, String(e));
    }

    return {
      index: node.index,
      accepted: true,
    };
  });

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
        nodes[child].candidate.txId,
        RejectCodes.DependsOnRejectedTx,
        `depends on rejected tx ${nodes[rejectedRoot].candidate.txId.toString("hex")}`,
      ),
    );

    queue.push(...nodes[child].children);
  }
};

export const runPhaseBValidationWithPatch = (
  phaseACandidates: readonly PhaseAAccepted[],
  preStateEntries: PreState,
  config: PhaseBConfig,
): Effect.Effect<PhaseBResultWithPatch> =>
  Effect.gen(function* () {
    const accepted: PhaseAAccepted[] = [];
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
          nodes[cycleNode].candidate.txId,
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

      const buckets = buildConflictBuckets(readyNodes);
      const nextReady: number[] = [];
      for (const bucket of buckets) {
        const pendingBucket = bucket.filter(
          (node) => statusByIndex[node.index] === "pending",
        );
        if (pendingBucket.length === 0) {
          continue;
        }

        const decisions = yield* Effect.forEach(
          pendingBucket,
          (node) =>
            validateCandidateAgainstState(
              node,
              stateValue,
              spentByAccepted,
              config,
            ),
          {
            concurrency:
              config.bucketConcurrency <= 0
                ? "unbounded"
                : config.bucketConcurrency,
          },
        );

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
            cascadeRejectDescendants(
              nodes,
              node.index,
              statusByIndex,
              rejected,
            );
            continue;
          }

          statusByIndex[node.index] = "accepted";
          accepted.push(node.candidate);

          for (const inputOutRef of node.candidate.processedTx.spent) {
            const outRefHex = inputOutRef.toString("hex");
            spentByAccepted.add(outRefHex);
            statePatch.deletedOutRefs.add(outRefHex);
            statePatch.upsertedOutRefs.delete(outRefHex);
          }
          for (const produced of node.candidate.processedTx.produced) {
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
            node.candidate.txId,
            RejectCodes.DependsOnRejectedTx,
            "dependency chain unresolved due to rejected ancestor",
          ),
        );
      }
    }

    accepted.sort((left, right) =>
      left.arrivalSeq < right.arrivalSeq
        ? -1
        : left.arrivalSeq > right.arrivalSeq
          ? 1
          : 0,
    );

    return {
      accepted,
      rejected,
      statePatch: materializeStatePatch(statePatch),
    };
  });
