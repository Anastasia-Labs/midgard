/**
 * The CEK context's list steps (`cek-context-step.ak`): the resolved inputs,
 * outputs and signers, each folded downwards into its list, then the mint.
 * Also the per-execution run every context step pushes through.
 */
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  decodeMidgardLedgerOutputCommitment,
  hashMidgardMintAssetLeaf,
} from "@al-ft/midgard-core";

import {
  encodeMidgardCekValidationWitness,
  type MidgardCekContextControl,
  summarizeMidgardCekContextParts,
  summarizeMidgardCekLucidData,
} from "../cek-context.js";
import { runMidgardCekMintFold } from "../cek-context.mint-item-step.js";
import {
  emptyMidgardCekDataListSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekSmallConstrData,
} from "../script-context-proof.js";
import {
  preparedSignerMembership,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import type {
  ScriptExecutionProofEntry,
  ValidationTraceWalkState,
} from "./trace-builder-types.js";
import {
  exactDescriptorSummary,
  outRefSummary,
  sameSequence,
  sameSummary,
} from "./trace-descriptor-summary.js";
import { type ValidationMachineWorkWitness } from "./types.js";

/** What the CEK phase reads of the prepared transaction. */
export type CekPrepared = Pick<
  PreparedValidationTransaction,
  | "resolutionScheduleNodes"
  | "ledgerDescriptorState"
  | "decodedProofRedeemers"
  | "input"
  | "requiredObserversCollection"
  | "phaseALedgerTx"
  | "signerFrontier"
  | "canonicalSignerHashes"
  | "signerLeafHashes"
  | "redeemerLeafHashes"
  | "redeemerWitnessesCollection"
  | "scriptEvaluations"
  | "fieldPreimages"
>;

/** What the CEK phase reads of the walk state. */
export type CekState = Pick<
  ValidationTraceWalkState,
  | "scriptExecutionEntries"
  | "scriptPurposeEntries"
  | "scriptSourceEntries"
  | "admittedOutputDescriptorCbors"
  | "admittedOutputDescriptorLeafHashes"
  | "mintFoldControl"
>;

/** Where a CEK witness sits: its execution and the budget spent before it. */
export type CekWitnessAt = {
  readonly nativeControlCbor: Buffer;
  readonly executionCursor: number;
  readonly completedCpu: bigint;
  readonly completedMemory: bigint;
};

/** The CEK witness between executions, on a context step at
 * `contextControl`, or on a core step of the execution it built. */
export const cekWitnessCbor = (
  at: CekWitnessAt,
  contextControl: MidgardCekContextControl | null = null,
  core?: {
    readonly activeStateHash: Uint8Array;
    readonly exUnits: { readonly steps: bigint; readonly memory: bigint };
  },
): Buffer =>
  encodeMidgardCekValidationWitness({
    ...at,
    contextControl: core === undefined ? contextControl : null,
    activeStateHash: core?.activeStateHash ?? null,
    executionCpuLimit: core?.exUnits.steps ?? 0n,
    executionMemoryLimit: core?.exUnits.memory ?? 0n,
    programScriptHash: contextControl?.scriptHash ?? null,
    purposeKind: core === undefined ? -1n : BigInt(contextControl!.purposeKind),
    purposeIndex: core === undefined ? -1n : contextControl!.purposeIndex,
  });

/** One execution's context build: what its steps read, and the push of a
 * context witness at a control. */
export type CekContextRun = {
  readonly prepared: CekPrepared;
  readonly state: CekState;
  readonly execution: ScriptExecutionProofEntry;
  readonly languageTag: 3 | 128;
  readonly parts: ReturnType<typeof summarizeMidgardCekContextParts>;
  readonly resolvedLeaves: readonly Buffer[];
  readonly at: (
    control: MidgardCekContextControl,
    auxiliary?: ValidationMachineWorkWitness["auxiliary"],
  ) => void;
};

type ContextList =
  | "referenceItems"
  | "spendItems"
  | "outputItems"
  | "signerItems";

/** Folds `items` downwards into `list`, then closes it at `stage`. */
const foldContextList = (
  run: CekContextRun,
  start: MidgardCekContextControl,
  list: ContextList,
  stage: number,
  name: string,
  items: Iterable<
    readonly [
      ValidationMachineWorkWitness["auxiliary"],
      () => Parameters<typeof prependMidgardCekDataListSummary>[0],
    ]
  >,
): MidgardCekContextControl => {
  let control = start;
  for (const [auxiliary, summary] of items) {
    run.at(control, auxiliary);
    control = {
      ...control,
      [list]: prependMidgardCekDataListSummary(summary(), control[list]),
    };
  }
  run.at(control);
  control = { ...control, stage };
  if (!sameSequence(control[list], run.parts[list])) {
    throw new Error(`CEK ${name} context differs from the evaluated context`);
  }
  return control;
};

const txOut = (
  run: CekContextRun,
  descriptor: ReturnType<typeof decodeMidgardLedgerOutputCommitment>,
) =>
  run.languageTag === 128 ? descriptor.midgardTxOut : descriptor.cardanoTxOut;

/** The resolved inputs' TxInInfo items: references from stage 1 to 2, then
 * spends from 2 to 3 (`next_resolved`). */
export const resolvedContextItems = (
  run: CekContextRun,
  start: MidgardCekContextControl,
): MidgardCekContextControl => {
  const nodes = run.prepared.resolutionScheduleNodes;
  const spendCount = nodes.filter((node) => node.sourceKind === "spend").length;
  let control = start;
  for (const [sourceKind, from, to, list, stage] of [
    ["reference", nodes.length - 1, spendCount, "referenceItems", 2],
    ["spend", spendCount - 1, 0, "spendItems", 3],
  ] as const) {
    control = foldContextList(
      run,
      control,
      list,
      stage,
      `${sourceKind}-input`,
      (function* () {
        for (let itemIndex = from; itemIndex >= to; itemIndex -= 1) {
          const node = nodes[itemIndex]!;
          const descriptorCbor = run.prepared.ledgerDescriptorState.get(
            node.key.toString("hex"),
          );
          if (descriptorCbor === undefined) {
            throw new Error(
              `CEK ${sourceKind}-input context lost its authenticated ledger descriptor`,
            );
          }
          const descriptor =
            decodeMidgardLedgerOutputCommitment(descriptorCbor);
          yield [
            {
              kind: "cekResolvedContextItem",
              sourceKind,
              itemIndex,
              key: node.key,
              descriptorCbor,
              siblings: buildMidgardValidationMerkleMembership(
                run.resolvedLeaves,
                itemIndex,
              ).siblings,
            },
            () =>
              summarizeMidgardCekSmallConstrData(
                0n,
                prependMidgardCekDataListSummary(
                  outRefSummary(node.key),
                  prependMidgardCekDataListSummary(
                    exactDescriptorSummary(txOut(run, descriptor)),
                    emptyMidgardCekDataListSummary(),
                  ),
                ),
              ),
          ] as const;
        }
      })(),
    );
  }
  return control;
};

/** The outputs' TxOut items, stage 3 to 4 (`next_output`). */
export const outputContextItems = (
  run: CekContextRun,
  control: MidgardCekContextControl,
): MidgardCekContextControl => {
  const cbors = run.state.admittedOutputDescriptorCbors;
  return foldContextList(
    run,
    control,
    "outputItems",
    4,
    "output",
    (function* () {
      for (
        let outputIndex = cbors.length - 1;
        outputIndex >= 0;
        outputIndex -= 1
      ) {
        const descriptorCbor = cbors[outputIndex];
        if (descriptorCbor === undefined) {
          throw new Error(
            "CEK output context lost its authenticated output descriptor",
          );
        }
        const descriptor = decodeMidgardLedgerOutputCommitment(descriptorCbor);
        yield [
          {
            kind: "cekOutputContextItem",
            outputIndex,
            descriptorCbor,
            siblings: buildMidgardValidationMerkleMembership(
              run.state.admittedOutputDescriptorLeafHashes,
              outputIndex,
            ).siblings,
          },
          () => exactDescriptorSummary(txOut(run, descriptor)),
        ] as const;
      }
    })(),
  );
};

/** The signer items, stage 4 to 5 (`next_signer`). */
export const signerContextItems = (
  run: CekContextRun,
  control: MidgardCekContextControl,
): MidgardCekContextControl => {
  const { canonicalSignerHashes: hashes, signerFrontier } = run.prepared;
  return foldContextList(
    run,
    control,
    "signerItems",
    5,
    "signer",
    (function* () {
      for (
        let signerIndex = hashes.length - 1;
        signerIndex >= 0;
        signerIndex -= 1
      ) {
        const signerHash = hashes[signerIndex]!;
        yield [
          {
            kind: "cekSignerContextItem",
            frontier: signerFrontier,
            signerIndex,
            signerHash,
            siblings: preparedSignerMembership(run.prepared, signerIndex)
              .siblings,
          },
          () => summarizeMidgardCekLucidData(signerHash.toString("hex")),
        ] as const;
      }
    })(),
  );
};

/** The mint, stage 6 to 9 (`next_mint_init`, `next_mint_item`): folded
 * over the NativeScripts mint frontier, or skipped when it is empty. */
export const mintContextItems = (
  run: CekContextRun,
  start: MidgardCekContextControl,
): MidgardCekContextControl => {
  run.at(start);
  const assets = [...run.prepared.phaseALedgerTx!.mint.assets].map((asset) => ({
    policyId: Buffer.from(asset.policyId),
    assetName: Buffer.from(asset.assetName),
    quantity: asset.quantity,
  }));
  const leaves = assets.map((asset) => hashMidgardMintAssetLeaf(asset));
  if (
    !commitMidgardValidationMerkleFrontier(
      buildMidgardValidationMerkleFrontier(leaves),
    ).equals(
      commitMidgardValidationMerkleFrontier(
        run.state.mintFoldControl.assetFrontier,
      ),
    )
  ) {
    throw new Error(
      "CEK mint context does not match the authenticated NativeScripts mint frontier",
    );
  }
  const control =
    assets.length === 0
      ? { ...start, stage: 9, mintSummary: run.parts.mint }
      : runMidgardCekMintFold(
          { ...start, stage: 8 },
          assets,
          (pre, mintIndex) => {
            const asset = assets[mintIndex]!;
            run.at(pre, {
              kind: "cekMintContextItem",
              policyId: asset.policyId,
              assetName: asset.assetName,
              quantity: asset.quantity,
              siblings: buildMidgardValidationMerkleMembership(
                leaves,
                mintIndex,
              ).siblings,
            });
          },
          (pre) => run.at(pre),
        );
  if (!sameSummary(control.mintSummary, run.parts.mint)) {
    throw new Error("CEK mint context differs from the evaluated context");
  }
  return control;
};
