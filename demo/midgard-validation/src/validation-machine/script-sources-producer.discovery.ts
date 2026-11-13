/**
 * ScriptSources stages 8 to 10: each purpose matched to its script source
 * and, unless the source is native, to the redeemer that points at it,
 * each match folded into the execution frontier.
 */
import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationMerkleMembership,
  hashMidgardRedeemerItemProofControl,
  hashMidgardScriptExecutionLeaf,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  midgardScriptSourcePhysicalIndex,
} from "@al-ft/midgard-core";

import type { ScriptDiscoveryTraceControl } from "./control-encoding.js";
import {
  emptyScriptDiscoveryControl,
  type ResolvedInputReplay,
  type ScriptSourcesPrepared,
  scriptSourcesWitnessCbor,
} from "./prepare-producers.shared.js";
import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import { redeemerPointerMatchesPurpose } from "./redeemer-purpose.js";
import type { OutputAdmission } from "./script-sources-producer.outputs.js";
import type { ScriptPurposes } from "./script-sources-producer.purposes.js";
import type {
  ScriptExecutionProofEntry,
  ScriptPurposeProofEntry,
  ScriptSourceProofEntry,
} from "./trace-builder-types.js";
import { type PhaseOutcome, phaseStops, type TraceWalk } from "./trace-walk.js";

export type DiscoveryPrepared = ScriptSourcesPrepared &
  Pick<
    PreparedValidationTransaction,
    | "outputFrontier"
    | "redeemerFrontier"
    | "redeemerLeafHashes"
    | "decodedProofRedeemers"
    | "redeemerWitnessesCollection"
  >;

type PushAuxiliary = Parameters<TraceWalk["push"]>[2];

/** What stages 8 to 12 share: their folds, and the scans they push. */
export type DiscoveryRun = {
  readonly prepared: DiscoveryPrepared;
  readonly push: TraceWalk["push"];
  readonly replay: ResolvedInputReplay;
  readonly purposes: Pick<
    ScriptPurposes,
    "mintFoldControl" | "purposeFrontier"
  >;
  /** The inline, then reference sources, in proof order. */
  readonly sourceEntries: readonly ScriptSourceProofEntry[];
  /** The witness at `stage` under `discovery`. */
  readonly witness: (
    stage: number,
    discovery: ScriptDiscoveryTraceControl,
  ) => Buffer;
  readonly sourceScan: (sourceIndex: number) => PushAuxiliary;
  readonly redeemerScanBegin: (redeemerIndex: number) => PushAuxiliary;
  readonly redeemerItemTrace: (
    redeemerIndex: number,
  ) => ReturnType<typeof buildMidgardRedeemerItemProofTrace>;
};

export const discoveryRun = (
  prepared: DiscoveryPrepared,
  push: TraceWalk["push"],
  replay: ResolvedInputReplay,
  admission: Pick<OutputAdmission, "receiveScan">,
  purposes: Pick<ScriptPurposes, "mintFoldControl" | "purposeFrontier">,
  sourceEntries: readonly ScriptSourceProofEntry[],
): DiscoveryRun => {
  const sourceLeaves = sourceEntries.map((entry) => entry.leaf);
  const redeemerCount = prepared.decodedProofRedeemers.length;
  const redeemerItem = (redeemerIndex: number) =>
    prepared.redeemerWitnessesCollection.items[redeemerIndex]!;
  return {
    prepared,
    push,
    replay,
    purposes,
    sourceEntries,
    witness: (stage, discovery) =>
      scriptSourcesWitnessCbor(prepared, {
        ...replay,
        stage,
        purposeFrontier: purposes.purposeFrontier,
        outputCursor: prepared.outputFrontier.count,
        outputFrontier: prepared.outputFrontier,
        // The admitted descriptors stay; the receive sources are spent.
        receiveScan: {
          ...admission.receiveScan,
          sourceFrontier: prepared.emptyValidationFrontier,
        },
        mintFoldControl: purposes.mintFoldControl,
        discovery,
      }),
    sourceScan: (sourceIndex) => {
      const source = sourceEntries[sourceIndex]!;
      return {
        kind: "scriptSourceScan",
        sourceIndex,
        originKind: source.originKind,
        sourceKey: source.sourceKey,
        scriptLanguageTag: source.scriptLanguageTag,
        scriptHash: source.scriptHash,
        scriptTotalLength: source.scriptTotalLength,
        scriptItemCommitment: source.scriptItemCommitment,
        siblings: buildMidgardValidationMerkleMembership(
          sourceLeaves,
          sourceIndex,
        ).siblings,
      };
    },
    redeemerScanBegin: (redeemerIndex) => {
      const item = redeemerItem(redeemerIndex);
      return {
        kind: "redeemerScanBegin",
        itemIndex: redeemerIndex,
        itemCount: redeemerCount,
        totalLength: item.bytes.length,
        itemCommitment: item.commitment,
        siblings: buildMidgardValidationMerkleMembership(
          prepared.redeemerLeafHashes,
          redeemerIndex,
        ).siblings,
      };
    },
    redeemerItemTrace: (redeemerIndex) =>
      buildMidgardRedeemerItemProofTrace({
        itemIndex: redeemerIndex,
        itemCount: redeemerCount,
        itemBytes: redeemerItem(redeemerIndex).bytes,
        mode: MidgardRedeemerItemProofModes.Descriptor,
      }),
  };
};

export const setDiscoveryBit = (bitmap: bigint, index: number): bigint =>
  bitmap | (1n << BigInt(index));

/** The discovery control with no purpose in hand. */
export const resetCurrent = (
  discovery: ScriptDiscoveryTraceControl,
): ScriptDiscoveryTraceControl => ({
  ...discovery,
  sourceCursor: 0,
  redeemerCursor: 0,
  currentPurposeKind: -1,
  currentPurposeIndex: -1n,
  currentScriptHash: Buffer.alloc(0),
  currentSubject: Buffer.alloc(0),
  matchedSourceIndex: -1,
  matchedLanguageTag: -1,
  matchedSourceLeaf: Buffer.alloc(0),
  redeemerItemControlHash: Buffer.alloc(0),
});

/** What stage 10 leaves for the audit. */
export type ScriptDiscovery = {
  readonly kind: "discovered";
  readonly discovery: ScriptDiscoveryTraceControl;
  readonly executionEntries: readonly ScriptExecutionProofEntry[];
};

type MatchedSource = {
  readonly entry: ScriptSourceProofEntry;
  readonly sourceIndex: number;
  readonly languageTag: 0 | 3 | 128;
};

/**
 * Stages 8 to 10 over every purpose in `purposeEntries`. It stops on the
 * first purpose with no source or no redeemer.
 */
export const produceScriptDiscovery = (
  run: DiscoveryRun,
  purposeEntries: readonly ScriptPurposeProofEntry[],
): PhaseOutcome | ScriptDiscovery => {
  const { prepared, push, witness, sourceEntries } = run;
  const purposeLeaves = purposeEntries.map((entry) => entry.leaf);
  const executionEntries: ScriptExecutionProofEntry[] = [];
  let discovery = emptyScriptDiscoveryControl(prepared.emptyValidationFrontier);
  for (
    let purposeCursor = 0;
    purposeCursor < purposeEntries.length;
    purposeCursor += 1
  ) {
    const purpose = purposeEntries[purposeCursor]!;
    push("scriptSources", witness(8, discovery), {
      kind: "scriptPurposeScan",
      purposeKind: purpose.purposeKind,
      purposeIndex: purpose.purposeIndex,
      scriptHash: purpose.scriptHash,
      subject: purpose.subject,
      siblings: buildMidgardValidationMerkleMembership(
        purposeLeaves,
        purposeCursor,
      ).siblings,
    });
    discovery = {
      ...discovery,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: purpose.purposeKind,
      currentPurposeIndex: purpose.purposeIndex,
      currentScriptHash: purpose.scriptHash,
      currentSubject: purpose.subject,
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
    };

    let matchedSource: MatchedSource | undefined;
    for (
      let sourceCursor = 0;
      sourceCursor < sourceEntries.length;
      sourceCursor += 1
    ) {
      const sourceIndex = midgardScriptSourcePhysicalIndex(
        prepared.inlineScriptSourceFrontier.count,
        sourceEntries.length,
        sourceCursor,
      );
      const source = sourceEntries[sourceIndex]!;
      push("scriptSources", witness(9, discovery), run.sourceScan(sourceIndex));
      discovery = { ...discovery, sourceCursor: sourceCursor + 1 };
      if (source.scriptHash.equals(purpose.scriptHash)) {
        const languageTag = source.scriptLanguageTag;
        discovery = {
          ...discovery,
          matchedSourceIndex: sourceIndex,
          matchedLanguageTag: languageTag,
          matchedSourceLeaf: source.leaf,
          usedInlineBitmap:
            source.originKind === "inline"
              ? setDiscoveryBit(discovery.usedInlineBitmap, sourceIndex)
              : discovery.usedInlineBitmap,
        };
        matchedSource = { entry: source, sourceIndex, languageTag };
        break;
      }
    }
    if (matchedSource === undefined) {
      push("scriptSources", witness(9, discovery));
      return phaseStops(
        {
          ScriptSourceMissing: {
            purpose_kind: BigInt(discovery.currentPurposeKind),
            purpose_index: BigInt(discovery.currentPurposeIndex),
          },
        },
        "V1 source scan found no source for the purpose",
      );
    }

    if (matchedSource.languageTag === 0) {
      const executionLeaf = hashMidgardScriptExecutionLeaf({
        languageTag: 0,
        purposeLeaf: purpose.leaf,
        sourceLeaf: matchedSource.entry.leaf,
      });
      executionEntries.push({
        purpose,
        source: matchedSource.entry,
        sourceIndex: matchedSource.sourceIndex,
        languageTag: 0,
        redeemerLeaf: Buffer.alloc(0),
        leaf: executionLeaf,
      });
      discovery = resetCurrent({
        ...discovery,
        purposeCursor: purposeCursor + 1,
        executionFrontier: appendMidgardValidationMerkleLeaf(
          discovery.executionFrontier,
          executionLeaf,
        ),
      });
      continue;
    }

    let matchedRedeemerIndex = -1;
    for (
      let redeemerIndex = 0;
      redeemerIndex < prepared.decodedProofRedeemers.length;
      redeemerIndex += 1
    ) {
      const redeemer = prepared.decodedProofRedeemers[redeemerIndex]!;
      const itemTrace = run.redeemerItemTrace(redeemerIndex);
      push(
        "scriptSources",
        witness(10, discovery),
        run.redeemerScanBegin(redeemerIndex),
      );
      discovery = {
        ...discovery,
        redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
          itemTrace.initial,
        ),
      };
      for (const itemStep of itemTrace.steps) {
        push("scriptSources", witness(10, discovery), {
          kind: "redeemerItemStep",
          redeemerControl: null,
          control: itemStep.control,
          witness: itemStep.witness,
        });
        if (itemStep.next.stage !== MidgardRedeemerItemProofStages.Terminal) {
          discovery = {
            ...discovery,
            redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
              itemStep.next,
            ),
          };
          continue;
        }
        if (
          redeemerPointerMatchesPurpose({
            purposeKind: purpose.purposeKind,
            purposeIndex: purpose.purposeIndex,
            redeemerTag: redeemer.tag,
            redeemerIndex: redeemer.index,
          })
        ) {
          matchedRedeemerIndex = redeemerIndex;
          const redeemerLeaf = prepared.redeemerLeafHashes[redeemerIndex]!;
          const executionLeaf = hashMidgardScriptExecutionLeaf({
            languageTag: matchedSource.languageTag,
            purposeLeaf: purpose.leaf,
            sourceLeaf: matchedSource.entry.leaf,
            redeemerLeaf,
          });
          executionEntries.push({
            purpose,
            source: matchedSource.entry,
            sourceIndex: matchedSource.sourceIndex,
            languageTag: matchedSource.languageTag,
            redeemerLeaf,
            leaf: executionLeaf,
          });
          discovery = resetCurrent({
            ...discovery,
            purposeCursor: purposeCursor + 1,
            usedRedeemerBitmap: setDiscoveryBit(
              discovery.usedRedeemerBitmap,
              redeemerIndex,
            ),
            executionFrontier: appendMidgardValidationMerkleLeaf(
              discovery.executionFrontier,
              executionLeaf,
            ),
          });
        } else {
          discovery = {
            ...discovery,
            redeemerCursor: redeemerIndex + 1,
            redeemerItemControlHash: Buffer.alloc(0),
          };
        }
      }
      if (matchedRedeemerIndex >= 0) break;
    }
    if (matchedRedeemerIndex < 0) {
      push("scriptSources", witness(10, discovery));
      return phaseStops(
        {
          RedeemerMissing: {
            purpose_kind: BigInt(discovery.currentPurposeKind),
            purpose_index: BigInt(discovery.currentPurposeIndex),
          },
        },
        "V1 redeemer scan found no redeemer for the purpose",
      );
    }
  }
  push("scriptSources", witness(8, discovery));
  return { kind: "discovered", discovery, executionEntries };
};
