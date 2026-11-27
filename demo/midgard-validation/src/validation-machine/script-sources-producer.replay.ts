/**
 * ScriptSources stages 2 and 3: the resolved inputs replayed from the
 * retained ledger material, each adding its reference script source, its
 * spend purpose and its resolved context item.
 */
import {
  appendMidgardValidationMerkleLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardResolvedContextItemLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardScriptSourceLeaf,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../ledger-output-descriptor.js";
import {
  type ResolvedInputReplay,
  type ScriptSourcesPrepared,
  scriptSourcesWitnessCbor,
} from "./prepare-producers.shared.js";
import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import type {
  ScriptPurposeProofEntry,
  ScriptSourceProofEntry,
} from "./trace-builder-types.js";
import type { TraceWalk } from "./trace-walk.js";

const fail = (message: string) => Effect.fail(new Error(message));

/**
 * Stages 2 and 3. It returns the replay's folds with the reference sources
 * and spend purposes it found, in schedule order.
 */
export const produceResolvedInputReplay = (
  prepared: ScriptSourcesPrepared &
    Pick<
      PreparedValidationTransaction,
      "redeemerFrontier" | "ledgerState" | "ledgerDescriptorState"
    >,
  push: TraceWalk["push"],
): Effect.Effect<
  {
    readonly replay: ResolvedInputReplay;
    readonly sourceEntries: readonly ScriptSourceProofEntry[];
    readonly purposeEntries: readonly ScriptPurposeProofEntry[];
  },
  Error
> =>
  Effect.gen(function* () {
    push(
      "scriptSources",
      scriptSourcesWitnessCbor(prepared, {
        stage: 2,
        sourceFrontier: prepared.inlineScriptSourceFrontier,
        redeemerFrontier: prepared.redeemerFrontier,
      }),
    );
    let replay: ResolvedInputReplay = {
      sourceFrontier: prepared.inlineScriptSourceFrontier,
      redeemerFrontier: prepared.redeemerFrontier,
      replayCursor: 0,
      replayRemainingScheduleHash: prepared.resolutionScheduleHash,
      spendIndex: 0,
      purposeFrontier: prepared.emptyValidationFrontier,
      resolvedItemFrontier: prepared.emptyValidationFrontier,
    };
    const sourceEntries: ScriptSourceProofEntry[] = [];
    const purposeEntries: ScriptPurposeProofEntry[] = [];
    for (const node of prepared.resolutionScheduleNodes) {
      const outRefHex = node.key.toString("hex");
      const outputCbor = prepared.ledgerState.get(outRefHex);
      const descriptorCbor = prepared.ledgerDescriptorState.get(outRefHex);
      if (outputCbor === undefined || descriptorCbor === undefined) {
        return yield* fail(
          "resolved-input replay lost previously authenticated output material",
        );
      }
      const outputMaterial = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: node.key,
        outputCbor,
      });
      if (!outputMaterial.descriptorCbor.equals(descriptorCbor)) {
        return yield* fail(
          "resolved-input replay descriptor differs from retained output material",
        );
      }
      const { descriptor } = outputMaterial;
      push(
        "scriptSources",
        scriptSourcesWitnessCbor(prepared, { ...replay, stage: 3 }),
        {
          kind: "resolvedInputReplay",
          sourceKind: node.sourceKind,
          itemIndex: node.itemIndex,
          key: node.key,
          nextScheduleHash: node.nextScheduleHash,
          value: descriptorCbor,
          // The prior-ledger membership proof the lookup step
          // carried for this node authenticates the replayed value.
          proofCbor: node.proofCbor,
          siblings: [],
        },
      );
      if (!replay.replayRemainingScheduleHash.equals(node.scheduleHash)) {
        return yield* fail(
          "resolved-input replay schedule diverged from its committed hash chain",
        );
      }
      let { sourceFrontier, purposeFrontier, spendIndex } = replay;
      if (descriptor.referenceScriptLanguage !== -1) {
        const output = decodeMidgardTxOutput(outputCbor);
        if (output.script_ref === undefined) {
          return yield* fail(
            "resolved-output descriptor commits a missing retained reference script",
          );
        }
        const leaf = hashMidgardReferenceScriptSourceLeaf({
          sourceKey: node.key,
          scriptLanguageTag: descriptor.referenceScriptLanguage,
          scriptHash: descriptor.referenceScriptHash,
          scriptTotalLength: descriptor.referenceScriptTotalLength,
          itemCommitment: descriptor.referenceScriptItemCommitment,
        });
        if (
          !leaf.equals(
            hashMidgardScriptSourceLeaf({
              originKind: "reference",
              sourceKey: node.key,
              script: output.script_ref,
            }),
          )
        ) {
          return yield* fail(
            "retained reference script differs from its authenticated descriptor facts",
          );
        }
        sourceEntries.push({
          originKind: "reference",
          sourceKey: node.key,
          script: output.script_ref,
          authenticatedVersionedItemBytes: encodeMidgardVersionedScript(
            output.script_ref,
          ),
          scriptLanguageTag: descriptor.referenceScriptLanguage,
          scriptHash: descriptor.referenceScriptHash,
          scriptTotalLength: descriptor.referenceScriptTotalLength,
          scriptItemCommitment: descriptor.referenceScriptItemCommitment,
          leaf,
        });
        sourceFrontier = appendMidgardValidationMerkleLeaf(
          sourceFrontier,
          leaf,
        );
      }
      if (node.sourceKind === "spend") {
        const credential = decodeMidgardAddressBytes(
          descriptor.address,
        ).paymentCredential;
        if (credential.kind === "Script") {
          const purposeIndex = BigInt(spendIndex);
          const leaf = hashMidgardScriptPurposeLeaf({
            purposeKind: 0,
            purposeIndex,
            scriptHash: credential.hash,
            subject: node.key,
          });
          purposeEntries.push({
            purposeKind: 0,
            purposeIndex,
            scriptHash: Buffer.from(credential.hash),
            subject: node.key,
            leaf,
          });
          purposeFrontier = appendMidgardValidationMerkleLeaf(
            purposeFrontier,
            leaf,
          );
        }
        spendIndex += 1;
      }
      replay = {
        ...replay,
        sourceFrontier,
        purposeFrontier,
        spendIndex,
        resolvedItemFrontier: appendMidgardValidationMerkleLeaf(
          replay.resolvedItemFrontier,
          hashMidgardResolvedContextItemLeaf({
            sourceKind: node.sourceKind,
            itemIndex: replay.replayCursor,
            key: node.key,
            outputCbor: descriptorCbor,
          }),
        ),
        replayRemainingScheduleHash: node.nextScheduleHash,
        replayCursor: replay.replayCursor + 1,
      };
    }
    push(
      "scriptSources",
      scriptSourcesWitnessCbor(prepared, { ...replay, stage: 3 }),
    );
    return { replay, sourceEntries, purposeEntries };
  });
