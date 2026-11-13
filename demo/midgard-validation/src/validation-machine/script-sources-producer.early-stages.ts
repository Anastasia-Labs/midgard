/**
 * ScriptSources stages 0 and 1: each inline script witness hashed to its
 * canonical identity, then each redeemer item proved readable, each fold
 * checked against its canonical frontier.
 */
import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardBlake2b224Trace,
  buildMidgardBoundedItemChunkProof,
  buildMidgardRedeemerDataUnreadableTrace,
  buildMidgardRedeemerItemProofTrace,
  hashMidgardRedeemerItemLeaf,
  hashMidgardRedeemerItemProofControl,
  inspectMidgardRedeemerData,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MidgardBlake2b224TraceStages,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import { Effect } from "effect";

import { MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX } from "./canonical-field-item.js";
import {
  type PendingInlineSource,
  sameFold,
  type ScriptSourcesPrepared,
  scriptSourcesWitnessCbor,
} from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";

/** Stage 0. It returns the inline source count the stage carried. */
export const produceInlineSources = (
  prepared: ScriptSourcesPrepared &
    Pick<
      PreparedValidationTransaction,
      | "fieldPreimages"
      | "scriptWitnessesCollection"
      | "inlineScriptSourceEntries"
      | "inlineScriptSourceLeafHashes"
    >,
  push: TraceWalk["push"],
): Effect.Effect<number, Error> =>
  Effect.gen(function* () {
    const { items } = prepared.scriptWitnessesCollection;
    let sourceFrontier = prepared.emptyValidationFrontier;
    let inlineSourceCount = 0;
    const witness = (pendingSource?: PendingInlineSource): Buffer =>
      scriptSourcesWitnessCbor(prepared, {
        stage: 0,
        sourceFrontier,
        redeemerFrontier: prepared.emptyValidationFrontier,
        inlineSourceCount,
        redeemerTotalCount: 0,
        pendingSource,
      });
    for (const item of items) {
      push("scriptSources", witness(), {
        kind: "transactionFieldChunk",
        fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
        itemIndex: item.itemIndex,
        fieldPreimage: preparedFieldPreimage(
          prepared,
          MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
        ),
      });
      if (inlineSourceCount === 0) inlineSourceCount = items.length;
      const source = prepared.inlineScriptSourceEntries[item.itemIndex];
      if (source === undefined || source.originKind !== "inline") {
        return yield* Effect.fail(
          new Error(
            "bounded inline script item lost its canonical source entry",
          ),
        );
      }
      const scriptArray = readCborArrayHeader(
        item.bytes,
        0,
        "v1.script_source",
      );
      const scriptLanguage = readCborInteger(
        item.bytes,
        scriptArray.nextOffset,
        "v1.script_source.language",
      );
      const scriptPayload = readCborBytes(
        item.bytes,
        scriptLanguage.nextOffset,
        "v1.script_source.payload",
      );
      const payloadOffset =
        scriptPayload.nextOffset - scriptPayload.value.length;
      if (
        scriptArray.length !== 2 ||
        scriptPayload.nextOffset !== item.bytes.length ||
        scriptPayload.value.length !== source.script.scriptBytes.length ||
        !scriptPayload.value.equals(source.script.scriptBytes)
      ) {
        return yield* Effect.fail(
          new Error(
            "bounded inline script item is not its exact canonical versioned-script encoding",
          ),
        );
      }
      const languageTag: 0 | 3 | 128 =
        source.script.language === "NativeCardano"
          ? 0
          : source.script.language === "PlutusV3"
            ? 3
            : 128;
      if (scriptLanguage.value !== BigInt(languageTag)) {
        return yield* Effect.fail(
          new Error(
            "bounded inline script language diverged from its canonical source",
          ),
        );
      }
      const hashTrace = buildMidgardBlake2b224Trace(
        Buffer.concat([Buffer.from([languageTag]), source.script.scriptBytes]),
      );
      let pendingSource: PendingInlineSource = {
        sourceIndex: item.itemIndex,
        inlineSourceCount,
        languageTag,
        payloadOffset,
        payloadLength: scriptPayload.value.length,
        itemLength: item.bytes.length,
        itemCommitment: item.commitment,
        hashControl: hashTrace[0]!.control,
      };
      for (const hashStep of hashTrace) {
        let auxiliary: Parameters<TraceWalk["push"]>[2];
        if (hashStep.block !== null) {
          const first = hashStep.control.cursor === 0;
          const contentLength = hashStep.block.length - (first ? 1 : 0);
          const itemCursor = first
            ? payloadOffset
            : payloadOffset + hashStep.control.cursor - 1;
          const chunkIndex =
            contentLength === 0
              ? 0
              : Math.floor(itemCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
          const chunkProof = buildMidgardBoundedItemChunkProof(
            item,
            chunkIndex,
          );
          const offset =
            contentLength === 0
              ? payloadOffset
              : itemCursor - chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
          auxiliary = {
            kind: "scriptSourceHashBlock",
            chunkProof,
            nextChunkProof:
              contentLength > chunkProof.chunk.length - offset
                ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                : null,
          };
        }
        push("scriptSources", witness(pendingSource), auxiliary);
        pendingSource = { ...pendingSource, hashControl: hashStep.next };
      }
      if (
        pendingSource.hashControl.stage !==
          MidgardBlake2b224TraceStages.Terminal ||
        !pendingSource.hashControl.chainingValue
          .subarray(0, 28)
          .equals(source.scriptHash)
      ) {
        return yield* Effect.fail(
          new Error(
            "bounded inline script hash trace diverged from its canonical identity",
          ),
        );
      }
      push("scriptSources", witness(pendingSource));
      sourceFrontier = appendMidgardValidationMerkleLeaf(
        sourceFrontier,
        prepared.inlineScriptSourceLeafHashes[item.itemIndex]!,
      );
    }
    push("scriptSources", witness());
    if (!sameFold(sourceFrontier, prepared.inlineScriptSourceFrontier)) {
      return yield* Effect.fail(
        new Error(
          "authenticated inline source fold diverged from the canonical source frontier",
        ),
      );
    }
    return inlineSourceCount;
  });

type RedeemerItemStep = Pick<
  Extract<
    NonNullable<Parameters<TraceWalk["push"]>[2]>,
    { kind: "redeemerItemStep" }
  >,
  "control" | "witness"
>;

/** Stage 1, under stage 0's inline source count. */
export const produceRedeemerItems = (
  prepared: ScriptSourcesPrepared &
    Pick<
      PreparedValidationTransaction,
      | "fieldPreimages"
      | "redeemerWitnessesCollection"
      | "decodedProofRedeemers"
      | "canonicalRedeemerWitnessCbors"
      | "redeemerFrontier"
    >,
  push: TraceWalk["push"],
  inlineSourceCount: number,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    const { items } = prepared.redeemerWitnessesCollection;
    let redeemerFrontier = prepared.emptyValidationFrontier;
    let redeemerTotalCount = 0;
    const witness = (redeemerItemControlHash = Buffer.alloc(0)): Buffer =>
      scriptSourcesWitnessCbor(prepared, {
        stage: 1,
        sourceFrontier: prepared.inlineScriptSourceFrontier,
        redeemerFrontier,
        inlineSourceCount,
        redeemerTotalCount,
        redeemerItemControlHash,
      });
    const itemStep = (step: RedeemerItemStep) => ({
      kind: "redeemerItemStep" as const,
      redeemerControl: null,
      control: step.control,
      witness: step.witness,
    });
    for (const item of items) {
      const redeemer = prepared.decodedProofRedeemers[item.itemIndex];
      const canonicalCbor =
        prepared.canonicalRedeemerWitnessCbors[item.itemIndex];
      if (
        redeemer === undefined ||
        canonicalCbor === undefined ||
        !item.bytes.equals(canonicalCbor)
      ) {
        return yield* Effect.fail(
          new Error(
            "bounded redeemer item diverged from its canonical decoded witness",
          ),
        );
      }
      push("scriptSources", witness(), {
        kind: "transactionRedeemerItemBegin",
        // Stage 1: field 8, item index `control.redeemer_count`. Both are
        // fixed by the stage and its cursor, so the carriage is the whole
        // wire surface.
        fieldIndex: 8,
        fieldPreimage: preparedFieldPreimage(prepared, 8),
      });
      if (redeemerTotalCount === 0) redeemerTotalCount = items.length;
      if (
        inspectMidgardRedeemerData(Buffer.from(redeemer.dataCborHex, "hex"))
          .kind === "refusal"
      ) {
        const refusal = buildMidgardRedeemerDataUnreadableTrace({
          itemIndex: item.itemIndex,
          itemCount: redeemerTotalCount,
          itemBytes: item.bytes,
        });
        for (const step of [...refusal.steps, refusal]) {
          push(
            "scriptSources",
            witness(hashMidgardRedeemerItemProofControl(step.control)),
            itemStep(step),
          );
        }
        return phaseStops(
          { RedeemerMalformed: { redeemer_index: BigInt(item.itemIndex) } },
          "redeemer Data is unreadable",
        );
      }
      const itemTrace = buildMidgardRedeemerItemProofTrace({
        itemIndex: item.itemIndex,
        itemCount: redeemerTotalCount,
        itemBytes: item.bytes,
        mode: MidgardRedeemerItemProofModes.Data,
      });
      let activeItemControlHash = hashMidgardRedeemerItemProofControl(
        itemTrace.initial,
      );
      for (const step of itemTrace.steps) {
        push("scriptSources", witness(activeItemControlHash), itemStep(step));
        if (step.next.stage === MidgardRedeemerItemProofStages.Terminal) {
          redeemerFrontier = appendMidgardValidationMerkleLeaf(
            redeemerFrontier,
            hashMidgardRedeemerItemLeaf({
              redeemerIndex: item.itemIndex,
              itemLength: item.bytes.length,
              itemCommitment: item.commitment,
            }),
          );
          activeItemControlHash = Buffer.alloc(0);
        } else {
          activeItemControlHash = hashMidgardRedeemerItemProofControl(
            step.next,
          );
        }
      }
      if (activeItemControlHash.length !== 0) {
        return yield* Effect.fail(
          new Error("redeemer item proof did not reach its terminal control"),
        );
      }
    }
    push("scriptSources", witness());
    if (!sameFold(redeemerFrontier, prepared.redeemerFrontier)) {
      return yield* Effect.fail(
        new Error(
          "authenticated redeemer fold diverged from the canonical redeemer frontier",
        ),
      );
    }
    return phaseContinues;
  });
