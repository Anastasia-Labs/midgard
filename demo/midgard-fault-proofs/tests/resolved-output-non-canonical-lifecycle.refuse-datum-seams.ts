import { buildMidgardBoundedItem } from "@al-ft/midgard-core";

import {
  RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
  resolvedOutputDatumState,
  resolvedOutputDatumStepArgs,
  type ResolvedOutputEvidence,
} from "../src/resolved-output-non-canonical/index.js";
import { coverage } from "./resolved-output-non-canonical-lifecycle.record-carriage.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import type { makeResolvedOutputStages } from "./support/resolved-output-non-canonical-emulator.js";

type Stages = Awaited<ReturnType<typeof makeResolvedOutputStages>>;

/**
 * Every forged datum step against the live datum checkpoint, each refused on
 * chain: chunk proofs for another item (the cursor chunk's, or the next
 * chunk's beside the honest cursor chunk), the honest read handed another
 * successor, and, once a container is open, another stack. Answers whether
 * the step had a next chunk to substitute.
 */
export const refuseDatumSeams = async (
  stages: Stages,
  threadOutRef: string,
  evidence: ResolvedOutputEvidence,
) => {
  const plan = evidence.datumPlan!;
  const state = await stages.readDatumState(threadOutRef);
  const step = plan.steps.find(
    (candidate) =>
      BigInt(candidate.cursor) === state.cursor &&
      candidate.stackRoot === state.stack_root,
  );
  if (step === undefined) throw new Error("datum checkpoint is not planned");
  const args = resolvedOutputDatumStepArgs({ evidence, plan, step });
  const honest =
    step.next.kind === "finish"
      ? {
          nextStepIndex: 5 as const,
          nextData: {
            subject: evidence.subject,
            output_is_non_canonical: !step.next.canonical,
          },
        }
      : {
          nextStepIndex: 4 as const,
          nextData: resolvedOutputDatumState(
            evidence.subject,
            state.item_commitment,
            plan,
            step.next,
          ) as Record<string, unknown>,
        };
  // Chunk proofs for the output with the byte at the cursor flipped.
  const substituted = Buffer.from(evidence.resolved.outputCborHex, "hex");
  substituted[step.cursor] = substituted[step.cursor]! ^ 0xff;
  await expectOnchainRefusal(() =>
    stages.step04DatumRaw({
      threadOutRef,
      ...honest,
      args: resolvedOutputDatumStepArgs({
        evidence,
        plan,
        step,
        item: buildMidgardBoundedItem({
          fieldIndex: 2,
          itemIndex: evidence.resolved.outputIndex,
          bytes: substituted,
        }),
      }),
    }),
  );
  if (args.next_chunk_proof !== null) {
    // The cursor chunk's honest proof beside a proof of the next chunk with
    // its first byte flipped.
    const crossing = Buffer.from(evidence.resolved.outputCborHex, "hex");
    const nextStart =
      (Math.floor(step.cursor / RESOLVED_OUTPUT_DATUM_CHUNK_BYTES) + 1) *
      RESOLVED_OUTPUT_DATUM_CHUNK_BYTES;
    crossing[nextStart] = crossing[nextStart]! ^ 0xff;
    const { next_chunk_proof } = resolvedOutputDatumStepArgs({
      evidence,
      plan,
      step,
      item: buildMidgardBoundedItem({
        fieldIndex: 2,
        itemIndex: evidence.resolved.outputIndex,
        bytes: crossing,
      }),
    });
    await expectOnchainRefusal(() =>
      stages.step04DatumRaw({
        threadOutRef,
        ...honest,
        args: { ...args, next_chunk_proof },
      }),
    );
  }
  coverage.seamMutated("datum_chunk");
  // The honest read handed the other verdict, or a checkpoint a byte ahead.
  const forged =
    step.next.kind === "finish"
      ? {
          nextStepIndex: 5 as const,
          nextData: {
            subject: evidence.subject,
            output_is_non_canonical: step.next.canonical,
          },
        }
      : {
          nextStepIndex: 4 as const,
          nextData: {
            ...honest.nextData,
            cursor: BigInt(step.next.cursor) + 1n,
          },
        };
  await expectOnchainRefusal(() =>
    stages.step04DatumRaw({ threadOutRef, ...forged, args }),
  );
  coverage.seamMutated("datum_checkpoint");
  if (step.stack.length > 0) {
    await expectOnchainRefusal(() =>
      stages.step04DatumRaw({
        threadOutRef,
        ...honest,
        args: { ...args, tail_root: "00".repeat(32) },
      }),
    );
    coverage.seamMutated("datum_stack");
  }
  return args.next_chunk_proof !== null;
};
