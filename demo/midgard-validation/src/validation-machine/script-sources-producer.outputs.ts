/**
 * ScriptSources stages 4 and 5: each output's item commitment folded, then
 * each output proved, admitted under its network and signer rules, and its
 * protected script receive purpose collected.
 */
import {
  appendMidgardValidationMerkleLeaf,
  attachMidgardLedgerOutputProofFacts,
  buildMidgardValidationMerkleMembership,
  hashMidgardOutputDescriptorLeaf,
  hashMidgardOutputItemLeaf,
  hashMidgardScriptPurposeLeaf,
  midgardLedgerOutputProofFactsComplete,
  traceMidgardLedgerOutputProof,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { buildCanonicalMidgardLedgerOutputMaterial } from "../ledger-output-descriptor.js";
import {
  type ReceiveScan,
  type ResolvedInputReplay,
  sameFold,
  type ScriptSourcesPrepared,
  type ScriptSourcesWitness,
  scriptSourcesWitnessCbor,
} from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import { refusedOutputStopReason } from "./trace-builder-prepare.refused-output-stop-reason.js";
import type { ScriptPurposeProofEntry } from "./trace-builder-types.js";
import { type PhaseOutcome, phaseStops, type TraceWalk } from "./trace-walk.js";
import type { ValidationMachineSignerSetProof } from "./types.js";

type OutputsPrepared = ScriptSourcesPrepared &
  Pick<
    PreparedValidationTransaction,
    | "input"
    | "fieldPreimages"
    | "outputsCollection"
    | "outputCbors"
    | "outputLeafHashes"
    | "outputFrontier"
    | "canonicalSignerHashes"
    | "signerLeafHashes"
  >;

/** What stage 5 leaves for the purpose and discovery stages. */
export type OutputAdmission = {
  readonly kind: "admitted";
  readonly outputCursor: number;
  /** The receive sources and admitted descriptors the stage folded. */
  readonly receiveScan: ReceiveScan;
  readonly receiveSourceEntries: readonly ScriptPurposeProofEntry[];
  readonly admittedOutputDescriptorCbors: readonly Buffer[];
  readonly admittedOutputDescriptorLeafHashes: readonly Buffer[];
};

/** Stage 4, over stage 3's replay. */
const produceOutputCommitments = (
  prepared: OutputsPrepared,
  push: TraceWalk["push"],
  replay: ResolvedInputReplay,
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    const { items } = prepared.outputsCollection;
    let outputFrontier = prepared.emptyValidationFrontier;
    let outputTotalCount = 0;
    const witness = (): Buffer =>
      scriptSourcesWitnessCbor(prepared, {
        ...replay,
        stage: 4,
        outputFrontier,
        outputTotalCount,
      });
    for (const item of items) {
      const outputCbor = prepared.outputCbors[item.itemIndex];
      if (outputCbor === undefined || !item.bytes.equals(outputCbor)) {
        return yield* Effect.fail(
          new Error(
            "bounded output item diverged from its canonical decoded output",
          ),
        );
      }
      // Stage 4 folds only the authenticated (field_index, item_index,
      // item_length, item_commitment) tuple, all derived by the door from the
      // authenticated preimage. The item bytes are not revealed: that re-proves
      // only that an authenticated commitment has a preimage (canonicalDecode and
      // the stage-5 output traversal establish it) while growing the one-step
      // evidence past the L1 envelope for legal 16,384-byte outputs
      // (C21-STAGE4-GAP, Option A). The carriage keeps this redeemer O(1) in
      // output size only under tiers 2-3, where the preimage rides reference
      // inputs. The step carries the *plan input* (field and bytes); the tier is
      // resolved at evidence commitment, where a transaction exists to index
      // reference inputs into (#600): above §8.3's 14,336-byte tier-1 cap it is
      // tier 2 or 3 and O(1), below it tier-1 `Inline`. The producer never
      // refuses and never names a tier.
      push("scriptSources", witness(), {
        kind: "transactionRedeemerItemBegin",
        fieldIndex: 2,
        fieldPreimage: preparedFieldPreimage(prepared, 2),
      });
      if (outputTotalCount === 0) outputTotalCount = items.length;
      outputFrontier = appendMidgardValidationMerkleLeaf(
        outputFrontier,
        hashMidgardOutputItemLeaf({
          outputIndex: item.itemIndex,
          itemLength: item.bytes.length,
          itemCommitment: item.commitment,
        }),
      );
    }
    push("scriptSources", witness());
    if (!sameFold(outputFrontier, prepared.outputFrontier)) {
      return yield* Effect.fail(
        new Error(
          "authenticated output fold diverged from the canonical output frontier",
        ),
      );
    }
  });

/** A key-locked protected output's signer-set proof; none for any other. */
const protectedOutputSignerProof = (
  prepared: OutputsPrepared,
  outputCbor: Buffer,
): ValidationMachineSignerSetProof => {
  const output = decodeMidgardTxOutput(outputCbor);
  const address = decodeMidgardAddressBytes(output.address);
  if (!address.protected || address.paymentCredential.kind === "Script") {
    return { kind: "none" };
  }
  return preparedSignerProof(
    prepared,
    Buffer.from(address.paymentCredential.hash),
  );
};

/**
 * Stages 4 and 5. It stops on the first output the machine refuses, or
 * returns what the admission folded.
 */
export const produceOutputAdmission = (
  prepared: OutputsPrepared,
  push: TraceWalk["push"],
  replay: ResolvedInputReplay,
): Effect.Effect<PhaseOutcome | OutputAdmission, Error> =>
  Effect.gen(function* () {
    yield* produceOutputCommitments(prepared, push, replay);
    const empty = prepared.emptyValidationFrontier;
    let outputCursor = 0;
    let receiveScan: ReceiveScan = {
      sourceFrontier: empty,
      receiveCount: 0,
      previousHash: Buffer.alloc(0),
      candidateHash: Buffer.alloc(0),
      descriptorFrontier: empty,
    };
    const receiveSourceEntries: ScriptPurposeProofEntry[] = [];
    const admittedOutputDescriptorCbors: Buffer[] = [];
    const admittedOutputDescriptorLeafHashes: Buffer[] = [];
    const witness = (outputProof?: ScriptSourcesWitness["outputProof"]) =>
      scriptSourcesWitnessCbor(prepared, {
        ...replay,
        stage: 5,
        outputCursor,
        outputFrontier: prepared.outputFrontier,
        receiveScan,
        ...(outputProof === undefined ? {} : { outputProof }),
      });
    for (const outputCbor of prepared.outputCbors) {
      const outputItem = prepared.outputsCollection.items[outputCursor];
      if (outputItem === undefined || !outputItem.bytes.equals(outputCbor)) {
        return yield* Effect.fail(
          new Error("output admission lost its authenticated bounded item"),
        );
      }
      const proofOutcome = traceMidgardLedgerOutputProof({
        outputIndex: outputCursor,
        outputCbor,
      });
      push("scriptSources", witness(), {
        kind: "ledgerOutputProofBegin",
        outputIndex: outputCursor,
        totalLength: outputItem.bytes.length,
        itemCommitment: outputItem.commitment,
        siblings: buildMidgardValidationMerkleMembership(
          prepared.outputLeafHashes,
          outputCursor,
        ).siblings,
      });
      for (const proofStep of proofOutcome.kind === "accepted"
        ? proofOutcome.trace.steps
        : [...proofOutcome.steps, proofOutcome.refusingStep]) {
        push("scriptSources", witness(proofStep.control), {
          kind: "ledgerOutputProofStep",
          witness: proofStep.witness,
        });
      }
      // The machine refuses the output at the refusing step.
      if (proofOutcome.kind === "refused") {
        return phaseStops(
          refusedOutputStopReason(outputCursor, proofOutcome.refusal),
          `output ${outputCursor} fails its output proof (${proofOutcome.refusal})`,
        );
      }
      const outputProof = proofOutcome.trace;
      const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: outputCursor,
        outputCbor,
      });
      const signerProof = protectedOutputSignerProof(prepared, outputCbor);
      // Chained descriptor-fact attachment: each canonical group is
      // one checkpointed machine step over the same finalize witness
      // shape, its commitments derived from the terminal control
      // alone; the thin terminal finalize below requires the scan
      // fact to commit exactly the output's descriptor.
      let factsProof = outputProof.terminal;
      while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
        push("scriptSources", witness(factsProof), {
          kind: "ledgerOutputProofFinalize",
          signerProof: { kind: "none" },
        });
        const attached = attachMidgardLedgerOutputProofFacts(factsProof);
        if (attached === null) {
          return yield* Effect.fail(
            new Error("ledger output proof fact attachment failed closed"),
          );
        }
        factsProof = attached;
      }
      push("scriptSources", witness(factsProof), {
        kind: "ledgerOutputProofFinalize",
        signerProof,
      });
      const output = decodeMidgardTxOutput(outputCbor);
      const address = decodeMidgardAddressBytes(output.address);
      // Stage 5 is where the machine refuses both output faults, so
      // validation must have refused this output for the same one.
      const networkFault =
        BigInt(address.networkId) !== prepared.input.expectedNetworkId;
      if (
        networkFault ||
        (address.protected &&
          address.paymentCredential.kind === "PubKey" &&
          signerProof.kind !== "membership")
      ) {
        return phaseStops(
          networkFault
            ? "NetworkIdMismatch"
            : {
                ProtectedOutputSignerMissing: {
                  output_index: BigInt(outputCursor),
                },
              },
          `output ${outputCursor} fails at stage 5`,
        );
      }
      const descriptorLeaf = hashMidgardOutputDescriptorLeaf({
        outputIndex: outputCursor,
        descriptorCbor: outputMaterial.descriptorCbor,
      });
      let { sourceFrontier } = receiveScan;
      admittedOutputDescriptorCbors.push(outputMaterial.descriptorCbor);
      admittedOutputDescriptorLeafHashes.push(descriptorLeaf);
      if (address.protected && address.paymentCredential.kind === "Script") {
        const scriptHash = Buffer.from(address.paymentCredential.hash);
        const purposeEntry: ScriptPurposeProofEntry = {
          purposeKind: 3,
          purposeIndex: BigInt(sourceFrontier.count),
          scriptHash,
          subject: scriptHash,
          leaf: hashMidgardScriptPurposeLeaf({
            purposeKind: 3,
            purposeIndex: BigInt(sourceFrontier.count),
            scriptHash,
            subject: scriptHash,
          }),
        };
        receiveSourceEntries.push(purposeEntry);
        sourceFrontier = appendMidgardValidationMerkleLeaf(
          sourceFrontier,
          purposeEntry.leaf,
        );
      }
      receiveScan = {
        ...receiveScan,
        sourceFrontier,
        descriptorFrontier: appendMidgardValidationMerkleLeaf(
          receiveScan.descriptorFrontier,
          descriptorLeaf,
        ),
      };
      outputCursor += 1;
    }
    push("scriptSources", witness());
    return {
      kind: "admitted",
      outputCursor,
      receiveScan,
      receiveSourceEntries,
      admittedOutputDescriptorCbors,
      admittedOutputDescriptorLeafHashes,
    };
  });
