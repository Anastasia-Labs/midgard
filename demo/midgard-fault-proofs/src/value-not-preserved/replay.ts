import {
  decodeMidgardFieldPreimage,
  decodeMidgardMintPolicyItem,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  traceMidgardLedgerOutputScan,
} from "@al-ft/midgard-core";
import { MidgardTxCodecError } from "@al-ft/midgard-core/codec";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { eventLedgerBeforeStep } from "../event-ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type CheckedLedger, ledgerEntryLeaf } from "../ledger-at.js";
import { transactionHasNonCanonicalMintItem } from "../mint-item-non-canonical/replay.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import {
  acceptedTransactionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  collectReplayFindings,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import {
  VALUE_CONSERVATION_ARTIFACT,
  type ValueConservationArtifact,
} from "./artifact.js";
import { findValueNotPreserved } from "./finding.js";
import { ConservationClaim } from "./union-schemas.js";

export const VALUE_NOT_PRESERVED_VIOLATION = "value-not-preserved";
export const VALUE_NOT_PRESERVED_REJECTION_VIOLATION =
  "value-not-preserved-wrongful-rejection";

/** An output the ledger-output scan refuses has no ledger descriptor, and
 * the on-chain conservation fold cannot read it; the output canonicality
 * finding at that event proves it. */
const outputsScanRefused = (outputsPreimageCbor: Uint8Array): boolean =>
  decodeMidgardFieldPreimage(outputsPreimageCbor).some(
    (output) => traceMidgardLedgerOutputScan(output).refusal !== null,
  );

/** Whether `output` reads as a transaction output. One whose native
 * reference script does not decode is a canonical ledger entry that does
 * not: it has no value the conservation fold can read. */
const readsAsTransactionOutput = (output: Uint8Array): boolean => {
  try {
    decodeMidgardTxOutput(output);
    return true;
  } catch (cause) {
    if (cause instanceof MidgardTxCodecError) return false;
    throw cause;
  }
};

/** Public retained-DA reconstruction, including earlier transactions in the block. */
export const prepareValueConservationArtifact = async ({
  block,
  predecessor,
  sourceIndex,
  forced,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
  readonly sourceIndex: number;
  readonly forced: boolean;
}): Promise<ValueConservationArtifact | null> => {
  const source = forced
    ? block.reconstruction.forcedTransactions[sourceIndex]
    : block.reconstruction.transactions[sourceIndex];
  if (source === undefined)
    throw new Error("value conservation: source coordinate absent");
  const eventKey: SDK.EventKey = forced
    ? {
        ForcedTransactionEventKey: {
          tx_order_id:
            block.reconstruction.forcedTransactions[sourceIndex]!.key,
        },
      }
    : {
        L2TransactionEventKey: {
          tx_id: block.reconstruction.transactions[sourceIndex]!.txId,
        },
      };
  if (forced) {
    const verdict =
      block.reconstruction.forcedTransactions[sourceIndex]!.value.verdict;
    if (
      verdict === "ForcedTxValid" ||
      verdict.ForcedTxInvalid.reason !== "ValueNotPreserved"
    )
      return null;
  } else if (
    block.reconstruction.transactions[sourceIndex]!.validity !== "TxIsValid"
  )
    return null;
  const transaction = forced
    ? decodeMidgardForcedTxFullFromCanonicalCbor(source.fullTransactionCbor)
    : decodeMidgardNativeTxFullFromCanonicalCbor(source.fullTransactionCbor);
  if ("validity" in transaction && transaction.validity !== "TxIsValid")
    return null;
  // A mint item outside the field-5 grammar, an output the ledger-output
  // scan refuses, or one that does not read as a transaction output, has no
  // conservation meaning; the direct mint-item, output canonicality or output
  // reference script proof owns that transaction. A forced transaction's
  // output carries no reference script once its order is admitted, so one
  // that does is not the order's: no family yet convicts that, and the
  // decline holds the block.
  if (
    (!forced &&
      (transactionHasNonCanonicalMintItem(source.fullTransactionCbor) ||
        outputsScanRefused(transaction.body.outputsPreimageCbor))) ||
    !decodeMidgardFieldPreimage(transaction.body.outputsPreimageCbor).every(
      readsAsTransactionOutput,
    )
  )
    throw replayPrerequisiteFailure(
      block.headerHash,
      eventKey,
      "representable_field_shape",
    );
  const event = await buildEventToStepMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const transition = await buildIndexedTraceProof({
    reconstruction: block.reconstruction,
    stepIndex: event.value.step_index,
  });
  const { trie: ledger, outputs } = await eventLedgerBeforeStep(
    block.reconstruction,
    event.value.step_index,
    predecessor,
  );
  const inputItems = decodeMidgardFieldPreimage(
    transaction.body.spendInputsPreimageCbor,
  );
  if (inputItems.some((key) => !outputs.has(key.toString("hex")))) return null;
  const resolvedInputs = inputItems.map((key) => {
    const output = outputs.get(key.toString("hex"));
    if (output === undefined)
      throw new Error(
        "value conservation: unresolved spent input belongs to input absence proof",
      );
    return { key, output };
  });
  // A spent output that does not read as a transaction output was created by
  // an accepted step before this one, whose output reference script proof
  // convicts it, or the predecessor ledger holds it, and the resolved output
  // proof convicts this spend. A forced rejection's reason ranks after that
  // output's fault, and no family yet convicts that: the decline holds the
  // block.
  if (!resolvedInputs.every(({ output }) => readsAsTransactionOutput(output)))
    throw replayPrerequisiteFailure(
      block.headerHash,
      eventKey,
      "representable_field_shape",
    );
  const finding = findValueNotPreserved({
    validity: "TxIsValid",
    fee: transaction.body.fee,
    spentValues: resolvedInputs.map(({ output }) => ({
      kind: "resolved",
      value: decodeMidgardTxOutput(output).value,
    })),
    outputValues: decodeMidgardFieldPreimage(
      transaction.body.outputsPreimageCbor,
    ).map((bytes) => decodeMidgardTxOutput(bytes).value),
    mintItems: decodeMidgardFieldPreimage(
      transaction.body.mintPreimageCbor,
    ).map(decodeMidgardMintPolicyItem),
  });
  if (forced ? finding.kind !== "balanced" : finding.kind !== "fault")
    return null;
  const claim: ConservationClaim = forced
    ? "ForcedConservation"
    : finding.kind === "fault"
      ? {
          AcceptedImbalance: {
            asset: finding.claimedAsset,
            direction: finding.claimedDirection,
          },
        }
      : (() => {
          throw new Error("value conservation: inconsistent finding");
        })();
  const accepted = forced
    ? null
    : block.reconstruction.transactions[sourceIndex]!;
  return {
    schemaVersion: VALUE_CONSERVATION_ARTIFACT,
    headerCbor: Data.to(block.header, SDK.Header),
    transactionCbor: source.fullTransactionCbor.toString("hex"),
    claimCbor: Data.to(claim, ConservationClaim),
    forcedMembershipCbor: forced
      ? Data.to(
          await buildForcedTransactionLeafMembershipProof({
            reconstruction: block.reconstruction,
            eventKey,
          }),
          SDK.ForcedTransactionSourceMembershipProof,
        )
      : null,
    acceptedSourceCbor: accepted?.valueBytes.toString("hex") ?? null,
    acceptedPhasRoot:
      accepted === null
        ? null
        : block.reconstruction.rootData.transactions.phasRoot,
    acceptedProofCbor:
      accepted === null
        ? null
        : Data.to(
            await keyValuePhasProof(
              {
                ...block.reconstruction.rootData.transactions,
                root: block.reconstruction.rootData.transactions.phasRoot,
              },
              accepted.keyBytes,
              accepted.valueBytes,
            ),
            SDK.Proof,
          ),
    eventCbor: Data.to(event, SDK.EventToStepMembershipProof),
    transitionCbor: Data.to(transition, SDK.IndexedTraceProof),
    inputs: await Promise.all(
      resolvedInputs.map(async ({ key, output }) => ({
        outputCbor: output.toString("hex"),
        proofCbor: Data.to(
          await keyValuePhasProof(
            ledger,
            key,
            ledgerEntryLeaf({ outRef: key, output }, "value conservation").leaf,
          ),
          SDK.Proof,
        ),
      })),
    ),
  };
};

export const detectValueConservationFaults = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const candidates: { forced: boolean; index: number }[] = [];
  for (const forced of [false, true]) {
    const count = forced
      ? block.reconstruction.forcedTransactions.length
      : block.reconstruction.transactions.length;
    for (let index = 0; index < count; index++)
      candidates.push({ forced, index });
  }
  return collectReplayFindings(candidates, async ({ forced, index }) => {
    const artifact = await prepareValueConservationArtifact({
      block,
      predecessor,
      sourceIndex: index,
      forced,
    });
    if (artifact === null) return null;
    const violationId = forced
      ? VALUE_NOT_PRESERVED_REJECTION_VIOLATION
      : VALUE_NOT_PRESERVED_VIOLATION;
    return {
      ...(forced
        ? forcedTransactionSubject(
            block.reconstruction.forcedTransactions[index]!.key,
          )
        : acceptedTransactionSubject(
            block.reconstruction.transactions[index]!.txId,
          )),
      detectionId: `${violationId}:${index}`,
      headerHash: block.headerHash,
      violationId,
      position: BigInt((forced ? block.transactions.length : 0) + index),
      diagnostic: `authenticated value conservation contradiction at ${forced ? "forced" : "accepted"} source ${index}`,
    };
  });
};
