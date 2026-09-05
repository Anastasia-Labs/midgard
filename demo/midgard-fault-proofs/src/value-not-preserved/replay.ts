import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  decodeMidgardMintPolicyItem,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import {
  VALUE_CONSERVATION_ARTIFACT,
  type ValueConservationArtifact,
} from "./artifact.js";
import { findValueNotPreserved } from "./finding.js";
import { ConservationClaim } from "./union-schemas.js";

export const VALUE_NOT_PRESERVED_VIOLATION = "value-not-preserved";
export const VALUE_NOT_PRESERVED_REJECTION_VIOLATION =
  "value-not-preserved-wrongful-rejection";

const eventLedger = async (
  block: CanonicalBlockEvidence,
  stepIndex: bigint,
  predecessor?: CanonicalBlockEvidence,
) => {
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== block.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== block.header.prevUtxosRoot)
  )
    throw new Error("value conservation: unrelated predecessor");
  const outputs = new Map(
    (predecessor?.reconstruction.utxos ?? []).map((entry) => [
      entry.key.toString("hex"),
      entry,
    ]),
  );
  const root = async () =>
    await keyValuePhasRootWithCount(
      [...outputs.values()].map((entry) => ({
        key: entry.key,
        value: buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: entry.key,
          outputCbor: entry.value,
        }).descriptorCbor,
      })),
    );
  let ledger = await root();
  if (ledger.root !== block.header.prevUtxosRoot)
    throw new Error(
      "value conservation: authenticated predecessor ledger unavailable",
    );
  for (let index = 0n; index < stepIndex; index++) {
    const step = block.reconstruction.traceByStepIndex.get(index)?.value;
    if (
      step === undefined ||
      step.step_index !== index ||
      step.schema_version !== 1n ||
      step.pre_utxos_root !== ledger.root
    )
      throw new Error("value conservation: inconsistent prior transition");
    const event = block.reconstruction.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (event === undefined || event.phase !== step.phase)
      throw new Error("value conservation: missing prior source event");
    if (event.phase === "Withdrawal") {
      if (event.entry.value.validity === "WithdrawalIsValid") {
        const ref = event.entry.value.body.l2_outref;
        outputs.delete(
          encodeMidgardSpendInputItem({
            txId: Buffer.from(ref.transactionId, "hex"),
            outputIndex: Number(ref.outputIndex),
          }).toString("hex"),
        );
      }
    } else if (
      event.phase === "ForcedTransaction" ||
      event.phase === "L2Transaction"
    ) {
      const valid =
        event.phase === "ForcedTransaction"
          ? event.entry.value.verdict === "ForcedTxValid"
          : event.entry.validity === "TxIsValid";
      if (valid) {
        const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
          event.entry.fullTransactionCbor,
        );
        for (const input of decodeMidgardFieldPreimage(
          transaction.body.spendInputsPreimageCbor,
        ))
          outputs.delete(input.toString("hex"));
        for (const [outputIndex, value] of decodeMidgardFieldPreimage(
          transaction.body.outputsPreimageCbor,
        ).entries()) {
          const key = encodeMidgardSpendInputItem({
            txId: computeMidgardNativeTxId(transaction),
            outputIndex,
          });
          outputs.set(key.toString("hex"), { key, value });
        }
      }
    } else
      throw new Error(
        "value conservation: deposit phase precedes transaction phase",
      );
    ledger = await root();
    if (ledger.root !== step.post_utxos_root)
      throw new Error(
        "value conservation: prior committed effect root differs",
      );
  }
  return { ledger, outputs };
};

/** Public retained-DA reconstruction, including earlier transactions in the block. */
export const prepareValueConservationArtifact = async ({
  block,
  predecessor,
  sourceIndex,
  forced,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CanonicalBlockEvidence;
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
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
    source.fullTransactionCbor,
  );
  if (transaction.validity !== "TxIsValid") return null;
  const event = await buildEventToStepMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const transition = await buildIndexedTraceProof({
    reconstruction: block.reconstruction,
    stepIndex: event.value.step_index,
  });
  const { ledger, outputs } = await eventLedger(
    block,
    event.value.step_index,
    predecessor,
  );
  if (ledger.root !== transition.value.pre_utxos_root)
    throw new Error("value conservation: selected event pre-state differs");
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
  const finding = findValueNotPreserved({
    validity: "TxIsValid",
    fee: transaction.body.fee,
    spentValues: resolvedInputs.map(({ output }) => ({
      kind: "resolved",
      value: decodeMidgardTxOutput(output.value).value,
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
        outputCbor: output.value.toString("hex"),
        proofCbor: Data.to(
          await keyValuePhasProof(
            ledger,
            key,
            buildCanonicalMidgardLedgerEntryOutputMaterial({
              outRef: key,
              outputCbor: output.value,
            }).descriptorCbor,
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
  readonly predecessor?: CanonicalBlockEvidence;
}) => {
  const detections = [];
  for (const forced of [false, true]) {
    const count = forced
      ? block.reconstruction.forcedTransactions.length
      : block.reconstruction.transactions.length;
    for (let index = 0; index < count; index++) {
      const artifact = await prepareValueConservationArtifact({
        block,
        predecessor,
        sourceIndex: index,
        forced,
      });
      if (artifact === null) continue;
      const violationId = forced
        ? VALUE_NOT_PRESERVED_REJECTION_VIOLATION
        : VALUE_NOT_PRESERVED_VIOLATION;
      detections.push({
        detectionId: `${violationId}:${index}`,
        headerHash: block.headerHash,
        violationId,
        position: BigInt((forced ? block.transactions.length : 0) + index),
        diagnostic: `authenticated value conservation contradiction at ${forced ? "forced" : "accepted"} source ${index}`,
      });
    }
  }
  return detections;
};
