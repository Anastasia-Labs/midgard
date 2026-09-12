import {
  computeHash32,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeaf,
} from "@al-ft/midgard-core";
import {
  buildExecutionSourceMachineAuthenticationFromRetainedDa,
  type CanonicalBlockEvidence,
  executionNativeScriptInvalid,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";

/** Match the production forced execution adapter's exact source and machine witness. */
export const prepareJourneyNativeExecutionEvidence = async (
  block: CanonicalBlockEvidence,
  predecessor: { payload: SDK.DaPayload },
) => {
  const forced = block.reconstruction.forcedTransactions[0];
  if (forced === undefined || forced.value.verdict === "ForcedTxValid")
    throw new Error("Native execution fixture omitted its forced rejection");
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
    forced.fullTransactionCbor,
  );
  const resolvedOutputsByOutRef = new Map(
    predecessor.payload.block_body.utxos.map(([key, value]) => [
      key,
      Buffer.from(value, "hex"),
    ]),
  );
  const reconstruction =
    executionNativeScriptInvalid.reconstructExecutionNativeScriptPurposes({
      canonicalTransactionCbor: forced.fullTransactionCbor,
      resolvedOutputsByOutRef,
    });
  const purpose = reconstruction.purposes[0];
  if (purpose === undefined || purpose.source.originKind !== 0)
    throw new Error(
      "Native fixture expected a concrete inline execution source",
    );
  const entries = (values: readonly SDK.DaPayloadEntry[]) =>
    values.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }));
  const authentication =
    await buildExecutionSourceMachineAuthenticationFromRetainedDa({
      eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
      executionIndex: purpose.executionIndex,
      authenticatedValidationTraceEntries: entries(
        block.reconstruction.payload.block_body.validation_traces,
      ),
      retainedValidationWitnessEntries: entries(
        block.reconstruction.payload.block_body.validation_trace_witnesses,
      ),
      expectedValidationTracesRoot: block.header.validationTracesRoot,
      expectedPurposeKind: purpose.purposeKindTag,
    });
  const script = decodeMidgardVersionedScript(
    Buffer.from(purpose.source.versionedItemCbor, "hex"),
  );
  const evidence =
    executionNativeScriptInvalid.prepareExecutionNativeScriptInvalidEvidence({
      finding: {
        subject: SDK.forcedVerdictSubject({
          transactionId: forced.value.tx_id,
          sourceKey: forced.key,
          rejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
        }),
        executionIndex: purpose.executionIndex,
      },
      transactionIdHex: forced.value.tx_id,
      sourceDescriptorHashHex: hashMidgardInlineScriptSourceLeaf({
        sourceIndex: BigInt(purpose.source.sourceIndex),
        scriptLanguageTag: purpose.source.languageTag,
        scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
        scriptTotalLength: purpose.source.totalLength,
        itemCommitment: Buffer.from(purpose.source.itemCommitment, "hex"),
      }).toString("hex"),
      scriptItemHashHex: computeHash32(script.scriptBytes).toString("hex"),
      scriptBytes: script.scriptBytes,
      addressWitnessItems: decodeMidgardFieldPreimage(
        tx.witnessSet.addrTxWitsPreimageCbor,
      ),
      validityIntervalStart: tx.body.validityIntervalStart,
      validityIntervalEnd: tx.body.validityIntervalEnd,
    });
  if (!evidence.contradiction)
    throw new Error("Native execution evidence does not close");
  return { evidence, authentication: authentication.authentication };
};
