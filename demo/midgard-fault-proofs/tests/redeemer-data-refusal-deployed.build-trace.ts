import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  deriveMidgardNativeTxBodyCompact,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardForcedTxCanonical,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildDeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/** The program material of the fixture's one PlutusV3 script. */
export const programMaterialSidecarCbor = Buffer.from(
  "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
  "hex",
);

/** A real forced transaction whose original field-8 bytes end at a Data refusal. */
export const buildDataRefusalTrace = async (
  data: string,
  now = 1_800_000_000_000,
  missingScript = false,
) => {
  const spendCount = 1;
  const spent = Array.from({ length: spendCount }, (_, index) =>
    outRefFromByte(0x71, BigInt(index)),
  );
  const privateKey = CML.PrivateKey.generate_ed25519();
  const script = plutusV3ScriptWitness(
    Buffer.from(
      "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
      "hex",
    ),
  );
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(script),
    FUNDED_OUTPUT_LOVELACE,
  );
  const producedOutput = makeOutput(
    FUNDED_OUTPUT_LOVELACE * BigInt(spendCount),
  );
  const source = makeNativeTx({
    spendInputs: spent,
    outputs: [producedOutput],
    scriptWitnesses: missingScript ? [] : [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      { tag: 0, index: 0n, data: Buffer.from(data, "hex") },
    ]),
    scriptLanguages: ["PlutusV3"],
    privateKey,
  });
  const bodyHash = computeMidgardNativeTxId({
    version: source.tx.version,
    transactionBody: deriveMidgardNativeTxBodyCompact(source.tx.body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: source.tx.validity,
  });
  const transaction = encodeRecomputedNativeTx({
    ...source.tx,
    witnessSet: {
      ...source.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList([
        Buffer.from(
          CML.make_vkey_witness(
            CML.TransactionHash.from_raw_bytes(bodyHash),
            privateKey,
          ).to_cbor_bytes(),
        ),
      ]),
    },
  });
  const ledgerWitnessEntries = spent.map((outRef) => ({
    outRef,
    output: spentOutput,
  }));
  const sidecar = missingScript
    ? encodeMidgardCekProgramMaterialSidecar([])
    : programMaterialSidecarCbor;
  const sourceKey = { transactionId: "f7".repeat(32), outputIndex: 0n };
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: sourceKey },
  } as const;
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey as never, SDK.EventKeySchema as never),
        "hex",
      ),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
        decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor),
      ),
      programMaterialSidecarCbor: sidecar,
      priorUtxosRoot: "00".repeat(32),
      postUtxosRoot: "00".repeat(32),
      ledgerWitnessEntries,
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_INVALID_FIELD_TYPE",
    }),
  );
  const txCbor = encodeMidgardForcedTxCanonical(
    decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor),
  );
  return {
    trace,
    txCbor,
    eventKey,
    sourceKey,
    source: deriveMidgardForcedTxProofSourceFromCanonicalCbor(txCbor),
    transaction,
    ledgerWitnessEntries,
    programMaterialSidecarCbor: sidecar,
  };
};
