import { coreToTxOutput } from "@lucid-evolution/lucid";

import { type JournalJsonObject } from "./journal.js";
import {
  PROOF_CHUNK_PUBLICATION_RECOVERY,
  type ProofChunkRequirement,
} from "./proof-chunk-prerequisite.route-action-identity.js";
import {
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
} from "./transaction-boundary.js";

export const recoveryForTransaction = ({
  transaction,
  requirement,
  address,
}: {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly requirement: ProofChunkRequirement;
  readonly address: string;
}): JournalJsonObject => {
  if (transaction.referenceScripts.length !== 0) {
    throw new Error("proof-chunk publication unexpectedly used a script");
  }
  requireReferenceOnlyScriptWitnesses({
    transaction,
    label: "proof-chunk publication",
  });
  const outputs = transaction.signed.toTransaction().body().outputs();
  const claimed = new Set<number>();
  const recovered = requirement.chunkDatums.map((datumCbor) => {
    let outputIndex = -1;
    for (let index = 0; index < outputs.len(); index += 1) {
      if (claimed.has(index)) continue;
      const output = outputs.get(index);
      const decoded = coreToTxOutput(output);
      if (
        decoded.address === address &&
        output.datum_hash() === undefined &&
        output.datum()?.as_datum()?.to_canonical_cbor_hex() === datumCbor &&
        output.script_ref() === undefined &&
        Object.entries(decoded.assets).every(
          ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
        )
      ) {
        outputIndex = index;
        break;
      }
    }
    if (outputIndex < 0) {
      throw new Error(
        "proof-chunk publication body omitted an exact ADA-only inline-datum output",
      );
    }
    claimed.add(outputIndex);
    return Object.freeze({
      outRef: `${transaction.txHash}#${outputIndex.toString()}`,
      datumCbor,
    });
  });
  return Object.freeze({
    proofChunkPublication: Object.freeze({
      schemaVersion: PROOF_CHUNK_PUBLICATION_RECOVERY,
      proofCborSha256: requirement.proofCborSha256,
      outputs: Object.freeze(recovered),
    }),
  });
};
