/** The executions the `unusedScriptWitness` all-purposes walk
 * presents: each purpose script's selected source, and each spent item's
 * predecessor descriptor with its ledger membership. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeSourceEvidence } from "../direct-purpose-family/source-walk.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/index.js";
import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import { UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL as LABEL } from "./contracts.js";

export type Execution = SDK.DirectAllPurposesExecutionWitness;

/** The selected source of `scriptHash` with its source-map proof. A purpose
 * whose script has no source cannot complete the walk. */
export const executionOf = async (
  evidence: DirectPurposeSourceEvidence,
  scriptHash: string,
): Promise<Execution> => {
  const { selected, proof } = await evidence.lookup(scriptHash);
  if (selected === null)
    throw new Error(`${LABEL}: script ${scriptHash} has no source`);
  return { source: selected, proof };
};

/** Each spent item's predecessor descriptor and ledger membership, in raw
 * field order, with the execution of a script-locked input. */
export const spendWitnesses = async (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): Promise<readonly SDK.DirectAllPurposesSpendWitness[]> => {
  const descriptors = material.fields[0]!.map((item) => {
    const entry = material.ledger.root.entries.find((candidate) =>
      candidate.key.equals(item),
    );
    if (entry === undefined)
      throw new Error(`${LABEL}: spent descriptor absent`);
    return entry;
  });
  const proofs = await keyValuePhasMembershipProofs(
    material.ledger.root,
    descriptors,
  );
  return await Promise.all(
    descriptors.map(async (entry, index) => {
      const credential = decodeMidgardAddressBytes(
        decodeMidgardLedgerOutputCommitment(entry.value).address,
      ).paymentCredential;
      return {
        descriptor_cbor: entry.value.toString("hex"),
        proof: proofs[index]!,
        execution:
          credential.kind === "Script"
            ? await executionOf(evidence, credential.hash.toString("hex"))
            : null,
      };
    }),
  );
};
