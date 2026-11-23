/** The executions the `scriptIntegrityHashMismatch` all-purposes walk
 * presents: each purpose script's selected source, and each spent item's
 * predecessor descriptor with its ledger membership. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk/fraud-proof";

import type { DirectPurposeSourceEvidence } from "../direct-purpose-family/source-walk.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/index.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL as LABEL } from "./contracts.js";

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
    const leaf = material.resolvedInputs.byKey.get(item.toString("hex"))?.leaf;
    if (leaf === undefined)
      throw new Error(`${LABEL}: spent descriptor absent`);
    return { item, leaf };
  });
  return await Promise.all(
    descriptors.map(async ({ item, leaf }) => {
      const credential = decodeMidgardAddressBytes(
        decodeMidgardLedgerOutputCommitment(leaf).address,
      ).paymentCredential;
      return {
        descriptor_cbor: leaf.toString("hex"),
        proof: await material.resolvedInputs.proof(item),
        execution:
          credential.kind === "Script"
            ? await executionOf(evidence, credential.hash.toString("hex"))
            : null,
      };
    }),
  );
};
