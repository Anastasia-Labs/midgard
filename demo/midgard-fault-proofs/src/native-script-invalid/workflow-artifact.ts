import {
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";

import type { WorkflowJsonObject } from "../workflow/identity.js";
import {
  admitNativeScriptInvalidArtifact,
  prepareNativeScriptInvalidArtifact,
} from "./artifact.js";
import {
  admitNativeScriptInvalidForcedArtifact,
  NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT,
  prepareNativeScriptInvalidForcedArtifact,
} from "./forced-artifact.js";
import type { PreparedNativeScriptInvalid } from "./prepare.js";

type ForcedPlan = Awaited<
  ReturnType<typeof admitNativeScriptInvalidForcedArtifact>
>;
export type NativeScriptInvalidWorkflowArtifact = Readonly<{
  artifact: WorkflowJsonObject & { readonly headerHash: string };
  prepared: Omit<PreparedNativeScriptInvalid, "txInclusion"> & {
    readonly txInclusion?: PreparedNativeScriptInvalid["txInclusion"];
  };
  witnessSetHash: string;
  forced?: ForcedPlan;
}>;
export const admitNativeScriptInvalidWorkflowArtifact = async (
  artifact: WorkflowJsonObject,
): Promise<NativeScriptInvalidWorkflowArtifact> => {
  if (artifact.schemaVersion !== NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT) {
    const admitted = admitNativeScriptInvalidArtifact(artifact);
    return {
      ...admitted,
      witnessSetHash: admitted.prepared.txInclusion.nativeTx.witness_set_hash,
    };
  }
  const forced = await admitNativeScriptInvalidForcedArtifact(artifact);
  const evidence = forced.evidence;
  return {
    artifact: { ...artifact, headerHash: forced.headerHash },
    forced,
    witnessSetHash: evidence.state.bad_tx_witness_set_hash,
    prepared: {
      headerHash: forced.headerHash,
      badTxId: forced.transactionId,
      nativeTxCanonicalCbor: evidence.nativeTxCanonicalCbor,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      scriptIndex: evidence.scriptIndex,
      scriptItemCbor: evidence.scriptItemCbor,
      // A position field 6 lacks selects no script, so it has no hash.
      scriptHash:
        evidence.scriptItemCbor === ""
          ? ""
          : hashMidgardVersionedScript(
              decodeMidgardVersionedScript(
                Buffer.from(evidence.scriptItemCbor, "hex"),
              ),
            ),
      addrWitnessItemCbors: evidence.addrWitnessItemCbors,
      scriptWitnessItemCbors: evidence.scriptWitnessItemCbors,
    },
  };
};
export const prepareNativeScriptInvalidWorkflowArtifact = async (
  args: Parameters<typeof prepareNativeScriptInvalidArtifact>[0],
): Promise<WorkflowJsonObject> =>
  args.classification.selected.detectionId.startsWith(
    "native-script-invalid:forced:",
  )
    ? await prepareNativeScriptInvalidForcedArtifact({
        block: args.evidence,
        detectionId: args.classification.selected.detectionId,
      })
    : await prepareNativeScriptInvalidArtifact(args);
