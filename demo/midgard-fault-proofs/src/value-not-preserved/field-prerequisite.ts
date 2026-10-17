import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSourceFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";

import { planFaultProofFieldOpening } from "../field-opening.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
} from "../workflow/field-carriage-prerequisite.js";
import { journalJsonDigest } from "../workflow/journal.js";
import { admitValueConservationArtifact } from "./artifact.js";
import type { ValueNotPreservedContracts } from "./contracts.js";

/** The installed runner's exact publication/certificate plan, rebuilt from retained DA. */
export const createValueConservationFieldPrerequisite = ({
  contracts,
  certificate,
  ...ports
}: Omit<
  Parameters<typeof createAuthenticatedFieldCarriagePrerequisitePort>[0],
  "category" | "requirementForAction"
> & {
  readonly contracts: ValueNotPreservedContracts;
  readonly certificate: FieldCarriageRequirement["certificate"];
}) => {
  const cache = new Map<
    string,
    ReturnType<typeof admitValueConservationArtifact>
  >();
  return createAuthenticatedFieldCarriagePrerequisitePort({
    ...ports,
    category: "valueNotPreserved",
    requirementForAction: async ({ action, artifact }) => {
      if (action.input.stage !== "fold") return null;
      const key = journalJsonDigest(artifact);
      let prepared = cache.get(key);
      if (prepared === undefined) {
        prepared = admitValueConservationArtifact(artifact, contracts);
        cache.set(key, prepared);
      }
      const admitted = await prepared;
      const index = action.input.index;
      if (
        typeof index !== "number" ||
        !Number.isSafeInteger(index) ||
        index < 0 ||
        admitted.actions[index] === undefined
      )
        throw new Error(
          "value conservation: invalid publication action cursor",
        );
      const selected = admitted.actions[index]!;
      const fieldIndex =
        selected.fieldIndex ??
        (selected.position === "unionOutputScan" ? 2 : undefined);
      if (fieldIndex === undefined) return null;
      const proofSource = (
        admitted.source.claim === "ForcedConservation"
          ? deriveMidgardForcedTxProofSourceFromCanonicalCbor
          : deriveMidgardNativeTxProofSourceFromCanonicalCbor
      )(Buffer.from(admitted.artifact.transactionCbor, "hex"));
      return {
        planned: planFaultProofFieldOpening({
          anchorSourceKind:
            admitted.source.claim === "ForcedConservation" ? 1n : 0n,
          fieldIndex,
          anchorTxId: admitted.source.transaction_id,
          nativeTxCompactCbor: admitted.nativeTxCompactCbor,
          itemCbors: decodeMidgardFieldPreimage(
            Buffer.from(admitted.fields[fieldIndex], "hex"),
          ),
          owner: ports.signer.paymentKeyHash,
          publish: true,
          label: "value conservation",
        }),
        compactCbor: admitted.nativeTxCompactCbor,
        witnessSetCompactCbor:
          proofSource.witnessSetCompactCbor.toString("hex"),
        certificate,
      };
    },
  });
};
