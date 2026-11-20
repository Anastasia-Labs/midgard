import { isDeepStrictEqual } from "node:util";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKUsers from "@al-ft/midgard-sdk/users";
import { Data } from "@lucid-evolution/lucid";

/** Durable preimages. Authority comes from the commitment in the L1 checkpoint. */
export type TransitionDepositOpening = Readonly<{
  commitmentCbor: string;
  openingCbor: string;
}>;

export const transitionDepositOpening = (
  captured: Pick<
    ReturnType<typeof SDKUsers.captureEventHistoryWitness>,
    "commitment" | "openingCbor"
  >,
): TransitionDepositOpening => ({
  commitmentCbor: Data.to(captured.commitment, SDKUsers.EventHistoryCommitment),
  openingCbor: captured.openingCbor,
});

export const reopenTransitionDeposit = (
  retained: TransitionDepositOpening,
  policy: string,
  source: {
    key: SDK.OutputReference;
    value: SDK.DepositInfo;
    valueCbor?: string;
  },
  checkpointCommitmentCbor?: string,
) => {
  const commitment = Data.from(
    retained.commitmentCbor,
    SDKUsers.EventHistoryCommitment,
  );
  const opening = Data.from(retained.openingCbor, SDKUsers.EventHistoryOpening);
  if (
    commitment.kind !== "Deposit" ||
    commitment.policy !== policy ||
    (checkpointCommitmentCbor !== undefined &&
      Data.to(
        Data.from(checkpointCommitmentCbor, SDKUsers.EventHistoryCommitment),
        SDKUsers.EventHistoryCommitment,
      ) !== Data.to(commitment, SDKUsers.EventHistoryCommitment)) ||
    !SDKUsers.opensEventHistoryCommitmentCbor(
      commitment,
      plutusConstrFieldCbor(retained.openingCbor, [0]),
      plutusConstrFieldCbor(retained.openingCbor, [1]),
    ) ||
    !("DepositPayload" in opening.payload)
  )
    throw new Error(
      "Transition deposit opening differs from its authenticated commitment",
    );
  const event = opening.payload.DepositPayload.event;
  const infoCbor = plutusConstrFieldCbor(retained.openingCbor, [0, 0, 1]);
  // The raw leaf decides; the typed view must be its decoded view. Neither
  // side is re-encoded: user datum Data need not survive a typed encoder.
  if (
    source.valueCbor !== undefined &&
    !isDeepStrictEqual(
      Data.from(source.valueCbor, SDK.DepositInfo),
      source.value,
    )
  )
    throw new Error(
      "Transition deposit source view differs from its raw bytes",
    );
  if (
    Data.to(event.id, SDK.OutputReference) !==
      Data.to(source.key, SDK.OutputReference) ||
    aikenSerialisedPlutusDataCborPreservingMapOrder(infoCbor) !==
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        source.valueCbor ?? Data.to(source.value, SDK.DepositInfo),
      )
  )
    throw new Error(
      "Transition deposit source differs from the authenticated event",
    );
  return { commitment, opening, event, infoCbor };
};
